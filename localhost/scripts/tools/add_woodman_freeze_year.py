"""Add a configurable, inclusive Woodman freeze year to the v14 graph.

Only LUC=3 is affected. The selected year's LUC and TOF maps persist after
that year, but its transition map is applied once, never replayed. The real
simulation calendar and all demand, harvest and biomass clocks are unchanged.
"""
from __future__ import annotations

import argparse
import copy
import json
import sys
from pathlib import Path
import xml.etree.ElementTree as E

sys.dont_write_bytecode = True

from build_dinamica_sourcing_v12 import calculate, filename, node, number, port, prop, save, serialize_children
from dinamica_v12_transform import _parse_spans, _producers

PARAMETER = "woodman_luc_freeze_year"
MARKER_KEY = "mofuss.woodman.freeze.contract"
CONTRACT = "woodman_freeze_year_v1"
WIZARD_TAG = "Int_WoodmanFreezeYear"
EFFECTIVE_YEAR = "if v2 = 3 then min(v1, v3) else v1"
TRANSITION = ("if isNull(i1) then null else if v1 = 3 and v2 > v3 then 0 "
              "else if isNull(i2) then 0 else i2")
NEW_IDS = tuple(f"v{n}" for n in range(94000, 94007))


def _expression(n):
    return " ".join(n.findtext("inputport[@name='expression']").strip("[] \t\r\n").split())


def _values(n):
    return {int(v.findtext("inputport[@name='valueNumber']")):
            v.find("inputport[@name='value']").get("peerid")
            for v in n.findall("functor") if v.get("name") == "NumberValue"}


def validate_root_container_dependencies(root):
    """Reject cross-container cycles, including otherwise acyclic leaf graphs.

    Dinamica schedules each top-level Group as one unit. Moving a reader into
    an upstream group therefore creates a cycle even when individual output
    ports form an ordinary acyclic dependency graph.
    """
    parents = {child: parent for parent in root.iter() for child in parent}
    producers = _producers(root)

    def owner(element):
        while element in parents and parents[element] is not root:
            element = parents[element]
        return element

    edges = {}
    for element in root.iter():
        for inp in element.findall("inputport"):
            peer = inp.get("peerid")
            if peer not in producers:
                continue
            supplier, consumer = owner(producers[peer]), owner(element)
            if supplier is not consumer:
                edges.setdefault(supplier, set()).add(consumer)
    vertices = set(edges) | {item for values in edges.values() for item in values}
    waiting = {vertex: set() for vertex in vertices}
    for supplier, consumers in edges.items():
        for consumer in consumers:
            waiting[consumer].add(supplier)
    while waiting:
        ready = {vertex for vertex, dependencies in waiting.items() if not dependencies}
        if not ready:
            aliases = []
            for vertex in waiting:
                alias = vertex.find("property[@key='dff.functor.alias']")
                aliases.append(alias.get("value") if alias is not None else vertex.get("name"))
            raise ValueError("Cyclic root container dependencies: " + ", ".join(sorted(aliases)))
        waiting = {vertex: dependencies - ready for vertex, dependencies in waiting.items()
                   if vertex not in ready}


def validate_freeze_graph(root, *, allow_legacy_setup_location=False):
    p = _producers(root)
    marker = root.find(f"property[@key='{MARKER_KEY}']")
    if marker is None or marker.get("value") != CONTRACT:
        raise ValueError("Missing Woodman freeze contract")
    parents = {child: parent for parent in root.iter() for child in parent}
    setup_parents = {parents[p[ident]] for ident in ("v94000", "v94001", "v94002")}
    parameter_group = parents[p["v267"]]
    if setup_parents != {parameter_group}:
        if not allow_legacy_setup_location or setup_parents != {parents[p["v302"]]}:
            raise ValueError("Woodman freeze parameter readers must share the runtime parameter table's container")
    lookup = p["v94000"]
    if (lookup.findtext("inputport[@name='keys']") != f'[ "{PARAMETER}" ]' or
            lookup.findtext("inputport[@name='column']") != '"ParCHR"' or
            lookup.findtext("inputport[@name='valueIfNotFound']") != "2050" or
            lookup.find("inputport[@name='table']").get("peerid") != "v267"):
        raise ValueError("Woodman freeze parameter must be read by name with default 2050")
    if (p["v94002"].get("name") != "Int" or
            p["v94002"].find("inputport[@name='constant']").get("peerid") != "v94001"):
        raise ValueError("Woodman freeze parameter integer input changed")
    if (_expression(p["v94003"]) != EFFECTIVE_YEAR or
            _values(p["v94003"]) != {1: "v90001", 2: "v302", 3: "v94002"}):
        raise ValueError("Woodman effective input year changed")
    if _expression(p["v90001"]) != "v1 + v2 - 1":
        raise ValueError("Actual annual calendar must remain unchanged")
    for ident in ("v90002", "v90004", "v90006"):
        if _values(p[ident]) != {1: "v302", 2: "v94003"}:
            raise ValueError("Woodman filename does not use effective cover year: " + ident)
    for ident in ("v296", "v297"):
        if (_values(p[ident]).get(35) != "v94002" or
                "WoodmanFreezeYear=<v35>" not in p[ident].findtext("inputport[@name='format']")):
            raise ValueError("MC startup must validate the executed freeze year: " + ident)
    if (_expression(p["v90018"]) != TRANSITION or
            _values(p["v90018"]) != {1: "v302", 2: "v90001", 3: "v94002"}):
        raise ValueError("Woodman post-freeze transition suppression changed")
    raw_consumers = [n for n in root.iter("inputport") if n.get("peerid") == "v90007"]
    if len(raw_consumers) != 1 or raw_consumers[0] not in list(p["v90018"].iter()):
        raise ValueError("Raw transition map bypasses the freeze guard")
    if (p["v94005"].findtext("inputport[@name='format']") !=
            '"debugging_<v1>/woodman_luc_execution.csv"' or
            _values(p["v94005"]) != {1: "v38"}):
        raise ValueError("Woodman executed-configuration provenance filename changed")
    ids = [n.get("id") for n in root.iter() if n.get("id")]
    if len(ids) != len(set(ids)):
        raise ValueError("Duplicate output IDs in Woodman freeze graph")
    unknown = {n.get("peerid") for n in root.iter() if n.get("peerid")} - set(ids)
    if unknown:
        raise ValueError("Dangling freeze references: " + repr(unknown))
    if not allow_legacy_setup_location:
        validate_root_container_dependencies(root)


def add_freeze_year(text: str) -> tuple[str, dict]:
    """Patch narrow XML spans, preserving unrelated source and local changes."""
    root, data, spans = _parse_spans(text)
    p = _producers(root)
    if root.find(f"property[@key='{MARKER_KEY}']") is not None:
        validate_freeze_graph(root, allow_legacy_setup_location=True)
        parents = {child: parent for parent in root.iter() for child in parent}
        if parents[p["v94000"]] is parents[p["v267"]]:
            validate_freeze_graph(root)
            return text, {"already_applied": True, "contract": CONTRACT}
        # Repair the first freeze release's container placement without
        # touching equations, parameter values, or unrelated graph settings.
        setup = E.Element("fragment")
        edits = []
        for ident in ("v94000", "v94001", "v94002"):
            setup.append(copy.deepcopy(p[ident]))
            span = spans[id(p[ident])]
            edits.append((span.start, span.end, b""))
        target = spans[id(p["v267"])].end
        edits.append((target, target, ("\n" + serialize_children(setup, 8)).encode()))
        for start, end, replacement in sorted(edits, reverse=True):
            data = data[:start] + replacement + data[end:]
        output = data.decode("utf-8")
        validate_freeze_graph(E.fromstring(output))
        return output, {"already_applied": False, "contract": CONTRACT,
                        "parameter_container_repaired": True, "equations_unchanged": True}
    if set(NEW_IDS) & set(p):
        raise ValueError("Woodman freeze output IDs already occupied")
    if _expression(p["v90018"]) != "if isNull(i1) then null else if isNull(i2) then 0 else i2":
        raise ValueError("Unexpected v14 annual transition guard")
    for ident in ("v90002", "v90004", "v90006"):
        if _values(p[ident]) != {1: "v302", 2: "v90001"}:
            raise ValueError("Unexpected v14 annual filename inputs")
    edits = []

    def replace(original, replacement):
        replacement.tail = None
        E.indent(replacement, space="    ", level=4)
        s = spans[id(original)]
        edits.append((s.start, s.end, E.tostring(replacement, encoding="utf-8")))

    marker_at = data.index(b">", data.index(b"<script")) + 1
    edits.append((marker_at, marker_at,
                  f'\n    <property key="{MARKER_KEY}" value="{CONTRACT}" />'.encode()))
    setup = E.Element("fragment")
    lookup = node(setup, "GetTableValue", "Woodman freeze year from named parameter")
    port(lookup, "table", peer="v267")
    port(lookup, "keys", f'[ "{PARAMETER}" ]')
    port(lookup, "column", '"ParCHR"')
    port(lookup, "valueIfNotFound", "2050")
    E.SubElement(lookup, "outputport", name="result", id="v94000")
    calculate(setup, "Value", "Woodman freeze year numeric value", "v1", "v94001", values=("v94000",))
    constant = node(setup, "Int", "Woodman LUC freeze year (2000-2050; LUC=3 only)")
    prop(constant, "wizard.constant.input", WIZARD_TAG)
    prop(constant, "dff.functor.comment", "Inclusive final year of Woodman changes. Keep this year's cover and TOF thereafter, with zero subsequent transitions. Default 2050. Ignored for other LUC versions.")
    port(constant, "constant", peer="v94001")
    E.SubElement(constant, "outputport", name="object", id="v94002")
    # Runtime parameters belong together: the LUC-input Group is upstream of
    # this parameter Group. Placing this table reader beside the LUC selector
    # would introduce the reverse dependency and a native "Loop detected".
    s = spans[id(p["v267"])]
    edits.append((s.end, s.end, ("\n" + serialize_children(setup, 8)).encode()))
    annual = E.Element("fragment")
    calculate(annual, "Value", "Selected Woodman cover year (calendar unchanged)",
              EFFECTIVE_YEAR, "v94003", values=("v90001", "v302", "v94002"))
    s = spans[id(p["v90001"])]
    edits.append((s.end, s.end, ("\n" + serialize_children(annual, 16)).encode()))
    for ident in ("v90002", "v90004", "v90006"):
        replacement = copy.deepcopy(p[ident])
        for n in replacement.iter("inputport"):
            if n.get("peerid") == "v90001":
                n.set("peerid", "v94003")
        replace(p[ident], replacement)
    for ident in ("v296", "v297"):
        replacement = copy.deepcopy(p[ident])
        command = replacement.find("inputport[@name='format']")
        if "--args " not in command.text or 35 in _values(replacement):
            raise ValueError("Unexpected MC startup command structure")
        command.text = command.text.replace("--args ", "--args WoodmanFreezeYear=<v35> ", 1)
        number(replacement, "Value", "v94002", 35)
        replace(p[ident], replacement)
    transition = copy.deepcopy(p["v90018"])
    transition.find("inputport[@name='expression']").text = "[" + TRANSITION + "]"
    transition.find("property[@key='dff.functor.comment']").set("value", "After the inclusive freeze year, suppress all transition resets; never replay the held map's conversion year. Preserve annual NoData and initial-stock support.")
    for slot, peer in enumerate(("v302", "v90001", "v94002"), 1):
        number(transition, "Value", peer, slot)
    replace(p["v90018"], transition)

    # Record actual executed selectors per realization, independent of a later
    # parameters.csv or XML edit. Numeric Key values are documented explicitly.
    evidence = E.Element("fragment")
    keys = node(evidence, "LookupTable", "Executed configuration key domain")
    port(keys, "constant", '[\n    "Key" "Value"\n    1 0\n    2 0\n    3 0\n    4 0\n]')
    E.SubElement(keys, "outputport", name="object", id="v94006")
    table = node(evidence, "CalculateLookupTable", "Executed Woodman settings: 1 LUC, 2 freeze year, 3 contract version, 4 start year", True)
    port(table, "expression", "[if line = 1 then v1 else if line = 2 then v2 else if line = 3 then 1 else v3]")
    port(table, "keyName", '"Key"')
    port(table, "valueName", '"Value"')
    E.SubElement(table, "outputport", name="result", id="v94004")
    inp = node(table, "NumberTable", "Executed configuration keys")
    port(inp, "table", peer="v94006")
    port(inp, "tableNumber", "1")
    for slot, peer in enumerate(("v302", "v94002", "v247"), 1):
        number(table, "Value", peer, slot)
    filename(evidence, "debugging_<v1>/woodman_luc_execution.csv", ("v38",), "v94005")
    save(evidence, "LookupTable", "v94004", "v94005")
    s = spans[id(p["v38"])]
    edits.append((s.end, s.end, ("\n" + serialize_children(evidence, 12)).encode()))
    wizard = root.find("property[@key='metadata.wizard']")
    if wizard is not None:
        updated = copy.deepcopy(wizard)
        configuration = json.loads(wizard.get("value"))
        page = next(page for page in configuration["inputPages"]
                    if any(e.get("tag") == "Int_constant_4" for e in page.get("editors", [])))
        page["editors"].append({"name": "Woodman LUC freeze year", "description": "2000-2050, default 2050. LUC=3 only. Apply changes through this year, then hold its cover and TOF; biomass and demand keep evolving.", "tag": WIZARD_TAG})
        updated.set("value", json.dumps(configuration, indent=2, ensure_ascii=False))
        replace(wizard, updated)
    for start, end, replacement in sorted(edits, reverse=True):
        data = data[:start] + replacement + data[end:]
    output = data.decode("utf-8")
    validate_freeze_graph(E.fromstring(output))
    return output, {"already_applied": False, "contract": CONTRACT, "parameter": PARAMETER,
                    "default": 2050, "valid_years": [2000, 2050], "active_luc": 3,
                    "freeze_year_is_inclusive": True, "calendar_unchanged": True}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("model", type=Path)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    output, report = add_freeze_year(args.model.read_text(encoding="utf-8"))
    (args.output or args.model).write_text(output, encoding="utf-8")
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
