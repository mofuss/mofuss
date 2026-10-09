"""Build a conservative performance candidate for the frozen WebMoFuSS v3 model.

No model is run or installed. The default keeps every output writer in its
original scope and preserves arithmetic, map precision, input/output paths,
external processes, and wizard metadata. Native regression on the server's
Dinamica version remains required before replacing its working model.

Numeric tables with the current MC row use a selected-row cache. Other inputs
retain the original expressions behind a conditional fallback. This preserves
the demonstrated unused-text-column and missing-row lazy-access cases; native
regression is still required for the complete workflow and other edge cases.

The optional --last-mc-debugging avoids intermediate overwrites of shared
Debugging maps. It preserves completed files, but changes when those files
appear during a run, so it is deliberately not part of the default candidate.
"""
from __future__ import annotations

import argparse
import copy
import hashlib
import json
from pathlib import Path
import re
import sys
import xml.etree.ElementTree as ET

# Imported helpers must not leave generated artifacts in the source repository.
sys.dont_write_bytecode = True
REPOSITORY = Path(__file__).resolve().parents[2]
TOOLS = REPOSITORY / "localhost" / "scripts" / "tools"
sys.path.insert(0, str(TOOLS))
from dinamica_v12_transform import _parse_spans, _producers  # noqa: E402
from optimize_dinamica_windows import optimize_table_lookups  # noqa: E402

DEFAULT_SOURCE = REPOSITORY / "webmofuss" / "7_dyn_Sc17_webmofuss_ctrees_g_v3.egoml"
SOURCE_SHA256 = "e4ce6ab47a12bbc7ed1f290a95ad6c1477182d21e95fba1428d816ecc85bca2d"
TARGETS = ("v201", "v203", "v207", "v213")
STATISTICS_ONLY = ("v23", "v27", "v128", "v129", "v311", "v312")
DEBUG_CONDITION_ID = "v92050"
NUMERIC_CONDITION_ID = "v97099"
CACHE_CONDITION_ID = "v97105"


class CandidateContractError(ValueError):
    """The frozen source or an optimization invariant was not satisfied."""


def require(condition: bool, message: str) -> None:
    if not condition:
        raise CandidateContractError(message)


def alias(node: ET.Element) -> str:
    return next((p.get("value", "") for p in node.findall("property")
                 if p.get("key") == "dff.functor.alias"), "")


def input_port(node: ET.Element, name: str) -> ET.Element:
    matches = node.findall(f"inputport[@name='{name}']")
    require(len(matches) == 1, f"Expected one {name} port on {alias(node)}")
    return matches[0]


def _hook(node: ET.Element, kind: str, slot: int) -> ET.Element:
    matches = [n for n in node.findall("functor") if n.get("name") == "Number" + kind
               and n.findtext(f"inputport[@name='{kind.lower()}Number']") == str(slot)]
    require(len(matches) == 1, f"Expected one {kind} hook {slot} on {alias(node)}")
    return matches[0]


def _serialized(node: ET.Element) -> bytes:
    # Element tails belong to the surrounding original source, not the edit.
    return ET.tostring(node, encoding="utf-8").rstrip()


def _node(parent, name, label, *, container=False, inputs=(), output=None):
    node = ET.SubElement(parent, "containerfunctor" if container else "functor", name=name)
    ET.SubElement(node, "property", key="dff.functor.alias", value=label)
    for port_name, value, peer in inputs:
        port = ET.SubElement(node, "inputport", name=port_name)
        if peer:
            port.set("peerid", value)
        else:
            port.text = value
    if output:
        ET.SubElement(node, "outputport", name=output[0], id=output[1])
    return node


def _number(parent, kind, peer, slot):
    return _node(parent, "Number" + kind, f"WebMoFuSS guard input {kind} {slot}",
                 inputs=((kind.lower(), peer, True), (kind.lower() + "Number", str(slot), False)))


def _guarded_lookup_nodes(producers, optimized_producers, lookup_report):
    """Return local calculation replacements, outer helpers and MC helpers.

    Only metadata and numeric key lists are hoisted outside the MC loop. Actual
    row values are read only when all columns are numeric and all three source
    tables contain this positive MC key. Otherwise the unmodified expressions
    run in the same original scopes, retaining their per-pixel lazy branches.
    """
    outer, mc = ET.Element("fragment"), ET.Element("fragment")
    numeric_keys = ET.Element("containerfunctor", name="IfThen")
    ET.SubElement(numeric_keys, "property", key="dff.functor.alias",
                  value="WebMoFuSS guard: numeric source keys")
    ET.SubElement(numeric_keys, "inputport", name="condition", peerid=NUMERIC_CONDITION_ID)
    row_guard = _node(mc, "IfThen", "WebMoFuSS guard: numeric MC row check", container=True,
                      inputs=(("condition", NUMERIC_CONDITION_ID, True),))
    row_checks, attributes = [], []
    for index, row in enumerate(lookup_report["selected_rows"]):
        selected = optimized_producers[row["lookup_output"]]
        template = optimized_producers[input_port(_hook(selected, "Table", 1), "table").get("peerid")]
        columns = optimized_producers[input_port(template, "constant").get("peerid")]
        info = optimized_producers[input_port(columns, "table").get("peerid")]
        for node in (info, columns, template):
            outer.append(copy.deepcopy(node))
        attribute_id = f"v{97000 + index}"
        attributes.append(attribute_id)
        _node(outer, "ExtractLookupTableAttributes", f"WebMoFuSS guard: types {row['source_table']}",
              inputs=(("table", template.find("outputport").get("id"), True),
                      ("extractStatisticalKeyAttributes", ".no", False),
                      ("extractStatisticalValueAttributes", ".no", False),
                      ("extractDynamicKeyValueAttributes", ".yes", False)),
              output=("attributes", attribute_id))
        keys_id, key_lookup_id = f"v{97200 + 2 * index}", f"v{97201 + 2 * index}"
        _node(numeric_keys, "GetTableKeys", f"WebMoFuSS guard: keys {row['source_table']}",
              inputs=(("table", row["source_table"], True),), output=("keys", keys_id))
        _node(numeric_keys, "LookupTable", f"WebMoFuSS guard: numeric keys {row['source_table']}",
              inputs=(("constant", keys_id, True),), output=("object", key_lookup_id))
        check_id = f"v{97100 + index}"
        row_checks.append(check_id)
        _node(row_guard, "GetLookupTableValue", f"WebMoFuSS guard: row present {row['source_table']}",
              inputs=(("table", key_lookup_id, True), ("key", row["row_peer"], True),
                      ("valueIfNotFound", "0", False)), output=("value", check_id))

    numeric = _node(outer, "CalculateValue", "WebMoFuSS guard: all source columns numeric", container=True,
                    inputs=(("expression", "[t1[31] = t1[1] and t2[31] = t2[1] and t3[31] = t3[1]]", False),
                            ("defaultValue", ".none", False)), output=("result", NUMERIC_CONDITION_ID))
    for slot, peer in enumerate(attributes, 1):
        _number(numeric, "Table", peer, slot)
    outer.append(numeric_keys)
    row_condition = _node(row_guard, "CalculateValue", "WebMoFuSS guard: all MC rows present", container=True,
                          inputs=(("expression", "[v1 = v4 and v2 = v4 and v3 = v4]", False),
                                  ("defaultValue", ".none", False)), output=("result", "v97103"))
    for slot, peer in enumerate(row_checks + ["v10"], 1):
        _number(row_condition, "Value", peer, slot)
    nonnumeric = _node(mc, "IfNotThen", "WebMoFuSS guard: original text-table path", container=True,
                       inputs=(("condition", NUMERIC_CONDITION_ID, True),))
    _node(nonnumeric, "CalculateValue", "WebMoFuSS guard: disable cache", container=True,
          inputs=(("expression", "[0]", False), ("defaultValue", ".none", False)),
          output=("result", "v97104"))
    _node(mc, "ValueJunction", "WebMoFuSS guard: safe MC cache condition",
          inputs=(("possibleValue1", "v97103", True), ("possibleValue2", "v97104", True)),
          output=("value", CACHE_CONDITION_ID))
    cache = _node(mc, "IfThen", "WebMoFuSS guard: selected MC rows", container=True,
                   inputs=(("condition", CACHE_CONDITION_ID, True),))
    for row in lookup_report["selected_rows"]:
        cache.append(copy.deepcopy(optimized_producers[row["lookup_output"]]))

    replacements, branches = {}, []
    for index, output in enumerate(TARGETS):
        local = ET.Element("fragment")
        fallback_id, cached_id = f"v{94000 + index}", f"v{95000 + index}"
        for kind, original, private_id in (("IfNotThen", producers[output], fallback_id),
                                           ("IfThen", optimized_producers[output], cached_id)):
            branch = _node(local, kind, f"WebMoFuSS guard: {kind} {output}", container=True,
                           inputs=(("condition", CACHE_CONDITION_ID, True),))
            calculation = copy.deepcopy(original)
            calculation.find("outputport[@name='result']").set("id", private_id)
            branch.append(calculation)
        _node(local, "MapJunction", f"WebMoFuSS guard: original output {output}",
              inputs=(("possibleMap1", fallback_id, True), ("possibleMap2", cached_id, True)),
              output=("map", output))
        replacements[output] = list(local)
        branches.append({"output": output, "fallback_output": fallback_id, "cached_output": cached_id})
    return replacements, list(outer), list(mc), {
        "numeric_condition": NUMERIC_CONDITION_ID, "cache_condition": CACHE_CONDITION_ID,
        "missing_row_fallback": True, "calculation_branches": branches,
        "type_codes": {"string": 0, "real": 1}, "type_attributes": {"count": 1, "sum": 31},
    }


def build_candidate(source: str, *, last_mc_debugging: bool = False) -> tuple[str, dict]:
    """Return candidate text and an audit report without touching the filesystem.

    Only this exact reviewed source is accepted. This prevents later scientific
    model changes from silently inheriting assumptions valid only for old v3.
    Text edits retain all original bytes outside the reviewed edit spans.
    """
    raw = source.encode("utf-8")
    digest = hashlib.sha256(raw).hexdigest()
    require(digest == SOURCE_SHA256, "Source SHA-256 differs from the reviewed WebMoFuSS v3 model")
    root, data, spans = _parse_spans(source)
    producers = _producers(root)
    parents = {id(child): parent for parent in root.iter() for child in parent}
    mc, annual = producers["v8"], producers["v39"]
    require(mc.get("name") == "Repeat" and alias(mc) == "repeat775", "Unexpected MC repeat")
    require(annual.get("name") == "Repeat" and parents[id(annual)] is mc,
            "Expected annual Repeat directly inside the MC Repeat")
    require(input_port(mc, "iterations").get("peerid") == "v282", "Unexpected MC count")

    # Reuse the reviewed double-precision MC-row selection algorithm,
    # but apply its individual edits to the original text instead of accepting
    # ElementTree's reformatting of the entire model and embedded wizard HTML.
    lookup_text, lookup_report = optimize_table_lookups(source, target_outputs=TARGETS)
    optimized = ET.fromstring(lookup_text)
    optimized_producers = _producers(optimized)
    edits: list[tuple[int, int, bytes, str]] = []

    def replace(node: ET.Element, replacement: bytes, label: str) -> None:
        span = spans[id(node)]
        edits.append((span.start, span.end, replacement, label))

    require(len(lookup_report["selected_rows"]) == 3, "Expected three MC-row helpers")
    replacements, outer_nodes, mc_nodes, guard_report = _guarded_lookup_nodes(
        producers, optimized_producers, lookup_report)
    generated = outer_nodes + mc_nodes + [n for nodes in replacements.values() for n in nodes]
    generated_ids = [p.get("id") for node in generated for p in node.iter("outputport")]
    require(len(generated_ids) == len(set(generated_ids)), "Duplicate generated output IDs")
    require((set(generated_ids) & set(producers)) == set(TARGETS), "Guard IDs overlap the source graph")
    for output, nodes in replacements.items():
        replace(producers[output], b"\n".join(_serialized(node) for node in nodes),
                f"Guarded MC-row calculation with original fallback {output}")
    outer_insertion = spans[id(parents[id(mc)])].close_start
    edits.append((outer_insertion, outer_insertion,
                  b"\n        " + b"\n        ".join(_serialized(n) for n in outer_nodes) + b"\n    ",
                  "Once-per-run table metadata and guarded numeric keys"))
    additions = [_serialized(node) for node in mc_nodes]

    # Attribute key 12 is SUM and key 9 is valid-cell count: dynamic attributes
    # stay enabled. Key 5 is cell width, available from geometry alone.
    expected_keys = {**{key: "12" for key in STATISTICS_ONLY[:4]},
                     "v311": "9", "v312": "9", "v275": "5"}
    flag_changes = []
    for output, key in expected_keys.items():
        node = producers[output]
        require(node.get("name") == "ExtractMapAttributes", f"Unexpected attribute producer {output}")
        consumers = [p for p in root.iter("inputport") if p.get("peerid") == output]
        require(bool(consumers), f"Expected attribute consumers at {output}")
        for port in consumers:
            hook = parents[id(port)]
            require(hook.get("name") == "NumberTable", f"Unexpected attribute consumer at {output}")
            expression = input_port(parents[id(hook)], "expression").text or ""
            slot = hook.findtext("inputport[@name='tableNumber']")
            keys = re.findall(r"\bt" + re.escape(slot or "") + r"\[(\d+)\]", expression)
            require(bool(keys) and set(keys) == {key}, f"Unexpected attribute key at {output}")
        flags = ["extractStatisticalAttributes"]
        if output == "v275":
            flags.append("extractDynamicAttributes")
        for flag in flags:
            port = input_port(node, flag)
            require(port.get("peerid") is None and port.text == ".yes", f"Unexpected flag {output}/{flag}")
            span = spans[id(port)]
            require(data[span.open_end:span.close_start] == b".yes", "Unexpected attribute flag content")
            edits.append((span.open_end, span.close_start, b".no", f"Unused attribute flag {output}/{flag}"))
            flag_changes.append({"output_id": output, "flag": flag, "before": ".yes", "after": ".no"})

    gated_writers = []
    if last_mc_debugging:
        require(DEBUG_CONDITION_ID not in producers, "Final-MC condition ID is already used")
        writers = [node for node in annual.iter("functor") if node.get("name") == "SaveMap"
                   and node.findtext("inputport[@name='filename']", "").startswith('"Debugging/')]
        require(len(writers) == 25, "Expected 25 shared annual Debugging writers")
        for node in writers:
            scope = parents[id(node)]
            while scope is not annual and scope.get("name") == "Group":
                scope = parents[id(scope)]
            require(scope is annual and not node.findall("outputport"), "Unexpected Debugging writer scope or feedback")
            require(input_port(node, "step").get("peerid") == "v39", "Unexpected diagnostic year index")
            span = spans[id(node)]
            original = data[span.start:span.end]
            wrapper = (b'<containerfunctor name="IfThen">\n'
                       b'                    <property key="dff.functor.alias" value="WebMoFuSS performance: final MC diagnostic" />\n'
                       b'                    <inputport name="condition" peerid="v92050" />\n'
                       b'                    ' + original + b'\n                </containerfunctor>')
            replace(node, wrapper, f"Final-MC shared diagnostic {alias(node)}")
            gated_writers.append(node.findtext("inputport[@name='filename']"))
        additions.append(b'''<containerfunctor name="CalculateValue">
                <property key="dff.functor.alias" value="WebMoFuSS performance: final MC shared diagnostics" />
                <inputport name="expression">[v1 = v2]</inputport>
                <inputport name="defaultValue">.none</inputport>
                <outputport name="result" id="v92050" />
                <functor name="NumberValue">
                    <inputport name="value" peerid="v8" />
                    <inputport name="valueNumber">1</inputport>
                </functor>
                <functor name="NumberValue">
                    <inputport name="value" peerid="v282" />
                    <inputport name="valueNumber">2</inputport>
                </functor>
            </containerfunctor>''')

    insertion = spans[id(mc)].close_start
    edits.append((insertion, insertion, b"\n            " + b"\n            ".join(additions) + b"\n        ",
                  "MC-scoped helper additions"))
    ordered = sorted(edits, key=lambda e: e[0])
    require(all(left[1] <= right[0] for left, right in zip(ordered, ordered[1:])), "Overlapping candidate edits")
    result = data
    for start, end, replacement, _ in reversed(ordered):
        result = result[:start] + replacement + result[end:]
    revised = ET.fromstring(result)
    new_producers = _producers(revised)
    require(all(n.get("peerid") in new_producers for n in revised.iter() if n.get("peerid")),
            "Candidate contains an unresolved peer ID")
    report = {
        "schema": "webmofuss_v3_conservative_performance_v1",
        "source_sha256": digest,
        "candidate_sha256": hashlib.sha256(result).hexdigest(),
        "lookup_optimization": lookup_report,
        "lookup_guard": guard_report,
        "attribute_flags_changed": flag_changes,
        "last_mc_debugging": last_mc_debugging,
        "last_mc_debugging_writers": gated_writers,
        "existing_writers_preserved": True,
        "existing_writer_scopes_preserved": not last_mc_debugging,
        "arithmetic_and_cell_types_preserved": True,
        "native_validation": "Required before deployment; this builder provides static graph checks only.",
        "byte_edits": [{"source_start": start, "source_end": end, "label": label,
                        "replacement_bytes": len(replacement)} for start, end, replacement, label in ordered],
    }
    return result.decode("utf-8"), report


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", type=Path, default=DEFAULT_SOURCE)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--report", type=Path)
    parser.add_argument("--last-mc-debugging", action="store_true",
                        help="Write shared Debugging maps only on final MC; changes intermediate file availability")
    args = parser.parse_args()
    require(args.output.resolve() != args.source.resolve(), "Refusing to overwrite the original model")
    require(not args.output.exists(), "Output already exists; choose a new candidate path")
    if args.report:
        require(args.report.resolve() not in (args.source.resolve(), args.output.resolve()), "Report path conflicts with a model path")
        require(not args.report.exists(), "Report already exists; choose a new report path")
    source = args.source.read_bytes().decode("utf-8")
    candidate, report = build_candidate(source, last_mc_debugging=args.last_mc_debugging)
    args.output.write_bytes(candidate.encode("utf-8"))
    if args.report:
        args.report.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
