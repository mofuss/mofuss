"""Attribute v14 NRB to woodfuel depletion, excluding prescribed LUC losses.

The signed balance is observational: no balance or NRB output feeds stocks,
growth, harvest or sourcing. It excludes annual stock resets and downward
preharvest clamps, retains positive regrowth offsets, and retains the legacy
end-of-year 2 Mg stock seed. The balance uses the production engine's native
float32 maps; final NRB is bounded by realized harvest to absorb roundoff.
"""
from __future__ import annotations

import argparse
import copy
import hashlib
import json
from pathlib import Path
import xml.etree.ElementTree as E

from build_dinamica_sourcing_v12 import calculate, filename, node, port, prop, save, serialize_children
from dinamica_v12_transform import _parse_spans, _producers


CONTRACT = "woodfuel_attributed_signed_balance_v1"
MARKER_KEY = "mofuss.nrb.attribution.contract"
NEW_IDS = ("v93000", "v93001", "v93002", "v93003", "v93004")

# i1=S after the LUC reset, i2=P after harvest, i3=H realized harvest,
# i4=legacy deforestation exclusion, i5=B available before harvest,
# i6=immutable initial-stock support, i7=current TOF flag.
ANNUAL_NRB = (
    "if isNull(i6) then null else if isNull(i3) then null "
    "else if i3 <= 0 then 0 "
    "else if isNull(i1) or isNull(i2) or isNull(i5) then null "
    "else if i7 = 1 then 0 "
    "else if i4 = 0 then max(0, min(i3, min(i1, i5) - i2)) else 0"
)
ANNUAL_FNRB = (
    "if isNull(i3) then null else if isNull(i2) then null "
    "else if i2 <= 0 then 0 else i1 / i2"
)
# i1=previous Cend; i2=S; i3=B; i4=P; i5=H; i6=initial support; i7=E.
# Missing annual stock inside support is a domain gap, not a new initial cell.
# Zero-harvest gaps carry history; impossible positive-harvest gaps become
# NoData and stay visible rather than silently assigning zero depletion.
POST_BALANCE = (
    "if isNull(i6) then null else if isNull(i1) or isNull(i5) then null "
    "else if isNull(i2) or isNull(i3) or isNull(i4) or isNull(i7) then "
    "if i5 <= 0 then i1 else null "
    "else i1 + min(i2, i3) - i4"
)
# Correct for the seed without allowing a domain gap to erase prior history.
END_BALANCE = (
    "if isNull(i6) then null else if isNull(i1) or isNull(i5) then null "
    "else if isNull(i2) or isNull(i3) or isNull(i7) or isNull(i8) then "
    "if i5 <= 0 then i4 else null "
    "else i1 - (i2 - i3)"
)
TERMINAL_NRB = (
    "if isNull(i2) then null else if isNull(i4) then null "
    "else if i4 <= 0 then 0 "
    "else if isNull(i3) then max(0, min(i4, i1)) else if i3 > 0 then 0 "
    "else max(0, min(i4, i1))"
)
DEFORESTATION_GATE = "if isNull(i3) then null else if isNull(i2) then i1 else i1 + i2"
TERMINAL_FNRB = (
    "if isNull(i3) then null else if isNull(i2) then null "
    "else if i2 <= 0 then 0 else i1 / i2"
)


def _maps(element):
    return {int(n.findtext("inputport[@name='mapNumber']")):
            n.find("inputport[@name='map']").get("peerid")
            for n in element.findall("functor") if n.get("name") == "NumberMap"}


def _expression(element):
    return " ".join(element.findtext("inputport[@name='expression']").strip("[] \n\r\t").split())


def _validate_corrected(root):
    producers = _producers(root)
    marker = root.find(f"property[@key='{MARKER_KEY}']")
    if marker is None or marker.get("value") != CONTRACT:
        raise ValueError("Missing NRB attribution contract marker")
    expected = {
        "v97": (ANNUAL_NRB, ("v90008", "v131", "v130", "v112", "v171", "v200", "v90005")),
        "v96": (ANNUAL_FNRB, ("v97", "v130", "v200")),
        "v193": (TERMINAL_NRB, ("v93003", "v200", "v117", "v107")),
        "v194": (TERMINAL_FNRB, ("v193", "v107", "v200")),
        "v117": (DEFORESTATION_GATE, ("v77", "v112", "v200")),
        "v93000": ("if isNull(i1) then null else 0", ("v200",)),
        "v93002": (POST_BALANCE, ("v93001", "v90008", "v171", "v131", "v130", "v200", "v98")),
        "v93003": (END_BALANCE, ("v93002", "v98", "v131", "v93001", "v130", "v200", "v90008", "v171")),
    }
    for ident, (expression, maps) in expected.items():
        actual = producers.get(ident)
        if (actual is None or _expression(actual) != expression or
                _maps(actual) != dict(enumerate(maps, 1)) or
                actual.findtext("inputport[@name='cellType']") != ".float32"):
            raise ValueError("NRB attribution calculation changed: " + ident)
    mux = producers["v93001"]
    if (mux.get("name") != "MuxMap" or
            mux.find("inputport[@name='initial']").get("peerid") != "v93000" or
            mux.find("inputport[@name='feedback']").get("peerid") != "v93003"):
        raise ValueError("NRB attribution balance feedback changed")
    if producers["v93004"].findtext("inputport[@name='format']") != '"debugging_<v1>/Woodfuel_balance<v2,2>.tif"':
        raise ValueError("NRB attribution annual balance filename changed")
    filename_values = {int(n.findtext("inputport[@name='valueNumber']")):
                       n.find("inputport[@name='value']").get("peerid")
                       for n in producers["v93004"].findall("functor")
                       if n.get("name") == "NumberValue"}
    if filename_values != {1: "v38", 2: "v39"}:
        raise ValueError("NRB attribution annual filename must use current MC and annual step")
    writers = [n for n in root.iter("functor") if n.get("name") == "SaveMap"
               and n.find("inputport[@name='filename']") is not None
               and n.find("inputport[@name='filename']").get("peerid") == "v93004"]
    if len(writers) != 1 or writers[0].find("inputport[@name='map']").get("peerid") != "v93002":
        raise ValueError("NRB attribution must save exactly one annual postharvest balance family")
    parents = {child: parent for parent in root.iter() for child in parent}
    annual = producers["v39"]
    if (parents[writers[0]] is not annual or
            writers[0].findtext("inputport[@name='step']") != ".none" or
            writers[0].findtext("inputport[@name='suffixDigits']") != "0"):
        raise ValueError("NRB attribution annual balance writer changed")
    if any(parents[producers[ident]] is not annual for ident in ("v93001", "v93002", "v93003", "v93004")):
        raise ValueError("NRB attribution ledger is outside the annual loop")
    if parents[producers["v93000"]] is not parents[annual]:
        raise ValueError("NRB attribution balance initialization is not per realization")


def correct_nrb_attribution(text: str) -> tuple[str, dict]:
    """Apply the reviewed NRB observer correction without changing dynamics."""
    root, data, spans = _parse_spans(text)
    producers = _producers(root)
    if root.find(f"property[@key='{MARKER_KEY}']") is not None:
        _validate_corrected(root)
        return text, {"already_applied": True, "contract": CONTRACT}
    for ident in NEW_IDS:
        if ident in producers:
            raise ValueError("NRB attribution reserved ID already occupied: " + ident)
    for ident in ("v90008", "v90005", "v200", "v171", "v131", "v98", "v130", "v112", "v117", "v107"):
        if ident not in producers:
            raise ValueError("Expected v14 model state: " + ident)
    for ident, maps in {
        "v96": ("v90008", "v131", "v130", "v112"),
        "v97": {1: "v96", 3: "v130"},
        "v193": ("v98", "v200", "v117"),
        "v194": ("v193", "v107"),
        "v117": ("v77", "v112"),
    }.items():
        if _maps(producers[ident]) != (maps if isinstance(maps, dict) else dict(enumerate(maps, 1))):
            raise ValueError("Unexpected uncorrected NRB graph: " + ident)
    for ident, expected in {
        "v96": "if i4 = 0 then if i1 - i2 > 0 then (i1 - i2) / i3 else 0 else 0",
        "v97": "if isNull(i1) then null else i1 * i3",
        "v193": "if i1 >= i2 or i3 > 0 then 0 else i2 - i1",
        "v194": "if i2 = 0 then 0 else i1 / i2",
        "v117": "i1 + i2",
    }.items():
        if _expression(producers[ident]) != expected:
            raise ValueError("Unexpected uncorrected NRB expression: " + ident)
    edits = []
    root_open_end = data.index(b">", data.index(b"<script")) + 1
    edits.append((root_open_end, root_open_end,
                  f'\n    <property key="{MARKER_KEY}" value="{CONTRACT}" />'.encode()))

    def replace_calculation(ident, expression, maps, comment):
        original = producers[ident]
        replacement = copy.deepcopy(original)
        replacement.find("inputport[@name='expression']").text = "[\n    " + expression + "\n]"
        for child in list(replacement):
            if child.tag == "functor" and child.get("name") == "NumberMap":
                replacement.remove(child)
        prop(replacement, "dff.functor.comment", comment)
        for slot, peer in enumerate(maps, 1):
            entry = node(replacement, "NumberMap", "NRB attribution input " + str(slot))
            port(entry, "map", peer=peer)
            port(entry, "mapNumber", str(slot))
        # The source span excludes the original tail; serializing that tail
        # here would duplicate its indentation as a whitespace-only line.
        replacement.tail = None
        span = spans[id(original)]
        edits.append((span.start, span.end, E.tostring(replacement, encoding="utf-8")))

    replace_calculation("v97", ANNUAL_NRB,
                        ("v90008", "v131", "v130", "v112", "v171", "v200", "v90005"),
                        "Woodfuel-only annual NRB: bound realized depletion by harvest; exclude LUC resets, negative preharvest clamps and nondegradable TOF; preserve legacy deforestation gate")
    replace_calculation("v96", ANNUAL_FNRB, ("v97", "v130", "v200"),
                        "Annual woodfuel NRB divided by realized harvest; zero-harvest cells use legacy zero before division")
    replace_calculation("v193", TERMINAL_NRB, ("v93003", "v200", "v117", "v107"),
                        "Terminal woodfuel-only NRB from signed depletion balance with regrowth and seed offsets; exclude prescribed LUC losses and preserve legacy deforestation gate")
    replace_calculation("v194", TERMINAL_FNRB, ("v193", "v107", "v200"),
                        "Terminal woodfuel NRB divided by cumulative realized harvest; zero-harvest cells retain legacy zero")
    replace_calculation("v117", DEFORESTATION_GATE, ("v77", "v112", "v200"),
                        "Preserve the legacy deforestation exclusion and its accumulated history across zero-harvest annual-domain gaps within initial model stock support")

    initial = E.Element("fragment")
    calculate(initial, "Map", "Initial signed woodfuel balance on immutable stock support",
              "if isNull(i1) then null else 0", "v93000", maps=("v200",))
    annual = producers["v39"]
    insertion = spans[id(annual)].start
    edits.append((insertion, insertion, serialize_children(initial, 12).lstrip().encode() + b"\n            "))

    ledger = E.Element("fragment")
    mux = node(ledger, "MuxMap", "Previous signed woodfuel balance including stock seed")
    port(mux, "initial", peer="v93000")
    port(mux, "feedback", peer="v93003")
    E.SubElement(mux, "outputport", name="map", id="v93001")
    post = calculate(ledger, "Map", "Signed woodfuel balance after harvest", POST_BALANCE,
                     "v93002", maps=("v93001", "v90008", "v171", "v131", "v130", "v200", "v98"))
    prop(post, "dff.functor.comment", "Cpost = Cprevious + min(post-LUC start, preharvest stock) - postharvest stock. Signed regrowth offsets persist; zero-harvest domain gaps retain previous balance; no LUC reset enters this observer.")
    end = calculate(ledger, "Map", "Signed woodfuel balance including endpoint seed", END_BALANCE,
                    "v93003", maps=("v93002", "v98", "v131", "v93001", "v130", "v200", "v90008", "v171"))
    prop(end, "dff.functor.comment", "Cend = Cpost - (end stock - postharvest stock), retaining the existing 2 Mg forest seed. Float32 preserves compatibility with the production legacy engine.")
    filename(ledger, "debugging_<v1>/Woodfuel_balance<v2,2>.tif", ("v38", "v39"), "v93004")
    save(ledger, "Map", "v93002", "v93004")
    insertion = spans[id(annual)].close_start
    edits.append((insertion, insertion, serialize_children(ledger, 16).encode()))
    for start, end, replacement in sorted(edits, reverse=True):
        data = data[:start] + replacement + data[end:]
    output = data.decode()
    result = E.fromstring(output)
    _validate_corrected(result)
    result_producers = _producers(result)
    dangling = {p.get("peerid") for p in result.iter()
                if p.get("peerid") and p.get("peerid") not in result_producers}
    if dangling:
        raise ValueError("Dangling NRB attribution references: " + repr(dangling))
    return output, {
        "already_applied": False, "contract": CONTRACT,
        "source_sha256": hashlib.sha256(text.encode()).hexdigest(),
        "corrected_sha256": hashlib.sha256(data).hexdigest(),
        "changed_producer_ids": ["v96", "v97", "v117", "v193", "v194"],
        "new_producer_ids": list(NEW_IDS),
        "annual_balance_family": "debugging_<MC>/Woodfuel_balance<step,2>.tif",
        "balance_cell_type": "float32",
        "dynamics_unchanged": True,
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("source", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    if args.output.exists():
        raise FileExistsError("Refusing to replace an existing model: " + str(args.output))
    output, report = correct_nrb_attribution(args.source.read_text(encoding="utf-8"))
    args.output.write_text(output, encoding="utf-8")
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
