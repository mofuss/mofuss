"""Preserve completed outputs while avoiding redundant Windows model I/O.

This transform changes execution scope only. Shared Debugging files are written
by the last MC, matching the original overwrite behavior. The three immutable
Chapman-Richards parameter maps load once per MC when the uncapped branch is on.
No numerical expression, MC draw, scientific output, or cache is removed.
"""
from __future__ import annotations

import copy
import xml.etree.ElementTree as ET

from dinamica_v12_transform import _parse_spans, _producers, _whole_line_range


MARKER = "Windows performance: final MC shared diagnostics"
CR_MARKER = "Windows performance: immutable CR inputs per MC"


def optimize_io(text: str) -> tuple[str, dict]:
    root, data, spans = _parse_spans(text)
    producers = _producers(root)
    if any(p.get("value") == MARKER for p in root.iter("property")):
        return text, {"already_applied": True}
    if "v92050" in producers:
        raise ValueError("Performance condition ID v92050 already used")
    parents = {id(c): n for n in root.iter() for c in n}
    annual = producers["v39"]
    mc = producers["v8"]
    if annual.get("name") != "Repeat" or parents[id(annual)] is not mc:
        raise ValueError("Expected nested MC/year Repeat containers")
    writers = [n for n in annual.iter("functor") if n.get("name") == "SaveMap"
               and (n.findtext("inputport[@name='filename']") or "").startswith('"Debugging/')]
    if len(writers) != 17:
        raise ValueError("Expected 17 shared diagnostic writers in annual loop")
    for writer in writers:
        scope = parents[id(writer)]
        while scope is not annual and scope.get("name") == "Group":
            scope = parents[id(scope)]
        if scope is not annual:
            raise ValueError("A shared writer acquired conditional or repeated scope")
    edits = []
    for n in writers:
        if n.findall("outputport"):
            raise ValueError("Diagnostic writer unexpectedly supplies model feedback")
        span = spans[id(n)]
        original = data[span.start:span.end].decode()
        wrapper = ('<containerfunctor name="IfThen">\n'
                   '                    <property key="dff.functor.alias" value="Save shared diagnostic on final MC" />\n'
                   '                    <inputport name="condition" peerid="v92050" />\n'
                   '                    ' + original + '\n'
                   '                </containerfunctor>')
        edits.append((span.start, span.end, wrapper.encode()))
    condition = '''<containerfunctor name="CalculateValue">
                <property key="dff.functor.alias" value="Windows performance: final MC shared diagnostics" />
                <property key="dff.functor.comment" value="Only the last MC survives the original shared-file overwrites; retain every annual per-MC and sourcing output." />
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
            </containerfunctor>
            '''
    cr = ET.Element("containerfunctor", name="IfThen")
    ET.SubElement(cr, "property", key="dff.functor.alias", value=CR_MARKER)
    expected_files = {"v172": "k_c.tif", "v173": "m_c.tif", "v174": "A_c.tif"}
    original_parent = parents[id(producers["v172"])]
    branch_condition = original_parent.find("inputport[@name='condition']")
    if original_parent.get("name") != "IfThen" or branch_condition is None:
        raise ValueError("Expected conditional uncapped growth branch")
    # Retain the condition: capped runs need not possess these optional inputs.
    cr.append(copy.deepcopy(branch_condition))
    for port_id, basename in expected_files.items():
        n = producers[port_id]
        if (n.get("name") != "LoadMap" or parents[id(n)] is not original_parent
                or n.findtext("inputport[@name='filename']") != f'"LULCC/TempRaster/{basename}"'
                or n.findtext("inputport[@name='suffixDigits']") != "0"):
            raise ValueError(f"Immutable CR loader contract changed: {port_id}")
        moved = copy.deepcopy(n)
        step = moved.find("inputport[@name='step']")
        if step is None or step.get("peerid") != "v39":
            raise ValueError(f"Unexpected step input: {port_id}")
        step.attrib.pop("peerid")
        step.text = ".none"
        cr.append(moved)
        start, end = _whole_line_range(data, spans[id(n)])
        edits.append((start, end, b""))
    ET.indent(cr, space="    ", level=3)
    before_year = condition + ET.tostring(cr, encoding="unicode").rstrip() + "\n            "
    start = spans[id(annual)].start
    edits.append((start, start, before_year.encode()))
    for start, end, replacement in sorted(edits, reverse=True):
        data = data[:start] + replacement + data[end:]
    result = data.decode()
    _producers(ET.fromstring(result))
    return result, {"shared_diagnostic_writers_last_mc": len(writers),
                    "immutable_CR_loaders_once_per_mc": list(expected_files),
                    "all_completed_output_paths_preserved": True,
                    "numerical_expressions_unchanged": True}
