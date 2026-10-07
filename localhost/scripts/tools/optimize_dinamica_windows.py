"""Exact table-lookup optimization for the installed Windows Dinamica engine.

General Table lookups inside CalculateMap fall back to the interpreter in the
legacy Windows engine. Select the current Monte Carlo row into a double-valued
LookupTable outside the annual loop; only the table access syntax changes in
the pixel expression. Arithmetic order and float32 output stages are retained.

This module does not write canonical models. Call optimize_table_lookups(text)
and validate the returned candidate with the native regression tests.
"""
from __future__ import annotations

import copy
import re
import xml.etree.ElementTree as ET

TARGET_OUTPUTS = ("v201", "v203", "v207", "v213", "v90010", "v90011")
TABLE_SOURCES = {
    "v201": {1: "v242", 2: "v244"}, "v203": {1: "v244"},
    "v207": {2: "v244"}, "v213": {1: "v243"},
    "v90010": {1: "v244"}, "v90011": {1: "v243"},
}
_LOOKUP = re.compile(r"t(\d+)\[\[v1\]\[i1\s*\+\s*1\]\]")
_SELECTED_LOOKUP = re.compile(r"t(\d+)\[i1\s*\+\s*1\]")


def _node(parent, name, alias, container=False):
    result = ET.SubElement(parent, "containerfunctor" if container else "functor", name=name)
    ET.SubElement(result, "property", key="dff.functor.alias", value=alias)
    return result


def _port(parent, name, text=None, peer=None):
    result = ET.SubElement(parent, "inputport", name=name)
    if peer is not None:
        result.set("peerid", peer)
    result.text = text
    return result


def _number(parent, kind, peer, slot):
    n = _node(parent, "Number" + kind, "Selected MC row input " + str(slot))
    _port(n, {"Table": "table", "Value": "value"}[kind], peer=peer)
    _port(n, kind.lower() + "Number", str(slot))


def append_selected_row(parent, table_peer, row_peer, first_id):
    """Append exact source-column-index -> selected-row-value lookup nodes."""
    ids = [f"v{first_id + i}" for i in range(4)]
    info = _node(parent, "GetTableInfo", "Column metadata for " + table_peer)
    _port(info, "table", peer=table_peer)
    ET.SubElement(info, "outputport", name="tableInfo", id=ids[0])
    columns = _node(parent, "GetTableColumn", "All source column indices for " + table_peer)
    _port(columns, "table", peer=ids[0])
    _port(columns, "columnIndexOrName", '"Column_Type"')
    ET.SubElement(columns, "outputport", name="result", id=ids[1])
    template = _node(parent, "LookupTable", "Numeric source column indices for " + table_peer)
    _port(template, "constant", peer=ids[1])
    ET.SubElement(template, "outputport", name="object", id=ids[2])
    selected = _node(parent, "CalculateLookupTable", "Exact selected MC row for " + table_peer, True)
    _port(selected, "expression", "[t2[[v1][line]]]")
    _port(selected, "keyName", ".none")
    _port(selected, "valueName", ".none")
    ET.SubElement(selected, "outputport", name="result", id=ids[3])
    _number(selected, "Table", ids[2], 1)
    _number(selected, "Table", table_peer, 2)
    _number(selected, "Value", row_peer, 1)
    return ids[3]


def _table_hook(node, slot):
    return next(n.find("inputport[@name='table']") for n in node.findall("functor")
                if n.get("name") == "NumberTable" and
                n.find("inputport[@name='tableNumber']").text == str(slot))


def _existing_selected_rows(mc, producers):
    """Recognize only this transform's complete, exact helper chains."""
    selected = {}
    for node in mc.findall("containerfunctor"):
        alias = node.find("property[@key='dff.functor.alias']")
        if alias is None or not alias.get("value", "").startswith("Exact selected MC row for "):
            continue
        if node.get("name") != "CalculateLookupTable" or node.find("inputport[@name='expression']").text != "[t2[[v1][line]]]":
            raise ValueError("Altered selected-row helper")
        table_peer = _table_hook(node, 2).get("peerid")
        row = next(n for n in node.findall("functor") if n.get("name") == "NumberValue" and
                   n.find("inputport[@name='valueNumber']").text == "1")
        row_peer = row.find("inputport[@name='value']").get("peerid")
        template = producers[_table_hook(node, 1).get("peerid")]
        columns = producers[template.find("inputport[@name='constant']").get("peerid")]
        info = producers[columns.find("inputport[@name='table']").get("peerid")]
        if (row_peer != "v10" or template.get("name") != "LookupTable" or
                columns.get("name") != "GetTableColumn" or
                columns.find("inputport[@name='columnIndexOrName']").text != '"Column_Type"' or
                info.get("name") != "GetTableInfo" or
                info.find("inputport[@name='table']").get("peerid") != table_peer or
                alias.get("value") != "Exact selected MC row for " + table_peer):
            raise ValueError("Altered selected-row helper input chain")
        key = (table_peer, row_peer)
        if key in selected:
            raise ValueError("Duplicate selected-row helper")
        selected[key] = node.find("outputport[@name='result']").get("id")
    return selected


def optimize_table_lookups(source, *, target_outputs=TARGET_OUTPUTS, first_id=92000):
    """Return (candidate XML, report), selecting each distinct source once/MC.

    v13 contains the four baseline targets; v14 contains all six. Unknown or
    altered expressions fail closed. The MC repeat is required to ensure the
    selected row is refreshed for every draw, and never for every annual step.
    """
    root = ET.fromstring(source)
    producers = {p.get("id"): n for n in root.iter() for p in n.findall("outputport")}
    mc = next((n for n in root.iter("containerfunctor") if n.get("name") == "Repeat" and
               any(p.get("key") == "dff.functor.alias" and p.get("value") == "repeat775"
                   for p in n.findall("property"))), None)
    if mc is None:
        raise ValueError("Cannot locate the Monte Carlo repeat775 scope")
    selected = _existing_selected_rows(mc, producers)
    changes = []
    additions = ET.Element("fragment")
    all_ids = {p.get("id") for p in root.iter() if p.get("id")}
    for output in target_outputs:
        if output not in producers:
            if output.startswith("v900"):
                continue  # The validated v13 source has no annual LUC nodes.
            raise ValueError("Missing expected table-lookup output " + output)
        node = producers[output]
        expression = node.find("inputport[@name='expression']")
        before = expression.text or ""
        matches = list(_LOOKUP.finditer(before))
        if not matches:
            slots = {int(m.group(1)) for m in _SELECTED_LOOKUP.finditer(before)}
            if (not slots or "[[" in before or
                    slots != set(TABLE_SOURCES[output]) or
                    any(_table_hook(node, slot).get("peerid") != selected.get((TABLE_SOURCES[output][slot], "v10")) for slot in slots)):
                raise ValueError("Expected matrix or validated selected-row lookup at " + output)
            changes.append({"output": output, "before": before, "after": before, "already_optimized": True})
            continue
        row_input = next((n for n in node.findall("functor") if n.get("name") == "NumberValue" and
                          n.find("inputport[@name='valueNumber']").text == "1"), None)
        if row_input is None:
            raise ValueError("Missing Monte Carlo row input at " + output)
        row_peer = row_input.find("inputport[@name='value']").get("peerid")
        if row_peer != "v10":
            raise ValueError("Unexpected Monte Carlo row peer at " + output)
        slots = sorted({int(m.group(1)) for m in matches})
        if set(slots) != set(TABLE_SOURCES[output]) or "[[" in _LOOKUP.sub("lookup", before):
            raise ValueError("Unexpected additional table access at " + output)
        for slot in slots:
            port = _table_hook(node, slot)
            table_peer = port.get("peerid")
            if table_peer != TABLE_SOURCES[output][slot]:
                raise ValueError("Unexpected source table at " + output)
            key = (table_peer, row_peer)
            if key not in selected:
                start = first_id + len(selected) * 4
                new_ids = {f"v{i}" for i in range(start, start + 4)}
                if new_ids & all_ids:
                    raise ValueError("Reserved optimization output IDs already present")
                selected[key] = append_selected_row(additions, table_peer, row_peer, start)
                all_ids.update(new_ids)
            port.set("peerid", selected[key])
        expression.text = _LOOKUP.sub(lambda m: "t" + m.group(1) + "[i1 + 1]", before)
        if not re.search(r"\bv1\b", expression.text):
            node.remove(row_input)
        changes.append({"output": output, "before": before, "after": expression.text})
    for addition in additions:
        mc.append(copy.deepcopy(addition))
    ET.indent(root, space="    ")
    candidate = ET.tostring(root, encoding="unicode")
    return candidate, {
        "schema": "mofuss_windows_exact_mc_row_lookup_v1",
        "targets": changes,
        "selected_rows": [{"source_table": k[0], "row_peer": k[1], "lookup_output": v}
                          for k, v in selected.items()],
        "numeric_contract": "double lookup values; unchanged map arithmetic and output cell type",
    }
