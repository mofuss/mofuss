"""Conservative, text-preserving performance transform for the v11 Dinamica graph.

This module does not run Dinamica or install a model. ``optimize_model`` returns
the revised XML text and an audit report. Numerical expressions, map formats,
iteration order, model settings, and active output writers are preserved.
"""

from __future__ import annotations

import argparse
import hashlib
import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any
import xml.etree.ElementTree as ET
from xml.parsers import expat


class OptimizationContractError(ValueError):
    """The source graph differs from the reviewed optimization contract."""


@dataclass
class _Span:
    start: int
    open_end: int
    close_start: int = -1
    end: int = -1


STATISTICS_ONLY_IDS = ("v23", "v27", "v128", "v129", "v311", "v312")
GEOMETRY_ONLY_ID = "v275"
DEAD_PRODUCERS = {
    "v41": ("ExtractMapAttributes", "extractMapAttributes2420"),
    "v44": ("ExtractMapAttributes", "extractMapAttributes242"),
}
DEAD_VIEWER_ALIAS = "extractMapAttributes7205"


def _require(condition: bool, message: str) -> None:
    if not condition:
        raise OptimizationContractError(message)


def _tag_end(data: bytes, start: int) -> int:
    quote = None
    for pos in range(start, len(data)):
        char = data[pos]
        if quote is not None:
            if char == quote:
                quote = None
        elif char in (34, 39):
            quote = char
        elif char == 62:
            return pos + 1
    raise OptimizationContractError("Unterminated XML tag")


def _parse_spans(text: str) -> tuple[ET.Element, bytes, dict[int, _Span]]:
    """Parse XML while retaining UTF-8 byte ranges instead of reserializing it."""
    data = text.encode("utf-8")
    root = ET.fromstring(data)
    parser = expat.ParserCreate()
    ordered: list[tuple[str, dict[str, str], _Span]] = []
    stack: list[_Span] = []

    def start(tag: str, attrs: dict[str, str]) -> None:
        pos = parser.CurrentByteIndex
        span = _Span(pos, _tag_end(data, pos))
        ordered.append((tag, attrs, span))
        stack.append(span)

    def end(_tag: str) -> None:
        span = stack.pop()
        if data[span.start:span.open_end].rstrip().endswith(b"/>"):
            span.close_start = span.open_end
            span.end = span.open_end
        else:
            span.close_start = parser.CurrentByteIndex
            span.end = _tag_end(data, span.close_start)

    parser.StartElementHandler = start
    parser.EndElementHandler = end
    parser.Parse(data, True)
    elements = list(root.iter())
    _require(len(elements) == len(ordered), "XML span parser disagrees with element parser")
    spans: dict[int, _Span] = {}
    for element, (tag, attrs, span) in zip(elements, ordered):
        _require(element.tag == tag and element.attrib == attrs, "XML span mapping mismatch")
        spans[id(element)] = span
    return root, data, spans


def _alias(node: ET.Element) -> str:
    values = [p.get("value", "") for p in node.findall("property")
              if p.get("key") == "dff.functor.alias"]
    return values[0] if values else ""


def _producers(root: ET.Element) -> dict[str, ET.Element]:
    result: dict[str, ET.Element] = {}
    for node in root.iter():
        for port in node:
            if port.tag not in ("outputport", "internaloutputport") or "id" not in port.attrib:
                continue
            key = port.attrib["id"]
            _require(key not in result, f"Duplicate output port: {key}")
            result[key] = node
    return result


def _input(node: ET.Element, name: str) -> ET.Element:
    ports = [p for p in node.findall("inputport") if p.get("name") == name]
    _require(len(ports) == 1, f"Expected one {name} input on {_alias(node)}")
    return ports[0]


def _whole_line_range(data: bytes, span: _Span) -> tuple[int, int]:
    start = data.rfind(b"\n", 0, span.start) + 1
    if data[start:span.start].strip():
        start = span.start
    newline = data.find(b"\n", span.end)
    end = span.end
    if newline >= 0 and not data[span.end:newline].strip():
        end = newline + 1
    return start, end


def optimize_model(text: str) -> tuple[str, dict[str, Any]]:
    """Apply only reviewed statistics switches and asserted dead-node removals.

    The contract fails closed if a removed output acquired a consumer, a reviewed
    producer changed identity, or an expected statistics flag is no longer .yes.
    This is deliberately a one-time v11 transform, not an idempotent formatter.
    """
    root, data, spans = _parse_spans(text)
    producers = _producers(root)
    parent = {id(child): node for node in root.iter() for child in node}
    edits: list[tuple[int, int, bytes, str]] = []
    flag_changes: list[dict[str, str]] = []
    removed: list[dict[str, Any]] = []

    def disable(port_id: str, flag: str) -> None:
        _require(port_id in producers, f"Missing reviewed statistics producer {port_id}")
        node = producers[port_id]
        _require(node.get("name") == "ExtractMapAttributes", f"Unexpected producer for {port_id}")
        port = _input(node, flag)
        _require(not port.attrib.get("peerid") and (port.text or "").strip() == ".yes",
                 f"Expected literal .yes for {port_id}/{flag}")
        span = spans[id(port)]
        old = data[span.open_end:span.close_start]
        _require(old.strip() == b".yes", f"Unexpected content in {port_id}/{flag}")
        value_start = span.open_end + old.index(b".yes")
        edits.append((value_start, value_start + 4, b".no", f"{port_id}/{flag}"))
        flag_changes.append({"output_id": port_id, "alias": _alias(node),
                             "input": flag, "before": ".yes", "after": ".no"})

    for port_id in STATISTICS_ONLY_IDS:
        disable(port_id, "extractStatisticalAttributes")
    disable(GEOMETRY_ONLY_ID, "extractDynamicAttributes")
    disable(GEOMETRY_ONLY_ID, "extractStatisticalAttributes")

    nodes_to_remove: list[ET.Element] = []
    for port_id, (name, alias) in DEAD_PRODUCERS.items():
        _require(port_id in producers, f"Missing reviewed dead producer {port_id}")
        node = producers[port_id]
        _require(node.get("name") == name and _alias(node) == alias,
                 f"Unexpected dead producer identity for {port_id}")
        nodes_to_remove.append(node)
    viewers = [node for node in root.iter("functor") if _alias(node) == DEAD_VIEWER_ALIAS]
    _require(len(viewers) == 1, "Expected one initial-stock statistics viewer")
    viewer = viewers[0]
    _require(viewer.get("name") == "ExtractMapAttributes"
             and not viewer.findall("outputport") and not viewer.findall("internaloutputport"),
             "Initial-stock statistics viewer is no longer output-free")
    nodes_to_remove.append(viewer)

    removed_elements = {id(child) for node in nodes_to_remove for child in node.iter()}
    removed_ids = {port_id for port_id, node in producers.items() if id(node) in removed_elements}
    _require(removed_ids == set(DEAD_PRODUCERS), "Dead branches contain unexpected output ports")
    for node in root.iter():
        if node.get("peerid") in removed_ids:
            _require(id(node) in removed_elements,
                     f"Cannot remove {node.get('peerid')}: external consumer on "
                     f"{_alias(parent[id(node)]) or parent[id(node)].tag}")

    for node in nodes_to_remove:
        start, end = _whole_line_range(data, spans[id(node)])
        label = _alias(node)
        edits.append((start, end, b"", f"remove/{label}"))
        removed.append({"alias": label, "functor": node.get("name"),
                        "output_ids": [key for key, value in producers.items() if value is node]})

    ordered = sorted(edits, key=lambda edit: edit[0])
    for previous, current in zip(ordered, ordered[1:]):
        _require(previous[1] <= current[0], "Optimization edits overlap")
    result = data
    for start, end, replacement, _label in reversed(ordered):
        result = result[:start] + replacement + result[end:]
    revised = result.decode("utf-8")
    new_root = ET.fromstring(result)
    new_producers = _producers(new_root)
    old_unresolved = {node.get("peerid") for node in root.iter()
                      if node.get("peerid") and node.get("peerid") not in producers}
    new_unresolved = {node.get("peerid") for node in new_root.iter()
                      if node.get("peerid") and node.get("peerid") not in new_producers}
    _require(new_unresolved == old_unresolved, "Optimization introduced unresolved peer IDs")
    _require(set(producers) - set(new_producers) == removed_ids,
             "Unexpected change in output-port inventory")

    report = {
        "transform": "dinamica_v12_conservative_speed_v1",
        "source_sha256": hashlib.sha256(data).hexdigest().upper(),
        "result_sha256": hashlib.sha256(result).hexdigest().upper(),
        "flags_changed": flag_changes,
        "nodes_removed": removed,
        "removed_output_ids": sorted(removed_ids),
        "arithmetic_expressions_rewritten": 0,
        "map_storage_or_precision_changes": 0,
        "existing_output_writers_changed": 0,
        "ratio_viewer_preserved": True,
        "byte_edits": [{"source_start": start, "source_end": end, "label": label,
                        "replacement_bytes": len(replacement)}
                       for start, end, replacement, label in ordered],
        "verification_scope": "Static graph contract only; paired runtime equivalence is required.",
    }
    return revised, report


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", type=Path)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--report", type=Path)
    args = parser.parse_args()
    _require(args.input.resolve() != args.output.resolve(), "Source model must not be overwritten")
    _require(not args.output.exists(), "Output already exists; choose a new path")
    if args.report:
        _require(not args.report.exists(), "Report already exists; choose a new path")
    text = args.input.read_bytes().decode("utf-8")
    revised, report = optimize_model(text)
    args.output.write_bytes(revised.encode("utf-8"))
    if args.report:
        args.report.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
