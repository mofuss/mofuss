"""Build a conservative performance candidate for the frozen WebMoFuSS v3 model.

No model is run or installed. The default keeps every output writer in its
original scope and preserves arithmetic, map precision, input/output paths,
external processes, and wizard metadata. Native regression on the server's
Dinamica version remains required before replacing its working model.

Known limitation: row selection evaluates every table column eagerly. An unused
text column accepted by the original model makes this candidate fail. The real
server's numeric preprocessing contract must be verified before deployment.

The optional --last-mc-debugging avoids intermediate overwrites of shared
Debugging maps. It preserves completed files, but changes when those files
appear during a run, so it is deliberately not part of the default candidate.
"""
from __future__ import annotations

import argparse
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

    # Reuse the existing guarded, double-precision MC-row selection algorithm,
    # but apply its individual edits to the original text instead of accepting
    # ElementTree's reformatting of the entire model and embedded wizard HTML.
    lookup_text, lookup_report = optimize_table_lookups(source, target_outputs=TARGETS)
    optimized = ET.fromstring(lookup_text)
    optimized_producers = _producers(optimized)
    optimized_mc = optimized_producers["v8"]
    edits: list[tuple[int, int, bytes, str]] = []

    def replace(node: ET.Element, replacement: bytes, label: str) -> None:
        span = spans[id(node)]
        edits.append((span.start, span.end, replacement, label))

    for output in TARGETS:
        before, after = producers[output], optimized_producers[output]
        old_expression = input_port(before, "expression")
        new_expression = input_port(after, "expression")
        replace(old_expression, _serialized(new_expression), f"MC-row lookup expression {output}")
        for hook in before.findall("functor"):
            if hook.get("name") != "NumberTable":
                continue
            slot = int(hook.findtext("inputport[@name='tableNumber']"))
            old_table = input_port(hook, "table")
            new_table = input_port(_hook(after, "Table", slot), "table")
            if old_table.attrib != new_table.attrib:
                replace(old_table, _serialized(new_table), f"MC-row table hook {output}/{slot}")
        # All four original v1 uses were only row indices. Their now-unused
        # NumberValue hook is removed exactly as in the reusable transform.
        require(not re.search(r"\bv1\b", new_expression.text or ""), f"Residual MC row use at {output}")
        replace(_hook(before, "Value", 1), b"", f"Unused MC-row hook {output}")

    added_nodes = [node for node in optimized_mc
                   if any(p.get("id") not in producers for p in node.findall("outputport"))]
    require(len(added_nodes) == 12 and len(lookup_report["selected_rows"]) == 3,
            "Expected exactly three four-node MC-row helper chains")
    additions = [_serialized(node) for node in added_nodes]

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
