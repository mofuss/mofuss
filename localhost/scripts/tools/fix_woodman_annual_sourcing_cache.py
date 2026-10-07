"""Approved scientific correction for v14's annual W/V sourcing domains.

This is deliberately separate from performance optimization: it changes
allocation after land-cover/domain changes. The old
cache saves the first year's landscape-masked base for a whole IDW snapshot.
The candidate caches only the original float32 NPA-adjusted component, then
applies the unchanged landscape-mask expression on every annual iteration.

Sourcing replay contract:
* Read static W_npa_base/V_npa_base instead of W_base/V_base.
* Eligibility captures also encode the current landscape mask.
* Read MCxxx/accumulator_domainYY.tif for each year, not the static accumulator.
* Keep legacy readers for old runs; never mix the two capture contracts.
* Require the explicit static contract CSV marker written during capture.

No initial-stock support, NPA arithmetic, annual biomass threshold, Patcher,
normalization, demand, growth or harvest equations are changed. Fixed-domain
parity is a testable requirement, not a claim of dynamic-domain equivalence.
"""
from __future__ import annotations

import argparse
import copy
import hashlib
import json
from pathlib import Path
import xml.etree.ElementTree as E

from build_dinamica_sourcing_v12 import filename, number, save, serialize_children, node, port
from dinamica_v12_transform import _parse_spans, _producers


CONTRACT = "annual_domain_after_static_npa_cache_v1"
MARKER_KEY = "mofuss.sourcing.capture.contract"


def _validate_corrected(root):
    """Reject incomplete or edited graphs instead of silently calling them done."""
    producers = _producers(root)
    parents = {child: parent for parent in root.iter() for child in parent}
    marker = root.find(f"property[@key='{MARKER_KEY}']")
    if marker is None or marker.get("value") != CONTRACT:
        raise ValueError("Missing annual sourcing contract marker")
    for cold, junction, raw, domain, eligibility, slot, names, channel in (
        ("v6000", "v362", "v91000", "v90003", "v4001", 4, ("v6002", "v4005"), "W"),
        ("v6010", "v377", "v91010", "v90013", "v4021", 3, ("v6012", "v4025"), "V"),
    ):
        if producers[cold].findtext("inputport[@name='expression']") != "[i1]":
            raise ValueError("Corrected cold cache must preserve the float32 identity")
        if producers[cold].findtext("inputport[@name='cellType']") != ".float32":
            raise ValueError("Corrected cold cache must retain float32 rounding")
        if producers[raw].get("name") != "MapJunction":
            raise ValueError("Corrected unmasked cache junction missing")
        annual = producers[junction]
        refs = {p.get("peerid") for p in annual.iter("inputport") if p.get("peerid")}
        if refs != {raw, domain} or " ".join(annual.findtext("inputport[@name='expression']").split()) != "[ if isNull(i2) then 0 else i1 ]":
            raise ValueError("Corrected annual landscape mask changed")
        if parents[annual].get("name") != "ForEach" or annual.findtext("inputport[@name='cellType']") != ".float32":
            raise ValueError("Annual mask must execute outside the cache at float32 precision")
        mask = producers[eligibility]
        if f"if isNull(i{slot}) then 0 else" not in mask.findtext("inputport[@name='expression']"):
            raise ValueError("Corrected eligibility capture lacks the annual domain")
        domain_inputs = [n for n in mask.findall("functor") if n.get("name") == "NumberMap"
                         and n.findtext("inputport[@name='mapNumber']") == str(slot)]
        if len(domain_inputs) != 1 or domain_inputs[0].find("inputport[@name='map']").get("peerid") != domain:
            raise ValueError("Corrected eligibility capture uses the wrong annual domain")
        for ident in names:
            if channel + "_npa_base" not in producers[ident].findtext("inputport[@name='format']"):
                raise ValueError("Corrected static cache filename changed")
        writers = [n for n in root.iter("functor") if n.get("name") == "SaveMap"
                   and n.find("inputport[@name='filename']") is not None
                   and n.find("inputport[@name='filename']").get("peerid") == names[1]]
        if len(writers) != 1 or writers[0].find("inputport[@name='map']").get("peerid") != raw:
            raise ValueError("Corrected capture must save the unmasked static base")
    if "accumulator_domain<v2,2>" not in producers["v91020"].findtext("inputport[@name='format']"):
        raise ValueError("Corrected annual accumulator filename changed")
    markers = [n for n in root.iter("functor") if n.get("name") == "SaveLookupTable"
               and n.findtext("inputport[@name='filename']") == f'"Sourcing/static/{CONTRACT}.csv"']
    if len(markers) != 1 or " ".join(markers[0].findtext("inputport[@name='table']").split()) != '[ "Key" "Value" 1 1 ]':
        raise ValueError("Corrected capture contract CSV writer missing or changed")
    condition = parents[markers[0]].find("inputport[@name='condition']")
    if parents[markers[0]].get("name") != "IfThen" or condition is None or condition.get("peerid") != "v4000":
        raise ValueError("Capture contract marker must be written in the initialized capture branch")


def correct_annual_sourcing_cache(text: str) -> tuple[str, dict]:
    root, data, spans = _parse_spans(text)
    producers = _producers(root)
    if root.find(f"property[@key='{MARKER_KEY}']") is not None:
        _validate_corrected(root)
        return text, {"already_applied": True, "contract": CONTRACT,
                      "candidate_only": False, "requires_new_sourcing_reader": True}
    for ident in ("v91000", "v91010", "v91020"):
        if ident in producers:
            raise ValueError("Correction marker missing or reserved ID occupied: " + ident)
    for ident in ("v90003", "v90013", "v90016"):
        if ident not in producers:
            raise ValueError("Expected the Windows v14 annual-domain graph: " + ident)
    edits = []
    # A root property protects idempotent source transformations; the CSV is a
    # runtime marker, written only after the initialized annual graph starts.
    root_open_end = data.index(b">", data.index(b"<script")) + 1
    edits.append((root_open_end, root_open_end,
                  f'\n    <property key="{MARKER_KEY}" value="{CONTRACT}" />'.encode()))

    def replace(original, replacement):
        span = spans[id(original)]
        encoded = E.tostring(replacement, encoding="utf-8")
        edits.append((span.start, span.end, encoded))

    changed_ids = []
    for channel, cold_id, junction_id, raw_id, domain_id, mask_id, slot, names in (
        ("W", "v6000", "v362", "v91000", "v90003", "v4001", 4, ("v6002", "v4005")),
        ("V", "v6010", "v377", "v91010", "v90013", "v4021", 3, ("v6012", "v4025")),
    ):
        original = producers[cold_id]
        expression = original.find("inputport[@name='expression']")
        expected = "[ if isNull(i2) then 0 else i1 ]"
        if " ".join(expression.text.split()) != expected:
            raise ValueError("Unexpected cached landscape expression: " + cold_id)
        maps = {n.findtext("inputport[@name='mapNumber']"):
                n.find("inputport[@name='map']").get("peerid")
                for n in original.findall("functor") if n.get("name") == "NumberMap"}
        if maps.get("2") != domain_id:
            raise ValueError("Cold branch does not use the reviewed annual domain: " + channel)

        # Retain the original float32 stage, replacing its masking with identity.
        cold = copy.deepcopy(original)
        cold.find("inputport[@name='expression']").text = "[i1]"
        for n in list(cold.findall("functor")):
            if n.get("name") == "NumberMap" and n.findtext("inputport[@name='mapNumber']") == "2":
                cold.remove(n)
        cold.find("property[@key='dff.functor.alias']").set("value", channel + " exact unmasked NPA cache base")
        replace(original, cold)

        # Existing consumers retain their IDs and receive a newly masked base.
        original_junction = producers[junction_id]
        if original_junction.get("name") != "MapJunction":
            raise ValueError("Expected cold/warm cache junction: " + junction_id)
        junction = copy.deepcopy(original_junction)
        junction.find("outputport").set("id", raw_id)
        junction.find("property[@key='dff.functor.alias']").set("value", channel + " exact static NPA base")
        annual = copy.deepcopy(original)
        annual.find("outputport").set("id", junction_id)
        annual.find("property[@key='dff.functor.alias']").set("value", channel + " apply current annual landscape after cache")
        for n in annual.findall("functor"):
            if n.get("name") == "NumberMap" and n.findtext("inputport[@name='mapNumber']") == "1":
                n.find("inputport[@name='map']").set("peerid", raw_id)
        fragment = E.Element("fragment")
        fragment.extend((junction, annual))
        span = spans[id(original_junction)]
        edits.append((span.start, span.end, serialize_children(fragment, 20).lstrip().encode()))

        for ident in names:
            n = copy.deepcopy(producers[ident])
            p = n.find("inputport[@name='format']")
            old = channel + "_base"
            if old not in p.text:
                raise ValueError("Unexpected cache filename: " + ident)
            p.text = p.text.replace(old, channel + "_npa_base")
            replace(producers[ident], n)
        writers = [n for n in root.iter("functor") if n.get("name") == "SaveMap"
                   and n.find("inputport[@name='filename']") is not None
                   and n.find("inputport[@name='filename']").get("peerid") == names[1]]
        if len(writers) != 1 or writers[0].find("inputport[@name='map']").get("peerid") != junction_id:
            raise ValueError("Expected one exact static-base capture writer: " + channel)
        writer = copy.deepcopy(writers[0])
        writer.find("inputport[@name='map']").set("peerid", raw_id)
        replace(writers[0], writer)

        # Domain-zero precedes annual threshold and multiplication in the graph.
        mask = copy.deepcopy(producers[mask_id])
        p = mask.find("inputport[@name='expression']")
        marker = "if isNull(i1) then null else "
        if marker not in p.text:
            raise ValueError("Unexpected annual eligibility capture: " + mask_id)
        p.text = p.text.replace(marker, marker + f"if isNull(i{slot}) then 0 else ", 1)
        number(mask, "Map", domain_id, slot)
        replace(producers[mask_id], mask)
        changed_ids.extend((cold_id, junction_id, mask_id, *names))

    # The model's annual accumulator follows the annual domain as well.
    annual_loop = producers["v39"]
    capture = E.Element("fragment")
    filename(capture, "Sourcing/MC<v1,3>/accumulator_domain<v2,2>.tif",
             ("v38", "v39"), "v91020")
    save(capture, "Map", "v90016", "v91020")
    insertion = spans[id(annual_loop)].close_start
    edits.append((insertion, insertion, serialize_children(capture, 16).encode()))
    legacy = [n for n in root.iter("functor") if n.get("name") == "SaveMap"
              and n.findtext("inputport[@name='filename']") == '"Sourcing/static/accumulator_domain.tif"']
    if len(legacy) != 1:
        raise ValueError("Expected one old static accumulator capture")
    # Replace the old observer writer in its existing first-MC capture branch.
    # This creates no dependency on model calculations and never feeds dynamics.
    marker_group = E.Element("fragment")
    marker_writer = node(marker_group, "SaveLookupTable", "Annual sourcing capture contract")
    port(marker_writer, "table", '[\n    "Key" "Value"\n    1 1\n]')
    port(marker_writer, "filename", f'"Sourcing/static/{CONTRACT}.csv"')
    for key, value in (("suffixDigits", "0"), ("step", ".none"), ("workdir", ".none")):
        port(marker_writer, key, value)
    replace(legacy[0], marker_writer)
    for start, end, replacement in sorted(edits, reverse=True):
        data = data[:start] + replacement + data[end:]
    output = data.decode()
    result = E.fromstring(output)
    result_producers = _producers(result)
    _validate_corrected(result)
    dangling = {p.get("peerid") for p in result.iter() if p.get("peerid") and p.get("peerid") not in result_producers}
    if dangling:
        raise ValueError("Dangling candidate references: " + repr(dangling))
    return output, {
        "candidate_only": False, "already_applied": False, "contract": CONTRACT,
        "scientific_change": "Annual domains are applied after static NPA cache, including domain reentry and TOF-to-forest V eligibility",
        "fixed_domain_requirement": "Exact float32 values and null masks must remain identical",
        "source_sha256": hashlib.sha256(text.encode()).hexdigest(),
        "candidate_sha256": hashlib.sha256(data).hexdigest(),
        "changed_producer_ids": changed_ids,
        "new_producer_ids": ["v91000", "v91010", "v91020"],
        "requires_new_sourcing_reader": True,
        "sourcing_static_files": "Sourcing/static/{W,V}_npa_baseCCC_SS.tif",
        "sourcing_annual_accumulator": "Sourcing/MCxxx/accumulator_domainYY.tif",
        "initial_stock_support_unchanged": True,
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("source", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    if args.output.exists():
        raise FileExistsError("Refusing to replace an existing model: " + str(args.output))
    output, report = correct_annual_sourcing_cache(args.source.read_text(encoding="utf-8"))
    args.output.write_text(output, encoding="utf-8")
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
