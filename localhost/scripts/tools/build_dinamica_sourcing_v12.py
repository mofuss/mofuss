"""Build the observational v12 model without rewriting the v11 numerical graph.

The capture is a side branch: exact static float32 component bases, compact
annual eligibility/Patcher states, and losslessly encoded normalization scalars.
No captured quantity is fed back into model dynamics. Runtime parity is a
separate, mandatory release gate, not something this builder claims.
"""
from __future__ import annotations

import argparse
import copy
import hashlib
import json
from pathlib import Path
import xml.etree.ElementTree as E

from dinamica_v12_transform import _parse_spans, _producers, optimize_model
from dinamica_scalar_codec_v1 import append_scalar_codec_table


def prop(node, key, value):
    E.SubElement(node, "property", key=key, value=value)


def port(node, name, text=None, peer=None):
    attrs = {"name": name}
    if peer is not None:
        attrs["peerid"] = peer
    child = E.SubElement(node, "inputport", attrs)
    child.text = text
    return child


def node(parent, name, alias, container=False):
    result = E.SubElement(parent, "containerfunctor" if container else "functor", name=name)
    prop(result, "dff.functor.alias", alias)
    return result


def number(parent, kind, peer, slot):
    child = node(parent, "Number" + kind, "Sourcing input " + str(slot))
    port(child, {"Map": "map", "Value": "value", "Table": "table"}[kind], peer=peer)
    port(child, kind.lower() + "Number", str(slot))


def calculate(parent, kind, alias, expression, result, maps=(), values=(), tables=(), cell_type=".float32", null=".default"):
    child = node(parent, "Calculate" + kind, alias, True)
    port(child, "expression", "[\n    " + expression + "\n]")
    if kind == "Map":
        port(child, "cellType", cell_type)
        port(child, "nullValue", null)
        port(child, "resultIsSparse", ".no")
        port(child, "resultFormat", ".none")
    else:
        port(child, "defaultValue", ".none")
    E.SubElement(child, "outputport", name="result", id=result)
    for k, peers in (("Map", maps), ("Value", values), ("Table", tables)):
        for slot, peer in enumerate(peers, 1):
            number(child, k, peer, slot)
    return child


def filename(parent, pattern, peers, result):
    child = node(parent, "CreateString", "Sourcing filename", True)
    port(child, "format", '"' + pattern + '"')
    E.SubElement(child, "outputport", name="result", id=result)
    for slot, peer in enumerate(peers, 1):
        number(child, "Value", peer, slot)


def save(parent, kind, source, destination):
    child = node(parent, "Save" + kind, "Sourcing capture " + kind)
    port(child, "map" if kind == "Map" else "table", peer=source)
    port(child, "filename", peer=destination)
    port(child, "suffixDigits", "0")
    port(child, "step", ".none")
    if kind == "Map":
        port(child, "useCompression", ".yes")
    port(child, "workdir", ".none")


def serialize_children(group, indent):
    parts = []
    for child in group:
        E.indent(child, space="    ", level=indent // 4)
        parts.append(" " * indent + E.tostring(child, encoding="unicode").rstrip())
    return "\n".join(parts) + "\n"


def add_domestic_tof_correction(text: str):
    """v13 bug fix: a domestic W origin may not pool its TOF deficit regionally.

    Preconditions (checked by the release/install preflight): W domains are
    disjoint domestic country domains, or a single own-polygon/country component.
    All other growth, demand, stock clipping and V mechanics are unchanged.
    """
    root, data, spans = _parse_spans(text)
    producers = _producers(root)
    if any(f"v{x}" in producers for x in range(5000, 5011)):
        raise ValueError("TOF correction IDs already used")
    group = E.Element("fragment")
    mux = node(group, "MuxMap", "Origin-preserving W TOF redistribution accumulator")
    port(mux, "initial", peer="v191")
    port(mux, "feedback", peer="v5007")
    E.SubElement(mux, "outputport", name="map", id="v5000")
    calculate(group, "Map", "W origin shortfall on trees outside forests",
              "if i1 = 1 then if i2 > i3 then i2 - i3 else 0 else 0", "v5001",
              maps=("v204", "v366", "v213"))
    calculate(group, "Map", "Eligible forest weights for the same W origin",
              "if i1 = 1 then null else i2", "v5002", maps=("v204", "v364"))
    for source, target, label in (("v5001", "v5003", "W origin TOF shortfall sum"),
                                  ("v5002", "v5004", "W origin forest weight sum")):
        child = node(group, "ExtractMapAttributes", label)
        port(child, "map", peer=source)
        port(child, "extractDynamicAttributes", ".yes")
        port(child, "extractStatisticalAttributes", ".no")
        E.SubElement(child, "outputport", name="attributes", id=target)
    calculate(group, "Value", "W origin TOF shortfall scalar", "t1[12]", "v5005", tables=("v5003",))
    calculate(group, "Value", "W origin forest weight scalar", "t1[12]", "v5008", tables=("v5004",))
    calculate(group, "Map", "Redistribute TOF shortfall only within this W origin",
              "if t1[12] <= 0 or t2[12] <= 0 then 0 else i1 * t1[12] / t2[12]",
              "v5006", maps=("v5002",), tables=("v5003", "v5004"))
    calculate(group, "Map", "Add this origin's domestic TOF redistribution",
              "if isNull(i1) then i2 else if isNull(i2) then i1 else i1 + i2",
              "v5007", maps=("v5000", "v5006"))
    replacement = E.Element("fragment")
    child = calculate(replacement, "Map", "Domestic W TOF redistribution (v13 correction)",
                      "i1", "v93", maps=("v5007",))
    prop(child, "dff.functor.comment", "Bug correction: v11/v12 pooled all W TOF deficits and redistributed them using regional forest weights. v13 retains each domestic demand origin. With no eligible domestic forest, its shortfall remains unmet. This intentionally changes affected scientific outputs.")
    annual_span = spans[id(producers["v356"])]
    insert_at = data.rfind(b"\n", 0, annual_span.close_start) + 1
    old = spans[id(producers["v93"])]
    replace_from = data.rfind(b"\n", 0, old.start) + 1
    replace_to = old.end
    if data[replace_to:replace_to+2] == b"\r\n":
        replace_to += 2
    elif data[replace_to:replace_to+1] == b"\n":
        replace_to += 1
    for start, end, addition in sorted([
        (insert_at, insert_at, serialize_children(group, 20).encode()),
        (replace_from, replace_to, serialize_children(replacement, 16).encode()),
    ], reverse=True):
        data = data[:start] + addition + data[end:]
    return data.decode(), {"bug": "regional_pooling_of_domestic_W_TOF_shortfall",
                            "corrected": True, "changed_existing_output_id": "v93",
                            "requires_disjoint_domestic_W_domains": True,
                            "no_eligible_domestic_forest": "shortfall remains unmet"}


def add_capture(text: str, validation_components=False, domestic_tof=False):
    root, data, spans = _parse_spans(text)
    producers = _producers(root)
    if any(4000 <= int(x[1:]) < 5000 or int(x[1:]) >= 9000
           for x in producers if x.startswith("v") and x[1:].isdigit()):
        raise ValueError("Reserved sourcing IDs already present")
    if producers["v39"].get("name") != "Repeat":
        raise ValueError("Annual loop contract changed")
    additions = []
    annual = E.Element("fragment")
    calculate(annual, "Value", "Capture static sourcing bases once per snapshot",
              "if v1 = 1 and (v2 = 1 or v2 = v3) then 1 else 0", "v4000",
              values=("v38", "v39", "v354"))
    initial = node(annual, "IfThen", "Capture the exact accumulator null domain", True)
    port(initial, "condition", peer="v4000")
    writer = node(initial, "SaveMap", "Sourcing initial zero and null accumulator")
    port(writer, "map", peer="v191")
    port(writer, "filename", '"Sourcing/static/accumulator_domain.tif"')
    port(writer, "suffixDigits", "0")
    port(writer, "step", ".none")
    port(writer, "useCompression", ".yes")
    port(writer, "workdir", ".none")
    if domestic_tof:
        calculate(annual, "Map", "Sourcing TOF versus forest selector", "if i1 = 1 then 0 else 1",
                  "v4050", maps=("v204",), cell_type=".uint8", null="255")
        filename(annual, "Sourcing/MC<v1,3>/forest_state<v2,2>.tif", ("v38", "v39"), "v4051")
        save(annual, "Map", "v4050", "v4051")
    for offset, channel, patcher, threshold, loop, component, demand, attrs, base, normalized in (
        (0, "W", "v181", "v279", "v356", "v357", "v358", "v365", "v362", "v366"),
        (20, "V", "v184", "v278", "v370", "v372", "v373", "v380", "v377", "v381"),
    ):
        threshold_test = "i2 < v1 and i3 != 1" if channel == "W" else "i2 < v1"
        mask_id, mask_path = f"v{4001+offset}", f"v{4002+offset}"
        calculate(annual, "Map", channel + " exact sourcing eligibility state",
                  "if isNull(i1) then null else if " + threshold_test + " then 0 else i1 + 1",
                  mask_id, maps=(patcher, "v40", "v204") if channel == "W" else (patcher, "v40"),
                  values=(threshold,), cell_type=".uint8", null="255")
        filename(annual, f"Sourcing/MC<v1,3>/mask_{channel}<v2,2>.tif", ("v38", "v39"), mask_path)
        save(annual, "Map", mask_id, mask_path)

        group = E.Element("fragment")
        denominator = f"v{4003+offset}"
        calculate(group, "Value", channel + " exact eligible normalization sum", "t1[12]", denominator, tables=(attrs,))
        scalar_peers = [denominator, demand, "v6", "v5", "v4", "v354"]
        if domestic_tof and channel == "W":
            scalar_peers += ["v5005", "v5008"]
        table_id, _ = append_scalar_codec_table(
            group, scalar_peers,
            prefix=channel + " sourcing", first_id=9000 + offset * 100)
        table_path = f"v{4004+offset}"
        filename(group, f"Sourcing/MC<v1,3>/{channel}_scalars<v2,3>_<v3,2>.csv",
                 ("v38", component, "v39"), table_path)
        save(group, "LookupTable", table_id, table_path)
        once = node(group, "IfThen", channel + " static component snapshot", True)
        port(once, "condition", peer="v4000")
        base_path = f"v{4005+offset}"
        filename(once, f"Sourcing/static/{channel}_base<v1,3>_<v2,2>.tif", (component, "v354"), base_path)
        save(once, "Map", base, base_path)
        if validation_components:
            direct_path = f"v{4006+offset}"
            filename(group, f"Sourcing/MC<v1,3>/{channel}_component<v2,3>_<v3,2>.tif",
                     ("v38", component, "v39"), direct_path)
            save(group, "Map", normalized, direct_path)
            if domestic_tof and channel == "W":
                filename(group, "Sourcing/MC<v1,3>/W_redistributed<v2,3>_<v3,2>.tif",
                         ("v38", component, "v39"), "v4007")
                save(group, "Map", "v5006", "v4007")
        additions.append((producers[loop], group, 20))
    additions.append((producers["v39"], annual, 16))

    edits = []
    for target, group, indent in additions:
        span = spans[id(target)]
        line = data.rfind(b"\n", 0, span.close_start) + 1
        if data[line:span.close_start].strip():
            raise ValueError("Expected standalone container closing tag")
        edits.append((line, serialize_children(group, indent).encode("utf-8")))
    result = data
    for position, addition in sorted(edits, reverse=True):
        result = result[:position] + addition + result[position:]
    revised = E.fromstring(result)
    new_producers = _producers(revised)
    for element in revised.iter():
        if element.get("peerid") and element.get("peerid") not in new_producers:
            raise ValueError("Dangling peer after capture: " + element.get("peerid"))
    return result.decode("utf-8"), {
        "schema": "mofuss_runtime_sourcing_v1",
        "observational_only": True,
        "original_model_nodes_modified": 0,
        "static_bases": "Exact NPA-adjusted, analysis-masked float32 components, once per decadal snapshot",
        "mask_states": {"0": "threshold forces zero", "1": "eligible, Patcher zero", "2": "eligible, Patcher one", "255": "null Patcher"},
        "scalar_keys": {"1:3": "eligible denominator", "4:6": "origin demand", "7:9": "weeks", "10:12": "time slices", "13:15": "demand adjustment percent", "16:18": "IDW source step"},
        "validation_component_maps": validation_components,
        "sourcing_is_observational": "Downstream shared clipping needs explicitly proportional attribution; this capture does not change harvest.",
        "known_legacy_caveat": ("W TOF shortfall is corrected to remain within its own origin in v13."
                                if domestic_tof else "W TOF shortfall remains regionally redistributed in v12, exactly as in v11."),
    }


def cache_static_bases(text: str):
    """Reuse precisely the Dinamica float32 bases captured during MC01.

    Recompute on the first annual step of each decadal snapshot; load that
    lossless TIFF thereafter. No map arithmetic is combined or reordered.
    A fresh run always refreshes every used snapshot in its first MC draw.
    """
    root, data, spans = _parse_spans(text)
    producers = _producers(root)
    edits = []
    for offset, channel, comp, load_id, npa_id, base_id in (
        (0, "W", "v357", "v360", "v361", "v362"),
        (10, "V", "v372", "v375", "v376", "v377"),
    ):
        cold_id, loaded_id, path_id = (f"v{6000+offset+i}" for i in range(3))
        fragment = E.Element("fragment")
        cold = node(fragment, "IfThen", channel + " refresh exact static base", True)
        port(cold, "condition", peer="v4000")
        for peer in (load_id, npa_id, base_id):
            original = producers[peer]
            copied = copy.deepcopy(original)
            if peer == base_id:
                copied.find("outputport").set("id", cold_id)
            cold.append(copied)
        warm = node(fragment, "IfNotThen", channel + " reuse exact static base", True)
        port(warm, "condition", peer="v4000")
        filename(warm, f"Sourcing/static/{channel}_base<v1,3>_<v2,2>.tif", (comp, "v354"), path_id)
        cached = node(warm, "LoadMap", channel + " load lossless float32 static base")
        port(cached, "filename", peer=path_id)
        port(cached, "nullValue", ".none")
        port(cached, "loadAsSparse", ".no")
        port(cached, "suffixDigits", "0")
        port(cached, "step", ".none")
        port(cached, "workdir", ".none")
        E.SubElement(cached, "outputport", name="map", id=loaded_id)
        junction = node(fragment, "MapJunction", channel + " current exact static base")
        port(junction, "possibleMap1", peer=cold_id)
        port(junction, "possibleMap2", peer=loaded_id)
        E.SubElement(junction, "outputport", name="map", id=base_id)
        for i, peer in enumerate((load_id, npa_id, base_id)):
            span = spans[id(producers[peer])]
            start = data.rfind(b"\n", 0, span.start) + 1
            end = span.end
            if data[end:end+2] == b"\r\n":
                end += 2
            elif data[end:end+1] == b"\n":
                end += 1
            edits.append((start, end, serialize_children(fragment, 20).encode() if i == 0 else b""))
    for start, end, addition in sorted(edits, reverse=True):
        data = data[:start] + addition + data[end:]
    _producers(E.fromstring(data))
    return data.decode(), {"enabled": True, "format": "Dinamica-written float32 compressed GeoTIFF",
                            "refresh": "MC01, first step of each used decadal snapshot",
                            "reused": "all later annual steps and MC draws",
                            "original_float32_stages_preserved": True}


def build(text, validation_components=False, domestic_tof=False, static_cache=False):
    optimized, optimization = optimize_model(text)
    correction = None
    if domestic_tof:
        optimized, correction = add_domestic_tof_correction(optimized)
    captured, capture = add_capture(optimized, validation_components, domestic_tof)
    cache_report = {"enabled": False}
    if static_cache:
        captured, cache_report = cache_static_bases(captured)
    return captured, {"version": "v13" if domestic_tof else "v12", "source_sha256": hashlib.sha256(text.encode()).hexdigest(),
                      "result_sha256": hashlib.sha256(captured.encode()).hexdigest(),
                      "optimization": optimization, "capture": capture,
                      "static_cache": cache_report, "intentional_bug_correction": correction}


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("source", type=Path)
    ap.add_argument("--output", type=Path, required=True)
    ap.add_argument("--report", type=Path, required=True)
    ap.add_argument("--validation-components", action="store_true")
    ap.add_argument("--domestic-tof-correction", action="store_true",
                    help="Build v13 with the explicit domestic-W TOF bug correction; not byte-equivalent to v11")
    ap.add_argument("--cache-static-bases", action="store_true")
    args = ap.parse_args()
    if args.output.exists() or args.report.exists():
        raise FileExistsError("Refusing to overwrite a model/build report")
    result, report = build(args.source.read_bytes().decode("utf-8"), args.validation_components,
                           args.domestic_tof_correction, args.cache_static_bases)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.report.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_bytes(result.encode("utf-8"))
    args.report.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(json.dumps({"model": str(args.output), "sha256": report["result_sha256"]}))


if __name__ == "__main__":
    main()
