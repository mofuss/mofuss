"""Real-engine scalar codec regression; writes only under the supplied scratch root."""

from __future__ import annotations

import argparse
import hashlib
import json
import math
from pathlib import Path
import random
import struct
import subprocess
import sys
import xml.etree.ElementTree as ET

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "tools"))
from dinamica_scalar_codec_v1 import append_scalar_codec_table, decode_scalar_table


def bits(value: float) -> str:
    return struct.pack(">d", value).hex()


def probe_float64(engine: Path, scratch: Path) -> bool:
    """Ask the actual parser whether float64 is an accepted raster cell type."""
    scratch.mkdir(parents=True, exist_ok=True)
    script = ET.Element("script")
    node = ET.SubElement(script, "functor", name="CellType")
    ET.SubElement(node, "inputport", name="constant").text = ".float64"
    ET.SubElement(node, "outputport", name="object", id="v1")
    path = scratch / "float64_probe.egoml"
    ET.ElementTree(script).write(path, encoding="utf-8", xml_declaration=True)
    result = subprocess.run([str(engine), "-dont-run", str(path)], cwd=scratch,
                            text=True, capture_output=True, timeout=30)
    (scratch / "float64_probe.log").write_text(result.stdout + result.stderr, encoding="utf-8")
    return result.returncode == 0


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--engine", type=Path, default=Path(r"C:\Program Files\Dinamica EGO\DinamicaConsole.exe"))
    parser.add_argument("--interpreted", action="store_true")
    args = parser.parse_args()
    args.scratch.mkdir(parents=True, exist_ok=True)
    float64_supported = probe_float64(args.engine, args.scratch)
    values = [0.0, 1.0, -1.0, 0.1, -0.1, math.pi, -math.pi,
              3634714577.99164, 24947429.5194521, 2247736.430407,
              10571824.730664, 1e-30, 1e30, 1e-100, 1e100,
              sys.float_info.min, sys.float_info.max,
              math.nextafter(0.0, 1.0), math.nextafter(sys.float_info.min, 0.0)]
    for exponent in (-1000, -149, -100, -1, 0, 1, 26, 53, 100, 1000):
        boundary = math.ldexp(1.0, exponent)
        values.extend([math.nextafter(boundary, 0.0), boundary, math.nextafter(boundary, math.inf)])
    rng = random.Random(9182)
    values.extend(math.ldexp(rng.uniform(-1.0, 1.0), rng.randrange(-1022, 1024)) for _ in range(20))
    script = ET.Element("script")
    ET.SubElement(script, "property", key="dff.version", value="2.4.1.20140602")
    peers = []
    for index, value in enumerate(values):
        node = ET.SubElement(script, "containerfunctor", name="CalculateValue")
        ET.SubElement(node, "property", key="dff.functor.alias", value=f"Binary64 fixture {index + 1}")
        exponent = 0 if value == 0 else max(-1022, min(1023, math.frexp(abs(value))[1] - 1))
        scaled = math.ldexp(value, -exponent) * 67108864
        high = math.floor(scaled)
        low = int((scaled - high) * 134217728)
        expression = f"[ ({high} / 67108864 + {low} / (2 ^ 53)) * (2 ^ ({exponent})) ]"
        ET.SubElement(node, "inputport", name="expression").text = expression
        ET.SubElement(node, "inputport", name="defaultValue").text = ".none"
        peer = f"v{index + 1}"
        ET.SubElement(node, "outputport", name="result", id=peer)
        peers.append(peer)
    table_peer, _ = append_scalar_codec_table(script, peers, "Regression")
    save = ET.SubElement(script, "functor", name="SaveLookupTable")
    ET.SubElement(save, "property", key="dff.functor.alias", value="Save exact scalar integers")
    ET.SubElement(save, "inputport", name="table", peerid=table_peer)
    ET.SubElement(save, "inputport", name="filename").text = '"encoded.csv"'
    ET.SubElement(save, "inputport", name="suffixDigits").text = "0"
    ET.SubElement(save, "inputport", name="step").text = ".none"
    ET.SubElement(save, "inputport", name="workdir").text = ".none"
    ET.indent(script)
    model_path = args.scratch / "scalar_codec_fixture.egoml"
    ET.ElementTree(script).write(model_path, encoding="utf-8", xml_declaration=True)
    command = [str(args.engine), "-processors", "1", "-log-level", "3", str(model_path)]
    if args.interpreted:
        command.insert(1, "-disable-native-expressions")
    result = subprocess.run(command, cwd=args.scratch, text=True, capture_output=True, timeout=180)
    (args.scratch / "engine.log").write_text(result.stdout + result.stderr, encoding="utf-8")
    if result.returncode:
        raise RuntimeError(f"Dinamica scalar fixture failed: {result.returncode}\n{result.stdout}\n{result.stderr}")
    decoded = decode_scalar_table(args.scratch / "encoded.csv")
    if len(values) != len(decoded):
        raise AssertionError("Captured scalar count mismatch")
    records = [dict(index=index + 1, original=value, reconstructed=actual,
                    original_hex=bits(value), reconstructed_hex=bits(actual),
                    binary64_exact=bits(value) == bits(actual))
               for index, (value, actual) in enumerate(zip(values, decoded))]
    original_bytes = b"".join(struct.pack(">d", value) for value in values)
    decoded_bytes = b"".join(struct.pack(">d", value) for value in decoded)
    report = dict(case_count=len(values), all_binary64_exact=all(x["binary64_exact"] for x in records),
                  float64_raster_type_supported=float64_supported,
                  original_sha256=hashlib.sha256(original_bytes).hexdigest(),
                  decoded_sha256=hashlib.sha256(decoded_bytes).hexdigest(), cases=records)
    (args.scratch / "scalar_codec_report.json").write_text(json.dumps(report, indent=2), encoding="utf-8")
    print(json.dumps({k: v for k, v in report.items() if k != "cases"}, indent=2))
    assert report["all_binary64_exact"], [x for x in records if not x["binary64_exact"]]


if __name__ == "__main__":
    main()
