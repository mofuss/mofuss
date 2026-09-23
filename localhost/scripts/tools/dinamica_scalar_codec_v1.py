"""Lossless binary64 capture through Dinamica's limited-precision CSV writer.

Values are encoded as three small, exactly represented integers.  No model
state is changed; the generated nodes only consume scalar values.  Field j
uses lookup-table keys 3*j+1 (exponent), 3*j+2 (high), and 3*j+3 (low).
"""

from __future__ import annotations

import csv
import math
from pathlib import Path
import xml.etree.ElementTree as ET


def append_scalar_codec_table(
    parent: ET.Element,
    value_peers: list[str],
    prefix: str,
    first_id: int = 9000,
) -> tuple[str, int]:
    """Append observer nodes and return (lookup_table_peer, next_free_id).

    All input scalars must be finite.  Both signs, zero and subnormals are
    supported.  The CSV writer sees only integers of at most nine decimal digits.
    Negative zero is canonicalized to positive zero.
"""
    if not value_peers:
        raise ValueError("At least one scalar value is required")
    next_id = first_id

    def allocate() -> str:
        nonlocal next_id
        peer = f"v{next_id}"
        next_id += 1
        return peer

    def calculate(alias: str, expression: str, peers: list[str]) -> str:
        node = ET.SubElement(parent, "containerfunctor", name="CalculateValue")
        ET.SubElement(node, "property", key="dff.functor.alias", value=alias)
        ET.SubElement(node, "inputport", name="expression").text = f"[\n{expression}\n]"
        ET.SubElement(node, "inputport", name="defaultValue").text = ".none"
        peer = allocate()
        ET.SubElement(node, "outputport", name="result", id=peer)
        for number, upstream in enumerate(peers, 1):
            value = ET.SubElement(node, "functor", name="NumberValue")
            ET.SubElement(value, "property", key="dff.functor.alias", value=f"{alias} input {number}")
            ET.SubElement(value, "inputport", name="value", peerid=upstream)
            ET.SubElement(value, "inputport", name="valueNumber").text = str(number)
        return peer

    table_peer: str | None = None
    for index, value_peer in enumerate(value_peers):
        exponent = calculate(
            f"{prefix} scalar {index + 1} binary exponent",
            "if abs(v1) > 0 then max(-1022, min(1023, floor(ln(abs(v1)) / ln(2)))) else 0",
            [value_peer],
        )
        high = calculate(
            f"{prefix} scalar {index + 1} high mantissa",
            "if abs(v1) > 0 then floor(v1 * (2 ^ (-floor(v2 / 2))) * (2 ^ (floor(v2 / 2) - v2)) * 67108864) else 0",
            [value_peer, exponent],
        )
        low = calculate(
            f"{prefix} scalar {index + 1} low mantissa",
            "if abs(v1) > 0 then (v1 * (2 ^ (-floor(v2 / 2))) * (2 ^ (floor(v2 / 2) - v2)) * 67108864 - v3) * 134217728 else 0",
            [value_peer, exponent, high],
        )
        for offset, scalar_peer in enumerate((exponent, high, low), 1):
            node = ET.SubElement(parent, "functor", name="SetLookupTableValue")
            ET.SubElement(node, "property", key="dff.functor.alias", value=f"{prefix} exact scalar field {index * 3 + offset}")
            table_input = ET.SubElement(node, "inputport", name="table")
            if table_peer is None:
                table_input.text = '[\n"Key" "Value"\n]'
            else:
                table_input.set("peerid", table_peer)
            ET.SubElement(node, "inputport", name="key").text = str(index * 3 + offset)
            ET.SubElement(node, "inputport", name="value", peerid=scalar_peer)
            table_peer = allocate()
            ET.SubElement(node, "outputport", name="updatedTable", id=table_peer)
    assert table_peer is not None
    return table_peer, next_id


def decode_scalar_triplet(exponent: float, high: float, low: float) -> float:
    """Recover the original binary64 value without decimal rounding."""
    if any(not math.isfinite(x) or x != int(x) for x in (exponent, high, low)):
        raise ValueError("Scalar codec fields must be finite integers")
    if not -1022 <= exponent <= 1023:
        raise ValueError("Scalar codec exponent is out of range")
    if abs(high) > 268435456 or not 0 <= low < 134217728:
        raise ValueError("Scalar codec mantissa is out of range")
    return math.ldexp(high / 67108864 + low / 9007199254740992, int(exponent))


def decode_scalar_table(path: str | Path) -> list[float]:
    """Read Dinamica's two-column CSV, tolerating its trailing empty column."""
    with Path(path).open(encoding="utf-8-sig", newline="") as stream:
        rows = csv.reader(stream)
        next(rows)
        fields = {int(row[0].strip()): float(row[1].strip()) for row in rows if row and row[0].strip()}
    if sorted(fields) != list(range(1, len(fields) + 1)) or len(fields) % 3:
        raise ValueError("Incomplete or nonconsecutive scalar codec table")
    return [decode_scalar_triplet(fields[i], fields[i + 1], fields[i + 2]) for i in range(1, len(fields) + 1, 3)]
