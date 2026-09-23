"""Static safety contract for the conservative v12 speed transform.

Run directly with Python. Tests perform no simulations and write no files.
Runtime equivalence remains a separate paired Dinamica comparison.
"""

from __future__ import annotations

import importlib.util
from pathlib import Path
import sys
import unittest
import xml.etree.ElementTree as ET

sys.dont_write_bytecode = True
SCRIPT_ROOT = Path(__file__).resolve().parents[1]
MODULE_PATH = SCRIPT_ROOT / "tools" / "dinamica_v12_transform.py"
spec = importlib.util.spec_from_file_location("dinamica_v12_transform", MODULE_PATH)
assert spec is not None and spec.loader is not None
optimizer = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = optimizer
spec.loader.exec_module(optimizer)


def alias(node: ET.Element) -> str:
    return next((p.get("value", "") for p in node.findall("property")
                 if p.get("key") == "dff.functor.alias"), "")


def tree_signature(node: ET.Element, *, omit: set[str], flags: set[tuple[str, str]]) -> tuple:
    """Compare every retained XML element, attribute, text value, and order."""
    if node.tag in ("functor", "containerfunctor") and alias(node) in omit:
        return ()
    output_ids = {p.get("id", "") for p in node.findall("outputport")}
    children = []
    for child in node:
        if child.tag == "inputport" and any((key, child.get("name", "")) in flags for key in output_ids):
            children.append((child.tag, tuple(sorted(child.attrib.items())), ".no", ()))
        else:
            signature = tree_signature(child, omit=omit, flags=flags)
            if signature:
                children.append(signature)
    return node.tag, tuple(sorted(node.attrib.items())), (node.text or "").strip(), tuple(children)


class ConservativeTransformTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = (SCRIPT_ROOT / "10_dyn_Sc17_webmofuss_ctrees_g_v11.egoml").read_bytes().decode("utf-8")
        cls.result, cls.report = optimizer.optimize_model(cls.source)
        cls.before = ET.fromstring(cls.source)
        cls.after = ET.fromstring(cls.result)

    def test_only_approved_graph_changes(self) -> None:
        removed = {"extractMapAttributes2420", "extractMapAttributes242", "extractMapAttributes7205"}
        flags = {(key, "extractStatisticalAttributes")
                 for key in ("v23", "v27", "v128", "v129", "v311", "v312", "v275")}
        flags.add(("v275", "extractDynamicAttributes"))
        self.assertEqual(tree_signature(self.before, omit=removed, flags=flags),
                         tree_signature(self.after, omit=set(), flags=set()))
        self.assertEqual(len(self.report["flags_changed"]), 8)
        self.assertEqual(len(self.report["nodes_removed"]), 3)
        self.assertEqual(self.report["removed_output_ids"], ["v41", "v44"])

    def test_all_bytes_outside_reviewed_spans_are_preserved(self) -> None:
        old = self.source.encode("utf-8")
        new = self.result.encode("utf-8")
        old_cursor = new_cursor = 0
        for edit in self.report["byte_edits"]:
            start, end = edit["source_start"], edit["source_end"]
            unchanged_length = start - old_cursor
            self.assertEqual(old[old_cursor:start], new[new_cursor:new_cursor + unchanged_length])
            new_cursor += unchanged_length
            if edit["label"].startswith("remove/"):
                self.assertEqual(edit["replacement_bytes"], 0)
            else:
                self.assertEqual(old[start:end], b".yes")
                self.assertEqual(new[new_cursor:new_cursor + 3], b".no")
            new_cursor += edit["replacement_bytes"]
            old_cursor = end
        self.assertEqual(old[old_cursor:], new[new_cursor:])

    def test_remaining_expressions_storage_and_writers_are_identical(self) -> None:
        names = {"CalculateMap", "CalculateCategoricalMap", "CalculateValue", "SaveMap",
                 "SaveLookupTable", "SaveTable", "Patcher", "RunExternalProcess", "Repeat", "ForEach"}

        def snapshot(root: ET.Element) -> list:
            output = []
            for node in root.iter():
                if node.get("name") not in names:
                    continue
                ports = [(port.tag, tuple(sorted(port.attrib.items())), (port.text or "").strip())
                         for port in node if port.tag in ("inputport", "outputport", "internaloutputport")]
                output.append((node.get("name"), alias(node), ports))
            return output

        self.assertEqual(snapshot(self.before), snapshot(self.after))

    def test_external_consumer_prevents_dead_node_removal(self) -> None:
        for port in ("v41", "v44"):
            with self.subTest(port=port):
                extra = f'<functor name="Map"><inputport name="map" peerid="{port}" /></functor>'
                altered = self.source.replace("</script>", extra + "</script>")
                with self.assertRaisesRegex(optimizer.OptimizationContractError, "external consumer"):
                    optimizer.optimize_model(altered)

    def test_changed_identity_fails_closed(self) -> None:
        altered = self.source.replace('value="extractMapAttributes2420"', 'value="Changed calculation"', 1)
        with self.assertRaisesRegex(optimizer.OptimizationContractError, "identity"):
            optimizer.optimize_model(altered)

    def test_initial_stock_viewer_must_remain_output_free(self) -> None:
        marker = '<property key="dff.functor.alias" value="extractMapAttributes7205" />'
        altered = self.source.replace(marker, marker + '<outputport name="attributes" id="newStats" />', 1)
        with self.assertRaisesRegex(optimizer.OptimizationContractError, "output-free"):
            optimizer.optimize_model(altered)

    def test_unicode_and_crlf_are_preserved(self) -> None:
        altered = self.source.replace('value="VegDyn"', 'value="VegDyn – carbón 🌳"', 1)
        revised, _report = optimizer.optimize_model(altered)
        self.assertIn('value="VegDyn – carbón 🌳"', revised)
        self.assertEqual(revised.count("\r\n"), self.result.count("\r\n"))

    def test_reapplication_fails_instead_of_silently_drifting(self) -> None:
        with self.assertRaises(optimizer.OptimizationContractError):
            optimizer.optimize_model(self.result)


if __name__ == "__main__":
    unittest.main(verbosity=2)
