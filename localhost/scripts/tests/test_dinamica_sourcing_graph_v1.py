"""Static release contracts; complements, never replaces, real-engine tests."""
import sys
import unittest
from pathlib import Path
import xml.etree.ElementTree as E

SCRIPTS = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(SCRIPTS / "tools"))
from build_dinamica_sourcing_v12 import build
from dinamica_v12_transform import _producers, optimize_model


def normalized_xml(node):
    def nested(x):
        return (x.tag, tuple(sorted(x.attrib.items())), (x.text or "").strip(), tuple(nested(c) for c in x))
    return nested(node)


class TestSourcingGraph(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.source = (SCRIPTS / "10_dyn_Sc17_webmofuss_ctrees_g_v11.egoml").read_bytes().decode()
        optimized, _ = optimize_model(cls.source)
        cls.old = _producers(E.fromstring(optimized))
        cls.capture, _ = build(cls.source)
        cls.cached, _ = build(cls.source, static_cache=True)
        cls.corrected, _ = build(cls.source, domestic_tof=True, static_cache=True)

    def test_pure_observer_leaves_all_existing_leaf_producers_identical(self):
        new = _producers(E.fromstring(self.capture))
        for peer, node in self.old.items():
            if node.get("name") not in ("ForEach", "Repeat"):
                self.assertEqual(normalized_xml(node), normalized_xml(new[peer]), peer)

    def test_cache_retains_original_float32_arithmetic_and_order(self):
        new = _producers(E.fromstring(self.cached))
        for peer, node in self.old.items():
            if node.get("name") in ("ForEach", "Repeat"):
                continue
            if peer in ("v362", "v377"):
                replacement = new["v6000" if peer == "v362" else "v6010"]
                replacement.find("outputport").set("id", peer)
                self.assertEqual(normalized_xml(node), normalized_xml(replacement))
            else:
                self.assertEqual(normalized_xml(node), normalized_xml(new[peer]), peer)

    def test_no_original_science_node_consumes_observer_output(self):
        new = _producers(E.fromstring(self.capture))
        for peer, node in self.old.items():
            if node.get("name") in ("ForEach", "Repeat"):
                continue
            self.assertFalse(any(int(x.get("peerid")[1:]) >= 4000
                                 for x in new[peer].iter() if x.get("peerid")), peer)

    def test_masks_distinguish_zero_from_null(self):
        new = _producers(E.fromstring(self.capture))
        for peer in ("v4001", "v4021"):
            self.assertEqual(new[peer].find("inputport[@name='cellType']").text, ".uint8")
            self.assertEqual(new[peer].find("inputport[@name='nullValue']").text, "255")
            expression = new[peer].find("inputport[@name='expression']").text
            self.assertIn("if isNull(i1) then null", expression)
            self.assertIn("then 0 else i1 + 1", expression)

    def test_v13_only_changes_existing_redistribution_arithmetic(self):
        v12, v13 = _producers(E.fromstring(self.cached)), _producers(E.fromstring(self.corrected))
        for peer, node in v12.items():
            if peer == "v93" or node.get("name") in ("Repeat", "ForEach") or int(peer[1:]) >= 4000:
                continue
            self.assertEqual(normalized_xml(node), normalized_xml(v13[peer]), peer)
        self.assertIn("v5007", [x.get("peerid") for x in v13["v93"].iter()])
        self.assertEqual(v13["v5000"].find("inputport[@name='feedback']").get("peerid"), "v5007")
        self.assertIn("v364", [x.get("peerid") for x in v13["v5002"].iter()])
        self.assertIn("v366", [x.get("peerid") for x in v13["v5001"].iter()])
        self.assertIn("<= 0", v13["v5006"].find("inputport[@name='expression']").text)

    def test_all_original_save_nodes_are_identical(self):
        original = E.fromstring(self.source)
        old = [normalized_xml(n) for n in original.iter("functor") if n.get("name", "").startswith("Save")]
        for text in (self.capture, self.cached, self.corrected):
            new = [normalized_xml(n) for n in E.fromstring(text).iter("functor") if n.get("name", "").startswith("Save")]
            for expected in old:
                self.assertIn(expected, new)

    def test_no_dangling_peers_or_duplicate_outputs(self):
        for text in (self.capture, self.cached, self.corrected):
            root = E.fromstring(text)
            producers = _producers(root)
            for element in root.iter():
                if element.get("peerid"):
                    self.assertIn(element.get("peerid"), producers)


if __name__ == "__main__":
    unittest.main()
