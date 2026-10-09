"""Contracts for the optional Woodman freeze-year graph transformation."""
from __future__ import annotations
import sys
from pathlib import Path
import unittest
import xml.etree.ElementTree as E

sys.dont_write_bytecode = True

HERE = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(HERE / "tools"))
from add_woodman_freeze_year import (add_freeze_year, validate_freeze_graph,
                                   validate_root_container_dependencies)
from build_woodman_dinamica_v14 import build_model, SOURCE, TARGET


class WoodmanFreezeYearGraphTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.legacy = build_model(SOURCE.read_text(encoding="utf-8"), freeze_year=False)
        cls.updated, cls.report = add_freeze_year(cls.legacy)

    def test_production_reproduces_and_patch_is_idempotent(self):
        self.assertEqual(self.updated, TARGET.read_text(encoding="utf-8"))
        unchanged, report = add_freeze_year(self.updated)
        self.assertEqual(unchanged, self.updated)
        self.assertTrue(report["already_applied"])

    def test_contract_cannot_hide_a_replayed_transition(self):
        broken = self.updated.replace("else if v1 = 3 and v2 &gt; v3 then 0 ", "", 1)
        self.assertNotEqual(broken, self.updated)
        with self.assertRaisesRegex(ValueError, "transition suppression"):
            add_freeze_year(broken)

    def test_contract_rejects_an_accidentally_frozen_demand_clock(self):
        broken = self.updated.replace("[v1 + v2 - 1]", "[min(v1 + v2 - 1, 2026)]", 1)
        with self.assertRaisesRegex(ValueError, "calendar"):
            add_freeze_year(broken)

    def test_parameter_is_optional_and_key_based(self):
        validate_freeze_graph(E.fromstring(self.updated))
        self.assertEqual(self.report["default"], 2050)
        self.assertEqual(self.report["valid_years"], [2000, 2050])
        self.assertTrue(self.report["freeze_year_is_inclusive"])

    def misplaced_parameter_fixture(self):
        root = E.fromstring(self.updated)
        parents = {child: parent for parent in root.iter() for child in parent}
        producers = {p.get("id"): n for n in root.iter() for p in n.findall("outputport")}
        # Reproduce the original release error: upstream LUC inputs read the
        # downstream parameter Group, which already reads the LUC inputs.
        upstream = parents[producers["v302"]]
        for ident in ("v94000", "v94001", "v94002"):
            entry = producers[ident]
            parents[entry].remove(entry)
            upstream.append(entry)
        return root

    def test_full_graph_container_cycle_is_rejected(self):
        with self.assertRaisesRegex(ValueError, "Cyclic root container dependencies"):
            validate_root_container_dependencies(self.misplaced_parameter_fixture())

    def test_existing_misplaced_release_is_repaired_without_numerical_changes(self):
        misplaced = E.tostring(self.misplaced_parameter_fixture(), encoding="unicode")
        repaired, report = add_freeze_year(misplaced)
        self.assertTrue(report["parameter_container_repaired"])
        self.assertTrue(report["equations_unchanged"])
        validate_freeze_graph(E.fromstring(repaired))

        def signature(element):
            return (element.tag, tuple(sorted(element.attrib.items())),
                    " ".join((element.text or "").split()), tuple(signature(c) for c in element))

        self.assertEqual(signature(E.fromstring(repaired)), signature(E.fromstring(self.updated)))


if __name__ == "__main__":
    unittest.main()
