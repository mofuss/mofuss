"""Structural regression for the observational v14 NRB attribution contract."""
from __future__ import annotations

import copy
from pathlib import Path
import sys
import unittest
import xml.etree.ElementTree as E

HERE = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(HERE / "tools"))
from build_woodman_dinamica_v14 import build_model, SOURCE, TARGET
from dinamica_v12_transform import _producers
from fix_woodman_nrb_attribution import (
    CONTRACT, MARKER_KEY, NEW_IDS, LEDGER_IDS, LEDGER_NULL, correct_nrb_attribution,
)


def signature(element):
    """Compare XML values without incidental whitespace serialization."""
    return (element.tag, tuple(sorted(element.attrib.items())),
            " ".join((element.text or "").split()), tuple(signature(c) for c in element))


class NrbAttributionGraphTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.reference = build_model(SOURCE.read_text(encoding="utf-8"), nrb_attribution=False)
        cls.corrected, cls.report = correct_nrb_attribution(cls.reference)

    def test_generated_model_and_idempotence(self):
        target = TARGET.read_text(encoding="utf-8")
        self.assertEqual(build_model(SOURCE.read_text(encoding="utf-8")), target)
        # Applying two independent patchers in reverse order changes only the
        # order of their root metadata properties, which has no graph meaning.
        roots = [E.fromstring(value) for value in (self.corrected, target)]
        for root in roots:
            properties = sorted(root.findall("property"), key=lambda p: p.get("key"))
            for item in properties:
                root.remove(item)
            for index, item in enumerate(properties):
                root.insert(index, item)
        self.assertEqual(signature(roots[0]), signature(roots[1]))
        same, report = correct_nrb_attribution(self.corrected)
        self.assertEqual(same, self.corrected)
        self.assertTrue(report["already_applied"])
        self.assertEqual(report["contract"], CONTRACT)

    def test_only_observational_nodes_change(self):
        before = E.fromstring(self.reference)
        after = E.fromstring(self.corrected)
        # Strip exactly the new observer nodes and replace the five accounting
        # calculators. The whole original model, including every non-producer,
        # must then match, proving growth/harvest/sourcing/stock identity.
        old = _producers(before)
        new = _producers(after)
        parents = {child: parent for parent in after.iter() for child in parent}
        for ident in self.report["changed_producer_ids"]:
            entry = new[ident]
            parent = parents[entry]
            index = list(parent).index(entry)
            parent.remove(entry)
            parent.insert(index, copy.deepcopy(old[ident]))
        for ident in NEW_IDS:
            parents[new[ident]].remove(new[ident])
        for writer in list(after.iter("functor")):
            filename = writer.find("inputport[@name='filename']")
            if writer.get("name") == "SaveMap" and filename is not None and filename.get("peerid") == "v93004":
                parents[writer].remove(writer)
        after.remove(after.find(f"property[@key='{MARKER_KEY}']"))
        self.assertEqual(signature(before), signature(after))

    def test_marker_does_not_hide_incomplete_correction(self):
        broken = self.corrected.replace("i1 + min(i2, i3) - i4", "i1 + i2 - i4", 1)
        with self.assertRaisesRegex(ValueError, "calculation changed"):
            correct_nrb_attribution(broken)

    def test_legacy_signed_nodata_migration_changes_only_three_literals(self):
        safe = f'<inputport name="nullValue">{LEDGER_NULL}</inputport>'
        legacy = '<inputport name="nullValue">.default</inputport>'
        self.assertEqual(self.corrected.count(safe), 3)
        old = self.corrected.replace(safe, legacy)
        migrated, report = correct_nrb_attribution(old)
        self.assertEqual(migrated, self.corrected)
        self.assertTrue(report["ledger_null_migrated"])
        self.assertFalse(report["already_applied"])
        self.assertEqual(report["changed_producer_ids"], list(LEDGER_IDS))
        self.assertEqual(report["balance_null_value"], LEDGER_NULL)
        # Partial safe migrations remain idempotently repairable.
        partial = self.corrected.replace(safe, legacy, 1)
        self.assertEqual(correct_nrb_attribution(partial)[0], self.corrected)

    def test_unreviewed_signed_nodata_is_rejected(self):
        changed = self.corrected.replace(
            f'<inputport name="nullValue">{LEDGER_NULL}</inputport>',
            '<inputport name="nullValue">-9998</inputport>', 1)
        with self.assertRaisesRegex(ValueError, "NoData encoding changed"):
            correct_nrb_attribution(changed)

    def test_legacy_encoding_does_not_bypass_graph_validation(self):
        old = self.corrected.replace(
            f'<inputport name="nullValue">{LEDGER_NULL}</inputport>',
            '<inputport name="nullValue">.default</inputport>')
        old = old.replace("i1 + min(i2, i3) - i4", "i1 + i2 - i4", 1)
        with self.assertRaisesRegex(ValueError, "calculation changed"):
            correct_nrb_attribution(old)

    def test_unreviewed_source_is_rejected(self):
        source = E.fromstring(self.reference)
        _producers(source)["v193"].find("inputport[@name='expression']").text = "[i2 - i1]"
        with self.assertRaisesRegex(ValueError, "Unexpected uncorrected NRB expression"):
            correct_nrb_attribution(E.tostring(source, encoding="unicode"))

    def test_no_balance_output_feeds_physical_mechanics(self):
        source = E.fromstring(self.corrected)
        allowed = {"v193", "v93001", "v93002", "v93003"}
        producers = _producers(source)
        for ident, element in producers.items():
            # Container outputs also own full nested physical graphs; inspect
            # only direct map carriers and direct MuxMap inputs here.
            references = {p.get("peerid") for p in element.findall("inputport")}
            references.update(p.get("peerid") for n in element.findall("functor")
                              if n.get("name") == "NumberMap" for p in n.findall("inputport"))
            if references.intersection(NEW_IDS[:4]):
                self.assertIn(ident, allowed)

    def test_deforestation_history_only_feeds_accounting(self):
        source = E.fromstring(self.corrected)
        parents = {child: parent for parent in source.iter() for child in parent}
        for entry in source.iter("inputport"):
            if entry.get("peerid") not in ("v117", "v77"):
                continue
            owner = parents[entry]
            if owner.get("name") == "NumberMap":
                owner = parents[owner]
            outputs = [port.get("id") for port in owner.findall("outputport")]
            self.assertTrue(owner.get("name") == "SaveMap" or
                            outputs in (["v77"], ["v117"], ["v193"]))


if __name__ == "__main__":
    unittest.main()
