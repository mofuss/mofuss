"""Static compatibility checks; native Dinamica execution is a separate gate."""
from pathlib import Path
import re
import sys
import unittest
import xml.etree.ElementTree as ET

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "tools"))
import build_webmofuss_fast as builder


def inventory(root, names):
    return sorted(ET.tostring(node, encoding="unicode").strip() for node in root.iter()
                  if node.get("name") in names)


def scope_map(root):
    parents = {id(child): parent for parent in root.iter() for child in parent}
    result = {}
    for node in root.iter():
        if node.get("name") not in ("SaveMap", "SaveTable", "SaveLookupTable", "RunExternalProcess"):
            continue
        scope, current = [], node
        while id(current) in parents:
            current = parents[id(current)]
            if current.tag == "containerfunctor":
                scope.append((current.get("name"), builder.alias(current)))
        result[(node.get("name"), builder.alias(node))] = scope
    return result


class WebMoFuSSFastContract(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.source = builder.DEFAULT_SOURCE.read_bytes().decode("utf-8")
        cls.text, cls.report = builder.build_candidate(cls.source)
        cls.original, cls.candidate = ET.fromstring(cls.source), ET.fromstring(cls.text)
        cls.before = builder._producers(cls.original)
        cls.after = builder._producers(cls.candidate)

    def test_shipped_candidate_exactly_matches_reviewed_builder(self):
        shipped = builder.DEFAULT_SOURCE.with_name(builder.DEFAULT_SOURCE.stem + "_fast.egoml")
        self.assertEqual(shipped.read_bytes(), self.text.encode("utf-8"))

    def test_io_external_calls_constants_and_metadata_are_unchanged(self):
        unchanged_names = {"SaveMap", "SaveTable", "SaveLookupTable", "RunExternalProcess",
                           "LoadMap", "LoadCategoricalMap", "LoadTable", "LoadLookupTable", "LoadWeights",
                           "Int", "PositiveInt", "Double", "Bool", "String", "Patcher"}
        self.assertEqual(inventory(self.original, unchanged_names), inventory(self.candidate, unchanged_names))
        self.assertEqual(scope_map(self.original), scope_map(self.candidate))
        self.assertEqual([ET.tostring(n) for n in self.original.findall("property")],
                         [ET.tostring(n) for n in self.candidate.findall("property")])
        # Wizard strings and other unmodified portions retain their exact bytes.
        cursor = 0
        for edit in self.report["byte_edits"]:
            segment = self.source.encode()[cursor:edit["source_start"]]
            self.assertIn(segment, self.text.encode())
            cursor = edit["source_end"]
        self.assertIn(self.source.encode()[cursor:], self.text.encode())

    def test_only_the_four_lookup_syntaxes_change_and_precision_is_unchanged(self):
        for output, before in self.before.items():
            after = self.after[output]
            old_expression = before.findtext("inputport[@name='expression']")
            new_expression = after.findtext("inputport[@name='expression']")
            if output in builder.TARGETS:
                expected = re.sub(r"t(\d+)\[\[v1\]\[i1\s*\+\s*1\]\]", r"t\1[i1 + 1]", old_expression)
                self.assertEqual(new_expression, expected)
            else:
                self.assertEqual(old_expression, new_expression)
            for field in ("cellType", "nullValue", "useCompression"):
                self.assertEqual(before.findtext(f"inputport[@name='{field}']"),
                                 after.findtext(f"inputport[@name='{field}']"))

    def test_helpers_refresh_once_per_mc_and_have_no_annual_scope(self):
        parents = {id(child): parent for parent in self.candidate.iter() for child in parent}
        additions = set(self.after) - set(self.before)
        self.assertEqual(additions, {f"v{i}" for i in range(92000, 92012)})
        for key in additions:
            self.assertIs(parents[id(self.after[key])], self.after["v8"])
        for row in self.report["lookup_optimization"]["selected_rows"]:
            selected = self.after[row["lookup_output"]]
            self.assertEqual(selected.findtext("inputport[@name='expression']"), "[t2[[v1][line]]]")
            self.assertEqual(builder.input_port(builder._hook(selected, "Value", 1), "value").get("peerid"), "v10")
            self.assertEqual(builder.input_port(builder._hook(selected, "Table", 2), "table").get("peerid"), row["source_table"])
        self.assertEqual({row["source_table"] for row in self.report["lookup_optimization"]["selected_rows"]},
                         {"v242", "v243", "v244"})

    def test_only_unused_attribute_flags_change_live_normalizers_remain(self):
        self.assertEqual(len(self.report["attribute_flags_changed"]), 8)
        for key in builder.STATISTICS_ONLY:
            self.assertEqual(self.after[key].findtext("inputport[@name='extractDynamicAttributes']"), ".yes")
            self.assertEqual(self.after[key].findtext("inputport[@name='extractStatisticalAttributes']"), ".no")
        for flag in ("extractDynamicAttributes", "extractStatisticalAttributes"):
            self.assertEqual(self.after["v275"].findtext(f"inputport[@name='{flag}']"), ".no")
        for key in ("v41", "v44"):
            self.assertEqual(ET.tostring(self.before[key]).strip(), ET.tostring(self.after[key]).strip())
        self.assertEqual(len([n for n in self.candidate.iter("functor")
                              if builder.alias(n) == "extractMapAttributes7205"]), 1)

    def test_all_peers_resolve_and_output_ids_remain_unique(self):
        self.assertTrue(all(node.get("peerid") in self.after for node in self.candidate.iter() if node.get("peerid")))
        ids = [node.get("id") for node in self.candidate.iter() if node.get("id")]
        self.assertEqual(len(ids), len(set(ids)))
        self.assertTrue(set(self.before).issubset(self.after))

    def test_unknown_or_altered_sources_fail_closed(self):
        variants = [self.source + "\n", self.source.replace('value="repeat775"', 'value="changed"', 1),
                    self.source.replace('peerid="v242"', 'peerid="v244"', 1), self.text]
        for source in variants:
            with self.subTest(source_length=len(source)):
                with self.assertRaisesRegex(builder.CandidateContractError, "Source SHA-256"):
                    builder.build_candidate(source)

    def test_final_mc_debug_gating_is_explicit_and_preserves_all_writer_nodes(self):
        self.assertFalse(self.report["last_mc_debugging"])
        self.assertNotIn(builder.DEBUG_CONDITION_ID, self.after)
        text, report = builder.build_candidate(self.source, last_mc_debugging=True)
        root = ET.fromstring(text)
        prods = builder._producers(root)
        parents = {id(child): parent for parent in root.iter() for child in parent}
        self.assertEqual(len(report["last_mc_debugging_writers"]), 25)
        self.assertEqual(inventory(self.original, {"SaveMap", "SaveTable", "SaveLookupTable"}),
                         inventory(root, {"SaveMap", "SaveTable", "SaveLookupTable"}))
        condition = prods[builder.DEBUG_CONDITION_ID]
        self.assertIs(parents[id(condition)], prods["v8"])
        self.assertEqual(condition.findtext("inputport[@name='expression']"), "[v1 = v2]")
        self.assertEqual([p.get("peerid") for p in condition.iter("inputport") if p.get("peerid")], ["v8", "v282"])
        for node in root.iter("functor"):
            if node.get("name") == "SaveMap" and node.findtext("inputport[@name='filename']", "").startswith('"Debugging/'):
                parent = parents[id(node)]
                self.assertEqual(parent.get("name"), "IfThen")
                self.assertEqual(builder.input_port(parent, "condition").get("peerid"), builder.DEBUG_CONDITION_ID)
        self.assertTrue(all(node.get("peerid") in prods for node in root.iter() if node.get("peerid")))


if __name__ == "__main__":
    unittest.main()
