"""Protect the performance transform's complete-output and graph contracts."""
from pathlib import Path
import sys
import unittest
import xml.etree.ElementTree as ET

SCRIPTS = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(SCRIPTS / "tools"))
from optimize_windows_io import optimize_io, MARKER
from dinamica_v12_transform import _producers


class WindowsIOContract(unittest.TestCase):
    def test_numerical_expressions_and_all_writers_are_preserved(self):
        for version in (13, 14):
            source = (SCRIPTS / f"10_dyn_Sc17_webmofuss_ctrees_g_v{version}.egoml").read_text(encoding="utf-8")
            # Works before and after deploying the idempotent transform.
            result, _ = optimize_io(source)
            before, after = ET.fromstring(source), ET.fromstring(result)
            expr = lambda r: {k: n.findtext("inputport[@name='expression']")
                              for k, n in _producers(r).items()
                              if k != "v92050" and n.find("inputport[@name='expression']") is not None}
            self.assertEqual(expr(before), expr(after))
            writers = lambda r: sorted(ET.tostring(n, encoding="unicode").strip()
                                       for n in r.iter("functor")
                                       if n.get("name") in ("SaveMap", "SaveTable", "SaveLookupTable"))
            self.assertEqual(writers(before), writers(after))
            self.assertEqual(optimize_io(result)[0], result)

    def test_immutable_inputs_and_final_mc_scope(self):
        for version in (13, 14):
            source = (SCRIPTS / f"10_dyn_Sc17_webmofuss_ctrees_g_v{version}.egoml").read_text(encoding="utf-8")
            root = ET.fromstring(optimize_io(source)[0])
            ids = _producers(root)
            parents = {id(c): n for n in root.iter() for c in n}
            for key in ("v172", "v173", "v174"):
                self.assertEqual(parents[id(parents[id(ids[key])])], ids["v8"])
                self.assertEqual(ids[key].findtext("inputport[@name='step']"), ".none")
            self.assertEqual(ids["v92050"].findtext("inputport[@name='expression']"), "[v1 = v2]")
            self.assertEqual([p.get('peerid') for p in ids['v92050'].iter('inputport') if p.get('peerid')], ['v8', 'v282'])
            writers = [n for n in root.iter('functor') if n.get('name') == 'SaveMap'
                       and (n.findtext("inputport[@name='filename']") or '').startswith('"Debugging/')]
            self.assertEqual(len(writers), 17)
            self.assertTrue(all(parents[id(n)].find("inputport[@name='condition']").get('peerid') == 'v92050'
                                for n in writers))


if __name__ == '__main__':
    unittest.main()
