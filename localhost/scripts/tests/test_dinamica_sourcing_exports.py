"""Required annual sourcing exports, shared by Windows and Linux models.

Run with python3 -B -m unittest discover -s localhost/scripts/tests
  -p test_dinamica_sourcing_exports.py
Set MOFUSS_TEST_EGO to the native console executable to also exercise the
production SaveMap/filename nodes on disposable four-cell, two-MC data.
"""
import copy
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile
import unittest
import xml.etree.ElementTree as E

SCRIPTS = Path(__file__).resolve().parents[1]
REQUIRED = (
    "Proj_harv_Wtot", "Proj_harv_Vtot", "Proj_harv_Wdef", "Proj_harv_Vdef",
    "Non_harv_AGR", "Ex_agr_harv", "harv_AGR", "Expect_harv_tot", "Harvest_tot",
)


def normalized(node):
    return (node.tag, sorted(node.attrib.items()), (node.text or "").strip(),
            [normalized(child) for child in node])


def export_nodes(root, stem):
    names = [n for n in root.iter("containerfunctor") if n.get("name") == "CreateString"
             and n.findtext('inputport[@name="format"]') == f'"debugging_<v1>/{stem}.tif"']
    if len(names) != 1:
        raise AssertionError(f"Expected one MC filename for {stem}, found {len(names)}")
    peer = names[0].find("outputport").get("id")
    saves = [n for n in root.iter("functor") if n.get("name") == "SaveMap"
             and n.find(f'inputport[@name="filename"][@peerid="{peer}"]') is not None]
    if len(saves) != 1:
        raise AssertionError(f"Expected one SaveMap for {stem}, found {len(saves)}")
    return names[0], saves[0]


def annual_step(root, save):
    peer = save.find('inputport[@name="step"]').get("peerid")
    # The existing Linux Harvest_tot saver uses a Step wrapper around v39.
    producer = next((n for n in root.iter() if
                     n.find(f'outputport[@id="{peer}"]') is not None), None)
    if producer is not None and producer.get("name") == "Step":
        peer = producer.find('inputport[@name="step"]').get("peerid")
    return peer


class TestSourcingExports(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.windows = E.parse(SCRIPTS / "10_dyn_Sc17_webmofuss_ctrees_g_v13.egoml").getroot()
        cls.linux = E.parse(SCRIPTS / "10_dyn_Sc17_webmofuss_ctrees_g_v13_linux.egoml").getroot()

    def test_all_reader_maps_have_identical_windows_linux_exports(self):
        reader = (SCRIPTS / "postprocessing_sourcing/2post_runtime_sourcing_v1.R").read_text()
        block = re.search(r"\.rs_maps <- c\((.*?)\)", reader, re.S).group(1)
        self.assertEqual(set(re.findall(r'"([^"]+)"', block)), set(REQUIRED))
        for stem in REQUIRED:
            for win, linux in zip(export_nodes(self.windows, stem), export_nodes(self.linux, stem)):
                win, linux = copy.deepcopy(win), copy.deepcopy(linux)
                if win.get("name") == "SaveMap":
                    win.find('inputport[@name="step"]').set("peerid", annual_step(self.windows, win))
                    linux.find('inputport[@name="step"]').set("peerid", annual_step(self.linux, linux))
                self.assertEqual(normalized(win), normalized(linux), stem)

    def test_exports_run_in_every_mc_and_year_without_a_debug_switch(self):
        for root in (self.windows, self.linux):
            parents = {child: parent for parent in root.iter() for child in parent}
            for stem in REQUIRED:
                name, save = export_nodes(root, stem)
                self.assertEqual(name.find('functor/inputport[@name="value"]').get("peerid"), "v38")
                self.assertEqual(annual_step(root, save), "v39")
                self.assertEqual(save.findtext('inputport[@name="suffixDigits"]'), "2")
                ancestors = []
                current = save
                while current in parents:
                    current = parents[current]
                    if current.tag == "containerfunctor":
                        ancestors.append(current.get("name"))
                self.assertEqual(ancestors.count("Repeat"), 2, stem)
                self.assertTrue(all(kind in ("Group", "Repeat") for kind in ancestors), stem)

    def test_linux_graph_has_no_dangling_peers_or_duplicate_ids(self):
        ids = [n.get("id") for n in self.linux.iter() if n.get("id")]
        self.assertEqual(len(ids), len(set(ids)))
        for node in self.linux.iter():
            if node.get("peerid"):
                self.assertIn(node.get("peerid"), ids)

    @unittest.skipUnless(os.environ.get("MOFUSS_TEST_EGO"), "Set MOFUSS_TEST_EGO for native export test")
    def test_native_exports_preserve_pixels_and_mc_year_layout(self):
        from osgeo import gdal, osr
        sys.path.insert(0, str(SCRIPTS / "tools"))
        from build_dinamica_sourcing_v12 import node, port, calculate
        gdal.UseExceptions()
        with tempfile.TemporaryDirectory(prefix="mofuss_sourcing_exports_") as temporary:
            work = Path(temporary)
            source = work / "input.tif"
            crs = osr.SpatialReference(); crs.ImportFromEPSG(3395)
            ds = gdal.GetDriverByName("GTiff").Create(str(source), 2, 2, 1, gdal.GDT_Float32)
            ds.SetProjection(crs.ExportToWkt()); ds.SetGeoTransform((0, 1000, 0, 2000, 0, -1000))
            import numpy as np
            ds.GetRasterBand(1).SetNoDataValue(-9999)
            ds.GetRasterBand(1).WriteArray(np.array([[1, 2], [-9999, 4]], dtype="float32")); ds = None
            root = E.Element("script")
            E.SubElement(root, "property", key="dff.version", value="2.4.1.20140602")
            load = node(root, "LoadMap", "Fixture raster")
            for key, value in (("filename", '"input.tif"'), ("nullValue", ".none"),
                               ("loadAsSparse", ".no"), ("suffixDigits", "0"),
                               ("step", ".none"), ("workdir", ".none")):
                port(load, key, value)
            E.SubElement(load, "outputport", name="map", id="v9000")
            outer = node(root, "Repeat", "Two MC realizations", True)
            port(outer, "iterations", "2")
            E.SubElement(outer, "internaloutputport", name="step", id="v8")
            mc = node(outer, "Step", "MC index"); port(mc, "step", peer="v8")
            E.SubElement(mc, "outputport", name="step", id="v38")
            annual = node(outer, "Repeat", "Two years", True)
            port(annual, "iterations", "2")
            E.SubElement(annual, "internaloutputport", name="step", id="v39")
            year = node(annual, "Step", "Year index"); port(year, "step", peer="v39")
            E.SubElement(year, "outputport", name="step", id="v59")
            for index, stem in enumerate(REQUIRED):
                name, save = export_nodes(self.linux, stem)
                peer = save.find('inputport[@name="map"]').get("peerid")
                calculate(annual, "Map", "Fixture " + stem,
                          f"i1 + v1 * 100 + v2 * 10 + {index}", peer,
                          maps=("v9000",), values=("v38", "v59"))
                annual.extend((copy.deepcopy(name), copy.deepcopy(save)))
            for mc in (1, 2): (work / f"debugging_{mc}").mkdir()
            E.ElementTree(root).write(work / "exports.egoml", encoding="utf-8", xml_declaration=True)
            result = subprocess.run([os.environ["MOFUSS_TEST_EGO"], "-processors", "1",
                                     "-log-level", "3", str(work / "exports.egoml")],
                                    cwd=work, capture_output=True, text=True, timeout=90)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertEqual(len(list(work.glob("debugging_*/*.tif"))), len(REQUIRED) * 4)
            for mc in (1, 2):
                for year in (1, 2):
                    for index, stem in enumerate(REQUIRED):
                        ds = gdal.Open(str(work / f"debugging_{mc}/{stem}{year:02d}.tif"))
                        values = ds.ReadAsArray()
                        expected = np.array([1, 2, 4]) + mc * 100 + year * 10 + index
                        np.testing.assert_array_equal(values[[0, 0, 1], [0, 1, 1]], expected)
                        self.assertEqual(values[1, 0], ds.GetRasterBand(1).GetNoDataValue())
                        self.assertEqual(ds.GetGeoTransform(), (0, 1000, 0, 2000, 0, -1000))
                        self.assertTrue(crs.IsSame(osr.SpatialReference(wkt=ds.GetProjection())))
                        ds = None


if __name__ == "__main__":
    unittest.main()
