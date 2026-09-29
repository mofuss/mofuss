"""Check metadata-only repairs and BAU/ICS configuration without simulations."""
from pathlib import Path
import csv
import hashlib
import sys
import tempfile
import unittest
import xml.etree.ElementTree as ET
from osgeo import gdal, osr

SCRIPTS = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(SCRIPTS))
from prepare_linux_inputs import prepare, MODEL


class PrepareInputsTest(unittest.TestCase):
    def fixture(self, root, label='WGS 84 / World Mercator'):
        for d in ('LULCC/TempRaster', 'LULCC/TempTables', 'In/DemandScenarios',
                  'LULCC/DownloadedDatasets/SourceDataGlobal'):
            (root / d).mkdir(parents=True)
        crs = osr.SpatialReference(); crs.ImportFromEPSG(3395)
        for name, wkt in [('mask_c', crs.ExportToWkt()),
                          ('Loss_00', f'LOCAL_CS["{label}",UNIT["metre",1]]')]:
            ds = gdal.GetDriverByName('GTiff').Create(str(root / f'LULCC/TempRaster/{name}.tif'), 2, 2, 1, gdal.GDT_Byte)
            ds.SetGeoTransform((1000, 1000, 0, -1000, 0, -1000)); ds.SetProjection(wkt)
            ds.GetRasterBand(1).SetNoDataValue(255); ds.WriteRaster(0, 0, 2, 2, bytes([0, 1, 255, 2])); ds = None
        for channel in ('W', 'V'):
            (root / f'In/DemandScenarios/{channel}_origin_component_index.csv').write_text('ComponentIndex\n1\n')
        (root / 'LULCC/TempTables/Country.csv').write_text('Key.,Country\n1,Global\n')
        params = 'Var,ParCHR\nstart_year,2000\nend_year,2050\nmonte_carlo_runs,3\nuncapped_regrowth,1\nnpa_ease,10\n'
        (root / 'LULCC/TempTables/parameters_dinamica.csv').write_text(params)
        (root / 'LULCC/DownloadedDatasets/SourceDataGlobal/parameters.csv').write_text(params + 'scenario_ver,ICS3_v2\n')
        (root / MODEL).write_bytes((SCRIPTS / MODEL).read_bytes())

    def test_repair_preserves_pixels_and_only_changes_two_role_controls(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory); self.fixture(root)
            path = root / 'LULCC/TempRaster/Loss_00.tif'
            original = path.read_bytes()
            params = {p: p.read_bytes() for p in root.rglob('parameters*.csv')}
            before = ET.parse(root / MODEL)
            result = prepare(root, configure_model=True)
            ds = gdal.Open(str(path))
            self.assertEqual(ds.ReadRaster(), bytes([0, 1, 255, 2]))
            self.assertEqual(ds.GetRasterBand(1).GetNoDataValue(), 255)
            self.assertFalse(osr.SpatialReference(wkt=ds.GetProjection()).IsLocal()); ds = None
            self.assertEqual((Path(result['backup']) / path.relative_to(root)).read_bytes(), original)
            self.assertEqual({p: p.read_bytes() for p in params}, params)
            expected = {'v256': '.no', 'v261': '"ICS"'}
            for e in before.getroot().iter():
                for p in e.findall('outputport'):
                    if p.get('id') in expected:
                        e.find('inputport[@name="constant"]').text = expected[p.get('id')]
            self.assertEqual(ET.tostring(before.getroot()), ET.tostring(ET.parse(root / MODEL).getroot()))
            self.assertEqual(prepare(root, configure_model=True)['changes'], [])
            self.assertFalse((root / 'Temp').exists())

    def test_unknown_local_crs_is_rejected_before_editing(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory); self.fixture(root, label='Unknown coordinates')
            path = root / 'LULCC/TempRaster/Loss_00.tif'; original = path.read_bytes()
            with self.assertRaisesRegex(RuntimeError, 'CRS needs manual review'):
                prepare(root)
            self.assertEqual(path.read_bytes(), original)


if __name__ == '__main__':
    unittest.main()
