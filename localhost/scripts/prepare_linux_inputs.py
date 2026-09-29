#!/usr/bin/env python3
"""Prepare legacy input metadata and sourcing components; never simulate."""
from pathlib import Path
import argparse
import csv
import datetime
import hashlib
import json
import os
import shutil
import subprocess
import xml.etree.ElementTree as ET

MODEL = '10_dyn_Sc17_webmofuss_ctrees_g_v13_linux.egoml'
LEGACY_CRS = ('Loss_00', 'Gain_00', 'Losses_calib', 'Loss_10', 'rivers_c_d',
              'roads_c_d', 'Gain_00_null', 'Gain_20', 'Loss_00_null')


def prepare(scenario, *, configure_model=False):
    from osgeo import gdal, osr
    gdal.UseExceptions()
    scenario = Path(scenario).resolve()
    stamp = datetime.datetime.now(datetime.timezone.utc).strftime('%Y%m%dT%H%M%S%fZ')
    backup = scenario / 'Logs/linux_input_backups' / stamp
    changes = []

    def preserve(path):
        target = backup / path.relative_to(scenario)
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(path, target)
        return target

    mask = scenario / 'LULCC/TempRaster/mask_c.tif'
    reference = gdal.Open(str(mask))
    target_crs = osr.SpatialReference(wkt=reference.GetProjection())
    mercator = osr.SpatialReference(); mercator.ImportFromEPSG(3395)
    target_crs.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    mercator.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    pending = []
    for name in LEGACY_CRS:
        path = scenario / f'LULCC/TempRaster/{name}.tif'
        if not path.exists(): continue  # Retired inputs may have been omitted.
        ds = gdal.Open(str(path))
        source_crs = osr.SpatialReference(wkt=ds.GetProjection())
        if not source_crs.IsLocal(): continue
        label = ''.join(ds.GetProjection().lower().split())
        if 'wgs84/worldmercator' not in label or not target_crs.IsSame(mercator):
            raise RuntimeError(f'CRS needs manual review: {path}')
        if ((ds.RasterXSize, ds.RasterYSize) != (reference.RasterXSize, reference.RasterYSize)
                or ds.GetGeoTransform() != reference.GetGeoTransform()):
            raise RuntimeError(f'Grid mismatch; cannot repair metadata alone: {path}')
        pending.append((path, ds.GetProjection(), hashlib.sha256(ds.ReadRaster()).hexdigest(),
                        ds.GetRasterBand(1).DataType, ds.GetRasterBand(1).GetNoDataValue()))
        ds = None
    for path, old_crs, cell_hash, dtype, nodata in pending:
        saved = preserve(path)
        # Break symlinks/hardlinks before an in-place GDAL metadata update.
        if path.is_symlink() or path.stat().st_nlink > 1:
            temporary = path.with_name(path.name + '.linux-copy')
            if temporary.exists(): raise RuntimeError(f'Stale staging file: {temporary}')
            shutil.copy2(saved, temporary); os.replace(temporary, path)
        ds = gdal.Open(str(path), gdal.GA_Update)
        ds.SetProjection(reference.GetProjection()); ds = None
        ds = gdal.Open(str(path))
        assert hashlib.sha256(ds.ReadRaster()).hexdigest() == cell_hash
        assert ds.GetGeoTransform() == reference.GetGeoTransform()
        assert ds.GetRasterBand(1).DataType == dtype
        assert ds.GetRasterBand(1).GetNoDataValue() == nodata
        changes.append({'file': str(path.relative_to(scenario)), 'operation': 'CRS metadata only',
                        'old_crs': old_crs, 'new_crs': 'EPSG:3395', 'cell_sha256': cell_hash})
        ds = None
    reference = None

    indexes = [scenario / f'In/DemandScenarios/{channel}_origin_component_index.csv' for channel in ('W', 'V')]
    if not all(p.is_file() for p in indexes):
        if any(p.exists() for p in indexes):
            raise RuntimeError('Partial sourcing input installation; review before replacing components.')
        installer = Path(__file__).resolve().parent / '9_install_directional_IDW_outputs_v4.R'
        expression = ('Sys.setenv(MOFUSS_6F_NO_AUTORUN="1"); source(' + json.dumps(str(installer)) +
                      '); install_directional_idw_outputs(run_root=' + json.dumps(str(scenario)) + ', dry_run=FALSE)')
        subprocess.run([os.environ.get('MOFUSS_R', 'R'), '--vanilla', '--slave', '-e', expression],
                       cwd=scenario, check=True)
        changes.append({'operation': 'Install missing sourcing components using the project installer'})

    if configure_model:
        from run_linux import settings
        config, _ = settings(scenario)
        scenario_name = config['scenario_ver'].lower()
        if scenario_name.startswith('bau'):
            desired = {'v256': '.yes', 'v261': '"BaU"'}
        elif scenario_name.startswith(('ics', 'ccts')):
            desired = {'v256': '.no', 'v261': '"ICS"'}
        else:
            raise RuntimeError(f'Unknown scenario role: {scenario_name}')
        path = scenario / MODEL
        tree = ET.parse(path)
        updates = []
        for e in tree.getroot().iter():
            for p in e.findall('outputport'):
                if p.get('id') in desired:
                    value = e.find('inputport[@name="constant"]')
                    if value is None or value.get('peerid'):
                        raise RuntimeError('Unexpected model scenario controls; review before editing.')
                    text = desired[p.get('id')]
                    if value.text != text:
                        updates.append({'port': p.get('id'), 'old': value.text, 'new': text})
                        value.text = text
        if updates:
            preserve(path)
            tree.write(path, encoding='utf-8', xml_declaration=True)
            changes.append({'operation': 'Align BAU/ICS controls with scenario parameters', 'controls': updates})

    result = {'scenario': str(scenario), 'simulation_executed': False, 'changes': changes,
              'backup': str(backup) if backup.exists() else None}
    (scenario / 'Logs').mkdir(exist_ok=True)
    (scenario / 'Logs' / f'linux_inputs_{stamp}.json').write_text(json.dumps(result, indent=2) + '\n')
    print(f'Linux inputs prepared: {len(changes)} changes; no simulation was run.')
    return result


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--scenario', type=Path, default=Path.cwd())
    parser.add_argument('--configure-model', action='store_true', help='Set BAU/ICS controls from scenario_ver; ICS reuses BAU draws.')
    args = parser.parse_args()
    prepare(args.scenario, configure_model=args.configure_model)
