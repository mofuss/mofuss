#!/usr/bin/env python3
"""Run a MoFuSS scenario in its existing folder, or check it without running."""
from pathlib import Path
import argparse
import csv
import datetime
import fcntl
import hashlib
import json
import os
import re
import resource
import shutil
import subprocess
import time
import xml.etree.ElementTree as ET


def cpu_capacity():
    """Physical cores within affinity, further limited by scheduler/cgroup quotas."""
    logical = sorted(os.sched_getaffinity(0)) if hasattr(os, 'sched_getaffinity') else list(range(os.cpu_count() or 1))
    cores = set()
    for cpu in logical:
        topology = Path(f'/sys/devices/system/cpu/cpu{cpu}/topology')
        try:
            cores.add(((topology / 'physical_package_id').read_text().strip(),
                       (topology / 'core_id').read_text().strip()))
        except OSError:
            cores.add(('unknown', str(cpu)))
    limits = [len(logical)]
    scheduler = os.environ.get('SLURM_CPUS_PER_TASK')
    if scheduler and scheduler.isdigit():
        limits.append(max(1, int(scheduler)))
    # Follow the current process's cgroup and its ancestors, including when
    # the cgroup namespace exposes the current group at the mount root.
    cgroup_limits = []
    try:
        for line in Path('/proc/self/cgroup').read_text().splitlines():
            group, controllers, relative = line.split(':', 2)
            if group == '0':
                mount = Path('/sys/fs/cgroup')
                leaf = mount / relative.lstrip('/')
                candidates = [mount, leaf, *[p for p in leaf.parents if p != mount and mount in p.parents]]
                for base in candidates:
                    path = base / 'cpu.max'
                    if path.is_file():
                        quota, period = path.read_text().split()
                        if quota != 'max': cgroup_limits.append(max(1, int(quota) // int(period)))
            elif 'cpu' in controllers.split(','):
                for mount in (Path('/sys/fs/cgroup/cpu'), Path('/sys/fs/cgroup/cpu,cpuacct')):
                    base = mount / relative.lstrip('/')
                    for p in (mount, base, *[a for a in base.parents if mount in a.parents]):
                        try:
                            quota = int((p / 'cpu.cfs_quota_us').read_text())
                            period = int((p / 'cpu.cfs_period_us').read_text())
                            if quota > 0: cgroup_limits.append(max(1, quota // period))
                        except OSError:
                            pass
    except (OSError, ValueError):
        pass
    limits.extend(cgroup_limits)
    allowed = max(1, min(limits))
    return {'physical_cores': len(cores), 'logical_cpus': len(logical),
            'allowed_workers': allowed, 'auto_workers': max(1, min(len(cores), allowed)),
            'affinity': logical, 'scheduler_cpus_per_task': scheduler,
            'cgroup_worker_limits': sorted(set(cgroup_limits))}


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def settings(scenario):
    def table(path):
        with path.open(newline='') as stream:
            return {row['Var']: row['ParCHR'] for row in csv.DictReader(stream)}
    config_path = scenario / 'LULCC/TempTables/parameters_dinamica.csv'
    values = table(config_path)
    with (scenario / 'LULCC/TempTables/Country.csv').open(newline='') as stream:
        countries = [row['Country'] for row in csv.DictReader(stream) if row['Key.'] == '1']
    if len(countries) != 1:
        raise SystemExit('Country.csv must have exactly one country at Key.=1.')
    original_path = scenario / f'LULCC/DownloadedDatasets/SourceData{countries[0]}/parameters.csv'
    full = table(original_path)
    for key, value in values.items():
        if key in full and full[key] != value:
            raise SystemExit(f'Conflicting scenario settings for {key}: {value} versus {full[key]}')
    config = {key: int(values[key]) for key in
              ('start_year', 'end_year', 'monte_carlo_runs', 'uncapped_regrowth', 'npa_ease')}
    config['scenario_ver'] = full['scenario_ver']
    if config['monte_carlo_runs'] < 1 or config['end_year'] < config['start_year']:
        raise SystemExit('Invalid MC count or year range.')
    return config, {str(p.relative_to(scenario)): sha(p) for p in (config_path, original_path)}


MODEL = '10_dyn_Sc17_webmofuss_ctrees_g_v13_linux.egoml'
LEGACY_CRS = ('Loss_00', 'Gain_00', 'Losses_calib', 'Loss_10', 'rivers_c_d',
              'roads_c_d', 'Gain_00_null', 'Gain_20', 'Loss_00_null')


def prepare(scenario, *, configure_model=False):
    try:
        from osgeo import gdal, osr
    except ImportError:
        raise SystemExit("Python GDAL is missing. Install the native Python 3 GDAL bindings on this Linux computer.")
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

    if configure_model:
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



def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--scenario', type=Path, default=Path(__file__).resolve().parent)
    parser.add_argument('--processors', default='auto', help='Worker count, or auto (physical cores within allocation).')
    parser.add_argument('--check', action='store_true', help='Prepare compatibility metadata and validate readiness; do not simulate.')
    parser.add_argument('--log', type=Path)
    args = parser.parse_args()
    capacity = cpu_capacity()
    if args.processors == 'auto':
        args.processors = capacity['auto_workers']
    else:
        try:
            args.processors = int(args.processors)
        except ValueError:
            parser.error('processors must be auto or a positive integer')
    if not 1 <= args.processors <= capacity['allowed_workers']:
        parser.error(f'processors must be between 1 and the available allocation ({capacity["allowed_workers"]})')
    scenario = args.scenario.resolve()
    model = scenario / '10_dyn_Sc17_webmofuss_ctrees_g_v13_linux.egoml'
    if not os.environ.get('MOFUSS_EGO'):
        parser.error('MOFUSS_EGO is unset; invoke run_linux.sh or set MOFUSS_EGO to the installed Dinamica executable.')
    executable = Path(os.environ['MOFUSS_EGO']).resolve()
    wrapper = scenario / 'mofuss_r_linux.sh'
    for path in (model, executable, wrapper):
        if not path.is_file():
            raise SystemExit(f'Required file not found: {path}')
    config, config_hashes = settings(scenario)
    root = ET.parse(model).getroot()
    il_node = next(e for e in root.iter() if any(p.get('id') == 'v249' for p in e.findall('outputport')))
    il = float(il_node.find('inputport[@name="constant"]').text)
    seed = os.environ.get('MOFUSS_SEED', '')
    if seed and (not re.fullmatch(r'-?\d+', seed) or not -2147483647 <= int(seed) <= 2147483647):
        raise SystemExit('MOFUSS_SEED must be an R integer between -2147483647 and 2147483647.')
    dpi = os.environ.get('MOFUSS_PLOT_DPI', '1000')
    if not re.fullmatch(r'\d+', dpi) or not 72 <= int(dpi) <= 2147483647:
        raise SystemExit('MOFUSS_PLOT_DPI must be an integer of at least 72.')
    logs = scenario / 'Logs'
    logs.mkdir(exist_ok=True)
    lock = (logs / '.linux_run.lock').open('a')
    try:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except BlockingIOError:
        raise SystemExit('This scenario is already running through run_linux.sh.')
    # Script 9 installs directional inputs. This launcher never installs IDW.
    indexes = [scenario / f'In/DemandScenarios/{channel}_origin_component_index.csv'
               for channel in ('W', 'V')]
    if not all(p.is_file() for p in indexes):
        raise SystemExit('Directional IDW inputs are not installed. Run 9_install_directional_IDW_outputs_v4.R first.')
    prepare(scenario, configure_model=True)
    root = ET.parse(model).getroot()
    producers = {p.get('id'): e for e in root.iter() for p in e.findall('outputport')}
    def constant(peer):
        return producers[peer].find('inputport[@name="constant"]').text.strip()
    is_ics = config['scenario_ver'].lower().startswith(('ics', 'ccts'))
    expected_role, expected_mc = ('"ICS"', '.no') if is_ics else ('"BaU"', '.yes')
    if constant('v261') != expected_role or constant('v256') != expected_mc:
        raise SystemExit('Model BAU/ICS controls do not match scenario_ver after automatic configuration.')
    stamp = datetime.datetime.now(datetime.timezone.utc).strftime('%Y%m%dT%H%M%S%fZ')
    mode = 'check' if args.check else 'run'
    invocation = Path(os.environ.get('MOFUSS_RUN_FROM', os.getcwd()))
    log = (invocation / args.log).resolve() if args.log else logs / f'linux_{mode}_{stamp}.log'
    log.parent.mkdir(parents=True, exist_ok=True)
    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
    checks = []
    start = time.monotonic()
    print(f'{"Checking" if args.check else "Running"} {model.name}\nFolder: {scenario}\n'
          f'MC: {config["monte_carlo_runs"]}; years: {config["start_year"]}–{config["end_year"]}; '
          f'uncapped_regrowth: {config["uncapped_regrowth"]}\n'
          f'EGO workers: {args.processors} ({capacity["physical_cores"]} physical, '
          f'{capacity["logical_cpus"]} logical CPUs visible)\nLog: {log}', flush=True)
    with log.open('w') as output:
        def execute(command, label, success_text=None):
            output.write(f'\n--- {label} ---\n'); output.flush()
            child = subprocess.Popen(command, cwd=scenario, stdout=subprocess.PIPE,
                                     stderr=subprocess.STDOUT, text=True, errors='replace',
                                     start_new_session=True)
            completed = success_text is None
            failed = False
            try:
                for line in child.stdout:
                    clean = re.sub(r'\x1b\[[0-9;]*m', '', line)
                    output.write(clean); output.flush()
                    print(clean, end='', flush=True)
                    completed |= success_text is not None and success_text in clean
                    failed |= 'Model script failed' in clean or 'Execution halted' in clean
                status = child.wait()
            except KeyboardInterrupt:
                import signal
                os.killpg(child.pid, signal.SIGTERM)
                child.wait()
                raise SystemExit(130)
            passed = status == 0 and completed and not failed
            checks.append({'check': label, 'passed': passed, 'process_status': status})
            return passed

        # Read and parse scripts, load dependencies, then run the source's own
        # DryRun preflight. It returns before any initialization or MC draw.
        r_code = '''
files <- c(list.files(pattern="[.]R$"), list.files("LaTeX", pattern="[.]R$", full.names=TRUE))
for (f in files) invisible(parse(file=f))
packages <- c("msm", "raster", "tidyverse", "readxl", "readr", "tibble", "animation",
              "data.table", "foreach", "jpeg", "png", "sf", "tiff")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing)) stop("Missing R packages: ", paste(missing, collapse=", "))
if (!capabilities("cairo")) stop("R was built without Cairo graphics.")
for (name in c("ffmpeg", "pdflatex", "zip")) {
  if (!nzchar(Sys.which(name))) stop("Missing report executable: ", name)
}
for (asset in c("lmodern.sty", "ec-lmr10.tfm", "lmr10.pfb")) {
  font <- system2("kpsewhich", asset, stdout=TRUE)
  if (!length(font)) stop("Latin Modern TeX asset unavailable: ", asset)
}
cat(sprintf("[OK] Parsed %d R scripts; R packages and report tools available.\\n", length(files)))
'''
        ok = execute([str(wrapper), '--vanilla', '--slave', '-e', r_code], 'R and report dependencies')
        if ok:
            script = 'bypassMC_v8.R' if is_ics else 'rnorm_v8.R'
            arguments = ['DryRun=1', f'MC={config["monte_carlo_runs"]}',
                         f'IT={config["start_year"]}', f'IL={il}',
                         f'STdyn={config["end_year"] - config["start_year"]}']
            if is_ics:
                arguments.extend(['RerunMC=0', f'LUCmap_v={constant("v302")}',
                                  f'AGBmap_v={constant("v280")}',
                                  'PatcherBypassed=' + ('1' if constant('v313') == '.yes' else '0')])
            ok = execute([str(wrapper), '--vanilla', '--slave', f'--file={script}', '--args', *arguments],
                         'R input preflight', '[DRY-RUN]')
        # Check installed directional inputs and literal input paths without
        # interpreting simulation expressions or touching generated products.
        if ok:
            missing = []
            skipped = []
            constants = {}
            for e in root.iter():
                constant = e.find('inputport[@name="constant"]')
                if constant is not None and constant.text:
                    for p in e.findall('outputport'):
                        constants[p.get('id')] = constant.text.strip()
            steps = config['end_year'] - config['start_year'] + 1
            for channel in ('W', 'V'):
                index = scenario / f'In/DemandScenarios/{channel}_origin_component_index.csv'
                if index.is_file():
                    with index.open(newline='') as stream:
                        components = [int(row['ComponentIndex']) for row in csv.DictReader(stream)]
                else:
                    missing.append(str(index.relative_to(scenario)))
                    components = []
                for period in range(1, steps + 1):
                    rel = f'In/DemandScenarios/{channel}_origin_demand{period:02d}.csv'
                    if not (scenario / rel).is_file(): missing.append(rel)
                for period in range(1, steps + 1, 10):
                    for component in components:
                        rel = f'In/{channel}_origin_components/IDW_C++_fw_{channel.lower()}{component:03d}_{period:02d}.tif'
                        if not (scenario / rel).is_file(): missing.append(rel)
            for e in root.iter():
                if e.get('name', '').startswith('Load'):
                    p = e.find('inputport[@name="filename"]')
                    if p is not None and p.text and not p.get('peerid'):
                        rel = p.text.strip().strip('"')
                        if not rel.startswith(('In/', 'LULCC/')):
                            continue
                        # These optional inputs are inside disabled branches
                        # in the supplied Windows graph. Check their switches.
                        if ((rel == 'LULCC/TempRaster/AnnLoss.tif' and constants.get('v253') == '.no')
                                or (rel in ('In/Indice_v.tif', 'In/Indice_w.tif') and constants.get('v164') == '.no')):
                            skipped.append(rel)
                            continue
                        digits = e.find('inputport[@name="suffixDigits"]')
                        step = e.find('inputport[@name="step"]')
                        paths = [Path(rel)]
                        if digits is not None and digits.text and int(digits.text) > 0 and step is not None:
                            if step.get('peerid') in ('v59', 'v354'):
                                periods = range(1, steps + 1, 10 if step.get('peerid') == 'v354' else 1)
                            elif step.text and step.text.strip().isdigit():
                                periods = [int(step.text)]
                            else:
                                raise RuntimeError(f'Unrecognized input filename step for {rel}')
                            base = Path(rel)
                            paths = [base.with_name(f'{base.stem}{i:0{int(digits.text)}d}{base.suffix}') for i in periods]
                        missing.extend(str(path) for path in paths if not (scenario / path).is_file())
            ok = not missing
            checks.append({'check': 'Scenario input files', 'passed': ok, 'missing': sorted(set(missing)),
                           'inputs_in_disabled_branches': sorted(set(skipped))})
            message = '[OK] Required scenario input files exist.' if ok else 'Missing inputs: ' + ', '.join(sorted(set(missing))) + '\nComplete preprocessing and run 9_install_directional_IDW_outputs_v4.R before starting the model.'
            print(message, flush=True); output.write(message + '\n')

        command = ['stdbuf', '-oL', '-eL', str(executable), f'-processors={args.processors}',
                   '-predefined-seed', '-log-level=4' if args.check else '-log-level=3']
        if args.check:
            command.append('-dont-run')
        command.append(str(model))
        if ok:
            ok = execute(command, 'Dinamica model validation' if args.check else 'Full Dinamica run',
                         'Model script read successfully' if args.check else 'Model script ran successfully')

    current_hashes = {rel: sha(scenario / rel) for rel in config_hashes}
    checks.append({'check': 'Scenario parameter files unchanged', 'passed': current_hashes == config_hashes})
    batch = scenario / 'Temp/mc_batch_ready.csv'
    mc_inputs = {}
    if not args.check and batch.is_file():
        with batch.open(newline='') as stream:
            names = [row['file'] for row in csv.DictReader(stream)]
        for name in names:
            path = scenario / 'Temp' / name
            if path.is_file(): mc_inputs[name] = sha(path)
    result = {'passed': ok and all(c['passed'] for c in checks), 'mode': mode,
              'scenario': str(scenario), 'settings': config, 'settings_sha256': config_hashes,
              'model_sha256': sha(model), 'engine': str(executable), 'seed': seed or None,
              'plot_dpi_cap': int(dpi), 'checks': checks, 'mc_inputs_sha256': mc_inputs,
              'elapsed_seconds': round(time.monotonic() - start, 3), 'started_utc': stamp,
              'workers': args.processors, 'cpu_capacity': capacity,
              'windows_comparison': 'Not performed by this launcher', 'log': str(log)}
    log.with_suffix('.json').write_text(json.dumps(result, indent=2) + '\n')
    if result['passed']:
        print('Checks passed; no simulation was run.' if args.check else 'Simulation and reporting completed.', flush=True)
    else:
        print(f'{mode.capitalize()} failed; see {log}', flush=True)
    raise SystemExit(0 if result['passed'] else 1)


if __name__ == '__main__':
    main()
