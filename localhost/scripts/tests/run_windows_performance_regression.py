"""Serial Windows performance regression on copied, frozen small fixtures.

The default suite covers v13 fixed cover and v14 changing cover, capped and
uncapped, plus v14 with active Patcher. Every fixture uses three MC draws and
three years. Four external R preparation/report calls are removed identically;
the scientific graph and frozen MC inputs remain active. No production run is
modified. Baseline and candidate are compared within a model version, so an
intentional v13/v14 scientific difference cannot masquerade as a speed change.

Each run has a separate TEMP directory, one engine processor, and a predefined
engine seed. Run suites serially on an otherwise idle machine before treating
elapsed times as a speed estimate. Native compiler warnings are retained.

Historical dynamic cases select LUC=1 and therefore reject the corrected v14
input/capture contract. Use run_windows_annual_eligibility_regression.py for it.
"""
from __future__ import annotations

import argparse
import contextlib
import csv
import io
import json
from pathlib import Path
import re
import sys
import xml.etree.ElementTree as ET

sys.dont_write_bytecode = True
import dinamica_runtime_regression_v1 as harness
import run_woodman_dinamica_v14_full_regression as cohorts


CASES = {
    'v13_fixed_capped': (13, False, False, 0),
    'v13_fixed_uncapped': (13, False, False, 1),
    'v14_fixed_capped': (14, False, False, 0),
    'v14_fixed_uncapped': (14, False, False, 1),
    'v14_dynamic_capped': (14, True, False, 0),
    'v14_dynamic_uncapped': (14, True, False, 1),
    'v14_dynamic_capped_patcher': (14, True, True, 0),
}
DEFAULT_CASES = ','.join(k for k in CASES if not k.startswith('v14_fixed'))

EXACT_RASTERS = r'''
suppressPackageStartupMessages(library(terra));a<-commandArgs(TRUE)
left<-a[1];right<-a[2];plan<-read.csv(a[3]);rows<-list()
for(j in seq_len(nrow(plan))){
 p<-plan$path[j];x<-rast(file.path(left,p));y<-rast(file.path(right,p))
 geometry<-compareGeom(x,y,stopOnError=FALSE);type<-identical(datatype(x),datatype(y))
 if(!geometry){rows[[j]]<-data.frame(path=p,geometry_equal=FALSE,type_equal=type,mask_differences=NA,value_differences=NA,max_abs_error=NA);next}
 xv<-as.numeric(values(x));yv<-as.numeric(values(y));mask<-xor(is.na(xv),is.na(yv))
 both<-!is.na(xv)&!is.na(yv);changed<-xv[both]!=yv[both];different<-sum(changed)
 error<-if(different)max(abs(xv[both][changed]-yv[both][changed]))else 0
 rows[[j]]<-data.frame(path=p,geometry_equal=geometry,type_equal=type,mask_differences=sum(mask),value_differences=different,max_abs_error=error)
}
out<-if(length(rows))do.call(rbind,rows)else data.frame(path=character(),geometry_equal=logical(),type_equal=logical(),mask_differences=integer(),value_differences=integer(),max_abs_error=double())
write.csv(out,a[4],row.names=FALSE)
stopifnot(all(out$geometry_equal),all(out$type_equal),all(out$mask_differences==0),all(out$value_differences==0))
cat('PASS: every differing TIFF has exactly equal raster geometry, type, values and NoData mask.\n')
'''


def producer(root, peer):
    return next(n for n in root.iter() if any(p.get('id') == peer for p in n.findall('outputport')))


def configured_model(source, destination, active_patcher):
    tree = ET.parse(source)
    root = tree.getroot()
    # The small fixture uses MODIS class keys. Its annual copies provide the
    # controlled v14 transition inputs; the input channel is equal in each pair.
    producer(root, 'v302').find("inputport[@name='constant']").text = '1'
    producer(root, 'v313').find("inputport[@name='constant']").text = '.no' if active_patcher else '.yes'
    tree.write(destination, encoding='utf-8', xml_declaration=True)


def fixture_name(label, case):
    return label + '__' + case


def stage_fixture(args, case):
    version, dynamic, active, uncapped = CASES[case]
    name = fixture_name(args.label, case)
    target = args.root / name
    model = args.v13 if version == 13 else args.v14
    if version == 14 and dynamic:
        cohorts.require_legacy_capture_contract(ET.parse(model).getroot(),
                                               "This historical dynamic performance case")
    source_hash = harness.sha256(model)
    if target.exists():
        record = json.loads((target / 'performance_manifest.json').read_text())
        if record['source_model_sha256'] != source_hash:
            raise ValueError('Existing fixture uses a different model: ' + name)
        return target
    models = args.root / 'model_snapshots'
    models.mkdir(exist_ok=True)
    configured = models / (name + '.egoml')
    configured_model(model, configured, active)
    with contextlib.redirect_stdout(io.StringIO()):
        harness.stage(argparse.Namespace(source=args.source, root=args.root, name=name,
                                        model=configured, years=3, mc=3, uncapped=uncapped))
    # Reuse one fully prepared cohort input set for each pair. This CSV is
    # descriptive; its corresponding raster values are in the copied inputs.
    (target / 'injected_cohorts.csv').write_bytes((args.source / 'injected_cohorts.csv').read_bytes())
    if dynamic:
        cohorts.r_call(args, 'prepare_' + name, cohorts.DYNAMIC, target, 1)
    cohorts.freeze_added_inputs(target)
    harness.emit_json(target / 'performance_manifest.json', {
        'label': args.label, 'case': case, 'version': version, 'dynamic': dynamic,
        'patcher_active': active, 'years': 3, 'mc': 3,
        'source_model': str(model), 'source_model_sha256': source_hash,
        'configured_model_sha256': harness.sha256(configured),
        'fixture_model_sha256': harness.sha256(target / 'regression_model.egoml'),
        'fixture_edits': 'LUC channel 1; explicit Patcher flag; identical removal of four external R calls; duration and MC parameters',
    })
    return target


def audit_fixture(args, case, target):
    version, dynamic, active, uncapped = CASES[case]
    for mc in range(1, 4):
        script = (cohorts.CHECK
                  .replace('Temp/2_IniSt01.tif', f'Temp/2_IniSt{mc:02d}.tif')
                  .replace('debugging_1', f'debugging_{mc}')
                  .replace('cohort_results.csv', f'cohort_results_mc{mc:02d}.csv'))
        cohorts.r_call(args, f'check_{target.name}_mc{mc:02d}', script,
                       target, 'yes' if dynamic else 'no')
    lines = (target / 'runtime.log').read_text(errors='replace').splitlines()
    saves = {}
    calculate_seconds = 0.0
    for line in lines:
        match = re.search(r'Saving map "([^"]+)"', line)
        if match:
            path = match.group(1).replace('\\', '/')
            if '/Debugging/' in path:
                saves['shared_Debugging'] = saves.get('shared_Debugging', 0) + 1
            else:
                saves['other'] = saves.get('other', 0) + 1
        match = re.search(r'"CalculateMap" ran successfully \(elapsed ([\d.]+) s\)', line)
        if match:
            calculate_seconds += float(match.group(1))
    stats = {
        'native_compiler_warnings': sum('Unable to generate a native version' in line for line in lines),
        'calculate_map_logged_seconds': calculate_seconds,
        'save_counts': saves, 'cohort_checks_passed': True,
        'patcher_active': active,
        'patcher_calls_logged': sum('Running "Patcher"' in line for line in lines),
    }
    harness.emit_json(target / 'performance_audit.json', stats)
    return stats


def compare_pair(args, case):
    left_name = fixture_name(args.compare_to, case)
    right_name = fixture_name(args.label, case)
    left, right = args.root / left_name, args.root / right_name
    inputs_a = json.loads((left / 'frozen_input_hashes.json').read_text())
    inputs_b = json.loads((right / 'frozen_input_hashes.json').read_text())
    if inputs_a != inputs_b:
        raise AssertionError('Frozen scientific inputs differ: ' + case)
    old = json.loads((left / 'scientific_output_sha256.json').read_text())
    new = json.loads((right / 'scientific_output_sha256.json').read_text())
    if old.keys() != new.keys():
        raise AssertionError('Scientific output inventory differs: ' + case)
    differences = sorted(p for p in old if old[p] != new[p])
    nonrasters = [p for p in differences if not p.lower().endswith(('.tif', '.tiff'))]
    if nonrasters:
        raise AssertionError('CSV/nonraster bytes changed: ' + repr(nonrasters))
    plan = args.root / f'exact_raster_plan_{right_name}.csv'
    with plan.open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=('path',))
        writer.writeheader()
        writer.writerows({'path': p} for p in differences)
    cohorts.r_call(args, 'exact_rasters_' + right_name, EXACT_RASTERS, left, right,
                   plan, args.root / f'exact_raster_result_{right_name}.csv')
    before = json.loads((left / 'runtime_result.json').read_text())
    after = json.loads((right / 'runtime_result.json').read_text())
    result = {
        'case': case, 'baseline': left_name, 'candidate': right_name,
        'output_count': len(old), 'identical_output_bytes': len(old) - len(differences),
        'byte_differences': differences, 'all_output_bytes_identical': not differences,
        'retained_scientific_content_identical': True,
        'baseline_seconds': before['elapsed_seconds'], 'candidate_seconds': after['elapsed_seconds'],
        'observed_speed_ratio': before['elapsed_seconds'] / after['elapsed_seconds'],
        'baseline_command': before['command'], 'candidate_command': after['command'],
        'timing_limit': 'Single bounded paired run; external R stages excluded; not a production speed guarantee',
    }
    harness.emit_json(args.root / f'performance_comparison_{right_name}.json', result)
    print(json.dumps(result), flush=True)
    return result


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--root', type=Path, required=True)
    p.add_argument('--source', type=Path, required=True)
    p.add_argument('--label', required=True)
    p.add_argument('--v13', type=Path, required=True)
    p.add_argument('--v14', type=Path, required=True)
    p.add_argument('--compare-to')
    p.add_argument('--cases', default=DEFAULT_CASES)
    p.add_argument('--engine', type=Path, default=harness.DEFAULT_ENGINE)
    p.add_argument('--rscript', type=Path, default=Path('C:/Program Files/R/R-4.6.0/bin/Rscript.exe'))
    p.add_argument('--processors', type=int, default=1)
    p.add_argument('--engine-arg', action='append', default=[],
                   help='Additional engine flag; use --engine-arg=-flag and repeat as needed.')
    p.add_argument('--disable-native-expressions', action='store_true')
    p.add_argument('--stage-only', action='store_true')
    args = p.parse_args()
    args.root = args.root.resolve()
    if not any(part.lower() == 'mofuss_active' for part in args.root.parts):
        p.error('Use a named temporary subdirectory under MoFuSS_Active.')
    if not re.fullmatch(r'[A-Za-z0-9_-]+', args.label):
        p.error('Label must contain letters, digits, underscore or hyphen.')
    selected = args.cases.split(',')
    if any(case not in CASES for case in selected):
        p.error('Unknown case; choices: ' + ','.join(CASES))
    args.root.mkdir(parents=True, exist_ok=True)
    # Existing r_call helper writes diagnostics to its scratch attribute.
    args.scratch = args.root
    records = []
    for case in selected:
        target = stage_fixture(args, case)
        if args.stage_only:
            print('STAGED', target.name, flush=True)
            continue
        if not (target / 'runtime_result.json').exists():
            print('RUNNING', target.name, flush=True)
            harness.run(argparse.Namespace(root=args.root, name=target.name, engine=args.engine,
                        processors=args.processors, timeout=1200, verify_only=False,
                        disable_native_expressions=args.disable_native_expressions,
                        extra_engine_flags=args.engine_arg))
        else:
            previous = json.loads((target / 'runtime_result.json').read_text())
            if previous['returncode'] != 0:
                raise RuntimeError('Previous run failed; use a fresh label: ' + target.name)
            command = previous['command']
            if (Path(command[0]).resolve() != args.engine.resolve()
                    or command[command.index('-processors') + 1] != str(args.processors)
                    or ('-disable-native-expressions' in command) != args.disable_native_expressions
                    or previous.get('extra_engine_flags', []) != args.engine_arg):
                raise ValueError('Existing evidence used different engine options; use a fresh label.')
            print('REUSING completed evidence', target.name, flush=True)
        audit_fixture(args, case, target)
        if args.compare_to:
            records.append(compare_pair(args, case))
    if not args.stage_only:
        harness.emit_json(args.root / ('suite_' + args.label + '.json'), {
            'label': args.label, 'cases': selected, 'all_selected_checks_passed': True,
            'comparisons': records,
        })
        print('PASS:', args.label, ', '.join(selected), flush=True)


if __name__ == '__main__':
    main()
