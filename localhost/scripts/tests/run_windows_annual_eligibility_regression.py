"""Bounded full-graph regression for the approved annual sourcing correction.

Uses Windows Dinamica 2.4.1, three frozen MC rows and three years. Fixed v14
is compared with retained v13; dynamic v14 is compared with an independently
configured old v14 that recomputes its original NPA plus landscape mask every
year. Observer maps do not feed model calculations. External R setup/report
calls are removed identically. No canonical simulation is modified.
"""
from __future__ import annotations

import argparse
import contextlib
import csv
import io
import json
from pathlib import Path
import re
import shutil
import sys
import xml.etree.ElementTree as ET

sys.dont_write_bytecode = True
import dinamica_runtime_regression_v1 as harness
import run_woodman_dinamica_v14_full_regression as cohorts

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'tools'))
from build_dinamica_sourcing_v12 import filename, save

CASES = {
    'fixed_capped': (False, 0, False),
    'fixed_uncapped': (False, 1, False),
    'dynamic_capped': (True, 0, False),
    'dynamic_uncapped': (True, 1, False),
    'dynamic_capped_patcher': (True, 0, True),
}

PERSISTENT_GAIN = r'''
suppressPackageStartupMessages(library(terra));a<-commandArgs(TRUE);root<-a[1]
co<-read.csv(file.path(root,'injected_cohorts.csv'))
cells<-co$cell[co$cohort%in%c('TOF_zero','TOF_positive')]
forest<-co$luc[co$cohort=='forest_positive']
put<-function(name,cells,x){p<-file.path(root,'LULCC/TempRaster',name);r<-rast(p);v<-values(r);v[cells]<-x;values(r)<-v;writeRaster(r,p,overwrite=TRUE,datatype='FLT4S',NAflag=-2147483648)}
for(y in 2001:2002){
 put(paste0('LULCt3_c_',y,'.tif'),cells,forest)
 put(paste0('TOFvsFOR_mask3_',y,'.tif'),cells,0)
 put(paste0('LULCt3_transition_',y,'.tif'),cells,if(y==2001)4 else 0)
}
cat('Persistent TOF-to-forest cohorts:',paste(cells,collapse=','),'\n')
'''

EXACT_AND_SUPPORT = r'''
suppressPackageStartupMessages(library(terra));a<-commandArgs(TRUE)
left<-a[1];right<-a[2];plan<-read.csv(a[3]);rows<-list()
for(j in seq_len(nrow(plan))){
 p<-plan$path[j];rule<-plan$rule[j];x<-rast(file.path(left,p));y<-rast(file.path(right,p))
 stopifnot(compareGeom(x,y,stopOnError=FALSE),identical(datatype(x),datatype(y)))
 xv<-as.numeric(values(x));yv<-as.numeric(values(y));both<-!is.na(xv)&!is.na(yv)
 changed<-xor(is.na(xv),is.na(yv));changed[both]<-xv[both]!=yv[both]
 mc<-if(grepl('^debugging_[0-9]+/',p))as.integer(sub('^debugging_([0-9]+)/.*','\\1',p))else if(grepl('^Sourcing/MC[0-9]+/',p))as.integer(sub('^Sourcing/MC([0-9]+)/.*','\\1',p))else 1L
 eligible<-is.finite(as.numeric(values(rast(file.path(left,sprintf('Temp/2_IniSt%02d.tif',mc))))))
 expected<-switch(rule,exact=rep(FALSE,length(xv)),positive_to_zero=is.finite(xv)&is.finite(yv)&xv>0&yv==0,one_to_null=is.finite(xv)&xv==1&is.na(yv),zero_to_null=is.finite(xv)&xv==0&is.na(yv))
 stopifnot(!any(changed&eligible),all(expected[changed]))
 rows[[j]]<-data.frame(path=p,rule=rule,eligible_changes=sum(changed&eligible),excluded_changes=sum(changed&!eligible),value_changes=sum(changed&both),mask_changes=sum(changed&!both))
}
out<-if(length(rows))do.call(rbind,rows)else data.frame(path=character(),rule=character(),eligible_changes=integer(),excluded_changes=integer(),value_changes=integer(),mask_changes=integer())
write.csv(out,a[4],row.names=FALSE)
cat('PASS exact or explicitly bounded initial-support raster comparisons:',nrow(out),'\n')
'''

WARM_GAIN_CHECK = r'''
suppressPackageStartupMessages(library(terra));a<-commandArgs(TRUE);root<-a[1]
co<-read.csv(file.path(root,'injected_cohorts.csv'));co<-co[co$cohort%in%c('TOF_zero','TOF_positive'),]
read<-function(p)as.numeric(values(rast(file.path(root,p))))[co$cell]
rows<-list();j<-0
for(mc in 1:3){for(year in 1:3){
 d<-co;d$mc<-mc;d$step<-year;d$vehicle_mask<-read(sprintf('Sourcing/MC%03d/mask_V%02d.tif',mc,year))
 paths<-list.files(file.path(root,sprintf('Sourcing/MC%03d',mc)),pattern=sprintf('^V_component[0-9]+_%02d[.]tif$',year),full.names=FALSE)
 stopifnot(length(paths)>0)
 d$vehicle_pressure<-Reduce('+',lapply(paths,function(p)read(file.path(sprintf('Sourcing/MC%03d',mc),p))))
 growth<-list.files(file.path(root,paste0('debugging_',mc)),pattern=sprintf('^Growth.*%02d[.]tif$',year),full.names=FALSE)
 growth<-growth[!grepl('less_harv',growth)];stopifnot(length(growth)==1)
 d$available_biomass<-read(file.path(paste0('debugging_',mc),growth))
 d$realized_total_harvest<-read(sprintf('debugging_%d/Harvest_tot%02d.tif',mc,year))
 if(year==1)stopifnot(all(is.na(d$vehicle_pressure)|d$vehicle_pressure==0))
 if(year==2)stopifnot(all(d$vehicle_mask==0),all(d$vehicle_pressure==0),all(d$realized_total_harvest==0))
 if(year==3)stopifnot(all(is.finite(d$available_biomass)),all(is.finite(d$vehicle_mask)))
 j<-j+1;rows[[j]]<-d
}}
write.csv(do.call(rbind,rows),file.path(root,'persistent_tof_to_forest_gain.csv'),row.names=FALSE)
out<-do.call(rbind,rows);later<-out[out$step==3,]
cat('PASS conversion-year exclusion. Positive post-conversion V pressure records:',sum(later$vehicle_pressure>0,na.rm=TRUE),'of',nrow(later),'(zero is an explicit bounded-test limitation).\n')
'''


def producer(root, peer):
    return next(n for n in root.iter() if any(p.get('id') == peer for p in list(n)
                                            if p.tag in ('outputport', 'internaloutputport')))


def configured_model(source, destination, dynamic, active, reference):
    tree = ET.parse(source)
    root = tree.getroot()
    producer(root, 'v302').find("inputport[@name='constant']").text = '3' if dynamic else '1'
    producer(root, 'v313').find("inputport[@name='constant']").text = '.no' if active else '.yes'
    if reference and dynamic:
        # Independent reference: preserve the original cold-branch arithmetic
        # and force its execution each year. This bypasses all cache reuse.
        condition = producer(root, 'v4000')
        expression = condition.find("inputport[@name='expression']")
        if 'v1 = 1' not in expression.text or 'v2 = v3' not in expression.text:
            raise ValueError('Unexpected original cold-cache condition')
        expression.text = '[1]'
    for offset, channel, loop, component, normalized in (
        (0, 'W', 'v356', 'v357', 'v366'),
        (20, 'V', 'v370', 'v372', 'v381'),
    ):
        target = producer(root, loop)
        path_id = 'v' + str(4006 + offset)
        if any(p.get('id') == path_id for p in root.iter('outputport')):
            raise ValueError('Observer ID already present: ' + path_id)
        filename(target, f'Sourcing/MC<v1,3>/{channel}_component<v2,3>_<v3,2>.tif',
                 ('v38', component, 'v39'), path_id)
        save(target, 'Map', normalized, path_id)
        if channel == 'W':
            filename(target, 'Sourcing/MC<v1,3>/W_redistributed<v2,3>_<v3,2>.tif',
                     ('v38', component, 'v39'), 'v4007')
            save(target, 'Map', 'v5006', 'v4007')
    tree.write(destination, encoding='utf-8', xml_declaration=True)


def stage(args, case, role):
    dynamic, uncapped, active = CASES[case]
    source_model = args.candidate if role == 'candidate' else args.v14_reference if dynamic else args.v13_reference
    name = role + '__' + case
    target = args.root / name
    if target.exists():
        record = json.loads((target / 'eligibility_manifest.json').read_text())
        if record['source_sha256'] != harness.sha256(source_model):
            raise ValueError('Existing evidence uses a different source: ' + name)
        return target
    configured = args.root / 'model_snapshots' / (name + '.egoml')
    configured.parent.mkdir(exist_ok=True)
    configured_model(source_model, configured, dynamic, active, role == 'reference')
    with contextlib.redirect_stdout(io.StringIO()):
        harness.stage(argparse.Namespace(source=args.source, root=args.root, name=name,
                                        model=configured, years=3, mc=3, uncapped=uncapped))
    shutil.copy2(args.source / 'injected_cohorts.csv', target / 'injected_cohorts.csv')
    directory = target / 'LULCC/TempRaster'
    if dynamic:
        # Channel 3 is selected without substituting different class semantics:
        # exact channel 1 scientific inputs are copied under channel 3 names.
        for path in list(directory.iterdir()):
            if re.match(r'^(LULCt1_|TOFvsFOR_mask1)', path.name):
                renamed = path.name.replace('LULCt1_', 'LULCt3_').replace('TOFvsFOR_mask1', 'TOFvsFOR_mask3')
                shutil.copy2(path, directory / renamed)
        shutil.copy2(target / 'Temp/LULC_Categories1.csv', target / 'Temp/LULC_Categories3.csv')
        for stem in ('TOFvsFOR_Categories', 'growth_parameters'):
            shutil.copy2(target / ('LULCC/TempTables/' + stem + '1.csv'),
                         target / ('LULCC/TempTables/' + stem + '3.csv'))
        cohorts.r_call(args, 'transitions_' + name, cohorts.DYNAMIC, target, 3)
        cohorts.r_call(args, 'persistent_gain_' + name, PERSISTENT_GAIN, target)
    else:
        # A fixed branch must work when year-labelled inputs truly do not exist.
        # Delete only newly copied task fixtures; never source/canonical inputs.
        for path in directory.iterdir():
            if re.search(r'_(?:2000|2001|2002)[.]tif$', path.name):
                if path.resolve().parent != directory.resolve():
                    raise ValueError('Fixture path escaped staging directory')
                path.unlink()
    frozen = json.loads((target / 'frozen_input_hashes.json').read_text())
    frozen = {path: digest for path, digest in frozen.items() if (target / path).is_file()}
    for path in directory.iterdir():
        if path.is_file():
            frozen[path.relative_to(target).as_posix()] = harness.sha256(path)
    if dynamic:
        frozen['Temp/LULC_Categories3.csv'] = harness.sha256(target / 'Temp/LULC_Categories3.csv')
        for stem in ('TOFvsFOR_Categories', 'growth_parameters'):
            relative = 'LULCC/TempTables/' + stem + '3.csv'
            frozen[relative] = harness.sha256(target / relative)
    harness.emit_json(target / 'frozen_input_hashes.json', frozen)
    manifest = dict(case=case, role=role, dynamic=dynamic, uncapped=uncapped,
                    patcher_active=active, years=3, mc=3, source=str(source_model),
                    source_sha256=harness.sha256(source_model),
                    configured_sha256=harness.sha256(configured),
                    fixture_model_sha256=harness.sha256(target / 'regression_model.egoml'),
                    reference_method='original cold NPA and annual landscape mask recomputed every year' if dynamic else 'retained v13 fixed-input scientific graph',
                    observer_outputs='Per-origin exact W/V normalized and W redistributed float32 maps')
    harness.emit_json(target / 'eligibility_manifest.json', manifest)
    return target


def audit(args, case, target):
    dynamic, _, active = CASES[case]
    for mc in range(1, 4):
        script = cohorts.CHECK.replace('Temp/2_IniSt01.tif', f'Temp/2_IniSt{mc:02d}.tif').replace('debugging_1', f'debugging_{mc}').replace('cohort_results.csv', f'cohort_results_mc{mc:02d}.csv')
        # Persistent TOF_zero becomes forest in year 2: conversion-year stock
        # zero is still finite, and all original cohort assertions remain valid.
        cohorts.r_call(args, f'cohorts_{target.name}_{mc}', script, target, 'yes' if dynamic else 'no')
    log = (target / 'runtime.log').read_text(errors='replace')
    if active and log.count('Running "Patcher"') < 36:
        raise AssertionError('Active Patcher branch did not execute')
    if dynamic:
        cohorts.r_call(args, 'warm_gain_' + target.name, WARM_GAIN_CHECK, target)


def compare(args, case):
    dynamic = CASES[case][0]
    left, right = (args.root / (role + '__' + case) for role in ('reference', 'candidate'))
    frozen = [json.loads((p / 'frozen_input_hashes.json').read_text()) for p in (left, right)]
    if frozen[0] != frozen[1]:
        raise AssertionError('Reference/candidate inputs differ: ' + case)
    inventories = [json.loads((p / 'scientific_output_sha256.json').read_text()) for p in (left, right)]
    def comparable(path):
        # Static contract changed deliberately; candidate annual accumulators
        # replace legacy static replay inputs. Reader replay checks them directly.
        return not path.startswith('Sourcing/static/') and not re.fullmatch(r'Sourcing/MC[0-9]+/accumulator_domain[0-9]+[.]tif', path)
    old, new = ({p: digest for p, digest in inv.items() if comparable(p)} for inv in inventories)
    if old.keys() != new.keys():
        raise AssertionError('Comparable output inventory changed: ' + repr(old.keys() ^ new.keys()))
    changed = [p for p in old if old[p] != new[p]]
    plan = []
    for path in changed:
        if not path.lower().endswith(('.tif', '.tiff')):
            raise AssertionError('Nonraster bytes changed: ' + path)
        rule = 'exact'
        if not dynamic:
            rule = next((r for pattern, r in cohorts.SUPPORT_RULES if re.fullmatch(pattern, path)), 'exact')
        plan.append(dict(path=path, rule=rule))
    plan_path = args.root / ('comparison_plan_' + case + '.csv')
    with plan_path.open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=('path', 'rule'))
        writer.writeheader()
        writer.writerows(plan)
    result_path = args.root / ('raster_comparison_' + case + '.csv')
    cohorts.r_call(args, 'exact_' + case, EXACT_AND_SUPPORT, left, right, plan_path, result_path)
    result = dict(case=case, passed=True, compared_files=len(old), identical_bytes=len(old)-len(changed),
                  byte_differences=changed, raster_audit=str(result_path),
                  static_capture_contracts_excluded=True,
                  initial_support_auxiliary_exceptions_allowed=not dynamic,
                  exact_csv_and_scalar_files=True,
                  reference=str(left), candidate=str(right))
    harness.emit_json(args.root / ('comparison_' + case + '.json'), result)
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source', type=Path, required=True)
    parser.add_argument('--root', type=Path, required=True)
    parser.add_argument('--v13-reference', type=Path, required=True)
    parser.add_argument('--v14-reference', type=Path, required=True)
    parser.add_argument('--candidate', type=Path, required=True)
    parser.add_argument('--engine', type=Path, default=harness.DEFAULT_ENGINE)
    parser.add_argument('--rscript', type=Path, default=Path('C:/Program Files/R/R-4.6.0/bin/Rscript.exe'))
    parser.add_argument('--cases', default=','.join(CASES))
    operation = parser.add_mutually_exclusive_group()
    operation.add_argument('--stage-only', action='store_true')
    operation.add_argument('--compare-only', action='store_true',
                           help='Recheck saved completed outputs; never rerun engines or cohort preparations.')
    args = parser.parse_args()
    args.root = args.root.resolve()
    if 'MoFuSS_Active' not in args.root.parts:
        parser.error('Root must be a named task directory under MoFuSS_Active')
    args.root.mkdir(parents=True, exist_ok=True)
    args.scratch = args.root
    selected = args.cases.split(',')
    if not set(selected) <= CASES.keys():
        parser.error('Unknown case')
    results = []
    for case in selected:
        for role in ('reference', 'candidate'):
            if args.compare_only and not (args.root / (role + '__' + case) / 'eligibility_manifest.json').is_file():
                raise FileNotFoundError('No completed comparison fixture: ' + role + '__' + case)
            target = stage(args, case, role)
            if args.stage_only:
                continue
            if args.compare_only:
                record = json.loads((target / 'runtime_result.json').read_text())
                if record['returncode'] != 0:
                    raise RuntimeError('Cannot compare a failed runtime: ' + str(target))
                continue
            result_path = target / 'runtime_result.json'
            if result_path.exists():
                record = json.loads(result_path.read_text())
                if record['returncode'] != 0:
                    raise RuntimeError('Preserved failed attempt needs a fresh root: ' + str(target))
            else:
                print('RUNNING', target.name, flush=True)
                harness.run(argparse.Namespace(root=args.root, name=target.name, engine=args.engine,
                                               processors=1, timeout=1800, verify_only=False,
                                               disable_native_expressions=False))
            audit(args, case, target)
        if not args.stage_only:
            results.append(compare(args, case))
            print('PASS', case, flush=True)
            harness.emit_json(args.root / 'integration_suite.json', dict(cases=results, all_passed=True,
                              candidate_sha256=harness.sha256(args.candidate), engine=str(args.engine),
                              limitation='Three years, three frozen MC rows, bounded real-data grid; no 51-year or production-timing claim'))
    if not args.stage_only:
        all_cases = []
        for case in CASES:
            path = args.root / ('comparison_' + case + '.json')
            if path.exists():
                verdict = json.loads(path.read_text())
                verdict['runtime_results'] = [str(args.root / (role + '__' + case) / 'runtime_result.json')
                                              for role in ('reference', 'candidate')]
                all_cases.append(verdict)
        harness.emit_json(args.root / 'integration_release_gate.json', {
            'all_checks_passed': len(all_cases) == len(CASES) and all(x['passed'] for x in all_cases),
            'source_v13_sha256': harness.sha256(args.v13_reference),
            'source_v14_sha256': harness.sha256(args.v14_reference),
            'candidate_source': str(args.candidate),
            'candidate_sha256': harness.sha256(args.candidate),
            'cases': all_cases, 'required_cases': list(CASES),
            'fixture_source': str(args.source), 'years': 3, 'mc': 3,
            'distinct_mc_rows': {
                name: len({tuple(row[1:]) for row in list(csv.reader((args.source / 'Temp' / name).open()))[1:4]})
                for name in ('i_st_all.csv', 'k_all.csv', 'rmax_all.csv')
            },
            'input_identity': 'Every copied input SHA256 identical within each comparison pair',
            'dynamic_reference': 'Original pre-correction v14 cold NPA and annual-landscape stages executed every year',
            'fixed_reference': 'Retained v13 with no year-labelled input rasters present in either fixture',
            'scope': 'Scientific graph and per-origin observer components, all CSV/scalars byte exact; legacy initial-NoData auxiliary exclusions explicit; reader replay gate separate',
        })


if __name__ == '__main__':
    main()
