"""Bounded native Windows full-graph v13/v14 regression with explicit AGB cohorts.

Copies frozen inputs from a completed small run into a new MoFuSS_Active folder.
Only those copies receive numeric-zero, NULL, positive and TOF test cohorts.
Annual LUC/TOF maps are exact baseline copies for the fixed-map parity gate.
Optional dynamic tests exercise forest loss/return and domain disappearance and
reentry. Four legacy external R calls are removed by the existing harness;
the entire scientific graph, initialization, demand and sourcing remain active.

This historical driver requires the legacy v14 capture/input contract. For the
corrected annual-eligibility graph, use run_windows_annual_eligibility_regression.py.
The fixture helpers remain importable by that driver.
"""
from __future__ import annotations

import argparse
import contextlib
import csv
import io
import json
from pathlib import Path
import re
import subprocess
import sys
import xml.etree.ElementTree as ET

sys.dont_write_bytecode = True
import dinamica_runtime_regression_v1 as harness


def require_legacy_capture_contract(root, context):
    if root.find("property[@key='mofuss.sourcing.capture.contract']") is not None:
        raise ValueError(
            context + " does not support corrected v14: its historical LUC=1 "
            "dynamic selection and/or sourcing inventory are obsolete. Use "
            "run_windows_annual_eligibility_regression.py instead; this driver "
            "remains available for historical models without the new capture contract."
        )


PREPARE = r'''
suppressPackageStartupMessages(library(terra))
a<-commandArgs(TRUE);root<-a[1];channel<-as.integer(a[2]);years<-as.integer(a[3])
file<-function(x)file.path(root,'LULCC/TempRaster',x)
get<-function(x)as.numeric(values(rast(file(x))))
luc<-get(paste0('LULCt',channel,'_c.tif'));tof<-get(paste0('TOFvsFOR_mask',channel,'.tif'))
agb<-get('agb3_c.tif');A<-get('A_c.tif');k<-get('k_c.tif');m<-get('m_c.tif')
forest<-which(is.finite(luc)&tof==0&is.finite(agb)&agb>0&is.finite(A)&A>0&is.finite(k)&k>0&is.finite(m)&m>0)
outside<-which(is.finite(luc)&tof==1&is.finite(agb))
stopifnot(length(forest)>=10,length(outside)>=4)
cells<-c(forest[1:10],outside[1:4])
cohorts<-c('forest_zero','forest_NULL','forest_positive','forest_return','domain_reentry','forest_control','forest_NULL_tof_gain','forest_NULL_reentry','forest_NULL_control','forest_missing_CR','TOF_zero','TOF_NULL','TOF_positive','TOF_loss_return')
agb[cells]<-c(0,NA,50,50,50,50,NA,NA,NA,50,0,NA,50,50)
put<-function(name,v){r<-rast(file(name));values(r)<-v;writeRaster(r,file(name),overwrite=TRUE,datatype='FLT4S',NAflag=-2147483648)}
put('agb3_c.tif',agb)
for(name in c('A_c.tif','k_c.tif','m_c.tif')){v<-get(name);v[cells]<-switch(name,A_c.tif=100,k_c.tif=.05,m_c.tif=2);v[cells[cohorts=='forest_missing_CR']]<-NA;put(name,v)}
zero<-rast(file(paste0('LULCt',channel,'_c.tif')));values(zero)<-ifelse(is.finite(luc),0,NA)
for(y in 2000:(2000+years-1)){
 stopifnot(file.copy(file(paste0('LULCt',channel,'_c.tif')),file(paste0('LULCt',channel,'_c_',y,'.tif')),overwrite=TRUE))
 stopifnot(file.copy(file(paste0('TOFvsFOR_mask',channel,'.tif')),file(paste0('TOFvsFOR_mask',channel,'_',y,'.tif')),overwrite=TRUE))
 writeRaster(zero,file(paste0('LULCt',channel,'_transition_',y,'.tif')),datatype='INT1U',NAflag=255,overwrite=TRUE)
}
write.csv(data.frame(cohort=cohorts,cell=cells,luc=luc[cells],tof=tof[cells],raw_agb=agb[cells]),file.path(root,'injected_cohorts.csv'),row.names=FALSE,na='')
'''

DYNAMIC = r'''
suppressPackageStartupMessages(library(terra));a<-commandArgs(TRUE);root<-a[1];channel<-as.integer(a[2]);co<-read.csv(file.path(root,'injected_cohorts.csv'))
file<-function(x)file.path(root,'LULCC/TempRaster',x)
change<-function(name,cells,vals){r<-rast(file(name));v<-as.numeric(values(r));v[cells]<-vals;values(r)<-v;writeRaster(r,file(name),overwrite=TRUE,datatype='FLT4S',NAflag=-2147483648)}
ret<-co$cell[co$cohort%in%c('forest_return','forest_NULL')];entry<-co$cell[co$cohort%in%c('domain_reentry','forest_NULL_reentry')];tof_luc<-co$luc[co$cohort=='TOF_positive'];forest_luc<-co$luc[co$cohort=='forest_positive']
change(paste0('LULCt',channel,'_c_2001.tif'),c(ret,entry),c(rep(tof_luc,length(ret)),rep(NA,length(entry))))
change(paste0('TOFvsFOR_mask',channel,'_2001.tif'),c(ret,entry),c(rep(1,length(ret)),rep(NA,length(entry))))
change(paste0('LULCt',channel,'_transition_2001.tif'),c(ret,entry),c(rep(1,length(ret)),rep(NA,length(entry))))
change(paste0('LULCt',channel,'_transition_2002.tif'),ret,rep(2,length(ret)))
gain<-co$cell[co$cohort=='forest_NULL_tof_gain'];loss<-co$cell[co$cohort%in%c('TOF_NULL','TOF_loss_return')]
change(paste0('LULCt',channel,'_c_2001.tif'),c(gain,loss),c(tof_luc,rep(forest_luc,length(loss))))
change(paste0('TOFvsFOR_mask',channel,'_2001.tif'),c(gain,loss),c(1,rep(0,length(loss))))
change(paste0('LULCt',channel,'_transition_2001.tif'),c(gain,loss),c(3,rep(4,length(loss))))
change(paste0('LULCt',channel,'_transition_2002.tif'),c(gain,loss),c(4,rep(3,length(loss))))
'''

CHECK = r'''
suppressPackageStartupMessages(library(terra));a<-commandArgs(TRUE);root<-a[1];dynamic<-a[2]=='yes';co<-read.csv(file.path(root,'injected_cohorts.csv'))
val<-function(p)as.numeric(values(rast(file.path(root,p))))[co$cell]
co$initial<-val('Temp/2_IniSt01.tif');stopifnot(co$initial[co$cohort=='forest_zero']==0,all(is.na(co$initial[grepl('^forest_NULL',co$cohort)])),co$initial[co$cohort=='forest_positive']>0,is.finite(co$initial[co$cohort=='TOF_NULL']))
uncapped<-grepl('uncapped',basename(root))
out<-list();i<-0
for(y in 1:3){d<-co;d$step<-y
 growth<-sprintf('debugging_1/Growth%02d.tif',y);if(!file.exists(file.path(root,growth)))growth<-sprintf('debugging_1/Growth_CR%02d.tif',y)
 if(!file.exists(file.path(root,growth))){hits<-list.files(file.path(root,'debugging_1'),pattern=paste0('^Growth.*',sprintf('%02d',y),'[.]tif$'),full.names=FALSE);hits<-hits[!grepl('less_harv',hits)];stopifnot(length(hits)==1);growth<-file.path('debugging_1',hits)}
 d$available<-val(growth);d$harvest<-val(sprintf('debugging_1/Harvest_tot%02d.tif',y));d$post_harvest<-val(sprintf('debugging_1/Growth_less_harv%02d.tif',y))
 q<-grepl('^forest_NULL',d$cohort);stopifnot(all(is.na(d$available[q])),all(d$harvest[q]==0),all(is.na(d$post_harvest[q])))
 q<-d$cohort%in%c('forest_zero','TOF_zero');stopifnot(all(is.finite(d$available[q])),all(is.finite(d$post_harvest[q])))
 q<-d$cohort=='forest_missing_CR';if(uncapped)stopifnot(all(is.na(d$available[q])),all(is.na(d$post_harvest[q])))else stopifnot(all(is.finite(d$available[q])),all(is.finite(d$post_harvest[q])))
 if(dynamic&&y%in%c(2,3)){q<-d$cohort=='forest_return';stopifnot(all(d$available[q]==0),all(d$harvest[q]==0),all(d$post_harvest[q]==0))}
 if(dynamic&&y==2){q<-d$cohort%in%c('TOF_NULL','TOF_loss_return');stopifnot(all(d$available[q]==0),all(d$harvest[q]==0),all(d$post_harvest[q]==0))}
 if(dynamic&&y==3){q<-d$cohort%in%c('TOF_NULL','TOF_loss_return');stopifnot(all(is.finite(d$available[q])),all(is.finite(d$post_harvest[q])))}
 if(dynamic&&y==2){q<-d$cohort=='domain_reentry';stopifnot(is.na(d$available[q]),is.na(d$post_harvest[q]))}
 if(dynamic&&y==3){q<-d$cohort=='domain_reentry';stopifnot(is.finite(d$available[q]),is.finite(d$post_harvest[q]))}
 i<-i+1;out[[i]]<-d
}
write.csv(do.call(rbind,out),file.path(root,'cohort_results.csv'),row.names=FALSE,na='')
cat('PASS: explicit raw-zero/NULL initialization and annual cohort checks',root,'\n')
'''

# These auxiliary products expose the deliberately narrower annual domain.
# Actual growth, realized harvest, biomass and all CSVs must remain byte-identical.
SUPPORT_RULES = (
    (r'Debugging/IniProb_[VW][0-9]+[.]tif', 'positive_to_zero'),
    (r'Sourcing/static/[VW]_base[0-9]+_[0-9]+[.]tif', 'positive_to_zero'),
    (r'Sourcing/MC[0-9]+/forest_state[0-9]+[.]tif', 'one_to_null'),
    (r'Debugging/Cum_(?:exp_)?harv[0-9]+[.]tif', 'zero_to_null'),
    (r'Sourcing/static/accumulator_domain[.]tif', 'zero_to_null'),
    (r'Temp/2_(?:CON_TOT|EXP_CON_TOT|fNRB)[0-9]+[.]tif', 'zero_to_null'),
    (r'debugging_[0-9]+/(?:Ex_agr_harv|Non_harv_AGR|Proj_harv_Vdef|Proj_harv_Vtot)[0-9]+[.]tif', 'zero_to_null'),
)

SUPPORT_CHECK = r'''
suppressPackageStartupMessages(library(terra));a<-commandArgs(TRUE);root<-a[1];mode<-a[2]
left<-file.path(root,paste0('v13_',mode));right<-file.path(root,paste0('v14_',mode))
initial<-rast(file.path(left,'Temp/2_IniSt01.tif'));eligible<-is.finite(as.numeric(values(initial)))
plan<-read.csv(file.path(root,paste0('support_plan_',mode,'.csv')));rows<-list()
for(j in seq_len(nrow(plan))){
 p<-plan$path[j];rule<-plan$rule[j];oldr<-rast(file.path(left,p));newr<-rast(file.path(right,p))
 stopifnot(compareGeom(initial,oldr,stopOnError=FALSE),compareGeom(oldr,newr,stopOnError=FALSE))
 old<-as.numeric(values(oldr));new<-as.numeric(values(newr));both<-is.finite(old)&is.finite(new)
 different<-xor(is.finite(old),is.finite(new));different[both]<-old[both]!=new[both]
 expected<-switch(rule,positive_to_zero=both&old>0&new==0,one_to_null=is.finite(old)&old==1&!is.finite(new),zero_to_null=is.finite(old)&old==0&!is.finite(new))
 stopifnot(!any(different&eligible),all(expected[different]))
 rows[[j]]<-data.frame(path=p,rule=rule,eligible_changes=sum(different&eligible),excluded_changes=sum(different&!eligible))
}
report<-if(length(rows))do.call(rbind,rows)else data.frame(path=character(),rule=character(),eligible_changes=integer(),excluded_changes=integer())
write.csv(report,file.path(root,paste0('support_comparison_',mode,'.csv')),row.names=FALSE)
cat('PASS: support-aware fixed-LUC check;',nrow(plan),'allowlisted auxiliary files differ only on initially missing model stock; zero eligible-cell changes.\n')
'''


def r_call(args, name, script, *arguments):
    import os
    path = args.scratch / (name + '.R')
    path.write_text(script, encoding='utf-8')
    temp = args.scratch / 'r_temp'
    temp.mkdir(exist_ok=True)
    environment = os.environ.copy()
    environment.update(TMP=str(temp), TEMP=str(temp), TMPDIR=str(temp))
    result = subprocess.run([str(args.rscript), str(path), *map(str, arguments)],
                            capture_output=True, text=True, check=False, env=environment,
                            cwd=args.scratch)
    (args.scratch / (name + '.log')).write_text(result.stdout + result.stderr, encoding='utf-8')
    if result.returncode:
        raise RuntimeError(result.stdout + result.stderr)


def freeze_added_inputs(target):
    values = json.loads((target / 'frozen_input_hashes.json').read_text())
    for path in (target / 'LULCC/TempRaster').iterdir():
        if path.is_file():
            values[path.relative_to(target).as_posix()] = harness.sha256(path)
    harness.emit_json(target / 'frozen_input_hashes.json', values)


def audit_support_differences(args, label):
    """Retain the strict byte audit and separately enforce intended mask changes."""
    comparison = json.loads((args.scratch / f'comparison_v13_{label}_vs_v14_{label}.json').read_text())
    if comparison['missing'] or comparison['additional']:
        raise AssertionError('Fixed-LUC output inventory changed: ' + label)
    plan = []
    for path in comparison['different']:
        rule = next((rule for pattern, rule in SUPPORT_RULES if re.fullmatch(pattern, path)), None)
        if rule is None:
            raise AssertionError('Non-auxiliary fixed-LUC output changed: ' + path)
        plan.append({'path': path, 'rule': rule})
    with (args.scratch / f'support_plan_{label}.csv').open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=('path', 'rule'))
        writer.writeheader()
        writer.writerows(plan)
    r_call(args, 'check_support_' + label, SUPPORT_CHECK, args.scratch, label)
    harness.emit_json(args.scratch / f'support_contract_{label}.json', {
        'support_contract_passed': True,
        'all_baseline_scientific_outputs_sha256_identical': comparison['all_baseline_scientific_outputs_sha256_identical'],
        'identical_files': comparison['identical'], 'compared_files': comparison['compared'],
        'allowlisted_auxiliary_files': len(plan),
        'eligible_cell_changes': 0,
        'unchanged_required': 'All other output bytes, including actual growth, realized harvest, stock and every CSV',
    })


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source', type=Path, required=True)
    parser.add_argument('--scratch', type=Path, required=True)
    parser.add_argument('--engine', type=Path, default=harness.DEFAULT_ENGINE)
    parser.add_argument('--rscript', type=Path, default=Path('C:/Program Files/R/R-4.6.0/bin/Rscript.exe'))
    parser.add_argument('--mode', choices=('capped', 'uncapped', 'both'), default='both')
    parser.add_argument('--dynamic', action='store_true')
    parser.add_argument('--dynamic-only', action='store_true',
                        help='Run only the two v14 transition/domain fixtures; no static parity claim.')
    parser.add_argument('--disable-native-expressions', action='store_true')
    args = parser.parse_args()
    args.scratch = args.scratch.resolve()
    if not any(part.lower() == 'mofuss_active' for part in args.scratch.parts):
        parser.error('Scratch must be a named folder below MoFuSS_Active.')
    if args.scratch.exists() and any(args.scratch.iterdir()):
        parser.error('Scratch must be new or empty; old evidence is never overwritten.')
    scripts = Path(__file__).resolve().parents[1]
    source13 = scripts / '10_dyn_Sc17_webmofuss_ctrees_g_v13.egoml'
    source14 = scripts / '10_dyn_Sc17_webmofuss_ctrees_g_v14.egoml'
    tree13, tree14 = ET.parse(source13), ET.parse(source14)
    require_legacy_capture_contract(tree14.getroot(), "The legacy full-graph regression")
    args.scratch.mkdir(parents=True, exist_ok=True)
    def producer(tree, peer):
        return next(n for n in tree.iter() if any(p.get('id') == peer for p in n.findall('outputport')))
    channel = int(producer(tree13, 'v302').find("inputport[@name='constant']").text)
    producer(tree14, 'v302').find("inputport[@name='constant']").text = str(channel)
    candidate = args.scratch / 'v14_same_luc_channel.egoml'
    tree14.write(candidate, encoding='utf-8', xml_declaration=True)
    harness.emit_json(args.scratch / 'source_identity.json', {
        'v13_sha256': harness.sha256(source13), 'v14_sha256': harness.sha256(source14),
        'fixture_v14_sha256': harness.sha256(candidate), 'selected_luc_channel': channel,
        'fixture_change': 'Select the same LUC channel as v13; no scientific equations changed'})
    def stage(name, source, model, uncapped):
        with contextlib.redirect_stdout(io.StringIO()):
            harness.stage(argparse.Namespace(source=source, root=args.scratch, name=name,
                          model=model, years=3, mc=1, uncapped=uncapped))
        return args.scratch / name
    prepared = stage('cohort_source', args.source, source13, 0)
    r_call(args, 'prepare_cohorts', PREPARE, prepared, channel, 3)
    for year in range(2000, 2003):
        for stem in (f'LULCt{channel}_c', f'TOFvsFOR_mask{channel}'):
            directory = prepared / 'LULCC/TempRaster'
            if harness.sha256(directory / (stem + '.tif')) != harness.sha256(directory / f'{stem}_{year}.tif'):
                raise AssertionError('Fixed annual map differs from baseline: ' + stem)
    freeze_added_inputs(prepared)
    modes = (0, 1) if args.mode == 'both' else (int(args.mode == 'uncapped'),)
    for mode in modes:
        label = 'uncapped' if mode else 'capped'
        static_models = () if args.dynamic_only else ((13, source13), (14, candidate))
        for version, model in static_models:
            name = f'v{version}_{label}'
            target = stage(name, prepared, model, mode)
            (target / 'injected_cohorts.csv').write_bytes((prepared / 'injected_cohorts.csv').read_bytes())
            print('RUNNING', name, flush=True)
            harness.run(argparse.Namespace(root=args.scratch, name=name, engine=args.engine,
                        processors=1, timeout=1200, verify_only=False,
                        disable_native_expressions=args.disable_native_expressions))
            r_call(args, 'check_' + name, CHECK, target, 'no')
        if not args.dynamic_only:
            try:
                harness.compare(argparse.Namespace(root=args.scratch, left='v13_' + label, right='v14_' + label))
            except SystemExit:
                # The strict byte audit remains false when exclusion changes
                # auxiliary masks; only the explicit support contract can pass.
                pass
            audit_support_differences(args, label)
        if args.dynamic or args.dynamic_only:
            name = 'v14_dynamic_' + label
            target = stage(name, prepared, candidate, mode)
            (target / 'injected_cohorts.csv').write_bytes((prepared / 'injected_cohorts.csv').read_bytes())
            r_call(args, 'prepare_' + name, DYNAMIC, target, channel)
            freeze_added_inputs(target)
            print('RUNNING', name, flush=True)
            harness.run(argparse.Namespace(root=args.scratch, name=name, engine=args.engine,
                        processors=1, timeout=1200, verify_only=False,
                        disable_native_expressions=args.disable_native_expressions))
            r_call(args, 'check_' + name, CHECK, target, 'yes')
    print('PASS: full graph, explicit zero/NULL cohorts, ' +
          ('dynamic transitions/domains.' if args.dynamic_only else 'fixed-LUC scientific outcomes and explicit initial-support contract; see separate strict byte audits.'), flush=True)


if __name__ == '__main__':
    main()
