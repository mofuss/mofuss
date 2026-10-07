"""Bounded Dinamica engine comparison using the frozen performance fixtures.

Every TIFF must preserve exact pixels, NoData, grid, CRS and datatype. CSV
delimiters/whitespace may differ. Temp/x_Cons_W.csv and x_Cons_W_all.csv permit
tiny aggregate differences; Temp/2_AGBt1_NRB*.csv permits only proven binary32
half-up versus half-even decimal printer ties; Temp/2_AGBt1x*.csv permits tiny
output-only stock-total aggregation/rendering differences. Their absolute AND relative
bounds are fixed here. All other numeric values, keys and decoded
Sourcing binary64 scalars remain exact. No production model/run is changed.

Default coverage: v13 fixed capped/uncapped, v14 dynamic capped/uncapped and
active Patcher, each three years by three MC draws. --timing-qualified is an
explicit assertion that the compared runs were performed serially without
other heavy work; normal correctness runs do not establish speed qualification.
"""
from __future__ import annotations

import argparse
import csv
from decimal import Decimal, InvalidOperation, ROUND_HALF_UP, ROUND_HALF_EVEN
import json
import math
from pathlib import Path
import re
import struct
import sys

sys.dont_write_bytecode = True
import run_windows_performance_regression as fixtures

harness = fixtures.harness
cohorts = fixtures.cohorts
DEFAULT_ENGINE = Path('C:/Users/UNAM/AppData/Local/Programs/DinamicaEGO-8.13/DinamicaConsole8.exe')
ENGINE_FLAGS = ['-disable-parallel-steps', '-disable-parallel-functors']
TOLERATED_CSVS = {'Temp/x_Cons_W.csv', 'Temp/x_Cons_W_all.csv'}
# Each permitted aggregate ends at SaveTable/SaveLookupTable and its reporting
# table's own Mux. It does not feed the raster dynamics. Proven cases:
# * v228/v229 demand sums: 464628.569657004 versus 464628.569657.
# * v101 = attributes(v99)[12], NRB-masked stock: both engines' sole selected
#   MC3/year1 pixel is exactly 1727.125244140625; decimal writers print ...063
#   and ...062 at 15 significant digits (v13 uncapped fixture).
# * v103 = attributes(v98)[12], stock total: both engines' 40,300 valid pixels
#   in Temp/2_AGBt101.tif sum to 44294735.123870849609375; CSV last digits differ
#   by 1e-7 (v14 dynamic capped fixture). This is an aggregate/rendering
#   difference, not a proven binary32 printer tie. Core raster equality is
#   still required independently for every allowed case.
ABS_TOLERANCE = Decimal('1e-6')
REL_TOLERANCE = Decimal('1e-12')

EXACT_RASTERS = r'''
suppressPackageStartupMessages(library(terra));a<-commandArgs(TRUE)
terraOptions(tempdir=tempdir(),memfrac=.1,progress=0)
plan<-read.csv(a[3]);rows<-list()
for(j in seq_len(nrow(plan))){
 p<-plan$path[j];x<-rast(file.path(a[1],p));y<-rast(file.path(a[2],p))
 geom<-compareGeom(x,y,stopOnError=FALSE)
 grid<-identical(as.vector(ext(x)),as.vector(ext(y)))&&identical(res(x),res(y))&&identical(dim(x),dim(y))
 type<-identical(datatype(x),datatype(y));crs_ok<-identical(crs(x),crs(y))
 row<-data.frame(path=p,geometry_equal=geom,exact_grid_equal=grid,type_equal=type,crs_equal=crs_ok,
                 mask_differences=NA_integer_,value_differences=NA_integer_,max_abs_error=NA_real_,
                 baseline_sum=NA_real_,candidate_sum=NA_real_,baseline_mean=NA_real_,candidate_mean=NA_real_)
 if(geom&&grid){
  xv<-as.numeric(values(x));yv<-as.numeric(values(y));both<-!is.na(xv)&!is.na(yv)
  row$mask_differences<-sum(xor(is.na(xv),is.na(yv)))
  changed<-xv[both]!=yv[both];row$value_differences<-sum(changed)
  row$max_abs_error<-if(any(changed))max(abs(xv[both][changed]-yv[both][changed]))else 0
  row$baseline_sum<-sum(xv,na.rm=TRUE);row$candidate_sum<-sum(yv,na.rm=TRUE)
  row$baseline_mean<-mean(xv,na.rm=TRUE);row$candidate_mean<-mean(yv,na.rm=TRUE)
 }
 rows[[j]]<-row
}
write.csv(do.call(rbind,rows),a[4],row.names=FALSE)
'''


def csv_rows(path):
    result = []
    with path.open(encoding='utf-8-sig', newline='') as stream:
        for row in csv.reader(stream):
            row = [value.strip() for value in row]
            while row and row[-1] == '':
                row.pop()
            if row:
                result.append(row)
    return result


def decimal_number(value):
    try:
        return Decimal(value)
    except InvalidOperation:
        return None


def decode_scalars(rows):
    values = {int(row[0]): float(row[1]) for row in rows[1:]}
    if len(values) != len(rows) - 1 or sorted(values) != list(range(1, len(values) + 1)) or len(values) % 3:
        raise ValueError('Scalar codec keys must be unique consecutive triples.')
    result = []
    for key in range(1, len(values) + 1, 3):
        exponent, high, low = (values[key + i] for i in range(3))
        if not all(math.isfinite(v) and int(v) == v for v in (exponent, high, low)):
            raise ValueError('Scalar codec components must be finite integers.')
        result.append(math.ldexp(high / 2**26 + low / 2**53, int(exponent)))
    return result


def binary32_printer_tie(a, b):
    """Accept only two 15-significant-digit renderings of one exact binary32."""
    try:
        x, y = (struct.pack('>f', float(value)) for value in (a, b))
        if x != y:
            return None
        exact = Decimal(struct.unpack('>f', x)[0])
        if not exact.is_finite() or not exact:
            return None
        quantum = Decimal(1).scaleb(exact.adjusted() - 14)
        up = exact.quantize(quantum, rounding=ROUND_HALF_UP)
        even = exact.quantize(quantum, rounding=ROUND_HALF_EVEN)
        return str(exact) if up != even and {a, b} == {up, even} else None
    except (OverflowError, InvalidOperation):
        return None


def compare_csv_tables(relative, left, right):
    shape = [len(row) for row in left] == [len(row) for row in right]
    numeric, textual, tolerated, differences = 0, 0, 0, []
    max_delta = Decimal(0)
    if shape:
        for row_index, (old_row, new_row) in enumerate(zip(left, right)):
            for column, (old, new) in enumerate(zip(old_row, new_row)):
                if old == new:
                    continue
                a, b = decimal_number(old), decimal_number(new)
                if a is not None and b is not None and a.is_finite() and b.is_finite():
                    if a == b:
                        continue
                    numeric += 1
                    delta = abs(a - b)
                    max_delta = max(max_delta, delta)
                    scale = max(abs(a), abs(b))
                    bounded_value = (row_index > 0 and column == 1 and delta <= ABS_TOLERANCE
                                     and delta <= REL_TOLERANCE * scale)
                    shared_binary32 = (binary32_printer_tie(a, b) if bounded_value and
                                       re.fullmatch(r'Temp/2_AGBt1_NRB\d+\.csv', relative) else None)
                    reason = ('bounded_demand_aggregate' if bounded_value and relative in TOLERATED_CSVS
                              else 'binary32_decimal_printer_tie' if shared_binary32
                              else 'bounded_output_stock_total' if bounded_value and
                              re.fullmatch(r'Temp/2_AGBt1x\d+\.csv', relative) else '')
                    allowed = bool(reason)
                    tolerated += int(allowed)
                    differences.append({'file': relative, 'row': row_index, 'column': column,
                                        'key': old_row[0], 'old': old, 'new': new,
                                        'absolute_delta': str(delta), 'allowed': allowed,
                                        'allowed_reason': reason, 'shared_binary32_exact': shared_binary32 or ''})
                else:
                    textual += 1
                    differences.append({'file': relative, 'row': row_index, 'column': column,
                                        'key': old_row[0], 'old': old, 'new': new,
                                        'absolute_delta': '', 'allowed': False,
                                        'allowed_reason': '', 'shared_binary32_exact': ''})
    is_codec = bool(re.fullmatch(r'Sourcing/MC\d+/[WV]_scalars\d+_\d+\.csv', relative))
    decoded_count, codec_equal = 0, True
    if is_codec:
        try:
            a, b = decode_scalars(left), decode_scalars(right)
            decoded_count = len(a)
            codec_equal = (len(a) == len(b) and all(struct.pack('>d', x) == struct.pack('>d', y)
                                                  for x, y in zip(a, b)))
        except (ValueError, OverflowError, IndexError):
            codec_equal = False
    return ({'file': relative, 'shape_equal': shape, 'normalized_text_equal': left == right,
             'numeric_differences': numeric, 'tolerated_numeric_differences': tolerated,
             'text_differences': textual, 'max_decimal_delta': str(max_delta),
             'scalar_codec': is_codec, 'decoded_values': decoded_count,
             'decoded_values_exact': codec_equal,
             'passed': shape and textual == 0 and numeric == tolerated and codec_equal}, differences)


def write_rows(path, rows, fields=None):
    with path.open('w', encoding='utf-8', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=fields or list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)


def timing_profile(target, elapsed_seconds):
    text = (target / 'runtime.log').read_text(errors='replace')
    repeats = [float(value) for value in re.findall(r'"Repeat" ran successfully \(elapsed ([\d.]+) s\)', text)]
    # These bounded graphs have an outer MC Repeat and one inner annual Repeat.
    # Three MC draws produce three inner completions, then the outer completion.
    expected_structure = len(repeats) == 4
    return {
        'repeat_structure_recognized': expected_structure,
        'annual_loops_seconds_per_mc': repeats[:3] if expected_structure else None,
        'all_mc_loop_seconds': repeats[-1] if expected_structure else None,
        'outside_mc_loop_seconds': elapsed_seconds - repeats[-1] if expected_structure else None,
        'calculate_map_logged_seconds': sum(float(v) for v in re.findall(
            r'"Calculate\s*Map" ran successfully \(elapsed ([\d.]+) s\)', text)),
        'native_compiler_warnings': text.count('Unable to generate a native version'),
        'patcher_calls': len(re.findall(r'Running "Patcher"', text)),
        'limit': 'Outside-MC time includes compilation, initialization and finalization; it is not a direct compiler timer. Old-engine functor timers are coarsely rounded.',
    }


def compare_pair(args, case):
    left_name = fixtures.fixture_name(args.compare_to, case)
    right_name = fixtures.fixture_name(args.label, case)
    left, right = args.root / left_name, args.root / right_name
    before = json.loads((left / 'runtime_result.json').read_text())
    after = json.loads((right / 'runtime_result.json').read_text())
    for name, runtime in ((left_name, before), (right_name, after)):
        if runtime['returncode'] != 0:
            raise RuntimeError(f'Cannot compare failed engine run {name}: exit {runtime["returncode"]}')
    inputs_a = json.loads((left / 'frozen_input_hashes.json').read_text())
    inputs_b = json.loads((right / 'frozen_input_hashes.json').read_text())
    if inputs_a != inputs_b:
        raise AssertionError('Frozen scientific inputs differ: ' + case)
    # Rehash the scientific outputs rather than trusting saved inventories.
    old, new = harness.science_hashes(left), harness.science_hashes(right)
    missing, added = sorted(old.keys() - new.keys()), sorted(new.keys() - old.keys())
    shared = sorted(old.keys() & new.keys())
    tiffs = [p for p in shared if p.lower().endswith(('.tif', '.tiff'))]
    csvs = [p for p in shared if p.lower().endswith('.csv')]
    prefix = args.root / ('engine_comparison_' + right_name)
    plan, raster_path = Path(str(prefix) + '_raster_plan.csv'), Path(str(prefix) + '_rasters.csv')
    write_rows(plan, [{'path': p} for p in tiffs], ['path'])
    if not tiffs:
        raise AssertionError('No scientific TIFFs to compare: ' + case)
    cohorts.r_call(args, 'engine_exact_' + right_name, EXACT_RASTERS, left, right, plan, raster_path)
    with raster_path.open(newline='') as stream:
        rasters = list(csv.DictReader(stream))
    raster_ok = all(all(r[field] == 'TRUE' for field in
                        ('geometry_equal', 'exact_grid_equal', 'type_equal', 'crs_equal'))
                    and r['mask_differences'] == '0' and r['value_differences'] == '0' for r in rasters)
    csv_results, csv_differences = [], []
    for p in csvs:
        result, differences = compare_csv_tables(p, csv_rows(left / p), csv_rows(right / p))
        csv_results.append(result)
        csv_differences.extend(differences)
    write_rows(Path(str(prefix) + '_csv.csv'), csv_results)
    write_rows(Path(str(prefix) + '_csv_differences.csv'), csv_differences,
               ['file', 'row', 'column', 'key', 'old', 'new', 'absolute_delta', 'allowed',
                'allowed_reason', 'shared_binary32_exact'])
    core_parity = (not missing and not added and raster_ok and all(r['passed'] for r in csv_results)
                   and before['returncode'] == 0 and after['returncode'] == 0)
    reduction = 1 - after['elapsed_seconds'] / before['elapsed_seconds']
    processors_before = before['command'][before['command'].index('-processors') + 1]
    processors_after = after['command'][after['command'].index('-processors') + 1]
    timing_options_comparable = (processors_before == processors_after and
                                ('-disable-native-expressions' in before['command']) ==
                                ('-disable-native-expressions' in after['command']))
    result = {
        'case': case, 'baseline': left_name, 'candidate': right_name,
        'engine_core_parity': core_parity, 'frozen_inputs_identical': True,
        'missing_outputs': missing, 'additional_outputs': added,
        'output_count': len(old), 'identical_output_bytes': sum(old[p] == new[p] for p in shared),
        'tiff_count': len(tiffs), 'all_tiff_values_masks_geometry_type_exact': raster_ok,
        'csv_count': len(csvs), 'csv_comparison_passed': all(r['passed'] for r in csv_results),
        'csv_numeric_changed_fields': sum(r['numeric_differences'] for r in csv_results),
        'csv_tolerated_changed_fields': sum(r['tolerated_numeric_differences'] for r in csv_results),
        'decoded_binary64_count': sum(r['decoded_values'] for r in csv_results),
        'decoded_binary64_all_exact': all(r['decoded_values_exact'] for r in csv_results),
        'csv_tolerance': {'files': sorted(TOLERATED_CSVS), 'value_column_only': True,
                          'absolute_max': str(ABS_TOLERANCE), 'relative_max': str(REL_TOLERANCE),
                          'both_bounds_required': True,
                          'binary32_printer_tie_family': 'Temp/2_AGBt1_NRB*.csv',
                          'bounded_output_stock_total_family': 'Temp/2_AGBt1x*.csv',
                          'printer_tie_rule': 'Both parsed values round to the same binary32; they must exactly equal its distinct 15-significant-digit half-up/half-even renderings.'},
        'baseline_seconds': before['elapsed_seconds'], 'candidate_seconds': after['elapsed_seconds'],
        'observed_speed_ratio': before['elapsed_seconds'] / after['elapsed_seconds'],
        'observed_elapsed_reduction_fraction': reduction,
        'timing_qualified': args.timing_qualified,
        'timing_options_comparable': timing_options_comparable,
        'minimum_elapsed_reduction_fraction': args.min_elapsed_reduction,
        'speed_qualified': (core_parity and args.timing_qualified and timing_options_comparable
                            and reduction >= args.min_elapsed_reduction),
        'baseline_command': before['command'], 'candidate_command': after['command'],
        'baseline_profile': timing_profile(left, before['elapsed_seconds']),
        'candidate_profile': timing_profile(right, after['elapsed_seconds']),
        'timing_limit': 'Bounded fixture, three years by three MC draws; external R steps excluded; not a production speed guarantee.',
    }
    harness.emit_json(Path(str(prefix) + '.json'), result)
    print(json.dumps(result), flush=True)
    return result


def self_test():
    rows = [['Key*', 'Value'], ['1', '464628.569657004']]
    close = [['Key*', 'Value'], ['1', '464628.569657']]
    assert compare_csv_tables('Temp/x_Cons_W.csv', rows, close)[0]['passed']
    assert not compare_csv_tables('Temp/3_CON_TOT.csv', rows, close)[0]['passed']
    assert not compare_csv_tables('Temp/x_Cons_W.csv', rows, [['Key*', 'Value'], ['1', '464628.57']])[0]['passed']
    assert not compare_csv_tables('Temp/x_Cons_W.csv', rows, [['Key*', 'Value'], ['1.0000000000001', rows[1][1]]])[0]['passed']
    assert not compare_csv_tables('Temp/x_Cons_W.csv', rows, close + [['2', '3']])[0]['passed']
    tie_a = [['Key', 'Value'], ['1', '1727.12524414063']]
    tie_b = [['Key', 'Value'], ['1', '1727.12524414062']]
    assert compare_csv_tables('Temp/2_AGBt1_NRB03.csv', tie_a, tie_b)[0]['passed']
    assert not compare_csv_tables('Temp/2_AGBt1_NRB03.csv', tie_a, [['Key', 'Value'], ['1', '1727.12524414061']])[0]['passed']
    assert not compare_csv_tables('Temp/other.csv', tie_a, tie_b)[0]['passed']
    total_a = [['Key', 'Value'], ['3', '44294735.1238709']]
    total_b = [['Key', 'Value'], ['3', '44294735.1238708']]
    assert compare_csv_tables('Temp/2_AGBt1x01.csv', total_a, total_b)[0]['passed']
    assert not compare_csv_tables('Temp/2_AGBt1x01.csv', total_a, [['Key', 'Value'], ['3', '44294735.123872']])[0]['passed']
    assert compare_csv_tables('Temp/exact.csv', [['Key', 'Value'], ['1', '2.0']], [['Key', 'Value'], ['1.0', '2']])[0]['passed']
    codec = [['Key', 'Value'], ['1', '1'], ['2', str(2**25)], ['3', '0']]
    assert decode_scalars(codec) == [1.0]
    changed = [r[:] for r in codec]
    changed[-1][1] = '1'
    assert not compare_csv_tables('Sourcing/MC001/W_scalars001_01.csv', codec, changed)[0]['passed']
    print('PASS: exact CSV, restricted tolerance, key/shape rejection and binary64 codec gates.')


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--root', type=Path)
    p.add_argument('--source', type=Path)
    p.add_argument('--label')
    p.add_argument('--v13', type=Path)
    p.add_argument('--v14', type=Path)
    p.add_argument('--compare-to', default='combined_native')
    p.add_argument('--cases', default=fixtures.DEFAULT_CASES)
    p.add_argument('--engine', type=Path, default=DEFAULT_ENGINE)
    p.add_argument('--rscript', type=Path, default=Path('C:/Program Files/R/R-4.6.0/bin/Rscript.exe'))
    p.add_argument('--processors', type=int, default=1)
    p.add_argument('--timing-qualified', action='store_true')
    p.add_argument('--min-elapsed-reduction', type=float, default=.20)
    p.add_argument('--stage-only', action='store_true')
    p.add_argument('--run-only', action='store_true', help='Run/audit candidates while baseline suite is still running; compare separately afterward.')
    p.add_argument('--compare-only', action='store_true')
    p.add_argument('--self-test', action='store_true')
    args = p.parse_args()
    if args.self_test:
        self_test()
        return
    if sum((args.stage_only, args.run_only, args.compare_only)) > 1:
        p.error('Choose at most one of --stage-only, --run-only and --compare-only.')
    required = ('root', 'label') if args.compare_only else ('root', 'source', 'label', 'v13', 'v14')
    if any(getattr(args, k) is None for k in required):
        p.error('Required arguments: ' + ', '.join('--' + k for k in required))
    args.root = args.root.resolve()
    if 'MoFuSS_Active' not in args.root.parts or args.root.name == 'MoFuSS_Active':
        p.error('Use a named temporary subdirectory under MoFuSS_Active.')
    if any(not re.fullmatch(r'[A-Za-z0-9_-]+', v) for v in (args.label, args.compare_to)):
        p.error('Labels must contain letters, digits, underscore or hyphen.')
    selected = args.cases.split(',')
    if any(case not in fixtures.CASES for case in selected):
        p.error('Unknown case; choices: ' + ','.join(fixtures.CASES))
    if not 0 < args.min_elapsed_reduction < 1 or args.processors < 1:
        p.error('Reduction must be between zero and one; processors must be positive.')
    args.root.mkdir(parents=True, exist_ok=True)
    args.scratch = args.root
    args.engine_arg = ENGINE_FLAGS
    args.disable_native_expressions = False
    records = []
    for case in selected:
        target = (args.root / fixtures.fixture_name(args.label, case) if args.compare_only
                  else fixtures.stage_fixture(args, case))
        if args.stage_only:
            print('STAGED', target.name, flush=True)
            continue
        if not (target / 'runtime_result.json').exists():
            if args.compare_only:
                raise FileNotFoundError('Candidate did not run: ' + str(target))
            print('RUNNING', target.name, flush=True)
            harness.run(argparse.Namespace(root=args.root, name=target.name, engine=args.engine,
                        processors=args.processors, timeout=1200, verify_only=False,
                        disable_native_expressions=False, extra_engine_flags=args.engine_arg))
        previous = json.loads((target / 'runtime_result.json').read_text())
        command = previous['command']
        if previous['returncode'] != 0:
            raise RuntimeError('Candidate run failed; use a fresh label: ' + target.name)
        if not args.compare_only and (Path(command[0]).resolve() != args.engine.resolve()
                or command[command.index('-processors') + 1] != str(args.processors)
                or previous.get('extra_engine_flags', []) != args.engine_arg):
            raise ValueError('Existing evidence used different engine options; use a fresh label.')
        if not args.compare_only:
            fixtures.audit_fixture(args, case, target)
        if args.run_only:
            print('AUDITED; comparison pending:', target.name, flush=True)
        else:
            records.append(compare_pair(args, case))
    if args.run_only:
        print('DONE: candidate suite; run --compare-only to determine engine parity.', flush=True)
        return
    if not args.stage_only:
        result = {'label': args.label, 'cases': selected,
                  'engine_core_parity': all(r['engine_core_parity'] for r in records),
                  'speed_qualified': all(r['speed_qualified'] for r in records),
                  'comparisons': records}
        harness.emit_json(args.root / ('engine_suite_' + args.label + '.json'), result)
        if not result['engine_core_parity']:
            raise SystemExit('FAIL: engine core parity; see detailed comparison CSVs.')
        print('PASS: engine core parity;', args.label, ', '.join(selected), flush=True)


if __name__ == '__main__':
    main()
