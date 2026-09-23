# Runtime sourcing release verification — 23 September 2026

## Installed source identities

| Model | SHA-256 | Role |
|---|---|---|
| v11 | `32d72554b3c6714925ea72e1f9c277d56620a26973fb3ea94cae13fa0825bea8` | Unchanged reference, retained in all four ECSA folders |
| v12 | `823123754b941bb22dde05ca7fc862754df4c9943fdf50163b5b5197b1951a79` | Observation, exact static-base cache, statistics cleanup; old mechanics |
| v13 | `a1d1f4d32cab12fa3ec9682db752180317c81e511114ce5a9ccf3c5e979ed28c` | v12 plus confirmed domestic-W TOF correction; installed release |

v13 was copied and SHA-256 verified in:

- `E:/ECSA_1000m_bau1_2050_mc3_capped`
- `E:/ECSA_1000m_bau1_2050_mc3_uncapped`
- `E:/ECSA_1000m_ics3_2050_mc3_capped`
- `E:/ECSA_1000m_ics3_2050_mc3_uncapped`

The original model, input parameters, demand/IDW inputs and R runtime helpers
were not changed by installation. Small provenance snapshots were added under
each run's `Sourcing/metadata`. No ECSA simulation or IDW computation was run.

## SHA-256 regression gates

The installed Dinamica EGO 2.4.1.20140602 engine executed isolated fixtures on
the original completed LSO grid (269×273 cells). The regional fixture divided
that grid into two artificial countries, with asymmetric domestic W origins
and overlapping V origins. Seven MC input tables were frozen identically;
deterministic Patcher bypass, one engine processor and a predefined engine seed
were used. Identical staging removed four external R initialization/reporting
calls and adjusted only fixture duration, MC count and capped/uncapped mode.

| v11 versus final v12 test | Existing scientific TIFF/CSV files identical |
|---|---:|
| Capped, 3 years × 3 MC | 225 / 225 |
| Uncapped, 3 years × 3 MC | 234 / 234 |
| Capped, 21 years × 1 MC, including decadal transitions | 637 / 637 |
| Two-country regional fixture, 3 years × 3 MC | 225 / 225 |
| Two-country zero-demand endpoint, 3 years × 1 MC | 115 / 115 |
| **Total** | **1,436 / 1,436** |

A separate v11-versus-v11 repeatability gate matched 225/225 files. There were
no missing baseline scientific outputs. New sourcing outputs were additional,
not replacements. Logs, timestamps, TIFF sidecars and reports were not equality
targets. No failed or obsolete experimental model is part of these totals.

## v13 intentional exception and bug proof

The old TOF shortfall branch pooled domestic W demand regionally. A separate
native-engine four-cell test, generated with the production correction code,
proved three cases:

1. Forty units of W pressure previously moved from country A to country B;
   v13 keeps those units in A.
2. With no eligible forest in A, eighty units previously moved to B; v13 leaves
   them unmet.
3. With no TOF shortfall, realized-harvest TIFFs are byte-identical, SHA-256
   `8ab103437a768db66c407565eea5b0281c8901fb872bc712bf734c5ae6f93b75`.

In the complete two-country fixture, v13 intentionally changed 109 of the 225
old outputs as corrected harvest propagated through the biological dynamics.
The sourcing ledger attributed 24,678.49 tonnes of artificial cross-border W
harvest in v12 over 3 years × 3 MC; the corrected v13 ledger attributed zero.
These are synthetic test results, not real-country trade estimates.

The singleton v13 test matched 216/225 files byte-for-byte. The other nine were
`Ex_agr_harv` diagnostic rasters: each changed 915 null cells to zero. All
39,406 common finite cells, all finite sums, and all other harvest/AGB/NRB/scalar
outputs remained identical. This diagnostic null-domain difference is disclosed,
not counted as an exact TIFF match.

## Sourcing reconstruction and input-domain checks

- Exact recorded binary64 scalar encoding: 69/69 bitwise cases passed in each
  of native and interpreted engine modes, including extreme finite values.
- Static optimizer tests: 8 passed; observer/cache graph contracts: 7 passed;
  focused R accounting checks: 25 passed.
- Actual capture reader: 72 MC-years across eight completed fixtures passed
  exact reconstructed W/V pressure checks; v13 redistribution also matched.
- Maximum signed annual origin/source reconciliation residual in these tests:
  0.000421828 tonnes, explicitly retained as float32 accounting residual.
- Zero-demand endpoint: W, V and W+V demand and harvest were zero; domestic and
  imported shares were NA, not zero, NaN, infinity or an inflated percentage.
- All 13 ECSA domestic W masks were checked cell-by-cell: 9,317,660 allowed
  cells, zero overlaps, zero uncovered country cells and zero wrong-country
  cells. Each mask is identical across the four folders. Administrative raster
  SHA-256: `2061dcc0d750eb7a72972dab877ae671de9d708a423617446add977d6182b0ee`.
- Frozen country crosswalks and current/frozen component indices and parameters
  were checked successfully for all four prepared ECSA runs, each with 3 MC.

## Performance and limits

The cache eliminates repeated NPA/analysis-mask raster operations after the
first occurrence of a decadal snapshot. It does not fuse float32 operations or
change the origin summation order. Unneeded statistical summaries are removed.

No full-ECSA speed improvement is claimed. Small fixtures showed fixed capture
and compilation overhead: for example, the two-country 3×3 test took about
159.5 s in v11 and 168.9 s in v12. The small singleton tests were also slower.
Parallel test jobs and compiler/file caching mean these times are not controlled
full-region performance benchmarks.

Full 51-year ECSA production, stochastic Patcher mode, and the external R
reporting lifecycle were not exercised by the equality suite. The production R
bundle itself is unchanged. The evidence establishes the listed test results
and reviewed graph transformations, not universal equality for every possible
dataset or parameter combination. v13 deliberately cannot satisfy old-output
equality where the confirmed domestic-W bug affected results.

## Reproduction and retained records

Source tests/builders are retained alongside this record. Detailed manifests,
all copied-input hashes, output hashes, logs, raster diagnostics and temporary
fixtures are under `E:/MoFuSS_Active/ecsa_sourcing_and_speed_v1`. Key files are
`regression_release_summary.json`, `ecsa_v13_installation.json`,
`domestic_tof_bugproof/bugproof_report.json`, and the `runtime_sourcing_validation`
subfolders. Those temporary materials may be removed later; this verification
record and the reproducible source tests remain in the repository.
