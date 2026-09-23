# Dinamica runtime regression

`dinamica_runtime_regression_v1.py` stages independent runtime fixtures beneath
`E:/MoFuSS_Active/ecsa_sourcing_and_speed_v1/runtime_regression`. The default
completed source is supplied explicitly; the ECSA upgrade uses the small LSO
run for full-engine smoke checks. Production directories are only read.

## Scope

The staged model removes four known legacy `RunExternalProcess` functors, which
have no output ports. This freezes the original Monte Carlo parameter tables
and skips R reporting. The same transformation is applied to baseline and
candidate models. New external functors such as sourcing diagnostics are kept.
No core map expressions, numeric data types, null rules, or accumulators are
changed by this staging transformation.

The fixture uses a short test duration by changing only its two parameter CSVs.
The frozen stochastic draws are copied from one completed source run; Dinamica
also receives `-predefined-seed` and the same explicit processor count. The
seed flag does not seed R and would not, by itself, make separately generated
Monte Carlo batches comparable. Each child process receives its own TEMP/TMP
directory to prevent collisions between Dinamica's native-expression DLLs;
the desktop environment is not changed.

## Example

Run from the source repository with the bundled Python executable:

```text
python localhost/scripts/tests/dinamica_runtime_regression_v1.py stage --source E:/LSO_1000m_bau1_2050_mc3_capped --model localhost/scripts/10_dyn_Sc17_webmofuss_ctrees_g_v11.egoml --name baseline_capped --years 3 --mc 3 --uncapped 0
python localhost/scripts/tests/dinamica_runtime_regression_v1.py run --name baseline_capped --verify-only
python localhost/scripts/tests/dinamica_runtime_regression_v1.py run --name baseline_capped
```

Stage the candidate with the same source, years, MC count and mode, using a new
fixture name, and run it. Then:

```text
python localhost/scripts/tests/dinamica_runtime_regression_v1.py compare --left baseline_capped --right candidate_capped
```

Use `--years 21 --mc 1` to cross decennial periods 11 and 21. Use
`--uncapped 1` on both staged models to exercise uncapped regrowth.

## Output and interpretation

Every copied input is SHA-256 checked. The harness records source and staged
model hashes, all staged input hashes, exact engine expressions and map type
ports, engine launch command, elapsed runtime, and newly created scientific
TIFF/CSV hashes. Comparing fixtures requires the frozen inputs to be identical.
The comparison flags missing baseline outputs and every differing byte hash;
new candidate-only scientific outputs are listed separately.

Logs, timestamps, TIFF sidecars and reporting documents are not scientific
output hash targets. TIFF byte-hash differences are failures here even if a
separate investigation later proves their pixel values identical.

These are dynamic-engine equivalence checks for the tested geometry, years,
draws and engine settings. They do not alone prove full ECSA equivalence, test
R report generation, or exercise a regional permission graph when the source
is single-country LSO. A separate regional fixture and representative/full
ECSA checks are required for those claims. Fixtures are never overwritten or
automatically deleted; use a fresh descriptive name for each repeat.
