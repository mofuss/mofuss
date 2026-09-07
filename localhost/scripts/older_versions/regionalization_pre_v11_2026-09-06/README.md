# Superseded regionalization pipeline files

This folder preserves the script and test versions superseded by the
origin-preserving W/V v11 pipeline on 2026-09-06. These files are retained for
provenance and reproducibility; they are not part of the active workflow and
must not be copied into new MoFuSS working folders.

The active post-harmonization sequence is:

1. `6_scenarios_v4.R`
2. `7_parameters_dinamica_v1.R`
3. `8_prepare_directional_IDW_inputs_v3.R`
4. External CostDistance_IDW processing on the HPC cluster
5. `9_install_directional_IDW_outputs_v4.R`
6. `10_dyn_Sc17_webmofuss_ctrees_g_v11.egoml`

The active upstream scripts are `2_copy_files_v4.R`,
`3_demand4IDW_v11.R`, and `5_harmonizer_v8.R`.
