# Copyright 2025 Stockholm Environment Institute

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0

# Build an auditable list of directional charcoal flows between neighbouring
# countries from the preserved Global South regionalization evidence archive.
#
# This script does not change the MoFuSS regionalization, demand preparation,
# IDW jobs, or model inputs. It produces evidence tables for review.
#
# Inclusion rule for a literature-supported bilateral charcoal direction:
#   1. the countries share a direct land boundary in the reviewed topology;
#   2. the evidence packet or correction claim explicitly concerns charcoal;
#   3. at least one registered document contains the direction claim;
#   4. at least one affirmative physical-flow claim is recorded;
#   5. M67 evidence weight is at least 4; and
#   6. either the direction is current/material or a source-coding correction,
#      or it is a route-specific historical direction that passes the archive's
#      product, physical-movement, direction, and materiality gates.
#
# Formal customs/route microdata without a literature document, fuelwood-only
# evidence, direction-unresolved evidence, and route-partial historical claims
# remain in the audit output. Explicit historical routes are retained in a
# separate evidence tier so that temporal uncertainty is not mistaken for an
# absence of evidence.

options(stringsAsFactors = FALSE)

.stopf <- function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)

.parse_args <- function(args) {
  out <- list(
    archive_dir = "E:/MoFuSS_Global_South_Regionalization_Evidence_Archive_v1_2026-08-29",
    output_dir = "E:/MoFuSS_Active/bilateral_charcoal_evidence_v2",
    candidate_id = "M67_GME_V2",
    min_edge_weight = 4L,
    strict_reference_counts = TRUE
  )

  for (arg in args) {
    if (!grepl("^--[^=]+=", arg)) .stopf("Invalid argument: %s", arg)
    key <- sub("^--([^=]+)=.*$", "\\1", arg)
    value <- sub("^--[^=]+=", "", arg)
    key <- gsub("-", "_", key, fixed = TRUE)
    if (!key %in% names(out)) .stopf("Unknown argument: --%s", gsub("_", "-", key))
    out[[key]] <- value
  }

  out$min_edge_weight <- suppressWarnings(as.integer(out$min_edge_weight))
  if (is.na(out$min_edge_weight) || out$min_edge_weight < 1L || out$min_edge_weight > 5L) {
    .stopf("--min-edge-weight must be an integer from 1 to 5")
  }
  out$strict_reference_counts <- tolower(as.character(out$strict_reference_counts)) %in%
    c("true", "yes", "1")
  out
}

.read_csv <- function(path) {
  if (!file.exists(path)) .stopf("Required evidence file not found: %s", path)
  out <- read.csv(
    path,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = character()
  )
  names(out) <- sub("^\\ufeff", "", names(out))
  names(out) <- sub("^<U\\+FEFF>", "", names(out))
  if (length(names(out)) && grepl("candidate_id", names(out)[[1]], fixed = TRUE)) {
    names(out)[[1]] <- "candidate_id"
  }
  out
}

.require_columns <- function(x, columns, label) {
  missing <- setdiff(columns, names(x))
  if (length(missing)) {
    .stopf("%s is missing required column(s): %s", label, paste(missing, collapse = ", "))
  }
}

.nonempty <- function(x) !is.na(x) & nzchar(trimws(x))

.contains <- function(x, pattern) {
  x[is.na(x)] <- ""
  grepl(pattern, x, ignore.case = TRUE, perl = TRUE)
}

.collapse_unique <- function(x, separator = " | ") {
  x <- trimws(x[.nonempty(x)])
  x <- unique(x)
  if (!length(x)) "" else paste(x, collapse = separator)
}

.lookup <- function(key, table_key, values, default = "") {
  idx <- match(key, table_key)
  out <- values[idx]
  out[is.na(out)] <- default
  out
}

.aggregate_trace <- function(trace, directions) {
  trace <- trace[
    trace$direction %in% directions & trace$source_type == "registered_document",
    , drop = FALSE
  ]
  if (!nrow(trace)) return(data.frame(direction = directions))

  groups <- split(trace, trace$direction)
  rows <- lapply(groups, function(z) {
    data.frame(
      direction = z$direction[[1]],
      document_source_count = length(unique(z$source_id[.nonempty(z$source_id)])),
      document_source_ids = .collapse_unique(z$source_id),
      source_titles = .collapse_unique(z$source_title),
      source_locators = .collapse_unique(z$source_locator),
      source_urls = .collapse_unique(z$source_url),
      archive_relative_paths = .collapse_unique(z$archive_relative_path),
      source_specific_limitations = .collapse_unique(z$source_specific_limitations),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

cfg <- .parse_args(commandArgs(trailingOnly = TRUE))
archive_dir <- normalizePath(cfg$archive_dir, winslash = "/", mustWork = TRUE)
output_dir <- gsub("\\\\", "/", cfg$output_dir)

review_dir <- file.path(archive_dir, "06_review_and_decision_artifacts")
claim_dir <- file.path(archive_dir, "05_claim_and_page_locators")

paths <- list(
  qualifying = file.path(review_dir, "moderate_global_directional_qualifying_directions_v2.csv"),
  membership = file.path(review_dir, "moderate_global_directional_regionalization_membership_v2.csv"),
  trace = file.path(review_dir, "moderate_global_directional_source_traceability_v2.csv"),
  corrections = file.path(review_dir, "moderate_global_source_coding_corrections_v2.csv"),
  dossiers = file.path(claim_dir, "full_direction_evidence_dossiers_v1.csv"),
  packets = file.path(claim_dir, "directed_claim_evidence_packets_v3.csv"),
  gates = file.path(claim_dir, "full_direction_gate_assessment_v1.csv")
)

qualifying <- .read_csv(paths$qualifying)
membership <- .read_csv(paths$membership)
trace <- .read_csv(paths$trace)
corrections <- .read_csv(paths$corrections)
dossiers <- .read_csv(paths$dossiers)
packets <- .read_csv(paths$packets)
gates <- .read_csv(paths$gates)

.require_columns(
  qualifying,
  c(
    "candidate_id", "direction", "exporter_iso3", "importer_iso3",
    "directed_case_disposition", "affirmative_physical_claim_count",
    "edge_weight", "registered_document_source_ids", "derived_dataset_source_ids",
    "physical_route_state", "primary_unresolved_issue", "same_candidate_region",
    "exporter_region_id", "importer_region_id", "G7_status", "evidence_class"
  ),
  basename(paths$qualifying)
)
.require_columns(
  dossiers,
  c("direction", "pair_id", "topology_type"),
  basename(paths$dossiers)
)
.require_columns(
  packets,
  c("direction", "commodity_scopes"),
  basename(paths$packets)
)
.require_columns(
  gates,
  c(
    "direction", "G2_status", "G3_status", "G4_status", "G5_status",
    "G6_status"
  ),
  basename(paths$gates)
)
.require_columns(
  corrections,
  c("direction", "claim", "locator"),
  basename(paths$corrections)
)
.require_columns(
  membership,
  c("iso3", "country"),
  basename(paths$membership)
)
.require_columns(
  trace,
  c(
    "direction", "source_type", "source_id", "source_title", "source_locator",
    "source_url", "archive_relative_path", "source_specific_limitations"
  ),
  basename(paths$trace)
)

qualifying <- qualifying[qualifying$candidate_id == cfg$candidate_id, , drop = FALSE]
if (!nrow(qualifying)) .stopf("No directions found for candidate_id=%s", cfg$candidate_id)
if (anyDuplicated(qualifying$direction)) .stopf("Qualifying-direction table contains duplicate directions")
if (anyDuplicated(membership$iso3)) .stopf("Membership table contains duplicate ISO3 rows")

qualifying$edge_weight <- suppressWarnings(as.integer(qualifying$edge_weight))
qualifying$affirmative_physical_claim_count <- suppressWarnings(
  as.integer(qualifying$affirmative_physical_claim_count)
)

qualifying$pair_id <- .lookup(qualifying$direction, dossiers$direction, dossiers$pair_id)
qualifying$topology_type <- .lookup(
  qualifying$direction, dossiers$direction, dossiers$topology_type
)
qualifying$commodity_scopes <- .lookup(
  qualifying$direction, packets$direction, packets$commodity_scopes
)
qualifying$correction_claim <- .lookup(
  qualifying$direction, corrections$direction, corrections$claim
)
qualifying$correction_locator <- .lookup(
  qualifying$direction, corrections$direction, corrections$locator
)
for (gate_name in c("G2_status", "G3_status", "G4_status", "G5_status", "G6_status")) {
  qualifying[[gate_name]] <- .lookup(
    qualifying$direction, gates$direction, gates[[gate_name]]
  )
}

qualifying$supplier_country <- .lookup(
  qualifying$exporter_iso3, membership$iso3, membership$country
)
qualifying$importer_country <- .lookup(
  qualifying$importer_iso3, membership$iso3, membership$country
)

qualifying$is_direct_land_neighbor <- qualifying$topology_type == "direct_land"
# The three v2 source-coding corrections were manually page-verified charcoal
# claims. One compact paraphrase (COD>BDI) omits the repeated commodity word,
# so a located correction record itself is treated as the commodity assertion.
qualifying$has_charcoal_literature_claim <-
  .contains(qualifying$commodity_scopes, "charcoal") |
  .contains(qualifying$correction_claim, "charcoal") |
  (
    qualifying$directed_case_disposition ==
      "moderate_candidate_source_coding_correction" &
      .nonempty(qualifying$correction_locator)
  )
qualifying$has_charcoal_data_evidence <-
  .contains(
    qualifying$derived_dataset_source_ids,
    "HS4402|COMEXSTAT_ROUTE_MICRODATA|SARS_ROUTE_MICRODATA"
  )
qualifying$has_any_charcoal_evidence <-
  qualifying$has_charcoal_literature_claim |
  qualifying$has_charcoal_data_evidence
qualifying$has_registered_literature <- .nonempty(
  qualifying$registered_document_source_ids
)
qualifying$has_affirmative_physical_claim <-
  !is.na(qualifying$affirmative_physical_claim_count) &
  qualifying$affirmative_physical_claim_count >= 1L

accepted_dispositions <- c(
  "baseline_domain_test_eligible",
  "not_evaluable_due_to_provenance_or_dependency",
  "moderate_candidate_source_coding_correction"
)
qualifying$has_baseline_disposition <-
  qualifying$directed_case_disposition %in% accepted_dispositions
qualifying$meets_weight_threshold <-
  !is.na(qualifying$edge_weight) & qualifying$edge_weight >= cfg$min_edge_weight

qualifying$baseline_permission <- with(
  qualifying,
  is_direct_land_neighbor &
    has_charcoal_literature_claim &
    has_registered_literature &
    has_affirmative_physical_claim &
    has_baseline_disposition &
    meets_weight_threshold
)

# This second path captures explicit, direction-preserving charcoal routes whose
# only failed temporal gate is current continuity. Requiring route-specific
# corroboration and passes at G2/G3/G4/G6 prevents broad regional narratives,
# mixed wood-energy claims, or direction-unresolved interfaces from entering.
qualifying$historical_route_permission <- with(
  qualifying,
  is_direct_land_neighbor &
    has_charcoal_literature_claim &
    has_registered_literature &
    has_affirmative_physical_claim &
    meets_weight_threshold &
    directed_case_disposition == "historical_or_reverse_validation_only" &
    evidence_class == "route_specific_corroboration" &
    G2_status == "pass" &
    G3_status == "pass" &
    G4_status == "pass" &
    G5_status == "fail" &
    G6_status == "pass"
)
qualifying$literature_supported_permission <- with(
  qualifying,
  baseline_permission | historical_route_permission
)

qualifying$evidence_tier <- "Excluded"
qualifying$evidence_tier[
  qualifying$baseline_permission &
    qualifying$directed_case_disposition == "baseline_domain_test_eligible"
] <- "A_corroborated"
qualifying$evidence_tier[
  qualifying$baseline_permission &
    qualifying$directed_case_disposition != "baseline_domain_test_eligible"
] <- "B_explicit_literature"
qualifying$evidence_tier[
  qualifying$historical_route_permission
] <- "C_explicit_historical_route"

qualifying$exclusion_reason <- ""
excluded <- !qualifying$literature_supported_permission
qualifying$exclusion_reason[excluded & !qualifying$is_direct_land_neighbor] <-
  "not_a_direct_land_neighbour_pair"
qualifying$exclusion_reason[
  excluded & qualifying$is_direct_land_neighbor & !qualifying$has_any_charcoal_evidence
] <- "no_charcoal_evidence_in_admitted_direction"
qualifying$exclusion_reason[
  excluded & qualifying$is_direct_land_neighbor & qualifying$has_any_charcoal_evidence &
    !qualifying$has_charcoal_literature_claim
] <- "formal_or_derived_data_only_no_charcoal_literature_claim"
qualifying$exclusion_reason[
  excluded & qualifying$is_direct_land_neighbor & qualifying$has_charcoal_literature_claim &
    !qualifying$has_registered_literature
] <- "charcoal_claim_has_no_registered_literature_source"
qualifying$exclusion_reason[
  excluded & qualifying$is_direct_land_neighbor & qualifying$has_charcoal_literature_claim &
    qualifying$has_registered_literature & !qualifying$has_affirmative_physical_claim
] <- "no_affirmative_physical_flow_claim"
qualifying$exclusion_reason[
  excluded & qualifying$is_direct_land_neighbor & qualifying$has_charcoal_literature_claim &
    qualifying$has_registered_literature & qualifying$has_affirmative_physical_claim &
    !qualifying$meets_weight_threshold
] <- "below_minimum_evidence_weight"
qualifying$exclusion_reason[
  excluded & qualifying$is_direct_land_neighbor & qualifying$has_charcoal_literature_claim &
    qualifying$has_registered_literature & qualifying$has_affirmative_physical_claim &
    qualifying$meets_weight_threshold & !qualifying$has_baseline_disposition
] <- paste0("disposition_", qualifying$directed_case_disposition[
  excluded & qualifying$is_direct_land_neighbor & qualifying$has_charcoal_literature_claim &
    qualifying$has_registered_literature & qualifying$has_affirmative_physical_claim &
    qualifying$meets_weight_threshold & !qualifying$has_baseline_disposition
])

trace_summary <- .aggregate_trace(trace, qualifying$direction)
for (column in setdiff(names(trace_summary), "direction")) {
  qualifying[[column]] <- .lookup(
    qualifying$direction, trace_summary$direction, trace_summary[[column]],
    default = if (column == "document_source_count") 0 else ""
  )
}
qualifying$document_source_count <- as.integer(qualifying$document_source_count)

supported <- qualifying[qualifying$literature_supported_permission, , drop = FALSE]
supported <- supported[
  order(supported$evidence_tier, supported$importer_iso3, supported$exporter_iso3),
]

baseline_output <- data.frame(
  direction = supported$direction,
  supplier_iso3 = supported$exporter_iso3,
  supplier_country = supported$supplier_country,
  importer_iso3 = supported$importer_iso3,
  importer_country = supported$importer_country,
  pair_id = supported$pair_id,
  evidence_tier = supported$evidence_tier,
  edge_weight = supported$edge_weight,
  affirmative_physical_claim_count = supported$affirmative_physical_claim_count,
  temporal_gate_G5 = supported$G5_status,
  independence_gate_G7 = supported$G7_status,
  same_M67_region = supported$same_candidate_region,
  supplier_region_id = supported$exporter_region_id,
  importer_region_id = supported$importer_region_id,
  document_source_count = supported$document_source_count,
  document_source_ids = supported$document_source_ids,
  source_titles = supported$source_titles,
  source_locators = supported$source_locators,
  source_urls = supported$source_urls,
  physical_route_state = supported$physical_route_state,
  unresolved_issue = supported$primary_unresolved_issue,
  recommended_model_use = ifelse(
    supported$evidence_tier == "A_corroborated",
    "baseline_bilateral_permission",
    ifelse(
      supported$evidence_tier == "B_explicit_literature",
      "baseline_permission_with_sensitivity_test",
      "historical_route_permission_with_temporal_sensitivity"
    )
  ),
  stringsAsFactors = FALSE
)

audit_output <- data.frame(
  direction = qualifying$direction,
  supplier_iso3 = qualifying$exporter_iso3,
  supplier_country = qualifying$supplier_country,
  importer_iso3 = qualifying$importer_iso3,
  importer_country = qualifying$importer_country,
  pair_id = qualifying$pair_id,
  topology_type = qualifying$topology_type,
  commodity_scopes = qualifying$commodity_scopes,
  correction_claim = qualifying$correction_claim,
  edge_weight = qualifying$edge_weight,
  directed_case_disposition = qualifying$directed_case_disposition,
  affirmative_physical_claim_count = qualifying$affirmative_physical_claim_count,
  registered_document_source_ids = qualifying$registered_document_source_ids,
  derived_dataset_source_ids = qualifying$derived_dataset_source_ids,
  is_direct_land_neighbor = qualifying$is_direct_land_neighbor,
  has_charcoal_literature_claim = qualifying$has_charcoal_literature_claim,
  has_charcoal_data_evidence = qualifying$has_charcoal_data_evidence,
  has_any_charcoal_evidence = qualifying$has_any_charcoal_evidence,
  has_registered_literature = qualifying$has_registered_literature,
  has_affirmative_physical_claim = qualifying$has_affirmative_physical_claim,
  has_baseline_disposition = qualifying$has_baseline_disposition,
  meets_weight_threshold = qualifying$meets_weight_threshold,
  baseline_permission = qualifying$baseline_permission,
  historical_route_permission = qualifying$historical_route_permission,
  literature_supported_permission = qualifying$literature_supported_permission,
  evidence_tier = qualifying$evidence_tier,
  exclusion_reason = qualifying$exclusion_reason,
  same_M67_region = qualifying$same_candidate_region,
  physical_route_state = qualifying$physical_route_state,
  unresolved_issue = qualifying$primary_unresolved_issue,
  stringsAsFactors = FALSE
)
audit_output <- audit_output[
  order(!audit_output$literature_supported_permission, audit_output$evidence_tier, audit_output$direction),
]

source_output <- trace[
  trace$direction %in% supported$direction & trace$source_type == "registered_document",
  c(
    "direction", "exporter_iso3", "importer_iso3", "source_id", "source_title",
    "publisher_or_corporate_author", "publication_date", "source_url",
    "archive_relative_path", "sha256_verified", "preservation_mode",
    "redistribution_status", "source_locator", "evidence_text",
    "source_specific_limitations"
  ),
  drop = FALSE
]
source_output <- source_output[order(source_output$direction, source_output$source_id), ]

if (anyDuplicated(baseline_output$direction)) {
  .stopf("Baseline output contains duplicate directions")
}
if (!all(supported$is_direct_land_neighbor)) {
  .stopf("Literature-supported output contains a non-neighbour direction")
}
if (!all(supported$has_charcoal_literature_claim)) {
  .stopf("Literature-supported output contains a direction without an explicit charcoal claim")
}
if (!all(supported$has_registered_literature)) {
  .stopf("Literature-supported output contains a direction without a registered literature source")
}
if ("MWI>TZA" %in% baseline_output$direction) {
  .stopf("MWI>TZA must not pass the default bilateral literature rule")
}
if ("AGO>NAM" %in% baseline_output$direction) {
  .stopf("AGO>NAM must not pass a charcoal-only rule; its physical literature claim is fuelwood")
}

if (cfg$strict_reference_counts) {
  if (nrow(qualifying) != 66L) {
    .stopf("Expected 66 M67 admitted directions; found %d", nrow(qualifying))
  }
  if (cfg$min_edge_weight == 4L) {
    if (nrow(baseline_output) != 30L) {
      .stopf("Expected 30 literature-supported charcoal directions; found %d", nrow(baseline_output))
    }
    if (sum(baseline_output$evidence_tier == "A_corroborated") != 5L) {
      .stopf("Expected 5 Tier-A directions")
    }
    if (sum(baseline_output$evidence_tier == "B_explicit_literature") != 7L) {
      .stopf("Expected 7 Tier-B directions")
    }
    if (sum(baseline_output$evidence_tier == "C_explicit_historical_route") != 18L) {
      .stopf("Expected 18 Tier-C explicit historical routes")
    }
  }
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(output_dir)) .stopf("Could not create output directory: %s", output_dir)

baseline_path <- file.path(output_dir, "bilateral_charcoal_permissions_literature_supported_v2.csv")
audit_path <- file.path(output_dir, "bilateral_charcoal_permissions_evidence_audit_v2.csv")
sources_path <- file.path(output_dir, "bilateral_charcoal_permissions_sources_v2.csv")

write.csv(baseline_output, baseline_path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
write.csv(audit_output, audit_path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
write.csv(source_output, sources_path, row.names = FALSE, na = "", fileEncoding = "UTF-8")

cat(sprintf("Archive: %s\n", archive_dir))
cat(sprintf("Candidate: %s\n", cfg$candidate_id))
cat(sprintf("Minimum evidence weight: %d\n", cfg$min_edge_weight))
cat(sprintf("Literature-supported charcoal directions: %d\n", nrow(baseline_output)))
cat(sprintf("  Tier A corroborated: %d\n", sum(baseline_output$evidence_tier == "A_corroborated")))
cat(sprintf("  Tier B explicit literature: %d\n", sum(baseline_output$evidence_tier == "B_explicit_literature")))
cat(sprintf("  Tier C explicit historical routes: %d\n", sum(baseline_output$evidence_tier == "C_explicit_historical_route")))
cat(sprintf("Directions retained only in audit: %d\n", sum(!audit_output$literature_supported_permission)))
cat(sprintf("Baseline output: %s\n", normalizePath(baseline_path, winslash = "/", mustWork = TRUE)))
cat(sprintf("Audit output: %s\n", normalizePath(audit_path, winslash = "/", mustWork = TRUE)))
cat(sprintf("Source output: %s\n", normalizePath(sources_path, winslash = "/", mustWork = TRUE)))
