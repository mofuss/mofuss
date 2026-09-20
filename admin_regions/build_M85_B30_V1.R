# Build the minimum evidence-connected regionalization required by the accepted
# bilateral charcoal permissions. Direction is not encoded by region membership;
# it remains in bilateral_charcoal_permissions_v1.csv.

candidate_id <- "M85_B30_V1"
permission_set_id <- "B30_V1"

catalog_file <- "regionalization_M67_GME_V2.csv"
permission_file <- "bilateral_charcoal_permissions_v1.csv"

catalog <- read.csv(catalog_file, check.names = FALSE, stringsAsFactors = FALSE)
permissions <- read.csv(permission_file, check.names = FALSE, stringsAsFactors = FALSE)
permissions <- permissions[
  permissions$PermissionSetID == permission_set_id & permissions$ModelDefault,
  ,
  drop = FALSE
]

stopifnot(
  nrow(catalog) == 111L,
  !anyDuplicated(catalog$GID_0),
  nrow(permissions) == 30L,
  !anyDuplicated(permissions$Direction),
  all(permissions$EvidenceTier %in% c("A", "B", "C")),
  all(paste0(permissions$SupplierISO3, ">", permissions$ImporterISO3) ==
        permissions$Direction)
)

country_ids <- sort(unique(catalog$GID_0))
edge_ids <- unique(c(permissions$SupplierISO3, permissions$ImporterISO3))
if (!all(edge_ids %in% country_ids)) {
  stop("Permission endpoints missing from the 111-country catalog: ",
       paste(setdiff(edge_ids, country_ids), collapse = ", "))
}

# Deterministic union-find on the undirected form of the accepted directed graph.
# The resulting components are the smallest computational landscapes containing
# every accepted flow. Directed access is applied later by demand script v13.
parent <- stats::setNames(country_ids, country_ids)
find_root <- function(x) {
  while (!identical(parent[[x]], x)) {
    parent[[x]] <<- parent[[parent[[x]]]]
    x <- parent[[x]]
  }
  x
}
union_nodes <- function(a, b) {
  root_a <- find_root(a)
  root_b <- find_root(b)
  if (!identical(root_a, root_b)) {
    if (root_a < root_b) {
      parent[[root_b]] <<- root_a
    } else {
      parent[[root_a]] <<- root_b
    }
  }
}
for (row_index in seq_len(nrow(permissions))) {
  union_nodes(
    permissions$SupplierISO3[[row_index]],
    permissions$ImporterISO3[[row_index]]
  )
}
component_root <- vapply(country_ids, find_root, character(1))
components <- split(country_ids, component_root)
components <- components[order(
  -vapply(components, length, integer(1)),
  vapply(components, function(x) paste(x, collapse = ";"), character(1))
)]

multi_component_definitions <- list(
  "BDI;BWA;COD;KEN;MOZ;MWI;RWA;SSD;TZA;UGA;ZAF;ZMB;ZWE" = list(
    region_id = "M85_01_ECSA", run_code = "ECSA",
    label = "East-Central-Southern Africa bilateral charcoal network"
  ),
  "CHN;KHM;LAO;MMR;THA;VNM" = list(
    region_id = "M85_02_MSEA", run_code = "MSEA",
    label = "Mainland Southeast Asia bilateral charcoal network"
  ),
  "BFA;CIV;GHA;TGO" = list(
    region_id = "M85_03_GOG4", run_code = "GOG4",
    label = "Ghana-centred Gulf of Guinea bilateral charcoal network"
  ),
  "ARG;PRY" = list(
    region_id = "M85_04_ARPY", run_code = "ARPY",
    label = "Argentina-Paraguay bilateral charcoal region"
  ),
  "BOL;BRA" = list(
    region_id = "M85_05_BOBR", run_code = "BOBR",
    label = "Bolivia-Brazil bilateral charcoal region"
  ),
  "CMR;COG" = list(
    region_id = "M85_06_CMCG", run_code = "CMCG",
    label = "Cameroon-Congo bilateral charcoal region"
  ),
  "DOM;HTI" = list(
    region_id = "M85_07_HISP", run_code = "HISP",
    label = "Hispaniola bilateral charcoal region"
  ),
  "BEN;NGA" = list(
    region_id = "M85_08_BNNG", run_code = "BNNG",
    label = "Benin-Nigeria bilateral charcoal region"
  ),
  "GMB;SEN" = list(
    region_id = "M85_09_GMSN", run_code = "GMSN",
    label = "Gambia-Senegal bilateral charcoal region"
  )
)

component_keys <- vapply(
  components,
  function(x) paste(sort(x), collapse = ";"),
  character(1)
)
observed_multi_keys <- component_keys[vapply(components, length, integer(1)) > 1L]
if (!setequal(observed_multi_keys, names(multi_component_definitions))) {
  stop(
    "Accepted permissions changed the expected connected components. Observed: ",
    paste(observed_multi_keys, collapse = " | ")
  )
}

assignments <- vector("list", length(components))
for (component_index in seq_along(components)) {
  members <- sort(components[[component_index]])
  key <- paste(members, collapse = ";")
  if (length(members) > 1L) {
    definition <- multi_component_definitions[[key]]
    region_id <- definition$region_id
    run_code <- definition$run_code
    region_label <- definition$label
    region_basis <- "minimum_evidence_connected_component"
  } else {
    iso3 <- members[[1L]]
    country_name <- catalog$NAME_0[match(iso3, catalog$GID_0)]
    region_id <- paste0("M85_S_", iso3)
    run_code <- iso3
    region_label <- paste0(country_name, " domestic-only singleton")
    region_basis <- "no_accepted_bilateral_connection"
  }

  internal_edges <- permissions[
    permissions$SupplierISO3 %in% members &
      permissions$ImporterISO3 %in% members,
    ,
    drop = FALSE
  ]
  assignments[[component_index]] <- data.frame(
    GID_0 = members,
    CandidateID = candidate_id,
    CandidateRegionID = region_id,
    RunCode = run_code,
    Subregion = region_label,
    PermissionSetID = permission_set_id,
    RegionBasis = region_basis,
    ComponentSize = length(members),
    AcceptedEdgeCount = nrow(internal_edges),
    stringsAsFactors = FALSE
  )
}
assignments <- do.call(rbind, assignments)

sources_by_tier <- function(importer, tier) {
  values <- sort(unique(permissions$SupplierISO3[
    permissions$ImporterISO3 == importer &
      permissions$EvidenceTier == tier
  ]))
  paste(values, collapse = ";")
}
assignments$VSrcA <- vapply(assignments$GID_0, sources_by_tier, character(1), tier = "A")
assignments$VSrcB <- vapply(assignments$GID_0, sources_by_tier, character(1), tier = "B")
assignments$VSrcC <- vapply(assignments$GID_0, sources_by_tier, character(1), tier = "C")
assignments$ImporterV <- as.integer(
  nzchar(assignments$VSrcA) | nzchar(assignments$VSrcB) | nzchar(assignments$VSrcC)
)
assignments$EvidenceConfidence <- ifelse(
  nzchar(assignments$VSrcA), "tier_A_inbound",
  ifelse(nzchar(assignments$VSrcB), "tier_B_inbound",
         ifelse(nzchar(assignments$VSrcC), "tier_C_inbound", "domestic_only"))
)
assignments$Status <- "model_ready_bilateral_permissions_v1"

output <- merge(
  catalog[, c("MajorRegion", "GID_0", "NAME_0")],
  assignments,
  by = "GID_0",
  all.x = TRUE,
  sort = FALSE
)
output <- output[match(catalog$GID_0, output$GID_0), , drop = FALSE]
output <- output[, c(
  "CandidateID", "MajorRegion", "CandidateRegionID", "RunCode", "Subregion",
  "GID_0", "NAME_0", "ImporterV", "EvidenceConfidence", "Status",
  "PermissionSetID", "RegionBasis", "ComponentSize", "AcceptedEdgeCount",
  "VSrcA", "VSrcB", "VSrcC"
)]

stopifnot(
  nrow(output) == 111L,
  !anyDuplicated(output$GID_0),
  length(unique(output$CandidateRegionID)) == 85L,
  sum(output$ComponentSize > 1L) == 35L,
  max(output$ComponentSize) == 13L
)
region_lookup <- unique(output[, c("GID_0", "CandidateRegionID")])
permission_region_check <- merge(
  permissions,
  region_lookup,
  by.x = "SupplierISO3", by.y = "GID_0"
)
names(permission_region_check)[names(permission_region_check) == "CandidateRegionID"] <-
  "SupplierRegionID"
permission_region_check <- merge(
  permission_region_check,
  region_lookup,
  by.x = "ImporterISO3", by.y = "GID_0"
)
names(permission_region_check)[names(permission_region_check) == "CandidateRegionID"] <-
  "ImporterRegionID"
if (any(permission_region_check$SupplierRegionID !=
        permission_region_check$ImporterRegionID)) {
  stop("At least one accepted flow crosses a generated computational region.")
}

write.csv(output, "regionalization_M85_B30_V1.csv", row.names = FALSE, na = "")

adapter_versions <- c(
  SSA = "subregionsSSA_v7_M85_B30_V1.csv",
  LATAM = "subregionsLATAM_v5_M85_B30_V1.csv",
  ASIA = "subregionsASIA_v7_M85_B30_V1.csv",
  OCEANIA = "subregionsOCEANIA_v3_M85_B30_V1.csv",
  NorAfr = "subregionsNorAfri_v5_M85_B30_V1.csv"
)
adapter_columns <- setdiff(names(output), "MajorRegion")
for (major_region in names(adapter_versions)) {
  adapter <- output[output$MajorRegion == major_region, adapter_columns, drop = FALSE]
  write.csv(adapter, adapter_versions[[major_region]], row.names = FALSE, na = "")
}

message(
  candidate_id, " built: 111 countries, 85 regions, 9 multi-country regions, ",
  "76 singletons, largest component 13 countries."
)
