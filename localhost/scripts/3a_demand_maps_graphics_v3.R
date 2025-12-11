# MoFuSS
# Version 3
# Date: Nov 2025

# 2dolist ----
# if (grepl("demand\\.tif$", n) && grepl("(fuelwood|charcoal)", n)) return(paste0("Tonnes of wood eq per ",pixel_size_label," cell"))

# Internal parameters ----

# Load libraries ----
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)
library(openxlsx)  # or writexl if you prefer

# Detect OS
os <- Sys.info()["sysname"]

setwd(countrydir)
getwd()

# Read parameters table ----
# Read parameters table (recognizing the delimiter)
detect_delimiter <- function(file_path) {
  # Read the first line of the file
  first_line <- readLines(file_path, n = 1)
  # Check if the first line contains ',' or ';'
  if (grepl(";", first_line)) {
    return(";")
  } else {
    return(",")
  }
}
# Detect the delimiter
delimiter <- detect_delimiter(parameters_file_path)
# Read the CSV file with the detected delimiter
country_parameters <- read_delim(parameters_file_path, delim = delimiter)
print(tibble::as_tibble(country_parameters), n=100)

country_parameters %>%
  dplyr::filter(Var == "region2BprocessedCtry_iso") %>%
  pull(ParCHR) -> region2BprocessedCtry_iso 

# ============================================
# Auto-plan builder for 2-year pairs (pop_out + demand_out)
# ============================================

# ---- Packages (same as before) ----
pkgs <- c("terra","sf","tmap","classInt","fs","stringr","dplyr")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install, quiet = TRUE)
invisible(lapply(pkgs, require, character.only = TRUE))

# ---- Helper: pick ADM0/ADM1 layer names in your GPKG (unchanged) ----
pick_gadm_layers <- function(gpkg_path) {
  lay <- sf::st_layers(gpkg_path)$name
  layer0 <- grep("(level_0$|_0$|adm0$|gadm.*_0$)", lay, ignore.case = TRUE, value = TRUE)[1]
  layer1 <- grep("(level_1$|_1$|adm1$|gadm.*_1$)", lay, ignore.case = TRUE, value = TRUE)[1]
  list(adm0 = layer0, adm1 = layer1)
}

# ---- UPDATED: more general two-map function with dynamic year labels ----
two_map_vertical_gadm <- function(
    tif_bottom, tif_top,                     # files (any years)
    year_bottom = 2010, year_top = 2050,     # dynamic years for subtitles
    gpkg_path, country_gid0 = region2BprocessedCtry_iso,
    legend_title = "People per 1 ha cell",
    out_dir = "C:/Users/aghil/Downloads", out_name = "map.png",
    dpi = 600, width_mm = 220, height_mm = 260,
    method = c("jenks","quantile","equal")[2], k = 7, zero_as_class = TRUE,
    basemap = c("none","adm0_gray")[2], raster_alpha = 0.70,
    show_adm1 = TRUE, adm1_lwd = 0.6, adm1_col = "grey25",
    show_adm0 = TRUE,  adm0_lwd = 0.3,  adm0_col = "grey40",
    label_adm1 = TRUE, label_size = 0.8,
    save = TRUE
){
  r_bot <- terra::rast(tif_bottom)
  r_top <- terra::rast(tif_top)
  if (!terra::compareGeom(r_bot, r_top, stopOnError = FALSE)) {
    r_top <- terra::project(r_top, r_bot, method = "near"); r_top <- terra::resample(r_top, r_bot, method = "near")
  }
  # transparent zeros
  r_bot_c <- r_bot; r_top_c <- r_top
  r_bot_c[r_bot_c < 1] <- NA
  r_top_c[r_top_c < 1] <- NA
  
  # GADM layers
  layers <- pick_gadm_layers(gpkg_path)
  if (is.na(layers$adm0) || is.na(layers$adm1)) stop("ADM0/ADM1 layers not found in GPKG.")
  adm0_all <- sf::st_read(gpkg_path, layer = layers$adm0, quiet = TRUE)
  adm1_all <- sf::st_read(gpkg_path, layer = layers$adm1, quiet = TRUE)
  stopifnot("GID_0" %in% names(adm0_all), "GID_0" %in% names(adm1_all))
  adm0_sf <- adm0_all[adm0_all$GID_0 == country_gid0, , drop = FALSE]
  adm1_sf <- adm1_all[adm1_all$GID_0 == country_gid0, , drop = FALSE]
  adm0_sf <- sf::st_transform(adm0_sf, terra::crs(r_bot, proj = TRUE))
  adm1_sf <- sf::st_transform(adm1_sf, terra::crs(r_bot, proj = TRUE))
  
  # Breaks from bottom year values (stable legend across both)
  sample_size <- min(2e6, terra::ncell(r_bot_c))
  vbase <- terra::spatSample(r_bot_c, size = sample_size, method = "regular", na.rm = TRUE)[[1]]
  vbase <- vbase[is.finite(vbase)]
  if (zero_as_class) {
    vpos <- vbase[vbase > 0]; if (!length(vpos)) stop("Bottom raster has only 0/NA values.")
    ci <- classInt::classIntervals(vpos, n = k, style = method); brks_base <- c(0, ci$brks)
  } else {
    ci <- classInt::classIntervals(vbase, n = k, style = method); brks_base <- ci$brks
  }
  brks_base <- sort(unique(round(brks_base, 0)))
  if (length(brks_base) < 2) brks_base <- round(range(vbase, na.rm = TRUE), 0)
  last_finite <- max(brks_base[is.finite(brks_base)])
  brks <- c(brks_base[brks_base < last_finite], last_finite, Inf)
  fmt <- function(x) format(x, big.mark = ",", trim = TRUE, scientific = FALSE)
  labels <- c(paste(fmt(head(brks, -2)), "to", fmt(head(tail(brks, -1), -1))),
              paste0("≥ ", fmt(last_finite)))
  
  grid_layer <- if (terra::is.lonlat(r_bot_c)) tm_graticules(n.x=5, n.y=5, labels.size=0.55, lwd=0.25)
  else tm_grid(n.x=5, n.y=5, labels.size=0.55, lwd=0.25)
  tmap_mode("plot"); tmap_options(bg.color = "white")
  
  base_tm <- NULL
  if (basemap == "adm0_gray" && nrow(adm0_sf) > 0) {
    base_tm <- tm_shape(adm0_sf) + tm_fill("grey85") + tm_borders("grey60", lwd = 0.3)
  }
  
  make_map <- function(r, subtitle){
    tm <- if (is.null(base_tm)) NULL else base_tm
    tm <- tm +
      tm_shape(r) + tm_raster(style="fixed", breaks=brks, labels=labels, palette="viridis",
                              title=legend_title, colorNA="grey90", alpha=raster_alpha,
                              legend.format=list(big.mark=",", digits=0))
    if (show_adm1 && nrow(adm1_sf) > 0) {
      tm <- tm + tm_shape(adm1_sf) + tm_borders(col=adm1_col, lwd=adm1_lwd)
      if (label_adm1 && "NAME_1" %in% names(adm1_sf)) {
        tm <- tm + tm_shape(adm1_sf) + tm_text("NAME_1", size=label_size, col="black",
                                               shadow=TRUE, remove.overlap=TRUE)
      }
    }
    if (show_adm0 && nrow(adm0_sf) > 0) tm <- tm + tm_shape(adm0_sf) + tm_borders(col=adm0_col, lwd=adm0_lwd)
    tm + grid_layer +
      tm_compass(position=c("left","top"), size=0.7) +
      tm_scale_bar(position=c("left","bottom"), text.size=0.5) +
      tm_layout(main.title=as.character(subtitle), main.title.size=1.2,
                legend.outside=FALSE, legend.position=c("right","top"), legend.just=c("right","top"),
                legend.bg.color="white", legend.bg.alpha=0.95,
                frame=TRUE, inner.margins=c(0.01,0.02,0.02,0.02))
  }
  
  map_top <- make_map(r_top_c, year_top)
  map_bot <- make_map(r_bot_c, year_bottom)
  combo <- tmap_arrange(map_top, map_bot, ncol=1, heights=c(1,1))
  
  if (save) {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    out_file <- file.path(out_dir, out_name)
    tmap_save(combo, filename=out_file, dpi=dpi, width=width_mm, height=height_mm, units="mm", asp=0)
  }
  invisible(combo)
}

# ---- Utilities ----
.check_files <- function(paths) {
  ok <- file.exists(paths); if (!all(ok)) stop("Missing file(s):\n  - ", paste(paths[!ok], collapse = "\n  - "))
}

# Extract {prefix}{YEAR}{suffix}.tif parts to build a pair key
.split_key <- function(path) {
  b <- basename(path)
  m <- str_match(b, "^(.*)_([0-9]{4})([^/]*)\\.tif$")
  if (any(is.na(m))) return(NULL)
  list(prefix = m[2], year = as.integer(m[3]), suffix = m[4])
}

# Guess legend from filename
files <- list.files(paste0(demanddir, "/demand_out"), 
                    pattern = "\\.tif$", 
                    full.names = TRUE)
r_res <- rast(files[1])   # load the first TIFF
res_xy <- res(r_res)      # returns c(res_x, res_y) in map units
pixel_size_label <- case_when(
  all(res_xy >= 90 & res_xy <= 110) ~ "1 ha",
  all(res_xy >= 900 & res_xy <= 1100) ~ "1 km²",
  TRUE ~ paste0("Pixel size = ", round(res_xy[1]), " m × ", round(res_xy[2]), " m")
)
pixel_size_label

.guess_legend <- function(path) {
  n <- tolower(basename(path))
  if (grepl("users\\.tif$", n)) return(paste0("People per ",pixel_size_label," cell"))
  if (grepl("demand\\.tif$", n) && grepl("(fuelwood)", n)) return(paste0("Tonnes of wood per ",pixel_size_label," cell"))
  if (grepl("demand\\.tif$", n) && grepl("(charcoal)", n)) return(paste0("Tonnes of wood eq per ",pixel_size_label," cell"))
  if (grepl("demand\\.tif$", n) && grepl("(electricity)", n)) return(paste0("MWh per ",pixel_size_label," cell")) # Check this out!!! w/Rob
  if (grepl("demand\\.tif$", n)) return(paste0("Tonnes of fuel per ",pixel_size_label," cell"))
  if (grepl("popadj", n)) return(paste0("People per ",pixel_size_label," cell"))
  if (grepl("rururb", n)) return(paste0("0to2 = Rural, ≥2 = Urban"))
  paste0("DEBUG: Value per ",pixel_size_label," cell")
}

# Variable/type for output name from filename like: WorldPop_biomass_2050_users.tif
.var_type_from_name <- function(path) {
  n <- basename(path)
  n2 <- sub("\\.tif$", "", n, ignore.case = TRUE)
  # remove WorldPop_ if present
  n2 <- sub("^WorldPop_", "", n2, ignore.case = TRUE)
  # drop year
  n2 <- sub("_(?:19|20|21)[0-9]{2}(_|$)", "\\1", n2, perl=TRUE) # keeps trailing underscore if any
  parts <- strsplit(n2, "_")[[1]]
  parts <- parts[parts != ""]
  if (length(parts) == 0) return(list(var="layer", type="value"))
  if (length(parts) == 1) return(list(var=parts[1], type="value"))
  list(var = parts[1], type = paste(parts[-1], collapse="_"))
}

# Build pairs within a single directory for two years
.find_year_pairs_in_dir <- function(dir_path, year_bottom, year_top) {
  files <- fs::dir_ls(dir_path, regexp = "\\.tif$", type = "file")
  if (!length(files)) return(dplyr::tibble())
  meta <- lapply(files, .split_key)
  keep <- !vapply(meta, is.null, logical(1))
  files <- files[keep]; meta <- meta[keep]
  df <- dplyr::tibble(
    file = files,
    prefix = vapply(meta, `[[`, "", "prefix"),
    year = vapply(meta, `[[`, 0L, "year"),
    suffix = vapply(meta, `[[`, "", "suffix")
  )
  # Pairs are defined by same prefix+suffix, with the two target years
  df_bot <- df %>% dplyr::filter(year == year_bottom)
  df_top <- df %>% dplyr::filter(year == year_top)
  key <- function(x) paste0(x$prefix, "___", x$suffix)
  df_pairs <- dplyr::inner_join(
    df_bot %>% dplyr::mutate(key = key(.)),
    df_top %>% dplyr::mutate(key = key(.)),
    by = "key",
    suffix = c("_bot","_top")
  )
  if (!nrow(df_pairs)) return(dplyr::tibble())
  dplyr::tibble(
    tif_bottom = df_pairs$file_bot,
    tif_top    = df_pairs$file_top
  )
}

# ---- MAIN: Build atlas plan automatically from demanddir ----
build_atlas_plan <- function(
    demanddir,
    years = c(2010, 2050),
    country_csv_path = NULL,            # <-- placeholder (you'll wire this)
    country_gid0_default = region2BprocessedCtry_iso        # fallback
){
  stopifnot(length(years) == 2)
  year_bottom <- min(years); year_top <- max(years)
  
  # ----- PLACEHOLDER to read your CSV and pick country_gid0 -----
  # TODO (you): country_tbl <- readr::read_csv(country_csv_path)
  # TODO (you): country_gid0 <- country_tbl |> filter(...) |> pull(GID_0) |> first()
  country_gid0 <- country_gid0_default  # <-- using fallback for now
  
  dirs <- c(file.path(demanddir, "pop_out"),
            file.path(demanddir, "demand_out"))
  pairs_list <- lapply(dirs, .find_year_pairs_in_dir, year_bottom = year_bottom, year_top = year_top)
  pairs <- dplyr::bind_rows(pairs_list)
  if (!nrow(pairs)) stop("No 2-year pairs found in pop_out/ or demand_out/ for years: ",
                         paste(years, collapse = " & "), "\nChecked:\n  - ", paste(dirs, collapse = "\n  - "))
  
  # Fill legend titles and output names
  plan <- pairs %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      legend_title = .guess_legend(tif_top),
      vt = list(.var_type_from_name(tif_top)),
      var = vt$var,
      type = vt$type,
      out_name = sprintf("%s_%s_%s_%d_over_%d.png",
                         country_gid0, var, if (type=="") "value" else type, year_top, year_bottom)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      country_gid0 = country_gid0,
      year_top = year_top,
      year_bottom = year_bottom
    ) %>%
    dplyr::select(country_gid0, legend_title, year_top, year_bottom, tif_top, tif_bottom, out_name) %>%
    dplyr::distinct(out_name, .keep_all = TRUE)  # avoid dup names if any
  
  plan
}

# ---- Defaults (unchanged except function name) ----
ATLAS_DEFAULTS <- list(
  gpkg_path     = paste0(admindir,"/gadm_410-levels.gpkg"),
  out_dir       = paste0(countrydir,"/demand_atlas"),
  dpi           = 600, width_mm = 220, height_mm = 260,
  method        = "quantile", k = 7, zero_as_class = TRUE,
  basemap       = "adm0_gray", raster_alpha = 0.70,
  show_adm1     = TRUE,  adm1_lwd = 0.6, adm1_col = "grey25",
  show_adm0     = TRUE,  adm0_lwd = 0.8, adm0_col = "grey40",
  label_adm1    = TRUE,  label_size = 0.8
)

# ---- Runner (same interface; now calls the UPDATED map func) ----
run_atlas <- function(plan, defaults = ATLAS_DEFAULTS, skip_first = TRUE, skip_if_exists = FALSE) {
  if (!dir.exists(defaults$out_dir)) dir.create(defaults$out_dir, recursive = TRUE)
  results <- vector("list", nrow(plan))
  for (i in seq_len(nrow(plan))) {
    row <- plan[i, ]
    out_path <- file.path(defaults$out_dir, row$out_name)
    do_save <- !(skip_first && i == 1)
    if (skip_if_exists && file.exists(out_path)) {
      cat(sprintf("\n[%d/%d] %s | %s over %s -> exists, skipping\n",
                  i, nrow(plan), row$country_gid0, row$year_top, row$year_bottom))
      results[[i]] <- list(row=i, out=out_path, ok=TRUE, saved=FALSE, skipped_existing=TRUE)
      next
    }
    cat(sprintf("\n[%d/%d] %s | %s over %s (save=%s)\n",
                i, nrow(plan), row$country_gid0, row$year_top, row$year_bottom,
                if (do_save) "yes" else "no"))
    tryCatch({
      .check_files(c(row$tif_top, row$tif_bottom, defaults$gpkg_path))
      two_map_vertical_gadm(
        tif_bottom   = row$tif_bottom,
        tif_top      = row$tif_top,
        year_bottom  = row$year_bottom,
        year_top     = row$year_top,
        gpkg_path    = defaults$gpkg_path,
        country_gid0 = row$country_gid0,
        legend_title = row$legend_title,
        out_dir      = defaults$out_dir,
        out_name     = row$out_name,
        dpi          = defaults$dpi, width_mm = defaults$width_mm, height_mm = defaults$height_mm,
        method       = defaults$method,
        k            = defaults$k,
        zero_as_class = defaults$zero_as_class,
        basemap      = defaults$basemap, raster_alpha = defaults$raster_alpha,
        show_adm1    = defaults$show_adm1, adm1_lwd = defaults$adm1_lwd, adm1_col = defaults$adm1_col,
        show_adm0    = defaults$show_adm0, adm0_lwd = defaults$adm0_lwd, adm0_col = defaults$adm0_col,
        label_adm1   = defaults$label_adm1, label_size = defaults$label_size,
        save = do_save
      )
      results[[i]] <- list(row=i, out=if (do_save) out_path else NA, ok=TRUE, saved=do_save)
      cat(if (do_save) "  ✔ Saved\n" else "  ⏭ Rendered, not saved (first item)\n")
    }, error = function(e) {
      results[[i]] <- list(row=i, out=NA, ok=FALSE, err=conditionMessage(e))
      cat("  ✖ FAILED:", conditionMessage(e), "\n")
    })
  }
  invisible(results)
}


# 1) Build the plan by scanning both folders and pairing e.g. 2010 vs 2050:
plan <- build_atlas_plan(
  demanddir = demanddir,
  years = c(2010, 2050),
  country_csv_path = NULL,           # TODO: set your CSV path later
  country_gid0_default = region2BprocessedCtry_iso       # fallback for now
)

# 2) Run:
atlas_results <- run_atlas(plan, skip_first = FALSE, skip_if_exists = FALSE)

# 3) Summary (same as before)
field <- function(x, nm, default = NULL) if (is.null(x[[nm]])) default else x[[nm]]
ok_idx <- vapply(atlas_results, function(x) isTRUE(field(x, "ok", FALSE)), logical(1))
cat("\nDone. ✔ Succeeded:", sum(ok_idx), "/", length(atlas_results),
    "| ✖ Failed:", sum(!ok_idx), "/", length(atlas_results), "\n")

  
