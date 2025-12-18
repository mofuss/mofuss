# MoFuSS
# Version 3
# Date: Mar 2024

# 2dolist ----
# Zoom regions falling outside country borders breaks the map atlas
# If zooms are not present by passzooms drawings

# Internal parameters ----
DEBUG_OSM <- FALSE
RUN_DEMAND_MAPS <- TRUE

# Load libraries ----
library(readr)
library(dplyr)
library(stringr)
library(fs)
library(terra)
library(sf)
library(tmap)
library(classInt)
library(osmdata)

# Detect zoom and dem files ----

base_path <- file.path(countrydir, "LULCC", "DownloadedDatasets")

# ---- Zoom files ----
zoom_kmls <- list.files(
  path = base_path,
  pattern = "zoom.*\\.kml$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(zoom_kmls) < 2) {
  message("Zoom KMLs missing (<2 found). Skipping demand figures.")
  RUN_DEMAND_MAPS <- FALSE
}

zoom1_path <- zoom_kmls[grepl("zoom[_-]?1", basename(zoom_kmls), ignore.case = TRUE)]
zoom2_path <- zoom_kmls[grepl("zoom[_-]?2", basename(zoom_kmls), ignore.case = TRUE)]

# Safety checks
if (length(zoom1_path) != 1)  RUN_DEMAND_MAPS <- FALSE
if (length(zoom2_path) != 1)  RUN_DEMAND_MAPS <- FALSE

if (RUN_DEMAND_MAPS) {
  
  zoom1_path
  zoom2_path
  
  # dem_candidates <- list.files(
  #   path = base_path,
  #   pattern = "DTEM_pcs.*\\.tif$",
  #   recursive = TRUE,
  #   full.names = TRUE,
  #   ignore.case = TRUE
  # )
  # 
  # dem_candidates
  # 
  # if (length(dem_candidates) == 0) {
  #   stop("No DEM found")
  # }
  # 
  # if (length(dem_candidates) > 1) {
  #   message("Multiple DEMs found, using the first one:")
  #   print(dem_candidates)
  # }
  # 
  # dem_path <- dem_candidates[1]
  dem_path <- paste0(countrydir,"/LULCC/DEM_c.tif")
  
  # Read parameters table ----
  detect_delimiter <- function(file_path) {
    first_line <- readLines(file_path, n = 1)
    if (grepl(";", first_line)) ";" else ","
  }
  delimiter <- detect_delimiter(parameters_file_path)
  country_parameters <- readr::read_delim(parameters_file_path, delim = delimiter, show_col_types = FALSE)
  
  region2BprocessedCtry_iso <- country_parameters %>%
    dplyr::filter(Var == "region2BprocessedCtry_iso") %>%
    dplyr::pull(ParCHR) %>%
    .[1]
  
  GEE_scale <- country_parameters %>%
    dplyr::filter(Var == "GEE_scale") %>%
    dplyr::pull(ParCHR) %>%
    .[1]
  
  # Helper: pick GADM layers ----
  pick_gadm_layers <- function(gpkg_path) {
    lay <- sf::st_layers(gpkg_path)$name
    layer0 <- grep("(level_0$|_0$|adm0$|gadm.*_0$)", lay, ignore.case = TRUE, value = TRUE)[1]
    layer1 <- grep("(level_1$|_1$|adm1$|gadm.*_1$)", lay, ignore.case = TRUE, value = TRUE)[1]
    list(adm0 = layer0, adm1 = layer1)
  }
  
  
  # Utilities ----
  
  .check_files <- function(paths) {
    ok <- file.exists(paths)
    if (!all(ok)) stop("Missing file(s):\n  - ", paste(paths[!ok], collapse = "\n  - "))
  }
  
  append_zoom_suffix <- function(out_name, zoom_id = c("zoom1","zoom2")) {
    zoom_id <- match.arg(zoom_id)
    sub("\\.png$", paste0("_", zoom_id, ".png"), out_name, ignore.case = TRUE)
  }
  
  .split_key <- function(path) {
    b <- basename(path)
    m <- str_match(b, "^(.*)_([0-9]{4})([^/]*)\\.tif$")
    if (any(is.na(m))) return(NULL)
    list(prefix = m[2], year = as.integer(m[3]), suffix = m[4])
  }
  
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
  
  .guess_pixel_label <- function(GEE_scale) {
    dplyr::case_when(
      GEE_scale == 100  ~ "1 ha",
      GEE_scale == 1000 ~ "1 km²",
      TRUE             ~ paste0("Pixel size = ", GEE_scale, " m")
    )
  }
  
  .guess_legend <- function(path, pixel_size_label) {
    n <- tolower(basename(path))
    if (grepl("users\\.tif$", n)) return(paste0("People per ", pixel_size_label, " cell"))
    if (grepl("demand\\.tif$", n) && grepl("(fuelwood)", n)) return(paste0("Tonnes of wood per ", pixel_size_label, " cell"))
    if (grepl("demand\\.tif$", n) && grepl("(charcoal)", n)) return(paste0("Tonnes of wood eq per ", pixel_size_label, " cell"))
    if (grepl("demand\\.tif$", n) && grepl("(electricity)", n)) return(paste0("MWh per ", pixel_size_label, " cell"))
    if (grepl("demand\\.tif$", n)) return(paste0("Tonnes of fuel per ", pixel_size_label, " cell"))
    if (grepl("popadj", n)) return(paste0("People per ", pixel_size_label, " cell"))
    if (grepl("rururb", n)) return("0to2 = Rural, ≥2 = Urban")
    paste0("Value per ", pixel_size_label, " cell")
  }
  
  .var_type_from_name <- function(path) {
    n <- basename(path)
    n2 <- sub("\\.tif$", "", n, ignore.case = TRUE)
    n2 <- sub("^WorldPop_", "", n2, ignore.case = TRUE)
    n2 <- sub("_(?:19|20|21)[0-9]{2}(_|$)", "\\1", n2, perl = TRUE)
    parts <- strsplit(n2, "_")[[1]]
    parts <- parts[parts != ""]
    if (length(parts) == 0) return(list(var="layer", type="value"))
    if (length(parts) == 1) return(list(var=parts[1], type="value"))
    list(var = parts[1], type = paste(parts[-1], collapse="_"))
  }
  
  
  ## Hillshade helper ----
  
  make_hillshade <- function(dem_path, r_template) {
    .check_files(dem_path)
    dem <- terra::rast(dem_path)
    
    # project/resample DEM to match template raster
    if (!terra::compareGeom(dem, r_template, stopOnError = FALSE)) {
      dem <- terra::project(dem, r_template, method = "bilinear")
      dem <- terra::resample(dem, r_template, method = "bilinear")
    }
    
    slope  <- terra::terrain(dem, v = "slope",  unit = "radians")
    aspect <- terra::terrain(dem, v = "aspect", unit = "radians")
    terra::shade(slope, aspect, angle = 45, direction = 315) # 0..1
  }
  
  
  ## KML zooms -> bbox polygons in raster CRS ----
  
  read_zoom_bboxes_from_kml <- function(kml1, kml2, r_ref) {
    .check_files(c(kml1, kml2))
    
    z1 <- sf::st_read(kml1, quiet = TRUE)
    z2 <- sf::st_read(kml2, quiet = TRUE)
    
    if (is.na(sf::st_crs(z1))) sf::st_crs(z1) <- 4326
    if (is.na(sf::st_crs(z2))) sf::st_crs(z2) <- 4326
    
    to_bbox_poly <- function(s) {
      bb <- sf::st_bbox(s)
      bb2 <- sf::st_bbox(c(
        xmin = min(bb["xmin"], bb["xmax"]),
        xmax = max(bb["xmin"], bb["xmax"]),
        ymin = min(bb["ymin"], bb["ymax"]),
        ymax = max(bb["ymin"], bb["ymax"])
      ), crs = sf::st_crs(s))
      sf::st_sf(geometry = sf::st_as_sfc(bb2), crs = sf::st_crs(s))
    }
    
    z1_bb <- to_bbox_poly(z1)
    z2_bb <- to_bbox_poly(z2)
    
    crs_target <- sf::st_crs(terra::crs(r_ref, proj = TRUE))
    z1_bb <- sf::st_transform(z1_bb, crs_target)
    z2_bb <- sf::st_transform(z2_bb, crs_target)
    
    dplyr::bind_rows(
      sf::st_sf(id = "zoom1", geometry = sf::st_geometry(z1_bb)),
      sf::st_sf(id = "zoom2", geometry = sf::st_geometry(z2_bb))
    )
  }
  
  
  ## Plan builder ----
  
  build_atlas_plan <- function(demanddir, years = c(2010,2050), country_gid0_default = region2BprocessedCtry_iso) {
    stopifnot(length(years) == 2)
    year_bottom <- min(years); year_top <- max(years)
    
    dirs <- c(file.path(demanddir, "pop_out"),
              file.path(demanddir, "demand_out"))
    
    pairs <- dplyr::bind_rows(lapply(dirs, .find_year_pairs_in_dir, year_bottom = year_bottom, year_top = year_top))
    if (!nrow(pairs)) stop("No 2-year pairs found for years: ", paste(years, collapse = " & "))
    
    demand_files <- fs::dir_ls(file.path(demanddir, "demand_out"), regexp="\\.tif$", type="file")
    if (!length(demand_files)) stop("No tif found in demand_out to infer pixel size label.")
    pixel_size_label <- .guess_pixel_label(GEE_scale = GEE_scale)
    
    plan <- pairs %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        country_gid0 = country_gid0_default,
        year_top = year_top,
        year_bottom = year_bottom,
        legend_title = .guess_legend(tif_top, pixel_size_label),
        vt = list(.var_type_from_name(tif_top)),
        var = vt$var,
        type = vt$type,
        out_name = sprintf("%s_%s_%s_%d_over_%d.png",
                           country_gid0, var, if (type=="") "value" else type, year_top, year_bottom)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(country_gid0, legend_title, year_top, year_bottom, tif_top, tif_bottom, out_name) %>%
      dplyr::distinct(out_name, .keep_all = TRUE)
    
    plan
  }
  
  get_osm_zoom_layers <- function(bb_lonlat, cache_file = NULL, quiet = FALSE) {
    stopifnot(all(c("xmin","ymin","xmax","ymax") %in% names(bb_lonlat)))
    
    if (!is.null(cache_file) && file.exists(cache_file)) {
      if (!quiet) message("Loading OSM cache: ", cache_file)
      return(readRDS(cache_file))
    }
    
    q_roads <- osmdata::opq(bbox = bb_lonlat) |>
      osmdata::add_osm_feature(key = "highway")
    
    roads_sf <- osmdata::osmdata_sf(q_roads)
    
    roads <- NULL
    if (!is.null(roads_sf$osm_lines) && nrow(roads_sf$osm_lines)) roads <- roads_sf$osm_lines
    if (is.null(roads) && !is.null(roads_sf$osm_multilines) && nrow(roads_sf$osm_multilines)) roads <- roads_sf$osm_multilines
    
    q_places <- osmdata::opq(bbox = bb_lonlat) |>
      osmdata::add_osm_feature(key = "place", value = c("city","town","village","hamlet"))
    
    places_sf <- osmdata::osmdata_sf(q_places)
    
    places <- NULL
    if (!is.null(places_sf$osm_points) && nrow(places_sf$osm_points)) places <- places_sf$osm_points
    
    out <- list(roads = roads, places = places)
    
    if (!is.null(cache_file) &&
        ( (!is.null(out$roads) && nrow(out$roads)) || (!is.null(out$places) && nrow(out$places)) )) {
      dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
      saveRDS(out, cache_file)
      if (!quiet) message("Saved OSM cache: ", cache_file)
    }
    
    out
  }
  
  bbox_fingerprint <- function(bb) {
    paste(round(c(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"]), 5), collapse = "_")
  }
  
  
  # Main map function (full + zooms) ----
  two_map_vertical_gadm <- function(
    tif_bottom, tif_top,
    year_bottom, year_top,
    gpkg_path, country_gid0,
    legend_title,
    out_dir, out_name,
    dpi = 600, width_mm = 220, height_mm = 260,
    method = "quantile", k = 7, zero_as_class = TRUE,
    basemap = "adm0_gray", raster_alpha = 0.70,
    show_adm1 = TRUE, adm1_lwd = 0.6, adm1_col = "grey25",
    show_adm0 = TRUE, adm0_lwd = 0.8, adm0_col = "grey40",
    label_adm1 = TRUE, label_size = 0.8,
    
    bboxes_sf = NULL,
    draw_bboxes_on = c("both","bottom","top","none")[1],
    make_zooms = TRUE,
    
    dem_path = NULL,
    hillshade_alpha = 0.35,
    hillshade_only_for_zooms = TRUE,
    
    # ---- OSM options (zooms only) ----
    add_osm = TRUE,
    osm_cache = TRUE,
    osm_roads_lwd = 1.0,
    osm_roads_col = "grey25",
    osm_places_size = 0.75,
    osm_places_col  = "black",
    osm_places_max  = 12,     # keep labels sane
    osm_name_maxchar = 32,    # drop crazy-long names
    osm_drop_nonascii = TRUE, # drop weird encodings that often cause overprint
    
    save = TRUE
  ){
    draw_bboxes_on <- match.arg(draw_bboxes_on)
    tmap::tmap_mode("plot")
    
    r_bot <- terra::rast(tif_bottom)
    r_top <- terra::rast(tif_top)
    if (!terra::compareGeom(r_bot, r_top, stopOnError = FALSE)) {
      r_top <- terra::project(r_top, r_bot, method = "near")
      r_top <- terra::resample(r_top, r_bot, method = "near")
    }
    
    # transparent zeros
    r_bot_c <- r_bot; r_top_c <- r_top
    r_bot_c[r_bot_c < 1] <- NA
    r_top_c[r_top_c < 1] <- NA
    
    # GADM
    layers <- pick_gadm_layers(gpkg_path)
    adm0_all <- sf::st_read(gpkg_path, layer = layers$adm0, quiet = TRUE)
    adm1_all <- sf::st_read(gpkg_path, layer = layers$adm1, quiet = TRUE)
    
    adm0_sf <- adm0_all[adm0_all$GID_0 == country_gid0, , drop = FALSE]
    adm1_sf <- adm1_all[adm1_all$GID_0 == country_gid0, , drop = FALSE]
    
    adm0_sf <- sf::st_transform(adm0_sf, terra::crs(r_bot, proj = TRUE))
    adm1_sf <- sf::st_transform(adm1_sf, terra::crs(r_bot, proj = TRUE))
    
    if (!is.null(bboxes_sf)) {
      bboxes_sf <- sf::st_transform(bboxes_sf, terra::crs(r_bot, proj = TRUE))
    }
    
    # breaks from bottom raster
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
    last_finite <- max(brks_base[is.finite(brks_base)])
    brks <- c(brks_base[brks_base < last_finite], last_finite, Inf)
    
    fmt <- function(x) format(x, big.mark = ",", trim = TRUE, scientific = FALSE)
    labels <- c(
      paste(fmt(head(brks, -2)), "to", fmt(head(tail(brks, -1), -1))),
      paste0("≥ ", fmt(last_finite))
    )
    
    # base background for FULL maps
    base_tm <- NULL
    if (basemap == "adm0_gray" && nrow(adm0_sf) > 0) {
      base_tm <- tm_shape(adm0_sf) + tm_fill("grey85") + tm_borders("grey60", lwd = 0.3)
    }
    
    add_bbox_overlay <- function(tm_obj) {
      if (is.null(bboxes_sf) || draw_bboxes_on == "none") return(tm_obj)
      
      bb1 <- bboxes_sf[bboxes_sf$id == "zoom1", , drop = FALSE]
      bb2 <- bboxes_sf[bboxes_sf$id == "zoom2", , drop = FALSE]
      
      if (nrow(bb1)) {
        z1pt <- sf::st_centroid(sf::st_geometry(bb1))
        z1sf <- sf::st_sf(lbl = "z1", geometry = z1pt, crs = sf::st_crs(bb1))
        tm_obj <- tm_obj +
          tm_shape(bb1) + tm_borders(col = "red", lwd = 2) +
          tm_shape(z1sf) + tm_text("lbl", size = 1.1, col = "black")
      }
      
      if (nrow(bb2)) {
        z2pt <- sf::st_centroid(sf::st_geometry(bb2))
        z2sf <- sf::st_sf(lbl = "z2", geometry = z2pt, crs = sf::st_crs(bb2))
        tm_obj <- tm_obj +
          tm_shape(bb2) + tm_borders(col = "red", lwd = 2) +
          tm_shape(z2sf) + tm_text("lbl", size = 1.1, col = "black")
      }
      
      tm_obj
    }
    
    make_full_map <- function(r, subtitle, draw_bbox = FALSE) {
      tm <- if (is.null(base_tm)) NULL else base_tm
      tm <- tm +
        tm_shape(r) +
        tm_raster(
          style="fixed", breaks=brks, labels=labels, palette="viridis",
          title=legend_title, colorNA="grey90", alpha=raster_alpha
        )
      
      if (show_adm1 && nrow(adm1_sf) > 0) {
        tm <- tm + tm_shape(adm1_sf) + tm_borders(col=adm1_col, lwd=adm1_lwd)
        if (label_adm1 && "NAME_1" %in% names(adm1_sf)) {
          tm <- tm + tm_shape(adm1_sf) + tm_text("NAME_1", size=label_size, col="black", remove.overlap=TRUE)
        }
      }
      if (show_adm0 && nrow(adm0_sf) > 0) {
        tm <- tm + tm_shape(adm0_sf) + tm_borders(col=adm0_col, lwd=adm0_lwd)
      }
      if (draw_bbox) tm <- add_bbox_overlay(tm)
      
      tm +
        tm_compass(position=c("left","top"), size=0.7) +
        tm_scalebar(position=c("left","bottom"), text.size=0.5) +
        tm_layout(
          main.title = as.character(subtitle), main.title.size = 1.2,
          legend.outside=FALSE, legend.position=c("right","top"),
          legend.bg.color="white", legend.bg.alpha=0.95,
          frame=TRUE, inner.margins=c(0.01,0.02,0.02,0.02)
        )
    }
    
    draw_top_bbox <- (draw_bboxes_on %in% c("both","top"))
    draw_bot_bbox <- (draw_bboxes_on %in% c("both","bottom"))
    
    map_top <- make_full_map(r_top_c, year_top, draw_bbox = draw_top_bbox)
    map_bot <- make_full_map(r_bot_c, year_bottom, draw_bbox = draw_bot_bbox)
    combo_full <- tmap_arrange(map_top, map_bot, ncol=1)
    
    if (save) {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      tmap_save(combo_full, filename=file.path(out_dir, out_name),
                dpi=dpi, width=width_mm, height=height_mm, units="mm", asp=0)
    }
    
    # ---- ZOOMS ----
    if (save && make_zooms && !is.null(bboxes_sf) && nrow(bboxes_sf) > 0) {
      
      # hillshade aligned to full raster grid, then cropped per zoom
      hs_full <- NULL
      if (!is.null(dem_path) && file.exists(dem_path)) {
        hs_full <- make_hillshade(dem_path, r_bot_c)
      }
      
      # helper: clean OSM place labels to avoid the "black mess"
      clean_osm_places <- function(places_sf, bbox_poly) {
        if (is.null(places_sf) || !nrow(places_sf)) return(NULL)
        if (!("name" %in% names(places_sf))) return(NULL)
        
        p <- places_sf
        
        # crop to zoom bbox (in 4326 already when fetched)
        p <- sf::st_crop(p, sf::st_bbox(bbox_poly))
        
        nm <- p$name
        nm <- trimws(nm)
        
        # drop empty / too long
        keep <- !is.na(nm) & nzchar(nm) & (nchar(nm) <= osm_name_maxchar)
        
        # drop non-ascii if requested (often fixes the “barcode” effect)
        if (osm_drop_nonascii) {
          keep <- keep & (nm == iconv(nm, to = "ASCII//TRANSLIT"))
        }
        
        p <- p[keep, , drop = FALSE]
        if (!nrow(p)) return(NULL)
        
        # de-duplicate names
        p <- p[!duplicated(p$name), , drop = FALSE]
        if (!nrow(p)) return(NULL)
        
        # keep biggest places first if we have a hint
        # (osmdata usually gives place = city/town; we also use "population" if present)
        score <- rep(0, nrow(p))
        if ("population" %in% names(p)) {
          suppressWarnings(score <- as.numeric(p$population))
          score[!is.finite(score)] <- 0
        } else if ("place" %in% names(p)) {
          score <- ifelse(p$place == "city", 2, ifelse(p$place == "town", 1, 0))
        }
        
        ord <- order(score, decreasing = TRUE)
        p <- p[ord, , drop = FALSE]
        
        # hard cap + spatial thinning
        if (nrow(p) > osm_places_max) p <- p[seq_len(osm_places_max), , drop = FALSE]
        
        # snap duplicates sitting on same coordinate (common)
        coords <- sf::st_coordinates(p)
        key <- paste(round(coords[,1], 5), round(coords[,2], 5))
        p <- p[!duplicated(key), , drop = FALSE]
        
        if (!nrow(p)) return(NULL)
        p
      }
      
      make_zoom_combo <- function(zoom_id) {
        bb <- bboxes_sf[bboxes_sf$id == zoom_id, , drop = FALSE]
        if (!nrow(bb)) return(NULL)
        
        bb_bbox <- sf::st_bbox(bb)
        e <- terra::ext(bb_bbox)
        
        # crop rasters
        r_top_z <- terra::crop(r_top_c, e, snap = "out")
        r_bot_z <- terra::crop(r_bot_c, e, snap = "out")
        
        # crop admin layers
        adm0_z <- sf::st_crop(adm0_sf, bb_bbox)
        adm1_z <- sf::st_crop(adm1_sf, bb_bbox)
        
        # crop hillshade
        hs_z <- NULL
        if (!is.null(hs_full)) {
          hs_z <- terra::crop(hs_full, e, snap = "out")
        }
        
        zoom_label <- if (zoom_id == "zoom1") "Zoom 1" else if (zoom_id == "zoom2") "Zoom 2" else zoom_id
        
        # --- OSM fetch (bbox in lon/lat) ---
        osm_roads <- NULL
        osm_places <- NULL
        
        if (isTRUE(add_osm)) {
          bb_ll <- sf::st_transform(bb, 4326)
          bb_lonlat <- sf::st_bbox(bb_ll)
          
          if (isTRUE(DEBUG_OSM)) {
            
            message("\n--- OSM DEBUG ", zoom_id, " ---")
            print(bb_lonlat)
            message("bbox dx,dy (deg): ",
                    round(bb_lonlat["xmax"] - bb_lonlat["xmin"], 4), ", ",
                    round(bb_lonlat["ymax"] - bb_lonlat["ymin"], 4))
            
            options(osmdata_timeout = 180)  # be generous
            
            # Force verbose errors:
            osm <- NULL
            osm_err <- NULL
            
            osm <- tryCatch(
              {
                get_osm_zoom_layers(bb_lonlat, cache_file = NULL)
              },
              error = function(e) { osm_err <<- e; NULL }
            )
            
            if (is.null(osm)) {
              message("OSM fetch FAILED: ", conditionMessage(osm_err))
            } else {
              message("OSM fetch OK.")
              message("roads:  ", if (is.null(osm$roads)) "NULL" else nrow(osm$roads))
              message("places: ", if (is.null(osm$places)) "NULL" else nrow(osm$places))
            }
            message("--- END OSM DEBUG ---\n")
          }
          
          cache_dir <- file.path(out_dir, "_osm_cache")
          
          cache_file <- file.path(
            cache_dir,
            paste0(
              country_gid0, "_",
              zoom_id, "_",
              bbox_fingerprint(bb_lonlat),
              ".rds"
            )
          )
          
          osm <- NULL
          try({
            if (isTRUE(osm_cache)) {
              osm <- get_osm_zoom_layers(bb_lonlat, cache_file = cache_file)
            } else {
              osm <- get_osm_zoom_layers(bb_lonlat, cache_file = NULL)
            }
          }, silent = TRUE)
          
          if (!is.null(osm)) {
            # roads
            # roads (filtered)
            if (!is.null(osm$roads) && nrow(osm$roads) > 0) {
              
              rd <- sf::st_crop(osm$roads, bb_lonlat)
              
              # keep only major roads
              
              # # Primary only (very clean):
              #   keep_hw <- c("primary","primary_link")
              
              # National-ish network (good default):
              keep_hw <- c("motorway","trunk","primary","secondary",
                           "motorway_link","trunk_link","primary_link","secondary_link")
              
              # # Urban context (busy):
              #   keep_hw <- c("primary","secondary","tertiary","residential","unclassified",
              #                "primary_link","secondary_link","tertiary_link")
              
              if ("highway" %in% names(rd)) rd <- rd[rd$highway %in% keep_hw, , drop = FALSE]
              
              # optional: simplify geometry a bit (speeds plotting + reduces “hair”)
              # increase dTolerance if you want more simplification (units = degrees here, still in lon/lat)
              if (nrow(rd) > 0) rd <- sf::st_simplify(rd, dTolerance = 0.0002, preserveTopology = TRUE)
              
              rd <- sf::st_transform(rd, terra::crs(r_bot, proj = TRUE))
              rd <- sf::st_crop(rd, bb_bbox)
              
              osm_roads <- rd
            }
            
            
            # places (clean + transform)
            if (!is.null(osm$places) && nrow(osm$places) > 0) {
              pl_clean <- clean_osm_places(osm$places, bb_ll)
              if (!is.null(pl_clean) && nrow(pl_clean) > 0) {
                pl_clean <- sf::st_transform(pl_clean, terra::crs(r_bot, proj = TRUE))
                pl_clean <- sf::st_crop(pl_clean, bb_bbox)
                osm_places <- pl_clean
              }
            }
          }
        }
        
        make_zoom_map <- function(r, subtitle) {
          tm <- NULL
          
          # hillshade background (NO legend)
          if (!is.null(hs_z)) {
            tm <- tm +
              tm_shape(hs_z) +
              tm_raster(
                style="cont", palette="-Greys",
                alpha = hillshade_alpha,
                legend.show = FALSE,
                col.legend = tm_legend(show = FALSE)
              )
          } else if (basemap == "adm0_gray" && nrow(adm0_z) > 0) {
            tm <- tm + tm_shape(adm0_z) + tm_fill("grey85") + tm_borders("grey60", lwd = 0.3)
          }
          
          # OSM roads under raster (nice context)
          if (!is.null(osm_roads) && nrow(osm_roads) > 0) {
            tm <- tm + tm_shape(osm_roads) + tm_lines(col = osm_roads_col, lwd = osm_roads_lwd)
          }
          
          # main raster
          tm <- tm +
            tm_shape(r) +
            tm_raster(style="fixed", breaks=brks, labels=labels, palette="viridis",
                      title=legend_title, colorNA=NA, alpha=raster_alpha)
          
          # boundaries (cropped)
          if (show_adm1 && nrow(adm1_z) > 0) tm <- tm + tm_shape(adm1_z) + tm_borders(col=adm1_col, lwd=adm1_lwd)
          if (show_adm0 && nrow(adm0_z) > 0) tm <- tm + tm_shape(adm0_z) + tm_borders(col=adm0_col, lwd=adm0_lwd)
          
          # OSM place labels on top (cleaned + capped)
          if (!is.null(osm_places) && nrow(osm_places) > 0) {
            tm <- tm + tm_shape(osm_places) +
              tm_text(
                "name",
                size = osm_places_size,
                col  = osm_places_col,
                remove.overlap = TRUE,
                just = "center"
              )
          }
          
          tm +
            tm_compass(position=c("left","top"), size=0.7) +
            tm_scalebar(position=c("left","bottom"), text.size=0.5) +
            tm_layout(
              main.title = as.character(paste0(subtitle, "  •  ", zoom_label)),
              main.title.size = 1.2,
              legend.outside=FALSE, legend.position=c("right","top"),
              legend.bg.color="white", legend.bg.alpha=0.95,
              frame=TRUE, inner.margins=c(0.01,0.02,0.02,0.02)
            ) +
            tm_shape(adm0_z)  # extent anchor
        }
        
        map_top_z <- make_zoom_map(r_top_z, year_top)
        map_bot_z <- make_zoom_map(r_bot_z, year_bottom)
        tmap_arrange(map_top_z, map_bot_z, ncol=1)
      }
      
      z1 <- make_zoom_combo("zoom1")
      if (!is.null(z1)) {
        tmap_save(z1, filename=file.path(out_dir, append_zoom_suffix(out_name, "zoom1")),
                  dpi=dpi, width=width_mm, height=height_mm, units="mm", asp=0)
      }
      
      z2 <- make_zoom_combo("zoom2")
      if (!is.null(z2)) {
        tmap_save(z2, filename=file.path(out_dir, append_zoom_suffix(out_name, "zoom2")),
                  dpi=dpi, width=width_mm, height=height_mm, units="mm", asp=0)
      }
    }
    
    invisible(combo_full)
  }
  
  
  
  # Defaults + Runner ----
  
  ATLAS_DEFAULTS <- list(
    gpkg_path     = paste0(admindir,"/gadm_410-levels.gpkg"),
    out_dir       = paste0(countrydir,"/demand_atlas"),
    dem_path      = dem_path,
    hillshade_alpha = 0.35,
    dpi           = 600,
    width_mm      = 220,
    height_mm     = 260,
    method        = "quantile",
    k             = 7,
    zero_as_class = TRUE,
    basemap       = "adm0_gray",
    raster_alpha  = 0.70,
    show_adm1     = TRUE,  adm1_lwd = 0.6, adm1_col = "grey25",
    show_adm0     = TRUE,  adm0_lwd = 0.8, adm0_col = "grey40",
    label_adm1    = TRUE,  label_size = 0.8
  )
  
  run_atlas <- function(plan, defaults = ATLAS_DEFAULTS,
                        skip_first = TRUE, skip_if_exists = FALSE,
                        zoom1_path = NULL, zoom2_path = NULL,
                        draw_bboxes_on = "both",
                        make_zooms = TRUE) {
    
    dir.create(defaults$out_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Load bboxes once (in raster CRS)
    bboxes_sf <- NULL
    if (!is.null(zoom1_path) && !is.null(zoom2_path) && make_zooms) {
      r0 <- terra::rast(plan$tif_bottom[1])
      bboxes_sf <- read_zoom_bboxes_from_kml(zoom1_path, zoom2_path, r_ref = r0)
    }
    
    results <- vector("list", nrow(plan))
    
    for (i in seq_len(nrow(plan))) {
      row <- plan[i, ]
      out_path <- file.path(defaults$out_dir, row$out_name)
      do_save <- !(skip_first && i == 1)
      
      if (skip_if_exists && file.exists(out_path)) {
        cat(sprintf("\n[%d/%d] %s -> exists, skipping\n", i, nrow(plan), row$out_name))
        results[[i]] <- list(row=i, out=out_path, ok=TRUE, saved=FALSE, skipped_existing=TRUE)
        next
      }
      
      cat(sprintf("\n[%d/%d] %s | %d over %d (save=%s)\n",
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
          dpi          = defaults$dpi,
          width_mm     = defaults$width_mm,
          height_mm    = defaults$height_mm,
          method       = defaults$method,
          k            = defaults$k,
          zero_as_class = defaults$zero_as_class,
          basemap      = defaults$basemap,
          raster_alpha = defaults$raster_alpha,
          show_adm1    = defaults$show_adm1,
          adm1_lwd     = defaults$adm1_lwd,
          adm1_col     = defaults$adm1_col,
          show_adm0    = defaults$show_adm0,
          adm0_lwd     = defaults$adm0_lwd,
          adm0_col     = defaults$adm0_col,
          label_adm1   = defaults$label_adm1,
          label_size   = defaults$label_size,
          bboxes_sf    = bboxes_sf,
          draw_bboxes_on = draw_bboxes_on,
          make_zooms   = make_zooms,
          dem_path     = defaults$dem_path,
          hillshade_alpha = defaults$hillshade_alpha,
          save         = do_save
        )
        
        results[[i]] <- list(row=i, out=if (do_save) out_path else NA, ok=TRUE, saved=do_save)
        cat(if (do_save) "  ✔ Saved (and zooms if enabled)\n" else "  ⏭ Rendered, not saved\n")
        
      }, error = function(e) {
        results[[i]] <- list(row=i, out=NA, ok=FALSE, err=conditionMessage(e))
        cat("  ✖ FAILED:", conditionMessage(e), "\n")
      })
    }
    
    invisible(results)
  }
  
  # RUN ----
  
  plan <- build_atlas_plan(
    demanddir = demanddir,
    years = c(2010, 2050),
    country_gid0_default = region2BprocessedCtry_iso
  )
  
  atlas_results <- run_atlas(
    plan,
    skip_first = FALSE,
    skip_if_exists = FALSE,
    zoom1_path = zoom1_path,
    zoom2_path = zoom2_path,
    draw_bboxes_on = "both",
    make_zooms = TRUE
  )
  
  # Summary
  field <- function(x, nm, default = NULL) if (is.null(x[[nm]])) default else x[[nm]]
  ok_idx <- vapply(atlas_results, function(x) isTRUE(field(x, "ok", FALSE)), logical(1))
  cat("\nDone. ✔ Succeeded:", sum(ok_idx), "/", length(atlas_results),
      "| ✖ Failed:", sum(!ok_idx), "/", length(atlas_results), "\n")
  
  
} else {
  message("Demand figures not generated for this run.")
}

