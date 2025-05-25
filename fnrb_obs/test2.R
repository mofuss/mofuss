library(terra)
library(sf)

# Load country polygon
regions <- st_read("D:/admin_regions/mofuss_regions0.gpkg")
kenya <- regions[regions$GID_0 == "IND", ]
kenya_proj <- st_transform(kenya, "EPSG:4326")  # ensure match with rasters

# Load ESA AGB (Mg/ha)
agb_esa_2010 <- rast("C:/Users/aghil/Downloads/agbd_2010_1000m.tif")
agb_esa_2022 <- rast("C:/Users/aghil/Downloads/agbd_2022_1000m.tif")

# Load and convert Ctrees AGB from MgCO2/ha to Mg biomass/ha
agb_ctrees_2010 <- rast("C:/Users/aghil/Downloads/ctrees_global_2010_AGC_pantropic_1km_MgC02_ha.tif") * (12/44) * (1/0.47)
agb_ctrees_2022 <- rast("C:/Users/aghil/Downloads/ctrees_global_2022_AGC_pantropic_1km_MgC02_ha.tif") * (12/44) * (1/0.47)

# Function to compute biomass loss
compute_agb_loss <- function(r2010, r2022, country_poly, label) {
  # Crop and mask
  r2010_clip <- mask(crop(r2010, vect(country_poly)), vect(country_poly))
  r2022_clip <- mask(crop(r2022, vect(country_poly)), vect(country_poly))
  
  # Calculate pixel area in hectares
  area_ha <- cellSize(r2010_clip, unit = "ha")
  
  # Convert Mg/ha → Mg per pixel
  r2010_Mg <- r2010_clip * area_ha
  r2022_Mg <- r2022_clip * area_ha
  
  # Compute only losses (ignore gains)
  loss <- r2010_Mg - r2022_Mg
  loss[loss < 0] <- NA
  
  # Sum and convert to Mt
  total_loss_Mt <- global(loss, "sum", na.rm = TRUE)[1,1] / 1e6
  cat(paste0("Total AGB loss from 2010 to 2022 (", label, "): ", round(total_loss_Mt, 2), " Mt\n"))
  return(total_loss_Mt)
}

# Calculate total losses
esa_loss <- compute_agb_loss(agb_esa_2010, agb_esa_2022, kenya_proj, "ESA")
ctrees_loss <- compute_agb_loss(agb_ctrees_2010, agb_ctrees_2022, kenya_proj, "Ctrees")
3,634,295,476