# Choose scenario
Gbau <- "D:/SSA_adm0_mozambique_growth_bau/debugging_1"
Gics <- "D:/SSA_adm0_mozambique_growth_ics/debugging_1"
NGbau <- "D:/SSA_adm0_mozambique_no_growth_bau/debugging_1"
NGics <- "D:/SSA_adm0_mozambique_no_growth_ics/debugging_1"

Gbau <- "D:/amazon_research_1000m_growth_bau/debugging_1"
Gics <- "D:/amazon_research_1000m_growth_ics/debugging_1"
NGbau <- "D:/amazon_research_1000m_no_growth_bau/debugging_1"
NGics <- "D:/amazon_research_1000m_no_growth_ics/debugging_1"


library(terra)

# Growth----

agb2050_Gbau <- rast(paste0(Gbau, "/Growth41.tif"))
agb2020_Gbau <- rast(paste0(Gbau, "/Growth11.tif"))

agb2050_Gics <- rast(paste0(Gics, "/Growth41.tif"))
agb2020_Gics <- rast(paste0(Gics, "/Growth11.tif"))

# Sum all pixel values for each raster
total_agb_2050_Gbau <- global(agb2050_Gbau, sum, na.rm = TRUE)
total_agb_2020_Gbau <- global(agb2020_Gbau, sum, na.rm = TRUE)

total_agb_2050_Gics <- global(agb2050_Gics, sum, na.rm = TRUE)
total_agb_2020_Gics <- global(agb2020_Gics, sum, na.rm = TRUE)

# Print total biomass values
print(total_agb_2050_Gbau)
print(total_agb_2020_Gbau)

print(total_agb_2050_Gics)
print(total_agb_2020_Gics)

seqpot_G <- total_agb_2050_Gics-total_agb_2050_Gbau
seqpot_G

# NO Growth

agb2050_NGbau <- rast(paste0(NGbau, "/Growth41.tif"))
agb2020_NGbau <- rast(paste0(NGbau, "/Growth11.tif"))

agb2050_NGics <- rast(paste0(NGics, "/Growth41.tif"))
agb2020_NGics <- rast(paste0(NGics, "/Growth11.tif"))

# Sum all pixel values for each raster
total_agb_2050_NGbau <- global(agb2050_NGbau, sum, na.rm = TRUE)
total_agb_2020_NGbau <- global(agb2020_NGbau, sum, na.rm = TRUE)

total_agb_2050_NGics <- global(agb2050_NGics, sum, na.rm = TRUE)
total_agb_2020_NGics <- global(agb2020_NGics, sum, na.rm = TRUE)

# Print total biomass values
print(total_agb_2050_NGbau)
print(total_agb_2020_NGbau)

print(total_agb_2050_NGics)
print(total_agb_2020_NGics)

seqpot_NG <- total_agb_2050_NGics-total_agb_2050_NGbau
seqpot_NG


