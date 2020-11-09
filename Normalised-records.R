# This script compiles data and calculate long-term trend for figure 1.

# Calculate the d13C z-score of all WEu mid-latitude records that cover a significant part of the last deglaciation

library(tidyverse)
library(sf)
library(rgeos)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(RColorBrewer)
library(ggsci)
if ("PaleoSpec" %in% rownames(installed.packages()) == F) remotes::install_github("EarthSystemDiagnostics/paleospec")
if ("nest" %in% rownames(installed.packages()) == F) remotes::install_github("krehfeld/nest")
library(nest)

#########################################################################################################################
# Load data ########################################################################################################################

# Load data and count the records (entities)
d <- read.csv("Data/Selected_records.csv") 


# Show the selected cave sites on a map
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot2::theme_set(theme_bw())

worldmap <- ggplot(data = world) + 
  geom_sf() +
  geom_point(data = d, aes(x = longitude, y = latitude), size = 5, 
             shape = 21, fill = "lightpink1") +
  coord_sf(xlim = c(-10, 15), ylim = c(37, 58))  + #, expand = FALSE) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0, "cm"), pad_y = unit(3, "cm"),
                         style = north_arrow_fancy_orienteering) 

CAN <- d %>%
  filter(entity_name == "Candela")

worldmap + geom_point(data = CAN, aes(x = longitude, y = latitude), size = 7, 
                      shape = 23, fill = "orangered2")   


# Plot the original timeseries
original.ts <- 
  ggplot(data = d, aes(x = interp_age, y = d13C_measurement, colour = entity_name)) +
  geom_line() + 
  xlab('Age (U-Th, yr BP)') +
  ylab('d13C (permil)') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) 

original.ts + scale_color_npg() #scale_fill_brewer(palette = "Set2")


# Calculate long term trends using NESTool
# Bind the results to zoo time series

d.small <- select(d, entity_id, interp_age, d13C_measurement) 
entities <- c("29", "32", "166", "180", "563", "572", "587", "650")
for (e in 1:length(entities)) {
  ent <- d.small %>%
    filter(entity_id == entities[e]) 
  ent_d   <- zoo::zoo(select(ent, d13C_measurement), order.by = ent$interp_age)
  trend_d <- gaussbandpass(ent_d,300,1000)$trend
  if (e == 1) {
    all_trends_d <- trend_d
  } else {
    all_trends_d <- merge(all_trends_d,
                          trend_d,
                          all = TRUE, fill = NA, suffixes = NULL)
  }
}
names(all_trends_d) <- paste0("ent", entities)

# Plot long term trends
plot_trends_d <- na.approx(all_trends_d)
autoplot.zoo(plot_trends_d, facets = NULL) + xlab("Age [yr BP]") + ylab("d13C [‰ VPDB]") + labs(color="Record")

# Export long term trends as CSV file 
all_trends_d <- as.data.frame(all_trends_d)
write.csv(all_trends_d,'all_trends_d13C.csv')
