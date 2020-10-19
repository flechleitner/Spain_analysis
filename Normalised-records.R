#This script compiles data and calculate long-term trend for figure 1.

#Calculate the d13C z-score of all WEu mid-latitude records that cover a significant part of the last deglaciation

library(tidyverse)
library(zoo)
library(nest)
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")
library("ggspatial")
library("RColorBrewer")
library("ggsci")


#Load data and count the entities 
d <- read.csv("Data/Selected_records.csv") 


#Show the selected cave sites on a map
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

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
  
  
#Plot the original timeseries
original.ts <- 
  ggplot(data = d, aes(x = interp_age, y = d13C_measurement, colour = entity_name)) +
  geom_line() + 
  xlab('Age (U-Th, yr BP)') +
  ylab('d13C (permil)') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) 
  
original.ts + scale_color_npg() #scale_fill_brewer(palette = "Set2")


#Calculate long term trend using NESTool
## Bind the results to zoo time series

d.small <- select(d_new, entity_id, interp_age, d13C_measurement, zscore_d13C)

ent_29 <- d.small %>%
          filter(entity_id == "29") 
#zscore_29 <- (ent_29$d13C_measurement - mean(ent_29$d13C_measurement))/sd(ent_29$d13C_measurement)
#ent_29    <- cbind(ent_29,zscore_29)
ent_29_d   <- zoo(select(ent_29, d13C_measurement), order.by = ent_29$interp_age)
ent_29_z   <- zoo(select(ent_29, zscore_d13C), order.by = ent_29$interp_age)
trend_29_d <- gaussbandpass(ent_29_d,300,1000)$trend
trend_29_z <- gaussbandpass(ent_29_z,300,1000)$trend

ent_32 <- d.small %>%
          filter(entity_id == "32")
ent_32_d   <- zoo(select(ent_32, d13C_measurement), order.by = ent_32$interp_age)
ent_32_z   <- zoo(select(ent_32, zscore_d13C), order.by = ent_32$interp_age)
trend_32_d <- gaussbandpass(ent_32_d,300,1000)$trend
trend_32_z <- gaussbandpass(ent_32_z,300,1000)$trend

ent_166 <- d.small %>%
  filter(entity_id == "166")
ent_166_d   <- zoo(select(ent_166, d13C_measurement), order.by = ent_166$interp_age)
ent_166_z   <- zoo(select(ent_166, zscore_d13C), order.by = ent_166$interp_age)
trend_166_d <- gaussbandpass(ent_166_d,300,1000)$trend
trend_166_z <- gaussbandpass(ent_166_z,300,1000)$trend

ent_180 <- d.small %>%
  filter(entity_id == "180")
ent_180_d   <- zoo(select(ent_180, d13C_measurement), order.by = ent_180$interp_age)
ent_180_z   <- zoo(select(ent_180, zscore_d13C),      order.by = ent_180$interp_age)
trend_180_d <- gaussbandpass(ent_180_d,300,1000)$trend
trend_180_z <- gaussbandpass(ent_180_z,300,1000)$trend

ent_563 <- d.small %>%
  filter(entity_id == "563")
ent_563_d   <- zoo(select(ent_563, d13C_measurement), order.by = ent_563$interp_age)
ent_563_z   <- zoo(select(ent_563, zscore_d13C),      order.by = ent_563$interp_age)
trend_563_d <- gaussbandpass(ent_563_d,300,1000)$trend
trend_563_z <- gaussbandpass(ent_563_z,300,1000)$trend

ent_572 <- d.small %>%
  filter(entity_id == "572")
ent_572_d   <- zoo(select(ent_572, d13C_measurement), order.by = ent_572$interp_age)
ent_572_z   <- zoo(select(ent_572, zscore_d13C),      order.by = ent_572$interp_age)
trend_572_d <- gaussbandpass(ent_572_d,300,1000)$trend
trend_572_z <- gaussbandpass(ent_572_z,300,1000)$trend

ent_587 <- d.small %>%
  filter(entity_id == "587")
ent_587_d   <- zoo(select(ent_587, d13C_measurement), order.by = ent_587$interp_age)
ent_587_z   <- zoo(select(ent_587, zscore_d13C),      order.by = ent_587$interp_age)
trend_587_d <- gaussbandpass(ent_587_d,300,1000)$trend
trend_587_z <- gaussbandpass(ent_587_z,300,1000)$trend

ent_650 <- d.small %>%
  filter(entity_id == "650")
ent_650_d   <- zoo(select(ent_650, d13C_measurement), order.by = ent_650$interp_age)
ent_650_z   <- zoo(select(ent_650, zscore_d13C),      order.by = ent_650$interp_age)
trend_650_d <- gaussbandpass(ent_650_d,300,1000)$trend
trend_650_z <- gaussbandpass(ent_650_z,300,1000)$trend

#all_trends <- cbind(trend_29, trend_32, trend_166, trend_180, trend_563, trend_572, trend_587, trend_650, trend_652)

all_trends_d <- merge(trend_29_d, 
                      trend_32_d, 
                      trend_166_d, 
                      trend_180_d, 
                      trend_563_d, 
                      trend_572_d, 
                      trend_587_d, 
                      trend_650_d, 
                     # trend_652_d,
                     all = TRUE, fill = NA, suffixes = NULL,
                     retclass = "data.frame") #c("zoo", "list", "data.frame"))

write.csv(all_trends_d,'all_trends_d13C.csv')


all_trends_z <- merge(trend_29_z, 
                      trend_32_z, 
                      trend_166_z, 
                      trend_180_z, 
                      trend_563_z, 
                      trend_572_z, 
                      trend_587_z, 
                      trend_650_z, 
                      # trend_652_d,
                      all = TRUE, fill = NA, suffixes = NULL,
                      retclass = "data.frame") #c("zoo", "list", "data.frame"))

write.csv(all_trends_z,'all_trends_d13Czscore.csv')


plot(trend_29_d)
lines(trend_32_d,  col = "limegreen")
lines(trend_166_d, col = "red")
lines(trend_180_d, col = "blue")
lines(trend_563_d, col = "orange")
lines(trend_572_d, col = "yellow")
lines(trend_587_d, col = "grey")
lines(trend_650_d, col = "purple")
#lines(trend_652_d, col = "pink")
colrs<-c("black","limegreen","red","blue", "orange", "yellow", "grey", "purple", "pink")
legend("bottom",c("ent29","ent32","ent166","ent180","ent563","ent572","ent587","ent650",  "ent652"),
       col=colrs,lty=1,lwd=c(1,2,2,2))



