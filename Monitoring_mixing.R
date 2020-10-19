#Analysis of monitoring data and determination of soil gas end members and mixing lines

library(tidyverse)

monitoring <- read.csv("Data/Monitoring_data.csv") 

#atmospheric end member (pre-industrial)
#atmo_pCO2 <- 280
#atmo_d13C <- -6.5


#Keeling plot of all monitoring data
monitoring.plot <- 
  ggplot(data = monitoring, aes(x = 1/pCO2, y = d13Ccorr, colour = Monitoring_site)) +
  geom_point() + 
  annotate("point", x = 1/280, y = -6.5, colour = "blue", size = 3) +#define atmospheric end member
  annotate("text", x = 1/310, y = -5.8, colour = "blue", label = "atmosphere") +
  xlab('1/pCO2 (1/ppmv)') +
  ylab('d13C (permil)') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) 
monitoring.plot

#Plot regression line through the data except for the "forest" data
