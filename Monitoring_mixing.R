# Analysis of monitoring data and determination of soil gas end members and mixing lines

library(tidyverse)

#########################################################################################################################
# Load data ########################################################################################################################

monitoring <- read.csv("Data/Monitoring_data.csv")


# Keeling plot of all monitoring data
monitoring.plot <- 
  ggplot(data = monitoring, aes(x = pCO2, y = d13Ccorr, colour = Site)) +
  geom_point() + 
  xlab('pCO2 (ppmv)') +
  ylab('d13C (permil)') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) 
monitoring.plot


# Quick statistical tests on the data to see if linear regression is a good model #################################################################################

# Data without forests and winter months to best estimate the soil respired end member
monitoring_resp <- monitoring %>% 
  filter(Site != 'Forest') %>% 
  filter(Season != 'Winter')

# Scatter plot
scatter.smooth(x=monitoring_resp$inv_pCO2, y=monitoring_resp$d13Ccorr, main="pCO2 ~ d13Ccorr")

# Correlation
# Full dataset
corr <- cor(monitoring$inv_pCO2, monitoring$d13Ccorr) 
# Dataset with summer values only
corr_resp <- cor(monitoring_resp$inv_pCO2, monitoring_resp$d13Ccorr)



######################################
# Linear regression calculation
# Use linear model to predict respired end members with different d13C and pCO2 and intermediate values to feed into CaveCalc

inv_pCO2 <- monitoring_resp$inv_pCO2
d13Ccorr <- monitoring_resp$d13Ccorr

# Create linear model that is forced through the atmosphere as one end member

# Using atmospheric pre-industrial values: pCO2=280 ppm (1/pCO2=0.00357), d13C=-6.5

# Create sequence of pCO2 values to match
mod_invpCO2 <- data.frame(inv_pCO2 = c(seq(1/7800, 1/280, length.out = 21)))#7800
atmo.lm <- lm(d13Ccorr ~ I(inv_pCO2-0.00357)-1, offset = rep(-6.5, length(d13Ccorr))) 
print(atmo.lm)
summary(atmo.lm)

# Predict d13C values based on the regression
d13Cp_atmo <-  predict(atmo.lm, mod_invpCO2, interval="confidence", level = 0.95) #includes 95% confidence interval

# This defined mixing line 1 that follows the linear regression through the monitoring data
reg <- cbind(mod_invpCO2, d13Cp_atmo)

# Extract data for mixing lines to feed into CaveCalc
# Use regression line +/-3 permil for the respired end member to define 3 mixing lines that cover a reasonably broad range
# of possible values (e.g. Bostroem et al., 2007)

# Extract respired end member from mix1 (@7800 ppmv pCO2)
resp_EM1 <- reg %>% filter (inv_pCO2 == 1/7800) %>% select(inv_pCO2, fit) %>% rename(d13C = fit)

# Create new end members based on resp end member d13C +/- 3permil
resp_EM2 <- reg %>% filter (inv_pCO2 == 1/7800) %>% select(inv_pCO2, fit) %>% rename(d13C = fit) %>% mutate(d13C = d13C+3)
resp_EM3 <- reg %>% filter (inv_pCO2 == 1/7800) %>% select(inv_pCO2, fit) %>% rename(d13C = fit) %>% mutate(d13C = d13C-3)

# Create mixing lines for EM1, 2 and 3
# Create sequence of pCO2 values at intervals of 500 ppm
mix_invpCO2 <- 1/(seq(280, 7800, by = 250))

mix1   <- tail(reg$fit, n=1) + ((mix_invpCO2 - tail(reg$inv_pCO2, n=1)) * (resp_EM1$d13C - tail(reg$fit, n=1)) / (resp_EM1$inv_pCO2 - tail(reg$inv_pCO2, n=1)))
mix2   <- tail(reg$fit, n=1) + ((mix_invpCO2 - tail(reg$inv_pCO2, n=1)) * (resp_EM2$d13C - tail(reg$fit, n=1)) / (resp_EM2$inv_pCO2 - tail(reg$inv_pCO2, n=1)))
mix3   <- tail(reg$fit, n=1) + ((mix_invpCO2 - tail(reg$inv_pCO2, n=1)) * (resp_EM3$d13C - tail(reg$fit, n=1)) / (resp_EM3$inv_pCO2 - tail(reg$inv_pCO2, n=1)))  

mix_lines <- as.data.frame(cbind(mix_invpCO2, mix1, mix2, mix3)) #(mix1[1:2], mix2, mix3) 
mix_lines <- rename(mix_lines,  d13C_mix1 = mix1, d13C_mix2 = mix2, d13C_mix3 = mix3) 

monitoring.plot <- 
  ggplot(data = monitoring %>% filter(Season == 'Winter'), aes(x = inv_pCO2, y = d13Ccorr)) + 
  geom_point(colour = "grey") +
  geom_point(data = monitoring %>% filter(Site == 'Forest'), aes(x = inv_pCO2, y = d13Ccorr), colour = "green") + 
  geom_point(data = monitoring_resp, aes(x = inv_pCO2, y = d13Ccorr), size = 2) +
  geom_line(data = reg, aes(x = inv_pCO2, y = fit)) +
  geom_line(data = reg, aes(x = inv_pCO2, y = lwr)) +
  geom_line(data = reg, aes(x = inv_pCO2, y = upr)) +
  geom_line(data = mix_lines, aes(x= mix_invpCO2, y = d13C_mix1), colour = "red") +
  geom_line(data = mix_lines, aes(x= mix_invpCO2, y = d13C_mix2), colour = "blue") +
  geom_line(data = mix_lines, aes(x= mix_invpCO2, y = d13C_mix3), colour = "blue") +
  xlab('1/pCO2 (1/ppmv)') +
  ylab('d13C (permil)') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) 
monitoring.plot


monitoring.plot <- 
  ggplot(data = monitoring %>% filter(Season == 'Winter'), aes(x = pCO2, y = d13Ccorr)) + 
  geom_point(colour = "grey") +
  geom_point(data = monitoring %>% filter(Site == 'Forest'), aes(x = pCO2, y = d13Ccorr), colour = "green") + 
  geom_point(data = monitoring_resp, aes(x = pCO2, y = d13Ccorr), size = 2) +
  geom_line(data = reg, aes(x = 1/inv_pCO2, y = fit)) +
  geom_line(data = reg, aes(x = 1/inv_pCO2, y = lwr)) +
  geom_line(data = reg, aes(x = 1/inv_pCO2, y = upr)) +
  geom_line(data = mix_lines, aes(x= 1/mix_invpCO2, y = d13C_mix1), colour = "red") +
  geom_line(data = mix_lines, aes(x= 1/mix_invpCO2, y = d13C_mix2), colour = "blue") +
  geom_line(data = mix_lines, aes(x= 1/mix_invpCO2, y = d13C_mix3), colour = "blue") +
  xlab('pCO2 (ppmv)') +
  ylab('d13C (permil)') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) 
monitoring.plot

# Save output as csv file
write.csv(mix_lines, file = "Soil_mixing_lines.csv", row.names=FALSE) #mixing lines

write.csv(reg, file = "Monitoring_regression.csv", row.names = FALSE) #regression from monitoring data
