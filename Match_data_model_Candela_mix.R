# Script to process large numbers of CaveCalc output
# This allows to process the mixing line results from CaveCalc.
# Run once for each mixing line (see "load data")

# Load packages and custom functions
library(tidyverse)
library(gridExtra)
library(viridis)
library(rmatio)
source("functions.R")

#########################################################################################################################
# Load data ########################################################################################################################

# Import original proxy records and CaveCalc output
# Input data coming from the .csv file:

# Load stalagmite measured data
stal_data <- read.csv("Data/CAN_input_lowres_test.csv", stringsAsFactors = F) # MW   stringsAsFactors


# Load CaveCalc model output

# LG mixing lines
#create a list of the files from the target directory
file_list <- list.files(path="Data/CAN_28052021_mix/LG", full.names = T, pattern = glob2rx("*.mat"))

CaveCalc_out_LG <- data.frame()
for (ii in 1:length(file_list)) {
  temp_data       <- read.mat(file_list[ii])
  data_extr       <- CaveCalc_extr(temp_data)
  CaveCalc_out_LG <- rbind(CaveCalc_out_LG, data_extr)
  CaveCalc_out_LG <- CaveCalc_out_LG[CaveCalc_out_LG$d13C!=-999, ] 
}

# DEG mixing lines
#create a list of the files from the target directory
file_list <- list.files(path="Data/CAN_28052021_mix/DEG", full.names = T, pattern = glob2rx("*.mat"))

CaveCalc_out_DG <- data.frame()
for (ii in 1:length(file_list)) {
  temp_data        <- read.mat(file_list[ii])
  data_extr        <- CaveCalc_extr(temp_data)
  CaveCalc_out_DG <- rbind(CaveCalc_out_DG, data_extr)
  CaveCalc_out_DG <- CaveCalc_out_DG[CaveCalc_out_DG$d13C!=-999, ] 
}

# EH mixing lines
#create a list of the files from the target directory
file_list <- list.files(path="Data/CAN_28052021_mix/EH", full.names = T, pattern = glob2rx("*.mat"))

CaveCalc_out_EH <- data.frame()
for (ii in 1:length(file_list)) {
  temp_data        <- read.mat(file_list[ii])
  data_extr        <- CaveCalc_extr(temp_data)
  CaveCalc_out_EH <- rbind(CaveCalc_out_EH, data_extr)
  CaveCalc_out_EH <- CaveCalc_out_EH[CaveCalc_out_EH$d13C!=-999, ] 
}


# Extract the model parameters for each simulation
parameters_EH <- apply(CaveCalc_out_EH[1:5], 2, unique)
parameters_EH
parameters_LG <- apply(CaveCalc_out_LG[1:5], 2, unique)
parameters_LG
parameters_DG <- apply(CaveCalc_out_DG[1:5], 2, unique)
parameters_DG


# Plot the output from the CaveCalc simulations #############################################################################################

# Plot 1: d13C of modelled stalagmite calcite vs. gas volume. Each dot corresponds to one model solution, colour-coded by soil pCO2.
# The two subplots are showing the results for the EH and LGM simulations.
EH_gasvol <- ggplot(data = CaveCalc_out_EH, aes(gas.volume, d13C, colour = soil.pCO2)) + 
  geom_point(alpha = 0.5) + ggtitle("EH") + #theme(legend.position = "none") +
  xlab('Gas volume (l)') +
  ylab('d13C calcite') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) + scale_color_gradientn(colours = viridis(5)) #panel.grid.minor = element_blank(),
LG_gasvol <- ggplot(data = CaveCalc_out_LG, aes(gas.volume, d13C, colour = soil.pCO2)) + 
  geom_point(alpha = 0.5) + ggtitle("LG") +
  xlab('Gas volume (l)') +
  ylab('d13C calcite') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) + scale_color_gradientn(colours = viridis(5))
DG_gasvol <- ggplot(data = CaveCalc_out_DG, aes(gas.volume, d13C, colour = soil.pCO2)) + 
  geom_point(alpha = 0.5) + ggtitle("DG") +
  xlab('Gas volume (l)') +
  ylab('d13C calcite') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) + scale_color_gradientn(colours = viridis(5))#panel.grid.minor = element_blank(),
grid.arrange(EH_gasvol, LG_gasvol, DG_gasvol, ncol= 2)


# Plot 2: Same as plot 1 but for DCP vs. gas volume.
# The two subplots are showing the results for the EH and LGM simulations.
EH_gasvol_DCP <- ggplot(data = CaveCalc_out_EH, aes(gas.volume, DCP, col = soil.pCO2)) + 
  geom_point() + ggtitle("EH") + #theme(legend.position = "none") +
  xlab('Gas volume (l)') +
  ylab('DCP (%)') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) + scale_color_gradientn(colours = viridis(5))
LG_gasvol_DCP <- ggplot(data = CaveCalc_out_LG, aes(gas.volume, DCP, col = soil.pCO2)) + 
  geom_point() + ggtitle("LG") +
  xlab('Gas volume (l)') +
  ylab('DCP (%)') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) + scale_color_gradientn(colours = viridis(5))
DG_gasvol_DCP <- ggplot(data = CaveCalc_out_DG, aes(gas.volume, DCP, col = soil.pCO2)) + 
  geom_point() + ggtitle("DG") +
  xlab('Gas volume (l)') +
  ylab('DCP (%)') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) + scale_color_gradientn(colours = viridis(5))
grid.arrange(EH_gasvol_DCP, LG_gasvol_DCP, DG_gasvol_DCP, ncol = 2) # widths = c(7, 11)


# Plot 3: Same as plot 1 but for d44Ca vs. gas volume.
# The two subplots are showing the results for the EH and LGM simulations.
EH_gasvol_Ca <- ggplot(data = CaveCalc_out_EH, aes(gas.volume, d44Ca, col = soil.pCO2)) + 
  geom_point() + ggtitle("EH") + #theme(legend.position = "none") +
  xlab('Gas volume (l)') +
  ylab('d44Ca calcite') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) + scale_color_gradientn(colours = viridis(5))
LG_gasvol_Ca <- ggplot(data = CaveCalc_out_LG, aes(gas.volume, d44Ca, col = soil.pCO2)) + 
  geom_point() + ggtitle("LG") +
  xlab('Gas volume (l)') +
  ylab('d44Ca calcite') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) + scale_color_gradientn(colours = viridis(5))
DG_gasvol_Ca <- ggplot(data = CaveCalc_out_DG, aes(gas.volume, d44Ca, col = soil.pCO2)) + 
  geom_point() + ggtitle("DG") +
  xlab('Gas volume (l)') +
  ylab('d44Ca calcite') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) + scale_color_gradientn(colours = viridis(5))
grid.arrange(EH_gasvol_Ca, LG_gasvol_Ca, DG_gasvol_Ca, ncol = 2)



# Mixing line analysis #################################################################################

# Calculate solutions based on a single proxy
# Select best fits between stalagmite data and model runs separately for DCP, d44Ca, and d13C.
# The solutions are split based on the dataset to adjust for the different time periods, i.e.: stal_data[1:2,] are data points
# belonging to the Early Holocene and therefore will be matched with model output from that set of simulations (EH). stal_data[3:7,]
# are data points belonging to the Late Glacial and will be matched with model output from the LGM set of simulations.

colnames(stal_data)[2] <- "ts" #Renames "Age" in stal_data to ts for consistency

CaveCalc_out_EH <- tibble::add_column(CaveCalc_out_EH, ts=NA) 
CaveCalc_out_LG <- tibble::add_column(CaveCalc_out_LG, ts=NA)
CaveCalc_out_DG <- tibble::add_column(CaveCalc_out_DG, ts=NA)

proxy.DCP.soln.EH  <- ci_proxy(model = CaveCalc_out_EH, model.col = "DCP", proxy = stal_data[1:2,], proxy.col = "DCP", proxy.age = 2)
proxy.DCP.soln.LG  <- ci_proxy(model = CaveCalc_out_LG, model.col = "DCP", proxy = stal_data[7:7,], proxy.col = "DCP", proxy.age = 2)
proxy.DCP.soln.DG  <- ci_proxy(model = CaveCalc_out_DG, model.col = "DCP", proxy = stal_data[3:6,], proxy.col = "DCP", proxy.age = 2)
proxy.DCP.soln     <- rbind(proxy.DCP.soln.EH, proxy.DCP.soln.LG, proxy.DCP.soln.DG)
proxy.Ca.soln.EH   <- ci_proxy(model = CaveCalc_out_EH, model.col = "d44Ca", proxy = stal_data[1:2,], proxy.col = "d44Ca", proxy.age = 2)
proxy.Ca.soln.LG   <- ci_proxy(model = CaveCalc_out_LG, model.col = "d44Ca", proxy = stal_data[7:7,], proxy.col = "d44Ca", proxy.age = 2)
proxy.Ca.soln.DG   <- ci_proxy(model = CaveCalc_out_DG, model.col = "d44Ca", proxy = stal_data[3:6,], proxy.col = "d44Ca", proxy.age = 2)
proxy.Ca.soln      <- rbind(proxy.Ca.soln.EH, proxy.Ca.soln.LG, proxy.Ca.soln.DG)
proxy.d13C.soln.EH <- ci_proxy(model = CaveCalc_out_EH, model.col = "d13C", proxy = stal_data[1:2,], proxy.col = "d13C", proxy.age = 2)
proxy.d13C.soln.LG <- ci_proxy(model = CaveCalc_out_LG, model.col = "d13C", proxy = stal_data[7:7,], proxy.col = "d13C", proxy.age = 2)
proxy.d13C.soln.DG <- ci_proxy(model = CaveCalc_out_DG, model.col = "d13C", proxy = stal_data[3:6,], proxy.col = "d13C", proxy.age = 2)
proxy.d13C.soln    <- rbind(proxy.d13C.soln.EH, proxy.d13C.soln.LG, proxy.d13C.soln.DG)


# Now we further constrain the solution by finding matches for the other two proxies from a) the d13C solution (solnd13C)
# b) from the DCP solution (solnDCP) and c) from the Ca solution (solnCa).

# Solution 1 and 2 go together: find best matches following d13C -> DCP -> Ca
# solution 1: use d13C solution and find best matches for DCP within that dataset
# Solution d13C
proxy.fin.soln1.EH <- constr_soln(soln = proxy.d13C.soln.EH, soln.col = "DCP", proxy = stal_data[1:2,], proxy.col = "DCP", proxy.age = 2)
proxy.fin.soln1.LG <- constr_soln(soln = proxy.d13C.soln.LG, soln.col = "DCP", proxy = stal_data[7:7,], proxy.col = "DCP", proxy.age = 2)
proxy.fin.soln1.DG <- constr_soln(soln = proxy.d13C.soln.DG, soln.col = "DCP", proxy = stal_data[3:6,], proxy.col = "DCP", proxy.age = 2)
proxy.fin.soln1    <- rbind(proxy.fin.soln1.EH, proxy.fin.soln1.LG, proxy.fin.soln1.DG) 
# solution 2: use solution 1 and find best matches for d44Ca within that dataset
proxy.fin.soln2.EH <- constr_soln(soln = proxy.fin.soln1.EH, soln.col = "d44Ca", proxy = stal_data[1:2,], proxy.col = "d44Ca", proxy.age = 2)
proxy.fin.soln2.LG <- constr_soln(soln = proxy.fin.soln1.LG, soln.col = "d44Ca", proxy = stal_data[7:7,], proxy.col = "d44Ca", proxy.age = 2)
proxy.fin.soln2.DG <- constr_soln(soln = proxy.fin.soln1.DG, soln.col = "d44Ca", proxy = stal_data[3:6,], proxy.col = "d44Ca", proxy.age = 2)
proxy.fin.soln2    <- rbind(proxy.fin.soln2.EH, proxy.fin.soln2.LG, proxy.fin.soln2.DG)

# Solution 3 and 4 go together: find best matches following d13C -> Ca -> DCP
# solution 3: use d13C solution and find best matches for d44Ca within that dataset
proxy.fin.soln3.EH <- constr_soln(soln = proxy.d13C.soln.EH, soln.col = "d44Ca", proxy = stal_data[1:2,], proxy.col = "d44Ca", proxy.age = 2)
proxy.fin.soln3.LG <- constr_soln(soln = proxy.d13C.soln.LG, soln.col = "d44Ca", proxy = stal_data[7:7,], proxy.col = "d44Ca", proxy.age = 2)
proxy.fin.soln3.DG <- constr_soln(soln = proxy.d13C.soln.DG, soln.col = "d44Ca", proxy = stal_data[3:6,], proxy.col = "d44Ca", proxy.age = 2)
proxy.fin.soln3    <- rbind(proxy.fin.soln3.EH, proxy.fin.soln3.LG, proxy.fin.soln3.DG)
# solution 4: use solution 3 and find best matches for DCP within that dataset
proxy.fin.soln4.EH <- constr_soln(soln = proxy.fin.soln3.EH, soln.col = "DCP", proxy = stal_data[1:2,], proxy.col = "DCP", proxy.age = 2)
proxy.fin.soln4.LG <- constr_soln(soln = proxy.fin.soln3.LG, soln.col = "DCP", proxy = stal_data[7:7,], proxy.col = "DCP", proxy.age = 2)
proxy.fin.soln4.DG <- constr_soln(soln = proxy.fin.soln3.DG, soln.col = "DCP", proxy = stal_data[3:6,], proxy.col = "DCP", proxy.age = 2)
proxy.fin.soln4    <- rbind(proxy.fin.soln4.EH, proxy.fin.soln4.LG, proxy.fin.soln4.DG)

# Solution 5 and 6 go together: find best matches following DCP -> Ca -> d13C
# solution 5: use DCP solution and find best matches for d44Ca within that dataset
proxy.fin.soln5.EH <- constr_soln(soln = proxy.DCP.soln.EH, soln.col = "d44Ca", proxy = stal_data[1:2,], proxy.col = "d44Ca", proxy.age = 2) 
proxy.fin.soln5.LG <- constr_soln(soln = proxy.DCP.soln.LG, soln.col = "d44Ca", proxy = stal_data[7:7,], proxy.col = "d44Ca", proxy.age = 2)
proxy.fin.soln5.DG <- constr_soln(soln = proxy.DCP.soln.DG, soln.col = "d44Ca", proxy = stal_data[3:6,], proxy.col = "d44Ca", proxy.age = 2)
proxy.fin.soln5    <- rbind(proxy.fin.soln5.EH, proxy.fin.soln5.LG, proxy.fin.soln5.DG)
# solution 6: use solution 5 and find best matches for d13C within that dataset
proxy.fin.soln6.EH <- constr_soln(soln = proxy.fin.soln5.EH, soln.col = "d13C", proxy = stal_data[1:2,], proxy.col = "d13C", proxy.age = 2) 
proxy.fin.soln6.LG <- constr_soln(soln = proxy.fin.soln5.LG, soln.col = "d13C", proxy = stal_data[7:7,], proxy.col = "d13C", proxy.age = 2)
proxy.fin.soln6.DG <- constr_soln(soln = proxy.fin.soln5.DG, soln.col = "d13C", proxy = stal_data[3:6,], proxy.col = "d13C", proxy.age = 2)
proxy.fin.soln6    <- rbind(proxy.fin.soln6.EH, proxy.fin.soln6.LG, proxy.fin.soln6.DG)

# Solution 7 and 8 go together: find best matches following DCP -> d13C -> Ca
# solution 7: use DCP solution and find best matches for d13C within that dataset
proxy.fin.soln7.EH <- constr_soln(soln = proxy.DCP.soln.EH, soln.col = "d13C", proxy = stal_data[1:2,], proxy.col = "d13C", proxy.age = 2) 
proxy.fin.soln7.LG <- constr_soln(soln = proxy.DCP.soln.LG, soln.col = "d13C", proxy = stal_data[7:7,], proxy.col = "d13C", proxy.age = 2)
proxy.fin.soln7.DG <- constr_soln(soln = proxy.DCP.soln.DG, soln.col = "d13C", proxy = stal_data[3:6,], proxy.col = "d13C", proxy.age = 2)
proxy.fin.soln7    <- rbind(proxy.fin.soln7.EH, proxy.fin.soln7.LG, proxy.fin.soln7.DG)
# solution 8: use solution 7 and find best matches for Ca within that dataset
proxy.fin.soln8.EH <- constr_soln(soln = proxy.fin.soln7.EH, soln.col = "d44Ca", proxy = stal_data[1:2,], proxy.col = "d44Ca", proxy.age = 2) 
proxy.fin.soln8.LG <- constr_soln(soln = proxy.fin.soln7.LG, soln.col = "d44Ca", proxy = stal_data[7:7,], proxy.col = "d44Ca", proxy.age = 2)
proxy.fin.soln8.DG <- constr_soln(soln = proxy.fin.soln7.DG, soln.col = "d44Ca", proxy = stal_data[3:6,], proxy.col = "d44Ca", proxy.age = 2)
proxy.fin.soln8    <- rbind(proxy.fin.soln8.EH, proxy.fin.soln8.LG, proxy.fin.soln8.DG)

# Solution 9 and 10 go together: find best matches following Ca -> DCP -> d13C
# solution 9: use Ca solution and find best matches for DCP within that dataset
proxy.fin.soln9.EH <- constr_soln(soln = proxy.Ca.soln.EH, soln.col = "DCP", proxy = stal_data[1:2,], proxy.col = "DCP", proxy.age = 2) 
proxy.fin.soln9.LG <- constr_soln(soln = proxy.Ca.soln.LG, soln.col = "DCP", proxy = stal_data[7:7,], proxy.col = "DCP", proxy.age = 2)
proxy.fin.soln9.DG <- constr_soln(soln = proxy.Ca.soln.DG, soln.col = "DCP", proxy = stal_data[3:6,], proxy.col = "DCP", proxy.age = 2)
proxy.fin.soln9    <- rbind(proxy.fin.soln9.EH, proxy.fin.soln9.LG, proxy.fin.soln9.DG)
# solution 10: use solution 9 and find best matches for d13C within that dataset
proxy.fin.soln10.EH <- constr_soln(soln = proxy.fin.soln9.EH, soln.col = "d13C", proxy = stal_data[1:2,], proxy.col = "d13C", proxy.age = 2) 
proxy.fin.soln10.LG <- constr_soln(soln = proxy.fin.soln9.LG, soln.col = "d13C", proxy = stal_data[7:7,], proxy.col = "d13C", proxy.age = 2)
proxy.fin.soln10.DG <- constr_soln(soln = proxy.fin.soln9.DG, soln.col = "d13C", proxy = stal_data[3:6,], proxy.col = "d13C", proxy.age = 2)
proxy.fin.soln10    <- rbind(proxy.fin.soln10.EH, proxy.fin.soln10.LG, proxy.fin.soln10.DG)

# Solution 11 and 12 go together: find best matches following Ca -> d13C -> DCP
# solution 11: use Ca solution and find best matches for d13C within that dataset
proxy.fin.soln11.EH <- constr_soln(soln = proxy.Ca.soln.EH, soln.col = "d13C", proxy = stal_data[1:2,], proxy.col = "d13C", proxy.age = 2) 
proxy.fin.soln11.LG <- constr_soln(soln = proxy.Ca.soln.LG, soln.col = "d13C", proxy = stal_data[7:7,], proxy.col = "d13C", proxy.age = 2)
proxy.fin.soln11.DG <- constr_soln(soln = proxy.Ca.soln.DG, soln.col = "d13C", proxy = stal_data[3:6,], proxy.col = "d13C", proxy.age = 2)
proxy.fin.soln11    <- rbind(proxy.fin.soln11.EH, proxy.fin.soln11.LG, proxy.fin.soln11.DG)
# solution 12: use solution 11 and find best matches for DCP within that dataset
proxy.fin.soln12.EH <- constr_soln(soln = proxy.fin.soln11.EH, soln.col = "DCP", proxy = stal_data[1:2,], proxy.col = "DCP", proxy.age = 2) 
proxy.fin.soln12.LG <- constr_soln(soln = proxy.fin.soln11.LG, soln.col = "DCP", proxy = stal_data[7:7,], proxy.col = "DCP", proxy.age = 2)
proxy.fin.soln12.DG <- constr_soln(soln = proxy.fin.soln11.DG, soln.col = "DCP", proxy = stal_data[3:6,], proxy.col = "DCP", proxy.age = 2)
proxy.fin.soln12    <- rbind(proxy.fin.soln12.EH, proxy.fin.soln12.LG, proxy.fin.soln12.DG)

# Calculate median of all solution ensembles for each time step
proxy.fin.soln <- rbind(proxy.fin.soln2, proxy.fin.soln4, proxy.fin.soln6, proxy.fin.soln8, proxy.fin.soln10, 
                        proxy.fin.soln12)


median.fin.soln <- proxy.fin.soln %>%  group_by(ts) %>% 
  summarise(
    DCP        = median(DCP),
    d13C       = median(d13C),
    d44Ca      = median(d44Ca),
    MgCa       = median(MgCa),
    soil.pCO2  = median(soil.pCO2),
    cave.pCO2  = median(cave.pCO2),
    gas.volume = median(gas.volume),
    soil.R14C  = median(soil.R14C),
    soil.d13C  = median(soil.d13C)
  )

upper.quantile.fin.soln <- proxy.fin.soln %>%  group_by(ts) %>% 
  summarise(
    DCP        = quantile(DCP,       probs = .75),
    d13C       = quantile(d13C,      probs = .75),
    d44Ca      = quantile(d44Ca,     probs = .75),
    MgCa       = quantile(MgCa,      probs = .75),
    soil.pCO2  = quantile(soil.pCO2, probs = .75),
    cave.pCO2  = quantile(cave.pCO2, probs = .75),
    gas.volume = quantile(gas.volume,probs = .75),
    soil.R14C  = quantile(soil.R14C, probs = .75),
    soil.d13C  = quantile(soil.d13C, probs = .75)
  )

lower.quantile.fin.soln <- proxy.fin.soln %>%  group_by(ts) %>% 
  summarise(
    DCP        = quantile(DCP,       probs = .25),
    d13C       = quantile(d13C,      probs = .25),
    d44Ca      = quantile(d44Ca,     probs = .25),
    MgCa       = quantile(MgCa,      probs = .25),
    soil.pCO2  = quantile(soil.pCO2, probs = .25),
    cave.pCO2  = quantile(cave.pCO2, probs = .25),
    gas.volume = quantile(gas.volume,probs = .25),
    soil.R14C  = quantile(soil.R14C, probs = .25),
    soil.d13C  = quantile(soil.d13C, probs = .25)
  )

# Calculate the residual of d13C for median solution
res.d13C <- stal_data$d13C - median.fin.soln$d13C
res.d13C.lower <- stal_data$d13C - lower.quantile.fin.soln$d13C
res.d13C.upper <- stal_data$d13C - upper.quantile.fin.soln$d13C

res.d13C <- data.frame(stal_data$ts, res.d13C, res.d13C.upper, res.d13C.lower)
colnames(res.d13C)[1] <- "ts"


# Visualisation of results #################################################################################

# DCP over time. Each dot represents a solution and the colour-coding shows the soil pCO2, cave pCO2, gas volume, and soil R14C
# of that solution.
DCP.soilCO2 <- 
  ggplot(data = proxy.fin.soln, aes(x = ts, y = DCP, colour = soil.pCO2)) +
  geom_point(alpha = 0.7, size = 4) + 
  xlab('Age (U-Th, yr BP)') +
  ylab('DCP (%)') +
  geom_line(data = median.fin.soln, aes(x = ts, y = DCP), color = "blue") +
  geom_line(data = stal_data, aes(x=ts, y= DCP), color= "black") +
  geom_line(data = stal_data, aes(x = ts, y = DCP_uci), color="grey") +
  geom_line(data = stal_data, aes(x = ts, y = DCP_lci), color="grey") +
  geom_point(data = stal_data, aes(x = ts, y = DCP), color = "black", fill = "white", size = 2, stroke = 1) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) + scale_color_gradientn(colours = viridis(5)) #panel.grid.minor = element_blank(),

DCP.caveCO2 <- 
  ggplot(data = proxy.fin.soln, aes(x = ts, y = DCP, colour = cave.pCO2)) +
  geom_point(alpha = 0.7, size = 4) + 
  xlab('Age (U-Th, yr BP)') +
  ylab('DCP (%)') + 
  geom_line(data = median.fin.soln, aes(x = ts, y = DCP), color = "blue") +
  geom_line(data = stal_data, aes(x=ts, y= DCP), color= "black") +
  geom_line(data = stal_data, aes(x = ts, y = DCP_uci), color="grey") +
  geom_line(data = stal_data, aes(x = ts, y = DCP_lci), color="grey") +
  geom_point(data = stal_data, aes(x = ts, y = DCP), color = "black", fill = "white", size = 2, stroke = 1) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) + scale_color_gradientn(colours = magma(5)) #panel.grid.minor = element_blank(),

DCP.gasv <- 
  ggplot(data = proxy.fin.soln, aes(x = ts, y = DCP, colour = gas.volume)) +
  geom_point(alpha = 0.7, size = 4) + 
  xlab('Age (U-Th, yr BP)') +
  ylab('DCP (%)') +
  geom_line(data = median.fin.soln, aes(x = ts, y = DCP), color = "blue") +
  geom_line(data = stal_data, aes(x=ts, y= DCP), color= "black") +
  geom_line(data = stal_data, aes(x = ts, y = DCP_uci), color="grey") +
  geom_line(data = stal_data, aes(x = ts, y = DCP_lci), color="grey") +
  geom_point(data = stal_data, aes(x = ts, y = DCP), color = "black", fill = "white", size = 2, stroke = 1) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) + scale_color_gradientn(colours = cividis(5)) #panel.grid.minor = element_blank(),

DCP.soilage <- 
  ggplot(data = proxy.fin.soln, aes(x = ts, y = DCP, colour = soil.R14C)) +
  geom_point(alpha = 0.7, size = 4) + 
  xlab('Age (U-Th, yr BP)') +
  ylab('DCP (%)') +
  geom_line(data = median.fin.soln, aes(x = ts, y = DCP), color = "blue") +
  geom_line(data = stal_data, aes(x=ts, y= DCP), color= "black") +
  geom_line(data = stal_data, aes(x = ts, y = DCP_uci), color="grey") +
  geom_line(data = stal_data, aes(x = ts, y = DCP_lci), color="grey") +
  geom_point(data = stal_data, aes(x = ts, y = DCP), color = "black", fill = "white", size = 2, stroke = 1) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) + scale_color_gradientn(colours = plasma(5)) #panel.grid.minor = element_blank(),

# Show all plots together
grid.arrange(DCP.soilCO2, DCP.caveCO2, DCP.gasv , DCP.soilage, nrow = 4)


# d44Ca over time. Each dot represents a solution and the colour-coding shows the soil pCO2, cave pCO2, and gas volume of that solution.
Ca.soilCO2 <- 
  ggplot(data = proxy.fin.soln, aes(x = ts, y = d44Ca, colour = soil.pCO2)) +
  geom_point(alpha = 0.7, size = 4) +
  xlab('Age (U-Th, yr BP)') +
  ylab('d44Ca (permil)') +
  geom_line(data = median.fin.soln, aes(x = ts, y = d44Ca), color = "blue") +
  geom_line(data = stal_data, aes(x=ts, y= d44Ca), color= "black") +
  geom_line(data = stal_data, aes(x = ts, y = d44Ca_uci), color="grey") +
  geom_line(data = stal_data, aes(x = ts, y = d44Ca_lci), color="grey") +
  geom_point(data = stal_data, aes(x = ts, y = d44Ca), color = "black", fill = "white", size = 2, stroke = 1) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) + scale_color_gradientn(colours = viridis(5)) #panel.grid.minor = element_blank(),

Ca.caveCO2 <- 
  ggplot(data = proxy.fin.soln, aes(x = ts, y = d44Ca, colour = cave.pCO2)) +
  geom_point(alpha = 0.7, size = 4) +
  xlab('Age (U-Th, yr BP)') +
  ylab('d44Ca (permil)') +
  geom_line(data = median.fin.soln, aes(x = ts, y = d44Ca), color = "blue") +
  geom_line(data = stal_data, aes(x=ts, y= d44Ca), color= "black") +
  geom_line(data = stal_data, aes(x = ts, y = d44Ca_uci), color="grey") +
  geom_line(data = stal_data, aes(x = ts, y = d44Ca_lci), color="grey") +
  geom_point(data = stal_data, aes(x = ts, y = d44Ca), color = "black", fill = "white", size = 2, stroke = 1) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) + scale_color_gradientn(colours = magma(5)) #panel.grid.minor = element_blank(),

Ca.gasv <- 
  ggplot(data = proxy.fin.soln, aes(x = ts, y = d44Ca, colour = gas.volume)) +
  geom_point(alpha = 0.7, size = 4) +
  xlab('Age (U-Th, yr BP)') +
  ylab('d44Ca (permil)') +
  geom_line(data = median.fin.soln, aes(x = ts, y = d44Ca), color = "blue") +
  geom_line(data = stal_data, aes(x=ts, y= d44Ca), color= "black") +
  geom_line(data = stal_data, aes(x = ts, y = d44Ca_uci), color="grey") +
  geom_line(data = stal_data, aes(x = ts, y = d44Ca_lci), color="grey") +
  geom_point(data = stal_data, aes(x = ts, y = d44Ca), color = "black", fill = "white", size = 2, stroke = 1) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) + scale_color_gradientn(colours = cividis(5)) #panel.grid.minor = element_blank(),

# Show all plots together
grid.arrange(Ca.soilCO2, Ca.caveCO2, Ca.gasv, nrow = 3)


# d13C over time. Each dot represents a solution and the colour-coding shows the soil pCO2, cave pCO2, and gas volume of that solution.
d13C.soilCO2 <- 
  ggplot(data = proxy.fin.soln, aes(x = ts, y = d13C, colour = soil.pCO2)) +
  geom_point(alpha = 0.7, size = 4) +
  xlab('Age (U-Th, yr BP)') +
  ylab('d13C (permil)') +
  geom_line(data = median.fin.soln, aes(x = ts, y = d13C), color = "blue") +
  geom_line(data = stal_data, aes(x=ts, y= d13C), color= "black") +
  geom_line(data = stal_data, aes(x = ts, y = d13C_uci), color="grey") +
  geom_line(data = stal_data, aes(x = ts, y = d13C_lci), color="grey") +
  geom_point(data = stal_data, aes(x = ts, y = d13C), color = "black", fill = "white", size = 2, stroke = 1) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) + scale_color_gradientn(colours = viridis(5)) #panel.grid.minor = element_blank(),

d13C.caveCO2 <- 
  ggplot(data = proxy.fin.soln, aes(x = ts, y = d13C, colour = cave.pCO2)) +
  geom_point(alpha = 0.7, size = 4) + 
  xlab('Age (U-Th, yr BP)') +
  ylab('d13C (permil)') +
  geom_line(data = median.fin.soln, aes(x = ts, y = d13C), color = "blue") +
  geom_line(data = stal_data, aes(x=ts, y= d13C), color= "black") +
  geom_line(data = stal_data, aes(x = ts, y = d13C_uci), color="grey") +
  geom_line(data = stal_data, aes(x = ts, y = d13C_lci), color="grey") +
  geom_point(data = stal_data, aes(x = ts, y = d13C), color = "black", fill = "white", size = 2, stroke = 1) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) + scale_color_gradientn(colours = magma(5)) #panel.grid.minor = element_blank(),

d13C.gasv <- 
  ggplot(data = proxy.fin.soln, aes(x = ts, y = d13C, colour = gas.volume)) +
  geom_point(alpha = 0.7, size = 4) + 
  xlab('Age (U-Th, yr BP)') +
  ylab('d13C (permil)') +
  geom_line(data = median.fin.soln, aes(x = ts, y = d13C), color = "blue") +
  geom_line(data = stal_data, aes(x=ts, y= d13C), color= "black") +
  geom_line(data = stal_data, aes(x = ts, y = d13C_uci), color="grey") +
  geom_line(data = stal_data, aes(x = ts, y = d13C_lci), color="grey") +
  geom_point(data = stal_data, aes(x = ts, y = d13C), color = "black", fill = "white", size = 2, stroke = 1) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) + scale_color_gradientn(colours = cividis(5)) #panel.grid.minor = element_blank(),

# Show all plots together
grid.arrange(d13C.soilCO2, d13C.caveCO2, d13C.gasv, nrow = 3)


# Mg/Ca over time. This proxy is not used in the analysis part and only shown here to test how well the models represent it.
Mg.plot <- 
  ggplot(data = proxy.fin.soln, aes(x = ts, y = MgCa)) +
  geom_point(colour = "skyblue4", alpha = 0.7, size = 4) + 
  xlab('Age (U-Th, yr BP)') +
  ylab('Mg/Ca (mmol/mol)') +
  geom_line(data = median.fin.soln, aes(x = ts, y = MgCa), color = "blue") +
  geom_line(data = stal_data, aes(x=ts, y= Mg.Ca), color= "black") +
  geom_point(data = stal_data, aes(x = ts, y = Mg.Ca), color = "black", fill = "white", size = 2, stroke = 1) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) 
Mg.plot


# Residual d13C over time
resd13C.plot <-
  ggplot(data = res.d13C, aes(x = ts, y = res.d13C)) +
  geom_line(colour = "blue", size = 1) +
  geom_line(data   = res.d13C, aes(x = ts, y = res.d13C.lower), color="dodgerblue1") +  
  geom_line(data   = res.d13C, aes(x = ts, y = res.d13C.upper), color="dodgerblue1") +  
  geom_point(data  = res.d13C, aes(x = ts, y = res.d13C), color = "blue", size = 3) +
  xlab('Age (U-Th, yr BP)') +
  ylab('Residual d13C (permil)') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) 
resd13C.plot  


# Plots of the model parameters as selected from the best fits to the proxies.
# Soil pCO2 over time.
ggplot() +
  geom_point(data = proxy.fin.soln,  aes(x = ts,  y = soil.pCO2), color = "blue")+
  geom_line (data = median.fin.soln, aes(x = ts,  y = soil.pCO2), color = "blue")+
  xlab('Age (U-Th, yr BP)') +
  ylab('Soil air pCO2 (ppmv)') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) 

# Cave pCO2 over time. 
ggplot() +
  geom_point(data = proxy.fin.soln,  aes(x = ts,  y = cave.pCO2), color = "blue")+
  geom_line (data = median.fin.soln, aes(x = ts,  y = cave.pCO2), color = "blue")+
  xlab('Age (U-Th, yr BP)') +
  ylab('Cave air pCO2 (ppmv)') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) 

# Gas volume over time. 
ggplot() +
  geom_point(data = proxy.fin.soln,  aes(x = ts,  y = gas.volume), color = "blue")+
  geom_line (data = median.fin.soln, aes(x = ts,  y = gas.volume), color = "blue")+
  xlab('Age (U-Th, yr BP)') +
  ylab('Gas Volume (L)') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) 

# Soil age over time. 
ggplot() +
  geom_point(data = proxy.fin.soln,  aes(x = ts,  y = soil.R14C), color = "blue")+
  geom_line (data = median.fin.soln, aes(x = ts,  y = soil.R14C), color = "blue")+
  xlab('Age (U-Th, yr BP)') +
  ylab('Soil R14C') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) 


# Soil d13C over time. 
ggplot() +
  geom_point(data = proxy.fin.soln,  aes(x = ts,  y = soil.d13C), color = "blue")+
  geom_line (data = median.fin.soln, aes(x = ts,  y = soil.d13C), color = "blue")+
  xlab('Age (U-Th, yr BP)') +
  ylab('Soil d13C') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     axis.line = element_line(colour = "black")) 

# Save data
write.csv(proxy.fin.soln,'solution_mix_ci.csv')
write.csv(median.fin.soln, 'median_mix_ci.csv')
write.csv(lower.quantile.fin.soln, 'lowerQ_mix.csv')
write.csv(upper.quantile.fin.soln, 'upperQ_mix.csv')
write.csv(res.d13C, 'resd13C_mix_ci.csv')
