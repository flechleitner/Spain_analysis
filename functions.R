# thresh_proxy ########################################################################################################################
# Find best solutions for the model calibrating against a proxy
# Filter model data based on closest matching proxy values, loop through proxy values
# Find values that are within a threshold of the measured proxy (e.g., within +/- 5% of the proxy)
# source("helpers/functionscript.R")
thresh_proxy <- function(model, model.col = 4, proxy, proxy.col = 3, proxy.age = 0, thresh = 30){
  output <- NULL
  #model <- tibble::add_column(model, ts=NA)
  for (i in 1:dim(proxy)[1]) {
    #Find values that are within thresh of the measured proxy value
    thresh.values <- filter(model, model[model.col] <= proxy[i,proxy.col]*(1+thresh/100) & model[model.col]  >= proxy[i,proxy.col]*(1-thresh/100)) 
    thresh.values$ts <- proxy[i, proxy.age]
    output <- rbind(output, thresh.values)
  }
  return(output)
}

# eucl_proxy ########################################################################################################################
# Find best solutions for the model calibrating against a proxy
# Filter model data based on closest matching proxy values, loop through proxy values
# Min euclidean distance between column numbers model.col (default: 4) and proxy.col (default: 3)
# retain attribute columns proxy.age (default: 0)
# source("helpers/functionscript.R")

  eucl_proxy <- function(model, model.col = 4, proxy, proxy.col = 3, proxy.age = 0){
  proxy.col.atts <- NULL
  model.col.min  <- NULL 
    for (i in 1:dim(proxy)[1]) {
    dist <- sqrt((model[,model.col] - proxy[i,proxy.col])^2) # euclidean distance
    lowest <- quantile(dist, probs = c(0.05)) #Selects the lowest 20% of the distances
    min.which <- which(dist <= lowest)  #which(dist[(dist <= lowest)])
    #min.which <- which(dist == min(dist))
    model.col.min <- c(model.col.min, min.which) # multiple min vals
    proxy.col.atts.new <- # replicate proxy.age by number of multiple min vals
      matrix(
        proxy[i, proxy.age],
        nrow = length(min.which),
        ncol = length(proxy.age),
        byrow = T
      )
    proxy.col.atts <- rbind(proxy.col.atts, proxy.col.atts.new) # append
  }
  output <- model %>% slice(model.col.min) # select model data rows
  if (proxy.age != 0) { 
    output <- cbind(proxy.col.atts, output) # concatenate attribute columns
    colnames(output)[1:length(proxy.age)] <- 1:length(proxy.age) # rename attribute columns to numbers
  }
  return(output)
}

# ci_proxy ########################################################################################################################
# Find best solutions for the model calibrating against a proxy
# Filter model data based on proxy value that fall within the confidence interval, loop through proxy values
# Min euclidean distance between column numbers model.col (default: 4) and proxy.col (default: 3)
# retain attribute columns proxy.age (default: 0)
# source("helpers/functionscript.R")
ci_proxy <- function(model, model.col = 4, proxy, proxy.col = 3, proxy.age = 0){
  output <- NULL
  #model <- tibble::add_column(model, ts=NA)
  for (i in 1:dim(proxy)[1]) {
    proxy.uci <- proxy[i, paste0(proxy.col, "_uci")]
    proxy.lci <- proxy[i, paste0(proxy.col, "_lci")]
    #Find values that are within confidence interval of the measured proxy value
    ci.values <- filter(model, model[model.col] <= proxy.uci & model[,model.col] >= proxy.lci)
   
    if (nrow(ci.values) == 0) next
    
    ci.values$ts <- proxy[i, proxy.age]
    output <- rbind(output, ci.values)
  }
  return(output)
}


# constr_soln ########################################################################################################################
# Further constrain the solution by matching several proxies
constr_soln <- function(soln, soln.col = 5, proxy, proxy.col = 3, proxy.age = 0){
  output <- NULL
  for (t in proxy$ts) { 
    soln_ts <- soln %>% filter(ts == t) #%>% select(-ts)
    stal_data_ts <- proxy %>% filter(ts == t) #%>% select(-ts)
    #output.new <- thresh_proxy(model = soln_ts, model.col = soln.col, proxy = stal_data_ts, proxy.col = proxy.col, proxy.age = proxy.age)
    #output.new <- eucl_proxy(model = soln_ts, model.col = soln.col, proxy = stal_data_ts, proxy.col = proxy.col, proxy.age = proxy.age)
    output.new <- ci_proxy(model = soln_ts, model.col = soln.col, proxy = stal_data_ts, proxy.col = proxy.col, proxy.age = proxy.age)
    output <- rbind(output, output.new) # append
  }
  return(output)
}



# CaveCalc data extraction ##########################################################################################################
#Extract the data from CaveCalc .mat output files

CaveCalc_extr <- function(m) {
  #Extract settings
  cave.pCO2=list()
  for (i in seq_along(m)) {
    tempobj= (m[[i]][["settings"]][["cave_pCO2"]]) 
    name <- paste('cavepCO2',names(m)[[i]],sep='_')
    cave.pCO2[[name]] <- tempobj
  }
  cave.pCO2 <- unlist(cave.pCO2)
  
  soil.pCO2=list()
  for (i in seq_along(m)) {
    tempobj= (m[[i]][["settings"]][["soil_pCO2"]]) 
    name <- paste('soilpCO2',names(m)[[i]],sep='_')
    soil.pCO2[[name]] <- tempobj
  }
  soil.pCO2 <- unlist(soil.pCO2)
  
  gas.volume=list()
  for (i in seq_along(m)) {
    tempobj= (m[[i]][["settings"]][["gas_volume"]]) 
    name <- paste('gas.volume',names(m)[[i]],sep='_')
    gas.volume[[name]] <- tempobj
  }
  gas.volume <- unlist(gas.volume)
  
  soil.R14C=list()
  for (i in seq_along(m)) {
    tempobj= (m[[i]][["settings"]][["soil_R14C"]]) 
    name <- paste('soil.R14C',names(m)[[i]],sep='_')
    soil.R14C[[name]] <- tempobj
  }
  soil.R14C <- unlist(soil.R14C)
  
  soil.d13C=list()
  for (i in seq_along(m)) {
    tempobj= (m[[i]][["settings"]][["soil_d13C"]]) 
    name <- paste('soil.d13C',names(m)[[i]],sep='_')
    soil.d13C[[name]] <- tempobj
  }
  soil.d13C <- unlist(soil.d13C)
  
  
  #Extract results
  d13C=list()
  for (i in seq_along(m)) {
    tempobj= (m[[i]][["results"]][["d13C_Calcite"]]) 
    name <- paste('d13C',names(m)[[i]],sep='_')
    d13C[[name]] <- tempobj
  }
  d13C <- unlist(d13C)
  
  DCP=list()
  for (i in seq_along(m)) {
    tempobj= (m[[i]][["results"]][["DCP"]]) 
    name <- paste('DCP',names(m)[[i]],sep='_')
    DCP[[name]] <- tempobj
  }
  DCP <- unlist(DCP)
  
  d44Ca=list()
  for (i in seq_along(m)) {
    tempobj= (m[[i]][["results"]][["d44Ca_Calcite"]]) 
    name <- paste('d44Ca',names(m)[[i]],sep='_')
    d44Ca[[name]] <- tempobj
  }
  d44Ca <- unlist(d44Ca)
  
  MgCa=list()
  for (i in seq_along(m)) {
    tempobj= (m[[i]][["results"]][["MgCa_molmol_Calcite"]])
    name <- paste('MgCa',names(m)[[i]],sep='_')
    MgCa[[name]] <- tempobj
  }
  MgCa <- unlist(MgCa) *1000 #Convert the Mg/Ca data to mmol/mol to match stal data
  
  #Combine all in one dataframe
  CaveCalc_out <- cbind(soil.pCO2, soil.d13C, soil.R14C, cave.pCO2, gas.volume, d13C, d44Ca, MgCa, DCP)
}
  