
### load sensitivity layer and fishing to calculate impact for an ecoregion

# load the grid for the region
  load(paste(pathdir,"1 - Input env",paste(EcoReg,"region_grid_sensitivity.RData",sep="_"),sep="/")) 

# load the Fisheries for the region
  load(paste(pathdir_nogit,paste(EcoReg,"fisheries.RData",sep="_"),sep="/")) 
  load(paste(pathdir_nogit,paste(EcoReg,"fisheries_per_metier_comb.RData",sep="_"),sep="/")) 

# calculate state and impact for all metiers and per metier group as defined in document
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("Impact_continuous_longevity.R")
  source("Habitatstatefishing.R") ## takes a few minutes
  
  setwd(pathdir_nogit)
  save(State_reg,file=paste(EcoReg,"state.RData",sep="_"))

# estimate impact indicator 2
# load the Fisheries for the region
  load(paste(pathdir_nogit,paste(EcoReg,"fisheries.RData",sep="_"),sep="/")) 
  load(paste(pathdir_nogit,paste(EcoReg,"fisheries_per_metier_comb.RData",sep="_"),sep="/")) 
  
# calculate state and impact for all metiers and per metier grouping inverse lifespan
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("Impact_inverse_longevity.R")
  source("Habitatstatefishing_inverselongevity.R") ## takes a few minutes
  
  setwd(pathdir_nogit)
  save(State_reg,file=paste(EcoReg,"state_IL.RData",sep="_"))
  
  rm(list=ls()[! ls() %in% c("Fisheries_Atlantic","FisheriesMet_Atlantic",
                             "Period","EcoReg","pathdir","pathdir_nogit")])
  