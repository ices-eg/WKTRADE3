rm(list = ls())

### github folder
  pathdir <- "C:/Users/pdvd/Online for git/WKTRADE3"

### folder for restricted VMS data
  pathdir_nogit <- "C:/Users/pdvd/Online for git/WKTRADE3 - Fisheries restricted"

### get all libraries
source(paste(pathdir,"Utilities/Libraries_WKTRADE3.R",sep="/"))

### select time period
  Period    <- 2009:2018    # period with fishing data to calculate impact
  AssPeriod <- 2013:2018    # assessment period

### select region
  Assregion <- "Greater North Sea"
  EcoReg    <- "Greater North Sea"
  Assunit   <- "(sub-)Region"   
  
### load processed file, with longevity and state/impact 
  if (p %in% regions_with_impact){
    load(paste(pathdir_nogit,paste(EcoReg,"state_IL.RData",sep="_"),sep="/")) 
    State_reg_IL <- State_reg # rename the state from the inverse longevity
    load(paste(pathdir_nogit,paste(EcoReg,"state.RData",sep="_"),sep="/")) 
  }
  load(paste(pathdir_nogit,paste(EcoReg,"Fisheries.RData",sep="_"),sep="/")) 
  load(paste(pathdir_nogit,paste(EcoReg,"fisheries_per_metier_comb.RData",sep="_"),sep="/")) 
  setwd(paste(pathdir,"1 - Input env",sep="/"))
  load(paste(EcoReg,"region_grid_sensitivity.RData",sep="_")) 
  load(paste(EcoReg,"MSFD_per_csquare.RData",sep="_")) 

### run script for trade-off analysis - reductions in effort
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("Tradeoff_effort_removal.R") 

### run script for reductions and prohibitions by gear type
  setwd(paste(pathdir,"Utilities",sep="/"))
  # source("Tradeoff_gear_prohibitions.R") # takes 20 minutes
  source("Tradeoff_gear_prohibitions_results.R") 

### run script for trade-off analysis - freeze footprint
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("Tradeoff_freeze_footprint.R")  

### run habitat management
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("Tradeoff_habitat_management_allRegions.R")

