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
  
### run all regions and sub_regions
 
 # select the assessment region
 Assregion_index <- c("Greater North Sea","L2.2.1", "L2.2.2", "L2.2.5", "L2.2.7","Baltic Sea")
 
 # select the ecoregion for the assessment region
 EcoReg_index    <- c(rep("Greater North Sea",5),rep("Baltic Sea",1))
  
 # select whether the assessment region is "Ecoregion" or "OSPAR_subregion" or "HELCOM_subregion" (not yet available)
 Assunit_index   <- c("Ecoregion",rep("OSPAR_subregion",4),"Ecoregion")   
 
 for(p in 1:6){
   Assregion <- Assregion_index[p]
   EcoReg    <- EcoReg_index[p]
   Assunit <- Assunit_index[p]     

### load processed file, with longevity and state/impact 
  load(paste(pathdir_nogit,paste(EcoReg,"state_IL.RData",sep="_"),sep="/")) 
  State_reg_IL <- State_reg # rename the state from the inverse longevity
  load(paste(pathdir_nogit,paste(EcoReg,"state.RData",sep="_"),sep="/")) 
  load(paste(pathdir_nogit,paste(EcoReg,"Fisheries.RData",sep="_"),sep="/")) 
  load(paste(pathdir_nogit,paste(EcoReg,"fisheries_per_metier_comb.RData",sep="_"),sep="/")) 
  
  setwd(paste(pathdir,"1 - Input env",sep="/"))
  load(paste(EcoReg,"region_grid_sensitivity.RData",sep="_")) 

### run script for spatial temporal analysis
  
  
### run script to process FBIT figures and tables
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("Processing_assessment.R")
  
### run script to make FBIT output  
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("map_plot.R")
  source("Output_assessment.R")
  
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
 source("Tradeoff_habitat_management.R")
 source("Tradeoff_habitat_management_table.R")
 
 } 
  
  
  
