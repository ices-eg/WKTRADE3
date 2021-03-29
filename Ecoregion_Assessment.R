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
  
### create list of marine reporting areas
  Sregions <- c("Greater North Sea", "Baltic Sea","Celtic Seas","Bay of Biscay and the Iberian Coast")
  NS_div <- c("Northern_NS", "Kattegat_NS", "Channel_NS", "Southern_NS" ,"NTrench_NS")
  BS_div <- c("Aland_BS", "GulfF_BS", "GulfR_BS" ,"ArkBor_BS","Western_BS" ,"Proper_BS" ,"Bothnian_BS")
  CS_div <- c("deep_CS",  "channel_CS"  ,"south_CS", "North_CS" ,"shetland_CS" ,"Irishsea_CS", "Middle_CS") 
  BoBIC_div <- c("Shallow_BoB", "ShallowNorth_IC","ShallowSouth_IC", "Galicia_IC",  "Deep_IC", "Deep_BoB")
  divis <- c(NS_div,BS_div,CS_div,BoBIC_div)

### run all areas in a loop 
  Assregion_index <- c(Sregions, divis)  # get the reporting region
  EcoReg_index    <- c(Sregions, rep(Sregions[1],5),rep(Sregions[2],7),
                       rep(Sregions[3],7),rep(Sregions[4],6))  # get the (sub-)region for the reporting region
  Assunit_index   <- c(rep("(sub-)Region",4),rep("Division",25)) # is reporting region a "(sub-)Region" or "Division"?
  regions_with_impact <- c(1,2,5,6,7,8,10:16) # get all areas with longevity data

  for (p in 11:29){
   Assregion <- Assregion_index[p]
   EcoReg    <- EcoReg_index[p]
   Assunit <- Assunit_index[p]     

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
   
### run script for spatial temporal analysis (takes a long time)
   setwd(paste(pathdir,"Utilities",sep="/"))
   source("Core_fishing_by_metier_analysis_results.R")
   
### run script to process FBIT figures and tables
   setwd(paste(pathdir,"Utilities",sep="/"))
   source("Processing_assessment.R")
  
### run script to make FBIT output  
   setwd(paste(pathdir,"Utilities",sep="/"))
   source("map_plot.R")
   source("Output_assessment.R")
   
### run habitat management
   setwd(paste(pathdir,"Utilities",sep="/"))
   source("Tradeoff_habitat_management_allRegions.R")
 }
 
 
 # run trade off analysis for the Greater North Sea
  
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
 

  
  
  
