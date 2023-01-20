rm(list = ls())

### libraries needed
library(rgdal)

### github folder
pathdir <- "C:/Users/danie/Documents/Online for git/WKTRADE3"

### create folder for restricted data
pathdir_nogit <- "C:/Users/danie/Documents/Online for git/"

# run only first time
setwd(pathdir_nogit)
dir.create(paste("WKTRADE4 - Fisheries restricted"))
pathdir_nogit <- paste(pathdir_nogit,"WKTRADE4 - Fisheries restricted",sep="/")

# ------------------------------------------------------------------------------
# fishing data was obtained via FBIT (ICES VMS 2022 datacall) 
# for all Atlantic regions
# data were copied into the restricted folder

# the below files prepare the fishing data for WKTRADE4 per ecoregion
# and estimate state for North Sea and Baltic Sea
# ------------------------------------------------------------------------------

load(paste(pathdir_nogit,"fisheries_FBIT_VMSdatacall2022.RData",sep="/"))
Fisheries_Atlantic <- Fisheries
load(paste(pathdir_nogit,"fisheries_metier_FBIT_VMSdatacall2022.RData",sep="/"))
FisheriesMet_Atlantic <- FisheriesMet

Period <- 2009:2021 # period with fishing data to calculate impact

EcoReg  <- "Baltic Sea"
source(paste(pathdir,"Utilities/Get_fishing_data.R",sep="/")) 
source(paste(pathdir,"2 - Data processing/Calculating_habitat_state_across_regions.R",sep="/")) 

EcoReg  <- "Greater North Sea"
source(paste(pathdir,"Utilities/Get_fishing_data.R",sep="/")) 
source(paste(pathdir,"2 - Data processing/Calculating_habitat_state_across_regions.R",sep="/")) 

EcoReg  <- "Celtic Seas"
source(paste(pathdir,"Utilities/Get_fishing_data.R",sep="/")) 

EcoReg  <- "Bay of Biscay and the Iberian Coast"
source(paste(pathdir,"Utilities/Get_fishing_data.R",sep="/")) 
