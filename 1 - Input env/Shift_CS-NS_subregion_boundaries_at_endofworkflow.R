
# install libraries
  library(rgdal)
  library(sp)
  library(raster)
  library(ncdf4)

# set folder directory
  pathdir <- "C:/etc..."

# assign MSFD subregions based on https://www.eea.europa.eu/data-and-maps/data/msfd-regions-and-subregions
  shapeMSFD <- readOGR(dsn = paste(pathdir,"MarineSubregions",sep="/") ,layer="MarineSubregions")
  # plot(shapeMSFD)
  shapeMSFD@proj4string # check coordinates reference system
  shapeMSFD <- spTransform(shapeMSFD,CRS(proj4string(Region))) # make it similar to region
  shapeMSFD@proj4string # check coordinates reference system again
  tr <- over(Region,shapeMSFD)
  Region@data$MSFDsub <- tr$name_text 
  
# select all csquares in NS ices ecoregion that are part of Celtic MSFD sub-region
  CS <- subset(Region,Region@data$MSFDsub == "Celtic Seas")
  CS$Ecoregion <- "Celtic Seas"
  CS$division <- "south_CS"
  CS_csq <- CS$csquares
  
# now first save new North Sea sub-region
  NS <- subset(Region,!(Region@data$MSFDsub == "Celtic Seas"))
  NS <- subset (NS, select = -MSFDsub)
  Region <- NS
  setwd("C:/Users/pdvd/Online for git/WKTRADE3/1 - Input env")
  save(Region, file="Greater North Sea_region_grid_sensitivity.RData")

# load and save new Celtic Seas sub-region                 
  load("Celtic Seas_region_grid_sensitivity.RData")            
  CSnew <- CS[,c(1,2,4,8,14,15,16,5,6,3,9,7,17)]
          
  Region <- rbind(Region,CSnew)
  save(Region, file="Celtic Seas_region_grid_sensitivity.RData")                   

# now shift all MSFD per C-sq
  load("Greater North Sea_MSFD_per_csquare.RData")   
  msfd_csCeltic <- subset(msfd_csq,(msfd_csq$csquares %in% c(CS_csq)))
  msfd_csq <- subset(msfd_csq,!(msfd_csq$csquares %in% c(CS_csq)))  
  save(msfd_csq, file="Greater North Sea_MSFD_per_csquare.RData")   

  load("Celtic Seas_MSFD_per_csquare.RData")     
  msfd_csq <- rbind(msfd_csq,msfd_csCeltic)
  save(msfd_csq, file="Celtic Seas_MSFD_per_csquare.RData")   

# now shift all Fisheries per C-sq
  setwd("C:/Users/pdvd/Online for git/WKTRADE3 - Fisheries restricted")
  load("Greater North Sea_fisheries.RData")   
  FisheriesCeltic <- subset(Fisheries,(Fisheries$csquares %in% c(CS_csq)))
  Fisheries <- subset(Fisheries,!(Fisheries$csquares %in% c(CS_csq)))  
  save(Fisheries, file="Greater North Sea_fisheries.RData")   
  
  load("Celtic Seas_Fisheries.RData")
  table(colnames(Fisheries)==colnames(FisheriesCeltic))
  Fisheries <- rbind(Fisheries,FisheriesCeltic)
  save(Fisheries, file="Celtic Seas_Fisheries.RData")   
  
# now shift all Fisheries per metier C-sq
  load("Greater North Sea_fisheries_per_metier.RData")   
  FisheriesMetCeltic <- subset(FisheriesMet,(FisheriesMet$csquares %in% c(CS_csq)))
  FisheriesMet <- subset(FisheriesMet,!(FisheriesMet$csquares %in% c(CS_csq)))  
  save(FisheriesMet, file="Greater North Sea_fisheries_per_metier.RData")   
  
  load("Celtic Seas_fisheries_per_metier.RData")
  table(colnames(FisheriesMet)==colnames(FisheriesMetCeltic))
  FisheriesMet <- rbind(FisheriesMet,FisheriesMetCeltic)
  save(FisheriesMet, file="Celtic Seas_Fisheries_per_metier.RData")   
  
# now shift all Fisheries per metier combined C-sq
  load("Greater North Sea_fisheries_per_metier_comb.RData")   
  FisheriesMetCeltic <- subset(FisheriesMet,(FisheriesMet$csquares %in% c(CS_csq)))
  FisheriesMet <- subset(FisheriesMet,!(FisheriesMet$csquares %in% c(CS_csq)))  
  save(FisheriesMet, file="Greater North Sea_fisheries_per_metier_comb.RData")   
  
  load("Celtic Seas_fisheries_per_metier_comb.RData")
  table(colnames(FisheriesMet)==colnames(FisheriesMetCeltic))
  FisheriesMet <- rbind(FisheriesMet,FisheriesMetCeltic)
  save(FisheriesMet, file="Celtic Seas_fisheries_per_metier_comb.RData")   
  
# remove states in North Sea
  load("Greater North Sea_state.RData")   
  State_reg <- subset(State_reg,!(State_reg$Fisheries.csquares %in% c(CS_csq)))  
  save(State_reg, file="Greater North Sea_state.RData")   
  
  load("Greater North Sea_state_IL.RData")   
  State_reg <- subset(State_reg,!(State_reg$Fisheries.csquares %in% c(CS_csq)))  
  save(State_reg, file="Greater North Sea_state_IL.RData")  
  
    