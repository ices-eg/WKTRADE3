
# install libraries
  library(rgdal)
  library(sp)
  library(raster)
  library(ncdf4)

# ------------------------------------------------------------------------------
# there is a mismatch between DGEnv regional boundaries and ICES ecoregions
# output has been updated in WKTRADE3 to reflect a different CS and NS border
# in Celtic Seas
# The MSFD data has been taken from FBIT - FBIT uses the ICES borders
# the below code converts the FBIT MSFD borders to DGEnv boundaries
# ------------------------------------------------------------------------------

# set folder directory
  pathdir <- "C:/Users/danie/Documents/Online for git/WKTRADE3/1 - Input env/"
  setwd(pathdir)
  
  # load Celtic Sea without parts of English channel 
  load("Celtic Seas_region_grid_sensitivity.RData")
  
  # load MSFD North Sea
  load("Greater North Sea_MSFD_per_csquare.RData")
  
  # get all c-sq in NS from CS_MSFD
  msfd_addCS <- subset(msfd_csq,msfd_csq$csquares %in% unique(Region@data$csquares))
  
  msfd_csq <- subset(msfd_csq,!(msfd_csq$csquares %in% unique(Region@data$csquares)))  
  save(msfd_csq, file="Greater North Sea_MSFD_per_csquare.RData")   
  
  # load MSFD Celtic Seas
  load("Celtic Seas_MSFD_per_csquare.RData")
  msfd_csq <- rbind(msfd_csq,msfd_addCS)
  save(msfd_csq, file="Celtic Seas_MSFD_per_csquare.RData")   
  