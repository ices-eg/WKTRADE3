## In this script an optimization problem is set up where targets are set for 
## fisheries catch/value and area is used as cost.
## The problem is subsequently solved.
## The script was written by Genoveva Gonzalez-Mirelis
## In a first instance, this is run for the Greater North Sea ecoregion
## This will be added to the GitHub, where hopefully Daniel van Denderen can 
## make some final modifications to integrate it into the overall TRADE3
## workflow
## can download data from
#https://github.com/ices-eg/WKTRADE3/tree/master/1%20-%20Input%20env

library(stringr)
library(scales)
#library(gurobi)
library(prioritizr)
library(ggplot2)
library(raster)

### Input data

## load or create planning unit data
p <-5
Assregion <- Assregion_index[p]
EcoReg    <- EcoReg_index[p]
Assunit <- Assunit_index[p]     

### load processed file, with longevity and state/impact 
load(paste(pathdir_nogit,paste(EcoReg,"Fisheries.RData",sep="_"),sep="/")) 
load(paste(pathdir_nogit,paste(EcoReg,"fisheries_per_metier_comb.RData",sep="_"),sep="/")) 
setwd(paste(pathdir,"1 - Input env",sep="/"))
load(paste(EcoReg,"region_grid_sensitivity.RData",sep="_")) 

# select division from the region
if (Assregion != EcoReg){
  Region <-  subset(Region,Region@data$division == Assregion)
}

# add correct column names for map plots
idx <- which(names(Region@data)== "long")
colnames(Region@data)[idx]  <- "longitude"
idx <- which(names(Region@data)== "lat")
colnames(Region@data)[idx]  <- "latitude"

pu <- Region[,which(colnames(Region@data)%in%c("csquares","area_sqkm"))]
pu@data$locked_in<- FALSE
pu@data$locked_out <- FALSE

## Fishing data: average weight/value/sar per C-square as raster
# make point file
xy <- Region@data[,which(colnames(Region@data)%in%c("longitude","latitude"))]

years<-as.numeric(
  str_sub(colnames(FisheriesMet),-4,-1)
    )

idx <- which(years>2012)

# subset the FisheriesMet table according to year (select columns)
FisheriesMetSS <-cbind(FisheriesMet$csquares,FisheriesMet[,idx])
names(FisheriesMetSS)[1] <- names(FisheriesMet[1])

# remove csquares whose lat long are unknown (select rows)
FisheriesMetSS <- FisheriesMetSS[match(as.character(Region@data$csquares),FisheriesMetSS$csquares),]


### optimize all metiers simultaneously
# select the metric of choice
metric <- "surface_sar"

allmets <- c("DRB_MOL",
             "OT_CRU",
             "OT_DMF",
             "OT_MIX",
             "OT_SPF",
             "SDN_DMF",
             "SSC_DMF",
             "TBB_CRU",
             "TBB_DMF",
             "TBB_MOL"
)

z <- data.frame(matrix(NA,nrow=dim(FisheriesMetSS)[1], ncol = length(allmets)))

for(i in 1:length(allmets)){
  z[,i] <- rescale(
    sapply(metric, function(x) rowMeans(FisheriesMetSS[,grep(paste(allmets[i],metric, sep="_"), names(FisheriesMetSS))]))
  )
  }

colnames(z) <- allmets

# rasterize
feat_stack <- rasterFromXYZ(cbind(xy,z[,which(colSums(z, na.rm=TRUE)>0)]),
                             res = c(0.05,0.05),
                             crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                             digits = 5)

### Optimization,  for one single target
pf1 <- problem(pu, features = feat_stack,
              cost_column = "area_sqkm") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.90) %>%
  add_binary_decisions() %>%
  add_rsymphony_solver(gap = 0.1)

pf2 <- pf1 %>%
  add_boundary_penalties(penalty = 300, edge_factor = 0.5)

# solve the problem
sf2 <- solve(pf2)
newdat <- subset(sf2,sf2@data$solution_1 == 1)
  
save(newdat,file="solution90pc.RData")

for (p in 6:26) {
  Assregion <- Assregion_index[p]
  EcoReg    <- EcoReg_index[p]
  Assunit <- Assunit_index[p]     
  
  ### load processed file, with longevity and state/impact 
  load(paste(pathdir_nogit,paste(EcoReg,"Fisheries.RData",sep="_"),sep="/")) 
  load(paste(pathdir_nogit,paste(EcoReg,"fisheries_per_metier_comb.RData",sep="_"),sep="/")) 
  setwd(paste(pathdir,"1 - Input env",sep="/"))
  load(paste(EcoReg,"region_grid_sensitivity.RData",sep="_")) 
  
  # select division from the region
  if (Assregion != EcoReg){
    Region <-  subset(Region,Region@data$division == Assregion)
  }
  
  # add correct column names for map plots
  idx <- which(names(Region@data)== "long")
  colnames(Region@data)[idx]  <- "longitude"
  idx <- which(names(Region@data)== "lat")
  colnames(Region@data)[idx]  <- "latitude"
  
  pu <- Region[,which(colnames(Region@data)%in%c("csquares","area_sqkm"))]
  pu@data$locked_in<- FALSE
  pu@data$locked_out <- FALSE
  
  ## Fishing data: average weight/value per C-square as raster
  # make point file
  xy <- Region@data[,which(colnames(Region@data)%in%c("longitude","latitude"))]

  years<-as.numeric(
    str_sub(colnames(FisheriesMet),-4,-1)
  )
  
  idx <- which(years>2012)
  
  # subset the FisheriesMet table according to year (select columns)
  FisheriesMetSS <-cbind(FisheriesMet$csquares,FisheriesMet[,idx])
  names(FisheriesMetSS)[1] <- names(FisheriesMet[1])
  
  # remove csquares whose lat long are unknown (select rows)
  FisheriesMetSS <- FisheriesMetSS[match(as.character(Region@data$csquares),FisheriesMetSS$csquares),]
  
  ### optimize all metiers simultaneously
  # select the metric of choice
  metric <- "surface_sar"
  
  allmets <- c("DRB_MOL",
               "OT_CRU",
               "OT_DMF",
               "OT_MIX",
               "OT_SPF",
               "SDN_DMF",
               "SSC_DMF",
               "TBB_CRU",
               "TBB_DMF",
               "TBB_MOL"
  )
  
  z <- data.frame(matrix(NA,nrow=dim(FisheriesMetSS)[1], ncol = length(allmets)))
  
  for(i in 1:length(allmets)){
    z[,i] <- rescale(
      sapply(metric, function(x) rowMeans(FisheriesMetSS[,grep(paste(allmets[i],metric, sep="_"), names(FisheriesMetSS))]))
    )
  }
  
  colnames(z) <- allmets
  
  # rasterize
  feat_stack <- rasterFromXYZ(cbind(xy,z[,which(colSums(z, na.rm=TRUE)>0)]),
                              res = c(0.05,0.05),
                              crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                              digits = 5)

  ### Optimization,  for one single target
  pf1 <- problem(pu, features = feat_stack,
                 cost_column = "area_sqkm") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.90) %>%
    add_binary_decisions() %>%
    add_rsymphony_solver(gap = 0.1)
  
  pf2 <- pf1 %>%
    add_boundary_penalties(penalty = 300, edge_factor = 0.5)
  
  # solve the problem
  sf2 <- solve(pf2)
  sf2 <- subset(sf2,sf2@data$solution_1 == 1)
  newdat <- rbind(newdat,sf2)
  save(newdat,file="solution90pc.RData")
  print(p)
}
