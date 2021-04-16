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
library(gurobi)
library(prioritizr)
library(ggplot2)
library(raster)

### Input data

## load or create planning unit data

EcoReg = "Greater North Sea"

setwd(paste(pathdir,"1 - Input env",sep="/"))
load(paste(EcoReg,"region_grid_sensitivity.RData",sep="_"))
setwd(pathdir)

pu <- Region[,which(colnames(Region@data)%in%c("csquares","area_sqkm"))]

pu@data$locked_in<- FALSE
pu@data$locked_out <- FALSE


## Fishing data: average weight/value per C-square as raster

# make point file

xy <- Region@data[,which(colnames(Region@data)%in%c("long","lat"))]
#xy <- data.frame(csquare = FisheriesMet$csquares,
#                 lat =  Region$lat[match(FisheriesMet$csquares,Region$csquares)],
#                 long = Region$long[match(FisheriesMet$csquares,Region$csquares)])

#z <- FisheriesMet$DRB_MOL_total_value_2009 # should be the mean of all years 
#z <- Fisheries$total_value_2009 # use this file on the first run, column = sar, average across years > 2012

library(stringr)
library(scales)
library(gurobi)
library(prioritizr)


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
             "OT_MIX_CRU",
             "OT_MIX_CRU_DMF",
             "OT_DMF",
             "OT_MIX",
             "OT_MIX_DMF_BEN",
             "OT_MIX_DMF_PEL",
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

#feat_stack <- stack()

#for(i in 1:length(allmets)){
feat_stack <- rasterFromXYZ(cbind(xy,z[,which(colSums(z, na.rm=TRUE)>0)]),
                             res = c(0.05,0.05),
                             crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                             digits = 5)
  #feat_stack <- stack(feat_stack,f)
#}



### Optimization,  for one single target

pf1 <- problem(pu, features = feat_stack,
              cost_column = "area_sqkm") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.90) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0.1, presolve = 2, time_limit = 15)

pf2 <- pf1 %>%
  add_boundary_penalties(penalty = 300, edge_factor = 0.5)

# solve the problem
sf2 <- solve(pf2)

save(sf2,file="solution90pc.RData")

sf2r <- rasterize(sf2, r, field="solution_1", dataType=LOG1S)
plot(sf2r)

#print(eval_cost_summary(pf2, sf2[, "solution_1"]), width = Inf) # doesn't work

#print(eval_target_coverage_summary(p1, s1[, "solution_1"]), width = Inf) # doesn't work

# make a figure

#in.ras <- as.data.frame(as(sf2r,"SpatialPixelsDataFrame"))
#colnames(in.ras)<-c("value", "x", "y")
#in.ras$value <- factor(in.ras$value)

#jpeg("images/figureX.jpg", width = 350, height = 350)
#ggplot() +
#  geom_raster(data = in.ras, aes(x = x, y = y, fill=value)) +
#  scale_fill_discrete()+
#  geom_sf(data = world, fill=alpha("lightgrey", 0), color="lightgrey") +
#  coord_sf(xlim=c(min(in.ras$x),max(in.ras$x)), ylim=c(min(in.ras$y),max(in.ras$y))) + 
#  theme_bw()
#dev.off()


### evaluate irreplaceability

rc <- pf2 %>%
  replacement_cost(sf2[, "solution_1"]) # takes forever!

rc <- rasterize(rc, r, field="rc", dataType=LOG1S)

# make a figure

in.ras <- as.data.frame(as(rc,"SpatialPixelsDataFrame"))
colnames(in.ras)<-c("value", "x", "y")
in.ras$value <- factor(in.ras$value)

jpeg("images/figureXX.jpg", width = 350, height = 350)
ggplot() +
  geom_raster(data = in.ras, aes(x = x, y = y, fill=value)) +
  geom_sf(data = world, fill=alpha("lightgrey", 0), color="lightgrey") +
  coord_sf(xlim=c(min(in.ras$x),max(in.ras$x)), ylim=c(min(in.ras$y),max(in.ras$y))) + 
  theme_bw()
dev.off()



### Reduction of effort: examine rate of decrease in "fished area" (aka, optimization for decreasing targets)

target <- seq(0.95,0.05, by=-0.05)

res <- data.frame(n_cells_in=rep(NA,length(target)))

for(i in 1:length(target)){
  
  p1 <- problem(pu, features = feat_stack,
                cost_column = "area_sqkm") %>%
    add_min_set_objective() %>%
    add_relative_targets(target[i]) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0.1, presolve = 2, time_limit = 15)
  
  p2 <- p1 %>%
    add_boundary_penalties(penalty = 300, edge_factor = 0.5)
  
  # solve the problem
  s2 <- solve(p2)
  
  s2r <- rasterize(s2, r, field="solution_1", dataType=LOG1S)
  
  res[i,1] <- sum(values(s2r), na.rm = TRUE)
  
}

# plot it
#jpeg("images/figure5.jpg", width = 350, height = 350)
#plot(target,41600-res$n_cells_in,xlab = "Proportion fishery included (percent)", ylab= "Unfished (aka protected) area (n csquares)")
#dev.off()
