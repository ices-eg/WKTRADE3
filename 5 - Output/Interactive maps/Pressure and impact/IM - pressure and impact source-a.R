
### create list of marine reporting areas
Sregions <- c("Greater North Sea", "Baltic Sea","Celtic Seas","Bay of Biscay and the Iberian Coast")
NS_div <- c("Northern_NS", "Kattegat_NS", "Channel_NS", "Southern_NS" ,"NTrench_NS")
BS_div <- c("GulfF_BS", "GulfR_BS" ,"ArkBor_BS","Western_BS" ,"Proper_BS" ,"Bothnian_BS")
CS_div <- c("deep_CS" ,"south_CS", "North_CS" ,"Irishsea_CS", "Middle_CS") 
BoBIC_div <- c("Shallow_BoB", "ShallowNorth_IC","ShallowSouth_IC", "Galicia_IC",  "Deep_IC", "Deep_BoB")
divis <- c(NS_div,BS_div,CS_div,BoBIC_div)

### run all areas in a loop 
Assregion_index <- c(Sregions, divis)  # get the reporting region
EcoReg_index    <- c(Sregions, rep(Sregions[1],5),rep(Sregions[2],6),
                     rep(Sregions[3],5),rep(Sregions[4],6))  # get the (sub-)region for the reporting region
Assunit_index   <- c(rep("(sub-)Region",4),rep("Division",22)) # is reporting region a "(sub-)Region" or "Division"?
regions_with_impact <- c(1,2,5,6,7,8,10:15) # get all areas with longevity data

p <-1 
Assregion <- Assregion_index[p]
EcoReg    <- EcoReg_index[p]
Assunit <- Assunit_index[p]     

### load processed file, with longevity and state/impact 
load(paste(pathdir_nogit,paste(EcoReg,"state_IL.RData",sep="_"),sep="/")) 
setwd(paste(pathdir,"1 - Input env",sep="/"))
load(paste(EcoReg,"region_grid_sensitivity.RData",sep="_")) 

## get state map average
state_year <- paste("state",AssPeriod,sep="_")

idx <- which(names(Region@data)== "long")
colnames(Region@data)[idx]  <- "longitude"
idx <- which(names(Region@data)== "lat")
colnames(Region@data)[idx]  <- "latitude"

# show SAR
Trade_RE1 <- Region@data
nam <- c(state_year)
Trade_RE1 <- cbind(Trade_RE1, State_reg[match(Trade_RE1$csquares,State_reg$Fisheries.csquares), c(nam)])
Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 1
Trade_RE1$avgstate <- rowMeans(Trade_RE1[,state_year])
Trade_RE1$avgimpact <- 1-Trade_RE1$avgstate
cut_impact <- c(0,0.1,0.2,0.3,0.5,0.8,1)
Trade_RE1$cat<- as.factor(cut(Trade_RE1$avgimpact,cut_impact,right=T))
Region <- cbind(Region, Trade_RE1[match(Region@data$csquares,Trade_RE1$csquares), c("cat")])
colnames(Region@data)[ncol(Region@data)] <- "cat"
Region <- subset(Region,Region@data$Depth > -200)
Region_IL <- Region[,c("cat")]

p <- 2
Assregion <- Assregion_index[p]
EcoReg    <- EcoReg_index[p]
Assunit <- Assunit_index[p]     

### load processed file, with longevity and state/impact 
load(paste(pathdir_nogit,paste(EcoReg,"state_IL.RData",sep="_"),sep="/")) 
setwd(paste(pathdir,"1 - Input env",sep="/"))
load(paste(EcoReg,"region_grid_sensitivity.RData",sep="_")) 

## get state map average
state_year <- paste("state",AssPeriod,sep="_")

idx <- which(names(Region@data)== "long")
colnames(Region@data)[idx]  <- "longitude"
idx <- which(names(Region@data)== "lat")
colnames(Region@data)[idx]  <- "latitude"

# show SAR
Trade_RE1 <- Region@data
nam <- c(state_year)
Trade_RE1 <- cbind(Trade_RE1, State_reg[match(Trade_RE1$csquares,State_reg$Fisheries.csquares), c(nam)])
Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 1
Trade_RE1$avgstate <- rowMeans(Trade_RE1[,state_year])
Trade_RE1$avgimpact <- 1-Trade_RE1$avgstate
cut_impact <- c(0,0.1,0.2,0.3,0.5,0.8,1)
Trade_RE1$cat<- as.factor(cut(Trade_RE1$avgimpact,cut_impact,right=T))
Region <- cbind(Region, Trade_RE1[match(Region@data$csquares,Trade_RE1$csquares), c("cat")])
colnames(Region@data)[ncol(Region@data)] <- "cat"
Region <- Region[,c("cat")]

Region_IL <- rbind(Region_IL,Region)

### now do the same for state_PD
p <-1 
Assregion <- Assregion_index[p]
EcoReg    <- EcoReg_index[p]
Assunit <- Assunit_index[p]     

### load processed file, with longevity and state/impact 
load(paste(pathdir_nogit,paste(EcoReg,"state.RData",sep="_"),sep="/")) 
setwd(paste(pathdir,"1 - Input env",sep="/"))
load(paste(EcoReg,"region_grid_sensitivity.RData",sep="_")) 

## get state map average
state_year <- paste("state",AssPeriod,sep="_")

idx <- which(names(Region@data)== "long")
colnames(Region@data)[idx]  <- "longitude"
idx <- which(names(Region@data)== "lat")
colnames(Region@data)[idx]  <- "latitude"

# show SAR
Trade_RE1 <- Region@data
nam <- c(state_year)
Trade_RE1 <- cbind(Trade_RE1, State_reg[match(Trade_RE1$csquares,State_reg$Fisheries.csquares), c(nam)])
Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 1
Trade_RE1$avgstate <- rowMeans(Trade_RE1[,state_year])
Trade_RE1$avgimpact <- 1-Trade_RE1$avgstate
cut_impact <- c(0,0.1,0.2,0.3,0.5,0.8,1)
Trade_RE1$cat<- as.factor(cut(Trade_RE1$avgimpact,cut_impact,right=T))
Region <- cbind(Region, Trade_RE1[match(Region@data$csquares,Trade_RE1$csquares), c("cat")])
colnames(Region@data)[ncol(Region@data)] <- "cat"
Region <- subset(Region,Region@data$Depth > -200)
Region_PD <- Region[,c("cat")]

p <- 2
Assregion <- Assregion_index[p]
EcoReg    <- EcoReg_index[p]
Assunit <- Assunit_index[p]     

### load processed file, with longevity and state/impact 
load(paste(pathdir_nogit,paste(EcoReg,"state.RData",sep="_"),sep="/")) 
setwd(paste(pathdir,"1 - Input env",sep="/"))
load(paste(EcoReg,"region_grid_sensitivity.RData",sep="_")) 

## get state map average
state_year <- paste("state",AssPeriod,sep="_")

idx <- which(names(Region@data)== "long")
colnames(Region@data)[idx]  <- "longitude"
idx <- which(names(Region@data)== "lat")
colnames(Region@data)[idx]  <- "latitude"

# show SAR
Trade_RE1 <- Region@data
nam <- c(state_year)
Trade_RE1 <- cbind(Trade_RE1, State_reg[match(Trade_RE1$csquares,State_reg$Fisheries.csquares), c(nam)])
Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 1
Trade_RE1$avgstate <- rowMeans(Trade_RE1[,state_year])
Trade_RE1$avgimpact <- 1-Trade_RE1$avgstate
cut_impact <- c(0,0.1,0.2,0.3,0.5,0.8,1)
Trade_RE1$cat<- as.factor(cut(Trade_RE1$avgimpact,cut_impact,right=T))
Region <- cbind(Region, Trade_RE1[match(Region@data$csquares,Trade_RE1$csquares), c("cat")])
colnames(Region@data)[ncol(Region@data)] <- "cat"
Region <- Region[,c("cat")]

Region_PD <- rbind(Region_PD,Region)