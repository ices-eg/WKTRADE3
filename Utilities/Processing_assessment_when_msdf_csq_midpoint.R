
###### DATA PROCESSING

##### this is an old version where estimates of MSFD habitats are based on the mid-point of the c-square
##### in the new Processing_assessment this has been updated; includes all MSFD habitat types in the c-sq

###### figures and tables for ICES WKTRADE3
SSAR_year <- paste("surface_sar",AssPeriod,sep="_")
state_year <- paste("state",AssPeriod,sep="_")
weight_year <- paste("total_weight",AssPeriod,sep="_")
value_year <- paste("total_value",AssPeriod,sep="_")

setwd(pathdir_nogit)  
dir.create("Producing figures and tables", showWarnings = FALSE)
setwd(paste(pathdir_nogit,"Producing figures and tables",sep="/"))  
dir.create(paste(Assunit), showWarnings = FALSE)
setwd(paste(pathdir_nogit,"Producing figures and tables",Assunit,sep="/"))  
dir.create(paste(Assregion), showWarnings = FALSE)
setwd(paste(pathdir_nogit,"Producing figures and tables",Assunit,Assregion,sep="/"))  

# select division from the region
if (Assregion != EcoReg){
  Region <-  subset(Region,Region@data$division == Assregion)
}

# subset all areas with longevity (north sea has no longevity prediction for Norwegian trench)
if (p %in% regions_with_impact){
  Region <- Region[!(is.na(Region$medlong)),]
}

# add correct column names for map plots
idx <- which(names(Region@data)== "long")
colnames(Region@data)[idx]  <- "longitude"
idx <- which(names(Region@data)== "lat")
colnames(Region@data)[idx]  <- "latitude"

#####
# Figure A.1
################
figA1 <- Region@data
nam <- c(SSAR_year,weight_year,value_year)
figA1 <- cbind(figA1, Fisheries[match(figA1$csquares,Fisheries$csquares), c(nam)])
save(figA1, file="FigureA1.RData")

#####
# Table A.1
################
TA1dat <- Region@data

# remove areas deeper than 200 meter for overview of Greater North Sea
if (Assregion == "Greater North Sea"){
  TA1dat <-  subset(TA1dat,TA1dat$Depth >= -200)
}

nam <- c(SSAR_year)
TA1dat <- cbind(TA1dat, Fisheries[match(TA1dat$csquares,Fisheries$csquares), c(nam)])
TA1dat[,c(nam)][is.na(TA1dat[,c(nam)])] <- 0
TA1dat$avgsar <- rowMeans(TA1dat[,SSAR_year]) 

# indicator 1 intensity
TA1dat$sweptarea <- TA1dat[,"avgsar"]*TA1dat$area_sqkm
ind1 <- sum(TA1dat$sweptarea,na.rm=T)/sum(TA1dat$area_sqkm)

# indicator 2 proportion of grid cells fished (fished irrespective of swept area)
ind2 <- length(which(TA1dat[,"avgsar"]>0))/nrow(TA1dat)

# indicator 3 proportion of area fished
TA1dat$sweptarea2 <- TA1dat$sweptarea
TA1dat$sweptarea2 <- ifelse(TA1dat$sweptarea > TA1dat$area_sqkm,TA1dat$area_sqkm,TA1dat$sweptarea)
ind3 <- sum(TA1dat$sweptarea2,na.rm=T)/sum(TA1dat$area_sqkm)

# indicator 4 aggregation of fishing pressure
TA1dat <- TA1dat[order(TA1dat[,"avgsar"],decreasing = T),]
TA1dat$cumSSAR <- cumsum(TA1dat[,"avgsar"])
TA1dat$cumSSAR <- TA1dat$cumSSAR / sum(TA1dat[,"avgsar"])
ind4 <- min(which (TA1dat$cumSSAR > .9))/nrow(TA1dat)

# indicator 5 persistently unfished areas
ind5 <- length(which(TA1dat$avgsar == 0))/nrow(TA1dat)

# all areas without impact prediction
ind6_PD <- NA; ind6_IL<- NA; ind7_PD <-NA; ind7_IL <- NA

if (p %in% regions_with_impact){
  # indicator 6 average impact - PD model
  nam <- c(state_year)
  TA1dat_PD <- cbind(TA1dat, State_reg[match(TA1dat$csquares,State_reg$Fisheries.csquares), c(nam)])
  TA1dat_PD[,c(nam)][is.na(TA1dat_PD[,c(nam)])] <- 1
  TA1dat_PD$avgstate <- rowMeans(TA1dat_PD[,state_year]) 
  ind6_PD <- 1- mean(TA1dat_PD[,"avgstate"])
  
  # indicator 7 proportion of area with impact < 0.2 - PD model
  ind7_PD <- length(which(TA1dat_PD[,"avgstate"] >= 0.8))/nrow(TA1dat_PD)
  
  # indicator 6 average impact - IL model
  nam <- c(state_year)
  TA1dat_IL <- cbind(TA1dat, State_reg_IL[match(TA1dat$csquares,State_reg_IL$Fisheries.csquares), c(nam)])
  TA1dat_IL[,c(nam)][is.na(TA1dat_IL[,c(nam)])] <- 1
  TA1dat_IL$avgstate <- rowMeans(TA1dat_IL[,state_year]) 
  ind6_IL <- 1- mean(TA1dat_IL[,"avgstate"])
  
  # indicator 7 proportion of area with impact < 0.2 - IL model
  ind7_IL <- length(which(TA1dat_IL[,"avgstate"] >= 0.8))/nrow(TA1dat_IL)
}

A1table <- c(ind1,ind2,ind3,ind4,ind5,ind6_PD,ind6_IL,ind7_PD,ind7_IL)
save(A1table, file="TableA1.RData")

#####
# Table A.2
################
TA2dat <-  Region@data

# remove areas deeper than 200 meter for overview of Greater North Sea
if (Assregion == "Greater North Sea"){
  TA2dat <-  subset(TA2dat,TA2dat$Depth >= -200)
}

TA2dat$grid <- 1
TA2dat$MSFD <- as.character(TA2dat$MSFD)
TA2dat$MSFD[TA2dat$MSFD=="Na"]= "Unknown"
TA2dat$MSFD[TA2dat$MSFD=="<Na>"]= "Unknown"
TA2dat$MSFD[is.na(TA2dat$MSFD)]= "Unknown"

nam <- c(SSAR_year,weight_year,value_year)
TA2dat <- cbind(TA2dat, Fisheries[match(TA2dat$csquares,Fisheries$csquares), c(nam)])
TA2dat[,c(nam)][is.na(TA2dat[,c(nam)])] <- 0
TA2dat$avgsar <- rowMeans(TA2dat[,SSAR_year]) 
TA2dat$avgweight <- rowMeans(TA2dat[,weight_year],na.rm=T)
TA2dat$avgvalue <- rowMeans(TA2dat[,value_year],na.rm=T) 

TA2dat$sweptarea <- TA2dat[,"avgsar"]*TA2dat$area_sqkm
TA2dat$propgridfished <- ifelse(TA2dat[,"avgsar"] > 0,1,0)
TA2dat$propswept <- TA2dat$sweptarea
TA2dat$propswept <- ifelse(TA2dat$sweptarea > TA2dat$area_sqkm,TA2dat$area_sqkm,TA2dat$sweptarea)

nam <- c("area_sqkm","grid", "avgweight", "avgvalue","avgsar","sweptarea","propgridfished","propswept")
indexcol <- which(names(TA2dat) %in% nam)
A2table = aggregate( TA2dat[, indexcol], by= list(TA2dat$MSFD), FUN=function(x){sum(x)})
names(A2table)[1] = 'MSFD'

A2table <- as.data.frame(A2table)
A2table <- A2table[order(A2table$area_sqkm,decreasing = T),]
A2table$MSFD <- as.character(A2table$MSFD)

A2table$avgsar <- A2table$avgsar/A2table$grid
A2table$area_sqkm <- A2table$area_sqkm/1000 
A2table$sweptarea <- A2table$sweptarea/1000
A2table$avgweight <- A2table$avgweight/1000000
A2table$avgvalue <- A2table$avgvalue/1000000
A2table$propgridfished <- A2table$propgridfished / A2table$grid
A2table$propswept <- (A2table$propswept/1000) / A2table$area_sqkm

A2table$eff_fish <- NA
llenght <- max(which(A2table$grid>20))
for (i in 1:llenght){
  hab <- subset(TA2dat,TA2dat$MSFD == A2table[i,1])
  hab <- hab[order(hab[,"avgsar"],decreasing = T),]
  hab$cumSSAR <- cumsum(hab[,"avgsar"])
  hab$cumSSAR <- hab$cumSSAR / sum(hab[,"avgsar"])
  A2table$eff_fish[i] <- ifelse(is.na(hab$cumSSAR[1]),NA,min(which (hab$cumSSAR > .9))/nrow(hab))
}

save(A2table, file="TableA2.RData")

#####
# Figure A.3
################
A3dat <-  Region@data

# remove areas deeper than 200 meter for overview of Greater North Sea
if (Assregion == "Greater North Sea"){
  A3dat <-  subset(A3dat,A3dat$Depth >= -200)
}

SSARNames <- paste("surface_sar",Period,sep="_")
A3dat <- cbind(A3dat, Fisheries[match(A3dat$csquares,Fisheries$csquares), c(SSARNames)])
A3dat[,c(SSARNames)][is.na(A3dat[,c(SSARNames)])] <- 0

# left panel
# calculate for all data
indexcol <- which(names(A3dat) %in% SSARNames)
All = apply(A3dat[, indexcol], 2,  FUN=function(x){mean(x)})

# and calculate for most common habitat types
nam <- c("MSFD",SSARNames)
indexcol <- which(names(A3dat) %in% nam)
table_MSFD <- sort(table(A3dat$MSFD),decreasing = T)
mostcommonMSFD <- names(table_MSFD)[1:4]

AvgMSFD<- A3dat %>% 
  select(all_of(indexcol)) %>%
  filter(MSFD %in% c(mostcommonMSFD))

indexcol <- which(names(AvgMSFD) %in% SSARNames)
AvgMSFD2 = aggregate(AvgMSFD[, indexcol], by= list(AvgMSFD$MSFD), FUN=function(x){mean(x)})
names(AvgMSFD2)[1]= 'MSFD'
AvgMSFD2<-as.data.frame(AvgMSFD2)

A3left <-cbind((All),t(AvgMSFD2[1:4,2:(length(Period)+1)]),Period)
colnames(A3left) <-c("All",as.character(AvgMSFD2[1:4,1]),"Year")

# middle panel
A3middle <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))

rown<-c()
for (i in 1:length(Period)){
  nyear <-  paste("surface_sar",Period[i],sep="_")
  
  A3middle[i,1] <- length(which(A3dat[,nyear]>0.1))/length(A3dat[,nyear])
  A3middle[i,2] <- length(which(A3dat[,nyear][A3dat$MSFD == mostcommonMSFD[1]]>0.1))/length(A3dat[,nyear][A3dat$MSFD == mostcommonMSFD[1]])
  A3middle[i,3] <- length(which(A3dat[,nyear][A3dat$MSFD == mostcommonMSFD[2]]>0.1))/length(A3dat[,nyear][A3dat$MSFD == mostcommonMSFD[2]])
  A3middle[i,4] <- length(which(A3dat[,nyear][A3dat$MSFD == mostcommonMSFD[3]]>0.1))/length(A3dat[,nyear][A3dat$MSFD == mostcommonMSFD[3]])
  A3middle[i,5] <- length(which(A3dat[,nyear][A3dat$MSFD == mostcommonMSFD[4]]>0.1))/length(A3dat[,nyear][A3dat$MSFD == mostcommonMSFD[4]])
  rown<-c(rown,paste("Prop_fished",Period[i],sep="_"))
}
A3middle<-cbind(A3middle,Period)
colnames(A3middle) <-colnames(A3left)
rownames(A3middle) <-rown

# right panel
A3right <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))

rown<-c()
for (i in 1:length(Period)){
  nyear <-  paste("surface_sar",Period[i],sep="_")
  grd<-min(which(cumsum(sort(A3dat[,nyear],dec=T))/sum(A3dat[,nyear])>0.9))
  A3right[i,1]<-grd/nrow(A3dat)
  
  hab1<-subset(A3dat,A3dat$MSFD == mostcommonMSFD[1])
  grd<-min(which(cumsum(sort(hab1[,nyear],dec=T))/sum(hab1[,nyear])>0.9))
  A3right[i,2]<-grd/nrow(hab1)
  
  hab2<-subset(A3dat,A3dat$MSFD == mostcommonMSFD[2])
  grd<-min(which(cumsum(sort(hab2[,nyear],dec=T))/sum(hab2[,nyear])>0.9))
  A3right[i,3]<-grd/nrow(hab2)
  
  hab3<-subset(A3dat,A3dat$MSFD == mostcommonMSFD[3])
  grd<-min(which(cumsum(sort(hab3[,nyear],dec=T))/sum(hab3[,nyear])>0.9))
  A3right[i,4]<-grd/nrow(hab3)
  
  hab4<-subset(A3dat,A3dat$MSFD == mostcommonMSFD[4])
  grd<-min(which(cumsum(sort(hab4[,nyear],dec=T))/sum(hab4[,nyear])>0.9))
  A3right[i,5]<-grd/nrow(hab4)
  
  rown<-c(rown,paste("Prop_90_effort",Period[i],sep="_"))
}

A3right<-cbind(A3right,Period)
colnames(A3right) <-colnames(A3left)
rownames(A3right) <-rown

A3fig <- list(A3left,A3middle,A3right)
save(A3fig, file="FigureA3.RData")

#####
# Figure A.4
################
A4dat <-  Region@data

# remove areas deeper than 200 meter for overview of Greater North Sea
if (Assregion == "Greater North Sea"){
  A4dat <-  subset(A4dat,A4dat$Depth >= -200)
}

nam <- c(SSAR_year,weight_year,value_year)
A4dat <- cbind(A4dat, Fisheries[match(A4dat$csquares,Fisheries$csquares), c(nam)])
A4dat[ ,c(nam)][is.na(A4dat[ ,c(nam)]) ] = 0 
A4dat$avgsar <- rowMeans(A4dat[,SSAR_year]) 
A4dat$avgweight <- rowMeans(A4dat[,weight_year],na.rm=T)
A4dat$avgvalue <- rowMeans(A4dat[,value_year],na.rm=T) 
A4dat<-A4dat[order(-A4dat[,"avgsar"]),]
A4dat$sweptcumu <-cumsum(A4dat[,"avgsar"])/sum(A4dat[,"avgsar"])
A4dat$landcumu  <-cumsum(A4dat[,"avgweight"])/sum(A4dat[,"avgweight"])
A4dat$valuecumu <-cumsum(A4dat[,"avgvalue"])/sum(A4dat[,"avgvalue"])
A4dat$indixcumu <-(1:nrow(A4dat))/nrow(A4dat)

save(A4dat, file="FigureA4.RData")

#####
# Table A.3
################
datT3 <-Region@data

# remove areas deeper than 200 meter for overview of Greater North Sea
if (Assregion == "Greater North Sea"){
  datT3 <-  subset(datT3,datT3$Depth >= -200)
}

gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")

A3table <- as.data.frame(matrix(data=NA, ncol=length(gears), nrow = 5))

for (pp in 1:length(gears)){
  datgear <- datT3
  
  nam <- paste(rep(paste(gears[pp],"surface_sar",sep="_"),length(AssPeriod)),AssPeriod,sep="_")
  datgear <- cbind(datgear, FisheriesMet[match(datgear$csquares,FisheriesMet$csquares), c(nam)])
  datgear[,c(nam)][is.na(datgear[,c(nam)])] <- 0
  datgear$avgsar <- rowMeans(datgear[,nam])
  
  datgear$sweptarea <- datgear[,"avgsar"]*datgear$area_sqkm
  A3table[1,pp] <- sum(datgear$sweptarea,na.rm=T)/1000
  
  nam <- paste(rep(paste(gears[pp],"total_weight",sep="_"),length(AssPeriod)),AssPeriod,sep="_")
  datgear <- cbind(datgear, FisheriesMet[match(datgear$csquares,FisheriesMet$csquares), c(nam)])
  datgear[,c(nam)][is.na(datgear[,c(nam)])] <- 0
  datgear$avgweight <- rowMeans(datgear[,nam])
  A3table[2,pp] <- sum(datgear$avgweight)/1000000
  
  nam <- paste(rep(paste(gears[pp],"total_value",sep="_"),length(AssPeriod)),AssPeriod,sep="_")
  datgear <- cbind(datgear, FisheriesMet[match(datgear$csquares,FisheriesMet$csquares), c(nam)])
  datgear[,c(nam)][is.na(datgear[,c(nam)])] <- 0
  datgear$avgvalue <- rowMeans(datgear[,nam])
  A3table[3,pp] <- sum(datgear$avgvalue)/1000000
  
  A3table[4,pp] <- (sum(datgear$avgweight)/1000000) / (sum(datgear$sweptarea,na.rm=T)/1000)
  A3table[5,pp] <- (sum(datgear$avgvalue)/1000000) / (sum(datgear$sweptarea,na.rm=T)/1000)
}

colnames(A3table) <- gears
rownames(A3table) <- c("Area swept (1000 km2)","Landings (1000 tonnes)","Value (10^6 euro)",
                       "Landings (1000 tonnes)/Area swept (1000 km2)","Value (10^6 euro)/Area swept (1000 km2)")

save(A3table, file="TableA3.RData")


### impact estimations
if (p %in% regions_with_impact){
  
  #####
  # Figure A.5
  ################
  figA5 <- Region@data
  nam <- c(state_year)
  figA5 <- cbind(figA5, State_reg_IL[match(figA5$csquares,State_reg_IL$Fisheries.csquares), c(nam)])
  indexcol <- which(names(figA5) %in% state_year)
  colnames(figA5)[indexcol] <-  paste("state_IL",AssPeriod,sep="_")
  
  figA5 <- cbind(figA5, State_reg[match(figA5$csquares,State_reg$Fisheries.csquares), c(nam)])
  save(figA5, file="FigureA5.RData")
  
  #####
  # Figure A.6
  ################
  # estimate impact based on inverse longevity
  A6dat <- Region@data
  
  # remove areas deeper than 200 meter for overview of Greater North Sea
  if (Assregion == "Greater North Sea"){
    A6dat <-  subset(A6dat,A6dat$Depth >= -200)
  }
  
  stateNames <- paste("state",Period,sep="_")
  A6dat <- cbind(A6dat, State_reg_IL[match(A6dat$csquares,State_reg_IL$Fisheries.csquares), c(stateNames)])
  A6dat[,c(stateNames)][is.na(A6dat[,c(stateNames)])] <- 1
  
  # left panel
  indexcol <- which(names(A6dat) %in% stateNames)
  All = apply( A6dat[, indexcol], 2, FUN=function(x){mean(x)})
  
  # and calculate for most common habitat types
  nam <- c("MSFD",stateNames)
  indexcol <- which(names(A6dat) %in% nam)
  AvgMSFD<- A6dat %>% 
    select(all_of(indexcol)) %>%
    filter(MSFD %in% c(mostcommonMSFD))
  indexcol <- which(names(AvgMSFD) %in% stateNames)
  AvgMSFD2 = aggregate( AvgMSFD[, indexcol], by= list(AvgMSFD$MSFD), FUN=function(x){mean(x)})
  names(AvgMSFD2)= 'MSFD'
  AvgMSFD2<-as.data.frame(AvgMSFD2)
  
  A6left <-cbind(All,t(AvgMSFD2[1:4,2:(length(Period)+1)]),Period)
  colnames(A6left) <-c("All",as.character(AvgMSFD2[1:4,1]),"Year")
  
  # right panel
  A6right <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))
  
  mostcommonMSFD <- sort(mostcommonMSFD)
  rown<-c()
  for (i in 1:length(Period)){
    nyear <-  paste("state",Period[i],sep="_")
    
    A6right[i,1] <- length(which(A6dat[,nyear]>0.8))/length(A6dat[,nyear])
    A6right[i,2] <- length(which(A6dat[,nyear][A6dat$MSFD == mostcommonMSFD[1]]>0.8))/length(A6dat[,nyear][A6dat$MSFD == mostcommonMSFD[1]])
    A6right[i,3] <- length(which(A6dat[,nyear][A6dat$MSFD == mostcommonMSFD[2]]>0.8))/length(A6dat[,nyear][A6dat$MSFD == mostcommonMSFD[2]])
    A6right[i,4] <- length(which(A6dat[,nyear][A6dat$MSFD == mostcommonMSFD[3]]>0.8))/length(A6dat[,nyear][A6dat$MSFD == mostcommonMSFD[3]])
    A6right[i,5] <- length(which(A6dat[,nyear][A6dat$MSFD == mostcommonMSFD[4]]>0.8))/length(A6dat[,nyear][A6dat$MSFD == mostcommonMSFD[4]])
    rown<-c(rown,paste("State>0.8",Period[i],sep="_"))
  }
  A6right<-cbind(A6right,Period)
  colnames(A6right) <-colnames(A6left)
  rownames(A6right) <-rown
  
  A6left_IL <- A6left
  A6right_IL <- A6right
  
  A6dat <- Region@data
  stateNames <- paste("state",Period,sep="_")
  A6dat <- cbind(A6dat, State_reg[match(A6dat$csquares,State_reg$Fisheries.csquares), c(stateNames)])
  A6dat[,c(stateNames)][is.na(A6dat[,c(stateNames)])] <- 1
  
  # left panel
  indexcol <- which(names(A6dat) %in% stateNames)
  All = apply( A6dat[, indexcol], 2, FUN=function(x){mean(x)})
  
  # and calculate for most common habitat types
  nam <- c("MSFD",stateNames)
  indexcol <- which(names(A6dat) %in% nam)
  AvgMSFD<- A6dat %>% 
    select(all_of(indexcol)) %>%
    filter(MSFD %in% c(mostcommonMSFD))
  indexcol <- which(names(AvgMSFD) %in% stateNames)
  AvgMSFD2 = aggregate( AvgMSFD[, indexcol], by= list(AvgMSFD$MSFD), FUN=function(x){mean(x)})
  names(AvgMSFD2)= 'MSFD'
  AvgMSFD2<-as.data.frame(AvgMSFD2)
  
  A6left <-cbind(All,t(AvgMSFD2[1:4,2:(length(Period)+1)]),Period)
  colnames(A6left) <-c("All",as.character(AvgMSFD2[1:4,1]),"Year")
  
  # right panel
  A6right <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))
  
  mostcommonMSFD <- sort(mostcommonMSFD)
  rown<-c()
  for (i in 1:length(Period)){
    nyear <-  paste("state",Period[i],sep="_")
    
    A6right[i,1] <- length(which(A6dat[,nyear]>0.8))/length(A6dat[,nyear])
    A6right[i,2] <- length(which(A6dat[,nyear][A6dat$MSFD == mostcommonMSFD[1]]>0.8))/length(A6dat[,nyear][A6dat$MSFD == mostcommonMSFD[1]])
    A6right[i,3] <- length(which(A6dat[,nyear][A6dat$MSFD == mostcommonMSFD[2]]>0.8))/length(A6dat[,nyear][A6dat$MSFD == mostcommonMSFD[2]])
    A6right[i,4] <- length(which(A6dat[,nyear][A6dat$MSFD == mostcommonMSFD[3]]>0.8))/length(A6dat[,nyear][A6dat$MSFD == mostcommonMSFD[3]])
    A6right[i,5] <- length(which(A6dat[,nyear][A6dat$MSFD == mostcommonMSFD[4]]>0.8))/length(A6dat[,nyear][A6dat$MSFD == mostcommonMSFD[4]])
    rown<-c(rown,paste("State>0.8",Period[i],sep="_"))
  }
  A6right<-cbind(A6right,Period)
  colnames(A6right) <-colnames(A6left)
  rownames(A6right) <-rown
  
  A6fig <- list(A6left,A6right,A6left_IL,A6right_IL)
  save(A6fig, file="FigureA6.RData")
  
  ######
  # Figure A.8
  ################
  A8dat <- Region@data
  
  # remove areas deeper than 200 meter for overview of Greater North Sea
  if (Assregion == "Greater North Sea"){
    A8dat <-  subset(A8dat,A8dat$Depth >= -200)
  }
  
  gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  
  nam <- paste("state",rep(gears[1],length(Period)),Period,sep="_")
  A8dat <- cbind(A8dat, State_reg[match(A8dat$csquares,State_reg$Fisheries.csquares), c(nam)])
  A8dat[,c(nam)][is.na(A8dat[,c(nam)])] <- 1
  Avgear <- aggregate( A8dat[, nam], by= list(A8dat$MSFD), FUN=function(x){mean(x)})
  colnames(Avgear)[1] <- 'MSFD'
  AvMSFD_metier <- as.data.frame(Avgear)
  
  for (p in 2:length(gears)){
    nam <- paste("state",rep(gears[p],length(Period)),Period,sep="_")
    A8dat <- cbind(A8dat, State_reg[match(A8dat$csquares,State_reg$Fisheries.csquares), c(nam)])
    A8dat[,c(nam)][is.na(A8dat[,c(nam)])] <- 1
    Avgear <- aggregate( A8dat[, nam], by= list(A8dat$MSFD), FUN=function(x){mean(x)})
    colnames(Avgear)[1] <- 'MSFD'
    AvMSFD_metier <- cbind(AvMSFD_metier, Avgear[match(AvMSFD_metier$MSFD,Avgear$MSFD), c(2:(length(Period)+1))])
  }
  
  A8_A9fig <- subset(AvMSFD_metier, AvMSFD_metier$MSFD %in%  c(mostcommonMSFD))
  save(A8_A9fig, file="FigureA8_A9.RData")
  
  # redo analysis for inverse longevity impact
  A8dat <- Region@data
  
  gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  
  nam <- paste("state",rep(gears[1],length(Period)),Period,sep="_")
  A8dat <- cbind(A8dat, State_reg_IL[match(A8dat$csquares,State_reg_IL$Fisheries.csquares), c(nam)])
  A8dat[,c(nam)][is.na(A8dat[,c(nam)])] <- 1
  Avgear <- aggregate( A8dat[, nam], by= list(A8dat$MSFD), FUN=function(x){mean(x)})
  colnames(Avgear)[1] <- 'MSFD'
  AvMSFD_metier <- as.data.frame(Avgear)
  
  for (p in 2:length(gears)){
    nam <- paste("state",rep(gears[p],length(Period)),Period,sep="_")
    A8dat <- cbind(A8dat, State_reg_IL[match(A8dat$csquares,State_reg_IL$Fisheries.csquares), c(nam)])
    A8dat[,c(nam)][is.na(A8dat[,c(nam)])] <- 1
    Avgear <- aggregate( A8dat[, nam], by= list(A8dat$MSFD), FUN=function(x){mean(x)})
    colnames(Avgear)[1] <- 'MSFD'
    AvMSFD_metier <- cbind(AvMSFD_metier, Avgear[match(AvMSFD_metier$MSFD,Avgear$MSFD), c(2:(length(Period)+1))])
  }
  
  A8_A9fig <- subset(AvMSFD_metier, AvMSFD_metier$MSFD %in%  c(mostcommonMSFD))
  save(A8_A9fig, file="FigureA8_A9_IL.RData")
  
  #####
  # Table A.4
  ################
  datT4 <- Region@data
  
  # remove areas deeper than 200 meter for overview of Greater North Sea
  if (Assregion == "Greater North Sea"){
    datT4 <-  subset(datT4,datT4$Depth >= -200)
  }
  
  gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  
  A4table <- as.data.frame(matrix(data=NA, ncol=length(gears), nrow = 4))
  
  for (pp in 1:length(gears)){
    datgear <- datT4
    
    nam <- paste(rep(paste(gears[pp],"surface_sar",sep="_"),length(AssPeriod)),AssPeriod,sep="_")
    datgear <- cbind(datgear, FisheriesMet[match(datgear$csquares,FisheriesMet$csquares), c(nam)])
    datgear[,c(nam)][is.na(datgear[,c(nam)])] <- 0
    datgear$avgsar <- rowMeans(datgear[,nam])
    
    nam <- paste(rep(paste(gears[pp],"total_weight",sep="_"),length(AssPeriod)),AssPeriod,sep="_")
    datgear <- cbind(datgear, FisheriesMet[match(datgear$csquares,FisheriesMet$csquares), c(nam)])
    datgear[,c(nam)][is.na(datgear[,c(nam)])] <- 0
    datgear$avgweight <- rowMeans(datgear[,nam])
    
    nam <- paste(rep(paste(gears[pp],"total_value",sep="_"),length(AssPeriod)),AssPeriod,sep="_")
    datgear <- cbind(datgear, FisheriesMet[match(datgear$csquares,FisheriesMet$csquares), c(nam)])
    datgear[,c(nam)][is.na(datgear[,c(nam)])] <- 0
    datgear$avgvalue <- rowMeans(datgear[,nam])
    
    nam <- paste(rep(paste("state",gears[pp],sep="_"),length(AssPeriod)),AssPeriod,sep="_")
    datgear_PD <- cbind(datgear, State_reg[match(datgear$csquares,State_reg$Fisheries.csquares), c(nam)])
    datgear_PD[,c(nam)][is.na(datgear_PD[,c(nam)])] <- 0
    datgear_PD$avgimpact <- 1- rowMeans(datgear_PD[,nam])
    datgear_PD <- subset(datgear_PD,datgear_PD$avgsar >0)
    A4table[1,pp] <- (sum(datgear_PD$avgweight)/1000000) / sum(datgear_PD$avgimpact)
    A4table[2,pp] <- (sum(datgear_PD$avgvalue)/1000000) / sum(datgear_PD$avgimpact)
    
    datgear_IL <- cbind(datgear, State_reg_IL[match(datgear$csquares,State_reg_IL$Fisheries.csquares), c(nam)])
    datgear_IL[,c(nam)][is.na(datgear_IL[,c(nam)])] <- 0
    datgear_IL$avgimpact <- 1- rowMeans(datgear_IL[,nam])
    datgear_IL <- subset(datgear_IL,datgear_IL$avgsar >0)
    A4table[3,pp] <- (sum(datgear_IL$avgweight)/1000000) / sum(datgear_IL$avgimpact)
    A4table[4,pp] <- (sum(datgear_IL$avgvalue)/1000000) / sum(datgear_IL$avgimpact)
  }
  
  colnames(A4table) <- gears
  rownames(A4table) <- c("Landings (1000 tonnes)/PD impact","Value (10^6 euro)/PD impact",
                         "Landings (1000 tonnes)/L1 impact","Value (10^6 euro)/L1 impact")
  
  save(A4table, file="TableA4.RData")
  
}

rm(list= ls()[!(ls() %in% c('pathdir','pathdir_nogit','Assregion_index','Assunit_index','EcoReg_index',
                            'Period','AssPeriod',"EcoReg",'Fisheries','FisheriesMet','p','regions_with_impact',
                            'Region','State_reg','State_reg_IL',"Assunit","Assregion"))])