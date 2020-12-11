
###### Results tradeoff-freeze footprint
  SSAR_year <- paste("surface_sar",AssPeriod,sep="_")
  state_year <- paste("state",AssPeriod,sep="_")
  weight_year <- paste("total_weight",AssPeriod,sep="_")
  value_year <- paste("total_value",AssPeriod,sep="_")

  pathdir_tradeoff <- (paste(pathdir_nogit,"Producing figures and tables",Assunit,Assregion,"Trade-offs",sep="/")) 

# load region
  setwd(paste(pathdir,"1 - Input env",sep="/"))
  load(paste(EcoReg,"region_grid_sensitivity.RData",sep="_")) 
  Region <- Region[!(is.na(Region$medlong)),]
  idx <- which(names(Region@data)== "long")
  colnames(Region@data)[idx]  <- "longitude"
  idx <- which(names(Region@data)== "lat")
  colnames(Region@data)[idx]  <- "latitude"
  
  Region <- Region[!(is.na(Region$medlong)),]
  Region <- subset(Region@data,Region@data$Depth >= -200 & Region@data$Depth < 0)

  ### load processed files
  load(paste(pathdir_nogit,paste(EcoReg,"state.RData",sep="_"),sep="/")) 
  load(paste(pathdir_nogit,paste(EcoReg,"fisheries_per_metier.RData",sep="_"),sep="/")) 
  
  load(paste(pathdir_tradeoff,"state_freezefootprint.RData",sep="/")) 
  load(paste(pathdir_tradeoff,"FisheriesMet_freezefootprint.RData",sep="/")) 
  
  load(paste(pathdir_tradeoff,"state_freezecorefootprint.RData",sep="/")) 
  load(paste(pathdir_tradeoff,"FisheriesMet_freezecorefootprint.RData",sep="/")) 
  
  tab_freeze <- as.data.frame(matrix(NA,nrow=4,ncol=4))
  nam <- c(value_year)
  TradeF <- cbind(Region, Fisheries[match(Region$csquares,Fisheries$csquares), c(nam)])
  TradeF[,c(nam)][is.na(TradeF[,c(nam)])] <- 0
  TradeF$avgvalue <- rowMeans(TradeF[,value_year]) 
  tab_freeze[1,1] <- sum(TradeF$avgvalue)/1000000
  
  nam <- c(weight_year)
  TradeF <- cbind(Region, Fisheries[match(Region$csquares,Fisheries$csquares), c(nam)])
  TradeF[,c(nam)][is.na(TradeF[,c(nam)])] <- 0
  TradeF$avgweight <- rowMeans(TradeF[,weight_year]) 
  tab_freeze[1,2] <- sum(TradeF$avgweight)/1000000
  
  nam <- c(state_year)
  TradeF <- cbind(Region, State_reg[match(Region$csquares,State_reg$Fisheries.csquares), c(nam)])
  TradeF[,c(nam)][is.na(TradeF[,c(nam)])] <- 1
  TradeF$avgimpact <- 1- rowMeans(TradeF[,state_year]) 
  tab_freeze[1,3] <- mean(TradeF$avgimpact)
  
  nam <- c(SSAR_year)
  TradeF <- cbind(Region, Fisheries[match(Region$csquares,Fisheries$csquares), c(nam)])
  TradeF[,c(nam)][is.na(TradeF[,c(nam)])] <- 0
  TradeF$avgsar <- rowMeans(TradeF[,SSAR_year]) 
  tab_freeze[1,4] <- length(TradeF$avgsar[TradeF$avgsar == 0])/nrow(TradeF)*100

### now check again estimated per metier (control to see if we get the same number) :: almost the same
  # SAR value is the same (not shown), hence some value/weight not distributed per metier
  # whereas the total value/weight is a bit higher (check later!) 
  gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  years <- c(2013:2018)
  sum_values <- c()  

  for (pp in 1:length(years)){
    nam <- paste(paste(gears,"total_value",sep="_"),years[pp],sep="_")
    TradeF <- cbind(Region, FisheriesMet[match(Region$csquares,FisheriesMet$csquares), c(nam)])
    TradeF[,c(nam)][is.na(TradeF[,c(nam)])] <- 0
    sum_values <- cbind(sum_values,rowSums(TradeF[,nam])) 
  }
  sum_values <- rowMeans(sum_values)
  tab_freeze[2,1] <- sum(sum_values)/1000000
  
  #####
  sum_weight <- c()  
  
  for (pp in 1:length(years)){
    nam <- paste(paste(gears,"total_weight",sep="_"),years[pp],sep="_")
    TradeF <- cbind(Region, FisheriesMet[match(Region$csquares,FisheriesMet$csquares), c(nam)])
    TradeF[,c(nam)][is.na(TradeF[,c(nam)])] <- 0
    sum_weight <- cbind(sum_weight,rowSums(TradeF[,nam])) 
  }
  sum_weight <- rowMeans(sum_weight)
  tab_freeze[2,2] <- sum(sum_weight)/1000000  

  ####
  nam <- c(state_year)
  TradeF <- cbind(Region, State_reg[match(Region$csquares,State_reg$Fisheries.csquares), c(nam)])
  TradeF[,c(nam)][is.na(TradeF[,c(nam)])] <- 1
  TradeF$avgimpact <- 1- rowMeans(TradeF[,state_year]) 
  tab_freeze[2,3] <- mean(TradeF$avgimpact)
  
  #####
  sum_sar <- c()  
  
  for (pp in 1:length(years)){
    nam <- paste(paste(gears,"surface_sar",sep="_"),years[pp],sep="_")
    TradeF <- cbind(Region, FisheriesMet[match(Region$csquares,FisheriesMet$csquares), c(nam)])
    TradeF[,c(nam)][is.na(TradeF[,c(nam)])] <- 0
    sum_sar <- cbind(sum_sar,rowSums(TradeF[,nam])) 
  }
  sum_sar <- rowMeans(sum_sar)
  tab_freeze[2,4] <- length(sum_sar[sum_sar == 0])/length(sum_sar)*100 
  
#### now do it  again for the freeze footprints
  gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  years <- c(2013:2018)
  sum_values <- c()  
  
  for (pp in 1:length(years)){
    nam <- paste(paste(gears,"total_value",sep="_"),years[pp],sep="_")
    TradeF <- cbind(Region, FisheriesMet_freeze[match(Region$csquares,FisheriesMet_freeze$csquares), c(nam)])
    TradeF[,c(nam)][is.na(TradeF[,c(nam)])] <- 0
    sum_values <- cbind(sum_values,rowSums(TradeF[,nam])) 
  }
  sum_values <- rowMeans(sum_values)
  tab_freeze[3,1] <- sum(sum_values)/1000000
  
  #####
  sum_weight <- c()  
  
  for (pp in 1:length(years)){
    nam <- paste(paste(gears,"total_weight",sep="_"),years[pp],sep="_")
    TradeF <- cbind(Region, FisheriesMet_freeze[match(Region$csquares,FisheriesMet_freeze$csquares), c(nam)])
    TradeF[,c(nam)][is.na(TradeF[,c(nam)])] <- 0
    sum_weight <- cbind(sum_weight,rowSums(TradeF[,nam])) 
  }
  sum_weight <- rowMeans(sum_weight)
  tab_freeze[3,2] <- sum(sum_weight)/1000000  
  
  ####
  nam <- c(state_year)
  TradeF <- cbind(Region, State_reg_freeze[match(Region$csquares,State_reg_freeze$Fisheries.csquares), c(nam)])
  TradeF[,c(nam)][is.na(TradeF[,c(nam)])] <- 1
  TradeF$avgimpact <- 1- rowMeans(TradeF[,state_year]) 
  tab_freeze[3,3] <- mean(TradeF$avgimpact)
  
  #####
  sum_sar <- c()  
  
  for (pp in 1:length(years)){
    nam <- paste(paste(gears,"surface_sar",sep="_"),years[pp],sep="_")
    TradeF <- cbind(Region, FisheriesMet_freeze[match(Region$csquares,FisheriesMet_freeze$csquares), c(nam)])
    TradeF[,c(nam)][is.na(TradeF[,c(nam)])] <- 0
    sum_sar <- cbind(sum_sar,rowSums(TradeF[,nam])) 
  }
  sum_sar <- rowMeans(sum_sar)
  tab_freeze[3,4] <- length(sum_sar[sum_sar == 0]) /length(sum_sar)*100 
  
### and for the core freeze footprint
  gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  years <- c(2013:2018)
  sum_values <- c()  
  
  for (pp in 1:length(years)){
    nam <- paste(paste(gears,"total_value",sep="_"),years[pp],sep="_")
    TradeF <- cbind(Region, FisheriesMet_freezecore[match(Region$csquares,FisheriesMet_freezecore$csquares), c(nam)])
    TradeF[,c(nam)][is.na(TradeF[,c(nam)])] <- 0
    sum_values <- cbind(sum_values,rowSums(TradeF[,nam])) 
  }
  sum_values <- rowMeans(sum_values)
  tab_freeze[4,1] <- sum(sum_values)/1000000
  
  #####
  sum_weight <- c()  
  
  for (pp in 1:length(years)){
    nam <- paste(paste(gears,"total_weight",sep="_"),years[pp],sep="_")
    TradeF <- cbind(Region, FisheriesMet_freezecore[match(Region$csquares,FisheriesMet_freezecore$csquares), c(nam)])
    TradeF[,c(nam)][is.na(TradeF[,c(nam)])] <- 0
    sum_weight <- cbind(sum_weight,rowSums(TradeF[,nam])) 
  }
  sum_weight <- rowMeans(sum_weight)
  tab_freeze[4,2] <- sum(sum_weight)/1000000  
  
  ####
  nam <- c(state_year)
  TradeF <- cbind(Region, State_reg_freezecore[match(Region$csquares,State_reg_freezecore$Fisheries.csquares), c(nam)])
  TradeF[,c(nam)][is.na(TradeF[,c(nam)])] <- 1
  TradeF$avgimpact <- 1- rowMeans(TradeF[,state_year]) 
  tab_freeze[4,3] <- mean(TradeF$avgimpact)
  
  #####
  sum_sar <- c()  
  
  for (pp in 1:length(years)){
    nam <- paste(paste(gears,"surface_sar",sep="_"),years[pp],sep="_")
    TradeF <- cbind(Region, FisheriesMet_freezecore[match(Region$csquares,FisheriesMet_freezecore$csquares), c(nam)])
    TradeF[,c(nam)][is.na(TradeF[,c(nam)])] <- 0
    sum_sar <- cbind(sum_sar,rowSums(TradeF[,nam])) 
  }
  sum_sar <- rowMeans(sum_sar)
  tab_freeze[4,4] <- length(sum_sar[sum_sar == 0]) /length(sum_sar)*100 
  
  ###### plot 
  tab_freeze <- tab_freeze[-1,]
  tab_freeze[3,1:2] <- tab_freeze[3,1:2]/tab_freeze[1,1:2]*100
  tab_freeze[2,1:2] <- tab_freeze[2,1:2]/tab_freeze[1,1:2]*100
  tab_freeze[1,1:2] <- tab_freeze[1,1:2]/tab_freeze[1,1:2]*100
  
  par(mfrow=c(2,2))
  par(oma = c(4, 4, 0, 0)) # make room (i.e. the 4's) for the overall x and y axis titles
  par(mar = c(2, 2, 1, 1)) # make the plots be closer together
  
  plot(tab_freeze[1:3,1]~tab_freeze[1:3,3],type="o", xlab="",ylab="",ylim=c(75,100),
       xlim=c(0.06,0.12),pch=16,las=1, xaxt="n")
  text(tab_freeze[1:3,3],tab_freeze[1:3,1]-1, labels=c("","freeze","core freeze"),cex=0.9, font=2)
  points(tab_freeze[1,1]~tab_freeze[1,3],pch= 16,col="blue",cex=1.3)
  
  plot(tab_freeze[1:3,1]~tab_freeze[1:3,4],type="o", xlab="",ylab="",ylim=c(75,100),xlim=c(0,20),
       pch=16,las=1, xaxt="n")
  text(tab_freeze[1:3,4]-1.5,tab_freeze[1:3,1]-1, labels=c("","freeze","core freeze"),cex=0.9, font=2)
  points(tab_freeze[1,1]~tab_freeze[1,4],pch= 16,col="blue",cex=1.3)

  plot(tab_freeze[1:3,2]~tab_freeze[1:3,3],type="o", xlab="",ylab="",ylim=c(75,100),
       xlim=c(0.06,0.12),pch=16,las=1)
  text(tab_freeze[1:3,3],tab_freeze[1:3,2]-1, labels=c("","freeze","core freeze"),cex=0.9, font=2)
  points(tab_freeze[1,2]~tab_freeze[1,3],pch= 16,col="blue",cex=1.3)
  
  plot(tab_freeze[1:3,2]~tab_freeze[1:3,4],type="o", xlab="",ylab="",ylim=c(75,100),xlim=c(0,20),
       pch=16,las=1)
  text(tab_freeze[1:3,4]-1.5,tab_freeze[1:3,2]-1, labels=c("","freeze","core freeze"),cex=0.9, font=2)
  points(tab_freeze[1,2]~tab_freeze[1,4],pch= 16,col="blue",cex=1.3)

  mtext('average impact',at=.25,side=1,outer=T,cex=1, line=0.2) 
  mtext('% unfished North Sea',at=.75,side=1,outer=T,cex=1, line=0.2) 
  mtext('Value (%) relative to ref.',at=.75,side=2,outer=T,cex=1, line=0.2) 
  mtext('Weight (%) relative to ref.',at=.25,side=2,outer=T,cex=1, line=0.2)     
  