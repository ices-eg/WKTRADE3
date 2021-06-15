
####
# analysis to assess the freeze footprint management option
##########
    
  # set assessment variables
  SSAR_year <- paste("surface_sar",AssPeriod,sep="_")
  state_year <- paste("state",AssPeriod,sep="_")
  weight_year <- paste("total_weight",AssPeriod,sep="_")
  value_year <- paste("total_value",AssPeriod,sep="_")

  # link all fisheries data to the 0-200 m estimate
  nb <- ncol(Fisheries)
  Fishsum <- cbind(Region@data, Fisheries[match(Region@data$csquares, Fisheries$csquares), c(2:nb)])
  Fishsum <- subset(Fishsum,Fishsum$Depth >= -200 & Fishsum$Depth < 0)
  
  nb <- ncol(FisheriesMet)
  Fish <- cbind(Region@data, FisheriesMet[match(Region@data$csquares, FisheriesMet$csquares), c(2:nb)])
  Fish <- subset(Fish,Fish$Depth >= -200 & Fish$Depth < 0)

  PD <-  cbind(Region@data, State_reg[match(Region@data$csquares,State_reg$Fisheries.csquares), c(state_year)])
  PD <- subset(PD,PD$Depth >= -200 & PD$Depth < 0)
  
  IL <-  cbind(Region@data, State_reg_IL[match(Region@data$csquares,State_reg_IL$Fisheries.csquares), c(state_year)])
  IL <- subset(IL,IL$Depth >= -200 & IL$Depth < 0)
  
  # create a table with the data points for value/landings/unfished c-square/impact PD/impact L1
  tab_freeze <- as.data.frame(matrix(NA,nrow=3,ncol=5))

# get current situation
  current_Fish <- Fish
  current_PD   <- PD
  current_IL   <- IL
  gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  years <- c(2013:2018)
  sum_values <- c()  
  
  for (pp in 1:length(years)){
    nam <- paste(paste(gears,"total_value",sep="_"),years[pp],sep="_")
    current_Fish[,c(nam)][is.na(current_Fish[,c(nam)])] <- 0
    sum_values <- cbind(sum_values,rowSums(current_Fish[,nam])) 
  }
  sum_values <- rowMeans(sum_values)
  tab_freeze[1,1] <- sum(sum_values)/1000000
  
  #####
  sum_weight <- c()  
  
  for (pp in 1:length(years)){
    nam <- paste(paste(gears,"total_weight",sep="_"),years[pp],sep="_")
    current_Fish[,c(nam)][is.na(current_Fish[,c(nam)])] <- 0
    sum_weight <- cbind(sum_weight,rowSums(current_Fish[,nam])) 
  }
  sum_weight <- rowMeans(sum_weight)
  tab_freeze[1,2] <- sum(sum_weight)/1000000  
  
  #####
  sum_sar <- c()  
  
  for (pp in 1:length(years)){
    nam <- paste(paste(gears,"surface_sar",sep="_"),years[pp],sep="_")
    current_Fish[,c(nam)][is.na(current_Fish[,c(nam)])] <- 0
    sum_sar <- cbind(sum_sar,rowSums(current_Fish[,nam])) 
  }
  sum_sar <- rowMeans(sum_sar)
  tab_freeze[1,3] <- length(sum_sar[sum_sar == 0])/length(sum_sar)*100 
  
  # get PD impact
  nam <- c(state_year)
  current_PD[,c(nam)][is.na(current_PD[,c(nam)])] <- 1
  current_PD$avgimpact <- 1- rowMeans(current_PD[,state_year]) 
  tab_freeze[1,4] <- mean(current_PD$avgimpact)
  
  # get IL impact
  nam <- c(state_year)
  current_IL[,c(nam)][is.na(current_IL[,c(nam)])] <- 1
  current_IL$avgimpact <- 1- rowMeans(current_IL[,state_year]) 
  tab_freeze[1,5] <- mean(current_IL$avgimpact)
  
### freeze footprint to areas with fishing in 2012-2014
  nam <- paste("surface_sar",c(2012:2014), sep="_")
  Fishsum[,nam][is.na(Fishsum[,nam])] <- 0
  Fishsum$freeze <- rowMeans(Fishsum[,nam])
  Fishsum$freeze[Fishsum$freeze >0] <- 1
  
  # get freeze situation for fishing value/catch
  freeze_Fish <- cbind(Fish, Fishsum[match(Fish$csquares, Fishsum$csquares), c("freeze")])
  colnames(freeze_Fish)[ncol(freeze_Fish)] <- "freeze"
  freeze_Fish <- subset(freeze_Fish,freeze_Fish$freeze == 1)
  
  gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  years <- c(2013:2018)
  sum_values <- c()  
  
  for (pp in 1:length(years)){
    nam <- paste(paste(gears,"total_value",sep="_"),years[pp],sep="_")
    freeze_Fish[,c(nam)][is.na(freeze_Fish[,c(nam)])] <- 0
    sum_values <- cbind(sum_values,rowSums(freeze_Fish[,nam])) 
  }
  sum_values <- rowMeans(sum_values)
  tab_freeze[2,1] <- sum(sum_values)/1000000
  
  #####
  sum_weight <- c()  
  
  for (pp in 1:length(years)){
    nam <- paste(paste(gears,"total_weight",sep="_"),years[pp],sep="_")
    freeze_Fish[,c(nam)][is.na(freeze_Fish[,c(nam)])] <- 0
    sum_weight <- cbind(sum_weight,rowSums(freeze_Fish[,nam])) 
  }
  sum_weight <- rowMeans(sum_weight)
  tab_freeze[2,2] <- sum(sum_weight)/1000000  
  
  #####
  tab_freeze[2,3] <- 100-(nrow(freeze_Fish)/nrow(Fishsum)*100) 
  
  # get PD impact
  freeze_PD <- cbind(current_PD, Fishsum[match(current_PD$csquares, Fishsum$csquares), c("freeze")])
  colnames(freeze_PD)[ncol(freeze_PD)] <- "freeze"
  freeze_PD$freezeimpact <- freeze_PD$avgimpact * freeze_PD$freeze 
  tab_freeze[2,4] <- mean(freeze_PD$freezeimpact)
  
  # get IL impact
  freeze_IL <- cbind(current_IL, Fishsum[match(current_IL$csquares, Fishsum$csquares), c("freeze")])
  colnames(freeze_IL)[ncol(freeze_IL)] <- "freeze"
  freeze_IL$freezeimpact <- freeze_IL$avgimpact * freeze_IL$freeze 
  tab_freeze[2,5] <- mean(freeze_IL$freezeimpact)
  
### freeze footprint to areas with 90% of fishing effort in 2012-2014
  nam <- paste("surface_sar",c(2012:2014), sep="_")
  Fishsum[,nam][is.na(Fishsum[,nam])] <- 0
  Fishsum$SAR_freeze <- rowMeans(Fishsum[,nam])
  
  Fishsum <- Fishsum[order(Fishsum[,"SAR_freeze"],decreasing = F),]
  Fishsum$cumSSAR <- cumsum(Fishsum[,"SAR_freeze"])
  Fishsum$cumSSAR <- Fishsum$cumSSAR / sum(Fishsum[,"freeze"])
  nb <- min(which(Fishsum$cumSSAR > 0.1))
  Fishsum$core_freeze <- 1
  Fishsum$core_freeze[1:nb] <- 0
  
  # get core freeze situation
  freezecore_Fish <- cbind(Fish, Fishsum[match(Fish$csquares, Fishsum$csquares), c("core_freeze")])
  colnames(freezecore_Fish)[ncol(freezecore_Fish)] <- "core_freeze"
  freezecore_Fish <- subset(freezecore_Fish,freezecore_Fish$core_freeze == 1)
  
  gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  years <- c(2013:2018)
  sum_values <- c()  
  
  for (pp in 1:length(years)){
    nam <- paste(paste(gears,"total_value",sep="_"),years[pp],sep="_")
    freezecore_Fish[,c(nam)][is.na(freezecore_Fish[,c(nam)])] <- 0
    sum_values <- cbind(sum_values,rowSums(freezecore_Fish[,nam])) 
  }
  sum_values <- rowMeans(sum_values)
  tab_freeze[3,1] <- sum(sum_values)/1000000
  
  #####
  sum_weight <- c()  
  
  for (pp in 1:length(years)){
    nam <- paste(paste(gears,"total_weight",sep="_"),years[pp],sep="_")
    freezecore_Fish[,c(nam)][is.na(freezecore_Fish[,c(nam)])] <- 0
    sum_weight <- cbind(sum_weight,rowSums(freezecore_Fish[,nam])) 
  }
  sum_weight <- rowMeans(sum_weight)
  tab_freeze[3,2] <- sum(sum_weight)/1000000  
  
  #####
  tab_freeze[3,3] <- 100-(nrow(freezecore_Fish)/nrow(Fishsum)*100) 
  
  # get PD impact
  freezecore_PD <- cbind(current_PD, Fishsum[match(current_PD$csquares, Fishsum$csquares), c("core_freeze")])
  colnames(freezecore_PD)[ncol(freezecore_PD)] <- "core_freeze"
  freezecore_PD$freezecoreimpact <- freezecore_PD$avgimpact * freezecore_PD$core_freeze 
  tab_freeze[3,4] <- mean(freezecore_PD$freezecoreimpact)
  
  # get IL impact
  freezecore_IL <- cbind(current_IL, Fishsum[match(current_IL$csquares, Fishsum$csquares), c("core_freeze")])
  colnames(freezecore_IL)[ncol(freezecore_IL)] <- "core_freeze"
  freezecore_IL$freezecoreimpact <- freezecore_IL$avgimpact * freezecore_IL$core_freeze 
  tab_freeze[3,5] <- mean(freezecore_IL$freezecoreimpact)
  
###### plot 
  setwd(paste(pathdir,"5 - Output",Assunit,Assregion,sep="/"))
  pdf(paste(Assunit,Assregion,"MO_freeze_footprint.pdf",sep="_"),width=7,height=5.5) 
  
  tab_freeze[3,1:2] <- tab_freeze[3,1:2]/tab_freeze[1,1:2]*100
  tab_freeze[2,1:2] <- tab_freeze[2,1:2]/tab_freeze[1,1:2]*100
  tab_freeze[1,1:2] <- tab_freeze[1,1:2]/tab_freeze[1,1:2]*100
  
  par(mfrow=c(2,3))
  par(oma = c(4, 4, 0, 0)) # make room (i.e. the 4's) for the overall x and y axis titles
  par(mar = c(2, 2, 1, 1)) # make the plots be closer together
  
  plot(tab_freeze[1:3,1]~tab_freeze[1:3,4],type="o", xlab="",ylab="",ylim=c(75,100),
       xlim=c(0.06,0.12),pch=16,las=1, xaxt="n")
  text(tab_freeze[1:3,4],tab_freeze[1:3,1]-1, labels=c("","freeze","core freeze"),cex=0.9, font=2)
  points(tab_freeze[1,1]~tab_freeze[1,4],pch= 16,col="blue",cex=1.3)
  
  plot(tab_freeze[1:3,1]~tab_freeze[1:3,5],type="o", xlab="",ylab="",ylim=c(75,100),
       xlim=c(0.4,0.7),pch=16,las=1, xaxt="n")
  text(tab_freeze[1:3,5],tab_freeze[1:3,1]-1, labels=c("","freeze","core freeze"),cex=0.9, font=2)
  points(tab_freeze[1,1]~tab_freeze[1,5],pch= 16,col="blue",cex=1.3)
  
  plot(tab_freeze[1:3,1]~tab_freeze[1:3,3],type="o", xlab="",ylab="",ylim=c(75,100),xlim=c(0,60),
       pch=16,las=1, xaxt="n")
  text(tab_freeze[1:3,3]-1.5,tab_freeze[1:3,1]-1, labels=c("","freeze","core freeze"),cex=0.9, font=2)
  points(tab_freeze[1,1]~tab_freeze[1,3],pch= 16,col="blue",cex=1.3)
  
  plot(tab_freeze[1:3,2]~tab_freeze[1:3,4],type="o", xlab="",ylab="",ylim=c(75,100),
       xlim=c(0.06,0.12),pch=16,las=1)
  text(tab_freeze[1:3,4],tab_freeze[1:3,2]-1, labels=c("","freeze","core freeze"),cex=0.9, font=2)
  points(tab_freeze[1,2]~tab_freeze[1,4],pch= 16,col="blue",cex=1.3)
  
  plot(tab_freeze[1:3,2]~tab_freeze[1:3,5],type="o", xlab="",ylab="",ylim=c(75,100),
       xlim=c(0.4,0.7),pch=16,las=1)
  text(tab_freeze[1:3,5],tab_freeze[1:3,2]-1, labels=c("","freeze","core freeze"),cex=0.9, font=2)
  points(tab_freeze[1,2]~tab_freeze[1,5],pch= 16,col="blue",cex=1.3)
  
  plot(tab_freeze[1:3,2]~tab_freeze[1:3,3],type="o", xlab="",ylab="",ylim=c(75,100),xlim=c(0,60),
       pch=16,las=1)
  text(tab_freeze[1:3,3]-1.5,tab_freeze[1:3,2]-1, labels=c("","freeze","core freeze"),cex=0.9, font=2)
  points(tab_freeze[1,2]~tab_freeze[1,3],pch= 16,col="blue",cex=1.3)

  mtext('average PD impact',at=.17,side=1,outer=T,cex=0.8, line=0.2)
  mtext('average L1 impact',at=.50,side=1,outer=T,cex=0.8, line=0.2)
  mtext('% unfished c-squares',at=.84,side=1,outer=T,cex=0.8, line=0.2) 
  mtext('Value (%) relative to ref.',at=.75,side=2,outer=T,cex=0.8, line=0.2) 
  mtext('Weight (%) relative to ref.',at=.25,side=2,outer=T,cex=0.8, line=0.2)    
  
  dev.off()    
  
  rm(list= ls()[!(ls() %in% c('pathdir','pathdir_nogit','Assregion_index','Assunit_index',
                              'Period','AssPeriod',"EcoReg",'Fisheries','FisheriesMet',
                              'Region','State_reg','State_reg_IL',"EcoReg_index","Assunit","Assregion"))])
  