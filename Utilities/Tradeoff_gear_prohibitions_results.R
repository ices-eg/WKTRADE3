  
  
  # set assessment variables
  SSAR_year <- paste("surface_sar",AssPeriod,sep="_")
  state_year <- paste("state",AssPeriod,sep="_")
  weight_year <- paste("total_weight",AssPeriod,sep="_")
  value_year <- paste("total_value",AssPeriod,sep="_")
  
  Region <- Region[!(is.na(Region$medlong)),]
  idx <- which(names(Region@data)== "long")
  colnames(Region@data)[idx]  <- "longitude"
  idx <- which(names(Region@data)== "lat")
  colnames(Region@data)[idx]  <- "latitude"
  
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
  tab_gears <- as.data.frame(matrix(NA,nrow=11,ncol=5))
  
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
  tab_gears[1,1] <- sum(sum_values)/1000000
  
  #####
  sum_weight <- c()  

  for (pp in 1:length(years)){
    nam <- paste(paste(gears,"total_weight",sep="_"),years[pp],sep="_")
    current_Fish[,c(nam)][is.na(current_Fish[,c(nam)])] <- 0
    sum_weight <- cbind(sum_weight,rowSums(current_Fish[,nam])) 
  }
  sum_weight <- rowMeans(sum_weight)
  tab_gears[1,2] <- sum(sum_weight)/1000000  

#####
  sum_sar <- c()  
  
  for (pp in 1:length(years)){
    nam <- paste(paste(gears,"surface_sar",sep="_"),years[pp],sep="_")
    current_Fish[,c(nam)][is.na(current_Fish[,c(nam)])] <- 0
    sum_sar <- cbind(sum_sar,rowSums(current_Fish[,nam])) 
  }
  sum_sar <- rowMeans(sum_sar)
  tab_gears[1,3] <- length(sum_sar[sum_sar == 0])/length(sum_sar)*100 

# get PD impact
  nam <- c(state_year)
  current_PD[,c(nam)][is.na(current_PD[,c(nam)])] <- 1
  current_PD$avgimpact <- 1- rowMeans(current_PD[,state_year]) 
  tab_gears[1,4] <- mean(current_PD$avgimpact)

  # get IL impact
  nam <- c(state_year)
  current_IL[,c(nam)][is.na(current_IL[,c(nam)])] <- 1
  current_IL$avgimpact <- 1- rowMeans(current_IL[,state_year]) 
  tab_gears[1,5] <- mean(current_IL$avgimpact)

# now get data per gear grouping
  pathdir_tradeoff <- (paste(pathdir_nogit,"Producing figures and tables",Assunit,Assregion,"Trade-offs",sep="/"))  
  gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")

  for (iGear in 1:length(gears)){
    setwd(pathdir_tradeoff)  
    load(paste(paste("state_IL",gears[iGear],sep="_"),"RData",sep="."))
    State_reg_IL <- State_reg # rename the state from the inverse longevity
    load(paste(paste("state",gears[iGear],sep="_"),"RData",sep="."))
    load(paste(paste("FisheriesMet",gears[iGear],sep="_"),"RData",sep="."))
    
    nb <- ncol(FisheriesMet)
    Fish <- cbind(Region@data, FisheriesMet[match(Region@data$csquares, FisheriesMet$csquares), c(2:nb)])
    Fish <- subset(Fish,Fish$Depth >= -200 & Fish$Depth < 0)
    
    PD <-  cbind(Region@data, State_reg[match(Region@data$csquares,State_reg$Fisheries.csquares), c(state_year)])
    PD <- subset(PD,PD$Depth >= -200 & PD$Depth < 0)
    
    IL <-  cbind(Region@data, State_reg_IL[match(Region@data$csquares,State_reg_IL$Fisheries.csquares), c(state_year)])
    IL <- subset(IL,IL$Depth >= -200 & IL$Depth < 0)
    
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
    tab_gears[iGear+1,1] <- sum(sum_values)/1000000
    
    #####
    sum_weight <- c()  
    
    for (pp in 1:length(years)){
      nam <- paste(paste(gears,"total_weight",sep="_"),years[pp],sep="_")
      current_Fish[,c(nam)][is.na(current_Fish[,c(nam)])] <- 0
      sum_weight <- cbind(sum_weight,rowSums(current_Fish[,nam])) 
    }
    sum_weight <- rowMeans(sum_weight)
    tab_gears[iGear+1,2] <- sum(sum_weight)/1000000  
    
    #####
    sum_sar <- c()  
    
    for (pp in 1:length(years)){
      nam <- paste(paste(gears,"surface_sar",sep="_"),years[pp],sep="_")
      current_Fish[,c(nam)][is.na(current_Fish[,c(nam)])] <- 0
      sum_sar <- cbind(sum_sar,rowSums(current_Fish[,nam])) 
    }
    sum_sar <- rowMeans(sum_sar)
    tab_gears[iGear+1,3] <- length(sum_sar[sum_sar == 0])/length(sum_sar)*100 
    
    # get PD impact
    nam <- c(state_year)
    current_PD[,c(nam)][is.na(current_PD[,c(nam)])] <- 1
    current_PD$avgimpact <- 1- rowMeans(current_PD[,state_year]) 
    tab_gears[iGear+1,4] <- mean(current_PD$avgimpact)
    
    # get IL impact
    nam <- c(state_year)
    current_IL[,c(nam)][is.na(current_IL[,c(nam)])] <- 1
    current_IL$avgimpact <- 1- rowMeans(current_IL[,state_year]) 
    tab_gears[iGear+1,5] <- mean(current_IL$avgimpact)
  }
  
# make the plot
  setwd(paste(pathdir,"5 - Output",Assunit,Assregion,sep="/"))
  pdf(paste(Assunit,Assregion,"MO_gear_prohibition.pdf",sep="_"),width=7,height=5.5) 
  
  for (qq in 2:11){
    tab_gears[qq,1:2] <- tab_gears[qq,1:2]/tab_gears[1,1:2]*100
  }
  tab_gears[1,1:2] <- tab_gears[1,1:2]/tab_gears[1,1:2]*100
  
  par(mfrow=c(2,3))
  par(oma = c(4, 4, 0, 0)) # make room (i.e. the 4's) for the overall x and y axis titles
  par(mar = c(2, 2, 1, 1)) # make the plots be closer together
  
  plot(tab_gears[,1]~tab_gears[,4], xlab="",ylab="",ylim=c(60,100),
       xlim=c(0.06,0.12),pch=16,las=1, xaxt="n")
  text(tab_gears[,4],tab_gears[,1]-.3, labels=c("",gears),cex=0.9, font=2)
  points(tab_gears[1,1]~tab_gears[1,4],pch= 16,col="blue",cex=1.3)
  
  plot(tab_gears[,1]~tab_gears[,5], xlab="",ylab="",ylim=c(60,100),
       xlim=c(0.4,0.7),pch=16,las=1, xaxt="n")
  text(tab_gears[,5],tab_gears[,1]-.3, labels=c("",gears),cex=0.9, font=2)
  points(tab_gears[1,1]~tab_gears[1,5],pch= 16,col="blue",cex=1.3)
  
  plot(tab_gears[,1]~tab_gears[,3], xlab="",ylab="",ylim=c(60,100),xlim=c(0,25),
       pch=16,las=1, xaxt="n")
  text(tab_gears[,3]-1.5,tab_gears[,1]-.3, labels=c("",gears),cex=0.9, font=2)
  points(tab_gears[1,1]~tab_gears[1,3],pch= 16,col="blue",cex=1.3)
  
  plot(tab_gears[,2]~tab_gears[,4], xlab="",ylab="",ylim=c(30,100),
       xlim=c(0.06,0.12),pch=16,las=1)
  text(tab_gears[,4],tab_gears[,2]-.3, labels=c("",gears),cex=0.9, font=2)
  points(tab_gears[1,2]~tab_gears[1,4],pch= 16,col="blue",cex=1.3)
  
  plot(tab_gears[,2]~tab_gears[,5], xlab="",ylab="",ylim=c(30,100),
       xlim=c(0.4,0.7),pch=16,las=1)
  text(tab_gears[,5],tab_gears[,2]-.3, labels=c("",gears),cex=0.9, font=2)
  points(tab_gears[1,2]~tab_gears[1,5],pch= 16,col="blue",cex=1.3)
  
  plot(tab_gears[,2]~tab_gears[,3], xlab="",ylab="",ylim=c(30,100),xlim=c(0,25),
       pch=16,las=1)
  text(tab_gears[,3]-1.5,tab_gears[,2]-.3, labels=c("",gears),cex=0.9, font=2)
  points(tab_gears[1,2]~tab_gears[1,3],pch= 16,col="blue",cex=1.3)
  
  mtext('average PD impact',at=.17,side=1,outer=T,cex=0.8, line=0.2)
  mtext('average L1 impact',at=.50,side=1,outer=T,cex=0.8, line=0.2)
  mtext('% unfished c-squares',at=.84,side=1,outer=T,cex=0.8, line=0.2) 
  mtext('Value (%) relative to ref.',at=.75,side=2,outer=T,cex=0.8, line=0.2) 
  mtext('Weight (%) relative to ref.',at=.25,side=2,outer=T,cex=0.8, line=0.2)    
  
  dev.off()    
  
  
  
  
    