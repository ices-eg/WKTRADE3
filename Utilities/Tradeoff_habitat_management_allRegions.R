
###### Tradeoff - effort reduction with spatial closure per MSFD habitat type
  SSAR_year <- paste("surface_sar",AssPeriod,sep="_")
  state_year <- paste("state",AssPeriod,sep="_")
  weight_year <- paste("total_weight",AssPeriod,sep="_")
  value_year <- paste("total_value",AssPeriod,sep="_")

  # set folder directory
  setwd(paste(pathdir,"5 - Output",Assunit,Assregion,sep="/"))

  # define directory to load figure and table data products
  pathdir_prodFT <- paste(pathdir_nogit,"Producing figures and tables",Assunit,Assregion,sep="/")
  load(paste(pathdir_prodFT,"TableA2.RData",sep="/"))
  habitat <- as.character(A2table[1:4,1])
    
  # get regional data and connect to MSFD habitats with information within the c-square
  datmsfd <- Region@data

  # remove areas deeper than 200 meter for overview of Greater North Sea
  if (Assregion == "Greater North Sea"){
    datmsfd <-  subset(datmsfd,datmsfd$Depth >= -200)
  }
  
  colnames(datmsfd)[which(colnames(datmsfd)=="MSFD")] <- "MSFD_midpoint" 
  
  # account for area of MSDS habitat within csquares and make sure total area is < grid cell size 
  tnew <- aggregate(msfd_csq$area_km2, by=list(msfd_csq$csquares),FUN = sum)
  colnames(tnew) <- c("csquares","areanew")
  
  msfd_csq_new <- cbind(msfd_csq, tnew[match(msfd_csq$csquares,tnew$csquares), c("areanew")])
  colnames(msfd_csq_new)[ncol(msfd_csq_new)] <- "tot_area"
  datmsfd <- merge(datmsfd,msfd_csq_new,by = "csquares", all.x =T)
  datmsfd$MSFD <- as.character(datmsfd$MSFD)
  datmsfd$MSFD[datmsfd$MSFD=="Na"]= "Unknown"
  
  for (iHabitat in 1:4){
# 1) to close c-squares to fisheries, starting at the lowest effort c-squares, until 5, 10, 15, 20, 30% of effort has been removed
  Trade_RE1 <- subset(datmsfd,datmsfd$MSFD == habitat[iHabitat])
  nam <- c(SSAR_year)
  Trade_RE1 <- cbind(Trade_RE1, Fisheries[match(Trade_RE1$csquares,Fisheries$csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
  Trade_RE1$avgsar <- rowMeans(Trade_RE1[,nam])
  
  nam <- c(value_year)
  Trade_RE1 <- cbind(Trade_RE1, Fisheries[match(Trade_RE1$csquares,Fisheries$csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
  Trade_RE1$avgvalue <- rowMeans(Trade_RE1[,nam])
  
  nam <- c(weight_year)
  Trade_RE1 <- cbind(Trade_RE1, Fisheries[match(Trade_RE1$csquares,Fisheries$csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
  Trade_RE1$avgweight <- rowMeans(Trade_RE1[,nam])
  
  if (p %in% regions_with_impact){
    nam <- c(state_year)
    Trade_RE1 <- cbind(Trade_RE1, State_reg_IL[match(Trade_RE1$csquares,State_reg_IL$Fisheries.csquares), c(nam)])
    Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 1
    Trade_RE1$avgimpact_IL <- 1- rowMeans(Trade_RE1[,nam])
    indexcol <- which(names(Trade_RE1) %in% nam)
    Trade_RE1 <- Trade_RE1[,-c(indexcol)]
    
    nam <- c(state_year)
    Trade_RE1 <- cbind(Trade_RE1, State_reg[match(Trade_RE1$csquares,State_reg$Fisheries.csquares), c(nam)])
    Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 1
    Trade_RE1$avgimpact <- 1- rowMeans(Trade_RE1[,state_year])
  }
  
  RE1_dat <- as.data.frame(matrix(data=NA, nrow=10, ncol=5))
  # weighted mean PD impact
  if (p %in% regions_with_impact){
    RE1_dat[1,1] <- sum(Trade_RE1$avgimpact*Trade_RE1$area_km2)/sum(Trade_RE1$area_km2)  
  }
  # value
  RE1_dat[1,2] <- sum(Trade_RE1$avgvalue*Trade_RE1$area_km2)/sum(Trade_RE1$area_km2*Trade_RE1$avgvalue)*100
  # area within unfished grid cells
  area_unf <- ifelse(Trade_RE1$avgsar== 0, 1, 0)
  RE1_dat[1,3] <- sum(area_unf*Trade_RE1$area_km2)/sum(Trade_RE1$area_km2)*100
  # weight
  RE1_dat[1,4] <- sum(Trade_RE1$avgweight*Trade_RE1$area_km2)/sum(Trade_RE1$area_km2*Trade_RE1$avgweight)*100
  # weighted mean L1 impact
  if (p %in% regions_with_impact){
    RE1_dat[1,5] <- sum(Trade_RE1$avgimpact_IL*Trade_RE1$area_km2)/sum(Trade_RE1$area_km2)
  }
  # sort from low to high effort based on total area swept in each grid cell
  cut_effort        <- c(0.05,0.1,0.15,0.2,0.3,0.4,0.6,0.8,0.99)
  Trade_RE1$swept   <- Trade_RE1$avgsar * Trade_RE1$area_km2
  Trade_RE1         <- Trade_RE1[order(Trade_RE1[,"swept"],decreasing = F),]
  Trade_RE1$cumSSAR <- cumsum(Trade_RE1[,"swept"])
  Trade_RE1$cumSSAR <- Trade_RE1$cumSSAR / sum(Trade_RE1[,"swept"])
  
  if(!(is.na(Trade_RE1$cumSSAR[1]))){
  for (jj in 1:9){
    nb     <- min(which(Trade_RE1$cumSSAR > cut_effort[jj]))
    nb_end <- nrow(Trade_RE1)
    if (p %in% regions_with_impact){
      RE1_dat[jj+1,1] <- sum(Trade_RE1$avgimpact[nb:nb_end]*Trade_RE1$area_km2[nb:nb_end])/sum(Trade_RE1$area_km2)
      }
    RE1_dat[jj+1,2] <- sum(Trade_RE1$avgvalue[nb:nb_end]*Trade_RE1$area_km2[nb:nb_end])/sum(Trade_RE1$area_km2*Trade_RE1$avgvalue)*100
    RE1_dat[jj+1,3] <- sum(Trade_RE1$area_km2[1:nb])/sum(Trade_RE1$area_km2)*100
    RE1_dat[jj+1,4] <- sum(Trade_RE1$avgweight[nb:nb_end]*Trade_RE1$area_km2[nb:nb_end])/sum(Trade_RE1$area_km2*Trade_RE1$avgweight)*100
    if (p %in% regions_with_impact){
      RE1_dat[jj+1,5] <- sum(Trade_RE1$avgimpact_IL[nb:nb_end]*Trade_RE1$area_km2[nb:nb_end])/sum(Trade_RE1$area_km2)
  } }}

  colnames(RE1_dat) <- c("impact","value","UnF_area","weight","impact_IL") 

# now make the plot 
  if (p %in% regions_with_impact){
    png(paste(Assregion,"Hab",paste(iHabitat,"png",sep="."),sep="_"),
        width=8,height=3.5, units = "in", res = 300) 
    par(mfrow=c(1,3),mar=c(4,4,4,1)+.1)
    
    nam_points <- c("","-5","-10","","-20","","-40","","-80","-99")
    
    plot(RE1_dat$impact,RE1_dat$value, xlab="Average PD impact", ylab="Expected value relative to reference (%)",
         type="l",xlim=c(0,0.2),ylim=c(0,100),pch=16,las=1,yaxt="n",xaxt="n")
    points(RE1_dat$impact[c(2,3,5,7,9,10)],RE1_dat$value[c(2,3,5,7,9,10)],pch=16)
    text(RE1_dat$impact[c(2,3,5,7,9,10)]+0.012,RE1_dat$value[c(2,3,5,7,9,10)], 
         labels=nam_points[c(2,3,5,7,9,10)],cex=0.9, font=2)
    points(RE1_dat$impact[1],100,pch= 16,col="blue",cex=1.3)
    axis(1, c(0,0.1,0.2))
    axis(2, c(0,50, 100),las=1)
    
    plot(RE1_dat$impact_IL,RE1_dat$value, xlab="Average L1 impact", ylab="",type="l",
         xlim=c(0,1),ylim=c(0,100),pch=16,las=1, yaxt="n",xaxt="n")
    points(RE1_dat$impact_IL[c(2,3,5,7,9,10)],RE1_dat$value[c(2,3,5,7,9,10)],pch=16)
    text(RE1_dat$impact_IL[c(2,3,5,7,9,10)]+0.06,RE1_dat$value[c(2,3,5,7,9,10)], 
         labels=nam_points[c(2,3,5,7,9,10)],cex=0.9, font=2)
    points(RE1_dat$impact_IL[1],100,pch= 16,col="blue",cex=1.3)
    axis(1, c(0,0.5,1))
    axis(2, c(0,50, 100),c("","",""))
    
    plot(RE1_dat$UnF_area,RE1_dat$value, xlab="Percentage area unfished**", 
         ylab="",type="l",yaxt="n",
         xlim=c(0,100),ylim=c(0,100),pch=16,las=1,xaxt="n")
    points(RE1_dat$UnF_area[c(2,3,5,7,9,10)],RE1_dat$value[c(2,3,5,7,9,10)],pch=16)
    text(RE1_dat$UnF_area[c(2,3,5,7,9,10)]-6,RE1_dat$value[c(2,3,5,7,9,10)], 
         labels=nam_points[c(2,3,5,7,9,10)],cex=0.9, font=2)
    points(RE1_dat$UnF_area[1],100,pch= 16,col="blue",cex=1.3)  
    axis(1,c(0,50,100))
    axis(2, c(0,50, 100),c("","",""))
    
    # create the title, add percentage of area relative to total region
    tn <- round(A2table[iHabitat,2]/sum(A2table[,2])*100,digits = 0)
    mtext(paste(paste(habitat[iHabitat]),paste("(",tn,"% of total area)",sep = ""),sep = " "), side = 3, line = -3, outer = TRUE)
    
    dev.off()
    
  } else {
    png(paste(Assregion,"Hab",paste(iHabitat,"png",sep="."),sep="_"),
        width=8,height=3.5, units = "in", res = 300) 
   
    par(mfrow=c(1,3),mar=c(4,4,4,1)+.1)
    plot.new();text(0.5,0.5,"NA")
    plot.new();text(0.5,0.5,"NA")
    
    nam_points <- c("","-5","-10","","-20","","-40","","-80","-99")
    
    plot(RE1_dat$UnF_area,RE1_dat$value, xlab="Percentage area unfished", ylab="Expected value relative to reference (%)",
         type="l",xlim=c(0,100),ylim=c(0,100),pch=16,las=1,yaxt="n",xaxt="n")
    points(RE1_dat$UnF_area[c(2,3,5,7,9,10)],RE1_dat$value[c(2,3,5,7,9,10)],pch=16)
    text(RE1_dat$UnF_area[c(2,3,5,7,9,10)]-6,RE1_dat$value[c(2,3,5,7,9,10)], 
         labels=nam_points[c(2,3,5,7,9,10)],cex=0.9, font=2)
    points(RE1_dat$UnF_area[1],100,pch= 16,col="blue",cex=1.3)
    axis(1, c(0,50,100))
    axis(2, c(0,50, 100),las=1)
    
    # create the title, add percentage of area relative to total region
    tn <- round(A2table[iHabitat,2]/sum(A2table[,2])*100,digits = 0)
    mtext(paste(paste(habitat[iHabitat]),paste("(",tn,"% of total area)",sep = ""),sep = " "), side = 3, line = -3, outer = TRUE)
    
    dev.off()
  }
  

  RE1_dat <- round(as.matrix(RE1_dat), digits = 2)  
  RE1_dat <- data.frame(Effort = c(0,cut_effort)*100, RE1_dat[,c(1,5,3,2,4)])
  colnames(RE1_dat) <- c("Effort reduction (%)","PD impact","L1 impact","Unfished area (%)",
                         "Decline in value (%)","Decline in weight (%)")
  write.csv(RE1_dat, file= paste(Assregion,"Hab",paste(iHabitat,"csv",sep="."),sep="_"), row.names=FALSE)
  }
  
  rm(list= ls()[!(ls() %in% c('pathdir','pathdir_nogit','Assregion_index','Assunit_index','EcoReg_index',
                              'Period','AssPeriod',"EcoReg",'Fisheries','FisheriesMet','p','regions_with_impact',
                              'Region','State_reg','State_reg_IL',"Assunit","Assregion","msfd_csq"))])
  