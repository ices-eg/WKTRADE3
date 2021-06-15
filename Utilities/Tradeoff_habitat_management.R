
###### Tradeoff - Effort Removal

###### figures and tables for ICES WKTRADE3
  SSAR_year <- paste("surface_sar",AssPeriod,sep="_")
  state_year <- paste("state",AssPeriod,sep="_")
  weight_year <- paste("total_weight",AssPeriod,sep="_")
  value_year <- paste("total_value",AssPeriod,sep="_")

  Region <- Region[!(is.na(Region$medlong)),]
  idx <- which(names(Region@data)== "long")
  colnames(Region@data)[idx]  <- "longitude"
  idx <- which(names(Region@data)== "lat")
  colnames(Region@data)[idx]  <- "latitude"
  
  setwd(paste(pathdir,"5 - Output",Assunit,Assregion,sep="/"))
  habitat <- read.csv(file = "Ecoregion_Greater North Sea_Table_2.csv") 
  habitat <- as.character(habitat[1:5,1])
    
  setwd(paste(pathdir,"5 - Output",Assunit,Assregion,sep="/"))
  pdf(paste(Assunit,Assregion,"MO_habitat_management.pdf",sep="_"),width=6.5,height=7) 
  par(mfrow=c(5,3))
  par(oma = c(4, 4, 2, 0)) # make room (i.e. the 4's) for the overall x and y axis titles
  par(mar = c(1.5, 1.5, 1, 1)) # make the plots be closer together
  
  for (iHabitat in 1:4){
# removal of effort per habitat
# 1) to close c-squares to fisheries, starting at the lowest effort c-squares, until 5, 10, 15, 20, 30% of effort has been removed
  Trade_RE1 <- subset(Region@data,Region@data$Depth >= -200 & Region@data$Depth < 0)
  Trade_RE1 <- subset(Trade_RE1,Trade_RE1$MSFD == habitat[iHabitat])
  nam <- c(SSAR_year)
  Trade_RE1 <- cbind(Trade_RE1, Fisheries[match(Trade_RE1$csquares,Fisheries$csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
  Trade_RE1$avgsar <- rowMeans(Trade_RE1[,SSAR_year])
  
  nam <- c(value_year)
  Trade_RE1 <- cbind(Trade_RE1, Fisheries[match(Trade_RE1$csquares,Fisheries$csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
  Trade_RE1$avgvalue <- rowMeans(Trade_RE1[,value_year])
  
  nam <- c(weight_year)
  Trade_RE1 <- cbind(Trade_RE1, Fisheries[match(Trade_RE1$csquares,Fisheries$csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
  Trade_RE1$avgweight <- rowMeans(Trade_RE1[,weight_year])
  
  nam <- c(state_year)
  Trade_RE1 <- cbind(Trade_RE1, State_reg_IL[match(Trade_RE1$csquares,State_reg_IL$Fisheries.csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 1
  Trade_RE1$avgimpact_IL <- 1- rowMeans(Trade_RE1[,state_year])
  indexcol <- which(names(Trade_RE1) %in% nam)
  Trade_RE1 <- Trade_RE1[,-c(indexcol)]
  
  nam <- c(state_year)
  Trade_RE1 <- cbind(Trade_RE1, State_reg[match(Trade_RE1$csquares,State_reg$Fisheries.csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 1
  Trade_RE1$avgimpact <- 1- rowMeans(Trade_RE1[,state_year])
  
  RE1_dat <- as.data.frame(matrix(data=NA, nrow=11, ncol=5))
  RE1_dat[1,1] <- mean(Trade_RE1$avgimpact)
  RE1_dat[1,2] <- 100
  RE1_dat[1,3] <- length(Trade_RE1$avgsar[Trade_RE1$avgsar== 0])/nrow(Trade_RE1)*100
  RE1_dat[1,4] <- 100
  RE1_dat[1,5] <- mean(Trade_RE1$avgimpact_IL)
  
  cut_effort <- c(0.05,0.1,0.15,0.2,0.3,0.4,0.6,0.8,0.99)
  Trade_RE1 <- Trade_RE1[order(Trade_RE1[,"avgsar"],decreasing = F),]
  Trade_RE1$cumSSAR <- cumsum(Trade_RE1[,"avgsar"])
  Trade_RE1$cumSSAR <- Trade_RE1$cumSSAR / sum(Trade_RE1[,"avgsar"])
  
  for (jj in 1:9){
    nb <- min(which(Trade_RE1$cumSSAR > cut_effort[jj]))
    RE1_dat[jj+1,1] <- sum(Trade_RE1$avgimpact[nb:nrow(Trade_RE1)])/nrow(Trade_RE1)
    RE1_dat[jj+1,2] <- sum(Trade_RE1$avgvalue[nb:nrow(Trade_RE1)])/sum(Trade_RE1$avgvalue)*100
    RE1_dat[jj+1,3] <- nb/nrow(Trade_RE1)*100
    RE1_dat[jj+1,4] <- sum(Trade_RE1$avgweight[nb:nrow(Trade_RE1)])/sum(Trade_RE1$avgweight)*100
    RE1_dat[jj+1,5] <- sum(Trade_RE1$avgimpact_IL[nb:nrow(Trade_RE1)])/nrow(Trade_RE1)
  } 

  colnames(RE1_dat) <- c("impact","value","UnF_cells","weight","impact_IL") 

# now make the plot 
  nam_points <- c("","-5","-10","","-20","","-40","","-80","-99")
  
  plot(RE1_dat$impact,RE1_dat$value, xlab="Average PD impact", ylab="Expected value relative to reference (%)",
       type="l",xlim=c(0,0.2),ylim=c(0,100),pch=16,las=1,yaxt="n",xaxt="n")
  points(RE1_dat$impact[c(2,3,5,7,9,10)],RE1_dat$value[c(2,3,5,7,9,10)],pch=16)
  text(RE1_dat$impact[c(2,3,5,7,9,10)]+0.012,RE1_dat$value[c(2,3,5,7,9,10)], 
       labels=nam_points[c(2,3,5,7,9,10)],cex=0.9, font=2)
  points(mean(Trade_RE1$avgimpact),100,pch= 16,col="blue",cex=1.3)
  axis(1, c(0,0.1,0.2),c("","",""))
  axis(2, c(0,50, 100),las=1)
  
  plot(RE1_dat$impact_IL,RE1_dat$value, xlab="Average L1 impact", ylab="",type="l",
       xlim=c(0,1),ylim=c(0,100),pch=16,las=1, yaxt="n",xaxt="n")
  points(RE1_dat$impact_IL[c(2,3,5,7,9,10)],RE1_dat$value[c(2,3,5,7,9,10)],pch=16)
  text(RE1_dat$impact_IL[c(2,3,5,7,9,10)]+0.06,RE1_dat$value[c(2,3,5,7,9,10)], 
       labels=nam_points[c(2,3,5,7,9,10)],cex=0.9, font=2)
  points(mean(Trade_RE1$avgimpact_IL),100,pch= 16,col="blue",cex=1.3)
  axis(1, c(0,0.5,1),c("","",""))
  
  plot(RE1_dat$UnF_cells,RE1_dat$value, xlab="Unfished grid cells", ylab="",type="l",yaxt="n",
       xlim=c(0,100),ylim=c(0,100),pch=16,las=1,xaxt="n")
  points(RE1_dat$UnF_cells[c(2,3,5,7,9,10)],RE1_dat$value[c(2,3,5,7,9,10)],pch=16)
  text(RE1_dat$UnF_cells[c(2,3,5,7,9,10)]-6,RE1_dat$value[c(2,3,5,7,9,10)], 
       labels=nam_points[c(2,3,5,7,9,10)],cex=0.9, font=2)
  points(RE1_dat$UnF_cells[1],RE1_dat$value[1],pch= 16,col="blue",cex=1.3)  
  axis(1,c(0,50,100),c("","",""))
  
  }
  
  # make the last row with axis names
  Trade_RE1 <- subset(Region@data,Region@data$Depth >= -200 & Region@data$Depth < 0)
  Trade_RE1 <- subset(Trade_RE1,Trade_RE1$MSFD == habitat[5])
  nam <- c(SSAR_year)
  Trade_RE1 <- cbind(Trade_RE1, Fisheries[match(Trade_RE1$csquares,Fisheries$csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
  Trade_RE1$avgsar <- rowMeans(Trade_RE1[,SSAR_year])
  
  nam <- c(value_year)
  Trade_RE1 <- cbind(Trade_RE1, Fisheries[match(Trade_RE1$csquares,Fisheries$csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
  Trade_RE1$avgvalue <- rowMeans(Trade_RE1[,value_year])
  
  nam <- c(weight_year)
  Trade_RE1 <- cbind(Trade_RE1, Fisheries[match(Trade_RE1$csquares,Fisheries$csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
  Trade_RE1$avgweight <- rowMeans(Trade_RE1[,weight_year])
  
  nam <- c(state_year)
  Trade_RE1 <- cbind(Trade_RE1, State_reg_IL[match(Trade_RE1$csquares,State_reg_IL$Fisheries.csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 1
  Trade_RE1$avgimpact_IL <- 1- rowMeans(Trade_RE1[,state_year])
  indexcol <- which(names(Trade_RE1) %in% nam)
  Trade_RE1 <- Trade_RE1[,-c(indexcol)]
  
  nam <- c(state_year)
  Trade_RE1 <- cbind(Trade_RE1, State_reg[match(Trade_RE1$csquares,State_reg$Fisheries.csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 1
  Trade_RE1$avgimpact <- 1- rowMeans(Trade_RE1[,state_year])
  
  RE1_dat <- as.data.frame(matrix(data=NA, nrow=11, ncol=5))
  RE1_dat[1,1] <- mean(Trade_RE1$avgimpact)
  RE1_dat[1,2] <- 100
  RE1_dat[1,3] <- length(Trade_RE1$avgsar[Trade_RE1$avgsar== 0])/nrow(Trade_RE1)*100
  RE1_dat[1,4] <- 100
  RE1_dat[1,5] <- mean(Trade_RE1$avgimpact_IL)
  
  cut_effort <- c(0.05,0.1,0.15,0.2,0.3,0.4,0.6,0.8,0.99)
  Trade_RE1 <- Trade_RE1[order(Trade_RE1[,"avgsar"],decreasing = F),]
  Trade_RE1$cumSSAR <- cumsum(Trade_RE1[,"avgsar"])
  Trade_RE1$cumSSAR <- Trade_RE1$cumSSAR / sum(Trade_RE1[,"avgsar"])
  
  for (jj in 1:9){
    nb <- min(which(Trade_RE1$cumSSAR > cut_effort[jj]))
    RE1_dat[jj+1,1] <- sum(Trade_RE1$avgimpact[nb:nrow(Trade_RE1)])/nrow(Trade_RE1)
    RE1_dat[jj+1,2] <- sum(Trade_RE1$avgvalue[nb:nrow(Trade_RE1)])/sum(Trade_RE1$avgvalue)*100
    RE1_dat[jj+1,3] <- nb/nrow(Trade_RE1)*100
    RE1_dat[jj+1,4] <- sum(Trade_RE1$avgweight[nb:nrow(Trade_RE1)])/sum(Trade_RE1$avgweight)*100
    RE1_dat[jj+1,5] <- sum(Trade_RE1$avgimpact_IL[nb:nrow(Trade_RE1)])/nrow(Trade_RE1)
  } 
  
  colnames(RE1_dat) <- c("impact","value","UnF_cells","weight","impact_IL") 
  
  plot(RE1_dat$impact,RE1_dat$value, xlab="Average PD impact", ylab="Expected value relative to reference (%)",
       type="l",xlim=c(0,0.2),ylim=c(0,100),pch=16,las=1,yaxt="n",xaxt="n")
  points(RE1_dat$impact[c(2,3,5,7,9,10)],RE1_dat$value[c(2,3,5,7,9,10)],pch=16)
  text(RE1_dat$impact[c(2,3,5,7,9,10)]+0.012,RE1_dat$value[c(2,3,5,7,9,10)], 
       labels=nam_points[c(2,3,5,7,9,10)],cex=0.9, font=2)
  points(mean(Trade_RE1$avgimpact),100,pch= 16,col="blue",cex=1.3)
  axis(1, c(0,0.1,0.2))
  axis(2, c(0,50, 100),las=1)
  
  plot(RE1_dat$impact_IL,RE1_dat$value, xlab="Average L1 impact", ylab="",type="l",
       xlim=c(0,1),ylim=c(0,100),pch=16,las=1, yaxt="n",xaxt="n")
  points(RE1_dat$impact_IL[c(2,3,5,7,9,10)],RE1_dat$value[c(2,3,5,7,9,10)],pch=16)
  text(RE1_dat$impact_IL[c(2,3,5,7,9,10)]+0.06,RE1_dat$value[c(2,3,5,7,9,10)], 
       labels=nam_points[c(2,3,5,7,9,10)],cex=0.9, font=2)
  points(mean(Trade_RE1$avgimpact_IL),100,pch= 16,col="blue",cex=1.3)
  axis(1, c(0,0.5,1))
  
  plot(RE1_dat$UnF_cells,RE1_dat$value, xlab="Unfished grid cells", ylab="",type="l",yaxt="n",
       xlim=c(0,100),ylim=c(0,100),pch=16,las=1,xaxt="n")
  points(RE1_dat$UnF_cells[c(2,3,5,7,9,10)],RE1_dat$value[c(2,3,5,7,9,10)],pch=16)
  text(RE1_dat$UnF_cells[c(2,3,5,7,9,10)]-6,RE1_dat$value[c(2,3,5,7,9,10)], 
       labels=nam_points[c(2,3,5,7,9,10)],cex=0.9, font=2)
  points(RE1_dat$UnF_cells[1],RE1_dat$value[1],pch= 16,col="blue",cex=1.3)  
  axis(1,c(0,50,100))
  
  mtext('average PD impact',at=.17,side=1,outer=T,cex=0.8, line=0.5)
  mtext('average L1 impact',at=.50,side=1,outer=T,cex=0.8, line=0.5)
  mtext('% unfished c-squares',at=.84,side=1,outer=T,cex=0.8, line=0.5) 
  mtext('Value (%) relative to ref.',at=.5,side=2,outer=T,cex=0.8, line=0.7)
  mtext(habitat[1], outer=TRUE, line=-0.5,cex=0.8)
  mtext(habitat[2], outer=TRUE, line=-10,cex=0.8)
  mtext(habitat[3], outer=TRUE, line=-19.5,cex=0.8)
  mtext(habitat[4], outer=TRUE, line=-29,cex=0.8)
  mtext(habitat[5], outer=TRUE, line=-38.5,cex=0.8)
  
  dev.off()
  
  rm(list= ls()[!(ls() %in% c('pathdir','pathdir_nogit','Assregion_index','Assunit_index','Period','AssPeriod',"EcoReg",
                              'Fisheries','FisheriesMet','Region','State_reg','State_reg_IL',"EcoReg_index","Assunit","Assregion"))])
  
