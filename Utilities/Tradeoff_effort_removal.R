
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
  
# removal of effort
# 1) to close c-squares to fisheries, starting at the lowest effort c-squares, until 5, 10, 15, 20, 30% of effort has been removed
  Trade_RE1 <- subset(Region@data,Region@data$Depth >= -200 & Region@data$Depth < 0)
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

# 4) to close c-squares to fisheries, starting at the highest effort c-squares, until 5, 10, 15, 20, 30% etc. of effort has been removed
  RE4_dat <- as.data.frame(matrix(data=NA, nrow=11, ncol=4))
  RE4_dat[1,1] <- mean(Trade_RE1$avgimpact)
  RE4_dat[1,2] <- 100
  RE4_dat[1,3] <- length(Trade_RE1$avgsar[Trade_RE1$avgsar== 0])/nrow(Trade_RE1)*100
  RE4_dat[1,4] <- 100
  RE4_dat[1,5] <- mean(Trade_RE1$avgimpact_IL)
  
  cut_effort <- c(0.05,0.1,0.15,0.2,0.3,0.4,0.6,0.8,0.99)
  Trade_RE1 <- Trade_RE1[order(Trade_RE1[,"avgsar"],decreasing = T),]
  Trade_RE1$cumSSAR <- cumsum(Trade_RE1[,"avgsar"])
  Trade_RE1$cumSSAR <- Trade_RE1$cumSSAR / sum(Trade_RE1[,"avgsar"])
  
  for (jj in 1:9){
    nb <- min(which(Trade_RE1$cumSSAR > cut_effort[jj]))
    nb2 <- min(which(Trade_RE1$avgsar == 0))
    RE4_dat[jj+1,1] <- sum(Trade_RE1$avgimpact[nb:nb2])/nrow(Trade_RE1)
    RE4_dat[jj+1,2] <- (sum(Trade_RE1$avgvalue[nb:nb2])/sum(Trade_RE1$avgvalue))*100
    RE4_dat[jj+1,3] <- (nb + (nrow(Trade_RE1)-nb2)) / nrow(Trade_RE1)*100
    RE4_dat[jj+1,4] <- sum(Trade_RE1$avgweight[nb:nrow(Trade_RE1)])/sum(Trade_RE1$avgweight)*100
    RE4_dat[jj+1,5] <- sum(Trade_RE1$avgimpact_IL[nb:nb2])/nrow(Trade_RE1)    
  } 
  
  colnames(RE1_dat) <- c("impact","value","UnF_cells","weight","impact_IL") 
  colnames(RE4_dat) <- c("impact","value","UnF_cells","weight","impact_IL") 
  
  setwd(paste(pathdir,"5 - Output",Assunit,Assregion,sep="/"))
  pdf(paste(Assunit,Assregion,"MO_effort_red.pdf",sep="_"),width=7,height=5.5) 
  par(mfrow=c(2,3))
  par(oma = c(4, 4, 0, 0)) # make room (i.e. the 4's) for the overall x and y axis titles
  par(mar = c(2, 2, 1, 1)) # make the plots be closer together

  nam_points <- c("","-5","-10","-15","-20","-30","-40","-60","-80","-99")
  
  plot(RE1_dat$impact,RE1_dat$value, xlab="Average PD impact", ylab="Expected value relative to reference (%)",type="o",
       xlim=c(0,0.12),ylim=c(0,100),pch=16,las=1, xaxt="n")
  text(RE1_dat$impact,RE1_dat$value+5, labels=nam_points,cex=0.9, font=2)
  lines(RE4_dat$impact,RE4_dat$value,type="o",col="red",pch=16)
  text(RE4_dat$impact,RE4_dat$value-3, labels=nam_points,cex=0.9, font=2,col="red")
  points(mean(Trade_RE1$avgimpact),100,pch= 16,col="blue",cex=1.3)
  
  legend(0.02, 13, legend=c("low to high effort", "high to low effort"),
         col=c("black", "red"), ,bty = "n",lwd=2,y.intersp=0.8)
  
  plot(RE1_dat$impact_IL,RE1_dat$value, xlab="Average L1 impact", ylab="",type="o",
       xlim=c(0,0.7),ylim=c(0,100),pch=16,las=1, xaxt="n",yaxt="n")
  text(RE1_dat$impact_IL,RE1_dat$value+5, labels=nam_points,cex=0.9, font=2)
  lines(RE4_dat$impact_IL,RE4_dat$value,type="o",col="red",pch=16)
  text(RE4_dat$impact_IL,RE4_dat$value-3, labels=nam_points,cex=0.9, font=2,col="red")
  points(mean(Trade_RE1$avgimpact_IL),100,pch= 16,col="blue",cex=1.3)
  
  plot(RE1_dat$UnF_cells,RE1_dat$value, xlab="Unfished grid cells", ylab="",type="o",yaxt="n", xaxt="n",
       xlim=c(0,100),ylim=c(0,100),pch=16,las=1)
  text(RE1_dat$UnF_cells,RE1_dat$value+5, labels=nam_points,cex=0.9, font=2)
  lines(RE4_dat$UnF_cells,RE4_dat$value,type="o",col="red",pch=16)
  text(RE4_dat$UnF_cells,RE4_dat$value-3, labels=nam_points,cex=0.9, font=2,col="red")
  points(RE1_dat$UnF_cells[1],RE1_dat$value[1],pch= 16,col="blue",cex=1.3)  
  
  plot(RE1_dat$impact,RE1_dat$weight, xlab="Average PD impact", ylab="Expected weight relative to reference (%)",type="o",
       xlim=c(0,0.12),ylim=c(0,100),pch=16,las=1)
  text(RE1_dat$impact,RE1_dat$weight+5, labels=nam_points,cex=0.9, font=2)
  lines(RE4_dat$impact,RE4_dat$weight,type="o",col="red",pch=16)
  text(RE4_dat$impact,RE4_dat$weight-3, labels=nam_points,cex=0.9, font=2,col="red")
  points(mean(Trade_RE1$avgimpact),100,pch= 16,col="blue",cex=1.3)
  
  plot(RE1_dat$impact_IL,RE1_dat$weight, xlab="Average L1 impact", ylab="",type="o",
       xlim=c(0,0.7),ylim=c(0,100),pch=16,las=1,yaxt="n")
  text(RE1_dat$impact_IL,RE1_dat$weight+5, labels=nam_points,cex=0.9, font=2)
  lines(RE4_dat$impact_IL,RE4_dat$weight,type="o",col="red",pch=16)
  text(RE4_dat$impact_IL,RE4_dat$weight-3, labels=nam_points,cex=0.9, font=2,col="red")
  points(mean(Trade_RE1$avgimpact_IL),100,pch= 16,col="blue",cex=1.3)
  
  plot(RE1_dat$UnF_cells,RE1_dat$weight, xlab="Unfished grid cells", ylab="",type="o",yaxt="n",
       xlim=c(0,100),ylim=c(0,100),pch=16,las=1)
  text(RE1_dat$UnF_cells,RE1_dat$weight+5, labels=nam_points,cex=0.9, font=2)
  lines(RE4_dat$UnF_cells,RE4_dat$weight,type="o",col="red",pch=16)
  text(RE4_dat$UnF_cells,RE4_dat$weight-3, labels=nam_points,cex=0.9, font=2,col="red")
  points(RE1_dat$UnF_cells[1],RE1_dat$weight[1],pch= 16,col="blue",cex=1.3)  

  mtext('average PD impact',at=.17,side=1,outer=T,cex=0.8, line=0.2)
  mtext('average L1 impact',at=.50,side=1,outer=T,cex=0.8, line=0.2)
  mtext('% unfished c-squares',at=.84,side=1,outer=T,cex=0.8, line=0.2) 
  mtext('Value (%) relative to ref.',at=.75,side=2,outer=T,cex=0.8, line=0.2) 
  mtext('Weight (%) relative to ref.',at=.25,side=2,outer=T,cex=0.8, line=0.2)    
  
  dev.off()
  
  rm(list= ls()[!(ls() %in% c('pathdir','pathdir_nogit','Assregion_index','Assunit_index','Period','AssPeriod',"EcoReg",
                              'Fisheries','FisheriesMet','Region','State_reg','State_reg_IL',"EcoReg_index","Assunit","Assregion"))])
  
