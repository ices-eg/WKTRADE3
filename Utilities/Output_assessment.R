
###### Figures and tables for WKTRADE3

  # define directory to load figure and table data products
  pathdir_prodFT <- paste(pathdir_nogit,"Producing figures and tables",Assunit,Assregion,sep="/")

  # set directory for output
  setwd(paste(pathdir,"5 - Output",sep="/"))  
  dir.create(paste(Assunit))
  setwd(paste(pathdir,"5 - Output",Assunit,sep="/"))  
  dir.create(paste(Assregion))
  setwd(paste(pathdir,"5 - Output",Assunit,Assregion,sep="/"))

##### Figure A.1
  load(paste(pathdir_prodFT,"FigureA1.RData",sep="/"))

  sar    <- (map_plot(figA1,"surface_sar",AssPeriod,purples,Assregion))
  impact <- (map_plot(figA1,"impact",AssPeriod,bluegreen,Assregion))
  longevi <- (map_plot(figA1,"medlong",AssPeriod,sealand,Assregion))
  value  <- (map_plot(figA1,"total_value",AssPeriod,yellowred,Assregion))
  
  pdf(paste(Assunit,Assregion,"figureA1.pdf",sep="_"),width=12,height=9) 
  print(grid.arrange(sar,impact, longevi,value, nrow = 2))
  dev.off()
  
  jpeg(file = paste(Assunit,Assregion,"figureA1.jpeg",sep="_"), width=12, height=9,units ='in', res = 300)
  print(grid.arrange(sar,impact, longevi,value, nrow = 2))
  dev.off()
  
##### Figure A.2
  pdf(paste(Assunit,Assregion,"figureA2.pdf",sep="_"),width=6.5,height=5) 
  print(sar)
  dev.off()
  
  jpeg(file = paste(Assunit,Assregion,"figureA2.jpeg",sep="_"), width=6.5, height=5,units ='in', res = 300)
  print(sar)
  dev.off()

##### Figure A.3
  load(paste(pathdir_prodFT,"FigureA3.RData",sep="/"))

  pdf(paste(Assunit,Assregion,"figureA3.pdf",sep="_"),width=8,height=4) 
  par(mfrow=c(1,3))
  
  #left panel
  ma <- round(max(A3fig[[1]][,1:5]))
  left<-as.data.frame(A3fig[[1]])
  plot(left[,1]~left$Year,type="o",col="black",lwd=2, pch=16, yaxt="n",ylim=c(0,ma),
       ylab="Trawling intensity (y-1)",xlab="Year")
  lines(left[,2]~left$Year, col="red", type="o", lty=2)
  lines(left[,3]~left$Year, col="blue", type="o", lty=3)
  lines(left[,4]~left$Year, col="orange", type="o", lty=4)
  lines(left[,5]~left$Year, col="black", type="o", lty=5)
  axis(2,c(0,ma/2,ma),las=1)

  # middle panel
  middle<-as.data.frame(A3fig[[2]])
  plot(middle[,1]~middle$Year,type="o",col="black",lwd=2, pch=16, yaxt="n",ylim=c(0,1),
       ylab="Proportion fished",xlab="Year")
  lines(middle[,2]~middle$Year, col="red", type="o", lty=2)
  lines(middle[,3]~middle$Year, col="blue", type="o", lty=3)
  lines(middle[,4]~middle$Year, col="orange", type="o", lty=4)
  lines(middle[,5]~middle$Year, col="black", type="o", lty=5)
  axis(2,c(0,0.5,1),las=1)

  # right panel
  right<-as.data.frame(A3fig[[3]])
  plot(right[,1]~right$Year,type="o",col="black",lwd=2, pch=16, yaxt="n",ylim=c(0,1),
       ylab="Proportion fished with 90% effort",xlab="Year")
  lines(right[,2]~right$Year, col="red", type="o", lty=2)
  lines(right[,3]~right$Year, col="blue", type="o", lty=3)
  lines(right[,4]~right$Year, col="orange", type="o", lty=4)
  lines(right[,5]~right$Year, col="black", type="o", lty=5)
  axis(2,c(0,0.5,1),las=1)
  
  legend(Period[1],1,legend=colnames(right[1:5]),bty = "n",
         col=c("black", "red", "blue","orange","black"), lty=1:5, cex=0.8, x.intersp=2)

  dev.off()

# Figure A.4
  load(paste(pathdir_prodFT,"FigureA4.RData",sep="/"))
  
  pdf(paste(Assunit,Assregion,"figureA4.pdf",sep="_"),width=5.5,height=4.5) 
  plot(A4dat$sweptcumu~A4dat$indixcumu, xlab="Surface area \n(grid cells sorted from high to low trawling intensity)",
       ylab="Cumulative proportion",las=1,yaxt="n", lty=1, col="white", type="l")
  lines(x=c(-1,2),y=c(0.2,0.2),type="l",lty=4,lwd=0.2,col="light grey")
  lines(x=c(-1,2),y=c(0.4,0.4),type="l",lty=4,lwd=0.2,col="light grey")
  lines(x=c(-1,2),y=c(0.6,0.6),type="l",lty=4,lwd=0.2,col="light grey")
  lines(x=c(-1,2),y=c(0.8,0.8),type="l",lty=4,lwd=0.2,col="light grey")
  lines(x=c(0.2,0.2),y=c(-1,2),type="l",lty=4,lwd=0.2,col="light grey")
  lines(x=c(0.4,0.4),y=c(-1,2),type="l",lty=4,lwd=0.2,col="light grey")
  lines(x=c(0.6,0.6),y=c(-1,2),type="l",lty=4,lwd=0.2,col="light grey")
  lines(x=c(0.8,0.8),y=c(-1,2),type="l",lty=4,lwd=0.2,col="light grey")
  
  lines(A4dat$sweptcumu~A4dat$indixcumu, lty=1, col="black",type="l")
  lines(A4dat$landcumu~A4dat$indixcumu, lty=2, col="blue",type="l")
  lines(A4dat$valuecumu~A4dat$indixcumu, lty=3, col="red",type="l")
  
  legend(0.5,0.8,legend=c("Swept area", "Landings","Value"),col=c("black","blue", "red"),lty=1:3, cex=0.8, 
         x.intersp=2,box.lty=0, bg=NULL)
  
  axis(2,c(0,0.2,0.4,0.6,0.8,1),las=1)
  dev.off()
  
##### Figure A.5
  pdf(paste(Assunit,Assregion,"figureA5.pdf",sep="_"),width=6.5,height=5) 
  print(impact)
  dev.off()
  
  jpeg(file = paste(Assunit,Assregion,"figureA5.jpeg",sep="_"), width=6.5, height=5,units ='in', res = 300)
  print(impact)
  dev.off()
  
#Figure A.6
  load(paste(pathdir_prodFT,"FigureA6.RData",sep="/"))
  
  pdf(paste(Assunit,Assregion,"figureA6.pdf",sep="_"),width=7,height=5) 
  par(mfrow=c(1,2))
  
  #left panel
  left<-as.data.frame(A6fig[[1]])
  plot((1-left[,1])~left$Year,type="o",col="black",lwd=2, pch=16, yaxt="n",ylim=c(0,1),
       ylab="Impact (PD model)",xlab="Year")
  lines((1-left[,2])~left$Year, col="red", type="o", lty=2)
  lines((1-left[,3])~left$Year, col="blue", type="o", lty=3)
  lines((1-left[,4])~left$Year, col="orange", type="o", lty=4)
  lines((1-left[,5])~left$Year, col="black", type="o", lty=5)
  axis(2,c(0,0.5,1),las=1)
  
  legend(Period[1],1,legend=colnames(right[1:5]),bty = "n",
         col=c("black", "red", "blue","orange","black"), lty=1:5, cex=0.8, x.intersp=0.2,y.intersp = 0.8)
  
  # right panel
  right<-as.data.frame(A6fig[[2]])
  plot(right[,1]~right$Year,type="o",col="black",lwd=2, pch=16, yaxt="n",ylim=c(0,1),
       ylab="Proportion habitat with impact < 0.2",xlab="Year")
  lines(right[,2]~right$Year, col="red", type="o", lty=2)
  lines(right[,3]~right$Year, col="blue", type="o", lty=3)
  lines(right[,4]~right$Year, col="orange", type="o", lty=4)
  lines(right[,5]~right$Year, col="black", type="o", lty=5)
  axis(2,c(0,0.5,1),las=1)
  
  dev.off()

# Figure A.8
  load(paste(pathdir_prodFT,"FigureA8_A9.RData",sep="/"))
  
  gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  
  nam <- paste("state",rep(gears[1],length(AssPeriod)),AssPeriod,sep="_")
  Avgear <- data.frame(rowMeans(A8_A9fig[, nam]))
  
  A8_A9fig[,nam]

  for (p in 2:length(gears)){
    nam <- paste("state",rep(gears[p],length(AssPeriod)),AssPeriod,sep="_")
    Avgear <- cbind(Avgear,data.frame(rowMeans(A8_A9fig[, nam])))
  }
  colnames(Avgear) <- gears
  
  Avgear <- t(Avgear)
  colnames(Avgear) <- A8_A9fig[,1]
  Avgear <- 1-Avgear # get impact
  Avgear <- t(Avgear)

  pdf(paste(Assunit,Assregion,"figureA8.pdf",sep="_"),width=12,height=5) 
  barplot(Avgear,beside=T,yaxt="n",xaxt="n",ylab="Impact (1-B/K)",ylim=c(0,max(Avgear)+0.05),xlab="Metier")
  legend(30,0.16, as.character(A8_A9fig$MSFD),
         fill = gray.colors(4),bty = "n")
  axis(1,c(seq(3, 48, length = 10)),gears) 
  
  axis(2,c(0,round(max(Avgear)/2,digits = 2),round(max(Avgear),digits = 2)),las=1)
  box()
  dev.off()
  
# Figure A.9 impact for three gears with highest impact
  high_imp <- order(colSums(Avgear),decreasing = T)[1:3]
  high_imp <- gears[high_imp]
  nam1 <- paste("state",rep(high_imp[1],length(Period)),Period,sep="_")
  nam2 <- paste("state",rep(high_imp[2],length(Period)),Period,sep="_")
  nam3 <- paste("state",rep(high_imp[3],length(Period)),Period,sep="_")
  
  pdf(paste(Assunit,Assregion,"figureA9.pdf",sep="_"),width=6.5,height=6.5) 
  par(mfrow=c(2,2))
  
  ymax <- 1-min(A8_A9fig[,2:ncol(A8_A9fig)])
  
  plot(Period,1-A8_A9fig[1,nam1],type="o",ylim=c(0,ymax+0.03),xaxt="n",yaxt="n",xlab="year",
       ylab="Impact (1-B/K)",lwd=2, main=as.character(A8_A9fig[1,1]))
  lines(Period,1-A8_A9fig[1,nam2],lwd=2,col="red",type="o")
  lines(Period,1-A8_A9fig[1,nam3],lwd=2,col="blue",type="o")
  axis(1,seq(Period[1],Period[length(Period)],by=2))
  axis(2,c(0,round(ymax/2,digits = 2),round(ymax,digits = 2)),las=1)
  legend(x=Period[4],y=ymax,c(high_imp),col=c("black","red","blue"),
         lty=1,bty = "n",lwd=2,y.intersp=0.8)
  
  plot(Period,1-A8_A9fig[2,nam1],type="o",ylim=c(0,ymax+0.03),xaxt="n",yaxt="n",xlab="year",
       ylab="Impact (1-B/K)",lwd=2, main=as.character(A8_A9fig[2,1]))
  lines(Period,1-A8_A9fig[2,nam2],lwd=2,col="red",type="o")
  lines(Period,1-A8_A9fig[2,nam3],lwd=2,col="blue",type="o")
  axis(1,seq(Period[1],Period[length(Period)],by=2))
  axis(2,c(0,round(ymax/2,digits = 2),round(ymax,digits = 2)),las=1)
  
  plot(Period,1-A8_A9fig[3,nam1],type="o",ylim=c(0,ymax+0.03),xaxt="n",yaxt="n",xlab="year",
       ylab="Impact (1-B/K)",lwd=2, main=as.character(A8_A9fig[3,1]))
  lines(Period,1-A8_A9fig[3,nam2],lwd=2,col="red",type="o")
  lines(Period,1-A8_A9fig[3,nam3],lwd=2,col="blue",type="o")
  axis(1,seq(Period[1],Period[length(Period)],by=2))
  axis(2,c(0,round(ymax/2,digits = 2),round(ymax,digits = 2)),las=1)
  
  plot(Period,1-A8_A9fig[4,nam1],type="o",ylim=c(0,max(Avgear)+0.03),xaxt="n",yaxt="n",xlab="year",
       ylab="Impact (1-B/K)",lwd=2, main=as.character(A8_A9fig[4,1]))
  lines(Period,1-A8_A9fig[4,nam2],lwd=2,col="red",type="o")
  lines(Period,1-A8_A9fig[4,nam3],lwd=2,col="blue",type="o")
  axis(1,seq(Period[1],Period[length(Period)],by=2))
  axis(2,c(0,round(ymax/2,digits = 2),round(ymax,digits = 2)),las=1)
  
  dev.off()

##########
# Table A1
  load(paste(pathdir_prodFT,"TableA1.RData",sep="/"))
  col1 <- c("1. Intensity", "2. Proportion of cells fished", "3. Proportion of area fished", "4. Aggregation of fishing pressure",
          "5. Persistently unfished areas", "6. Average impact", "7. Proportion of area with impact < 0.2")
  A1table <- data.frame(Indicators = col1, values = A1table)
  write.csv(A1table, file= paste(Assunit,Assregion,"Table_1.csv",sep="_"), row.names=FALSE)

# Table A2
  load(paste(pathdir_prodFT,"TableA2.RData",sep="/"))
  colnames(A2table) <- c("MSFD habitat code","Extent of habitat (in 1000 km^2)", "Number of grid cells","Landings 10^3 tonnes",
                         "Value 10^6 euro","Swept area in 1000 km^2","Proportion of grid cells fished (I-2)",
                       "Proportion of area fished (I-3)","Fishing intensity (I-1)",
                       "Average impact (I-6)", "Proportion of habitat fished with 90% of effort (I-4)")
  
  A2table[,c(2:11)] <- round(A2table[,c(2:11)], digits = 2)
  
  write.csv(A2table, file= paste(Assunit,Assregion,"Table_2.csv",sep="_"), row.names=FALSE)
  
# Table A3
  load(paste(pathdir_prodFT,"TableA3.RData",sep="/"))
  A3table <- round(A3table, digits = 2)
  write.csv(A3table, file= paste(Assunit,Assregion,"Table_3.csv",sep="_"), row.names=T)
  
  rm(list= ls()[!(ls() %in% c('pathdir','pathdir_nogit','Assregion_index','Assunit_index','Period','AssPeriod',"EcoReg",
                              'Fisheries','FisheriesMet','Region','State_reg',"EcoReg_index","Assunit","Assregion"))])
  
  