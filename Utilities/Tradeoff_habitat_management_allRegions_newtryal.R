
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
  habitat <- as.character(A2table[,1])
    
  # get regional data and connect to MSFD habitats with information within the c-square
  datmsfd <- Region@data

  # remove areas deeper than 200 meter for overview of Greater North Sea
  if (Assregion == "Greater North Sea"){
    datmsfd <-  subset(datmsfd,datmsfd$Depth >= -200)
  }
  
  colnames(datmsfd)[which(colnames(datmsfd)=="MSFD")] <- "MSFD_midpoint" 
  
  # now estimate dominant habitat type per csquare
  dom_msfd <- msfd_csq %>% 
    group_by(csquares) %>%
    filter(area_km2 == max(area_km2)) %>%
    arrange(csquares,MSFD)
  dom_msfd <- as.data.frame(dom_msfd)
  
  # account for area of MSDS habitat within csquares and make sure total area is < grid cell size 
  datmsfd <-  cbind(datmsfd, dom_msfd[match(datmsfd$csquares,dom_msfd$csquares), c("MSFD")])
  colnames(datmsfd)[ncol(datmsfd)] <- "MSFD_Dom"
  datmsfd$MSFD_Dom <- as.character(datmsfd$MSFD_Dom) 
  datmsfd$MSFD_Dom[datmsfd$MSFD_Dom == "Na"] <- "Unknown"
  
  mateffort <- as.data.frame(matrix(data = NA, ncol = 10, nrow = length(habitat)))
  matvalue  <- as.data.frame(matrix(data = NA, ncol = 10, nrow = length(habitat)))
  matweight <- as.data.frame(matrix(data = NA, ncol = 10, nrow = length(habitat)))
    
  for (iHabitat in 1:length(habitat)){
  Trade_RE1 <- subset(datmsfd,datmsfd$MSFD_Dom == habitat[iHabitat])
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
  
  Trade_RE1$sweptarea <- Trade_RE1$avgsar * Trade_RE1$area_sqkm
  Trade_RE1 <- Trade_RE1[order(Trade_RE1[,"sweptarea"],decreasing = F),]
  Trade_RE1$cumarea <- cumsum(Trade_RE1[,"area_sqkm"])
  Trade_RE1$cumarea <- Trade_RE1$cumarea / sum(Trade_RE1[,"area_sqkm"])
  
  quat<- c(0.05,seq(0.1,.9,0.1))
  for (q in 1:length(quat)){
    idx <- min(which (Trade_RE1$cumarea > quat[q]))
    mateffort[iHabitat,q] <-  sum(Trade_RE1$sweptarea[1:idx])/sum(Trade_RE1$sweptarea)
    matweight[iHabitat,q] <- sum(Trade_RE1$avgweight[1:idx])/sum(Trade_RE1$avgweight)
    matvalue[iHabitat,q] <- sum(Trade_RE1$avgvalue[1:idx])/sum(Trade_RE1$avgvalue)
  }
  }
  
  mateffort[mateffort < 0.005 & mateffort >0] <- -100
  mateffort <- round(mateffort, digits = 3)*100
  mateffort <- mateffort %>%
    mutate_all(as.character)
  mateffort[mateffort == "-10000"] <- "<0.05"
  rownames(mateffort) <- habitat
  colnames(mateffort) <- quat

  matweight[matweight < 0.005 & matweight >0] <- -100
  matweight <- round(matweight, digits = 3)*100
  matweight <- matweight %>%
    mutate_all(as.character)
  matweight[matweight == "-10000"] <- "<0.05"
  rownames(matweight) <- habitat
  colnames(matweight) <- quat
  
  matvalue[matvalue < 0.005 & matvalue >0] <- -100
  matvalue <- round(matvalue, digits = 3)*100
  matvalue <- matvalue %>%
    mutate_all(as.character)
  matvalue[matvalue == "-10000"] <- "<0.05"
  rownames(matvalue) <- habitat
  colnames(matvalue) <- quat
  
  mateffort <- data.frame(mateffort, area_km2 = round(A2table[,2],digits =2))
  matvalue <- data.frame(matvalue, area_km2 = round(A2table[,2],digits =2))
  matweight <- data.frame(matweight, area_km2 = round(A2table[,2],digits =2))
  
  write.csv(mateffort,file="habitat_effort.csv")
  write.csv(matvalue,file="habitat_value.csv")
  write.csv(matweight,file="habitat_weight.csv")
  
  
