
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

tab_habitat <- as.data.frame(matrix(data = NA,nrow=5,ncol=10))

# first estimate the tota value of landings per metier
  Trade_RE1 <- subset(Region@data,Region@data$Depth >= -200 & Region@data$Depth < 0)
  nam <- c(SSAR_year)
  Trade_RE1 <- cbind(Trade_RE1, Fisheries[match(Trade_RE1$csquares,Fisheries$csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
  Trade_RE1$avgsar <- rowMeans(Trade_RE1[,SSAR_year])
  
  # connect with fisheries per metier 
  Trade_RE1 <- cbind(Trade_RE1, FisheriesMet[match(Trade_RE1$csquares,FisheriesMet$csquares), c(2:501)])
  
  # estimate total value of landings per metier for all habitats
  gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  years <- c(2013:2018)
  sum_values <- c()  
  
  for (pp in 1:length(gears)){
    nam <- paste(paste(gears[pp],"total_value",sep="_"),years,sep="_")
    Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
    sum_values <- cbind(sum_values,rowMeans(Trade_RE1[,nam])) 
  }
  sum_values <- colSums(sum_values)/1000000

for (iHabitat in 1:5){
  Trade_RE1 <- subset(Region@data,Region@data$Depth >= -200 & Region@data$Depth < 0)
  Trade_RE1 <- subset(Trade_RE1,Trade_RE1$MSFD == habitat[iHabitat])
  nam <- c(SSAR_year)
  Trade_RE1 <- cbind(Trade_RE1, Fisheries[match(Trade_RE1$csquares,Fisheries$csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
  Trade_RE1$avgsar <- rowMeans(Trade_RE1[,SSAR_year])
  
  Trade_RE1 <- cbind(Trade_RE1, FisheriesMet[match(Trade_RE1$csquares,FisheriesMet$csquares), c(2:501)])
  
  # estimate the cut-off value
  Trade_RE1 <- Trade_RE1[order(Trade_RE1[,"avgsar"],decreasing = F),]
  Trade_RE1$cumSSAR <- cumsum(Trade_RE1[,"avgsar"])
  Trade_RE1$cumSSAR <- Trade_RE1$cumSSAR / sum(Trade_RE1[,"avgsar"])
  nb <- min(which(Trade_RE1$cumSSAR > 0.1))
  
  # now estimate the loss of value
  sum_values_loss <- c()  
  for (pp in 1:length(gears)){
    nam <- paste(paste(gears[pp],"total_value",sep="_"),years,sep="_")
    Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
    sum_values_loss <- cbind(sum_values_loss,rowMeans(Trade_RE1[1:nb,nam])) 
  }
  sum_values_loss <- colSums(sum_values_loss)/1000000
  tab_habitat[iHabitat,] <- sum_values_loss/sum_values*100
  colnames(tab_habitat) <- gears
  rownames(tab_habitat) <- habitat
}
 
setwd(paste(pathdir,"5 - Output",Assunit,Assregion,sep="/"))
tab_habitat <- round(tab_habitat, digits = 2)
tab_habitat <- t(tab_habitat)
write.csv(tab_habitat, file= paste(Assunit,Assregion,"Table_MO_habitat.csv",sep="_"), row.names=T)
 