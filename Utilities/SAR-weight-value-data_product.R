
# get csv and shapefile data product

# get the region data
  setwd("C:/Users/pdvd/Online for git/WKTRADE3/1 - Input env")
  
  load("Bay of Biscay and the Iberian Coast_region_grid_sensitivity.RData")  
  idx <- which(names(Region@data)== "long")
  colnames(Region@data)[idx]  <- "longitude"
  idx <- which(names(Region@data)== "lat")
  colnames(Region@data)[idx]  <- "latitude"
  BoB <- Region[,c("csquares","longitude", "latitude")]

  load("Celtic Seas_region_grid_sensitivity.RData")  
  idx <- which(names(Region@data)== "long")
  colnames(Region@data)[idx]  <- "longitude"
  idx <- which(names(Region@data)== "lat")
  colnames(Region@data)[idx]  <- "latitude"
  CS <- Region[,c("csquares","longitude", "latitude")]

  load("Greater North Sea_region_grid_sensitivity.RData")  
  idx <- which(names(Region@data)== "long")
  colnames(Region@data)[idx]  <- "longitude"
  idx <- which(names(Region@data)== "lat")
  colnames(Region@data)[idx]  <- "latitude"
  NS <- Region[,c("csquares","longitude", "latitude")]

  load("Baltic Sea_region_grid_sensitivity.RData")  
  idx <- which(names(Region@data)== "long")
  colnames(Region@data)[idx]  <- "longitude"
  idx <- which(names(Region@data)== "lat")
  colnames(Region@data)[idx]  <- "latitude"
  BS <- Region[,c("csquares","longitude", "latitude")]
  Region <- rbind(BoB,CS,NS,BS)
  
# get the fisheries 
  setwd("C:/Users/pdvd/Online for git/WKTRADE3 - Fisheries restricted") 
  load("Baltic Sea_fisheries.RData")
  BS_F <- Fisheries   
  
  load("Bay of Biscay and the Iberian Coast_fisheries.RData")
  BoB_F <- Fisheries   
  
  load("Greater North Sea_fisheries.RData")
  NS_F <- Fisheries   
  
  load("Celtic Seas_fisheries.RData")
  CS_F <- Fisheries
  
  Fisheries <- rbind(BS_F,BoB_F,NS_F,CS_F)

# now add Fisheries to Region
  SSAR_year <- paste("surface_sar",AssPeriod,sep="_")
  weight_year <- paste("total_weight",AssPeriod,sep="_")
  value_year <- paste("total_value",AssPeriod,sep="_")
  
  Fisheries[,c(SSAR_year)][is.na(Fisheries[,c(SSAR_year)])] <- 0
  Fisheries$avg <- rowMeans(Fisheries[,SSAR_year])
  quat<-c(-1,0,0.1,0.5,1,5,10,100)
  Fisheries$cat <- as.factor(cut(Fisheries$avg,quat,right=T))
  Fisheries$cat <- as.character(Fisheries$cat)
  Fisheries$cat[Fisheries$cat == "(-1,0]"] <- "NA" 
  Fisheries$cat[Fisheries$cat == "(10,100]"] <- ">10" 
  Fisheries$cat <- as.factor(Fisheries$cat)
  
  Region <- Region@data
  Region <- cbind(Region, Fisheries[match(Region$csquares,Fisheries$csquares), c("cat")])
  colnames(Region)[ncol(Region)] <- "MBCG_surface_sar"
  Region$MBCG_surface_sar[is.na(Region$MBCG_surface_sar)] <- "NA"
  
  Fisheries[,c(value_year)][is.na(Fisheries[,c(value_year)])] <- 0
  Fisheries$avg <- rowMeans(Fisheries[,value_year])
  quat<-c(-1,0,100,1000,10000,100000,10000000)
  Fisheries$cat <- as.factor(cut(Fisheries$avg,quat,right=T))
  Fisheries$cat <- as.character(Fisheries$cat)
  Fisheries$cat[Fisheries$cat == "(-1,0]"] <- "NA" 
  Fisheries$cat[Fisheries$cat == "(1e+05,1e+07]"] <- ">1e+05" 
  Fisheries$cat <- as.factor(Fisheries$cat)
  
  Region <- cbind(Region, Fisheries[match(Region$csquares,Fisheries$csquares), c("cat")])
  colnames(Region)[ncol(Region)] <- "MBCG_total_value"
  Region$MBCG_total_value[is.na(Region$MBCG_total_value)] <- "NA"
  
  Fisheries[,c(weight_year)][is.na(Fisheries[,c(weight_year)])] <- 0
  Fisheries$avg <- rowMeans(Fisheries[,weight_year])
  quat<-c(-1,0,100,1000,10000,100000,10000000)
  Fisheries$cat <- as.factor(cut(Fisheries$avg,quat,right=T))
  Fisheries$cat <- as.character(Fisheries$cat)
  Fisheries$cat[Fisheries$cat == "(-1,0]"] <- "NA" 
  Fisheries$cat[Fisheries$cat == "(1e+05,1e+07]"] <- ">1e+05" 
  Fisheries$cat <- as.factor(Fisheries$cat)
  
  Region <- cbind(Region, Fisheries[match(Region$csquares,Fisheries$csquares), c("cat")])
  colnames(Region)[ncol(Region)] <- "MBCG_total_weight"
  Region$MBCG_total_weight[is.na(Region$MBCG_total_weight)] <- "NA"
  
# get the fisheries combined 
  setwd("C:/Users/pdvd/Online for git/WKTRADE3 - Fisheries restricted") 
  load("Baltic Sea_fisheries_per_metier_comb.RData")
  BS_F <- FisheriesMet   
  
  load("Bay of Biscay and the Iberian Coast_fisheries_per_metier_comb.RData")
  BoB_F <- FisheriesMet   
  
  load("Greater North Sea_fisheries_per_metier_comb.RData")
  NS_F <- FisheriesMet   
  
  load("Celtic Seas_fisheries_per_metier_comb.RData")
  CS_F <- FisheriesMet
  
  FisheriesMet <- rbind(BS_F,BoB_F,NS_F,CS_F)

# combine the fisheries metiers with the region
  gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  
  for (igear in 1:length(gears)){
    nam <- paste(rep(gears[igear],length(AssPeriod)),"surface_sar",AssPeriod,sep="_")
    FisheriesMet[,c(nam)][is.na(FisheriesMet[,c(nam)])] <- 0
    FisheriesMet$avg <- rowMeans(FisheriesMet[,nam])
    
    quat<-c(-1,0,0.1,0.5,1,5,10,100)
    FisheriesMet$cat <- as.factor(cut(FisheriesMet$avg,quat,right=T))
    FisheriesMet$cat <- as.character(FisheriesMet$cat)
    FisheriesMet$cat[FisheriesMet$cat == "(-1,0]"] <- "NA" 
    FisheriesMet$cat[FisheriesMet$cat == "(10,100]"] <- ">10" 
    FisheriesMet$cat <- as.factor(FisheriesMet$cat)
    
    Region  <- cbind(Region, FisheriesMet[match(Region$csquares,FisheriesMet$csquares), c("cat")])
    id_name <- paste(gears[igear],"surface_sar",sep = "_")
    colnames(Region)[ncol(Region)] <- id_name
    Region[,id_name][is.na(Region[,id_name])] <- "NA"
    
    # value
    nam <- paste(rep(gears[igear],length(AssPeriod)),"total_value",AssPeriod,sep="_")
    FisheriesMet[,c(nam)][is.na(FisheriesMet[,c(nam)])] <- 0
    FisheriesMet$avg <- rowMeans(FisheriesMet[,nam])
    
    quat<-c(-1,0,100,1000,10000,100000,10000000)
    FisheriesMet$cat <- as.factor(cut(FisheriesMet$avg,quat,right=T))
    FisheriesMet$cat <- as.character(FisheriesMet$cat)
    FisheriesMet$cat[FisheriesMet$cat == "(-1,0]"] <- "NA" 
    Fisheries$cat[Fisheries$cat == "(1e+05,1e+07]"] <- ">1e+05" 
    FisheriesMet$cat <- as.factor(FisheriesMet$cat)
    
    Region <- cbind(Region, FisheriesMet[match(Region$csquares,FisheriesMet$csquares), c("cat")])
    id_name <- paste(gears[igear],"total_value",sep = "_")
    colnames(Region)[ncol(Region)] <- id_name
    Region[,id_name][is.na(Region[,id_name])] <- "NA"
    
    # weight
    nam <- paste(rep(gears[igear],length(AssPeriod)),"total_weight",AssPeriod,sep="_")
    FisheriesMet[,c(nam)][is.na(FisheriesMet[,c(nam)])] <- 0
    FisheriesMet$avg <- rowMeans(FisheriesMet[,nam])
    
    quat<-c(-1,0,100,1000,10000,100000,10000000)
    FisheriesMet$cat <- as.factor(cut(FisheriesMet$avg,quat,right=T))
    FisheriesMet$cat <- as.character(FisheriesMet$cat)
    FisheriesMet$cat[FisheriesMet$cat == "(-1,0]"] <- "NA" 
    Fisheries$cat[Fisheries$cat == "(1e+05,1e+07]"] <- ">1e+05" 
    FisheriesMet$cat <- as.factor(FisheriesMet$cat)
    
    Region <- cbind(Region, FisheriesMet[match(Region$csquares,FisheriesMet$csquares), c("cat")])
    id_name <- paste(gears[igear],"total_weight",sep = "_")
    colnames(Region)[ncol(Region)] <- id_name
    Region[,id_name][is.na(Region[,id_name])] <- "NA"
  }
  
  data_out <- subset(Region, !(Region$MBCG_surface_sar == "NA" & 
                                 Region$MBCG_total_value== "NA" &
                                 Region$MBCG_total_weight == "NA"))
  
  setwd(pathdir_nogit)
  write.csv(data_out,file="TRADE3 data product.csv",row.names=F)

# get polygon shapefiles
  setwd("C:/Users/pdvd/Online for git/WKTRADE3/1 - Input env")
  
  load("Bay of Biscay and the Iberian Coast_region_grid_sensitivity.RData")  
  BoB <- Region[,c("csquares")]
  
  load("Celtic Seas_region_grid_sensitivity.RData")  
  CS <- Region[,c("csquares")]
  
  load("Greater North Sea_region_grid_sensitivity.RData")  
  NS <- Region[,c("csquares")]
  
  load("Baltic Sea_region_grid_sensitivity.RData")  
  BS <- Region[,"csquares"]
  
  Tot <- rbind(BoB,CS,NS,BS)
  
  setwd("C:/Users/pdvd/Online for git/WKTRADE3 - Fisheries restricted")
  alldat <- read.csv("TRADE3 data product.csv",sep=",",header=T)
  
  newobj <- merge(Tot, alldat, by.x="csquares", by.y="csquares")
  newobj <- subset(newobj,!(is.na(newobj$longitude)))
  
  ICESTrade <- newobj
  
  names(ICESTrade) <- c("csquares","long","lat",
                        "MBCG_SAR","MBCG_EUR","MBCG_KG",
                        "DRBMOL_SAR","DRBMOL_EUR","DRBMOL_KG",
                        "OTCRU_SAR","OTCRU_EUR","OTCRU_KG",
                        "OTDMF_SAR","OTDMF_EUR","OTDMF_KG",
                        "OTMIX_SAR","OTMIX_EUR","OTMIX_KG",
                        "OTSPF_SAR","OTSPF_EUR","OTSPF_KG",
                        "SDNDMF_SAR","SDNDMF_EUR","SDNDMF_KG",
                        "SSCDMF_SAR","SSCDMF_EUR","SSCDMF_KG",
                        "TBBCRU_SAR","TBBCRU_EUR","TBBCRU_KG",
                        "TBBDMF_SAR","TBBDMF_EUR","TBBDMF_KG",
                        "TBBMOL_SAR","TBBMOL_EUR","TBBMOL_KG")

  pathd <- "C:/Users/pdvd/Online for git/WKTRADE3 - Fisheries restricted"
  writeOGR(obj=ICESTrade, dsn=pathd, layer="ICESTrade_shapefiles", driver="ESRI Shapefile",overwrite_layer=TRUE) 
  
  