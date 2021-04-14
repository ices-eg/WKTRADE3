
library(sf)
library(tidyr)
library(mapview)

pathdir_output <- paste(pathdir,"5 - Output",Assunit,Assregion,sep="/")

# select division from the region
  if (Assregion != EcoReg){
    Region <-  subset(Region,Region@data$division == Assregion)
  }

# subset all areas with longevity (north sea has no longevity prediction for Norwegian trench)
  if (p %in% regions_with_impact){
    Region <- Region[!(is.na(Region$medlong)),]
  }

  reguni <- ifelse(Assunit == "Division","division","Ecoregion")

#Reformat data, define metiers
  vmsValue <- select(FisheriesMet,csquares, contains("_value_"))
  vmsValue <- cbind(vmsValue, Region@data[match(vmsValue$csquares,Region@data$csquares), c(reguni)])
  colnames(vmsValue)[ncol(vmsValue)] <- reguni
  vmsValue <-  subset(vmsValue,vmsValue[,reguni] == Assregion)
  vmsValue <- vmsValue[,1:(ncol(vmsValue)-1)]
  vmsValueLong <- gather(vmsValue, column, euro, 2:ncol(vmsValue), factor_key=TRUE)
  vmsValueLong <- vmsValueLong[!is.na(vmsValueLong$euro),]
  vmsValueLong$year <- sapply(strsplit(as.character(vmsValueLong$column), "_"), tail, 1)
  vmsValueLong <- vmsValueLong[vmsValueLong$year %in% c(2013:2018),]
  vmsValueLong$metier <- substr(vmsValueLong$column, 1, nchar(as.character(vmsValueLong$column))-17)
  vmsValueLong$wktradeMet <- vmsValueLong$metier
  
  # remove all metiers with few data
  tt <- aggregate(vmsValueLong$csquares,by=list(vmsValueLong$wktradeMet),FUN=length)
  tt <- tt[which(tt[,2]>1),1]
  vmsValueLong <- vmsValueLong[vmsValueLong$wktradeMet %in% c(tt),]
  wktradeMet <- as.data.frame(unique(vmsValueLong$wktradeMet))
  metiers <- unique(vmsValueLong$wktradeMet)
  
  # remove areas deeper than 200 meter for overview of Greater North Sea
  if (Assregion == "Greater North Sea"){
    vmsValueLong <- cbind(vmsValueLong, Region@data[match(vmsValueLong$csquares,Region@data$csquares), c("Depth")])
    colnames(vmsValueLong)[ncol(vmsValueLong)] <- "Depth"
    vmsValueLong <-  subset(vmsValueLong,vmsValueLong$Depth >= -200)
  }

#Find c-squares with 90% of landings
  #Total
  VMS_MBCG_sum <- vmsValueLong %>% 
    group_by(year, csquares, wktradeMet) %>% 
    summarise(euro_sum=sum(euro)) 

  #Sort and make cumulated percentage
  VMS_MBCG_sum2 <- VMS_MBCG_sum %>% 
    group_by(year, wktradeMet) %>% 
    arrange(year, wktradeMet, desc(euro_sum)) %>%
    mutate(cum=  cumsum(euro_sum)/sum(euro_sum))

  VMS_MBCG_90pct <- VMS_MBCG_sum2[VMS_MBCG_sum2$cum <= 0.9,]
  VMS_MBCG_90pct$t <- 1
  VMS_MBCG_90pct <- VMS_MBCG_90pct[!is.na(VMS_MBCG_90pct$wktradeMet),]

  #Sum number of years>90% by metier and c-square
  VMS_MBCG_90pct2 <- VMS_MBCG_90pct %>% 
    group_by( wktradeMet, csquares) %>% 
    summarise(n_years=sum(t)) 

#Plot number of years by metier
  core <- ggplot(data=VMS_MBCG_90pct2, aes(n_years)) + geom_bar(fill = "#0073C2FF") +
            theme_bw() +  facet_wrap(~wktradeMet)+
            labs(x="Number of years (2013-2018)", y="Count") +
            theme(axis.text.x = element_text(size=8), plot.title=element_text(size=8), 
            axis.title = element_text(size=8)) 

  jpeg(paste(pathdir_output,paste(Assregion,"coreF_fig1.jpg",sep="_"),sep="/"),
      width=5.2,height=4.5, units = "in", res = 300 ) 
    print(core)
  dev.off()
  
# source CSquare2LonLat
  source(paste(pathdir,"Utilities/CSquare2LonLat.R",sep="/"))
  VMS_MBCG_90pct$Longitude=CSquare2LonLat(as.character(VMS_MBCG_90pct$csquares),0.05)$SI_LONG
  VMS_MBCG_90pct$Latitude=CSquare2LonLat(as.character(VMS_MBCG_90pct$csquares),0.05)$SI_LAT         

# get polygons core regions
  MBCG_90pct_poly <- 
    VMS_MBCG_90pct %>% 
    group_by(wktradeMet, csquares, Longitude, Latitude) %>% 
    summarise(n_years=sum(t), euro_sum=sum(euro_sum))  %>%
    mutate(
      wkt = paste0('POLYGON((',
                   Longitude + 0.025, ' ', Latitude + 0.025, ', ',
                   Longitude - 0.025, ' ', Latitude + 0.025, ', ',
                   Longitude - 0.025, ' ', Latitude - 0.025, ', ',
                   Longitude + 0.025, ' ', Latitude - 0.025, ', ',
                   Longitude + 0.025, ' ', Latitude + 0.025, '))')
    ) %>%
    st_as_sf(crs = 4326, wkt = "wkt")
  
  setwd(pathdir_output)  
  dir.create("shapefiles", showWarnings = FALSE)

  write_shape <- function(m){
    st_write(MBCG_90pct_poly[MBCG_90pct_poly$wktradeMet==m,], paste(pathdir_output,"/shapefiles/MBCG_90pct_poly_",Assregion,"_",m,".shp", sep=""), driver="ESRI Shapefile", delete_dsn = T)
  }
  
  for(m in metiers){
    write_shape(m=m)
  }
  
# get polygons Total MBCG poly
  VMS_MBCG_sum$Longitude=CSquare2LonLat(as.character(VMS_MBCG_sum$csquares),0.05)$SI_LONG
  VMS_MBCG_sum$Latitude=CSquare2LonLat(as.character(VMS_MBCG_sum$csquares),0.05)$SI_LAT
  
  MBCG_total_poly <- 
    VMS_MBCG_sum %>% 
    group_by(wktradeMet,csquares, Longitude, Latitude) %>% 
    summarise(euro_sum=sum(euro_sum))  %>%
    mutate(
      wkt = paste0('POLYGON((',
                   Longitude + 0.025, ' ', Latitude + 0.025, ', ',
                   Longitude - 0.025, ' ', Latitude + 0.025, ', ',
                   Longitude - 0.025, ' ', Latitude - 0.025, ', ',
                   Longitude + 0.025, ' ', Latitude - 0.025, ', ',
                   Longitude + 0.025, ' ', Latitude + 0.025, '))')
    ) %>%
    st_as_sf(crs = 4326, wkt = "wkt")
  
  write_shape <- function(m){
    st_write(MBCG_total_poly[MBCG_total_poly$wktradeMet==m,], paste(pathdir_output,"/shapefiles/MBCG_total_poly_",Assregion,"_",m,".shp", sep=""), driver="ESRI Shapefile", delete_dsn = T)
  }
  
  for(m in metiers){
    write_shape(m=m)
  }
  
  MBCG_total_poly2 <- 
    VMS_MBCG_sum %>% 
    group_by(year, csquares, Longitude, Latitude) %>% 
    summarise(euro_sum=sum(euro_sum))  %>%
    mutate(
      wkt = paste0('POLYGON((',
                   Longitude + 0.025, ' ', Latitude + 0.025, ', ',
                   Longitude - 0.025, ' ', Latitude + 0.025, ', ',
                   Longitude - 0.025, ' ', Latitude - 0.025, ', ',
                   Longitude + 0.025, ' ', Latitude - 0.025, ', ',
                   Longitude + 0.025, ' ', Latitude + 0.025, '))')
    ) %>%
    st_as_sf(crs = 4326, wkt = "wkt")
  
  st_write(MBCG_total_poly2[MBCG_total_poly2$year==2013,], paste(pathdir_output,"/shapefiles/MBCG_total_poly_2013",Assregion,".shp", sep=""), driver="ESRI Shapefile", delete_dsn = T)
  st_write(MBCG_total_poly2[MBCG_total_poly2$year==2014,], paste(pathdir_output,"/shapefiles/MBCG_total_poly_2014",Assregion,".shp", sep=""), driver="ESRI Shapefile", delete_dsn = T)
  st_write(MBCG_total_poly2[MBCG_total_poly2$year==2015,], paste(pathdir_output,"/shapefiles/MBCG_total_poly_2015",Assregion,".shp", sep=""), driver="ESRI Shapefile", delete_dsn = T)
  st_write(MBCG_total_poly2[MBCG_total_poly2$year==2016,], paste(pathdir_output,"/shapefiles/MBCG_total_poly_2016",Assregion,".shp", sep=""), driver="ESRI Shapefile", delete_dsn = T)
  st_write(MBCG_total_poly2[MBCG_total_poly2$year==2017,], paste(pathdir_output,"/shapefiles/MBCG_total_poly_2017",Assregion,".shp", sep=""), driver="ESRI Shapefile", delete_dsn = T)
  st_write(MBCG_total_poly2[MBCG_total_poly2$year==2018,], paste(pathdir_output,"/shapefiles/MBCG_total_poly_2018",Assregion,".shp", sep=""), driver="ESRI Shapefile", delete_dsn = T)

#Reference fishing grounds: within 90% value at at least 2 years out of the 6 years (2013-2018)
  MBCG_reference <- MBCG_90pct_poly[MBCG_90pct_poly$n_years>=2,]
  MBCG_reference_s <- as_Spatial(MBCG_reference)
  
  #Polygon with fishing grounds per year
  poly <- function(x, m){
    print(x)
    # check if there is data
    MBCG_FG <- 
      VMS_MBCG_90pct %>% 
      filter(year==x & wktradeMet==m)
    if(nrow(MBCG_FG)>0){
    MBCG_FG <- 
      VMS_MBCG_90pct %>% 
      filter(year==x & wktradeMet==m) %>%
      group_by(csquares, Longitude, Latitude) %>% 
      summarise(euro_sum=sum(euro_sum))  %>%
      mutate(
        wkt = paste0('POLYGON((',
                     Longitude + 0.025, ' ', Latitude + 0.025, ', ',
                     Longitude - 0.025, ' ', Latitude + 0.025, ', ',
                     Longitude - 0.025, ' ', Latitude - 0.025, ', ',
                     Longitude + 0.025, ' ', Latitude - 0.025, ', ',
                     Longitude + 0.025, ' ', Latitude + 0.025, '))')
      ) %>%
      st_as_sf(crs = 4326, wkt = "wkt")
    
    MBCG_FG_dis <- st_union(MBCG_FG)
    
    fil <- paste(pathdir_output,"/shapefiles/MBCG_FG_",Assregion,"_",x,"_",m,".shp",sep="")
    st_write(MBCG_FG_dis, fil, driver="ESRI Shapefile", delete_dsn = T)
    }
  }
  
  for(i in 2013:2018){
    for(m in metiers){
      poly(x=i, m=m)
    }
  }    
           
#####
  metiers1 <- VMS_MBCG_90pct %>% group_by(year, wktradeMet) %>% count(year, wktradeMet)
  metiers1 <- metiers1 %>% group_by(wktradeMet) %>% count(wktradeMet) 
  metiers1 <- metiers1 %>%filter(n==6)
  metiers2 <- unique(metiers1$wktradeMet)
  metiers2 <- metiers2[metiers2  %in% unique(MBCG_reference_s$wktradeMet)]
  
  setwd(pathdir_output)  
  dir.create("CSV", showWarnings = FALSE)
  
  for(m in metiers2){
    
    #Read polygons by metier and year
    FG2013 <- st_read(paste(pathdir_output,"/shapefiles/MBCG_FG_",Assregion,"_2013_",m,".shp",sep=""))
    FG2014 <- st_read(paste(pathdir_output,"/shapefiles/MBCG_FG_",Assregion,"_2014_",m,".shp",sep=""))
    FG2015 <- st_read(paste(pathdir_output,"/shapefiles/MBCG_FG_",Assregion,"_2015_",m,".shp",sep=""))
    FG2016 <- st_read(paste(pathdir_output,"/shapefiles/MBCG_FG_",Assregion,"_2016_",m,".shp",sep=""))
    FG2017 <- st_read(paste(pathdir_output,"/shapefiles/MBCG_FG_",Assregion,"_2017_",m,".shp",sep=""))
    FG2018 <- st_read(paste(pathdir_output,"/shapefiles/MBCG_FG_",Assregion,"_2018_",m,".shp",sep=""))
    
    FG2013$year <- 2013
    FG2014$year <- 2014
    FG2015$year <- 2015
    FG2016$year <- 2016
    FG2017$year <- 2017
    FG2018$year <- 2018
    
    FG2013_v <- st_buffer(FG2013, 0.0) 
    FG2014_v <- st_buffer(FG2014, 0.0) 
    FG2015_v <- st_buffer(FG2015, 0.0) 
    FG2016_v <- st_buffer(FG2016, 0.0) 
    FG2017_v <- st_buffer(FG2017, 0.0) 
    FG2018_v <- st_buffer(FG2018, 0.0) 
    
    FG2013_s <- as_Spatial(FG2013_v)
    FG2014_s <- as_Spatial(FG2014_v)
    FG2015_s <- as_Spatial(FG2015_v)
    FG2016_s <- as_Spatial(FG2016_v)
    FG2017_s <- as_Spatial(FG2017_v)
    FG2018_s <- as_Spatial(FG2018_v)
    
    MBCG_reference_metier <- MBCG_reference_s[MBCG_reference_s$wktradeMet==m,] 
    
    FG13 <- raster::crop(FG2013_s, MBCG_reference_metier)
    FG14 <- raster::crop(FG2014_s, MBCG_reference_metier)
    FG15 <- raster::crop(FG2015_s, MBCG_reference_metier)
    FG16 <- raster::crop(FG2016_s, MBCG_reference_metier)
    FG17 <- raster::crop(FG2017_s, MBCG_reference_metier)
    FG18 <- raster::crop(FG2018_s, MBCG_reference_metier)
    
    FG13_sf <- st_as_sf(FG13)
    FG14_sf <- st_as_sf(FG14)
    FG15_sf <- st_as_sf(FG15)
    FG16_sf <- st_as_sf(FG16)
    FG17_sf <- st_as_sf(FG17)
    FG18_sf <- st_as_sf(FG18)
    
    st_write(FG13_sf, paste(pathdir_output,"/shapefiles/MBCG_FG_overlap_2013_",Assregion, "_", m,".shp", sep=""), delete_dsn = T)
    st_write(FG14_sf, paste(pathdir_output,"/shapefiles/MBCG_FG_overlap_2014_",Assregion, "_",m,".shp", sep=""), delete_dsn = T)
    st_write(FG15_sf, paste(pathdir_output,"/shapefiles/MBCG_FG_overlap_2015_",Assregion, "_",m,".shp", sep=""), delete_dsn = T)
    st_write(FG16_sf, paste(pathdir_output,"/shapefiles/MBCG_FG_overlap_2016_",Assregion, "_",m,".shp", sep=""), delete_dsn = T)
    st_write(FG17_sf, paste(pathdir_output,"/shapefiles/MBCG_FG_overlap_2017_",Assregion, "_",m,".shp", sep=""), delete_dsn = T)
    st_write(FG18_sf, paste(pathdir_output,"/shapefiles/MBCG_FG_overlap_2018_",Assregion, "_",m,".shp", sep=""), delete_dsn = T)
    
    #Corefishinggrounds
    MBCG_reference_sf <- st_as_sf(MBCG_reference_metier)
    MBCG_reference_sf$division <- Assregion
    st_write(MBCG_reference_sf, paste(pathdir_output,"/shapefiles/MBCG_FG_reference_",Assregion, "_",m,".shp", sep=""), delete_dsn = T)
    
    corefishinggrounds <- mapview(MBCG_reference_sf, zcol="euro_sum")
    
    #Area
    area2013_km2 <- st_area(FG2013)*0.000001
    area2014_km2 <- st_area(FG2014)*0.000001
    area2015_km2 <- st_area(FG2015)*0.000001
    area2016_km2 <- st_area(FG2016)*0.000001
    area2017_km2 <- st_area(FG2017)*0.000001
    area2018_km2 <- st_area(FG2018)*0.000001
    
    area_overlap13_km2 <- st_area(FG13_sf)*0.000001
    area_overlap14_km2 <- st_area(FG14_sf)*0.000001
    area_overlap15_km2 <- st_area(FG15_sf)*0.000001
    area_overlap16_km2 <- st_area(FG16_sf)*0.000001
    area_overlap17_km2 <- st_area(FG17_sf)*0.000001
    area_overlap18_km2 <- st_area(FG18_sf)*0.000001
    
    year <- c(2013, 2014, 2015, 2016, 2017, 2018)
    area_FG <- c(area2013_km2, area2014_km2, area2015_km2, area2016_km2, area2017_km2, area2018_km2)
    area_overlap <- c(area_overlap13_km2, area_overlap14_km2, 
                      area_overlap15_km2, area_overlap16_km2, area_overlap17_km2, area_overlap18_km2)
    
    FG_area_overlap <- data.frame(year, area_FG, area_overlap)
    FG_area_overlap$pct <- (area_overlap/area_FG)*100
    FG_area_overlap$metier <- m
    
    write.csv(FG_area_overlap, paste(pathdir_output,"/CSV/area_overlap_",Assregion,"_", m,".csv", sep=""))
  }
  
################################################################################
  combined_overlap <- data.frame(X = NA, year=NA, area_FG=NA, area_overlap=NA, pct=NA, metier=NA)
  
  for(m in metiers2){
    print(m)
    fil <- read.csv(paste(pathdir_output,"/CSV/area_overlap_",Assregion,"_",m,".csv",sep=""))
    combined_overlap <- rbind(combined_overlap,fil)
  }
  
  combined_overlap = combined_overlap[-1,]
  combined_overlap$pct[combined_overlap$pct >100 ] <- NA
  
  #Plot overlap by metier
  core2 <- ggplot(data=combined_overlap, aes(year, pct)) + geom_bar(stat = "identity") +
    theme_bw() + facet_wrap(~metier)+
    labs(x="Year (2013-2018)", y="Percent area overlap")+
    theme(axis.text.x = element_text(size=8), plot.title=element_text(size=8), axis.title = element_text(size=8)) 
 
  jpeg(paste(pathdir_output,paste(Assregion,"coreF_fig2.jpg",sep="_"),sep="/"),
      width=5.2,height=4.5, units = "in", res = 300 ) 
  print(core2)
  dev.off()
  
#Add to combined overlap: total value by metier and year (VMS_MBCG_sum)
  #Total euro
  euro_total <- vmsValueLong %>% 
    group_by(year, wktradeMet) %>% 
    summarise(euro_total=sum(euro)) 
  combined_overlap2 <- merge(combined_overlap, euro_total, by.x=c("year","metier"), by.y=c("year","wktradeMet"))
  
  #90% euro
  euro_90pct <- VMS_MBCG_90pct %>% 
    group_by(year, wktradeMet) %>% 
    summarise(euro_90pct=sum(euro_sum)) 
  combined_overlap3 <- merge(combined_overlap2, euro_90pct, by.x=c("year","metier"), by.y=c("year","wktradeMet"))
  
  #Plot pct cumulated value vs. area
  #Make polygon to calculate area
  
  VMS_MBCG_sum2$Longitude=CSquare2LonLat(as.character(VMS_MBCG_sum2$csquares),0.05)$SI_LONG
  VMS_MBCG_sum2$Latitude=CSquare2LonLat(as.character(VMS_MBCG_sum2$csquares),0.05)$SI_LAT
  
  VMS_MBCG_sum2$value_pct=VMS_MBCG_sum2$cum*100
  
  VMS_MBCG_sum_poly <-  VMS_MBCG_sum2 %>% 
    group_by(year, wktradeMet, csquares, Longitude, Latitude) %>% 
    mutate(
      wkt = paste0('POLYGON((',
                   Longitude + 0.025, ' ', Latitude + 0.025, ', ',
                   Longitude - 0.025, ' ', Latitude + 0.025, ', ',
                   Longitude - 0.025, ' ', Latitude - 0.025, ', ',
                   Longitude + 0.025, ' ', Latitude - 0.025, ', ',
                   Longitude + 0.025, ' ', Latitude + 0.025, '))')
    ) %>%
    st_as_sf(crs = 4326, wkt = "wkt")
  
  VMS_MBCG_sum_poly$area_km2 <- as.vector(st_area(VMS_MBCG_sum_poly)*0.000001)
  
  VMS_MBCG_sum_poly2 <- VMS_MBCG_sum_poly %>% 
    group_by(year, wktradeMet) %>% 
    mutate(area_pct=(cumsum(area_km2)/sum(area_km2))*100)
  
  VMS_MBCG_sum3 <- as.data.frame(VMS_MBCG_sum_poly2)
  
  #Plot number of years by metier
  core3 <- ggplot(data=VMS_MBCG_sum3, aes(area_pct, value_pct, colour=year)) +
                  geom_line() + theme_bw() + facet_wrap(~wktradeMet)+
    labs(x="Area %", y="Landing value %") +
    theme(axis.text.x = element_text(size=8), plot.title=element_text(size=8), axis.title = element_text(size=8)) 
  
  jpeg(paste(pathdir_output,paste(Assregion,"coreF_fig3.jpg",sep="_"),sep="/"),
      width=5.2,height=4.5, units = "in", res = 300 ) 
  print(core3)
  dev.off()
  
##################################################################################
##################################################################################
  # create core area management option
  
  #Greater North Sea
  # CFG_DRB_MOL <- st_read(paste(pathdir_output,"/shapefiles/MBCG_FG_reference_Greater North Sea_DRB_MOL.shp",sep=""))
  # CFG_OT_CRU  <- st_read(paste(pathdir_output,"/shapefiles/MBCG_FG_reference_Greater North Sea_OT_CRU.shp",sep=""))
  # CFG_OT_DMF  <- st_read(paste(pathdir_output,"/shapefiles/MBCG_FG_reference_Greater North Sea_OT_DMF.shp",sep=""))
  # CFG_OT_MIX  <- st_read(paste(pathdir_output,"/shapefiles/MBCG_FG_reference_Greater North Sea_OT_MIX.shp",sep=""))
  # CFG_OT_SPF  <- st_read(paste(pathdir_output,"/shapefiles/MBCG_FG_reference_Greater North Sea_OT_SPF.shp",sep=""))
  # CFG_SDN_DMF <- st_read(paste(pathdir_output,"/shapefiles/MBCG_FG_reference_Greater North Sea_SDN_DMF.shp",sep=""))
  # CFG_SSC_DMF <- st_read(paste(pathdir_output,"/shapefiles/MBCG_FG_reference_Greater North Sea_SSC_DMF.shp",sep=""))
  # CFG_TBB_CRU <- st_read(paste(pathdir_output,"/shapefiles/MBCG_FG_reference_Greater North Sea_TBB_CRU.shp",sep=""))
  # CFG_TBB_DMF <- st_read(paste(pathdir_output,"/shapefiles/MBCG_FG_reference_Greater North Sea_TBB_DMF.shp",sep=""))
  # CFG_TBB_MOL <- st_read(paste(pathdir_output,"/shapefiles/MBCG_FG_reference_Greater North Sea_TBB_MOL.shp",sep=""))
  
  # CFG_NS <- rbind(CFG_DRB_MOL, CFG_OT_CRU, CFG_OT_DMF, CFG_OT_MIX, CFG_OT_SPF, CFG_SDN_DMF, CFG_SSC_DMF,
  #                CFG_TBB_CRU, CFG_TBB_DMF, CFG_TBB_MOL, deparse.level = 1)
  
  # saveRDS(CFG_NS, file=paste(pathdir_output,"/CoreFishingGrounds_GreaterNorthSea.RDS",sep=""))
  
# now delete the two folders, too big for github
  unlink(paste(pathdir_output,"shapefiles",sep="/"), recursive = TRUE)
  unlink(paste(pathdir_output,"CSV",sep="/"), recursive = TRUE)
  
  
  rm(list= ls()[!(ls() %in% c('pathdir','pathdir_nogit','Assregion_index','Assunit_index','EcoReg_index',
                              'Period','AssPeriod',"EcoReg",'Fisheries','FisheriesMet','p','regions_with_impact',
                              'Region','State_reg','State_reg_IL',"Assunit","Assregion","msfd_csq"))])
  
  