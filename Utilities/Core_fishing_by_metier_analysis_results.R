
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
  vmsValueLong <- vmsValueLong[vmsValueLong$year %in% c(AssPeriod),]
  vmsValueLong$metier <- substr(vmsValueLong$column, 1, nchar(as.character(vmsValueLong$column))-17)
  vmsValueLong$wktradeMet <- vmsValueLong$metier
  
  # remove all metiers with few data
  tt <- aggregate(vmsValueLong$csquares,by=list(vmsValueLong$wktradeMet),FUN=length)
  tt <- tt[which(tt[,2]>50),1]
  vmsValueLong <- vmsValueLong[vmsValueLong$wktradeMet %in% c(tt),]
  wktradeMet <- as.data.frame(unique(vmsValueLong$wktradeMet))
  metiers <- unique(vmsValueLong$wktradeMet)

  # and remove metiers without data in all years
  #metiers1 <- vmsValueLong %>% group_by(year, wktradeMet) %>% count(year, wktradeMet)
  #metiers1 <- metiers1 %>% group_by(wktradeMet) %>% count(wktradeMet) 
  #metiers1 <- metiers1 %>%filter(n==6)
  #metiers2 <- unique(metiers1$wktradeMet)
  #vmsValueLong <- vmsValueLong[vmsValueLong$wktradeMet %in% c(metiers2),]
  #metiers <- c(metiers,metiers2)
  #metiers <- metiers[duplicated(metiers)]
  
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

  VMS_MBCG_90pct2$t <- 1
  VMS_MBCG_90pct2 <- VMS_MBCG_90pct2 %>% 
    group_by( wktradeMet, n_years) %>% 
    summarise(n_csquares=sum(t)) 
  
  VMS_MBCG_90pct2_tot <- VMS_MBCG_90pct2 %>% 
    group_by( wktradeMet) %>% 
    summarise(n_csquares_tot=sum(n_csquares)) 
  
  VMS_MBCG_90pct2 <- merge(VMS_MBCG_90pct2,VMS_MBCG_90pct2_tot, by=c("wktradeMet"))
  VMS_MBCG_90pct2$pct <- (VMS_MBCG_90pct2$n_csquares/VMS_MBCG_90pct2$n_csquares_tot)*100 

#Plot number of years by metier
  core <- ggplot(data=VMS_MBCG_90pct2, aes(x= n_years, y= pct)) +
            geom_bar(fill = "#0073C2FF",stat="identity") +
            theme_bw() +  facet_wrap(~wktradeMet)+
            labs(x=paste("Number of years (",AssPeriod[1],"-",
                         AssPeriod[length(AssPeriod)],")",sep=""),
                 y="Percent c-squares") +
            theme(axis.text.x = element_text(size=8), plot.title=element_text(size=8), 
            axis.title = element_text(size=8)) 
  
  # add total number of core c-squares
  xx <- max(VMS_MBCG_90pct2$n_years)-1
  yy <- max(VMS_MBCG_90pct2$pct) -10
  labtext <- ifelse(duplicated(VMS_MBCG_90pct2[,c("wktradeMet","n_csquares_tot")]) == T, "",
                    paste("n=",VMS_MBCG_90pct2$n_csquares_tot,sep=""))
  core <- core + geom_text(data=VMS_MBCG_90pct2,label=labtext, x=xx,y=yy,size=3) +  facet_wrap(~wktradeMet)
  
  plotsize <- 4.5
  plotsize <- ifelse(length(tt) < 4, 2.5,4.5)
  
  jpeg(paste(pathdir_output,paste(Assregion,"coreF_fig1.jpg",sep="_"),sep="/"),
      width=5.2,height=plotsize, units = "in", res = 150 ) 
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
  
  st_write(MBCG_total_poly2[MBCG_total_poly2$year==AssPeriod[1],], paste(pathdir_output,paste("/shapefiles/MBCG_total_poly_",AssPeriod[1],sep="_"),Assregion,".shp", sep=""), driver="ESRI Shapefile", delete_dsn = T)
  st_write(MBCG_total_poly2[MBCG_total_poly2$year==AssPeriod[2],], paste(pathdir_output,paste("/shapefiles/MBCG_total_poly_",AssPeriod[2],sep="_"),Assregion,".shp", sep=""), driver="ESRI Shapefile", delete_dsn = T)
  st_write(MBCG_total_poly2[MBCG_total_poly2$year==AssPeriod[3],], paste(pathdir_output,paste("/shapefiles/MBCG_total_poly_",AssPeriod[3],sep="_"),Assregion,".shp", sep=""), driver="ESRI Shapefile", delete_dsn = T)
  st_write(MBCG_total_poly2[MBCG_total_poly2$year==AssPeriod[4],], paste(pathdir_output,paste("/shapefiles/MBCG_total_poly_",AssPeriod[4],sep="_"),Assregion,".shp", sep=""), driver="ESRI Shapefile", delete_dsn = T)
  st_write(MBCG_total_poly2[MBCG_total_poly2$year==AssPeriod[5],], paste(pathdir_output,paste("/shapefiles/MBCG_total_poly_",AssPeriod[5],sep="_"),Assregion,".shp", sep=""), driver="ESRI Shapefile", delete_dsn = T)
  st_write(MBCG_total_poly2[MBCG_total_poly2$year==AssPeriod[6],], paste(pathdir_output,paste("/shapefiles/MBCG_total_poly_",AssPeriod[6],sep="_"),Assregion,".shp", sep=""), driver="ESRI Shapefile", delete_dsn = T)

#Reference fishing grounds: within 90% value at at least 2 years out of the 6 years (assess period)
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
  
  for(i in AssPeriod[1]:AssPeriod[length(AssPeriod)]){
    for(m in metiers){
      poly(x=i, m=m)
    }
  }    
           
#####
  metiers1 <- VMS_MBCG_90pct %>% group_by(year, wktradeMet) %>% count(year, wktradeMet)
  metiers1 <- metiers1 %>% group_by(wktradeMet) %>% count(wktradeMet) 
  metiers1 <- metiers1 %>%filter(n==6)
  metiers2 <- unique(metiers1$wktradeMet)
  metiers2 <- metiers2[metiers2  %in% unique(MBCG_reference$wktradeMet)]
  
  setwd(pathdir_output)  
  dir.create("CSV", showWarnings = FALSE)
  
  for(m in metiers2){
    #Read polygons by metier and year
    FGyear1 <- st_read(paste(pathdir_output,"/shapefiles/MBCG_FG_",Assregion,paste("_",AssPeriod[1],"_",sep=""),m,".shp",sep=""))
    FGyear2 <- st_read(paste(pathdir_output,"/shapefiles/MBCG_FG_",Assregion,paste("_",AssPeriod[2],"_",sep=""),m,".shp",sep=""))
    FGyear3 <- st_read(paste(pathdir_output,"/shapefiles/MBCG_FG_",Assregion,paste("_",AssPeriod[3],"_",sep=""),m,".shp",sep=""))
    FGyear4 <- st_read(paste(pathdir_output,"/shapefiles/MBCG_FG_",Assregion,paste("_",AssPeriod[4],"_",sep=""),m,".shp",sep=""))
    FGyear5 <- st_read(paste(pathdir_output,"/shapefiles/MBCG_FG_",Assregion,paste("_",AssPeriod[5],"_",sep=""),m,".shp",sep=""))
    FGyear6 <- st_read(paste(pathdir_output,"/shapefiles/MBCG_FG_",Assregion,paste("_",AssPeriod[6],"_",sep=""),m,".shp",sep=""))
    
    FGyear1$year <- AssPeriod[1]
    FGyear2$year <- AssPeriod[2]
    FGyear3$year <- AssPeriod[3]
    FGyear4$year <- AssPeriod[4]
    FGyear5$year <- AssPeriod[5]
    FGyear6$year <- AssPeriod[6]
    
    #FGyear1_v <- st_buffer(FGyear1, 0.0) 
    #FGyear2_v <- st_buffer(FGyear2, 0.0) 
    #FGyear3_v <- st_buffer(FGyear3, 0.0) 
    #FGyear4_v <- st_buffer(FGyear4, 0.0) 
    #FGyear5_v <- st_buffer(FGyear5, 0.0) 
    #FGyear6_v <- st_buffer(FGyear6, 0.0) 
    
    MBCG_reference_metier <- MBCG_reference[MBCG_reference$wktradeMet==m,]
    MBCG_reference_metier_union <- st_union(MBCG_reference_metier)
    
    FGy1 <- st_intersection(FGyear1,MBCG_reference_metier_union)
    FGy2 <- st_intersection(FGyear2,MBCG_reference_metier_union)
    FGy3 <- st_intersection(FGyear3,MBCG_reference_metier_union)
    FGy4 <- st_intersection(FGyear4,MBCG_reference_metier_union)
    FGy5 <- st_intersection(FGyear5,MBCG_reference_metier_union)
    FGy6 <- st_intersection(FGyear6,MBCG_reference_metier_union)
  
    FGy1_sf <- if(length(FGy1) > 0) {(FGy1)} 
    FGy2_sf <- if(length(FGy2) > 0) {(FGy2)} 
    FGy3_sf <- if(length(FGy3) > 0) {(FGy3)} 
    FGy4_sf <- if(length(FGy4) > 0) {(FGy4)} 
    FGy5_sf <- if(length(FGy5) > 0) {(FGy5)} 
    FGy6_sf <- if(length(FGy6) > 0) {(FGy6)} 
    
    try(st_write(FGy1_sf, paste(pathdir_output,"/shapefiles/MBCG_FG_overlap_",paste(AssPeriod[1],"_",sep=""),Assregion, "_",m,".shp", sep=""), delete_dsn = T))
    try(st_write(FGy2_sf, paste(pathdir_output,"/shapefiles/MBCG_FG_overlap_",paste(AssPeriod[2],"_",sep=""),Assregion, "_",m,".shp", sep=""), delete_dsn = T))
    try(st_write(FGy3_sf, paste(pathdir_output,"/shapefiles/MBCG_FG_overlap_",paste(AssPeriod[3],"_",sep=""),Assregion, "_",m,".shp", sep=""), delete_dsn = T))
    try(st_write(FGy4_sf, paste(pathdir_output,"/shapefiles/MBCG_FG_overlap_",paste(AssPeriod[4],"_",sep=""),Assregion, "_",m,".shp", sep=""), delete_dsn = T))
    try(st_write(FGy5_sf, paste(pathdir_output,"/shapefiles/MBCG_FG_overlap_",paste(AssPeriod[5],"_",sep=""),Assregion, "_",m,".shp", sep=""), delete_dsn = T))
    try(st_write(FGy6_sf, paste(pathdir_output,"/shapefiles/MBCG_FG_overlap_",paste(AssPeriod[6],"_",sep=""),Assregion, "_",m,".shp", sep=""), delete_dsn = T))
    
    #Corefishinggrounds
    MBCG_reference_metier$division <- Assregion
    st_write(MBCG_reference_metier, paste(pathdir_output,"/shapefiles/MBCG_FG_reference_",Assregion, "_",m,".shp", sep=""), delete_dsn = T)
    
    corefishinggrounds <- mapview(MBCG_reference_metier, zcol="euro_sum")
    
    #Area
    areay1_km2 <- try(st_area(FGyear1)*0.000001)
    areay2_km2 <- try(st_area(FGyear2)*0.000001)
    areay3_km2 <- try(st_area(FGyear3)*0.000001)
    areay4_km2 <- try(st_area(FGyear4)*0.000001)
    areay5_km2 <- try(st_area(FGyear5)*0.000001)
    areay6_km2 <- try(st_area(FGyear6)*0.000001)
    
    area_overlapy1_km2 <- ifelse(length(FGy1_sf$geometry) == 0, NA,st_area(FGy1_sf$geometry)*0.000001)
    area_overlapy2_km2 <- ifelse(length(FGy2_sf$geometry) == 0, NA,st_area(FGy2_sf$geometry)*0.000001)
    area_overlapy3_km2 <- ifelse(length(FGy3_sf$geometry) == 0, NA,st_area(FGy3_sf$geometry)*0.000001)
    area_overlapy4_km2 <- ifelse(length(FGy4_sf$geometry) == 0, NA,st_area(FGy4_sf$geometry)*0.000001)
    area_overlapy5_km2 <- ifelse(length(FGy5_sf$geometry) == 0, NA,st_area(FGy5_sf$geometry)*0.000001)
    area_overlapy6_km2 <- ifelse(length(FGy6_sf$geometry) == 0, NA,st_area(FGy6_sf$geometry)*0.000001)
    
    year <- AssPeriod
    area_FG <- c(areay1_km2, areay2_km2, areay3_km2, 
                 areay4_km2, areay5_km2, areay6_km2)
    area_overlap <- c(area_overlapy1_km2, area_overlapy2_km2, 
                      area_overlapy3_km2, area_overlapy4_km2, 
                      area_overlapy5_km2, area_overlapy6_km2)
    
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
    labs(x=paste("Year (",AssPeriod[1],"-",
                         AssPeriod[length(AssPeriod)],")",sep=""),
         y="Percent area overlap")+
    theme(axis.text.x = element_text(size=8), plot.title=element_text(size=8), axis.title = element_text(size=8)) 
 
  plotsize2 <- 4.5
  plotsize2 <- ifelse(length(metiers2) < 4, 2.5,4.5)
  
  jpeg(paste(pathdir_output,paste(Assregion,"coreF_fig2.jpg",sep="_"),sep="/"),
      width=5.2,height=plotsize2, units = "in", res = 150 ) 
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
      width=5.2,height=plotsize, units = "in", res = 150 ) 
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
                              'Region','State_reg','State_reg_IL',"Assunit","Assregion","msfd_csq","regions_with_corefishing"))])
  
  