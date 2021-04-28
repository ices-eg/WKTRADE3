
# estimate percentiles of SAR and value per metier

# Get the world map
  worldMap <- map_data("world")
  
# data process
  SSAR_year <- paste("surface_sar",AssPeriod,sep="_")
  weight_year <- paste("total_weight",AssPeriod,sep="_")
  value_year <- paste("total_value",AssPeriod,sep="_")

# add correct column names for map plots
  idx <- which(names(Region@data)== "long")
  colnames(Region@data)[idx]  <- "longitude"
  idx <- which(names(Region@data)== "lat")
  colnames(Region@data)[idx]  <- "latitude"

# get map 
  minlong <- round(min(Region@data$longitude)-1)
  maxlong <- round(max(Region@data$longitude)+1)
  minlat  <- round(min(Region@data$latitude)-1)
  maxlat  <- round(max(Region@data$latitude)+1)
  coordslim <- c(minlong,maxlong,minlat,maxlat)
  coordxmap <- round(seq(minlong,maxlong,length.out = 4))
  coordymap <- round(seq(minlat,maxlat,length.out = 4))
  copl <- c("#a6bddb","#74a9cf","#2b8cbe","#045a8d","#d9f0a3","#addd8e","#78c679","#31a354","#d95f0e","#993404")
  labeltext <- c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100")
  pointsize <- max(20/((maxlat-minlat)^1.5),20/((maxlong-minlong)^1.5))
  
#####
# Figure core percentiles
################
  Coref <- Region@data
  gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  output <- c()
  plotlist <- list()
  
  for (pp in 1:length(gears)){
  nam <- paste(rep(paste(gears[pp],"surface_sar",sep="_"),length(AssPeriod)),AssPeriod,sep="_")
  Coref <- cbind(Coref, FisheriesMet[match(Coref$csquares,FisheriesMet$csquares), c(nam)])
  Coref[,c(nam)][is.na(Coref[,c(nam)])] <- 0
  Coref$avgsar <- rowMeans(Coref[,nam])
  
  nam <- paste(rep(paste(gears[pp],"total_value",sep="_"),length(AssPeriod)),AssPeriod,sep="_")
  Coref <- cbind(Coref, FisheriesMet[match(Coref$csquares,FisheriesMet$csquares), c(nam)])
  Coref[,c(nam)][is.na(Coref[,c(nam)])] <- 0
  Coref$avgvalue <- rowMeans(Coref[,nam])
  
  if(sum(Coref$avgsar) > 0){
    output <- c(output, gears[pp])  
  
    effort <- Coref
    effort <- effort[order(effort$avgsar),]
    effort$perc <- cumsum(effort$avgsar) / sum(effort$avgsar)*100
    quat <- seq(0, 100,  10)
    effort$cat <- cut(effort$perc,c(quat),right = T) 
    effort <- subset(effort, !(effort$perc == 0))
    
    value <- Coref
    value <- value[order(value$avgvalue),]
    value$perc <- cumsum(value$avgvalue) / sum(value$avgvalue)*100
    quat <- seq(0, 100,  10)
    value$cat <- cut(value$perc,c(quat),right = T) 
    value <- subset(value, !(value$perc == 0))
    
    figmap <- ggplot() + geom_point(data=effort, aes(x=longitude, y=latitude , col=cat),
                                    shape=15,size=pointsize,na.rm=T)  + ggtitle(paste(gears[pp]))
    figmap <- figmap +  scale_color_manual(values = copl,name ="Percentiles",labels=labeltext)         
    figmap <- figmap +  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")
    figmap <- figmap +  theme(plot.background=element_blank(),
                              panel.background=element_blank(),
                              axis.text.y   = element_text(size=16),
                              axis.text.x   = element_text(size=16),
                              axis.title.y  = element_text(size=16),
                              axis.title.x  = element_text(size=16),
                              panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                              legend.text   = element_text(size=11),
                              legend.title  = element_text(size=11),
                              legend.position ="none") +
      scale_x_continuous(breaks=coordxmap, name = "longitude") +
      scale_y_continuous(breaks=coordymap, name = "latitude")  +
      coord_cartesian(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))
    plota <- figmap +   guides(colour = guide_legend(override.aes = list(size=5),nrow=2,byrow=TRUE))  
    assign(paste(gears[pp],"a",sep="_"),plota)
    
  ###  
    figmap <- ggplot() + geom_point(data=value, aes(x=longitude, y=latitude , col=cat),
                                    shape=15,size=pointsize,na.rm=T)  + ggtitle(paste(gears[pp]))
    figmap <- figmap +  scale_color_manual(values = copl,name ="Percentiles",labels=labeltext)         
    figmap <- figmap +  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")
    figmap <- figmap +  theme(plot.background=element_blank(),
                              panel.background=element_blank(),
                              axis.text.y   = element_text(size=16),
                              axis.text.x   = element_text(size=16),
                              axis.title.y  = element_text(size=16),
                              axis.title.x  = element_text(size=16),
                              panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                              legend.text   = element_text(size=11),
                              legend.title  = element_text(size=11),
                              legend.position ="none") +
      scale_x_continuous(breaks=coordxmap, name = "longitude") +
      scale_y_continuous(breaks=coordymap, name = "latitude")  +
      coord_cartesian(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))
    plotb <- figmap +   guides(colour = guide_legend(override.aes = list(size=5),nrow=2,byrow=TRUE))  
    assign(paste(gears[pp],"b",sep="_"),plotb)
    
    }}

  source("Core_fishing_get_legend.R")
  
  datplots <- paste(rep(output,each=2),c("a","b"),sep="_")  
  plist    <- mget(datplots)
  datplots <- paste(rep(output,each=1),c("a"),sep="_")  
  plist2    <- mget(datplots)

  id_height <- 4.5*length(output)
  
  # set directory
  setwd(paste(pathdir,"5 - Output",Assunit,Assregion,sep="/"))
  
  jpeg(paste(Assregion,"Core_percentiles.jpg",sep="_"),width=8,height=id_height, units = "in", res = 500 ) 
  print(grid.arrange(arrangeGrob(grobs = plist,ncol=2),mylegend,heights=c(10,.5)))
  dev.off()
  
 # jpeg(paste(Assregion,"Core_percentiles2.jpg",sep="_"),width=16,height=16, units = "in", res = 500 ) 
 # print(grid.arrange(grobs = plist2,ncol=4))
 # dev.off()
  
  rm(list= ls()[!(ls() %in% c('pathdir','pathdir_nogit','Assregion_index','Assunit_index','EcoReg_index',
                              'Period','AssPeriod',"EcoReg",'Fisheries','FisheriesMet','p','regions_with_impact',
                              'Region','State_reg','State_reg_IL',"Assunit","Assregion","msfd_csq","regions_with_corefishing"))])
  
  