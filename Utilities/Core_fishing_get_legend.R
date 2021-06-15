

nam <- paste(rep(paste("OT_DMF","surface_sar",sep="_"),length(AssPeriod)),AssPeriod,sep="_")
Coref <- cbind(Coref, FisheriesMet[match(Coref$csquares,FisheriesMet$csquares), c(nam)])
Coref[,c(nam)][is.na(Coref[,c(nam)])] <- 0
Coref$avgsar <- rowMeans(Coref[,nam])

effort <- Coref
effort <- effort[order(effort$avgsar),]
effort$perc <- cumsum(effort$avgsar) / sum(effort$avgsar)*100
quat <- seq(0, 100,  10)
effort$cat <- cut(effort$perc,c(quat),right = T) 
effort <- subset(effort, !(effort$perc == 0))

figmap <- ggplot() + geom_point(data=effort, aes(x=longitude, y=latitude , col=cat),
                                shape=15,size=0.5,na.rm=T)  + ggtitle(paste(gears[pp]))
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
                          legend.position ="top") +
  scale_x_continuous(breaks=coordxmap, name = "longitude") +
  scale_y_continuous(breaks=coordymap, name = "latitude")  +
  coord_cartesian(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))
plota <- figmap +   guides(colour = guide_legend(override.aes = list(size=5),nrow=2,byrow=TRUE))  

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(plota)