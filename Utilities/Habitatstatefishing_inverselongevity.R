
##################################################################
# calculate state for each year

# loop each year and calculate state 
  state_year <- c()
  ccname <- c()

  FisheriesMet<-cbind(FisheriesMet, Region@data[match(FisheriesMet$csquares,Region@data$csquares), c("intercept","slope")])
  
  # estimate impact from inverse longevity
  for (i in 1: length(Period)){
    loopdata <- FisheriesMet
    
    gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX",
               "OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
    
    nam <- paste(gears,"surface_sar",Period[i],sep="_")
    loopdata[,nam][is.na(loopdata[,nam])] <- 0

### calculate state for total SAR
    SAR_tot <- rowSums(loopdata[,nam])
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
       if (SAR_tot[j] > 0 ) {
      state[j] <- LSI(Fs=SAR_tot[j],a=loopdata$slope[j],b=loopdata$intercept[j])
    }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state",Period[i],sep="_"))
    
 ### calculate state for DRB_MOL
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata[,nam[1]][j] > 0 ) {
        state[j] <- LSI(Fs=loopdata[,nam[1]][j],a=loopdata$slope[j],b=loopdata$intercept[j])
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state_DRB_MOL",Period[i],sep="_"))
    
 ### calculate state for OT_CRU (combined)
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata[,nam[2]][j] > 0 ) {
        state[j] <- LSI(Fs=loopdata[,nam[2]][j],a=loopdata$slope[j],b=loopdata$intercept[j])
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state_OT_CRU",Period[i],sep="_"))

 ### calculate state for OT_DMF
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata[,nam[3]][j] > 0 ) {
        state[j] <- LSI(Fs=loopdata[,nam[3]][j], a=loopdata$slope[j],b=loopdata$intercept[j])
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state_OT_DMF",Period[i],sep="_"))
    
 ### calculate state for OT_MIX (combined)
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata[,nam[4]][j] > 0 ) {
        state[j] <- LSI(Fs=loopdata[,nam[4]][j],a=loopdata$slope[j],b=loopdata$intercept[j])
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state_OT_MIX",Period[i],sep="_"))
   
 ### calculate state for OT_SPF
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata[,nam[5]][j] > 0 ) {
        state[j] <- LSI(Fs=loopdata[,nam[5]][j],a=loopdata$slope[j],b=loopdata$intercept[j])
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state_OT_SPF",Period[i],sep="_"))    
     
 ### calculate state for SDN_DMF
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata[,nam[6]][j] > 0 ) {
        state[j] <- LSI(Fs=loopdata[,nam[6]][j],a=loopdata$slope[j],b=loopdata$intercept[j])
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state_SDN_DMF",Period[i],sep="_"))   
    
 ### calculate state for SSC_DMF
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata[,nam[7]][j] > 0 ) {
        state[j] <- LSI(Fs=loopdata[,nam[7]][j],a=loopdata$slope[j],b=loopdata$intercept[j])
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state_SSC_DMF",Period[i],sep="_"))   
   
 ### calculate state for TBB_CRU
     state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata[,nam[8]][j] > 0 ) {
        state[j] <- LSI(Fs=loopdata[,nam[8]][j],a=loopdata$slope[j],b=loopdata$intercept[j])
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state_TBB_CRU",Period[i],sep="_"))     
    
 ### calculate state for TBB_DMF
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata[,nam[9]][j] > 0 ) {
        state[j] <- LSI(Fs=loopdata[,nam[9]][j],a=loopdata$slope[j],b=loopdata$intercept[j])
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state_TBB_DMF",Period[i],sep="_"))     
    
 ### calculate state for TBB_MOL
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata[,nam[10]][j] > 0 ) {
        state[j] <- LSI(Fs=loopdata[,nam[10]][j],a=loopdata$slope[j],b=loopdata$intercept[j])
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state_TBB_MOL",Period[i],sep="_")) 
  
  }
  
  colnames(state_year)<-ccname
  State_reg <- data.frame(Fisheries$csquares,state_year)
