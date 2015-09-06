Sum.Save.Daily.Evapotranspiration <-
function(Croplayer, rainfed = FALSE){
  
  Crop <- Croplayer
  if (Croplayer == 'spring_barley' || Croplayer == 'fall_barley') Crop <- 'barley'
  if (Croplayer == 'spring_oats' || Croplayer == 'fall_oats') Crop <- 'oats'
  print(Croplayer)
  
  setwd(paste0(Path, '/CropWatR/Intermediates/'))    
  
  if (rainfed == FALSE){
    
    
    load(paste('Preseason_Soil.Evaporation', Croplayer, 'Rdata', sep = '.')) # Pre.KeETo
    load(paste('Preseason_Weed.Transpiration', Croplayer, 'Rdata', sep = '.')) # Pre.Kcb.tot
    Pre.Evap <- lapply(Pre.KeETo, function(x) x[,(grep('layer', names(x)))])
    Pre.weed.Kcb <- lapply(Pre.Kcb.tot, function(x) x[,(grep('layer', names(x)))])
    
    load(paste('Growing.Season_Soil.Evaporation', Croplayer, 'Rdata', sep = '.')) # E
    load(file = paste('Growing.Season_Transpiration', Croplayer, 'Rdata', sep = '.')) # Transp.final    
    
    Transpiration <- lapply(Transp.final, function(x) x[,(grep('layer', names(x)))])
    Evap <- lapply(E, function(x) x[,(grep('layer', names(x)))])
    Post.KeETo <- local(get(load(paste('Postseason_Soil.Evaporation', Croplayer, 'Rdata', sep = '.')))) # Post.KeETo
    Post.Kcb.tot <- local(get(load(paste('Postseason_Weed.Transpiration', Croplayer, 'Rdata', sep = '.')))) # Post.Kcb.tot
    Post.Evap <- lapply(Post.KeETo, function(x) x[,(grep('layer', names(x)))])
    Post.weed.Kcb <- lapply(Post.Kcb.tot, function(x) x[,(grep('layer', names(x)))])  
  }
  
  if (rainfed == TRUE){
    load(paste('Preseason_Soil.Evaporation', Croplayer, 'Rdata', sep = '.')) # Pre.KeETo
    load(paste('Preseason_Weed.Transpiration', Croplayer, 'Rdata', sep = '.')) # Pre.Kcb.tot
    Pre.Evap <- lapply(Pre.KeETo, function(x) x[,(grep('layer', names(x)))])
    Pre.weed.Kcb <- lapply(Pre.Kcb.tot, function(x) x[,(grep('layer', names(x)))])
    
    load(paste('Growing.Season.Rainfed_Soil.Evaporation', Croplayer, 'Rdata', sep = '.')) # E
    load(file = paste('Growing.Season.Rainfed_Transpiration', Croplayer, 'Rdata', sep = '.')) # Transp.final    
    
    Transpiration <- lapply(Transp.final, function(x) x[,(grep('layer', names(x)))])
    Evap <- lapply(E, function(x) x[,(grep('layer', names(x)))])
    Post.KeETo <- local(get(load(paste('Postseason_Soil.Evaporation', Croplayer, 'Rdata', sep = '.')))) # Post.KeETo
    Post.Kcb.tot <- local(get(load(paste('Postseason_Weed.Transpiration', Croplayer, 'Rdata', sep = '.')))) # Post.Kcb.tot
    Post.Evap <- lapply(Post.KeETo, function(x) x[,(grep('layer', names(x)))])
    Post.weed.Kcb <- lapply(Post.Kcb.tot, function(x) x[,(grep('layer', names(x)))])  
  }
  
  load(paste("BASE", Croplayer, 'MNRH_', 'MasterDF2', sep = '.')) # DF2
  IDs.1 <- as.numeric(rownames(DF2)) # as.numeric is crucial
  Coords <- cbind(DF2$x, DF2$y)
  Coords <- as.data.frame(cbind(IDs.1, Coords))
  IDs.2 <- as.numeric(unlist(lapply(E, function(x) rownames(x)))) # as.numeric is crucial
  table(IDs.2 %in% IDs.1)
  
  Rows <- as.data.frame(cbind(IDs.2))
  print(table(Coords$IDs.1 %in% Rows$IDs.2))
  Rows.Fin <- merge(Coords, Rows, by.x = 'IDs.1', by.y = 'IDs.2')
  names(Rows.Fin)[1:3] <- c('IDs', 'x', 'y')
  
  PreP <- Pre.Evap; PostP <- Post.Evap; GR.P <- Evap; Final <- Pre.Evap
  
  for (i in 1:length(Pre.Evap)){
    PreP[[i]] <- Pre.Evap[[i]] + Pre.weed.Kcb[[i]]
    GR.P[[i]] <- Evap[[i]] + Transpiration[[i]]
    PostP[[i]] <- Post.Evap[[i]] + Post.weed.Kcb[[i]]
    Final[[i]] <- as.data.frame(cbind(PreP[[i]], GR.P[[i]], PostP[[i]]))      
  }
  
  if (Croplayer == 'durum_wheat' || Croplayer == 'fall_barley'){
    Cut <- names(unlist(lapply(Final, function(x) which(nrow(x) == 0))))
    Fini <- Final[-(which(names(Final) %in% Cut))]
    Fini <- lapply(Final, function(x) x[,1:362])
  }
  if (Croplayer == 'sugarbeets'){
    Cut <- names(unlist(lapply(Final, function(x) which(ncol(x) < 362))))
    Fini <- Final[-(which(names(Final) %in% Cut))]
    lapply(Fini, dim)
  }
  
  if (Croplayer == 'alfalfa'){
    Fini <- lapply(Final, function(x) x[,1:358])
  }
  
  if (Croplayer != 'durum_wheat' && Croplayer != 'alfalfa' && Croplayer != 'fall_barley'){
    Fini <- lapply(Final, function(x) x[,1:362])    
  }
  
  Base <- Fini[[1]]
  for (i in 2:length(Fini)){
    names(Fini[[i]]) <- names(Base)
    Base <- rbind(Base, Fini[[i]])
  }
  Base$IDs <- as.numeric(rownames(Base))
  print(table(as.numeric(rownames(Base)) %in% Rows.Fin$IDs))
  
  Water.Balance <- merge(Rows.Fin, Base, by = 'IDs', all.y = TRUE)
  WB <- Water.Balance[,-c(1:3)]; Identifiers <- Water.Balance[,c(1:3)]
  Water.Balance <- Water.Balance[,-1]
  coordinates(Water.Balance) <- ~x+y
  proj4string(Water.Balance) <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  gridded(Water.Balance) = TRUE
  WB.brick <- brick(Water.Balance)
  projection(WB.brick) <- ("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  Crops.brick.2008 <- brick('cdl_10k_2008_albers.grd')
  WB.brick <- extend(WB.brick, Crops.brick.2008)
  print(cellStats(WB.brick, summary))
  names(WB.brick) <- gsub('layer', 'day', names(WB.brick))
  
  if (Croplayer != 'pasture_grass'){
    if (file.exists(paste0(Croplayer,'.grd'))){
      LU.brick <- brick(paste0(Croplayer,'.grd'))
    }
    WB.brick[is.na(LU.brick)] <- NA    
  }
  setwd(paste0(Path, '/CropWatR/Data'))
  
  if (rainfed == FALSE) writeRaster(WB.brick, filename = paste(Croplayer, 'Daily.ET.grd', sep = '.'), overwrite = TRUE)    
  if (rainfed == TRUE) writeRaster(WB.brick, filename = paste(Croplayer, 'Rainfed.Daily.ET.grd', sep = '.'), overwrite = TRUE) 
}
