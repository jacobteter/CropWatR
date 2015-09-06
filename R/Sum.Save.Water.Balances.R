Sum.Save.Water.Balances <-
function(Croplayer, rainfed = FALSE, type = c('seasonal', 'annual'), BW.GW = FALSE){
  
  setwd(paste0(Path, '/CropWatR/Intermediates/'))
  
  if (rainfed == FALSE && type == 'annual'){
    ####### I.  GW.infiltration:
    load(paste('Preseason_Deep.Percolation', Croplayer, 'Rdata', sep = '.')) # Pre.DP
    load(paste('Growing.Season_Deep.Percolation', Croplayer, 'Rdata', sep = '.')) # DP
    Post.DP <- local(get(load(paste('Postseason_Deep.Percolation', Croplayer,'Rdata', sep = '.')))) # Post.DP  
    Pre.GW.Infiltration <- unlist(lapply(Pre.DP, function(x) rowSums(x[,(grep('layer', names(x)))])))
    GS.GW.Infiltration <- unlist(lapply(DP, function(x) rowSums(x[,(grep('layer', names(x)))])))
    Post.GW.Infiltration <- unlist(lapply(Post.DP, function(x) rowSums(x[,(grep('layer', names(x)))])))
    GW.Infiltration <- rowSums(as.data.frame(cbind(Pre.GW.Infiltration, GS.GW.Infiltration, Post.GW.Infiltration)))
    print(paste("Infiltration Summary for", Croplayer))
    print(c(summary(Pre.GW.Infiltration), summary(GS.GW.Infiltration), summary(Post.GW.Infiltration), summary(GW.Infiltration)))
    
    ####### II.  Evaporation:  
    load(paste('Preseason_Soil.Evaporation', Croplayer, 'Rdata', sep = '.')) # Pre.KeETo
    load(paste('Growing.Season_Soil.Evaporation', Croplayer, 'Rdata', sep = '.')) # E
    Post.KeETo <- local(get(load(paste('Postseason_Soil.Evaporation', Croplayer, 'Rdata', sep = '.')))) # Post.KeETo
    Pre.Evap <- unlist(lapply(Pre.KeETo, function(x) rowSums(x[,(grep('layer', names(x)))])))
    Evap <- unlist(lapply(E, function(x) rowSums(x[,(grep('layer', names(x)))])))
    Post.Evap <- unlist(lapply(Post.KeETo, function(x) rowSums(x[,(grep('layer', names(x)))])))  
    Evaporation <- rowSums(as.data.frame(cbind(Pre.Evap, Evap, Post.Evap)))
    print(paste("Evaporation Summary for", Croplayer))
    
    load(paste('Preseason_Weed.Transpiration', Croplayer, 'Rdata', sep = '.')) # Pre.Kcb.tot
    Pre.weed.Kcb <- unlist(lapply(Pre.Kcb.tot, function(x) rowSums(x[,(grep('layer', names(x)))])))
    Post.Kcb.tot <- local(get(load(paste('Postseason_Weed.Transpiration', Croplayer, 'Rdata', sep = '.')))) # Post.Kcb.tot
    Post.weed.Kcb <- unlist(lapply(Post.Kcb.tot, function(x) rowSums(x[,(grep('layer', names(x)))])))
    
    Evapor.Fallow.Transpir <- rowSums(as.data.frame(cbind(Pre.Evap, Pre.weed.Kcb, Evap, Post.Evap, Post.weed.Kcb)))
    print(c(summary(Pre.Evap), summary(Evap), summary(Post.Evap), summary(Evaporation)))
    
    ####### III.  Runoff:    
    load(paste('Preseason_Runoff', Croplayer, 'Rdata', sep = '.')) # Pre.ROi
    load(paste('Growing.Season_Runoff', Croplayer, 'Rdata', sep = '.')) # ROi 
    Post.ROi <- local(get(load(paste('Postseason_Runoff', Croplayer, 'Rdata', sep = '.')))) # Post.ROi
    Pre.runoff <- unlist(lapply(Pre.ROi, function(x) rowSums(x[,(grep('layer', names(x)))])))
    runoff <- unlist(lapply(ROi, function(x) rowSums(x[,(grep('layer', names(x)))])))
    Post.runoff <- unlist(lapply(Post.ROi, function(x) rowSums(x[,(grep('layer', names(x)))])))
    Runoff <- rowSums(as.data.frame(cbind(Pre.runoff, runoff, Post.runoff)))  
    print(paste("Runoff Summary for", Croplayer))
    print(c(summary(Pre.runoff), summary(runoff), summary(Post.runoff), summary(Runoff)))
    
    ####### IV.  Crop Transpiration:
    load(file = paste('Growing.Season_Transpiration', Croplayer, 'Rdata', sep = '.')) # Transp.final
    Transpiration <- unlist(lapply(Transp.final, function(x) rowSums(x[,(grep('layer', names(x)))])))
    print(paste("Transpiration Summary for", Croplayer))
    print(summary(Transpiration))
    
    ## V. Weed Transpiration:
    load(paste('Preseason_Weed.Transpiration', Croplayer, 'Rdata', sep = '.')) # Pre.Kcb.tot
    Post.Kcb.tot <- local(get(load(paste('Postseason_Weed.Transpiration', Croplayer, 'Rdata', sep = '.')))) # Post.Kcb.tot
    Pre.weed.Kcb <- unlist(lapply(Pre.Kcb.tot, function(x) rowSums(x[,(grep('layer', names(x)))])))
    Post.weed.Kcb <- unlist(lapply(Post.Kcb.tot, function(x) rowSums(x[,(grep('layer', names(x)))])))
    Fallow.Transpiration <- rowSums(as.data.frame(cbind(Pre.weed.Kcb, Post.weed.Kcb)))
    print(paste("Weed Evaporation Summary for", Croplayer))
    print(c(summary(Pre.weed.Kcb), summary(Post.weed.Kcb), summary(Fallow.Transpiration)))
    
    ####### V. Irrigation:
    load(file = paste('Growing.Season_Irrigation', Croplayer, 'Rdata', sep = '.')) # Irr
    print(paste("Irrigation Summary for", Croplayer))
    Irrigation <- unlist(lapply(Irr, function(x) rowSums(x[,(grep('layer', names(x)))])))#
    Irrigate <- Irrigation
    Irrigate[Irrigate == 0] <- NA
    print(summary(Irrigate))
    
    ##### VI. Create long dataframe of coordinates:  
    load(paste("BASE", Croplayer, 'MNRH_', 'MasterDF2', sep = '.')) # DF2
    IDs.1 <- as.numeric(rownames(DF2)) # as.numeric is crucial
    Coords <- cbind(DF2$x, DF2$y)
    Coords <- as.data.frame(cbind(IDs.1, Coords))
    IDs.2 <- as.numeric(unlist(lapply(Pre.DP, function(x) rownames(x)))) # as.numeric is crucial
    table(IDs.2 %in% IDs.1)
    
    Water.Balance <- as.data.frame(cbind(IDs.2, Transpiration, Evapor.Fallow.Transpir, Runoff, GW.Infiltration, Irrigation))  
  }
  
  if (rainfed == TRUE && type == 'annual'){
    
    ####### I.  GW.infiltration:
    load(paste('Preseason_Deep.Percolation', Croplayer, 'Rdata', sep = '.')) # Pre.DP
    load(paste('Growing.Season.Rainfed_Deep.Percolation', Croplayer, 'Rdata', sep = '.')) # DP
    Post.DP <- local(get(load(paste('Postseason_Deep.Percolation', Croplayer,'Rdata', sep = '.')))) # Post.DP  
    Pre.GW.Infiltration <- unlist(lapply(Pre.DP, function(x) rowSums(x[,(grep('layer', names(x)))])))
    GS.GW.Infiltration <- unlist(lapply(DP, function(x) rowSums(x[,(grep('layer', names(x)))])))
    Post.GW.Infiltration <- unlist(lapply(Post.DP, function(x) rowSums(x[,(grep('layer', names(x)))])))
    GW.Infiltration <- rowSums(as.data.frame(cbind(Pre.GW.Infiltration, GS.GW.Infiltration, Post.GW.Infiltration)))
    print(paste("Infiltration Summary for", Croplayer))
    print(c(summary(Pre.GW.Infiltration), summary(GS.GW.Infiltration), summary(Post.GW.Infiltration), summary(GW.Infiltration)))
    
    ####### II.  Evaporation:    
    load(paste('Preseason_Soil.Evaporation', Croplayer, 'Rdata', sep = '.')) # Pre.KeETo
    load(paste('Growing.Season.Rainfed_Soil.Evaporation', Croplayer, 'Rdata', sep = '.')) # E
    Post.KeETo <- local(get(load(paste('Postseason_Soil.Evaporation', Croplayer, 'Rdata', sep = '.')))) # Post.KeETo  
    Pre.Evap <- unlist(lapply(Pre.KeETo, function(x) rowSums(x[,(grep('layer', names(x)))])))
    Evap <- unlist(lapply(E, function(x) rowSums(x[,(grep('layer', names(x)))])))
    Post.Evap <- unlist(lapply(Post.KeETo, function(x) rowSums(x[,(grep('layer', names(x)))])))
    
    load(paste('Preseason_Weed.Transpiration', Croplayer, 'Rdata', sep = '.')) # Pre.Kcb.tot
    Pre.weed.Kcb <- unlist(lapply(Pre.Kcb.tot, function(x) rowSums(x[,(grep('layer', names(x)))])))
    Post.Kcb.tot <- local(get(load(paste('Postseason_Weed.Transpiration', Croplayer, 'Rdata', sep = '.')))) # Post.Kcb.tot
    Post.weed.Kcb <- unlist(lapply(Post.Kcb.tot, function(x) rowSums(x[,(grep('layer', names(x)))])))
    
    Evapor.Fallow.Transpir <- rowSums(as.data.frame(cbind(Pre.Evap, Pre.weed.Kcb, Evap, Post.Evap, Post.weed.Kcb)))
    
    Evaporation <- rowSums(as.data.frame(cbind(Pre.Evap, Evap, Post.Evap)))
    print(paste("Evaporation Summary for", Croplayer))
    print(c(summary(Pre.Evap), summary(Evap), summary(Post.Evap), summary(Evaporation)))
    
    ####### III.  Runoff:    
    load(paste('Preseason_Runoff', Croplayer, 'Rdata', sep = '.')) # Pre.ROi
    load(paste('Growing.Season.Rainfed_Runoff', Croplayer, 'Rdata', sep = '.')) # ROi 
    Post.ROi <- local(get(load(paste('Postseason_Runoff', Croplayer, 'Rdata', sep = '.')))) # Post.ROi
    Pre.runoff <- unlist(lapply(Pre.ROi, function(x) rowSums(x[,(grep('layer', names(x)))])))
    runoff <- unlist(lapply(ROi, function(x) rowSums(x[,(grep('layer', names(x)))])))
    Post.runoff <- unlist(lapply(Post.ROi, function(x) rowSums(x[,(grep('layer', names(x)))])))
    Runoff <- rowSums(as.data.frame(cbind(Pre.runoff, runoff, Post.runoff)))  
    print(paste("Runoff Summary for", Croplayer))
    print(c(summary(Pre.runoff), summary(runoff), summary(Post.runoff), summary(Runoff)))
    
    ####### IV.  Transpiration:
    load(file = paste('Growing.Season.Rainfed_Transpiration', Croplayer, 'Rdata', sep = '.')) # Transp.final
    Transpiration <- unlist(lapply(Transp.final, function(x) rowSums(x[,(grep('layer', names(x)))])))
    print(paste("Transpiration Summary for", Croplayer))
    print(summary(Transpiration))
    
    ####### V. Weed Transpiration:
    load(paste('Preseason_Weed.Transpiration', Croplayer, 'Rdata', sep = '.')) # Pre.Kcb.tot
    Post.Kcb.tot <- local(get(load(paste('Postseason_Weed.Transpiration', Croplayer, 'Rdata', sep = '.')))) # Post.Kcb.tot
    Pre.weed.Kcb <- unlist(lapply(Pre.Kcb.tot, function(x) rowSums(x[,(grep('layer', names(x)))])))
    Post.weed.Kcb <- unlist(lapply(Post.Kcb.tot, function(x) rowSums(x[,(grep('layer', names(x)))])))
    Fallow.Transpiration <- rowSums(as.data.frame(cbind(Pre.weed.Kcb, Post.weed.Kcb)))
    print(paste("Weed Transpiration Summary for", Croplayer))
    print(c(summary(Pre.weed.Kcb), summary(Post.weed.Kcb), summary(Fallow.Transpiration)))
    
    ####### V. Irrigation: NA
    
    #   ##### VI. Create long dataframe of coordinates:  
    load(paste("BASE", Croplayer, 'MNRH_', 'MasterDF2', sep = '.')) # DF2
    IDs.1 <- as.numeric(rownames(DF2)) # as.numeric is crucial
    Coords <- cbind(DF2$x, DF2$y)
    Coords <- as.data.frame(cbind(IDs.1, Coords))
    IDs.2 <- as.numeric(unlist(lapply(Pre.DP, function(x) rownames(x)))) # as.numeric is crucial
    table(IDs.2 %in% IDs.1)
    
    Water.Balance <- as.data.frame(cbind(IDs.2, Transpiration, Evapor.Fallow.Transpir, Runoff, GW.Infiltration))
    
    
  }
  
  if (rainfed == FALSE && type == 'seasonal'){
    
    ####### I.  GW.infiltration:
    load(paste('Growing.Season_Deep.Percolation', Croplayer, 'Rdata', sep = '.')) # DP
    GS.GW.Infiltration <- unlist(lapply(DP, function(x) rowSums(x[,(grep('layer', names(x)))])))
    print(paste("Infiltration Summary for", Croplayer))
    print(summary(GS.GW.Infiltration))
    
    ####### II.  Evaporation:  
    load(paste('Growing.Season_Soil.Evaporation', Croplayer, 'Rdata', sep = '.')) # KeETo
    Evap <- unlist(lapply(E, function(x) rowSums(x[,(grep('layer', names(x)))])))
    print(paste("Evaporation Summary for", Croplayer))
    print(summary(Evap))
    
    # all.equal(lapply(Few, function(x) dim(x)), lapply(KeETo, function(x) dim(x)))
    
    ####### III.  Runoff:    
    load(paste('Growing.Season_Runoff', Croplayer, 'Rdata', sep = '.')) # ROi 
    runoff <- unlist(lapply(ROi, function(x) rowSums(x[,(grep('layer', names(x)))])))
    print(paste("Runoff Summary for", Croplayer))
    print(summary(runoff))
    
    ####### IV.  Transpiration:
    load(file = paste('Growing.Season_Transpiration', Croplayer, 'Rdata', sep = '.')) # Transp.final
    Transpiration <- unlist(lapply(Transp.final, function(x) rowSums(x[,(grep('layer', names(x)))])))
    print(paste("Transpiration Summary for", Croplayer))
    print(summary(Transpiration))
    
    ####### V. Irrigation:
    load(file = paste('Growing.Season_Irrigation', Croplayer, 'Rdata', sep = '.')) # Irr
    print(paste("Irrigation Summary for", Croplayer))
    Irrigation <- unlist(lapply(Irr, function(x) rowSums(x[,(grep('layer', names(x)))])))#
    Irrigate <- Irrigation
    Irrigate[Irrigate == 0] <- NA
    print(summary(Irrigate))
    
    #   ##### VI. Create long dataframe of coordinates:  
    load(paste("BASE", Croplayer, 'MNRH_', 'MasterDF2', sep = '.')) # DF2
    IDs.1 <- as.numeric(rownames(DF2)) # as.numeric is crucial
    Coords <- cbind(DF2$x, DF2$y)
    Coords <- as.data.frame(cbind(IDs.1, Coords))
    IDs.2 <- as.numeric(unlist(lapply(DP, function(x) rownames(x)))) # as.numeric is crucial
    table(IDs.2 %in% IDs.1)
    
    if (BW.GW == FALSE) Water.Balance <- as.data.frame(cbind(IDs.2, Transpiration, Evap, runoff,  GS.GW.Infiltration, Irrigation))
    
    
    if (BW.GW == TRUE){
      GreenWater <- Transpiration + Evap
      BlueWater <- Irrigation
      Water.Balance <- as.data.frame(cbind(IDs.2, GreenWater, BlueWater))
    }     
  }
  
  if (rainfed == TRUE && type == 'seasonal'){
    
    ####### I.  GW.infiltration:
    load(paste('Growing.Season.Rainfed_Deep.Percolation', Croplayer, 'Rdata', sep = '.')) # DP
    GS.GW.Infiltration <- unlist(lapply(DP, function(x) rowSums(x[,(grep('layer', names(x)))])))
    print(paste("Infiltration Summary for", Croplayer))
    print(summary(GS.GW.Infiltration))
    
    ####### II.  Evaporation:  
    load(paste('Growing.Season.Rainfed_Soil.Evaporation', Croplayer, 'Rdata', sep = '.')) # E
    Evap <- unlist(lapply(E, function(x) rowSums(x[,(grep('layer', names(x)))])))
    print(paste("Evaporation Summary for", Croplayer))
    print(summary(Evap))
    
    # all.equal(lapply(Few, function(x) dim(x)), lapply(KeETo, function(x) dim(x)))
    
    ####### III.  Runoff:    
    load(paste('Growing.Season.Rainfed_Runoff', Croplayer, 'Rdata', sep = '.')) # ROi 
    runoff <- unlist(lapply(ROi, function(x) rowSums(x[,(grep('layer', names(x)))])))
    print(paste("Runoff Summary for", Croplayer))
    print(summary(runoff))
    
    ####### IV.  Transpiration:
    load(file = paste('Growing.Season.Rainfed_Transpiration', Croplayer, 'Rdata', sep = '.')) # Transp.final
    Transpiration <- unlist(lapply(Transp.final, function(x) rowSums(x[,(grep('layer', names(x)))])))
    print(paste("Transpiration Summary for", Croplayer))
    print(summary(Transpiration))
    
    #   ##### VI. Create long dataframe of coordinates:  
    load(paste("BASE", Croplayer, 'MNRH_', 'MasterDF2', sep = '.')) # DF2
    IDs.1 <- as.numeric(rownames(DF2)) # as.numeric is crucial
    Coords <- cbind(DF2$x, DF2$y)
    Coords <- as.data.frame(cbind(IDs.1, Coords))
    IDs.2 <- as.numeric(unlist(lapply(DP, function(x) rownames(x)))) # as.numeric is crucial
    table(IDs.2 %in% IDs.1)
    
    if (BW.GW == FALSE) Water.Balance <- as.data.frame(cbind(IDs.2, Transpiration, Evap, runoff,  GS.GW.Infiltration))
    
    
    if (BW.GW == TRUE){
      GreenWater <- Transpiration + Evap
      BlueWater <- Irrigation
      Water.Balance <- as.data.frame(cbind(IDs.2, GreenWater, BlueWater))
    }     
  }
  
  print(table(Coords$IDs.1 %in% Water.Balance$IDs.2))
  Water.Balance <- merge(Coords, Water.Balance, by.x = 'IDs.1', by.y = 'IDs.2')
  names(Water.Balance)[1:3] <- c('IDs', 'x', 'y')
  Water.Balance[Water.Balance == 0] <- NA
  Water.Balance <- Water.Balance[,-1]
  coordinates(Water.Balance) <- ~x+y
  proj4string(Water.Balance) <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  gridded(Water.Balance) = TRUE
  WB.brick <- brick(Water.Balance)
  projection(WB.brick) <- ("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  Crops.brick.2008 <- brick('../Data/cdl_10k_2008_albers.grd')
  
  WB.brick <- extend(WB.brick, Crops.brick.2008)
  plot(WB.brick)
  print(cellStats(WB.brick, summary))
  
  if (rainfed == FALSE && type == 'annual') writeRaster(WB.brick, filename = paste(Croplayer, 'Basic.WB.grd', sep = '.'), overwrite = TRUE)
  if (rainfed == FALSE && type == 'seasonal') writeRaster(WB.brick, filename = paste(Croplayer, 'Growing.Season.WB.grd', sep = '.'), overwrite = TRUE)
  
  if (rainfed == TRUE && type == 'annual') writeRaster(WB.brick, filename = paste(Croplayer, 'Basic.Rainfed.WB.grd', sep = '.'), overwrite = TRUE)
  if (rainfed == TRUE && type == 'seasonal') writeRaster(WB.brick, filename = paste(Croplayer, 'Growing.Season.Rainfed.WB.grd', sep = '.'), overwrite = TRUE)
  
  if (BW.GW == FALSE && type == 'annual') writeRaster(WB.brick, filename = paste(Croplayer, 'Growing.Season.WB.grd', sep = '.'), overwrite = TRUE)    
  if (BW.GW == TRUE && type == 'seasonal') writeRaster(WB.brick, filename = paste(Croplayer, 'Growing.Season.GW.BW.WB.grd', sep = '.'), overwrite = TRUE)    
  if (BW.GW == TRUE && type != 'seasonal') print('Blue / Green water volumes only relevant for the growing season')
  
  setwd(paste0(Path, '/CropWatR/Data'))
  
}
