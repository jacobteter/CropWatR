SuperImpose.WB.on.LU <-
function(Croplayer, rainfed = FALSE, type = c('seasonal', 'annual'), Growing.Season.GW.BW = FALSE){
  load('Vars.Rdata')
  Irr.Vars <- Vars[-c(3,6,8,14,15)]
  if (Growing.Season.GW.BW == TRUE) class <- 'BW.GW'
  if (Growing.Season.GW.BW == FALSE) class <- 'WB'
  if (rainfed == FALSE) Irr <- 'irrigated'
  if (rainfed == TRUE) Irr <- 'rainfed'
  
  if (Croplayer %in% Irr.Vars){
    
    LU.brick <- raster(paste0(Intermediates, Croplayer, '.grd'))
    LU.brick[LU.brick == 0] <- NA    
    
    if (rainfed == FALSE && type == 'annual') WB.brick <- brick(paste0(Intermediates, Croplayer, '.Basic.WB.grd'))
    if (rainfed == FALSE && type == 'seasonal') WB.brick <- brick(paste0(Intermediates, Croplayer, '.Growing.Season.WB.grd'))
    if (rainfed == TRUE && type == 'annual') WB.brick <- brick(paste0(Intermediates, Croplayer, '.Basic.Rainfed.WB.grd'))
    if (rainfed == TRUE && type == 'seasonal') WB.brick <- brick(paste0(Intermediates, Croplayer, '.Growing.Season.Rainfed.WB.grd'))
    if (Growing.Season.GW.BW == FALSE && type == 'annual')  WB.brick <- brick(paste0(Intermediates, Croplayer, '.Basic.WB.grd')) 
    if (Growing.Season.GW.BW == TRUE && type == 'seasonal') WB.brick <- brick(paste0(Intermediates, Croplayer, '.Growing.Season.GW.BW.WB.grd'))    
    if (Growing.Season.GW.BW == TRUE && type == 'seasonal' && rainfed == FALSE) WB.brick <- brick(paste0(Intermediates, Croplayer, '.Growing.Season.BW.GW.WB.grd'))
    
    Names <- names(WB.brick)
    
    WB.brick <- extend(WB.brick, LU.brick)
    LU.brick <- extend(LU.brick, WB.brick)
    LU.brick <- extend(LU.brick, WB.brick)
    
    WB.brick <- calc(WB.brick, fun = function(x) replace(x, x < 0, 0.001))
    
    WB.total <- overlay(WB.brick, LU.brick, fun=prod)
    names(WB.total) <- names(WB.brick)
    WB.total[WB.total == 0] <- NA
    
    # mm treatment:
    LU.mm <- LU.brick
    LU.mm[LU.mm > 0] <- 1 
    # mask by land use
    WB.mm <- overlay(WB.brick, LU.mm, fun=prod)
    names(WB.mm) <- names(WB.brick)    
    
    print(paste('saving', Croplayer))
    print(cellStats(WB.total, summary))
    print(cellStats(WB.mm, summary))
    
    writeRaster(WB.total, filename = paste0(Intermediates, 'Total.', type, ".", class, ".", Irr, ".", Croplayer,'.grd'), overwrite = TRUE)
    writeRaster(WB.mm, filename = paste0(Intermediates, 'mm.', type, ".", class, ".", Irr, ".", Croplayer, '.grd'), overwrite = TRUE)  
    
  }
  
  if(!(Croplayer %in% Irr.Vars)){
    LU.brick <- raster(paste0(Intermediates, Croplayer, '.grd'))
    LU.brick[LU.brick == 0] <- NA

    if (rainfed == FALSE && type == 'annual') WB.brick <- brick(paste0(Intermediates, Croplayer, '.Basic.WB.grd'))
    if (rainfed == FALSE && type == 'seasonal') WB.brick <- brick(paste0(Intermediates, Croplayer, '.Growing.Season.WB.grd'))
    if (rainfed == TRUE && type == 'annual') WB.brick <- brick(paste0(Intermediates, Croplayer, '.Basic.Rainfed.WB.grd'))
    if (rainfed == TRUE && type == 'seasonal') WB.brick <- brick(paste0(Intermediates, Croplayer, '.Growing.Season.Rainfed.WB.grd'))
    if (Growing.Season.GW.BW == FALSE && type == 'annual')  WB.brick <- brick(paste0(Intermediates, Croplayer, '.Basic.WB.grd')) 
    if (Growing.Season.GW.BW == TRUE && type == 'seasonal') WB.brick <- brick(paste0(Intermediates, Croplayer, '.Growing.Season.GW.BW.WB.grd'))    
    if (Growing.Season.GW.BW == TRUE && type == 'seasonal' && rainfed == FALSE) WB.brick <- brick(paste0(Intermediates, Croplayer, '.Growing.Season.BW.GW.WB.grd'))
    
    Names <- names(WB.brick)
    
    WB.brick <- extend(WB.brick, LU.brick)
    LU.brick <- extend(LU.brick, WB.brick)
    
    WB.brick <- calc(WB.brick, fun = function(x) replace(x, x < 0, 0.001))
    
    WB.total <- overlay(WB.brick, LU.brick, fun=prod)
    names(WB.total) <- names(WB.brick)
    WB.total[WB.total == 0] <- NA
    
    # mm treatment:
    LU.mm <- LU.brick
    LU.mm[LU.mm > 0] <- 1 
    # mask by land use
    WB.mm <- overlay(WB.brick, LU.mm, fun=prod)
    names(WB.mm) <- names(WB.brick)    
    
    print(paste('saving', Croplayer))
    print(cellStats(WB.total, summary))
    print(cellStats(WB.mm, summary))
    
    writeRaster(WB.total, filename = paste0(Intermediates, 'Total.', type, ".", class, ".", Irr, ".", Croplayer,'.grd'), overwrite = TRUE)
    writeRaster(WB.mm, filename = paste0(Intermediates, 'mm.', type, ".", class, ".", Irr, ".", Croplayer, '.grd'), overwrite = TRUE)  
  }  
}
