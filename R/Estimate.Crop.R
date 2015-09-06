Estimate.Crop <-
function(crop){

  Subset.Growth.Season(crop)
  print(paste('Seasons subsetted and rescaled for', crop))
  Daily.Crop.Parameters(crop)
  print(paste('Daily crop parameters estimated for', crop))
  Final.Daily.ET.Calc(crop)
  print(paste('Daily crop water balance estimated for', crop))
  
  Sum.Save.Daily.Evapotranspiration(crop, rainfed = TRUE)
  Sum.Save.Daily.Evapotranspiration(crop, rainfed = FALSE)
  print(paste('Daily ET rasters made for', crop))
    
  Sum.Save.Water.Balances(crop, rainfed = FALSE, type = 'seasonal', BW.GW = FALSE)
  Sum.Save.Water.Balances(crop, rainfed = FALSE, type = 'annual', BW.GW = FALSE)
  Sum.Save.Water.Balances(crop, rainfed = TRUE, type = 'seasonal', BW.GW = FALSE)
  Sum.Save.Water.Balances(crop, rainfed = TRUE, type = 'annual', BW.GW = FALSE)
  Sum.Save.Water.Balances(crop, rainfed = FALSE, type = 'seasonal', BW.GW = TRUE)  

  Generate.Land.Use(crop)
  print(paste('land use raster generated for', crop))
  
  print(paste('Annual and seasonal water balance rasters saved for', crop))
  
  SuperImpose.WB.on.LU(crop, rainfed = FALSE, type = 'seasonal', Growing.Season.GW.BW = FALSE)
  SuperImpose.WB.on.LU(crop, rainfed = FALSE, type = 'annual', Growing.Season.GW.BW = FALSE)
  SuperImpose.WB.on.LU(crop, rainfed = TRUE, type = 'seasonal', Growing.Season.GW.BW = FALSE)
  SuperImpose.WB.on.LU(crop, rainfed = TRUE, type = 'annual', Growing.Season.GW.BW = FALSE)
  SuperImpose.WB.on.LU(crop, rainfed = FALSE, type = 'seasonal', Growing.Season.GW.BW = TRUE)  
  
  print(paste('Water balances superimposed on land use for', crop))
  
  
}
