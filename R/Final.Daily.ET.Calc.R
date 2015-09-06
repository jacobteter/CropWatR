Final.Daily.ET.Calc <-
function(Croplayer){
  if (file.exists(paste0(Intermediates, paste("Preseason_Weed.Transpiration", Croplayer, 'Rdata', sep = '.')))==FALSE){
    Fallow.Preseason.Daily.ET.Calc(Croplayer)
  }
  
  if (file.exists(paste0(Intermediates, paste("Postseason_Weed.Transpiration", Croplayer, 'Rdata', sep = '.')))
      && file.exists(paste0(Intermediates, paste("Growing.Season_Runoff", Croplayer, 'Rdata', sep = '.')))){    
    print(paste('Daily ETo calculation completed for', Croplayer))
  }
  if ((file.exists(paste0(Intermediates, paste("Postseason_Weed.Transpiration", Croplayer, 'Rdata', sep = '.')))
       && file.exists(paste0(Intermediates,paste("Growing.Season_Runoff", Croplayer, 'Rdata', sep = '.')))) == FALSE){    
    # Do the function:
    
    if (file.exists(paste0(Intermediates, paste("Growing.Season_Transpiration", Croplayer, "Rdata", sep = "."))) == FALSE){
      load('Vars.Rdata')
      if (Croplayer %in% Vars || Croplayer == 'silage'){
        Main.Growing.Season.Daily.ET.Calc(Croplayer)
        Main.Rainfed.Growing.Season.Daily.ET.Calc(Croplayer)
      }
      Others <- c('switchgrass', 'miscanthus', "idle_cropland", "pasture_grass")
      if (Croplayer %in% Others){
        Main.Growing.Season.Daily.ET.Calc(Croplayer)
      }
    }
    if(file.exists(paste0(Intermediates, paste('Postseason_Soil.Water.Balance', Croplayer, 'Rdata', sep = '.'))) == FALSE){
      Fallow.Postseason.Daily.ET.Calc(Croplayer)    
    } 
  }
}
