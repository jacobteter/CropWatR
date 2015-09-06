Save.Crops.List <-
function(PH, Croplayer, Kcb){ 
  # Variables: 'Precip_', 'ETo_', 'U2.final_', 'MNRH_')
  PH.season.breaks <- subset(PH, select = Initial:Late)
  PH[,which(names(PH) == "Initial"):which(names(PH) == "Late")] <- Rescale.Season(PH.season.breaks, PH$Growing_Season)
  deleteCols <- c('Crop', 'Total', 'Region', 'Plant_Date')
  PH <- PH[,-(which(names(PH) %in% deleteCols))]
  stages <- PH[,which(names(PH) == "Initial"):which(names(PH) == "Late")]
  Daily.Crops.list <- Daily.Crop.Curves(Croplayer, PH$State_Fips, stages, Kcb[,2:4], Kcb[,5])
  Daily.Crops.list <- Daily.Crops.list[order(names(Daily.Crops.list))]
  save(Daily.Crops.list, file = paste0(Intermediates, paste('CropsList', Croplayer, 'Rdata', sep = '.')))
}
