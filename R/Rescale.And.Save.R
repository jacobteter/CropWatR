Rescale.And.Save <-
function(Variable, PH, DataList, Croplayer, Kcb){ 
  # Variables: 'Precip_', 'ETo_', 'U2.final_', 'MNRH_')
  PH.season.breaks <- subset(PH, select = Initial:Late)
  PH[,which(names(PH) == "Initial"):which(names(PH) == "Late")] <- Rescale.Season(PH.season.breaks, PH$Growing_Season)
  deleteCols <- c('Crop', 'Total', 'Region', 'Plant_Date')
  PH <- PH[,-(which(names(PH) %in% deleteCols))]
  
  Daily.Crops.list <- Daily.Crop.Curves(Croplayer, PH$State_Fips, PH[,which(names(PH) == "Initial"):which(names(PH) == "Late")], Kcb[,2:4], Kcb[,5])
  
  Daily.Crops.list <- Daily.Crops.list[order(names(Daily.Crops.list))]

  Growing.Season <- DataList$Growing.Season
  Fallow.Season <- DataList$Fallow.Season
  Growing.Season <- Growing.Season[order(names(Growing.Season))]
  Fallow.Season <- Fallow.Season[order(names(Fallow.Season))]
  
  print('names equal?:')
  print(all.equal(names(Growing.Season), names(Daily.Crops.list)))
  print(all.equal(names(Growing.Season), names(Fallow.Season)))
  print('layer lengths equal?: (expect a "no"')
  print(all.equal(lapply(Growing.Season, function(x) length(grep('layer', names(x)))), 
                  lapply(Daily.Crops.list, nrow)))
  print(cbind(as.numeric(names(Growing.Season)), 
              sapply(Growing.Season, function(x) length(grep('layer', names(x)))), 
              sapply(Daily.Crops.list, nrow)))
  
  #### Correct.Mismatches  
  for (i in 1:length(Growing.Season)){
    while (length(grep('layer', names(Growing.Season[[i]]))) > nrow(Daily.Crops.list[[i]])){
      Growing.Season[[i]] <- Growing.Season[[i]][,-1]
      Fallow.Season[[i]] <- cbind(Growing.Season[[i]][,1], Fallow.Season[[i]])
    }
  }
  
  print('layer lengths equal?:')
  print(all.equal(lapply(Growing.Season, function(x) length(grep('layer', names(x)))), lapply(Daily.Crops.list, nrow)))  
  save(Growing.Season, file = paste0(Intermediates, paste('Growing.Season', Croplayer, Variable, 'Rdata', sep = '.')))
  save(Fallow.Season, file = paste0(Intermediates, paste('Fallow.Season', Croplayer, Variable, 'Rdata', sep = '.')))
}
