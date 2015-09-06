RasterVis.Or.Plot.Map.Water.Balances <-
function(Crop, mm = FALSE, rainfed = FALSE, type = c('annual', 'seasonal'), Raster.Vis = TRUE, metric = FALSE){
  
  if (mm == TRUE) Pat <- 'mm'
  if (mm == FALSE) Pat <- 'Total'
  if (rainfed == TRUE) Irr <- 'rainfed'
  if (rainfed == FALSE) Irr <- 'irrigated'
  
  Final <- brick(paste0(Intermediates, Pat, '.', type, ".WB.", Irr, ".", Crop,'.grd'))  
  # Doesn't do BW / GW yet
  Final[Final == 0] <- NA
  
  print('Final stats:')
  print(cellStats(Final, summary))
  plot(Final)
  names(Final)[which(names(Final) == 'GW.Infiltration')] <- 'Groundwater Infiltration'
  
  if (mm == FALSE){
    if (metric == FALSE){
      # 1 mm --> 10 m3/ha # 1 cubic meter = 0.000810713194 acre foot
      # CONVERSIONS: acres -> hectares; mm -> m3; m3 <- acre-feet
      # equivalently: 1 mm x acre == 0.0032808399 acre-foot
      Final <- Final*0.0032808399
      Final <- Final/10^3
      Subtitle <- "water balances in thousand acre-feet" 
      Type <- 'Acre-feet'
    }
    if (metric == TRUE){
      # 1 mm --> 10 m3/ha # 1 cubic meter = 0.000810713194 acre foot
      # CONVERSIONS: acres -> hectares; mm -> m3
      # equivalently: 1 millimeter acre = 0.000404685642 hectare meters 
      Final <- Final*0.000404685642
      Final <- Final/10^3
      Subtitle <- "water balances in thousand hectare-meters" 
      Type <- 'Hectare-meters'      
    }  
  }
  if (mm == TRUE){
    Subtitle <- "Water balances in mm"
    Type <- 'mm'
  }
  
  setwd(paste0(Path, '/CropWatR/Intermediates/'))    
  
  if (Raster.Vis == TRUE){
    if (mm == FALSE){
      my.ckey <- list(labels = list(cex = 1.25), col=GnYlRdTheme$regions$col, space = 'left')
      Type <- 'Total.WB'
    }
    if (mm == TRUE){
      my.ckey <- list(labels = list(cex = 1.25), col=GnYlRdTheme$regions$col, space = 'right')
      Subtitle <- "water balances in mm"
      Type <- 'mm'
    }
    
    ### Individual plots:

    png(filename = paste("Transpiration", Crop,  Type, type, Irr, "png", sep = "."),width = 300, height = 200)
    par(mai = c(0.1, 0.1, 0.1, 0.1), mar = c(0.1, 0.1, 0.1, 0.1), oma = c(0,0,0,0))
    p <- levelplot(raster(Final, layer = 1), cex = 1.15, border = "transparent", scales=list(draw=FALSE), margin = FALSE, # zscaleLog=TRUE, 
                   contour = FALSE, par.settings=GnYlRdTheme, colorkey=my.ckey) # at=my.at, colorkey=my.ckey, 
    p <- p + layer(sp.lines(SL.aeaCounties, lwd=0.05, col='gray'))
    p <- p + layer(sp.lines(SL.aeaStates, lwd=0.08, col='darkgray'))
    p <- p + layer(sp.lines(SL.aeaHuc2, lwd=0.15, col='black'))
    plot(p)
    dev.off() 
    
    png(filename = paste("Evaporation", Crop,  Type, type, Irr, "png", sep = "."),width = 300, height = 200)    
    par(mai = c(0.1, 0.1, 0.1, 0.1), mar = c(0.1, 0.1, 0.1, 0.1), oma = c(0,0,0,0))
    p <- levelplot(raster(Final, layer = 2), cex = 1.15, border = "transparent", scales=list(draw=FALSE), margin = FALSE, # zscaleLog=TRUE, 
                   contour = FALSE, par.settings=GnYlRdTheme, colorkey=my.ckey) # at=my.at, colorkey=my.ckey, 
    p <- p + layer(sp.lines(SL.aeaCounties, lwd=0.05, col='gray'))
    p <- p + layer(sp.lines(SL.aeaStates, lwd=0.08, col='darkgray'))
    p <- p + layer(sp.lines(SL.aeaHuc2, lwd=0.15, col='black'))
    plot(p)
    dev.off() 
    
    png(filename = paste("Runoff", Crop,  Type, type, Irr, "png", sep = "."),width = 300, height = 200)    
    par(mai = c(0.1, 0.1, 0.1, 0.1), mar = c(0.1, 0.1, 0.1, 0.1), oma = c(0,0,0,0))
    p <- levelplot(raster(Final, layer = 3), cex = 1.15, border = "transparent", scales=list(draw=FALSE), margin = FALSE, # zscaleLog=TRUE, 
                   contour = FALSE, par.settings=GnYlRdTheme, colorkey=my.ckey) # at=my.at, colorkey=my.ckey, 
    p <- p + layer(sp.lines(SL.aeaCounties, lwd=0.05, col='gray'))
    p <- p + layer(sp.lines(SL.aeaStates, lwd=0.08, col='darkgray'))
    p <- p + layer(sp.lines(SL.aeaHuc2, lwd=0.15, col='black'))
    plot(p)
    dev.off() 
    
    png(filename = paste("GW.Infiltration", Crop,  Type, type, Irr, "png", sep = "."),width = 300, height = 200)
    par(mai = c(0.1, 0.1, 0.1, 0.1), mar = c(0.1, 0.1, 0.1, 0.1), oma = c(0,0,0,0))
    p <- levelplot(raster(Final, layer = 4), cex = 1.15, border = "transparent", scales=list(draw=FALSE), margin = FALSE, # zscaleLog=TRUE, 
                   contour = FALSE, par.settings=GnYlRdTheme, colorkey=my.ckey) # at=my.at, colorkey=my.ckey, 
    p <- p + layer(sp.lines(SL.aeaCounties, lwd=0.05, col='gray'))
    p <- p + layer(sp.lines(SL.aeaStates, lwd=0.08, col='darkgray'))
    p <- p + layer(sp.lines(SL.aeaHuc2, lwd=0.15, col='black'))
    plot(p)
    dev.off() 
    
    if (rainfed == FALSE){
      png(filename = paste("Irrigation", Crop,  Type, type, Irr, "png", sep = "."),width = 300, height = 200)
      par(mai = c(0.1, 0.1, 0.1, 0.1), mar = c(0.1, 0.1, 0.1, 0.1), oma = c(0,0,0,0))
      p <- levelplot(raster(Final, layer = 5), cex = 1.15, border = "transparent", scales=list(draw=FALSE), margin = FALSE, # zscaleLog=TRUE, 
                     contour = FALSE, par.settings=GnYlRdTheme, colorkey=my.ckey) # at=my.at, colorkey=my.ckey, 
      p <- p + layer(sp.lines(SL.aeaCounties, lwd=0.05, col='gray'))
      p <- p + layer(sp.lines(SL.aeaStates, lwd=0.08, col='darkgray'))
      p <- p + layer(sp.lines(SL.aeaHuc2, lwd=0.15, col='black'))
      plot(p)
      dev.off() 
      
    }
    
  } 
  if (Raster.Vis == FALSE){
    aeaHuc2 <- shapefile('aea.HUC2.bounds.shp')
    aeaHuc2$REG_NAME <- strsplit(aeaHuc2$REG_NAME, " Region")
    
    ######### First cut on the total water balance maps:    
    pdf(filename = paste("BrickPlot", Type, type, Irr, Crop, "pdf"), width = 8, height = 2)
    par(mfrow = c(1, 5), mai = c(0, 0.1, 0, 0.8), mar = c(0, 0.1, 0, 5.5)) #, oma = c(0.3,0.1,0.6,0.5))
    
    plot(raster(Final, layer = 1), axes = FALSE, box = FALSE, main="Transpiration")
    par(bg="transparent")  
    plot(aeaHuc2, border="black", col="transparent", lwd = 0.25, add=TRUE)
    text(aeaHuc2, labels='REG_NAME', col="black", font=2, cex = .70)
    
    plot(raster(Final, layer = 2), axes = FALSE, box = FALSE, main="Evaporation")
    par(bg="transparent")  
    plot(aeaHuc2, border="black", col="transparent", lwd = 0.25, add=TRUE)
    text(aeaHuc2, labels='REG_NAME', col="black", font=2, cex = .70)
    
    plot(raster(Final, layer = 3), axes = FALSE, box = FALSE, main="Runoff")
    par(bg="transparent")  
    plot(aeaHuc2, border="black", col="transparent", lwd = 0.25, add=TRUE)
    text(aeaHuc2, labels='REG_NAME', col="black", font=2, cex = .70)
    
    plot(raster(Final, layer = 4), axes = FALSE, box = FALSE, main="Groundwater Infiltration")
    par(bg="transparent")  
    plot(aeaHuc2, border="black", col="transparent", lwd = 0.25, add=TRUE)
    text(aeaHuc2, labels='REG_NAME', col="black", font=2, cex = .70)
    
    plot(raster(Final, layer = 5), axes = FALSE, box = FALSE, main="Irrigation")
    par(bg="transparent")  
    plot(aeaHuc2, border="black", col="transparent", lwd = 0.25, add=TRUE)
    text(aeaHuc2, labels='REG_NAME', col="black", font=2, cex = .70)
    
    mtext(Subtitle, side=1, outer=TRUE, line=-3, cex = 1.25)
    dev.off()  
  }
  
  setwd(paste0(Path, '/CropWatR/Data'))
  
}
