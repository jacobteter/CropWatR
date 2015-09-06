Percent.Change.Perspective.Plot <-
function(Raster, Country, ColorScheme = list('terrain', 'heat', 'topo', 'cm'), Save = TRUE){
  # Plots a pretty overhead of the population count/density 
  # ColorSchemes are terrain, heat, topo, cm
  Pop3D <- Raster
  # summary.pop <- summary(Pop)
    
  zData <-round(as.matrix(Pop3D),1)
  # zData <- zData[c(50:540), c(50:300)]
  x <- 1:nrow(zData)
  y <- 1:ncol(zData)
  
  # z <- getValues(Pop, 1:ncell(Pop))
  
  nrz <- nrow(zData)
  ncz <- ncol(zData)
  
  if (Save == FALSE) quartz(width = 12, height = 9)
  # if (Save == TRUE && log == TRUE) png(filename = paste(Country, ColorScheme, Log, 'png', sep = '.'), width = round(ncz*0.7), height = round(nrz*0.35), bg = 'white')
  # if (Save == TRUE && log == FALSE) png(filename = paste(Country, ColorScheme, Log, 'png', sep = '.'), width = round(ncz*0.4), height = round(nrz*0.35), bg = 'white')
  DissDir <- '/Users/jacobteter/Desktop/Dissertation/'
  
  
  png(filename = paste0(DissDir, paste(Country, ColorScheme, 'png', sep = '.')), width = 1400, height = 600, bg = 'white')
  
  par(bg = "transparent", mar = c(4,0,0,0), mai = c(0.1, 0.1,0.5,0.1))
  # Create a function interpolating colors in the range of specified colors
  # jet.colors <- colorRampPalette( c("transparent", "green") )
  # Generate the desired number of colors from this palette
  nbcol <- 99
  
  
  Start <- -1*max(abs(cellStats(Pop3D, max)), abs(cellStats(Pop3D, min)))
  End <- 1*max(abs(cellStats(Pop3D, max)), abs(cellStats(Pop3D, min)))
  
  if (ColorScheme == 'heat') Pal <- rev(heat.colors(nbcol))[20:120]
  if (ColorScheme == 'terrain') Pal <- terrain.colors(nbcol) # , start = 0, end = 1)
  if (ColorScheme == 'rainbow') Pal <- rainbow(nbcol) # , start = 0, end = 1)
  # plot( d, col=rev( rainbow( 99, start=0,end=1 ) ), breaks=seq(min(minValue( d )),max(maxValue(d)),length.out=100) ) 
  
  if (ColorScheme == 'topo') Pal <- topo.colors(nbcol)
  if (ColorScheme == 'cm') Pal <- cm.colors(nbcol)
  if (ColorScheme == 'GreenToRed') Pal <- rev(brewer.pal(nbcol, 'RdBu'))
  
  color <- c("transparent", Pal) # look into heat.colors, topo.colors, etc.
  # Compute the z-value at the facet centres
  zfacet <- zData[-1, -1] + zData[-1, -ncz] + zData[-nrz, -1] + zData[-nrz, -ncz]
  # Recode facet z-values into color indices
  facetcol <- cut(zfacet, nbcol+1)
  # persp(x, y, z, col = color[facetcol], phi = 30, theta = -30)
  persp(x, y, z = zData, theta = 90, phi = 30,
        col = color[facetcol],
        scale = FALSE, expand = 0.75, 
        ltheta = 75, shade = 0.05, border = NA,
        box = F, ticktype = "detailed")
  if (Save == TRUE) dev.off()
}
