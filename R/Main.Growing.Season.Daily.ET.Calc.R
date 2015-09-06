Main.Growing.Season.Daily.ET.Calc <-
function(Croplayer){  
  #### II. GROWING SEASON ####
  load(paste0(Intermediates, paste('Growing.Season', Croplayer, 'ETo_', 'Rdata', sep = '.'))); ETo <- Growing.Season; rm(Growing.Season)
  load(paste0(Intermediates, paste('Growing.Season', Croplayer, 'Precip_', 'Rdata', sep = '.'))); Precip <- Growing.Season; rm(Growing.Season)
  
  CROP <- Croplayer
  
  load(paste0('../Intermediates/Daily.Crop.Profile.', CROP, '.Rdata')) # DailyKcb
  Root.depth <- lapply(DailyKcb, function(x) x$daily_root.depth)
  
  Qfc.minus.Qwp <- lapply(Precip, function(x) x$Qfc.minus.Qwp)   ## TAW:
  TEW <- lapply(Precip, function(x) x$ave_TEW); Dei <- TEW
  REW <- lapply(Precip, function(x) x$ave_REW)
  
  Precip <- lapply(Precip, function(x) x[,(grep('layer', names(x)))])
  load(paste0(Intermediates, paste('Few', Croplayer,'Rdata', sep = '.'))) # Few
  load(paste0(Intermediates, paste('KcMax', Croplayer,'Rdata', sep = '.'))) # KcMax
  KcMax <- lapply(KcMax, function(x) x[,(grep('layer', names(x)))])
  load(paste0(Intermediates, paste('Kcb.corrected', Croplayer, 'Rdata', sep = '.'))) # Kcb.corrected
  
  ETo <- lapply(ETo, function(x) x[,(grep('layer', names(x)))])
  sapply(ETo, function(x) length(x[x<0]))
  
  
  if (file.exists(paste0(Intermediates, paste('Growing.Saved', Croplayer, 'Rdata', sep = "."))) == FALSE){
    for (i in 1:length(ETo)){ # slow: takes roungly 45 seconds
      ETo[[i]][ETo[[i]] < 0] <- 0
      ETo[[i]] <- round(ETo[[i]], 3)
    }   
    print('ETo data cleaned')
        
    ### II. ROi calculation: # ROi[[i]][,j] # precipitation on day i runoff from soil surface [mm]
    ### Current treatment: 
    ROi <- Precip  # runoff is the EXCESS of heavy rainfall events, where 'EXCESS' means that the precipitation exceeds TEW:
    for (i in 1:length(ROi)){ # Takes about 45 seconds
      ROi[[i]] <- ROi[[i]]-TEW[[i]]
      ROi[[i]][ROi[[i]] < 0] <- 0
    }    
    print('Growing season runoff estimated')  
    
    Irr <- Precip  # initialize Irr to 0 for all days (just to be sure), then copy everything to THAT template
    for (i in 1:length(Irr)){ # Takes about 45 seconds
      Irr[[i]][Irr[[i]] > 0] <- 0
    }
    
    Fw.table <- read.csv('Fw.table.csv')
    Irr.Eff <- Fw.table$fw[1] # Precip; Sprinkler, Basin, Border
    Fw <- Irr
    for (i in 1:length(Fw)){ # Takes about 45 seconds
      Fw[[i]][Fw[[i]] == 0] <- Irr.Eff
    }  
    
    Growing.Files <- list(ETo, Precip, ROi, Irr, Fw)
    save(Growing.Files, file = paste0(Intermediates, paste('Growing.Saved', Croplayer, 'Rdata', sep = ".")))
  }
  
  if (file.exists(paste0(Intermediates, paste('Growing.Saved', Croplayer, 'Rdata', sep = "."))) == TRUE){
    load(paste0(Intermediates, paste('Growing.Saved', Croplayer, 'Rdata', sep = '.')))
    ETo <- Growing.Files[[1]]; Precip <- Growing.Files[[2]]; ROi <- Growing.Files[[3]]; Irr <- Growing.Files[[4]]; Fw <- Growing.Files[[5]]
  }
  
  Zr <- read.csv('crop.roots.csv')
  Zr <- Zr[Zr$crop == Croplayer,]
  TAW.base <- lapply(Qfc.minus.Qwp, function(x) 1000*(x[]*Zr$root_depth))
  
  Kr <- Irr; ETc <- Irr; De <- Irr; DPe <- Irr; Transp <- Irr; Ke <- Irr; E <- Irr; Transp <- Irr
  Pval <- Irr; RAW <- Irr; Ks <- Irr; Transp.final <- Irr; Dr <- Irr; DP <- Irr; TAW <- Irr  
  
  ### Growing Season Calcs ### # load necessary inputs #
  
  setwd(paste0(Path, '/CropWatR/Intermediates/'))
  
  load(paste('Preseason_Root.Zone.Depletion', Croplayer, 'Rdata', sep = '.')) #Pre.Dr
  load(paste('Preseason_Soil.Top.Layer.Depletion', Croplayer, 'Rdata', sep = '.')) #Pre.Dei
  load(paste('Preseason_Deep.Percolation', Croplayer, 'Rdata', sep = '.')) # Pre.DP 
  load(paste('Preseason_Soil.Evaporation', Croplayer, 'Rdata', sep = '.')) # Pre.KeETo
  load(paste('Preseason_Runoff', Croplayer, 'Rdata', sep = '.')) # Pre.ROi
  load(paste('Preseason_Weed.Transpiration', Croplayer, 'Rdata', sep = '.')) # Pre.Kcb.tot
  load(paste('Fallow.Saved', Croplayer, 'Rdata', sep = '.')); Pre.Few <- Fallow.File[[5]]
  
  setwd(paste0(Path, '/CropWatR/Data'))
  
  if (file.exists(paste0(Intermediates, paste("Growing.Season_Transpiration", Croplayer, "Rdata", sep = "."))) == TRUE){
    print(paste("Growing Season has been previously calculated for", Croplayer))
  }
  
  if (file.exists(paste0(Intermediates, paste("Growing.Season_Transpiration", Croplayer, "Rdata", sep = "."))) == FALSE){
    print(paste('executing Growing Season calculations for', Croplayer))
    Fw.table <- read.csv('Fw.table.csv')
    Irr.Eff <- Fw.table$fw[1] # Precip; Sprinkler, Basin, Border
    
    for (i in 1:length(Precip)){ 
      
      Irrigated <- c('alfalfa', 'cotton', 'corn', 'spring_barley', 'spring_oats', 'rice', 'soybeans', 'sorghum', 'spring_wheat', 'silage', 'peanuts', 'winter_wheat', 'silage')
      if (Croplayer %in% Irrigated) irr <- TRUE 
      
      for (j in 1:length(Precip[[i]])){ 
        
        if (j == 1){ # Initialize on first day of  GROWING season (So use the FINAL entry of the Pre.Dei values)
          Few[[i]][,j] <- pmin.int(Few[[i]][,j], Fw[[i]][,j])
          
          # Kr
          Kr[[i]][,j][Pre.Dei[[i]][,length(Pre.Dei[[i]])] > REW[[i]]] <- (TEW[[i]][Pre.Dei[[i]][,length(Pre.Dei[[i]])] > REW[[i]]] - Pre.Dei[[i]][,length(Pre.Dei[[i]])][Pre.Dei[[i]][,length(Pre.Dei[[i]])] > REW[[i]]])/(TEW[[i]][Pre.Dei[[i]][,length(Pre.Dei[[i]])] > REW[[i]]] - REW[[i]][Pre.Dei[[i]][,length(Pre.Dei[[i]])] > REW[[i]]]) 
          Kr[[i]][,j][Pre.Dei[[i]][,length(Pre.Dei[[i]])] <= REW[[i]]] <- 1        
          Kr[[i]][,j][Kr[[i]][,j] < 0] <- 0
          # Ke          
          Ke[[i]][,j] <- pmin.int(Kr[[i]][,j]*(KcMax[[i]][,j] - Kcb.corrected[[i]][,j]), Few[[i]][,j]*KcMax[[i]][,j])
          
          # STOP-GAP against negative KeETo values:
          Ke[[i]][,j][Ke[[i]][,j] < 0] <- 0    
          
          # E
          E[[i]][,j] <- Ke[[i]][,j]*ETo[[i]][,j]
          # DPe - topsoil percolation: 
          DPe[[i]][,j] <- (Precip[[i]][,j]-ROi[[i]][,j]) + (Irr[[i]][,j]/Fw[[i]][,j]) - Pre.Dei[[i]][,length(Pre.Dei[[i]])]
          DPe[[i]][,j][DPe[[i]][,j] < 0] <- 0
          # De # 
          De[[i]][,j] <- Pre.Dei[[i]][,length(Pre.Dei[[i]])] - (Precip[[i]][,j]-ROi[[i]][,j]) + Irr[[i]][,j]/Fw[[i]][,j] + (E[[i]][,j]/Few[[i]][,j]) + DPe[[i]][,j] # Multiply or Divide?
          # Limits on De
          De[[i]][,j][De[[i]][,j] < 0] <- 0
          De[[i]][,j][De[[i]][,j] > TEW[[i]]] <- TEW[[i]][De[[i]][,j] > TEW[[i]]]
          
          # Crop evapotranspiration
          ETc[[i]][,j] <- (Kcb.corrected[[i]][,j]+Ke[[i]][,j])*ETo[[i]][,j]          
          
          # WATER STRESS CALCS:
          Pval[[i]][,j] <- Zr$p.value + 0.04*(5 - (ETc[[i]][,j]))
          Pval[[i]][,j][Pval[[i]][,j] < 0.1] <- 0.1
          Pval[[i]][,j][Pval[[i]][,j] > 0.8] <- 0.8

          if (is.na(Root.depth[[i]][j]/Zr$root_depth)){
            Frac <- Root.depth[[i]][length(Root.depth[[i]])]/Zr$root_depth
          } else Frac <- Root.depth[[i]][j]/Zr$root_depth
          TAW[[i]][,j] <- TAW.base[[i]]*Frac          
          
          RAW[[i]][,j] <- Pval[[i]][,j]*TAW[[i]][,j]
  
          ### Root zone depletion (positive values, less than TAW) ## ignoring capillary rise
          Dr[[i]][,j] <- Pre.Dr[[i]][,length(Pre.Dr[[i]])] - (Precip[[i]][,j]-ROi[[i]][,j]) - Irr[[i]][,j] + ETc[[i]][,j] + Pre.DP[[i]][,length(Pre.DP[[i]])]
          # Limits on Dr:
          Dr[[i]][,j][Dr[[i]][,j] < 0] <- 0
          Dr[[i]][,j][Dr[[i]][,j] > TAW[[i]][,j]] <- TAW[[i]][,j][Dr[[i]][,j] > TAW[[i]][,j]]
                    
          # Adjusted transpiration component: (equation 80)
          Ks[[i]][,j][Dr[[i]][,j] > RAW[[i]][,j]] <- ((TAW[[i]][,j]-Dr[[i]][,j])[Dr[[i]][,j] > RAW[[i]][,j]]) / ((1 - Pval[[i]][,j][Dr[[i]][,j] > RAW[[i]][,j]])*TAW[[i]][,j][Dr[[i]][,j] > RAW[[i]][,j]])
          Ks[[i]][,j][Dr[[i]][,j] <= RAW[[i]][,j]] <- 1
          
          ### Soil water balance for the root zone (equation 85)
          DP[[i]][,j] <- (Precip[[i]][,j]-ROi[[i]][,j]) + Irr[[i]][,j] - ETc[[i]][,j] - Pre.Dr[[i]][,length(Pre.Dr[[i]])]
          # As long as the soil water content in the root zone is below field capacity (i.e., Dr, i > 0), the soil will not drain and DPi = 0.
          DP [[i]][,j][Dr[[i]][,j] > 0] <- 0
          DP [[i]][,j][DP[[i]][,j] < 0] <- 0
          
          Transp[[i]][,j] <- (Ks[[i]][,j]*Kcb.corrected[[i]][,j]+Ke[[i]][,j])*ETo[[i]][,j]
          Transp.final[[i]][,j] <- (Ks[[i]][,j]*Kcb.corrected[[i]][,j])*ETo[[i]][,j]
          
          # DPe - topsoil percolation: 
          DPe[[i]][,j] <- (Precip[[i]][,j]-ROi[[i]][,j]) + (Irr[[i]][,j]/Fw[[i]][,j]) - Pre.Dei[[i]][,length(Pre.Dei[[i]])]
          DPe[[i]][,j][DPe[[i]][,j] < 0] <- 0
          # De
          De[[i]][,j] <- Pre.Dei[[i]][,length(Pre.Dei[[i]])] - (Precip[[i]][,j]-ROi[[i]][,j]) + Irr[[i]][,j]/Fw[[i]][,j] + (E[[i]][,j]/Few[[i]][,j]) + DPe[[i]][,j] # Multiply or Divide?
          # Limits on De
          De[[i]][,j][De[[i]][,j] < 0] <- 0
          De[[i]][,j][De[[i]][,j] > TEW[[i]]] <- TEW[[i]][De[[i]][,j] > TEW[[i]]]
          
        }
        
        else { # all other days of the growing season 
          
          # Fw
          Fw[[i]][,j] <- Fw[[i]][,(j-1)]
          # Few
          Few[[i]][,j] <- pmin.int(Few[[i]][,j], Fw[[i]][,j])
          
          # Kr          
          Kr[[i]][,j][De[[i]][,(j-1)] > REW[[i]]] <- (TEW[[i]][De[[i]][,(j-1)] > REW[[i]]] - De[[i]][,(j-1)][De[[i]][,(j-1)] > REW[[i]]])/(TEW[[i]][De[[i]][,(j-1)] > REW[[i]]] - REW[[i]][De[[i]][,(j-1)] > REW[[i]]]) 
          Kr[[i]][,j][De[[i]][,(j-1)] <= REW[[i]]] <- 1      
          Kr[[i]][,j][Kr[[i]][,j] < 0] <- 0
          
          # Ke          
          Ke[[i]][,j] <- pmin.int(Kr[[i]][,j]*(KcMax[[i]][,j] - Kcb.corrected[[i]][,j]), Few[[i]][,j]*KcMax[[i]][,j]) 
          # Stop-gap against negative Ke values
          Ke[[i]][,j][Ke[[i]][,j] < 0] <- 0
          
          # E
          ETo[[i]]
          E[[i]][,j] <- Ke[[i]][,j]*ETo[[i]][,j]
          
          # DPe - topsoil percolation
          DPe[[i]][,j] <- (Precip[[i]][,j]-ROi[[i]][,j]) + (Irr[[i]][,j]/Fw[[i]][,j]) - De[[i]][,j-1]
          DPe[[i]][,j][DPe[[i]][,j] < 0] <- 0
          # De
          De[[i]][,j] <- De[[i]][,j-1] - (Precip[[i]][,j]-ROi[[i]][,j]) + Irr[[i]][,j]/Fw[[i]][,j] + (E[[i]][,j]/Few[[i]][,j]) + DPe[[i]][,j] # Multiply or Divide?
          # Limits on De
          De[[i]][,j][De[[i]][,j] < 0] <- 0
          De[[i]][,j][De[[i]][,j] > TEW[[i]]] <- TEW[[i]][De[[i]][,j] > TEW[[i]]]
          
          # Crop evapotranspiration
          ETc[[i]][,j] <- (Kcb.corrected[[i]][,j]+Ke[[i]][,j])*ETo[[i]][,j]          
          
          # WATER STRESS CALCS:
          # Calculate Daily p values, daily RAW: (equations 81, 82, & 84)
          Pval[[i]][,j] <- Zr$p.value + 0.04*(5 - (ETc[[i]][,j]))
          Pval[[i]][,j][Pval[[i]][,j] < 0.1] <- 0.1
          Pval[[i]][,j][Pval[[i]][,j] > 0.8] <- 0.8
          ### TAW should change daily; and as a function of (growing) root depth.
          if (is.na(Root.depth[[i]][j]/Zr$root_depth)){
            Frac <- Root.depth[[i]][length(Root.depth[[i]])]/Zr$root_depth
          } else Frac <- Root.depth[[i]][j]/Zr$root_depth
          TAW[[i]][,j] <- TAW.base[[i]]*Frac
          
          RAW[[i]][,j] <- Pval[[i]][,j]*TAW[[i]][,j]
          ### Pre irrigation Dr calc:
          ### Root zone depletion (positive values, less than TAW)
          ## ignoring capillary rise
          Dr[[i]][,j] <- Dr[[i]][,j-1] - (Precip[[i]][,j]-ROi[[i]][,j]) - Irr[[i]][,j] + ETc[[i]][,j] + DP[[i]][,j-1]
          # Limits on Dr:
          Dr[[i]][,j][Dr[[i]][,j] < 0] <- 0
          Dr[[i]][,j][Dr[[i]][,j] > TAW[[i]][,j]] <- TAW[[i]][,j][Dr[[i]][,j] > TAW[[i]][,j]]
                    
          # APPLY IRRIGATION - assume that they irrigate based on DAY-OF Dr [Dr[[i]][,(j)] >= RAW[[i]][,(j)]]
          ### It may be worth making the multipliers crop specific...
          if (irr == TRUE & Frac > 0.5 & j < length(Irr[[i]])*0.7){
            # Irr[[i]][,j][Dr[[i]][,(j)] >= 0.6*(RAW[[i]][,(j)])] <- RAW[[i]][,(j)][Dr[[i]][,(j)] >= 0.6*(RAW[[i]][,(j)])]
            Irr[[i]][,j][Ks[[i]][,j] <= 0.9] <- 0.03*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9]
            Cum <- rowSums(Irr[[i]][,c(1:j)])
            States <- read.csv('States.key.csv')
            
            Crop <- Croplayer
            if (Croplayer == 'spring_wheat' || Croplayer == 'winter_wheat') Crop <- 'wheat'
            if (Croplayer == 'spring_barley') Crop <- 'barley'
            if (Croplayer == 'spring_oats') Crop <- 'oats'
            if (Croplayer == 'silage') Crop <- 'corn'
            
            Mults <- Irr.Mults(Crop)
            Matched <- merge(States, Mults, by.x = 'State_name', by.y = 'State', all = TRUE)
            
            Matched$Mult[is.na(Matched$Mult)] <- 1
            m <- Matched$STATE_FIPS[as.character(Matched$STATE_FIPS) == names(Precip[i])]
            Multiplier <- Matched$Mult[as.character(Matched$STATE_FIPS) == names(Precip[i])]
                        
            if (Croplayer == 'alfalfa'){
              # if (m == 31) Multiplier <- 1
              if (m == 40 || m == 20) Multiplier <- 0.85
              if (m == 46 || m == 38) Multiplier <- 1.75
              if (m == 22 || m == 45) Multiplier <- 0.5
              if (m == 48 ) Multiplier <- 1.1
              if (m == 53  || m == 41 || m == 16) Multiplier <- Multiplier*1.35
              if (m == 6) Multiplier <- Multiplier*1.25
              if (m == 30) Multiplier <- Multiplier*1.75
              if (m == 4 || m == 55) Multiplier <- Multiplier*2.25
              if (m == 8 || m == 49 ) Multiplier <- Multiplier*2.5
              if (m == 56) Multiplier <- Multiplier*3.2
              
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9] <- 0.035*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9]
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 300] <-  0.065*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 300]
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 400] <-  0.1*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 400]
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 600] <-  0.15*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 600]
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 1000] <-  0.175*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 1000]
              ## Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 1800] <-  0.0075*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 1800]
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 2000] <- 0 
            }              
            
            if (Croplayer == 'spring_barley'){
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9] <- 0.070*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9]
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 1000] <-  0 
            }
            
            if (Croplayer == 'corn' && j > 10){
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9] <- 0.09*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9]
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 400] <-  0.11*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 400]
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 800] <-  0.08*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 800]
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 1000] <-  0 
              
              if (m == 6){
                Irr[[i]][,j][Ks[[i]][,j] <= 0.9] <- 0.12*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9]
                if (length(Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 1400]) > 0){
                  print('irrigation max exceeded for...on day...')
                  print(length(Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 1400]))
                  print(j)
                }
                
                Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 1400] <-  0 
              } 
              
              
            }
            
            if (Croplayer == 'cotton'){
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9] <- 0.045*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9]
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 450] <-  0.025*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 450]
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 650] <-  0.05*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 650]
            }
            
            if (Croplayer == 'spring_oats'){
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9] <- 0.08*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9]
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 2200] <-  0 
            }
            
            if (Croplayer == 'peanuts'){
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9] <- 0.06*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9]
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 300] <-  0.08*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 300]
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 1400] <-  0 
            }            
            
            if (Croplayer == 'rice'){
              Irr[[i]][,j][Dr[[i]][,(j)] >= 0.9*(RAW[[i]][,(j)])] <- 1.75*RAW[[i]][,(j)][Dr[[i]][,(j)] >= 0.9*(RAW[[i]][,(j)])]
              Irr[[i]][,j][Ks[[i]][,j] <= 0.6 & Cum[Ks[[i]][,j] <= 0.6] >= 200] <- 1*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.6 & Cum[Ks[[i]][,j] <= 0.6] >= 200]
              Irr[[i]][,j][Ks[[i]][,j] <= 0.6 & Cum[Ks[[i]][,j] <= 0.6] >= 500] <- 0.5*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.6 & Cum[Ks[[i]][,j] <= 0.6] >= 500]
              Irr[[i]][,j][Ks[[i]][,j] <= 0.6 & Cum[Ks[[i]][,j] <= 0.6] >= 850] <-  0.35*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.6 & Cum[Ks[[i]][,j] <= 0.6] >= 850]
            }          
            
            if (Croplayer == 'sorghum'){
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9] <- 0.05*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9]
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 400] <-  0.0275*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 100]
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 4800] <-  0 
            }
            
            if (Croplayer == 'soybeans'){
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9] <- 0.095*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9]
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 350] <-  0.02*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 350]
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 1200] <-  0 
            }
            
            if (Croplayer == 'spring_wheat'){
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9] <- 0.0075*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9]
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 1400] <-  0 
            }         
            
            if (Croplayer == 'winter_wheat'){
              if (m == 35  || m == 49 || m == 31 || m == 20) Multiplier <- Multiplier*0.5
              if (m == 16) Multiplier <- Multiplier*0.75
              if (m == 41 || m == 8 || m == 32) Multiplier <- Multiplier*1.5
              
              if (m == 48 || m == 46 || m == 38  || m == 45  || m == 37  || m == 51 || m == 40 || m == 5) Multiplier <- Multiplier*2
              if (m == 6 || m == 32) Multiplier <- Multiplier*3
              
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9] <- 0.018*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9]
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 350] <-  0.045*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 350]
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 1200] <-  0 
            }       
            
            if (Croplayer == 'silage'){
              Irr[[i]][,j][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 400] <-  0.07*RAW[[i]][,(j)][Ks[[i]][,j] <= 0.9 & Cum[Ks[[i]][,j] <= 0.9] >= 400]
            }     
            
            Irr[[i]][,j] <- Irr[[i]][,j]*Multiplier
          }
          
          Dr[[i]][,j] <- Dr[[i]][,j-1] - (Precip[[i]][,j]-ROi[[i]][,j]) - Irr[[i]][,j] + ETc[[i]][,j] + DP[[i]][,j-1]
          
          # Limits on Dr:
          Dr[[i]][,j][Dr[[i]][,j] < 0] <- 0
          Dr[[i]][,j][Dr[[i]][,j] > TAW[[i]][,j]] <- TAW[[i]][,j][Dr[[i]][,j] > TAW[[i]][,j]]
          
          # Adjusted transpiration component: (equation 80)
          Ks[[i]][,j][Dr[[i]][,j] > RAW[[i]][,j]] <- ((TAW[[i]][,j]-Dr[[i]][,j])[Dr[[i]][,j] > RAW[[i]][,j]]) / ((1 - Pval[[i]][,j][Dr[[i]][,j] > RAW[[i]][,j]])*TAW[[i]][,j][Dr[[i]][,j] > RAW[[i]][,j]])
          Ks[[i]][,j][Dr[[i]][,j] <= RAW[[i]][,j]] <- 1
          
          
          ### Soil water balance for the root zone (equation 85)
          DP[[i]][,j] <- (Precip[[i]][,j]-ROi[[i]][,j]) + Irr[[i]][,j] - ETc[[i]][,j] - Dr[[i]][,j-1]
          # As long as the soil water content in the root zone is below field capacity (i.e., Dr, i > 0), the soil will not drain and DPi = 0.
          DP [[i]][,j][Dr[[i]][,j] > 0] <- 0
          DP [[i]][,j][DP[[i]][,j] < 0] <- 0
          
          Transp[[i]][,j] <- (Ks[[i]][,j]*Kcb.corrected[[i]][,j]+Ke[[i]][,j])*ETo[[i]][,j]
          Transp.final[[i]][,j] <- (Ks[[i]][,j]*Kcb.corrected[[i]][,j])*ETo[[i]][,j]
          
          # DPe - topsoil percolation:
          DPe[[i]][,j] <- (Precip[[i]][,j]-ROi[[i]][,j]) + (Irr[[i]][,j]/Fw[[i]][,j]) - De[[i]][,j-1]
          DPe[[i]][,j][DPe[[i]][,j] < 0] <- 0
          # De 
          De[[i]][,j] <- De[[i]][,j-1] - (Precip[[i]][,j]-ROi[[i]][,j]) + Irr[[i]][,j]/Fw[[i]][,j] + (E[[i]][,j]/Few[[i]][,j]) + DPe[[i]][,j] # Multiply or Divide?
          # Limits on De
          De[[i]][,j][De[[i]][,j] < 0] <- 0
          De[[i]][,j][De[[i]][,j] > TEW[[i]]] <- TEW[[i]][De[[i]][,j] > TEW[[i]]]
          
        }
        
      }      
    }
  }
  
  print('Saving growing season SB files')
  
  setwd(paste0(Path, '/CropWatR/Intermediates/'))
  
  save(Few, file = paste('Growing.Season_Root.Zone.Depletion', Croplayer, 'Rdata', sep = '.')) # Few
  save(Kr, file = paste('Growing.Season_Kr', Croplayer, 'Rdata', sep = '.'))
  save(Ks, file = paste('Growing.Season_Ks', Croplayer, 'Rdata', sep = '.'))
  save(Pval, file = paste('Growing.Season_Pval', Croplayer, 'Rdata', sep = '.'))
  
  save(Dr, file = paste('Growing.Season_Root.Zone.Depletion', Croplayer, 'Rdata', sep = '.')) #Dr
  save(De, file = paste('Growing.Season_Soil.Water.Balance', Croplayer, 'Rdata', sep = '.'))
  save(DP, file = paste('Growing.Season_Deep.Percolation', Croplayer, 'Rdata', sep = '.'))
  save(ROi, file = paste('Growing.Season_Runoff', Croplayer, 'Rdata', sep = '.'))
  save(E, file = paste('Growing.Season_Soil.Evaporation', Croplayer, 'Rdata', sep = '.'))
  save(Irr, file = paste('Growing.Season_Irrigation', Croplayer, 'Rdata', sep = '.'))
  save(Transp.final, file = paste('Growing.Season_Transpiration', Croplayer, 'Rdata', sep = '.'))
  save(DPe, file = paste('Growing.Season.Root.Zone.Percolation.Loss', Croplayer, 'Rdata', sep = '.')) # DPe
  save(Few, file = paste('Growing.Season.Evaporation.Fractions', Croplayer, 'Rdata', sep = '.')) # Few

  setwd(paste0(Path, '/CropWatR/Data'))
  
  print('Calculation of Growing Season daily soil water balance, deep percolation, and evaporation complete')
  print('Growing Season initial run complete, on to post season')
}
