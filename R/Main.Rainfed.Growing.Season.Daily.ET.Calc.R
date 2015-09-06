Main.Rainfed.Growing.Season.Daily.ET.Calc <-
function(Croplayer, Auto = TRUE){  
  #### II. GROWING SEASON ####
  load('Vars.Rdata')
  Irr.Vars <- Vars[-c(3,6,8,14,15)]
  if (!(Croplayer %in% Irr.Vars)) stop('This function is for irrigated varieties only!')
  
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
  
  
  # Turn irrigation ON by default...
  
  if (file.exists(paste0(Intermediates,paste('Growing.Saved', Croplayer, 'Rdata', sep = "."))) == FALSE){
    # Daily ETo should be cleaned of negative cases, also might as well round to three decimals
    for (i in 1:length(ETo)){ # slow: takes roungly 45 seconds
      ETo[[i]][ETo[[i]] < 0] <- 0
      ETo[[i]] <- round(ETo[[i]], 3)
      ETo[[i]][ETo[[i]] > 28] <- 1.655
      print('ETo high vals warning:')
      print(length(ETo[[i]][ETo[[i]] > 18]))
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
    # Irr.Eff <- .625 # Furrow average # Irr.Eff <- Fw.table$fw[8] # Trickle
    # irrigation depth distributed over the entire field. Therefore, the value Ii/fw is used to describe the actual concentration 
    # of the irrigation volume over the fraction of the soil that is wetted (Figure 31).
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
  
  ## Added fall/spring varieties directly to the Zr file
  Zr <- read.csv('crop.roots.csv')
  Zr <- Zr[Zr$crop == Croplayer,]
  TAW.base <- lapply(Qfc.minus.Qwp, function(x) 1000*(x[]*Zr$root_depth))
  
  Kr <- Irr; ETc <- Irr; De <- Irr; DPe <- Irr; Transp <- Irr; Ke <- Irr; E <- Irr; Transp <- Irr
  Pval <- Irr; RAW <- Irr; Ks <- Irr; Transp.final <- Irr; Dr <- Irr; DP <- Irr; TAW <- Irr  
  
  ### Growing Season Calcs ###
  # load necessary inputs
  setwd(paste0(Path, '/CropWatR/Intermediates/'))
  
  load(paste('Preseason_Root.Zone.Depletion', Croplayer, 'Rdata', sep = '.')) #Pre.Dr
  load(paste('Preseason_Soil.Top.Layer.Depletion', Croplayer, 'Rdata', sep = '.')) #Pre.Dei
  load(paste('Preseason_Deep.Percolation', Croplayer, 'Rdata', sep = '.')) # Pre.DP 
  load(paste('Preseason_Soil.Evaporation', Croplayer, 'Rdata', sep = '.')) # Pre.KeETo
  load(paste('Preseason_Runoff', Croplayer, 'Rdata', sep = '.')) # Pre.ROi
  load(paste('Preseason_Weed.Transpiration', Croplayer, 'Rdata', sep = '.')) # Pre.Kcb.tot
  load(paste('Fallow.Saved', Croplayer, 'Rdata', sep = '.')); Pre.Few <- Fallow.File[[5]]
  
  setwd(paste0(Path, '/CropWatR/Data'))
  
  if (file.exists(paste0(Intermediates, paste("Growing.Season.Rainfed_Transpiration", Croplayer, "Rdata", sep = "."))) == TRUE & Auto == TRUE){
    print(paste("Growing Season has been previously calculated for", Croplayer))
  }
  
  if (file.exists(paste0(Intermediates, paste("Growing.Season.Rainfed_Transpiration", Croplayer, "Rdata", sep = "."))) == FALSE){
    
    Fw.table <- read.csv('Fw.table.csv')
    Irr.Eff <- Fw.table$fw[1] # Precip; Sprinkler, Basin, Border
    
    for (i in 1:length(Precip)){ 
      
      # Irrigated <- c('alfalfa', 'cotton', 'corn', 'spring_barley', 'spring_oats', 'rice', 'soybeans', 'sorghum', 'spring_wheat', 'silage', 'peanuts', 'winter_wheat', 'silage')
      # States <- c('4', '5', '6', '8', '16', '20', '28', '30', '31', '32', '35', '38', '40', '41', '46', '48', '49', '53', '56')
      # not yet Georgia, Iowa
      # if (Croplayer %in% Irrigated && names(Precip[i]) %in% States) irr <- TRUE 
      # if (!(names(Precip[i]) %in% States)) irr <- FALSE
      # if (Croplayer == 'corn' && names(Precip[i]) == '20') irr <- FALSE
      
      for (j in 1:length(Precip[[i]])){ 
        
        if (j == 1){ # Initialize on first day of  GROWING season (So use the FINAL entry of the Pre.Dei values)
          ### Fw / Few /Fc calcs must incorporate irrigation and precipitation events
          # Fw
          # Fw[[i]][,j] <- Pre.Few[[i]][,length(Pre.Few[[i]])]
          # Few
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
          # DPe
          # topsoil percolation: # 
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
          # Calculate Daily p values, daily RAW: (equations 81, 82, & 84)
          # A numerical approximation for adjusting p for ETc rate is 
          # p = pTable 22 + 0.04 (5 - ETc) where the adjusted p is limited to 0.1 <= p <= 0.8 and ETc is in mm/day.
          # ETc = ETc = (Kcb + Ke) ETo, or, in my calcs: (Transp[[i]][,j] + KeETo[[i]][,j])
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
          Dr[[i]][,j] <- Pre.Dr[[i]][,length(Pre.Dr[[i]])] - (Precip[[i]][,j]-ROi[[i]][,j]) - Irr[[i]][,j] + ETc[[i]][,j] + Pre.DP[[i]][,length(Pre.DP[[i]])]
          # Limits on Dr:
          Dr[[i]][,j][Dr[[i]][,j] < 0] <- 0
          Dr[[i]][,j][Dr[[i]][,j] > TAW[[i]][,j]] <- TAW[[i]][,j][Dr[[i]][,j] > TAW[[i]][,j]]
          
          # Dr[[i]][,j] <- Pre.Dr[[i]][,length(Pre.Dr[[i]])] - (Precip[[i]][,j]-ROi[[i]][,j]) - Irr[[i]][,j] + ETc[[i]][,j] + Pre.DP[[i]][,length(Pre.DP[[i]])]
          # Limits on Dr:
          # Dr[[i]][,j][Dr[[i]][,j] < 0] <- 0
          # Dr[[i]][,j][Dr[[i]][,j] > TAW[[i]][,j]] <- TAW[[i]][,j][Dr[[i]][,j] > TAW[[i]][,j]]
          
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
          # Perhaps recalculate with new Trans.final?
          
          # DPe
          # topsoil percolation: # 
          DPe[[i]][,j] <- (Precip[[i]][,j]-ROi[[i]][,j]) + (Irr[[i]][,j]/Fw[[i]][,j]) - Pre.Dei[[i]][,length(Pre.Dei[[i]])]
          DPe[[i]][,j][DPe[[i]][,j] < 0] <- 0
          # De # 
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
          
          #           if (length(E[[i]][,j][E[[i]][,j] > 5]) > 0){
          #             print('Evaporation triggered:')
          #             print('day col:')
          #             print(j)
          #             print('State code')
          #             print(names(Precip[i]))
          #             print('Evap profile')
          #             print(E[[i]][,j][E[[i]][,j] > 5])
          #             print('ETo profile')
          #             print(ETo[[i]][,j][E[[i]][,j] > 5])
          #             print('Ke profile')
          #             print(Ke[[i]][,j][E[[i]][,j] > 5])
          #           }
          
          # DPe
          # topsoil percolation: # 
          DPe[[i]][,j] <- (Precip[[i]][,j]-ROi[[i]][,j]) + (Irr[[i]][,j]/Fw[[i]][,j]) - De[[i]][,j-1]
          DPe[[i]][,j][DPe[[i]][,j] < 0] <- 0
          # De # 
          De[[i]][,j] <- De[[i]][,j-1] - (Precip[[i]][,j]-ROi[[i]][,j]) + Irr[[i]][,j]/Fw[[i]][,j] + (E[[i]][,j]/Few[[i]][,j]) + DPe[[i]][,j] # Multiply or Divide?
          # Limits on De
          De[[i]][,j][De[[i]][,j] < 0] <- 0
          De[[i]][,j][De[[i]][,j] > TEW[[i]]] <- TEW[[i]][De[[i]][,j] > TEW[[i]]]
          
          # Crop evapotranspiration
          ETc[[i]][,j] <- (Kcb.corrected[[i]][,j]+Ke[[i]][,j])*ETo[[i]][,j]          
          
          # WATER STRESS CALCS:
          # Calculate Daily p values, daily RAW: (equations 81, 82, & 84)
          # A numerical approximation for adjusting p for ETc rate is 
          # p = pTable 22 + 0.04 (5 - ETc) where the adjusted p is limited to 0.1 <= p <= 0.8 and ETc is in mm/day.
          # ETc = ETc = (Kcb + Ke) ETo, or, in my calcs: (Transp[[i]][,j] + KeETo[[i]][,j])          
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
          
          ### Post irrigation Dr calc:          
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
          # all.equal(Transp.final[[i]][,j], ETc[[i]][,j])
          # Perhaps recalculate with new Trans.final?
          
          #           if (length(Transp.final[[i]][,j][Transp.final[[i]][,j] > 15]) > 0){
          #             print('Transp.final triggered:')
          #             print('day col:')
          #             print(j)
          #             print('State code')
          #             print(names(Precip[i]))
          #             # print('ET profile')
          #             # print(Transp.final[[i]][,j][Transp.final[[i]][,j] > 15])
          #             # print('ETo profile')
          #             # print(ETo[[i]][,j][Transp.final[[i]][,j] > 15])
          #             # print('two days before ETo profile')
          #             # print(ETo[[i]][,j-2][Transp.final[[i]][,j] > 15])
          #             # print(ETo[[i]][,j-1][Transp.final[[i]][,j] > 15])
          #             # print('two days after ETo profile')
          #             # print(ETo[[i]][,j+1][Transp.final[[i]][,j] > 15])
          #             # print(ETo[[i]][,j+2][Transp.final[[i]][,j] > 15])            
          #             # print('Ke profile')
          #             # print(Ke[[i]][,j][Transp.final[[i]][,j] > 15])
          #           }
          #           
          
          # DPe
          # topsoil percolation: # 
          DPe[[i]][,j] <- (Precip[[i]][,j]-ROi[[i]][,j]) + (Irr[[i]][,j]/Fw[[i]][,j]) - De[[i]][,j-1]
          DPe[[i]][,j][DPe[[i]][,j] < 0] <- 0
          # De # 
          De[[i]][,j] <- De[[i]][,j-1] - (Precip[[i]][,j]-ROi[[i]][,j]) + Irr[[i]][,j]/Fw[[i]][,j] + (E[[i]][,j]/Few[[i]][,j]) + DPe[[i]][,j] # Multiply or Divide?
          # Limits on De
          De[[i]][,j][De[[i]][,j] < 0] <- 0
          De[[i]][,j][De[[i]][,j] > TEW[[i]]] <- TEW[[i]][De[[i]][,j] > TEW[[i]]]
          
          # sapply(Pval, function(x) length(x[x < 0])/length(x))
          # sapply(DPei, function(x) length(x[x < 0])/length(x[is.numeric(x)]))
          # sapply(Dei, function(x) length(x[x < 0])/length(x[x < 100000]))
        }
      }
      
      
      Few[[i]][,1] <- Few[[i]][,2]
      Kr[[i]][,1] <- Kr[[i]][,2]
      Ke[[i]][,1] <- Ke[[i]][,2]
      E[[i]][,1] <- E[[i]][,2]
      DPe[[i]][,1] <- DPe[[i]][,2]
      De[[i]][,1] <- De[[i]][,2]
      ETc[[i]][,1] <- ETc[[i]][,2]
      Pval[[i]][,1] <- Pval[[i]][,2] 
      TAW[[i]][,1] <- TAW[[i]][,2]  
      RAW[[i]][,1] <- RAW[[i]][,2]
      Dr[[i]][,1] <- Dr[[i]][,2]
      Dr[[i]][,1] <- Dr[[i]][,2]
      Ks[[i]][,1] <- Ks[[i]][,2]
      DP[[i]][,1] <-   DP[[i]][,2]
      Transp[[i]][,1] <- Transp[[i]][,2]
      Transp.final[[i]][,1] <- Transp.final[[i]][,2]
      
    }
  }
  
  print('Saving rainfed growing season SB files')
  
  setwd(paste0(Path, '/CropWatR/Intermediates/'))
  
  save(Few, file = paste('Growing.Season.Rainfed_Root.Zone.Depletion', Croplayer, 'Rdata', sep = '.')) # Few
  save(Kr, file = paste('Growing.Season.Rainfed_Kr', Croplayer, 'Rdata', sep = '.'))
  save(Ks, file = paste('Growing.Season.Rainfed_Ks', Croplayer, 'Rdata', sep = '.'))
  save(Pval, file = paste('Growing.Season.Rainfed_Pval', Croplayer, 'Rdata', sep = '.'))
  
  save(Dr, file = paste('Growing.Season.Rainfed_Root.Zone.Depletion', Croplayer, 'Rdata', sep = '.')) #Dr
  save(De, file = paste('Growing.Season.Rainfed_Soil.Water.Balance', Croplayer, 'Rdata', sep = '.'))
  save(DP, file = paste('Growing.Season.Rainfed_Deep.Percolation', Croplayer, 'Rdata', sep = '.'))
  save(ROi, file = paste('Growing.Season.Rainfed_Runoff', Croplayer, 'Rdata', sep = '.'))
  save(E, file = paste('Growing.Season.Rainfed_Soil.Evaporation', Croplayer, 'Rdata', sep = '.'))
  save(Transp.final, file = paste('Growing.Season.Rainfed_Transpiration', Croplayer, 'Rdata', sep = '.'))
  save(DPe, file = paste('Growing.Season.Rainfed.Root.Zone.Percolation.Loss', Croplayer, 'Rdata', sep = '.')) # DPe
  save(Few, file = paste('Growing.Season.Rainfed.Evaporation.Fractions', Croplayer, 'Rdata', sep = '.')) # Few

  setwd(paste0(Path, '/CropWatR/Data'))
  
  print('Calculation of Growing Season daily soil water balance, deep percolation, and evaporation complete')
  print('Growing Season initial run complete, on to post season')
}
