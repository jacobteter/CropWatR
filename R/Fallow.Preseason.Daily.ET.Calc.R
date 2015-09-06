Fallow.Preseason.Daily.ET.Calc <-
function(Croplayer, Overwrite = FALSE){    
  print(Croplayer)

  if (file.exists(paste0(Intermediates, paste('Fallow.Saved', Croplayer, 'Rdata', sep = '.')))){
    print('Fallow.File exists for this cropping patterns, loading it...')    
    load(paste0(Intermediates, paste('Fallow.Saved', Croplayer, 'Rdata', sep = '.')))

    ### SPLIT THIS LIST INTO COMPONENT FILES
    Pre.ETo <- Fallow.File[[1]]; Post.ETo <- Fallow.File[[2]]; Pre.Precip <- Fallow.File[[3]]; Post.Precip <- Fallow.File[[4]]
    Pre.Few <- Fallow.File[[5]]; Post.Few <- Fallow.File[[6]] ; Pre.ROi <- Fallow.File[[7]]; Post.ROi <- Fallow.File[[8]]; Qfc.minus.Qwp <- Fallow.File[[9]]
    Pre.Dei <- Fallow.File[[10]]; Post.Dei <- Fallow.File[[11]]; TAW <- Fallow.File[[12]]; TEW <- Fallow.File[[13]]; REW <- Fallow.File[[14]]
  }
  
  if (file.exists(paste0(Intermediates, 'Fallow.Saved.', Croplayer,'.Rdata')) == FALSE){
    
    #### a. Load files:
    load(paste0(Intermediates, paste('Fallow.Season', Croplayer, 'ETo_','Rdata', sep = '.'))); ETo <- Fallow.Season; rm(Fallow.Season)
    load(paste0(Intermediates, paste('Fallow.Season', Croplayer, 'Precip_', 'Rdata', sep = '.'))); Precip <- Fallow.Season; rm(Fallow.Season)
    # Coords <- lapply(Precip, function(x) as.data.frame(cbind(x$x, x$y)))  
    
    print('dimensions check?')
    print(all.equal(sapply(Precip, dim), sapply(ETo, dim)))
    
    if (file.exists(paste0(Intermediates, paste('Fallow.Few', Croplayer, 'Rdata', sep = '.'))) == FALSE){
      Fallow.Few.Calc(Croplayer)    
      load(paste0(Intermediates, paste('Fallow.Few', Croplayer, 'Rdata', sep = '.'))) # Fallow.Few  
      load(paste0(Intermediates, paste("Base", Croplayer, 'MNRH_', 'MasterDF', sep = '.'))) # DF
    }
    
    load(paste0(Intermediates, paste('Fallow.Few', Croplayer, 'Rdata', sep = '.'))) # Fallow.Few  
    load(paste0(Intermediates, paste("Base", Croplayer, 'MNRH_', 'MasterDF', sep = '.'))) # DF
    
    # Water Stress Calcs:  ## TAW:
    Qfc.minus.Qwp <- lapply(Precip, function(x) x$Qfc.minus.Qwp)
    # Assumption: rooting depth for fallow season weeds is: 
    root.depth <- 0.10
    TAW <- lapply(Qfc.minus.Qwp, function(x) 1000*(x[]*root.depth))
    TEW <- lapply(Precip, function(x) x$ave_TEW); Dei <- TEW
    REW <- lapply(Precip, function(x) x$ave_REW)
    
    Precip <- lapply(Precip, function(x) x[,(grep('layer', names(x)))])
    
    if (file.exists(paste0(Intermediates, paste('KcMax', Croplayer, 'Rdata', sep = '.'))) == FALSE) KcMAX(Croplayer)
    load(paste0(Intermediates, paste('KcMax', Croplayer, 'Rdata', sep = '.'))) # KcMax
       
    ROi <- Precip  # runoff is the EXCESS of heavy rainfall events, where 'EXCESS' means that the precipitation exceeds TEW:
    for (i in 1:length(ROi)){ # Takes about 45 seconds
      ROi[[i]] <- ROi[[i]]-TEW[[i]]
      ROi[[i]][ROi[[i]] < 0] <- 0
    }    
    print('Pre-/post-season runoff estimated')
    
    Dei <- lapply(TEW, function(x) (x[]*0.1))  # spot check: all.equal(lapply(Dei, function(x) sum(x)*2), lapply(TEW, sum)) # TRUE
    ETo <- lapply(ETo, function(x) x[,(grep('layer', names(x)))])
    
    for (i in 1:length(ETo)){ # slow: takes roungly 45 seconds
      ETo[[i]][ETo[[i]] < 0] <- 0
      ETo[[i]] <- round(ETo[[i]], 3)
      ETo[[i]][ETo[[i]] > 28] <- 1.655
    }    
    print('ETo data cleaned')
    
    Pre.ETo <- ETo; Post.ETo <- ETo; Pre.ROi <- ROi; Post.ROi <- ROi; Pre.Dei <- Dei; Post.Dei <- Dei
    Pre.Precip <- Precip; Post.Precip <- Precip; Pre.Few <- Fallow.Few; Post.Few <- Fallow.Few
    
    DaysRow <- lapply(ETo, function(x) as.numeric(gsub('layer.', '', names(x))))
    Cuts <- lapply(DaysRow, function(x) which(diff(x) > 1))
    Cuts <- sapply(Cuts, function(x) replace(x, length(x) == 0, 0))
    
    LengthCheck <- unlist(lapply(DaysRow, length))
    CutCheck <- unlist(Cuts)
    
    for (i in 1:length(ETo)){ # fast. ~2 seconds
      if (Cuts[[i]] > 0 && length(LengthCheck[i] > 0)){
        
        if (CutCheck[i]+1 >= LengthCheck[i]){
          Pre.ETo[[i]] <- ETo[[i]][,1:(Cuts[[i]][1]-1)]
          Post.ETo[[i]] <- ETo[[i]][,(Cuts[[i]][1]-3):Cuts[[i]][1]]
          Pre.Precip[[i]] <- Precip[[i]][,1:(Cuts[[i]][1]-1)]
          Post.Precip[[i]] <- Precip[[i]][,(Cuts[[i]][1]-3):Cuts[[i]][1]]
          Pre.Few[[i]] <- Fallow.Few[[i]][,1:(Cuts[[i]][1]-1)]
          Post.Few[[i]] <- Fallow.Few[[i]][,(Cuts[[i]][1]-3):Cuts[[i]][1]]
          Pre.ROi[[i]] <- ROi[[i]][,1:(Cuts[[i]][1]-1)]
          Post.ROi[[i]] <- ROi[[i]][,(Cuts[[i]][1]-3):Cuts[[i]][1]]
        }
        else {
          Pre.ETo[[i]] <- ETo[[i]][,1:Cuts[[i]][1]]
          Post.ETo[[i]] <- ETo[[i]][,(Cuts[[i]][1]+1):length(ETo[[i]])]
          Pre.Precip[[i]] <- Precip[[i]][,1:Cuts[[i]][1]]
          Post.Precip[[i]] <- Precip[[i]][,(Cuts[[i]][1]+1):length(Precip[[i]])]
          Pre.Few[[i]] <- Fallow.Few[[i]][,1:Cuts[[i]][1]]
          Post.Few[[i]] <- Fallow.Few[[i]][,(Cuts[[i]][1]+1):length(Fallow.Few[[i]])]
          Pre.ROi[[i]] <- ROi[[i]][,1:Cuts[[i]][1]]
          Post.ROi[[i]] <- ROi[[i]][,(Cuts[[i]][1]+1):length(ROi[[i]])]        
        }
      }
      if (Cuts[[i]] == 0){
        Pre.ETo[[i]] <- ETo[[i]]
        Post.ETo[[i]] <- ETo[[i]][,(length(ETo[[i]])-2):(length(ETo[[i]])-1)]
        Pre.Precip[[i]] <- Precip[[i]]
        Post.Precip[[i]] <- Precip[[i]][,(length(Precip[[i]])-1):length(Precip[[i]])]
        Pre.Few[[i]] <- Fallow.Few[[i]]
        Post.Few[[i]] <- Fallow.Few[[i]][,(length(Fallow.Few[[i]])-2):(length(Fallow.Few[[i]]-1))]
        Pre.ROi[[i]] <- ROi[[i]]
        Post.ROi[[i]] <- ROi[[i]][,(length(ROi[[i]])-1):length(ROi[[i]])]
      } 
    }
    print('pre/post season split complete') 
        
    Fallow.File <- list(Pre.ETo, Post.ETo, Pre.Precip, Post.Precip, Pre.Few, Post.Few, Pre.ROi, Post.ROi, Qfc.minus.Qwp, Pre.Dei, Post.Dei, TAW, TEW, REW)
    names(Fallow.File) <- c('Pre.ETo', 'Post.ETo', 'Pre.Precip', 'Post.Precip', 'Pre.Few', 'Post.Few', 'Pre.ROi', 'Post.ROi', 'Qfc.minus.Qwp', "Pre.Dei", "Post.Dei", "TAW", "TEW", "REW")
    save(Fallow.File, file = paste0(Intermediates, paste('Fallow.Saved', Croplayer, 'Rdata', sep = '.')))
  }
  
  if (file.exists(paste0(Intermediates, paste('KcMax.Fallow', Croplayer, 'Rdata', sep = '.'))) == FALSE) KcMAX.fallow(Croplayer) 
  
  load(paste0(Intermediates, paste('KcMax.Fallow', Croplayer, 'Rdata', sep = '.')))
  
  ###### Kr & Ke calculation:   
  Pre.Kr <- Pre.Precip; Pre.Ke <- Pre.Precip; Pre.Dei <- Pre.Precip; Pre.DPei <- Pre.Precip; Pre.Kcb.tot <- Pre.Precip; Pre.E <-  Pre.Precip; Pre.Fw <- Pre.Precip
  Pre.Dr <- Pre.Precip; Pre.DP <- Pre.Precip; Pre.Ks <- Pre.Precip; Dei <- TEW; Pre.Pval <-  Pre.Precip; Pre.TAW <-  Pre.Precip; Pre.RAW <- Pre.Precip; Pre.Kcb <- Pre.Precip
  
  if (!file.exists(paste0(Intermediates, paste("Preseason_Soil.Evaporation", Croplayer, "Rdata", sep = "."))) | Overwrite == TRUE){
    for (i in 1:length(Pre.Precip)){
      for (j in 1:length(Pre.Precip[[i]])){ 
        # Assumption: Kcb for preseason starts at 0.35
        Kcb <- 0.35
        
        if (j == 1){ # Initialize on first day of preseason
          Pre.Kr[[i]][,j][Dei[[i]] > REW[[i]]] <- (TEW[[i]][Dei[[i]] > REW[[i]]] - Dei[[i]][Dei[[i]] > REW[[i]]])/(TEW[[i]][Dei[[i]] > REW[[i]]] - REW[[i]][Dei[[i]] > REW[[i]]])
          Pre.Kr[[i]][,j][Dei[[i]] <= REW[[i]]] <- 1
          Pre.Kr[[i]][,j][Pre.Kr[[i]][,j] < 0] <- 0
          
          Pre.Ke[[i]][,j] <- pmin.int(Pre.Kr[[i]][,j]*(KcMax[[i]][,j] - Kcb), Pre.Few[[i]][,j]*KcMax[[i]][,j]) # NOTE: Pre.KeETo is the same as Ei in equation 77
          Pre.Ke[[i]][,j][Pre.Ke[[i]][,j] < 0] <- 0 
          
          Pre.E[[i]][,j] <- Pre.Ke[[i]][,j]*Pre.ETo[[i]][,j] # E
          
          # DPe - topsoil percolation: 
          Pre.DPei[[i]][,j] <- (Pre.Precip[[i]][,j]-Pre.ROi[[i]][,j]) - Dei[[i]]
          Pre.DPei[[i]][,j][Pre.DPei[[i]][,j] < 0] <- 0
          # De
          Pre.Dei[[i]][,j] <- Dei[[i]] - (Pre.Precip[[i]][,j]-Pre.ROi[[i]][,j]) + (Pre.E[[i]][,j]/Pre.Few[[i]][,j]) + Pre.DPei[[i]][,j] # Multiply or Divide?
          # Limits on De
          Pre.Dei[[i]][,j][Pre.Dei[[i]][,j] < 0] <- 0
          Pre.Dei[[i]][,j][Pre.Dei[[i]][,j] > TEW[[i]]] <- TEW[[i]][Pre.Dei[[i]][,j] > TEW[[i]]]
          
          # Crop evapotranspiration
          Pre.Kcb.tot[[i]][,j] <- (Kcb + Pre.Ke[[i]][,j])*Pre.ETo[[i]][,j]          
          
          # WATER STRESS CALCS:
          # Assuming base p value of 0.1 (shallow-rooted) weeds 
          P.value <- 0.1
          Pre.Pval[[i]][,j] <- P.value + 0.02*(5 - (Pre.Kcb.tot[[i]][,j]))
          Pre.Pval[[i]][,j][Pre.Pval[[i]][,j] < 0.1] <- 0.1
          Pre.Pval[[i]][,j][Pre.Pval[[i]][,j] > 0.8] <- 0.8
          ### TAW should change daily; and as a function of (growing) root depth.
          ### Root depth assumed at 0.1; grows daily
          Root.depth <- 0.10 + 0.002*j 
          Pre.TAW[[i]][,j] <- TAW[[i]]*Root.depth          
          
          Pre.RAW[[i]][,j] <- Pre.Pval[[i]][,j]*Pre.TAW[[i]][,j]
          ### Initialize Dr (positive values, less than TAW) ## ignoring capillary rise
          # Following heavy rain or irrigation, the user can assume that the root zone is near field capacity, # i.e., Dr, i-1 » 0.
          # Assumme initial level at 0.2 of TAW, so:
          Per.of.field.capacity <- 0.2
          Pre.Dr[[i]][,j] <- Pre.TAW[[i]][,j]*Per.of.field.capacity
          
          Pre.Dr[[i]][,j] <- Pre.Dr[[i]][,j] - (Pre.Precip[[i]][,j]-Pre.ROi[[i]][,j]) + Pre.Kcb.tot[[i]][,j] + Pre.DP[[i]][,j]
          # Limits on Dr:
          Pre.Dr[[i]][,j][Pre.Dr[[i]][,j] < 0] <- 0
          Pre.Dr[[i]][,j][Pre.Dr[[i]][,j] > Pre.TAW[[i]][,j]] <- Pre.TAW[[i]][,j][Pre.Dr[[i]][,j] > Pre.TAW[[i]][,j]]
          
          # Adjusted transpiration component: (equation 80)
          Pre.Ks[[i]][,j][Pre.Dr[[i]][,j] > Pre.RAW[[i]][,j]] <- ((Pre.TAW[[i]][,j]-Pre.Dr[[i]][,j])[Pre.Dr[[i]][,j] > Pre.RAW[[i]][,j]]) / ((1 - Pre.Pval[[i]][,j][Pre.Dr[[i]][,j] > Pre.RAW[[i]][,j]])*Pre.TAW[[i]][,j][Pre.Dr[[i]][,j] > Pre.RAW[[i]][,j]])
          Pre.Ks[[i]][,j][Pre.Dr[[i]][,j] <= Pre.RAW[[i]][,j]] <- 1
          
          ### Soil water balance for the root zone (equation 85)
          Pre.DP[[i]][,j] <- (Pre.Precip[[i]][,j]-Pre.ROi[[i]][,j]) - Pre.Kcb.tot[[i]][,j] # - Pre.Dr[[i]][,j-1] # ideally is the day before, but not here...
          # As long as the soil water content in the root zone is below field capacity (i.e., Dr, i > 0), the soil will not drain and DPi = 0.
          Pre.DP [[i]][,j][Pre.Dr[[i]][,j] > 0] <- 0
          Pre.DP [[i]][,j][Pre.DP[[i]][,j] < 0] <- 0
          
          Pre.Kcb[[i]][,j] <- (Pre.Ks[[i]][,j]*Pre.Kcb.tot[[i]][,j]+Pre.Ke[[i]][,j])*Pre.ETo[[i]][,j]
          Pre.Kcb.tot[[i]][,j] <- (Pre.Ks[[i]][,j]*Pre.Kcb.tot[[i]][,j])*Pre.ETo[[i]][,j]
                  
          # DPe - topsoil percolation
          Pre.DPei[[i]][,j] <- (Pre.Precip[[i]][,j]-Pre.ROi[[i]][,j]) # - Pre.Dei[[i]][,j-1]
          Pre.DPei[[i]][,j][Pre.DPei[[i]][,j] < 0] <- 0         
        }
        else { # all other days of the preseason
          ### Fw / Few /Fc calcs must incorporate irrigation and precipitation events
          # Fw - Make initial assumption about Dr <- 25% capacity
          Pre.Fw[[i]][,j] <- Pre.Few[[i]][,j-1]
          
          # Few
          Pre.Few[[i]][,j] <- pmin.int(Pre.Few[[i]][,j], Pre.Fw[[i]][,j])        
          
          # Kr
          Pre.Kr[[i]][,j][Pre.Dei[[i]][,(j-1)] > REW[[i]]] <- (TEW[[i]][Pre.Dei[[i]][,(j-1)] > REW[[i]]] - Pre.Dei[[i]][,(j-1)][Pre.Dei[[i]][,(j-1)] > REW[[i]]])/(TEW[[i]][Pre.Dei[[i]][,(j-1)] > REW[[i]]] - REW[[i]][Pre.Dei[[i]][,(j-1)] > REW[[i]]])
          Pre.Kr[[i]][,j][Pre.Dei[[i]][,(j-1)] <= REW[[i]]] <- 1
          Pre.Kr[[i]][,j][Pre.Kr[[i]][,j] < 0] <- 0
          
          # Assumption of daily increase in Kcb of weeds
          Kcb <- Kcb+(0.005*j)

          # Ke 
          Pre.Ke[[i]][,j] <- pmin.int(Pre.Kr[[i]][,j]*(KcMax[[i]][,j] - Kcb), Pre.Few[[i]][,j]*KcMax[[i]][,j]) # NOTE: Pre.KeETo is the same as Ei in equation 77
          Pre.Ke[[i]][,j][Pre.Ke[[i]][,j] < 0] <- 0     # STOP-GAP positive 1 default values:
          
          # E
          Pre.E[[i]][,j] <- Pre.Ke[[i]][,j]*Pre.ETo[[i]][,j]

          # DPe - topsoil percolation: 
          Pre.DPei[[i]][,j] <- (Pre.Precip[[i]][,j]-Pre.ROi[[i]][,j]) - Pre.Dei[[i]][,(j-1)]
          Pre.DPei[[i]][,j][Pre.DPei[[i]][,j] < 0] <- 0
          
          # De 
          Pre.Dei[[i]][,j] <- Pre.Dei[[i]][,(j-1)] - (Pre.Precip[[i]][,j]-Pre.ROi[[i]][,j]) + (Pre.E[[i]][,j]/Pre.Few[[i]][,j]) + Pre.DPei[[i]][,j] # Multiply or Divide?
          # Limits on De
          Pre.Dei[[i]][,j][Pre.Dei[[i]][,j] < 0] <- 0
          Pre.Dei[[i]][,j][Pre.Dei[[i]][,j] > TEW[[i]]] <- TEW[[i]][Pre.Dei[[i]][,j] > TEW[[i]]]
          
          # WATER STRESS CALCS:
          # Assuming base p value of 0.1 (shallow-rooted) weeds 
          P.value <- 0.1
          Pre.Pval[[i]][,j] <- P.value + 0.02*(5 - (Pre.Kcb.tot[[i]][,j]))
          Pre.Pval[[i]][,j][Pre.Pval[[i]][,j] < 0.1] <- 0.1
          Pre.Pval[[i]][,j][Pre.Pval[[i]][,j] > 0.8] <- 0.8
          ### Root depth assumed at 0.10
          Root.depth <- 0.10 + 0.002*j 
          Pre.TAW[[i]][,j] <- TAW[[i]]*Root.depth
          
          Pre.RAW[[i]][,j] <- Pre.Pval[[i]][,j]*Pre.TAW[[i]][,j]
          ### Initialize Dr (positive values, less than TAW) ## ignoring capillary rise          
          Pre.Dr[[i]][,j] <- Pre.Dr[[i]][,(j-1)] - (Pre.Precip[[i]][,j]-Pre.ROi[[i]][,j]) + Pre.Kcb.tot[[i]][,j] + Pre.DP[[i]][,(j-1)]
          
          # Limits on Dr:
          Pre.Dr[[i]][,j][Pre.Dr[[i]][,j] < 0] <- 0
          Pre.Dr[[i]][,j][Pre.Dr[[i]][,j] > Pre.TAW[[i]][,j]] <- Pre.TAW[[i]][,j][Pre.Dr[[i]][,j] > Pre.TAW[[i]][,j]]          
          
          # Adjusted transpiration component: (equation 80)
          Pre.Ks[[i]][,j][Pre.Dr[[i]][,j] > Pre.RAW[[i]][,j]] <- ((Pre.TAW[[i]][,j]-Pre.Dr[[i]][,j])[Pre.Dr[[i]][,j] > Pre.RAW[[i]][,j]]) / ((1 - Pre.Pval[[i]][,j][Pre.Dr[[i]][,j] > Pre.RAW[[i]][,j]])*Pre.TAW[[i]][,j][Pre.Dr[[i]][,j] > Pre.RAW[[i]][,j]])
          Pre.Ks[[i]][,j][Pre.Dr[[i]][,j] <= Pre.RAW[[i]][,j]] <- 1
          
          ### Soil water balance for the root zone (equation 85)
          Pre.DP[[i]][,j] <- (Pre.Precip[[i]][,j]-Pre.ROi[[i]][,j]) - Pre.Kcb.tot[[i]][,j] - Pre.Dr[[i]][,j-1] 
          # As long as the soil water content in the root zone is below field capacity (i.e., Dr, i > 0), the soil will not drain and DPi = 0.
          Pre.DP [[i]][,j][Pre.Dr[[i]][,j] > 0] <- 0
          Pre.DP [[i]][,j][Pre.DP[[i]][,j] < 0] <- 0
          
          Pre.Kcb[[i]][,j] <- (Pre.Ks[[i]][,j]*Kcb+Pre.Ke[[i]][,j])*Pre.ETo[[i]][,j]
          Pre.Kcb.tot[[i]][,j] <- (Pre.Ks[[i]][,j]*Kcb)*Pre.ETo[[i]][,j]
          
          # DPe - topsoil percolation: 
          Pre.DPei[[i]][,j] <- (Pre.Precip[[i]][,j]-Pre.ROi[[i]][,j]) - Pre.Dei[[i]][,j-1]
          Pre.DPei[[i]][,j][Pre.DPei[[i]][,j] < 0] <- 0
        }
      }      
    }
    print('Calculation of Preseason daily soil water balance, deep percolation, and evaporation complete')
    
    setwd(paste0(Path, '/CropWatR/Intermediates/'))
    
    save(Pre.Few, file = paste('Preseason_Few', Croplayer, 'Rdata', sep = '.'))
    save(Pre.Kr, file = paste('Preseason_Kr', Croplayer, 'Rdata', sep = '.'))
    save(Pre.Ks, file = paste('Preseason_Ks', Croplayer, 'Rdata', sep = '.'))
    save(Pre.Pval, file = paste('Preseason_Pval', Croplayer, 'Rdata', sep = '.'))
    
    save(Pre.Dr, file = paste('Preseason_Root.Zone.Depletion', Croplayer, 'Rdata', sep = '.'))
    save(Pre.Dei, file = paste('Preseason_Soil.Top.Layer.Depletion', Croplayer, 'Rdata', sep = '.'))
    save(Pre.DP, file = paste('Preseason_Deep.Percolation', Croplayer, 'Rdata', sep = '.'))
    Pre.KeETo <- Pre.E # renamed
    save(Pre.KeETo, file = paste('Preseason_Soil.Evaporation', Croplayer, 'Rdata', sep = '.'))
    save(Pre.ROi, file = paste('Preseason_Runoff', Croplayer, 'Rdata', sep = '.'))
    save(Pre.Kcb.tot, file = paste('Preseason_Weed.Transpiration', Croplayer, 'Rdata', sep = '.'))

    setwd(paste0(Path, '/CropWatR/Data'))
    
    print('Preseason files saved, on to final growing season run')
  }
  if (file.exists(paste0(Intermediates, paste("Preseason_Soil.Evaporation", Croplayer, "Rdata", sep = "."))) == TRUE && Overwrite == FALSE){
    print("Preseason has been previously calculated for this crop")
  }
}
