Fallow.Postseason.Daily.ET.Calc <-
function(Croplayer, Overwrite = FALSE){  
  
  load(paste0(Intermediates,  paste('Fallow.Saved', Croplayer, 'Rdata', sep = '.')))
  ### SPLIT THIS LIST INTO COMPONENT FILES
  Post.ETo <- Fallow.File[[2]]; Post.Precip <- Fallow.File[[4]]
  Post.Few <- Fallow.File[[6]] ; Post.ROi <- Fallow.File[[8]]; Qfc.minus.Qwp <- Fallow.File[[9]]
  Post.Dei <- Fallow.File[[11]]; TAW <- Fallow.File[[12]]; TEW <- Fallow.File[[13]]; REW <- Fallow.File[[14]]
  
  ### IS THE FOLLOWING RIGHT? # KcMax is taken as the season max KcMax, i.e.:
  Post.Kr <- Post.Precip; Post.Ke <- Post.Precip; Post.Dei <- Post.Precip; Post.DPei <- Post.Precip; Post.Kcb.tot <- Post.Precip; Post.E <-  Post.Precip; Post.Fw <- Post.Precip
  Post.Dr <- Post.Precip; Post.DP <- Post.Precip; Post.Ks <- Post.Precip; Post.Kcb.tot <- Post.Precip;  Post.Pval <-  Post.Precip; Post.TAW <-  Post.Precip; Post.RAW <- Post.Precip 
  Post.Kcb <- Post.Precip
  
  if (file.exists(paste0(Intermediates, paste('KcMax.Fallow', Croplayer, 'Rdata', sep = '.'))) == FALSE){
    KcMAX.fallow(Croplayer)
  }
  
  load(paste0(Intermediates, paste('KcMax.Fallow', Croplayer, 'Rdata', sep = '.'))) # KcMax # This is the FALLOW SEASON KcMAX!!  
  # For the post-season, i only want KcMax values 'after the cut'
  KcMax <- lapply(KcMax, function(x) x[,(grep('layer', names(x)))])   # clip off the coordinates for analysis
  
  DaysRow <- lapply(Post.Precip, function(x) as.numeric(gsub('layer.', '', names(x))))
  Cuts <- lapply(DaysRow, function(x) which(diff(x) > 1))  
  Cuts <- sapply(Cuts, function(x) replace(x, length(x) == 0, 0))
  
  LengthCheck <- unlist(lapply(DaysRow, length))
  CutCheck <- unlist(Cuts)
  
  for (i in 1:length(KcMax)){ # fast. ~2 seconds    # add another loop to do this cleaner?
    # New
    if (length(CutCheck) == 0){
      KcMax[[i]] <- KcMax[[i]][1:length(Post.Precip[[i]])]
    }
    if (length(Cuts[[i]]) == 0){
      KcMax[[i]] <- KcMax[[i]][1:length(KcMax[[i]])]
    }
    else {
      KcMax[[i]] <- KcMax[[i]][,Cuts[[i]]:length(KcMax[[i]])]
    }
    
    while (length(KcMax[[i]]) > length(Post.Precip[[i]])){
      KcMax[[i]] <- KcMax[[i]][,1:length(KcMax[[i]])-1]
    }
  }
  
  print('Post Season KcMax layer lengths equal?:')
  # Alfalfa postseason fails here...
  print(all.equal(lapply(KcMax, length), lapply(Post.Precip, length)))  
  
  ## 11.2014
  ## Post-season Kcb set at 0.175 # Changed from 0.35 on May 8th
  Kcb <- 0.55  
  ##### III. POST SEASON - only run
  
  load(paste0(Intermediates,  paste('Growing.Season', Croplayer, 'Precip_', 'Rdata', sep = '.'))); Precip <- Growing.Season; rm(Growing.Season)
  
  Qfc.minus.Qwp <- lapply(Precip, function(x) x$Qfc.minus.Qwp)
  # Assumption: rooting depth for fallow season weeds is: 
  root.depth <- 0.10
  TAW <- lapply(Qfc.minus.Qwp, function(x) 1000*(x[]*root.depth))
  TEW <- lapply(Precip, function(x) x$ave_TEW); Dei <- TEW
  REW <- lapply(Precip, function(x) x$ave_REW)
  
  if (!file.exists(paste0(Intermediates, paste('Postseason_Deep.Percolation', Croplayer, 'Rdata', sep = '.'))) | Overwrite == TRUE){
    
    # Default is irrgated:
    # i <- 'irr'
    
    Others <- c('switchgrass', 'miscanthus', "idle_cropland", "pasture_grass", "silage")
    load('Vars.Rdata')
    
    if(Croplayer %in% Vars){
      
      setwd(paste0(Path, '/CropWatR/Intermediates/'))
          
      load(paste('Growing.Season_Root.Zone.Depletion', Croplayer, 'Rdata', sep = '.')) #Dr
      load(paste('Growing.Season_Deep.Percolation', Croplayer, 'Rdata', sep = '.')) #DP
      load(paste('Growing.Season_Runoff', Croplayer, 'Rdata', sep = '.')) # ROi
      load(paste('Growing.Season_Soil.Evaporation', Croplayer, 'Rdata', sep = '.')) # E
      load(paste('Growing.Saved', Croplayer, 'Rdata', sep = '.'))
      load(paste('Growing.Season_Soil.Water.Balance', Croplayer, 'Rdata', sep = '.')) # De  
      DPe <- local(get(load(file = paste('Growing.Season.Root.Zone.Percolation.Loss', Croplayer, 'Rdata', sep = '.')))) # DPe
      load(file = paste('Growing.Season.Evaporation.Fractions', Croplayer, 'Rdata', sep = '.')) # Few
      
      setwd(paste0(Path, '/CropWatR/Data'))
      
      
    }
    if (Croplayer %in% Others){

      setwd(paste0(Path, '/CropWatR/Intermediates/'))
           
      load(paste('Growing.Season_Root.Zone.Depletion', Croplayer, 'Rdata', sep = '.')) #Dr
      load(paste('Growing.Season_Deep.Percolation', Croplayer, 'Rdata', sep = '.')) #DP
      load(paste('Growing.Season_Runoff', Croplayer, 'Rdata', sep = '.')) # ROi
      load(paste('Growing.Season_Soil.Evaporation', Croplayer, 'Rdata', sep = '.')) # E
      load(paste('Growing.Saved', Croplayer, 'Rdata', sep = '.'))
      # load(paste('Growing.Season.Rainfed_Soil.Water.Balance', Croplayer, 'Rdata', sep = '.')) # De    
      load(paste('Growing.Season_Soil.Water.Balance', Croplayer, 'Rdata', sep = '.')) # De
      load(file = paste('Growing.Season.Root.Zone.Percolation.Loss', Croplayer, 'Rdata', sep = '.')) # DPe
      load(file = paste('Growing.Season.Evaporation.Fractions', Croplayer, 'Rdata', sep = '.')) # Few
      
      setwd(paste0(Path, '/CropWatR/Data'))
      
    }
    ETo <- Growing.Files[[1]]; Precip <- Growing.Files[[2]]; ROi <- Growing.Files[[3]]; Irr <- Growing.Files[[4]]; Fw <- Growing.Files[[5]]
    
    ### DOUBLE-CHECK THE BELOW CALCULATIONS FOR POSTSEASON
    print("starting calculation of post season")
    
    for (i in 1:length(Post.Precip)){  
      for (j in 1:length(Post.Precip[[i]])){ 
        # changed from Kcb <- 0.25 on 12.2014 # changed from 0.35 on May 10, 2015
        Kcb <- 0.75
        
        if (j == 1){ # Initialize on first day of Post Season (So use the last day of the growing season as the previous value base)
          ### Fix this so that it can be Few, not Fw
          Post.Fw[[i]][,j] <- Few[[i]][,length(Few[[i]])]
          
          # Kr
          Post.Kr[[i]][,j][De[[i]][,length(De[[i]])] > REW[[i]]] <- (TEW[[i]][De[[i]][,length(De[[i]])] > REW[[i]]] - De[[i]][,length(De[[i]])][De[[i]][,length(De[[i]])] > REW[[i]]])/(TEW[[i]][De[[i]][,length(De[[i]])] > REW[[i]]] - REW[[i]][De[[i]][,length(De[[i]])] > REW[[i]]])
          # table(Dei[[i]] > REW[[i]])
          Post.Kr[[i]][,j][De[[i]][,length(De[[i]])] <= REW[[i]]] <- 1
          Post.Kr[[i]][,j][Post.Kr[[i]][,j] < 0] <- 0
          
          # Ke 
          Post.Ke[[i]][,j] <- pmin.int(Post.Kr[[i]][,j]*(KcMax[[i]][,j] - Kcb), Post.Few[[i]][,j]*KcMax[[i]][,j]) # NOTE: Pre.KeETo is the same as Ei in equation 77
          Post.Ke[[i]][,j][Post.Ke[[i]][,j] < 0] <- 0     # STOP-GAP positive 1 default values:
          
          # E
          Post.E[[i]][,j] <- Post.Ke[[i]][,j]*Post.ETo[[i]][,j]
          
          # DPe # topsoil percolation: # 
          Post.DPei[[i]][,j] <- (Post.Precip[[i]][,j]-Post.ROi[[i]][,j]) - De[[i]][,length(De[[i]])]
          Post.DPei[[i]][,j][Post.DPei[[i]][,j] < 0] <- 0
          # De # 
          Post.Dei[[i]][,j] <- De[[i]][,length(De[[i]])] - (Post.Precip[[i]][,j]-Post.ROi[[i]][,j]) + (Post.E[[i]][,j]/Post.Few[[i]][,j]) + DPe[[i]][,length(DPe[[i]])] # Multiply or Divide?
          # Limits on De
          Post.Dei[[i]][,j][Post.Dei[[i]][,j] < 0] <- 0
          Post.Dei[[i]][,j][Post.Dei[[i]][,j] > TEW[[i]]] <- TEW[[i]][Post.Dei[[i]][,j] > TEW[[i]]]
          
          # Crop evapotranspiration
          Post.Kcb[[i]][,j] <- (Kcb + Post.Ke[[i]][,j])*Post.ETo[[i]][,j]
          Post.Kcb.tot[[i]][,j] <- (Kcb)*Post.ETo[[i]][,j]          
          
          # WATER STRESS CALCS:
          # Calculate Daily p values, daily RAW: (equations 81, 82, & 84)
          # A numerical approximation for adjusting p for ETc rate is 
          # p = pTable 22 + 0.04 (5 - ETc) where the adjusted p is limited to 0.1 <= p <= 0.8 and ETc is in mm/day.
          # ETc = ETc = (Kcb + Ke) ETo, or, in my calcs: (Transp[[i]][,j] + KeETo[[i]][,j])
          # Assuming base p value of 0.05 (shallow-rooted) weeds 
          P.value <- 0.1
          Post.Pval[[i]][,j] <- P.value + 0.02*(5 - (Post.Kcb.tot[[i]][,j]))
          Post.Pval[[i]][,j][Post.Pval[[i]][,j] < 0.1] <- 0.1
          Post.Pval[[i]][,j][Post.Pval[[i]][,j] > 0.8] <- 0.8
          ### TAW should change daily; and as a function of (growing) root depth.
          ### Root depth assumed at 0.25
          Root.depth <- 0.10 + 0.002*j 
          Post.TAW[[i]][,j] <- TAW[[i]]*Root.depth
          
          Post.RAW[[i]][,j] <- Post.Pval[[i]][,j]*Post.TAW[[i]][,j]
          ### Initialize Dr (positive values, less than TAW)
          ## ignoring capillary rise
          # Following heavy rain or irrigation, the user can assume that the root zone is near field capacity, # i.e., Dr, i-1 » 0.
          # Assumme initial level at 0.2 of TAW, so:
          Per.of.field.capacity <- 0.2
          Post.Dr[[i]][,j] <- Post.TAW[[i]][,j]*Per.of.field.capacity
          
          Post.Dr[[i]][,j] <- Post.Dr[[i]][,j] - (Post.Precip[[i]][,j]-Post.ROi[[i]][,j]) + Post.Kcb.tot[[i]][,j] + Post.DP[[i]][,j]
          # Limits on Dr:
          Post.Dr[[i]][,j][Post.Dr[[i]][,j] < 0] <- 0
          Post.Dr[[i]][,j][Post.Dr[[i]][,j] > Post.TAW[[i]][,j]] <- Post.TAW[[i]][,j][Post.Dr[[i]][,j] > Post.TAW[[i]][,j]]
          
          # Adjusted transpiration component: (equation 80)
          Post.Ks[[i]][,j][Post.Dr[[i]][,j] > Post.RAW[[i]][,j]] <- ((Post.TAW[[i]][,j]-Post.Dr[[i]][,j])[Post.Dr[[i]][,j] > Post.RAW[[i]][,j]]) / ((1 - Post.Pval[[i]][,j][Post.Dr[[i]][,j] > Post.RAW[[i]][,j]])*Post.TAW[[i]][,j][Post.Dr[[i]][,j] > Post.RAW[[i]][,j]])
          Post.Ks[[i]][,j][Post.Dr[[i]][,j] <= Post.RAW[[i]][,j]] <- 1
          
          
          ### Soil water balance for the root zone (equation 85)
          Post.DP[[i]][,j] <- (Post.Precip[[i]][,j]- Post.ROi[[i]][,j]) - Post.Kcb.tot[[i]][,j] - Dr[[i]][,length(Dr[[i]])] # ideally is the day before, but not here...
          # As long as the soil water content in the root zone is below field capacity (i.e., Dr, i > 0), the soil will not drain and DPi = 0.
          Post.DP [[i]][,j][Post.Dr[[i]][,j] > 0] <- 0
          Post.DP [[i]][,j][Post.DP[[i]][,j] < 0] <- 0
          
          
          Post.Kcb.tot[[i]][,j] <- (Post.Ks[[i]][,j]*Post.Kcb.tot[[i]][,j])*Post.ETo[[i]][,j]
          Post.Kcb[[i]][,j] <- (Post.Ks[[i]][,j]*Post.Kcb[[i]][,j]+Post.Ke[[i]][,j])*Post.ETo[[i]][,j]
          # Perhaps recalculate with new Trans.final?
          
          # DPe
          # topsoil percolation: # 
          Post.DPei[[i]][,j] <- (Post.Precip[[i]][,j]-Post.ROi[[i]][,j]) - De[[i]][,length(De[[i]])]
          Post.DPei[[i]][,j][Post.DPei[[i]][,j] < 0] <- 0        
        }
        
        else { # all other days of the preseason
          
          # Kcb
          Kcb <- Kcb - 0.003*j
          Kcb[Kcb < 0.005] <- 0.005
          # print(Kcb)
          ### Fw / Few /Fc calcs must incorporate irrigation and precipitation events
          # Fw - Make intial assumption about Dr <- 25% capacity
          Post.Fw[[i]][,j] <- Post.Few[[i]][,j-1]
          
          # Few
          Post.Few[[i]][,j] <- pmin.int(Post.Few[[i]][,j], Post.Fw[[i]][,j])        
          
          # Kr
          Post.Kr[[i]][,j][Post.Dei[[i]][,(j-1)] > REW[[i]]] <- (TEW[[i]][Post.Dei[[i]][,(j-1)] > REW[[i]]] - Post.Dei[[i]][,(j-1)][Post.Dei[[i]][,(j-1)] > REW[[i]]])/(TEW[[i]][Post.Dei[[i]][,(j-1)] > REW[[i]]] - REW[[i]][Post.Dei[[i]][,(j-1)] > REW[[i]]])
          # table(Dei[[i]] > REW[[i]])
          Post.Kr[[i]][,j][Post.Dei[[i]][,(j-1)] <= REW[[i]]] <- 1
          Post.Kr[[i]][,j][Post.Kr[[i]][,j] < 0] <- 0
          
          # Ke 
          ###### KcMax HAS BEEN calculated for fallow season weather inputs! ######      
          Post.Ke[[i]][,j] <- pmin.int(Post.Kr[[i]][,j]*(KcMax[[i]][,j] - Kcb), Post.Few[[i]][,j]*KcMax[[i]][,j]) # NOTE: Post.KeETo is the same as Ei in equation 77
          Post.Ke[[i]][,j][Post.Ke[[i]][,j] < 0] <- 0     # STOP-GAP positive 1 default values:
          
          # E
          Post.E[[i]][,j] <- Post.Ke[[i]][,j]*Post.ETo[[i]][,j]
          
          if (length(Post.E[[i]][,j][Post.E[[i]][,j] > 5]) > 0){
            print('Evaporation triggered:')
            print('day col:')
            print(j)
            print('State code')
            print(names(Post.Precip[i]))
            print('Evap profile')
            print(Post.E[[i]][,j][Post.E[[i]][,j] > 5])
            print('ETo profile')
            print(Post.ETo[[i]][,j][Post.E[[i]][,j] > 5])
            print('Ke profile')
            print(Post.Ke[[i]][,j][Post.E[[i]][,j] > 5])
          }
          
          # DPe
          # topsoil percolation: # 
          Post.DPei[[i]][,j] <- (Post.Precip[[i]][,j]-Post.ROi[[i]][,j]) - Post.Dei[[i]][,(j-1)]
          Post.DPei[[i]][,j][Post.DPei[[i]][,j] < 0] <- 0
          # De # 
          Post.Dei[[i]][,j] <- Post.Dei[[i]][,(j-1)] - (Post.Precip[[i]][,j]-Post.ROi[[i]][,j]) + (Post.E[[i]][,j]/Post.Few[[i]][,j]) + Post.DPei[[i]][,j] # Multiply or Divide?
          # Limits on De
          Post.Dei[[i]][,j][Post.Dei[[i]][,j] < 0] <- 0
          Post.Dei[[i]][,j][Post.Dei[[i]][,j] > TEW[[i]]] <- TEW[[i]][Post.Dei[[i]][,j] > TEW[[i]]]
          
          # Crop evapotranspiration
          Post.Kcb[[i]][,j] <- (Kcb + Post.Ke[[i]][,j])*Post.ETo[[i]][,j]
          Post.Kcb.tot[[i]][,j] <- (Kcb)*Post.ETo[[i]][,j]          
                    
          # WATER STRESS CALCS:
          # Calculate Daily p values, daily RAW: (equations 81, 82, & 84)
          # A numerical approximation for adjusting p for ETc rate is 
          # p = pTable 22 + 0.04 (5 - ETc) where the adjusted p is limited to 0.1 <= p <= 0.8 and ETc is in mm/day.
          # ETc = ETc = (Kcb + Ke) ETo, or, in my calcs: (Transp[[i]][,j] + KeETo[[i]][,j])
          ### 10.20.2014
          # Assuming base p value of 0.05 (shallow-rooted) weeds 
          P.value <- 0.05
          Post.Pval[[i]][,j] <- P.value + 0.04*(5 - (Post.Kcb.tot[[i]][,j]))
          Post.Pval[[i]][,j][Post.Pval[[i]][,j] < 0.1] <- 0.1
          Post.Pval[[i]][,j][Post.Pval[[i]][,j] > 0.8] <- 0.8
          ### TAW should change daily; and as a function of (growing) root depth.
          ### Root depth assumed at 0.10
          Root.depth <- 0.05 + 0.002*j 
          Post.TAW[[i]][,j] <- TAW[[i]]*Root.depth
          
          Post.RAW[[i]][,j] <- Post.Pval[[i]][,j]*Post.TAW[[i]][,j]
          ### Initialize Dr (positive values, less than TAW)
          ## ignoring capillary rise          
          Post.Dr[[i]][,j] <- Post.Dr[[i]][,(j-1)] - (Post.Precip[[i]][,j]-Post.ROi[[i]][,j]) + Post.Kcb.tot[[i]][,j] + Post.DP[[i]][,(j-1)]
          # Limits on Dr:
          Post.Dr[[i]][,j][Post.Dr[[i]][,j] < 0] <- 0
          Post.Dr[[i]][,j][Post.Dr[[i]][,j] > Post.TAW[[i]][,j]] <- Post.TAW[[i]][,j][Post.Dr[[i]][,j] > Post.TAW[[i]][,j]]          
          
          # Adjusted transpiration component: (equation 80)
          Post.Ks[[i]][,j][Post.Dr[[i]][,j] > Post.RAW[[i]][,j]] <- ((Post.TAW[[i]][,j]-Post.Dr[[i]][,j])[Post.Dr[[i]][,j] > Post.RAW[[i]][,j]]) / ((1 - Post.Pval[[i]][,j][Post.Dr[[i]][,j] > Post.RAW[[i]][,j]])*Post.TAW[[i]][,j][Post.Dr[[i]][,j] > Post.RAW[[i]][,j]])
          Post.Ks[[i]][,j][Post.Dr[[i]][,j] <= Post.RAW[[i]][,j]] <- 1
          
          
          ### Soil water balance for the root zone (equation 85)
          Post.DP[[i]][,j] <- (Post.Precip[[i]][,j]-Post.ROi[[i]][,j]) - Post.Kcb.tot[[i]][,j] - Post.Dr[[i]][,j-1] 
          # As long as the soil water content in the root zone is below field capacity (i.e., Dr, i > 0), the soil will not drain and DPi = 0.
          Post.DP [[i]][,j][Post.Dr[[i]][,j] > 0] <- 0
          Post.DP [[i]][,j][Post.DP[[i]][,j] < 0] <- 0
          
          Post.Kcb.tot[[i]][,j] <- (Post.Ks[[i]][,j]*Post.Kcb.tot[[i]][,j])*Post.ETo[[i]][,j]
          Post.Kcb[[i]][,j] <- (Post.Ks[[i]][,j]*Post.Kcb[[i]][,j]+Post.Ke[[i]][,j])*Post.ETo[[i]][,j]
          # Perhaps recalculate with new Trans.final?
          
          # DPe
          # topsoil percolation: # 
          Post.DPei[[i]][,j] <- (Post.Precip[[i]][,j]-Post.ROi[[i]][,j]) - Post.Dei[[i]][,j-1]
          Post.DPei[[i]][,j][Post.DPei[[i]][,j] < 0] <- 0
          
          print(mean(Post.E[[i]][,j], na.rm = TRUE))
          print(mean(Post.Kcb.tot[[i]][,j], na.rm = TRUE))
          
          
        }
      }
      
    }
    print('Calculation of Postseason daily soil water balance, deep percolation, and evaporation complete')    
    
    setwd(paste0(Path, '/CropWatR/Intermediates/'))
    
    save(Post.Dei, file = paste('Postseason_Soil.Water.Balance', Croplayer, 'Rdata', sep = '.'))
    save(Post.DP, file = paste('Postseason_Deep.Percolation', Croplayer, 'Rdata', sep = '.'))
    save(Post.ROi, file = paste('Postseason_Runoff', Croplayer, 'Rdata', sep = '.'))
    Post.KeETo <- Post.E
    save(Post.KeETo, file = paste('Postseason_Soil.Evaporation', Croplayer, 'Rdata', sep = '.'))
    save(Post.Kcb.tot, file = paste('Postseason_Weed.Transpiration', Croplayer, 'Rdata', sep = '.'))
  
    setwd(paste0(Path, '/CropWatR/Data'))
    
    print('Postseason files saved')
    
  }
  if (file.exists(paste0(Intermediates, paste('Postseason_Deep.Percolation', Croplayer, 'Rdata', sep = '.'))) == TRUE && Overwrite == FALSE){
    print(paste("Post Season already estimated for", Croplayer))
  }
}
