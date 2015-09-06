Daily.Crop.Curves <-
function(Croplayer, StateNames, Stages, Kcb_tab, MaxHeight){
  ##### INPUTS:
  ### Stages: a vector of length four, giving the whole number length in days of the following four seasons
  ### 4 stages: initial, crop development, mid season, late season
  ### Kcb_tab:  a vector of length three, the Kcb for a given crop
  ### StateNames: a vector of length of resultant list, same order as the nrows, of State IDs
  
  #### OUTPUTS: a data.frame with (x) the length of the growing season (in days), and the following variables:
  # DailyKcb$Season_day : # of day in the growing season
  # DailyKcb$Stage_ID : factor (1:4); stage of growing season
  # DailyKcb$daily_Kcb : daily Kcb values
  # DailyKcb$daily_Kcb : day_height - daily interpolated height of crop
  # ALSO: returns the exposed soil fraction (1-Fc), over the lenght of the growing season
  
  ## Added Croplayer, need to access it to get rooting depths
  root.depth <- read.csv('crop.roots.csv')
  Crop <- Croplayer
  if (Croplayer == 'spring_barley' | Croplayer == 'fall_barley'){
    Crop <- 'barley'
  }
  if (Croplayer == 'spring_oats' | Croplayer == 'fall_oats'){
    Crop <- 'oats'
  }
  if (Croplayer == 'durum_wheat'){
    Crop <- 'spring_wheat'
  }
  
  root.depth <- subset(root.depth, crop == Crop, select = c(min_rooting_depth, max_rooting_depth))
  Season <- rowSums(Stages)  
  
  Base <- lapply(Season, function(x) x = c(1:x))
  Next <- Base; Kci <- Base; Height.Kci <- Base; DailyKcb <- Base; Growth_split <- Base; season.ID <- Base; day_height <- Base; MAX.Height <- Base; One.Minus.Fc <- Base   # Initialize vectors 
  Roots <- Base; Day_Roots <- Base
  
  for (i in 1:length(Next)){
    
    B <- c(Stages[i,1], sum(c(Stages[i,1], Stages[i,2])), sum(c(Stages[i,1], Stages[i,2], Stages[i,3])), sum(Stages[i,]))
    
    Xs <- c(1, B[1], mean(c(B[1], B[1], B[2])), mean(c(B[1], B[2])), mean(c(B[2], B[2], B[3])), B[3], mean(c(B[3], B[4])), B[4])
    
    Ys <- c(0.01, 0.02, Kcb_tab[[1]], Kcb_tab[[2]], sum(c(Kcb_tab[[1]], Kcb_tab[[2]], Kcb_tab[[3]])), Kcb_tab[[2]], mean(c(Kcb_tab[[2]], Kcb_tab[[3]])), Kcb_tab[[3]])
    
    P <- data.frame(bezierCurve(Xs,Ys,500))
    Q <- P[!duplicated(round(P$x)),]
    Q$x <- round(Q$x) 
    
    Kci[[i]] <- round(Q$y, digits = 2)
    
    ## Height calculation
    Growth_split[[i]] <- unlist(c(0.01, .015, rep(NA, times = ceiling(Stages[i,1]/2)-2), Kcb_tab[1], rep(NA, times = floor(Stages[i,1]/2)-1),
                                  rep(NA, times = Stages[i,2]-1), Kcb_tab[2],
                                  rep(Kcb_tab[2], times = Stages[i,3]), 
                                  rep(NA, times = (Stages[i,4]-1)), Kcb_tab[3]))  
    Height.Kci[[i]] <- spline(Base[[i]],  Growth_split[[i]], xout = Base[[i]], method = 'natural', ties = mean)$y
    MAX.Height[[i]] <- max(Height.Kci[[i]])
    day_height[[i]] <- round(Height.Kci[[i]]*MaxHeight/MAX.Height[[i]], 2)
    
    ## Root growth: (currently goes from min rooting to max, and then halfway back in the post-season)
    Zs <- c(0.1, 0.15, mean(c(0.15, root.depth[[1]])), root.depth[[1]], root.depth[[2]], sum(c(mean(c(0.15, root.depth[[1]])), root.depth[[2]])), root.depth[[2]], root.depth[[1]])
    R <- data.frame(bezierCurve(Xs,Zs,500))
    S <- R[!duplicated(round(R$x)),]
    S$x <- round(S$x)     
    
    Day_Roots[[i]] <- round(S$y, digits = 2)
    
    ### Season.ID
    season.ID[[i]] <- as.factor(c(rep(1, times = Stages[i,1]), rep(2, times = Stages[i,2]), 
                                  rep(3, times = Stages[i,3]), rep(4, times = Stages[i,4])))    
    DailyKcb[[i]] <- as.data.frame(cbind(Base[[i]], round(Kci[[i]], 2), round(Day_Roots[[i]], 2), day_height[[i]], as.factor(season.ID[[i]])))
    names(DailyKcb[[i]]) <- c('Season_day', 'daily_Kcb', "daily_root.depth", "day_height", "season.ID")
  }
  names(DailyKcb) <- StateNames
  save(DailyKcb, file = paste0(Intermediates, 'Daily.Crop.Profile.', Croplayer, '.Rdata'))
  return(DailyKcb)
}
