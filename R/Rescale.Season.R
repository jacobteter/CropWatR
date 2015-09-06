Rescale.Season <-
function(Stages, Season.length){
  # Given a numeric vector of length four for any given crop with the length (days) of each season, (Stages) 
  # and a single numeric value of the length of the actual (e.g. state-level) growing season (Season.length)
  # this function rescales the initial vector Stages to the actual (survey data) Season.length, and outputs this vector (Rescale)
  Season <- rowSums(Stages)
  Scalor <- Season.length/Season
  Rescale <- c(floor(Stages[,1]*Scalor), ceiling(Stages[,2]*Scalor), floor(Stages[,3]*Scalor), ceiling(Stages[,4]*Scalor))
  Rescale <- matrix(Rescale, ncol = 4)
  return(Rescale)
}
