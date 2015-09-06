Irr.Mults <-
function(Crop){
  survey <- read.csv('acre-feet.per.acre.csv')
  Sub <- survey[c(1, which(names(survey) %in% Crop))]
  Sub <- Sub[complete.cases(Sub),]
  Mean <- mean(Sub[,2])
  Sub$Mult <- round(Sub[,2]/Mean, digits = 3)
  Sub$State <- factor(Sub$State)
  return(Sub)
}
