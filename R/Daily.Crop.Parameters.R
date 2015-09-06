Daily.Crop.Parameters <-
function(Croplayer){ 
  Calc.Basal.Crop.Coeff(Croplayer)
  KcMAX(Croplayer)
  KcMAX.fallow(Croplayer)
  Calc.Fc.Few(Croplayer)
  Fallow.Few.Calc(Croplayer)
}
