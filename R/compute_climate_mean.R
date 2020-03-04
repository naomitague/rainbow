
compute_climate_mean = function(maxtemp, mintemp) {
  cmean = (maxtemp+mintemp)/2.0
  return(list(cmean, maxtemp))
}
