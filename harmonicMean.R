# harmonic mean
HM = function(x) {
  if(prod(x)==0) {
    return (0)
  } else {
    length(x)/sum(1/x)
  }
}