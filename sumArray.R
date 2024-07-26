sumArray = function(arrayOfNumbers) {
  result = 0
  if(typeof(arrayOfNumbers)=="double") {
    for (i in c(1:length(arrayOfNumbers))) {
    result = result + arrayOfNumbers[i]
    }
    result
    } else
      print("Warning: There is a non-numeric element in your array.")
}

sumArray(c(1:4))