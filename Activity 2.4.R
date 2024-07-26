calcSum = function(x, y) {
  if(x == 0 && y == 0) { #check if arguments are both 0
    return(0)
  } else if (x + y == 0) {#check if sum of args is 0
    print("Warning input varialbes result in division by 0.")
    quit(status=1)
  } else {
    ((x^2+y^2)/(x+y))
  }
}

calcSum(0.4,0.9)
