# The tilting function for binary treatment
tiltbin.modified <- function(weight = "overlap") {
  if (weight == "overlap") {
    return(function(x) {
      x * (1 - x)
    })
  } else if (weight == "IPW") {
    return(function(x, y) {
      y
    })
  } else if (weight == "matching") {
    return(function(x) {
      pmin(x, 1 - x)
    })
  } else if (weight == "entropy") {
    return(function(x) {
      -(x * log(x) + (1 - x) * log(1 - x))
    })
  } else if (weight == "treated") {
    return(function(x) {
      x
    })
  }
}
