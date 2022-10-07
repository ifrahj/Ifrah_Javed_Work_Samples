get_robust_se <- function(model) { 
    require(sandwich)
    
    sqrt(diag(sandwich::vcovHC(model)))
  }