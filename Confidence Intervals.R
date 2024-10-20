ci_percentage <- function(p){
  decimal <- round(p/100, digits = 2)
  alpha <- 1 - decimal
  lower <- alpha /2
  cumulative_probability <- decimal + lower
  c(zminus=-round(qnorm(cumulative_probability),digits = 2),zplus=round(qnorm(cumulative_probability),2))
}

ci_decimal <- function(x){
  alpha <- 1 - x
  lower <- alpha /2
  cumulative_probability <- 1-lower
  c(zminus=-round(qnorm(cumulative_probability),digits = 2),zplus=round(qnorm(cumulative_probability),2))
}

