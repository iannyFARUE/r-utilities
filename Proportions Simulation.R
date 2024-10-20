rm(list = ls())
us_population <- 250000000
p = 0.88
possible_entries <- c(rep("support",us_population*p),rep("no",us_population*(1-p)))

df <- data.frame(ID=1:10000,Mean=NA)
counter <- 1
while(counter <= 10000){
  sample_entries <- sample(possible_entries,size = 5)
  p_hat <- sum(sample_entries == "support") / length(sample_entries)
  df$Mean[counter] = p_hat
  counter <- counter + 1
}

hist(df$Mean)
sd(df$Mean)


zim_po <- 15000000
p <- 0.5
zim_population <- c(rep("yes",zim_po*p),rep("no",zim_po*(1-p)))


means_zimbabwe <- data.frame(ID=1:10000,MEAN=NA)
for(i in 1:10000){
  zimbas_10 <- sample(zim_population,size = 250)
  p_bar <- sum(zimbas_10=="yes")/length(zimbas_10)
  means_zimbabwe$MEAN[i] <- p_bar  
}

hist(means_zimbabwe$MEAN)

p_hat <- 0.848
n <- 1000
interval <- qnorm(0.005,lower.tail = FALSE)
sem <- sqrt((p_hat * (1- p_hat))/n)
ub <- p_hat+ ( interval* sem)
lb <- p_hat - (interval * sem)
paste(lb,"to",ub)

triple <- function(x){
  3 * x
}


math_magic <- function(a,b=1){
  if(b == 0){
    return(0)
  }
  a*b + a/b
}

proportions_ci <- function(p,n,ci_level=0.95){
  q <- 1-p
  sem <- round(sqrt((p*q)/n),digits = 4)
  print(paste("sem",sem))
  if(ci_level == 0.95){
     lb <- p - sem * 1.96
     ub <- p + sem * 1.96
     return(c("lower"=round(lb,digits = 4),"upper"=round(ub,digits = 4)))
  }

  if(ci_level == 0.90){
    lb <- p - sem * 1.65
    ub <- p + sem * 1.65
    return(c("lower"=round(lb,digits = 4),"upper"=round(ub,digits = 4)))
  }

  if(ci_level == 0.99){
    lb <- p - sem * 2.58
    ub <- p + sem * 2.58
    return(c("lower"=round(lb,digits = 4),"upper"=round(ub,digits = 4)))
  }

  return(c("lower"=0,upper=0))
}

p_hat <- 0.45
ci_level <- 0.90
n <- 1000
proportions_ci(p=p_hat,n=n,ci_level = ci_level)

# Exercise
# 5.7 -> 95% confidence interval
lo <- 0.45 - 0.012 * 1.96
up <- 0.45 + 0.012 * 1.96
c(lo,up)
# page 391 - answers
# page 187 - questions
# We are 95% confident that the proportion of US adults that live with one or more chronic conditions is between 42.6% and 47.4% 

# 5.8 -> 99% confidence interval
lo <- 0.52 - 0.024 * 2.58
up <- 0.52 + 0.024 * 2.58
c(lo,up)
# page 391 - answers
# page 187 - questions
# We are 99% confident that the proportion of US adults users who get atleast some news on twiter is between 45.8% and 58.2% 


# 5.9
#a) False - This is the plausible range of the true percentage of US adults who suffer from a chronic illness and sometime the truth is missed
# 95% confidence interval misses about 5% of the time

#b) Yes because a 95% confidence interval means that if we take repeated samples and calculate a 95% interval for each sample, then 95% of those interval will capture the true mean

#c) True - If we examine the 95% confidence interval in 5.7 above, we see that 50% is not included in this interval. This means that in a hypothesis test, we would reject the null hypothesis that the proportion is 0.5
#d) False - Describes uncertainity in the overall estimate from natural fluctuations due to randomness, not uncertainty corresponding to individual responses
