## calculate impact from the inverse of longevity

LSI <- function(Fs,a,b){        #  a = slope of binomial model, b = intercept of binomial model, Fs = fishing SAR
  step.size=.5
  longevity=seq(1,200,by=step.size)
  
  r = 5.31/longevity
  K = (a*exp(a * log(longevity)+b))/(longevity * (exp(a*log(longevity) + b) + 1)^2)     #1st derivative of logistic
  B = K; B[longevity > (1/Fs)] <- 0
  
  LSI = sum(B)*step.size
  LSI
  
}

