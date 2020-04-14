### Create function ###
### Liner model with line ###

# Description: 
#   it is very annoying that, while we fit linear model lm(), 
#   but still need abline() to draw the line.
#   Therefore, we creat lmln() - "linear model withl line" to fit linear model and draw the line at the same time

lmln <- function(y=yaxis,x=xaxis){
  lm <- lm(y~x)
  summ <- summary(lm)
  p.value <- coef(summ)[2,4] # take out P.value
  AdjR2 <- summ$adj.r.squared # take out Adj R-square
  plot(y~x)
  
  a <- coef(lm)[1] # intercept
  b <- coef(lm)[2] # slop
  if(p.value<0.05){lty=1;col="red"}
  if(p.value>0.05){lty=2;col="black"}
  abline(a=a,b=b,lty=lty)
  return(summ)
}

# # testing data
# y=c(13,35,76,5,4,45,7)
# x=c(1,2,3,4,5,6,7)
# lmln(y=y,x=x)
