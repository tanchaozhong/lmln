# lmln

#This is a R script created on R studio.

#It is very annoying that while fitting the linear model, still need to use abline to plot line.

#Therefore, we creat the funtion lmln() to fit linear model while draw the line.

#only can fit a single independent variable to a single respond variable
#only two colors - red (P < 0.05) and black (P > 0.05) to show
#only two line type - solid (P > 0.05) and dash (P < 0.05) to show

lmln <- function(y=yaxis,x=xaxis){
  lm <- lm(y~x)
  summ <- summary(lm)
  p.value <- coef(summ)[2,4] # store P.value
  AdjR2 <- summ$adj.r.squared # store Adj R-square
  plot(y~x)
  
  a <- coef(lm)[1] # intercept
  b <- coef(lm)[2] # slop
  if(p.value<0.05){lty=1;col="red"}
  if(p.value>0.05){lty=2;col="black"}
  abline(a=a,b=b,lty=lty)
  return(summ)
}

# Data for testing
y=c(13,35,76,5,4,45,7)
x=c(1,2,3,4,5,6,7)
lmln(y=y,x=x)
