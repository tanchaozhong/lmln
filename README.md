# lmln

#This is a R script created on R studio.

#It is very annoying that while fitting the linear model, still need to use abline to plot line.

#Therefore, we creat the funtion lmln() to fit linear model while draw the line.

#only can fit a single independent variable to a single respond variable
#only two colors - red (P < 0.05) and black (P > 0.05) to show
#only two line type - solid (P > 0.05) and dash (P < 0.05) to show

#lmln()
lmln <- function(y=yaxis,x=xaxis){
  #use lm()
  lm <- lm(y~x)
  #summary
  summ <- summary(lm)
  #store P.value
  p.value <- coef(summ)[2,4] 
  #store Adj R-square
  AdjR2 <- summ$adj.r.squared 
  #easy plot
  plot(y~x)
  #intercept
  a <- coef(lm)[1] 
  #slop
  b <- coef(lm)[2] 
  if(p.value<0.05){lty=1;col="red"};
  if(p.value>0.05){lty=2;col="black"}
  #plot the line y~x
  abline(a=a,b=b,lty=lty)
  #output the summary
  return(summ)
}

# Data for testing
y=c(13,35,76,5,4,45,7)
x=c(1,2,3,4,5,6,7)
lmln(y=y,x=x)
