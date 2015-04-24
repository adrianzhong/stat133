xUnique = 1:5
trueCoeff = c(0, 1, 1)

getData = function(coefs = c(0, 1, 1), xs = 1:5, dupl = 10,
                   sd = 5, seed=2222){
  ### This function creates the artificial data
  set.seed(seed)
  x = rep(xs, each = dupl)
  y = coefs[1] + coefs[2]*x + coefs[3] * x^2 + 
      rnorm(length(x), 0, sd)
  return(data.frame(x, y))
}

### 
genBootY = function(x, y, rep = TRUE){
  ### For each unique x value, take a sample of the
  ### corresponding y values, with replacement.
  ### Return a vector of random y values the same length as y
  ### You can assume that the xs are sorted
  ### Hint use tapply here!
  fun=function(y)
    sample(y,length(y),replace=T)
  return(as.vector(unlist(tapply(y,x,fun))))  
}

genBootR = function(fit, err, rep = TRUE){
  ### Sample the errors 
  ### Add the errors to the fit to create a y vector
  ### Return a vector of y values the same length as fit
  ### HINT: It can be easier to sample the indices than the values
  return(fit+sample(err,length(fit)))
}

fitModel = function(x, y, degree = 1){
  ### use the lm function to fit a line or a quadratic 
  ### e.g. y ~ x or y ~ x + I(x^2)
  ### y and x are numeric vectors of the same length
  ### Return the coefficients as a vector 
  ### HINT: Take a look at the repBoot function to see how to use lm()
  if (degree==1)
    coeff=lm(y~x)$coefficients
  else
    coeff=lm(y~x+I(x^2))$coefficients
  return(coeff)
}

oneBoot = function(data, fit = NULL, degree = 1){
  ###  data are either your data (from call to getData)
  ###  OR fit and errors from fit of line to data
  ###  OR fit and errors from fit of quadratic to data  
  ### Use fitModel to fit a model to this bootstrap Y 
  if (is.null(fit))
    bootstrapY=genBootY(data[,1],data[,2])
  else
    bootstrapY=genBootR(fit[,1],fit[,2])
  return (fitModel(data[,1],bootstrapY,degree))
}

repBoot = function(data, B = 1000){
  
  ### Set up the inputs you need for oneBoot, i.e.,
  ### create errors and fits for line and quadratic

  ### replicate a call to oneBoot B times
  ### format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a data frame with B rows
  ### and one or two columns, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list  
  ### Replicate a call to oneBoot B times for 
  ### each of the four conditions
  lresi=lm(data[,2]~data[,1])$residuals
  qresi=lm(data[,2]~data[,1]+I(data[,1]^2))$residuals
  lfit=lm(data[,2]~data[,1])$fitted.values
  qfit=lm(data[,2]~data[,1]+I(data[,1]^2))$fitted.values
  l=matrix(c(lfit,lresi),ncol=2)
  q=matrix(c(qfit,qresi),ncol=2)
  no1=c()
  no2=c()
  no3=c()
  no4=c()
  for (i in 1:B)
    no1=c(no1,oneBoot(data,fit=NULL,degree=1))
  for (i in 1:B)
    no2=c(no2,oneBoot(data,fit=NULL,degree=2))
  for (i in 1:B)
    no3=c(no3,oneBoot(data,fit=l,degree=1))
  for (i in 1:B)
    no4=c(no4,oneBoot(data,fit=q,degree=2))
  no1=as.data.frame(matrix(no1,ncol=2,byrow=T))
  no2=as.data.frame(matrix(no2,ncol=3,byrow=T))
  no3=as.data.frame(matrix(no3,ncol=2,byrow=T))
  no4=as.data.frame(matrix(no4,ncol=3,byrow=T))
  coeff=list(no1,no2,no3,no4)
  return(coeff)
} 



bootPlot = function(x, y, coeff, trueCoeff){
  ### x and y are the original data
  ### coeff is a matrix from repBoot
  ### trueCoeff contains the true coefficients 
  ### that generated the data
  
  ### Make a scatter plot of data
  plot(x,y)
  ### Add lines or curves for each row in coeff
  ### Use transparency
  ### You should use mapply to construct all 
  ### 1000 of the bootstrapped lines of best fit 
  ### Have a look at ?mapply for details.
  ### This can be done in ggplot2 or base graphics.
  if (ncol(coeff)==2){
    mapply(abline,coeff[,1],coeff[,2],col=rgb(1,0.2,0.8,alpha=0.5))
  }
  else{
    mapply(function(a,b,c){curve(c*x^2+b*x+a,add=T,col=rgb(1,0.2,0.8,alpha=0.5))},
           coeff[,1],coeff[,2],coeff[,3])
  }
  ### Use trueCoeff to add true line/curve - 
  ###  Make the true line/curve stand out
  curve(trueCoeff[3]*x^2+trueCoeff[2]*x+trueCoeff[1],add=T,col=rgb(0,0,0))
}

### Run your simulation by calling this function
### This function doesn't need any changing
runSim = function() {
  xUnique = 1:5
  trueCoeff = c(0, 1, 1)
  myData = getData(coefs = trueCoeff, xs = xUnique)
  expt = repBoot(data = myData)
  par(mfrow = c(2, 2))
  for (i in 1:4){
   bootPlot(myData$x, myData$y, 
            coeff = expt[[i]], trueCoeff) 
  }
  return(expt)
}
