#know the github login info before exam
#type get-exam to get the materials from github and final exam
#minimized the terminal and open the Rstudio
#push the github when the exam ends
#bcourses pages final 
#final is similar to last year, do last year final
#can create our own cheat sheet

## Brief list of topics (may not be complete)

### I. Variable creation, subsetting and indexing
# dataframes & matrices
mtcars$disp
mtcars[ ,"disp"]
mtcars[ ,3]

mtcars[mtcars$gear==4, -1]
mtcars[mtcars$hp > 100, 1]

# (named) lists
l <- list(1,2,3)
names(l) <- c("a","b","c")

l[[1]]
l[["a"]]
l$a

# vector
vec <- c(1, "abc", TRUE)
# can be either numeric, character or logical...
vec[1]

### II. Plotting
# Get to know different plotting functions:
?plot
?lines
?abline
?symbols
?text
?hist
?points
# Knowledge of plotting parameters:
?par
?rgb   #alpha for transparency

# No ggplot
# No googleVis

### III. Loops and conditioning
# For loop:
for (i in 1:10) {print(i)}

# while loop:
i <- 1
while (i < 11){
  print(i)
  i <- i+1
}

# apply family
?apply    apply(X, MARGIN, FUN, ...)
?replicate   
?tapply   tapply(X, INDEX, FUN = NULL, ..., simplify = TRUE)
?lapply
?mapply   mapply(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE,
                 USE.NAMES = TRUE)      
?sapply   sapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)
?order return index from the lowest to highest (as default)
order(c(3,2,4,1)) ==>   4,2,1,3  # return the nth element from the lowest to hightest
sort(c(3,2,4,1))==> 1,2,3,4

lapply approximately equals to sapply(as vector as possible)
tapply(c(1,2,3,4,5,6),INDEX=c(4,5,3,2,6,1),FUN=function(x) x+1)
x=c(3,4,2,6,1,5)
tapply(x,INDEX=x,FUN=function(x) x+1)
mapply(function(a,b,c){curve(c*x^2+b*x+a,add=T,col=rgb(1,0.2,0.8,alpha=0.5))},
       coeff[,1],coeff[,2],coeff[,3])
mapply can apply to functions that have more than one variables
matrix=matrix(1:20,4,5)
matrix(matrix,1,sum)  #1 is for rows, 2 is for column, c(1,2) is for rows and columns

# Conditioning
if (){}
else{}
### IV. Function definition
# Syntax
functionName <- function(arg1, arg2){
  body <- arg1 + arg2
  return(body)
}

# Dummy
apply(mtcars, 2, function(x){x[1] + x[2]})

### V. Simulation
# Combination of indexing, looping, function definition and visualization.
# See homework for examples

# Bootstrap
n <- 100
boot_mean <- rep(0,100)
for (i in 1:n){
  dat <- sample(x=iris$Sepal.Length, size=nrow(iris), replace=T)
  boot_mean[i] <- mean(dat)
}
var(boot_mean)

### VI. Regular Expressions
# Three major functions:
?grep
?gsub
?gregexpr
> gregexpr("a","asfasfasfasfa")
[[1]]
[1]  1  4  7 10 13
attr(,"match.length")
[1] 1 1 1 1 1
attr(,"useBytes")
[1] TRUE


# Other functions:
?paste
?strsplit
?table
?substring
# Wildcards and patterns
?regex

### VII. Linear Models   linear and quadratic 
# Functions
?lm
?predict

# Fitted values, coefficients, residuals 
mod <- lm(mpg~., data=mtcars)
mod$fitted    #predicted Y values
mod$coef      #estimates for predictor and intersect
mod$residuals



#review session
grep("^d",tmp)  #get words start from d
grep("^.a",tmp)  #the seconde char is a
grep("[0-9]",tmp)  #fine number [0-9] is a category, put them in square bracket
grep("[A-z][0-9]",tmp)  #first is a letter, and then a number
grep("[[:digit:]]", tmp)  #find digit




#self review
#final
#qutient and reminder   11%%5=1   11%/%5=2
#add points to a plot points()
#paste(x,collapse=" ") can make more than 1 strings in to one long string
#plot baby y vs. x