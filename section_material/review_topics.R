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
?apply
?replicate   
?tapply      
?mapply         

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

# Other functions:
?paste
?strsplit
?table

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