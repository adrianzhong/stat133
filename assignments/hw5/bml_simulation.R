#################################################################################

bml.init <- function(r, c, p){
  m=matrix(0,r,c)
  m=apply(m,c(1,2),function(x) sample(c(0,1,2),1,prob=c(1-p,p/2,p/2)))
  return(m)
}


#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

bml.step <- function(m){
  initial_m1=m
  for (i in 1:nrow(m)) {
    for (j in 1:(ncol(m)-1)){
      if (initial_m1[i,j]==1 & initial_m1[i,j+1]==0){
        m[i,j+1]=1
        m[i,j]=0
      }
    }
    if (initial_m1[i,ncol(m)]==1 & initial_m1[i,1]==0){
      m[i,1]=1
      m[i,ncol(m)]=0
    }    
  }
  initial_m2=m
  for (j in 1:ncol(m)){
    for (i in nrow(m):2){
      if (initial_m2[i,j]==2 & initial_m2[i-1,j]==0){
        m[i-1,j]=2
        m[i,j]=0
      }
    }
    if (initial_m2[1,j]==2 & initial_m2[nrow(m),j]==0){
      m[nrow(m),j]=2
      m[1,j]=0
    }
  }  
  if(any(m!=initial_m1))
    grid.new=T
  else
    grid.new=F
  return(list(m, grid.new))  
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  initial_m=bml.init(r,c,p)
  new_m=list(initial_m,T)
  max_iteration=1000
  for (i in 1:max_iteration){
    new_m=bml.step(new_m[[1]])
    if (!(new_m[[2]]))
      break
  }
  return (list(i,new_m[[1]],new_m[[2]]))
}



#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.

prob = seq(0.4, 0.9, by=0.05)
B = 100 #experiment times
result.matrix = matrix(nrow=B, ncol=length(prob))
for (j in 1:length(prob)){
  result.matrix[,j] = replicate(B, bml.sim(5,5,0.35+j*0.05)[[3]])
}
colnames(result.matrix) <- prob


counting_true=function(x){
  count=0
  for (i in 1:length(x)){
    if (x[i]==T)
      count=count+1    
  }
  return (count)
}

summary_true=function(x){
  count=c()
  for (j in 1:ncol(x))
    count=c(count,counting_true(x[,j]))
  return (count/nrow(x))
}



result.matrix2 = matrix(nrow=B, ncol=length(prob))
for (j in 1:length(prob)){
  result.matrix2[,j] = replicate(B, bml.sim(5,5,0.35+j*0.05)[[1]])
}
colnames(result.matrix2) <- prob

##########question 3
size_prob=function(r,c){
  v=c()
  for (i in 1:25){
    print(i)
    v=c(v, bml.sim(r,c,0.4)[[3]])
  }
  return(v)
}
size_10=size_prob(10,10)  #48
size_25=size_prob(25,25)   #48
size_50=size_prob(50,50)    #38
size_100=size_prob(100,100)  #23
size_250=size_prob(250,250)  
shape1=size_prob(40,40)
shape2=size_prob(80,20)
shape3=size_prob(160,10)
shape4=size_prob(320,5)


