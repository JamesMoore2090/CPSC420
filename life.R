rm(list = ls())
# Conway's Game of Life -- version 1.0
# CPSC 420 -- spring 2016

library(doParallel)
registerDoParallel(16)

# Possible states of CA cells.
EMPTY <- 0
POP <- 1

# Dimensions of CA.
height <- 30
width <- 30
num.gen <- 100

#NYC.END.ROW <- 15
#NYC.START.COL <- 15
stayAlive <- c(2,3)
shouldSpawn <- c(3,4,5)
#probs <- seq(0,1,.1)
num.trials <- 50

prob <- .2


# Return the number of populated neighbors of (row,col) in the current
# generation. (If (row,col) is a border cell of the grid, assume its
# non-existent across-the-border neighbors are *not* populated.)
num.neighbros <- function(da.loaf,row,col,gen) {
  bros <- 0
  if (row > 1 && col > 1 && da.loaf[row-1,col-1,gen] == POP) {
    bros <- bros + 1
  }
  if (row > 1 && da.loaf[row-1,col,gen] == POP) {
    bros <- bros + 1
  }
  if (row > 1 && col < width && da.loaf[row-1,col+1,gen] == POP) {
    bros <- bros + 1
  }
  if (col > 1 && da.loaf[row,col-1,gen] == POP) {
    bros <- bros + 1
  }
  if (col < width && da.loaf[row,col+1,gen] == POP) {
    bros <- bros + 1
  }
  if (row < height && col > 1 && da.loaf[row+1,col-1,gen] == POP) {
    bros <- bros + 1
  }
  if (row < height && da.loaf[row+1,col,gen] == POP) {
    bros <- bros + 1
  }
  if (row < height && col < width && da.loaf[row+1,col+1,gen] == POP) {
    bros <- bros + 1
  }
  return(bros)
}

# Return true if the cell in (row,col) of the previous generation was
# populated.
was.just.alive <- function(da.loaf,row,col,current.gen) {
  return (da.loaf[row,col,current.gen-1] == POP)
}

# Return true if the (presumably populated) cell in (row,col) of the previous
# generation should stay populated in the current generation.
should.stay.alive <- function(da.loaf,row,col,current.gen) {
  return (num.neighbros(da.loaf,row,col,current.gen-1) %in% stayAlive)
}

# Return true if the (presumably empty) cell in (row,col) of the previous
# generation should become populated in the current generation.
should.spawn <- function(da.loaf,row,col,current.gen) {
  return (num.neighbros(da.loaf,row,col,current.gen-1) %in% shouldSpawn)
}

# Plot the grid for the generation specified.





results <- foreach (numtrials=seq(1,num.trials), .combine=rbind) %dopar% {
  
  #end.pops <- vector()
 # for (prob in 1:probs) {
    
    # A random starting configuration.
    config <- matrix(rbinom(height*width,size=1,prob=prob), 
                     nrow=height)
    
    # Our system's state.
    da.loaf <- array(EMPTY, dim=c(height,width,num.gen))
    da.loaf[,,1] <- config
    
    for (gen in 2:num.gen) {
      
      for (row in 1:height) {
        
        for (col in 1:width) {
          
          if ((was.just.alive(da.loaf,row,col,gen) && 
               should.stay.alive(da.loaf,row,col,gen))  ||
              (!was.just.alive(da.loaf,row,col,gen) && 
               should.spawn(da.loaf,row,col,gen))) {
            
            da.loaf[row,col,gen] <- POP                
          }
        }
      }
   #}
    #end.pops <- c(end.pops,sum(da.loaf[,,num.gen]))
  }
  return(data.frame(num.trials=numtrials, output=sum(da.loaf[,,num.gen])))
}

# Plot post-mortem analysis.
#plot(results$init.prob, results$final.prob, type="l", 
#     main="Prob of initial cell pop vs. final pop count")
