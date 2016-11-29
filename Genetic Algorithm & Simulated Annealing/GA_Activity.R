rm(list=ls(all=TRUE))

dataset <- data.frame(item = c("pocketknife", "beans", "potatoes", "unions", 
                               "sleeping bag", "rope", "compass"), 
                      survivalpoints = c(10, 20, 15, 2, 30, 10, 30), 
                      weight = c(1, 5, 10, 1, 7, 5, 1))
weightlimit <- 20
MaxPossiblesurvivalpoints = sum(dataset$survivalpoints)

# Chromosome = c(1, 0, 0, 1, 1, 0, 0)
# gene 0 or 1

# Initital population generation

fnGenerateInitPop <- function(dataset){
  InitPopSize = 100
  initPop = as.data.frame(setNames(replicate(7,numeric(0), simplify = F), dataset$item))
  set.seed(1234)
  seeds = sample(5000:6000,InitPopSize,replace=FALSE)
  for ( i in 1:InitPopSize){
    
    set.seed(seeds[i])
    chromosome = sample(0:1,7,replace=TRUE)
    initPop[i,]= chromosome
    
  }
  
  return(initPop)
}

#We define the Objective function as follows.

fnEvaluate <- function(x){
  current_solution_survivalpoints <- x %*% dataset$survivalpoints
  current_solution_weight <- x %*% dataset$weight
  
  if (current_solution_weight > weightlimit) 
    return(0) else return(current_solution_survivalpoints)
}

# mutation : pick one position and change the value to 0 or 1 as the case may be
fnMutate<-function(individual){
  #change one value to 10-that value
  
  a=sample(1:4,1)
  individual[a]=1-individual[a]
  return(individual)
}

# Crossover : randomly select a point and swap the tails
fnCrossOver=function(p1,p2){
  a=sample(2:6,1)
  p11 = c(p1[1:a],p2[(a+1) : length(p2)])
  p12 = c(p2[1:a],p1[(a+1) : length(p1)])
  
  return(list(p11,p12))
}

#  Execute the genetic algorithm
fnRunGeneticAlgo<- function(initPop,fnEvaluate,fnMutate, 
                       fnCrossOver, mutstartProb,elitepercent, maxiterations,MaxPossiblesurvivalpoints){
  
  
  cat("max iterations =", maxiterations, "\n")
  # How many winners from each generation?
  
  origPopSize=nrow(initPop)
  topElite=round(elitepercent*origPopSize,0)
  fitN=apply(initPop,1,fnEvaluate)
  initPop=data.frame(initPop,fitN)
  #initPop=initPop[sort.list(initPop[,5]), ]
  initPop = initPop[order(-initPop$fitN),]
  # Main loop
  NewPop = initPop
  for (i in 1:maxiterations) {
    
    NewPop = NewPop[order(-NewPop$fitN),]
    ElitePop=NewPop[1:topElite,]
    print("ElitePop is:" )
    print(ElitePop)
    NewPop = NewPop[,-c(length(colnames(NewPop)))]
    NewPop = NewPop[-(1:nrow(NewPop)),1:7]
    mut=mutstartProb/i
    
    # Add mutated and bred forms of the winners
    while (nrow(NewPop)<origPopSize) {
      # Mutation
      if (runif(1,0,1)<mut) {
        c=sample(1:topElite,1)
        NewPop[nrow(NewPop)+1,]=fnMutate(ElitePop[c,1:7])
        if (nrow(NewPop)==origPopSize){break()}
      }
      
      # Crossover
      else {
        c1=sample(1:topElite,1)
        c2=sample(1:topElite,1)
        ls = (fnCrossOver(ElitePop[c1,1:7], ElitePop[c2,1:7]))
        NewPop[nrow(NewPop)+1,]=ls[[1]]
        NewPop[nrow(NewPop)+1,]=ls[[2]]
        if (nrow(NewPop)==origPopSize){break()}
      }
      
    }
    NewPop$fitN=apply(NewPop,1,fnEvaluate)
    NewPop = NewPop[order(-NewPop$fitN),]
 
    
    # stopping criteria
    if (NewPop[1,8] == MaxPossiblesurvivalpoints) {break()}
    
  }
# Print current best score

cat("Total survival points in iteration", i, " = ", NewPop[1,8], "\n")
  return(NewPop[1,])
}


fnExecuteMain <- function(dataset,fnEvaluate,fnMutate, 
                          fnCrossOver, mutstartProb,elitepercent, maxiterations,MaxPossiblesurvivalpoints){
  
  
  initPop = fnGenerateInitPop(dataset)
  solution = fnRunGeneticAlgo(initPop,fnEvaluate,fnMutate,fnCrossOver, mutstartProb,elitepercent,maxiterations,MaxPossiblesurvivalpoints)
  
  Finalsolution = as.numeric(solution[1,1:7])
  selecteditems = dataset[Finalsolution == 1, ]
  
  # solution vs available
  cat(paste(Finalsolution %*% dataset$survivalpoints, "/", sum(dataset$survivalpoints),"\n"))
  
  cat("Total Survivalpoints = ",sum(selecteditems$survivalpoints),"\n", "Total weight = ",sum(selecteditems$weight))
  return(selecteditems)
  
}

mutstartProb=0.5
elitepercent=0.2
maxiterations=10
Result = fnExecuteMain(dataset,fnEvaluate,fnMutate, 
                          fnCrossOver, mutstartProb=0.5,elitepercent=0.2, maxiterations=10,
                          MaxPossiblesurvivalpoints)





