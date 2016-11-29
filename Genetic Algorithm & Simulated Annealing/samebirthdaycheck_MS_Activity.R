rm(list=ls(all=TRUE))

sims = c(10,100,1000,10000,100000,1000000,10000000,100000000)

for ( i in 1:length(sims)){
  
  #start_time=proc.time()
  simulations = sims[i]
  
  yes = no =0

 
x=sample(1:7, simulations,replace = TRUE)
y=sample(1:7, simulations,replace = TRUE)



for (j in 1:simulations) {
  
  if(x[j]==y[j]){
    
  yes=yes+1
  }
  
    }

probability = yes/simulations

cat("probability is : ", probability, " for ",simulations, " simulations ","\n")
# exe_time=proc.time() - start_time
# print(exe_time)
    }


#########################################################


