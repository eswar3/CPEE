rm(list = ls(all = TRUE))
library(rlist)
library(base)
library(gdata)
library(Rmalschains)
library(rgp)
library(rgpui)
library(mcga)
library(DBI)
library(RMySQL)
library(ggplot2)
library(gridExtra)



##########################################
#Given data #Importing from MySQL to R

dbcon <-  dbConnect(RMySQL::MySQL(),username = "root",host = "127.0.0.1",port = 3306,dbname = "springs_clean",password = .rs.askForPassword("Enter password:"))
travel_time <- dbReadTable(dbcon, "travel_time_matrix_db")
service_time <- dbReadTable(dbcon, "stops_info_db")
penalty_db <- dbReadTable(dbcon, "parameters_info_db")

#############################################
#Service Time Visualization

stime<-service_time[7:195,]
length<-length(stime$TIME_TO_COMPLETE_WORK)
Service_Time <-stime$TIME_TO_COMPLETE_WORK

p1 <- ggplot(stime, aes(x =c(1:length) , y = stime$TIME_TO_COMPLETE_WORK))
p2<-p1 + labs(x="Stops",y = "Service Time") + scale_y_continuous("Service Time",limits=c(1,300),breaks=seq(0, 300, 50))   
p3<-p2 + geom_point(aes(color = factor(Service_Time )))  
p4<- p3+ scale_color_discrete(name ="Service Time")

plot(p4)
summary(Service_Time)

##########################################
#convert travel_time into a 195x195 matrix like dataframe
travel_time_df <- data.frame(matrix(NA, nrow = 195, ncol = 195))

#Assigning Row and Column Names
stopnames <- service_time$STOP_ID
colnames(travel_time_df) <- stopnames
row.names(travel_time_df) <- stopnames

for (i in 1:nrow(travel_time))
{
  travel_time_df[travel_time$FROM_STOP_ID[i], travel_time$TO_STOP_ID[i]] <-travel_time$TRAVEL_TIME[i]
}

#Imputing missing values(based on colmax)

colMax <- function(data) 
{
  apply(data,2,FUN=max,na.rm=TRUE )
}

max <- colMax(travel_time_df)

#Imputing for missing stops

for (i in c("STP68664","STP69424","STP69799","STP69927","STP70024","STP70151","STP70255","STP70398","STP70434"))
{
  for (j in 1:length(stopnames))
  {
    travel_time_df[i, j] <- max[j]
  }
  
}


############################################

#cluster stops nearer to depots using average distance 
depot_to_stop <-as.matrix(travel_time_df[1:6,1:195])
stop_to_depot <-t(travel_time_df[1:195,1:6])
avg_distance<-as.data.frame((depot_to_stop+stop_to_depot)/2)

#assigning depot numbers as first element (d1,d2,...d6)
cluster<-list()


for(j in 1:6)
{
  cluster[[j]]<-j
}

#clustering stops based on shortest time to travel from depots 

for(i in 7:195)
{
  
  temp <-which(avg_distance[,i] == min(avg_distance[,i]))
  
  if(length(temp)==1)
  {  
    cluster[[temp]] <-c(cluster[[temp]],i)
  }
  else
  {
    
    min<-length(stopnames)
    depot<-0
    for(ii in 1:length(temp))
    {
      if(length(cluster[[temp[ii]]])<min)
      {
        min<-length(cluster[[temp[ii]]])
        depot<-temp[ii]
      }
     }
    cluster[[depot]] <-c(cluster[[depot]],i)
  }
  
}
######################################################



######################################################
#Reading input parameters from user

input_param <- function()
{ 
  
  cat("Enter Min stops per vehicle")
  m <- as.integer(readline(prompt = ""))
  cat("Enter Max stops per vehicle")
  n <- as.integer(readline(prompt = ""))
  cat("Enter Max Route Time \n")
  o<-as.integer(readline(prompt = ""))
  cat("Enter Max_Early Time Windows\n")
  p<-as.integer(readline(prompt = ""))
  cat("Enter Max_Late Time Windows \n")
  q<-as.integer(readline(prompt = ""))
  cat("Enter Fixed Cost Per Vehicle ")
  r<-as.integer(readline(prompt = ""))
  cat("Enter Fixed Cost for Per Min Travel ")
  s<-as.integer(readline(prompt = ""))
  
  
  return(c(m,n,o,p,q,r,s))
}

#####################################################

# Calculate Total Route Time(trt) (trt =Travel Time + Service Time) 

trtcalc <-function(x)
{
  stime <-0
  ttime <-0
  for(kk in 1:(length(x)-1))
  {
    ttime=as.numeric(travel_time_df[x[kk],x[kk+1]])+ttime
    stime=service_time[x[kk],2]+stime
  }
  
  t <-stime+ttime
  
  return(t)  
}

###################################################
#Generating initial population
fn_initpop <-function(x,Minstop,Maxstop)
{
   set.seed(1234*Maxstop)
   mlist<-list()
   depot<-x[1]
   x<-setdiff(x, depot)
    for (ii in 1:InitPopSize)
   {
    
    clist <-x
    templist<-list()
    for(jj in 1: length(clist))
    {
      if (length(clist) >=MaxStops)
      {
        a <-resample(clist,size = 3,replace = FALSE)
        clist<-setdiff(clist, a)
        b<-trtcalc(c(depot,a,depot))
        zz<-trtcalc(a)
        if(b>=MaxRTime)
        {
          templist[[jj]] <-c(depot,a,depot)
        }
        else
        {
          c<-NULL
          while((length(a)<MaxStops) & (length(clist)> 0) & zz<180)
          {
            c <-resample(clist,size = 1)
            b<-trtcalc(c(depot,a,c,depot))
            zz<-trtcalc(c(a,c))
            if(b<=MaxRTime & zz<=180)
            {
              a<-c(a,c)
              clist<-setdiff(clist, a)
            }
            else
            {
              break
            }
          }
          templist[[jj]] <-c(depot,a,depot)
        }
      }
      else if ((length(clist)<MaxStops) & (length(clist) > 0))
      {
        a <- resample(clist, size =length(clist),replace = FALSE)
        templist[[jj]] <-c(depot,a,depot)
        clist<-setdiff(clist, a)
      }
      if (length(clist) == 0)
        break
    }
    mlist[[ii]]<-templist
  }
  return(mlist)
}

#################################
#We define the Penalty functions as follows.
# Max Route Time (660 mins check)

penalty1 <-function(x)
{
  
  if(x>MaxRTime)
  {
    c <-trunc((x-MaxRTime)/60)+1
    c<-(c*10*x)/100
    return(x+c)
  }
  else  
  {
    return(x)
  }
}

#Minimum Number Of Stops in a route check

penalty2 <-function(x,y)
{
  d <-length(x)-2
  if(d<MinStops)
  {
    sc<-(MinStops-d)*10
    sc<-((sc)/100)*y
    sc<-y+sc
    return(sc)
  }
  else
    return(y)
}

#vehicles should reach customers in the slot(11Am -2 PM) 

penalty3 <-function(x,y)
{
  # x is route, y is score of the route 
  if(y<=180)
  {
    return(y)
  }
  else
  {
    x<-setdiff(x,x[1])
    if(length(x)==1)
    {
      return(y)
    }
    else
    {
      stime <-0
      ttime <-0
      for(kk in 1:(length(x)-1))
      {
        ttime=as.numeric(travel_time_df[x[kk],x[kk+1]])+ttime
        stime=service_time[x[kk],2]+stime
      }
      ptime <-stime+ttime
      zz<-180+unlist(ETWindow)+unlist(LTWindow)
      
      if(ptime<=zz)
      {
        z<-y+0.1*y
        return(z)
      }
      else
      {
        diff<-(ptime-(180+ETWindow+LTWindow))/60
        diff<-trunc(diff)+1
        diff<-(diff*10*y)/100
        z<-y+diff
        return(z)
      }
    }
  }
}

###############################################
#We define the Cost/Objective function as follows.

#Final Score Per Solution
fscore <-function(x)
{
  sum(unlist(x))
}

#Route Count Per Solution

Rcount <-function(x)
{
  length(x)
}
# 3) Cost Per Solution
cost <-function(x,y)
{
  # x--Route count, Y score value  
  return((CPRoute*x)+(CPMin*y))
}
#############################################

#Function to pick elite solution

pickElite <-function(x,y)
{
  
  x[c(y)] 
  
}
##############################################################3
#function nto generate new population

fnnewgen <- function(x,y,z)
{
  
  while (length(x) < InitPopSize)
  {
    # Mutation
    if (runif(1,0,1)< z) 
    {
      parent<-resample(x,size =1)
      temp<-parent[[1]]
      t<-lapply(temp, function(z) swap(z))
      x<-list.append(x,t)
     
    }
    # Crossover
    else 
    {
      y<-setdiff(y,y[1])
      # length of elements to be swapped 
      elements_to_be_swapped<-resample(y,size = round((resample(0.90:0.95, size = 1, replace = F)*length(y)/10),0),replace = FALSE)
      #Pick 2 parents
      parents<-resample(x,size =2,replace = FALSE)
      
      while(length(elements_to_be_swapped)>1)
      {
        d <-resample(elements_to_be_swapped,size = 2, replace = FALSE)
        elements_to_be_swapped<-setdiff(elements_to_be_swapped,d)
        parents<-rapply(parents, function(z) ifelse(z == d[1],d[2],ifelse(z==d[2],d[1], z)), how = "list")
      }    
      x<-list.append(x,parents[[1]])
      x<-list.append(x,parents[[2]])
     
    }
  }

return(x)
}

###############################################

swap <-function(x)
{
  if(length(x)==3)
    return(x)
  else
  {
    temp<-x[length(x)-1]
    x[length(x)-1]<-x[2]
    x[2]<-temp
    return(x)
  }
}

####

#Print function for best results in every iteration 

bestsolution <-function(cost,best)
{
  bcost<-mapply(function(x,y) x[[unlist(y)]] ,cost,best)
  return(sum(bcost))
}

#########################################################
#  Execute the genetic algorithm

geneticAlgo<- function(PopList,maxiterations,mutProb,elite_percent)
{
  
  cat("max iterations =", maxiterations, "\n")
  
  #Applying Penalty on Inital Population
  
  rt<-rapply(PopList, function(x) trtcalc(x),how = "list")
  p1<-rapply(rt, function(x) penalty1(x),how = "list")
  p2 <-mapply(function(x,y) mapply(function(x,y) mapply(function(x,y) penalty2(x,y),x,y,SIMPLIFY=FALSE),x,y,SIMPLIFY=FALSE),PopList,p1,SIMPLIFY=FALSE)
  p3<-mapply(function(x,y) mapply(function(x,y) mapply(function(x,y) penalty3(x,y),x,y,SIMPLIFY=FALSE),x,y,SIMPLIFY=FALSE),PopList,p2,SIMPLIFY=FALSE)
  
  #Applying Cost function on Inital Population
  
  Route_count<-lapply(PopList,function(x) lapply(x,function(x) Rcount(x) ))
  f_score <-lapply(p3,function(x) lapply(x,function(x) fscore(x)))
  c<-mapply(function(x,y) mapply(function(x,y) cost(x,y),x,y,SIMPLIFY = FALSE),Route_count,f_score,SIMPLIFY=FALSE)
  sorted <-lapply(c,function(x) order(unlist(x),decreasing = FALSE))
  
  #Calculate Elite Population Percentage 
  
  elite=round(elite_percent*InitPopSize,0)
  sorted <-lapply(sorted,function(x) x[1:elite])
  
  # Main Iteration Loop
  best_cost<-c()
  best_min<-c()
  best_Route_count<-c()
  
  
  for (i in 1:maxiterations) 
  {
    
    ##Pick Elite Population
    Elitepop <-mapply(function(x,y) pickElite(x,y),PopList,sorted,SIMPLIFY = FALSE)
    
    #Mutation Probability
    mut =mutProb/i
    
    #Generate New Gen Population
    PopList<-mapply(function(x,y,z) fnnewgen(x,y,z),Elitepop,cluster,mutProb,SIMPLIFY = FALSE)
    
    #Penalty functins on new gen
    rt<- rapply(PopList, function(x) trtcalc(x),how = "list")
    p1<-rapply(rt, function(x) penalty1(x),how = "list")
  
    p2 <-mapply(function(x,y) mapply(function(x,y) mapply(function(x,y) penalty2(x,y),x,y,SIMPLIFY=FALSE),x,y,SIMPLIFY=FALSE),PopList,p1,SIMPLIFY=FALSE)
    p3<-mapply(function(x,y) mapply(function(x,y) mapply(function(x,y) penalty3(x,y),x,y,SIMPLIFY=FALSE),x,y,SIMPLIFY=FALSE),PopList,p2,SIMPLIFY=FALSE)
    
    # Cost functions on new gen
    Route_count<-lapply(PopList,function(x) lapply(x,function(x) length(x) ))
    f_score <-lapply(p3,function(x) lapply(x,function(x) fscore(x)))
    c<-mapply(function(x,y) mapply(function(x,y) cost(x,y),x,y,SIMPLIFY = FALSE),Route_count,f_score,SIMPLIFY=FALSE)
    sorted <-lapply(c,function(x) order(unlist(x),decreasing = FALSE))
    
    #Pick Elite  
    sorted <-lapply(sorted,function(x) x[1:elite])
    cat("Iteration", i, "\n")
    
    ##Print Best solution of the iterarion
    best<-sapply(sorted, `[[`, 1,simplify = F)
    best_cost[i]<- bestsolution(c,best)
    best_min[i]<-bestsolution(f_score,best)
    best_Route_count[i]<-bestsolution(Route_count,best)
    
    best_sol<-mapply(function(x,y) x[[unlist(y)]] ,PopList,best,SIMPLIFY = FALSE)
    
    print(paste0(best_sol))
    
    print(paste0("best cost in iteration ", i, "=",best_cost[i]))
    print(paste0("best Mins in iteration ", i, "=",best_min[i]))
    print(paste0("best No of routes iteration ", i, "=",best_Route_count[i]))
  }
  
  par(mfrow=c(1,2))
  
  plot(c(1:length(best_cost)),best_cost,xlab="Number of Iterations",ylab="Cost",xlim=c(1,maxiterations),ylim=c(19000,30000),type='s',col="blue",main="Fitness over time")
  
  plot(c(1:length(best_min)),best_min,xlab="Number of Iterations",ylab="Mins Of Travel",xlim=c(1,maxiterations),ylim=c(19000,30000),type='s',col="blue",main="Fitness over time")
  
  return("success")
}
###############################################

#Reading Parameters

param<-input_param()
MinStops<-param[1]
MaxStops<-param[2]
MaxRTime<-param[3]
ETWindow<-param[4]
LTWindow<-param[5]
CPRoute<-param[6]
CPMin<-param[7]

InitPopSize=200


#Generate Initial population

InitPopList<-mapply(function(x,y,z) fn_initpop(x,y,z),cluster,MinStops,MaxStops,SIMPLIFY = FALSE)

#Execute Genetic Algorithm+ 


system.time(geneticAlgo(InitPopList,200,0.3,0.35))




