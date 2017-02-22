#Running around function
wandering<-function(agents){
  #Select each agent randomly
  randIndex = sample(nrow(agents))
  for(i in 1:nrow(agents)){
    #For each agent
    ind<-randIndex[i]
    #Decide a direction
    ...
    #Compute where they are going (assuming a distance to be travelled equal to 1)
    ...
    #Make sure that the agents stay in the matrix
    ...
  }
  #Sends the agents matrix back (after having changed their characteristics)
  return(agents)
}

#Function computing who's around
say.hi<-function(agents,radius){
  #Select each agent randomly
  randIndex = sample(nrow(agents))
  for(i in 1:nrow(agents)){
    ind<-randIndex[i]
    #Look who's around
    agentsInSight<-which(withinCircle(agents,radius,agents$x[ind],agents$y[ind]))
    #Remove you
    agentsInSight<-agentsInSight[agentsInSight!=ind]
    #Change the color
    agents$col[ind]<-length(agentsInSight)
  }
  return(agents)
}

#Function that looks who's around in a radius
withinCircle<-function(agents,radius,x,y){
  #Build the results
  result<-c()
  #For each agent in the space
  for(i in 1:nrow(agents)){
    #Look whether they are too far or not
    if(...)
      result<-c(result,TRUE)
    else
      result<-c(result,FALSE)
  }
  #Send back the results
  return(result)
}

runModel<-function(nTime=100,nAgents=10,radius=10){
  #Creates agent matrix
  agents<-as.data.frame(matrix(NA,nrow=nAgents,ncol=3,dimnames=list(NULL,c("x","y","col"))))
  #Initialise agents randomly in the matrix
  agents$x<-runif(nAgents,-10,10)
  agents$y<-runif(nAgents,-10,10)
  #Simulations
  for(t in 1:nTime){
    #Have agents running around
    agents<-wandering(agents)
    #Have agents say hi
    agents<-say.hi(agents,radius)
    #Show where they are
    layout(matrix(c(1), 1, 1))
    layout.show(1)
    plot(agents$x,agents$y,col=agents$col,type='p',main="Agents position",lwd=2,xlab="",ylab="",ylim = c(-10,10),xlim=c(-10,10))
    Sys.sleep(2)
  }
}

runModel(nTime=20,nAgents=10,radius=10)
