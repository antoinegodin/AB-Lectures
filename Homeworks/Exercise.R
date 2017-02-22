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

runModel<-function(nTime=100,nAgents=10){
  #Creates agent matrix
  agents<-as.data.frame(matrix(NA,nrow=nAgents,ncol=3,dimnames=list(NULL,c("distance","x","y"))))
  #Initialise agents randomly in the matrix
  #Assigning random values
  for(i in 1:nAgents){
  	agents$x[i]<-runif(1,-10,10)
  	agents$y[i]<-runif(1,-10,10)
  }
  agents$distance<-runif(nAgents,0,2)
  #Simulations
  for(t in 1:nTime){
    #Have agents running around
    agents<-wandering(agents)
    #Show where they are
    layout(matrix(c(1), 1, 1))
    layout.show(1)
    plot(...,type='p',main="Agents' position",xlab="",ylab="",ylim = c(-10,10))
    Sys.sleep(0.1)
  }
}

runModel(nTime=20,nAgents=10)