#Running around function
wandering<-function(agents){
  #Select each agent randomly
  randIndex = sample(nrow(agents))
  for(i in 1:nrow(agents)){
    #For each agent
    ind<-randIndex[i]
    #Decide a direction
    angle<-runif(1,min= 0,max= 2)*pi
    #Compute where they are going (assuming a distance to be travelled equal to 1)
    agents$x[ind]<-agents$x[ind]+cos(angle)
    agents$y[ind]<-agents$y[ind]+sin(angle)
    #Make sure that the agents stay in the matrix
    if(agents$x[ind]>10)
      agents$x[ind]<-agents$x[ind]-20
    if(agents$x[ind]<(-10))
      agents$x[ind]<-agents$x[ind]+20
    if(agents$y[ind]>10)
      agents$y[ind]<-agents$y[ind]-20
    if(agents$y[ind]<(-10))
      agents$y[ind]<-agents$y[ind]+20
    
  }
  #Sends the agents matrix back (after having changed their characteristics)
  return(agents)
}

say.hi<-function(agents,radius){
  randIndex = sample(nrow(agents))
  for(i in 1:nrow(agents)){
    ind<-randIndex[i]
    agentsInSight<-which(withinCircle(agents,radius,agents$x[ind],agents$y[ind]))
    agentsInSight<-agentsInSight[agentsInSight!=ind]
    agents$col[ind]<-length(agentsInSight)
  }
  return(agents)
}

withinCircle<-function(agents,radius,x,y){
  result<-c()
  for(i in 1:nrow(agents)){
    if(sqrt((agents$x[i]-x)^2+(agents$y[i]-y)^2)<=radius)
      result<-c(result,TRUE)
    else
      result<-c(result,FALSE)
  }
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
