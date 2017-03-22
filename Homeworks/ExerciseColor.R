#Running around function
wandering<-function(agents){
	#Select each agent randomly
	randIndex = sample(nrow(agents))
	for(i in 1:nrow(agents)){
		#For each agent
		ind<-randIndex[i]
		#Decide a direction
		angle<-runif(1,0,2*pi)
		#Compute where they are going (assuming a distance to be travelled equal to 1)
		agents$x[ind]<-agents$x[ind]+cos(angle)*agents$distance[ind]
		agents$y[ind]<-agents$y[ind]+sin(angle)*agents$distance[ind]
		#Make sure that the agents stay in the matrix
		if(agents$x[ind]>10)
			agents$x[ind]<-agents$x[ind]-20
		else if(agents$x[ind]<(-10))
			agents$x[ind]<-agents$x[ind]+20
		if(agents$y[ind]>10)
			agents$y[ind]<-agents$y[ind]-20
		else if(agents$y[ind]<(-10))
			agents$y[ind]<-agents$y[ind]+20
		
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
    agents$col[ind]<-1+length(agentsInSight)
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
    if(sqrt((agents$x[i]-x)^2+(agents$y[i]-y)^2)<=radius)
      result<-c(result,TRUE)
    else
      result<-c(result,FALSE)
  }
  #Send back the results
  return(result)
}

runModel<-function(nTime=100,nAgents=10,radius=5){
	#Creates agent matrix
	agents<-as.data.frame(matrix(NA,nrow=nAgents,ncol=4,dimnames=list(NULL,c("distance","x","y","col"))))
	#Initialise agents randomly in the matrix
	#Assigning random values
	for(i in 1:nAgents){
		agents$x[i]<-runif(1,-10,10)
		#agents$x[i]<-1
		agents$y[i]<-runif(1,-10,10)
		#agents$y[i]<-15
	}
	agents$distance<-runif(nAgents,0,2)
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
    text(agents$x+0.4,agents$y+0.4,labels=c(1:10))
    Sys.sleep(5)
  }
}

runModel(nTime=20,nAgents=10,radius=1)
