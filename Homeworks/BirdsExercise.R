
wandering<-function(agents){
  randIndex = sample(nrow(agents))
  for(i in 1:nrow(agents)){
    ind<-randIndex[i]
    angle<-runif(1,min= 0,max= 2)*pi
    agents$x[ind]<-agents$x[ind]+cos(angle)
    agents$y[ind]<-agents$y[ind]+sin(angle)
    if(agents$x[ind]>10)
      agents$x[ind]<-agents$x[ind]-20
    if(agents$x[ind]<(-10))
      agents$x[ind]<-agents$x[ind]+20
    if(agents$y[ind]>10)
      agents$y[ind]<-agents$y[ind]-20
    if(agents$y[ind]<(-10))
      agents$y[ind]<-agents$y[ind]+20
    
  }
  return(agents)
}

trade.birds<-function(agents,radius,nTrades,allTrades,allTradesMoney){
  randIndex = sample(nrow(agents))
  for(i in 1:nrow(agents)){
    ind<-randIndex[i]
    #Look at all agents in sight
    agentsInSight<-...
    #Look at all sellers
    sellers<-...
    #If there are sellers
    if(length(sellers)>0){
    	#For the number of trade rounds possible
      for(j in 1:nTrades){
      	#Sellect the seller
        seller<-..
        #If possible to do a sale
        if(agents$money[ind]>agents$price[seller]){
        	#Do the sale
          ...
        	#Update the price setting
					...
				}
      }
    }
  }
  return(...)
}

withinCircle<-function(agents,radius,x,y){
  result<-c()
  for(i in 1:nrow(agents)){
    if(sqrt((agents$x[i]-x)^2+(agents$y[i]-y)^2)<radius)
      result<-c(result,TRUE)
    else
      result<-c(result,FALSE)
  }
  return(result)
}

runModel<-function(nTime=100,nTrades=1,radius=10,nAgents=10){
  agents<-as.data.frame(matrix(NA,nrow=nAgents,ncol=5,dimnames=list(NULL,c("x","y","birds","price","money"))))
  agents$x<-runif(nAgents,-10,10)
  agents$y<-runif(nAgents,-10,10)
  agents$birds<-3
  agents$money<-100
  agents$price<-10
  tradesLine<-c()
  tradesMoneyLine<-c()
  avPriceLine<-c()
  giniLine<-c()
  for(t in 1:nTime){
    allTrades<-0
    allTradesMoney<-0
    #Make agents wander
    ...
    #Make agents trade
    ...
    #Compute some indicators to plot
    averagePrice<-...
    allWealth<-...
    giniIndexReserve<-0
    wealthSum<-0
    lorenzPoints<-c()
    # now actually plot the Lorenz curve -- along the way, we also
    # calculate the Gini index.
    # (see the Info tab for a description of the curve and measure)
    sortedAgents<-...
    for(i in 1:nrow(agents)){
      wealthSum<-...
      lorenzPoints<-c(((wealthSum/allWealth) * 100),lorenzPoints)
      giniIndexReserve<-giniIndexReserve + (i/nrow(agents)) - (wealthSum / allWealth)
    }
    tradesLine<-c(tradesLine,allTrades)
    tradesMoneyLine<-c(tradesMoneyLine,allTradesMoney)
    giniLine<-c(giniLine,giniIndexReserve)
    avPriceLine<-c(avPriceLine,averagePrice)
    layout(matrix(c(1,2,3,4,5,6), 2, 3))
    layout.show(6)
    plot(lorenzPoints,type='l',main="Lorenz Curve",xlab="",ylab="")
    plot(agents$x,agents$y,type='p',main="Agents position",xlab="",ylab="")
    plot(1:t,tradesLine,type='l',xlab="",ylab="",main="Number of Trades")
    plot(1:t,tradesMoneyLine,type='l',xlab="",ylab="",main="Nominal value of Trades")
    plot(1:t,giniLine,type='l',xlab="",ylab="",main="Gini Index")
    plot(1:t,avPriceLine,type='l',xlab="",ylab="",main="Average price of Trades")
    Sys.sleep(0.1)
  }
}

runModel(nTime=300,nTrade=1,radius=10,nAgents=10)
