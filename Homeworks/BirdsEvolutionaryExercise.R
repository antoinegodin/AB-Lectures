
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
		agentsInSight<-which(withinCircle(agents,radius,agents$x[ind],agents$y[ind]))
		agentsInSight<-agentsInSight[agentsInSight!=ind]
		sellers<-agentsInSight[which(agents$birds[agentsInSight]>0)]
		if(length(sellers)>0){
			for(j in 1:nTrades){
				sel<-sellers[which.min(agents$price[sellers])]
				if(agents$money[ind]>agents$price[sel]){
					agents$birds[sel]<-agents$birds[sel]-1
					agents$money[sel]<-agents$money[sel]+agents$price[sel]
					agents$birds[ind]<-agents$birds[ind]+1
					agents$money[ind]<-agents$money[ind]-agents$price[sel]
					allTradesMoney<-allTradesMoney+agents$price[sel]
					allTrades<-allTrades+1
					if(agents$birds[sel]>3)
						agents$price[sel]<-agents$price[sel]*agents$price.up[sel]
					else if(agents$birds[sel]<3)
						agents$price[sel]<-agents$price[sel]*agents$price.down[sel]
				}
			}
		}
	}
	return(list("agents"=agents,"allTrades"=allTrades,"allTradesMoney"=allTradesMoney))
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

#Function thart find all the agents whose ranking are above a certain limit
findCelebrities<-function(agents,limit){
	...
	return(...)
}

#Function that makes agents evolve
evolve<-function(agents,evolve.par){
	randIndex = sample(nrow(agents))
	for(i in 1:nrow(agents)){
		ind<-randIndex[i]
		#Probability to evolve
		if(runif(1,0,1)<evolve.par){
			...
		}
	}
	return(agents)
}

#Function that makes poor agent evolve when looking at the celebrities
evolutionary.imperative<-function(agents,limit,ratioPoor){
	randIndex = sample(nrow(agents))
	for(i in 1:nrow(agents)){
		ind<-randIndex[i]
		#If the agent is poor
		if((agents$ranking[ind]<ratioPoor*nrow(agents))&(runif(1,0,1)<0.1)){
			#Select the celebrity
			celeb<-...
			#Update the behaviours
			agents$price.up[ind]<-agents$price.up[celebrities[celeb]]
			agents$price.down[ind]<-agents$price.down[celebrities[celeb]]
			agents$price[ind]<-agents$price[celebrities[celeb]]
		}
	}
	return(agents)
}

sort.wealth<-function(agents){
	sortedAgents<-with(agents, order(wealth,decreasing=TRUE))
	for(i in 1:nrow(agents)){
		agents$ranking[sortedAgents[i]]<-i
	}
	return(agents)
}

runModel<-function(nTime=100,nTrades=1,radius=10,nAgents=10,limit=0.5,evolve.par=0.1,ratioPoor=0.3){
	#TODO celebrity ranking
	agents<-as.data.frame(matrix(NA, nrow=nAgents, ncol=9, dimnames=list(NULL, c("x", "y", "birds", "price", "money", "wealth", "ranking", "price.up", "price.down"))))
	agents$x<-runif(nAgents,-10,10)
	agents$y<-runif(nAgents,-10,10)
	agents$birds<-3
	agents$money<-100
	agents$price<-10
	agents$price.up<-1.01010101
	agents$price.down<-0.99
	tradesLine<-c()
	tradesMoneyLine<-c()
	avPriceLine<-c()
	giniLine<-c()
	maxWealthLine<-c()
	relPovertyLine<-c()
	absPovertyLine<-c()
	for(t in 1:nTime){
		allTrades<-0
		allTradesMoney<-0
		#Make agents wander
		...
		#Make agents trade
		...
		#make agents evolve
		agents<-evolve(agents,evolve.par)
		#Make agents emulate the celebrities
		agents<-evolutionary.imperative(agents,limit,ratioPoor)
		#Compute some indicators to plot
		averagePrice<-...
		allWealth<-...
		giniIndexReserve<-0
		wealthSum<-0
		lorenzPoints<-c()
		# now actually plot the Lorenz curve -- along the way, we also
		# calculate the Gini index.
		# (see the Info tab for a description of the curve and measure)
		for(i in 1:nrow(agents)){
			wealthSum<-...
			lorenzPoints<-c(((wealthSum/allWealth) * 100),lorenzPoints)
			giniIndexReserve<-giniIndexReserve + (i/nrow(agents)) - (wealthSum / allWealth)
		}
		tradesLine<-c(tradesLine,allTrades)
		tradesMoneyLine<-c(tradesMoneyLine,allTradesMoney)
		giniLine<-c(giniLine,giniIndexReserve)
		avPriceLine<-c(avPriceLine,averagePrice)
		maxWealthLine<-c(maxWealthLine,max(agents$wealth))
		relPovertyLine<-c(relPovertyLine,median(agents$wealth))
		absPovertyLine<-c(absPovertyLine,sum(which(agents$wealth<averagePrice)))
		layout(matrix(c(1,2,3,4,5,6,7,8,9), 3, 3))
		layout.show(9)
		plot(lorenzPoints,type='l',main="Lorenz Curve",xlab="",ylab="")
		plot(agents$x,agents$y,type='p',main="Agents position",xlab="",ylab="")
		plot(1:t,tradesLine,type='l',xlab="",ylab="",main="Number of Trades")
		plot(1:t,tradesMoneyLine,type='l',xlab="",ylab="",main="Nominal value of Trades")
		plot(1:t,giniLine,type='l',xlab="",ylab="",main="Gini Index")
		plot(1:t,avPriceLine,type='l',xlab="",ylab="",main="Average price of Trades")
		plot(1:t,avPriceLine,type='l',xlab="",ylab="",main="Top Wealth")
		plot(1:t,relPovertyLine,type='l',xlab="",ylab="",main="Relative Poverty")
		plot(1:t,absPovertyLine,type='l',xlab="",ylab="",main="Absolute Poverty")
		Sys.sleep(0.1)
	}
}

runModel(nTime=100,nTrade=1,radius=10,nAgents=10)
