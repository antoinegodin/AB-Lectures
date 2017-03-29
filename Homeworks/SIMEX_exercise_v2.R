runModel<-function(nHH=100,nF=10,Time = 100,mc=1,plot=F,prop.income=0.6,prop.wealth=0.4,tax.rate=0.2,govt.exp=20,exp.adapt=0.5,wage=1,timeShock=50,shockValue=1,exp.income=0){
	
	# nHH=100
	# nF=10
	# Time = 100
	# mc=1
	# plot=F
	# prop.income=0.6
	# prop.wealth=0.4
	# tax.rate=0.2
	# govt.exp=20
	# exp.adapt=0.5
	# wage=1
	# timeShock=50
	# shockValue=1
	
	#INITIALISATION
	
	#Data Structure
	
	G = matrix(data = 0, nrow = mc, ncol = Time)
	C = matrix(data = 0, nrow = mc, ncol = Time)
	WB = matrix(data = 0, nrow = mc, ncol = Time)
	Y = matrix(data = 0, nrow = mc, ncol = Time)
	N = matrix(data = 0, nrow = mc, ncol = Time)
	UR = matrix(data = 0, nrow = mc, ncol = Time)
	Q = matrix(data = 0, nrow = mc, ncol = Time)
	Tax = matrix(data = 0, nrow = mc, ncol = Time)
	YD = matrix(data = 0, nrow = mc, ncol = Time)
	H = matrix(data = 0, nrow = mc, ncol = Time)
	YDe = matrix(data = 0, nrow = mc, ncol = Time)
	
	agents<-list()
	
	# Matrix Column names
	hhColNames<-c("wealth","wage","employment","propensity.income","propensity.wealth","tax.rate","consumption","expected.income","adaptive.expectation")
	fColNames<-c("productivity","price","employee","production","wealth","av.wage")
	gColNames<-c("debt","govSpending")
	
	#THIS IS THE MONTE CARLO REPETITION
	for(m in 1:mc){
	
		# m=1	
		set.seed(m)
		#MONTE CARLO SIMULATION INITIALISATION
		
		# Creating Matrices
		households<-list()
		for(i in 1:length(hhColNames))
			households[[hhColNames[i]]]<-as.data.frame(matrix(NA,ncol=nHH,nrow=Time))
		firms<-list()
		for(i in 1:length(fColNames))
			firms[[fColNames[i]]]<-as.data.frame(matrix(NA,ncol=nF,nrow=Time))
		government<-list()
		for(i in 1:length(gColNames))
			government[[gColNames[i]]]<-as.data.frame(matrix(NA,ncol=1,nrow=Time))
		
		#SETTING INITIAL VALUES
		households$wealth[]<-0
		households$employment[]<-0
		households$expected.income[]<-exp.income
		households$consumption[]<-0
		households$wage[]<-wage
		households$propensity.income[]<-prop.income
		households$propensity.wealth[]<-prop.wealth
		households$tax.rate[]<-tax.rate
		households$adaptive.expectation[]<-exp.adapt
		
		firms$employee[]<-0
		firms$production[]<-0
		firms$wealth[]<-0
		firms$av.wage[]<-wage
		firms$price[]<-1
		firms$productivity[]<-1
		
		government$debt[]<-0
		government$govSpending[]<-govt.exp
		
		#THIS IS THE PERIOD REPETITION WITHIN 1 MONTE CARLO SIMULATION
		for(t in 2:Time){
			
			# t=3
			
			#EPECTATIONS BASED ON LAGGED VALUES THAT CANNOT BE RE-INITIALISED
			households<-buildExpectation(households,t)
			firms<-pricingDecision(firms,t)
			
			
			#PERIOD INITIALISATION - NO NEED ANYMORE
			# firms$production<-0
			# firms$employee<-0
			# households$employment<-0
			firms$wealth[t,]<-firms$wealth[t-1,]
			households$wealth[t,]<-households$wealth[t-1,]
			government$debt[t,]<-government$debt[t-1,]
			
			#EXAMPLE OF SHOCK
			if(t>timeShock){
				households$propensity.income[t,]<-households$propensity.income[t,]*shockValue
			}
				
			
			#PERIOD SIMULATION
			returnFromConsumption<-consumption(households=households,firms=firms,t=t)
			households<-returnFromConsumption$households
			firms<-returnFromConsumption$firms
			returnFromGovt<-governmentSpending(government = government,firms=firms,t=t)
			government<-returnFromGovt$government
			firms<-returnFromGovt$firms
			returnFromLabor<-laborMarket(firms=firms,households = households,t=t)
			firms<-returnFromLabor$firms
			households<-returnFromLabor$households
			returnFromTax<-taxation(households=households,government = government,t=t)
			households<-returnFromTax$households
			government<-returnFromTax$government
			
			#DATA COLLECTION AND PLOTS
			#DATA COLLECTION
			
			G[m,t] = government$govSpending[t,]
			C[m,t] = sum(households$consumption[t,])
			WB[m,t] = sum(firms$employee[t,]*firms$av.wage[t,])
			Y[m,t] = C[m,t]+G[m,t]
			N[m,t] = sum(firms$employee[t,])
			UR[m,t] = length(households$employment[t,which(households$employment[t,]==0)])/nHH
			Q[m,t] = sum(firms$production[t,])
			H[m,t] = sum(households$wealth[t,])
			YDe[m,t] = sum(households$expected.income[t,])
			
			if(plot){
				#Ploting results
				layout(matrix(c(1,2,3,4), 2, 2))
				layout.show(4)
				
				# plot(1:t, G[m,1:t],type="l",ylim=range(G),col=1,ylab="Gov Spending",xlab="")
				
				# plot(1:t,C[m,1:t] + G[m,1:t],type="l",ylim=range(G+C),col=1,ylab="Output",xlab="")
			
			# plot(1:Time,Tax[1:Time,1],type="l",ylim=range(TAXwage+TAXProf),col=1,ylab="Taxes",xlab="")
			
			# plot(1:Time,YD[1:Time,1],type="l",ylim=range(YD),col=1,ylab="Disposable income",xlab="")
			
			plot(1:t,C[m,1:t],type="l",ylim=range(C),col=1,ylab="Consumption",xlab="")
			
			# plot(1:Time,G[1:Time,1] - Tax[1:Time,1],type="l",ylim=range(G-Tax),col=1,ylab="Public Deficit",xlab="")
			
			plot(1:t,YDe[m,1:t],type="l",ylim=range(YDe),col=1,ylab="Households Expected income",xlab="")
			
			plot(1:t,H[m,1:t],type="l",ylim=range(H),col=1,ylab="Wealth",xlab="")
			
			plot(1:t,UR[m,1:t],type="l",ylim=range(UR),col=1,ylab="Unemployment rate",xlab="")
			 Sys.sleep(0.2)
			}
		}
		agents[[m]]<-list("households"=households,"firms"=firms,"government"=government)
	}

	return(list("G"=G,"C"=C,"WB"=WB,"Y"=Y,"N"=N,"UR"=UR,"Q"=Q,"Tax"=Tax,"YD"=YD,"H"=H,"YDe"=YDe,"agents"=agents))
}





# Expectation on income: backward looking + random element, min=0

buildExpectation<-function(households=stop("Need to have households defined!"),t=stop("Need a time")){
	# Expected income = expected income(-1)-adapative*(expected income(-1)-wage*employment)
	households$expected.income[t,]<-households$expected.income[t-1,]-households$adaptive.expectation[t,]*(households$expected.income[t-1,]-households$employment[t-1,]*households$wage[t-1,]*(1-households$tax.rate[t-1,]))
	return(households)
}

# Pricing decision = unit costs = wage/productivity

pricingDecision<-function(firms=stop("Need to have firms defined!"),t=stop("Need a time")){
	firms$price[t,]<-firms$av.wage[t-1,]/firms$productivity[t,]
	return(firms)
}

# Households consumption (with money transfer)

consumption<-function(firms=stop("Need to have firms defined!"),households=stop("Need to have households defined!"),t=stop("Need a time")){
	#1. Compute consumption
	households$consumption[t,]<-floor(pmin(households$expected.income[t,]*households$propensity.income[t,]+households$wealth[t-1,]*households$propensity.wealth[t,],households$wealth[t-1,]))
	#2. Distribute the consumption to firms
	#a. Random sample of households
	randomHouseholds<-sample(ncol(households$wealth),ncol(households$wealth))
	for(i in 1:ncol(households$wealth)){
		#b. for the randomly selected household
		indexHH<-randomHouseholds[i]
		#c. if there is consupmtion to be done
		if(households$consumption[t,indexHH]>0){
			# d. do the consumption
			for(j in 1:households$consumption[t,indexHH]){
				#e. randomly select a supplier
				indexSupplier<-sample(ncol(firms$wealth),1)
				#f. do the transactions
				households$wealth[t,indexHH]<-households$wealth[t,indexHH]-firms$price[t,indexSupplier]
				firms$wealth[t,indexSupplier]<-firms$wealth[t,indexSupplier]+firms$price[t,indexSupplier]
				firms$production[t,indexSupplier]<-firms$production[t,indexSupplier]+1
			}
		}
	}
	return(list("firms"=firms,"households"=households))
}

# Government spending (with money transfer)

governmentSpending<-function(firms=stop("Need to have firms defined!"),government=stop("Need to have government defined!"),t=stop("Need a time")){
	# distribute spending to random firms until spending is exhausted
	for(i in 1:government$govSpending[t,]){
		indexF <- sample(ncol(firms$wealth),1)
		firms$wealth[t,indexF]=firms$wealth[t,indexF]+firms$price[t,indexF]
		government$debt[t,]=government$debt[t,]+firms$price[t,indexF]
		firms$production[t,indexF]=firms$production[t,indexF]+1
	}
	return(list("firms"=firms,"government"=government))
}


# Hiring decision (random process with mutliple hire allowed, with money transfer)

laborMarket<-function(firms=stop("Need to have firms defined!"),households=stop("Need to have households defined!"),t=stop("Need a time")){
	randomFirms<-sample(ncol(firms$wealth),ncol(firms$wealth))
	for(i in 1:ncol(firms$wealth)){
		indexF<-randomFirms[i]
		wagebill<-0
		if(firms$production[t,indexF]>0){
			for(j in 1:firms$production[t,indexF]){
				indexLabour<-sample(ncol(households$wealth),1)
				wagebill<-wagebill+households$wage[t,indexLabour]
				households$wealth[t,indexLabour]<-households$wealth[t,indexLabour]+households$wage[t,indexLabour]
				firms$wealth[t,indexF]<-firms$wealth[t,indexF]-households$wage[t,indexLabour]
				firms$employee[t,indexF]<-firms$employee[t,indexF]+1
				households$employment[t,indexLabour]<-households$employment[t,indexLabour]+1
			}
		}
		if(wagebill!=0){
			firms$av.wage[t,indexF]<-wagebill/firms$employee[t,indexF]
		}
	}
	return(list("firms"=firms,"households"=households))
}

# Production

# production<-function(firms=stop("Need to have firms defined!"),households=stop("Need to have households defined!"),government=stop("Need to have government defined!")){
# 	...
# 	return(list("firms"=firms,"households"=households,"government"=government))
# }

# Taxes

taxation<-function(households=stop("Need to have households defined!"),government=stop("Need to have government defined!"),t=stop("Need a time")){
	government$debt[t,]<-government$debt[t,]-sum(households$wage[t,]*households$employment[t,]*households$tax.rate[t,])
	households$wealth[t,]<-households$wealth[t,]-households$wage[t,]*households$employment[t,]*households$tax.rate[t,]
	return(list("households"=households,"government"=government))
}


