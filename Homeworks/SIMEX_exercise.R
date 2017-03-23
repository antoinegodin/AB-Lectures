runModel<-function(nHH=100,nF=10,Time = 100,mc=1,plot=F,prop.income=0.6,prop.wealth=0.4,tax.rate=0.2,govt.exp=20,exp.adapt=0.5,wage=1,timeShock=50,shockValue=1){
	
	# nHH=100
	# nF=10
	# Time = 100
	# mc=1
	
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
	
	# Matrix Column names
	hhColNames<-c("wealth","wage","employment","propensity.income","propensity.wealth","tax.rate","consumption","expected.income","adaptive.expectation")
	fColNames<-c("productivity","price","employee","production","wealth","av.wage")
	gColNames<-c("debt","govSpending")
	
	#THIS IS THE MONTE CARLO REPETITION
	for(m in 1:mc){
		
		
		set.seed(m)
		#MONTE CARLO SIMULATION INITIALISATION
		
		# Creating Matrices
		households<-as.data.frame(matrix(NA,nrow=nHH,ncol=length(hhColNames),dimnames=list(NULL,hhColNames)))
		firms<-as.data.frame(matrix(NA,nrow=nF,ncol=length(fColNames),dimnames=list(NULL,fColNames)))
		government<-as.data.frame(matrix(NA,nrow=1,ncol=length(gColNames),dimnames=list(NULL,gColNames)))
		
		#SETTING INITIAL VALUES
		households$wealth<-0
		households$employment<-0
		households$expected.income<-0
		households$consumption<-0
		households$wage<-wage
		households$propensity.income<-prop.income
		households$propensity.wealth<-prop.wealth
		households$tax.rate<-tax.rate
		households$adaptive.expectation<-exp.adapt
		
		firms$employee<-0
		firms$production<-0
		firms$wealth<-0
		firms$av.wage<-wage
		firms$price<-1
		firms$productivity<-1
		
		government$debt<-0
		government$govSpending<-govt.exp
		
		#THIS IS THE PERIOD REPETITION WITHIN 1 MONTE CARLO SIMULATION
		for(t in 1:Time){
			
			#EPECTATIONS BASED ON LAGGED VALUES THAT CANNOT BE RE-INITIALISED
			households<-buildExpectation(households)
			firms<-pricingDecision(firms)
			
			
			#PERIOD INITIALISATION
			firms$production<-0
			firms$employee<-0
			households$employment<-0
			
			#EXAMPLE OF SHOCK
			if(t>timeShock){
				households$propensity.income<-households$propensity.income*shockValue
			}
				
			
			#PERIOD SIMULATION
			returnFromConsumption<-consumption(households=households,firms=firms)
			households<-returnFromConsumption$households
			firms<-returnFromConsumption$firms
			returnFromGovt<-governmentSpending(government = government,firms=firms)
			government<-returnFromGovt$government
			firms<-returnFromGovt$firms
			returnFromLabor<-laborMarket(firms=firms,households = households)
			firms<-returnFromLabor$firms
			households<-returnFromLabor$households
			returnFromTax<-taxation(households=households,government = government)
			households<-returnFromTax$households
			government<-returnFromTax$government
			
			#DATA COLLECTION AND PLOTS
			#DATA COLLECTION
			
			G[m,t] = government$govSpending
			C[m,t] = sum(households$consumption)
			WB[m,t] = sum(firms$employee*firms$av.wage)
			Y[m,t] = C[m,t]+G[m,t]
			N[m,t] = sum(firms$employee)
			UR[m,t] = nrow(households[which(households$employment==0),])/nHH
			Q[m,t] = sum(firms$production)
			H[m,t] = sum(households$wealth)
			YDe[m,t] = sum(households$expected.income)
			
			if(plot){
				#Ploting results
				layout(matrix(c(1,2,3,4), 2, 2))
				layout.show(4)
				
				# plot(1:t, G[m,1:t],type="l",ylim=range(G),col=1,ylab="Gov Spending",xlab="")
				
				plot(1:t,C[m,1:t] + G[m,1:t],type="l",ylim=range(G+C),col=1,ylab="Output",xlab="")
			
			# plot(1:Time,Tax[1:Time,1],type="l",ylim=range(TAXwage+TAXProf),col=1,ylab="Taxes",xlab="")
			
			# plot(1:Time,YD[1:Time,1],type="l",ylim=range(YD),col=1,ylab="Disposable income",xlab="")
			
			plot(1:t,C[m,1:t],type="l",ylim=range(C),col=1,ylab="Consumption",xlab="")
			
			# plot(1:Time,G[1:Time,1] - Tax[1:Time,1],type="l",ylim=range(G-Tax),col=1,ylab="Public Deficit",xlab="")
			
			# plot(1:Time,YD[1:Time,1] - C[1:Time,1],type="l",ylim=range(YD-C),col=1,ylab="Households Savings",xlab="")
			
			plot(1:t,H[m,1:t],type="l",ylim=range(H),col=1,ylab="Wealth",xlab="")
			
			plot(1:t,UR[m,1:t],type="l",ylim=range(UR),col=1,ylab="Unemployment rate",xlab="")
			 Sys.sleep(0.2)
			}
		}
		
	}

	return(list("G"=G,"C"=C,"WB"=WB,"Y"=Y,"N"=N,"UR"=UR,"Q"=Q,"Tax"=Tax,"YD"=YD,"H"=H,"YDe"=YDe))
}





# Expectation on income: backward looking + random element, min=0

buildExpectation<-function(households=stop("Need to have households defined!")){
	# Expected income = expected income(-1)-adapative*(expected income(-1)-wage*employment)
	households$expected.income<-households$expected.income-households$adaptive.expectation*(households$expected.income-households$employment*households$wage)
	return(households)
}

# Pricing decision = unit costs = wage/productivity

pricingDecision<-function(firms=stop("Need to have firms defined!")){
	firms$price<-firms$av.wage/firms$productivity
	return(firms)
}

# Households consumption (with money transfer)

consumption<-function(firms=stop("Need to have firms defined!"),households=stop("Need to have households defined!")){
	#1. Compute consumption
	households$consumption<-floor(pmin(households$expected.income*households$propensity.income+households$wealth*households$propensity.wealth,households$wealth))
	#2. Distribute the consumption to firms
	#a. Random sample of households
	randomHouseholds<-sample(nrow(households),nrow(households))
	for(i in 1:nrow(households)){
		#b. for the randomly selected household
		indexHH<-randomHouseholds[i]
		#c. if there is consupmtion to be done
		if(households$consumption[indexHH]>0){
			# d. do the consumption
			for(j in 1:households$consumption[indexHH]){
				#e. randomly select a supplier
				indexSupplier<-sample(nrow(firms),1)
				#f. do the transactions
				households$wealth[indexHH]<-households$wealth[indexHH]-firms$price[indexSupplier]
				firms$wealth[indexSupplier]<-firms$wealth[indexSupplier]+firms$price[indexSupplier]
				firms$production[indexSupplier]<-firms$production[indexSupplier]+1
			}
		}
	}
	return(list("firms"=firms,"households"=households))
}

# Government spending (with money transfer)

governmentSpending<-function(firms=stop("Need to have firms defined!"),government=stop("Need to have government defined!")){
	# distribute spending to random firms until spending is exhausted
	for(i in 1:government$govSpending){
		indexF <- sample(nrow(firms),1)
		firms$wealth[indexF]=firms$wealth[indexF]+firms$price[indexF]
		government$debt=government$debt+firms$price[indexF]
		firms$production[indexF]=firms$production[indexF]+1
	}
	return(list("firms"=firms,"government"=government))
}


# Hiring decision (random process with mutliple hire allowed, with money transfer)

laborMarket<-function(firms=stop("Need to have firms defined!"),households=stop("Need to have households defined!")){
	randomFirms<-sample(nrow(firms),nrow(firms))
	for(i in 1:nrow(firms)){
		indexF<-randomFirms[i]
		wagebill<-0
		if(firms$production[indexF]>0){
			for(j in 1:firms$production[indexF]){
				indexLabour<-sample(nrow(households),1)
				wagebill<-wagebill+households$wage[indexLabour]
				households$wealth[indexLabour]<-households$wealth[indexLabour]+households$wage[indexLabour]
				firms$wealth[indexF]<-firms$wealth[indexF]-households$wage[indexLabour]
				firms$employee[indexF]<-firms$employee[indexF]+1
				households$employment[indexLabour]<-households$employment[indexLabour]+1
			}
		}
		if(wagebill!=0){
			firms$av.wage[indexF]<-wagebill/firms$employee[indexF]
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

taxation<-function(households=stop("Need to have households defined!"),government=stop("Need to have government defined!")){
	government$debt<-government$debt-sum(households$wage*households$employment*households$tax.rate)
	households$wealth<-households$wealth-households$wage*households$employment*households$tax.rate
	return(list("households"=households,"government"=government))
}


