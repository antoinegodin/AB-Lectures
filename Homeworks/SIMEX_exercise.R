#INITIALISATION

#Parameters Initialisation
nHH=100
nF=10
Time = 100
mc=1

#Behavioral Parameters

#Data Structure

G = matrix(data = 0, ncol = mc, nrow = Time)
C = matrix(data = 0, ncol = mc, nrow = Time)
WB = matrix(data = 0, ncol = mc, nrow = Time)
Y = matrix(data = 0, ncol = mc, nrow = Time)
N = matrix(data = 0, ncol = mc, nrow = Time)
UR = matrix(data = 0, ncol = mc, nrow = Time)
Q = matrix(data = 0, ncol = mc, nrow = Time)
Tax = matrix(data = 0, ncol = mc, nrow = Time)
YD = matrix(data = 0, ncol = mc, nrow = Time)
H = matrix(data = 0, ncol = mc, nrow = Time)


# Matrix Column names
hhColNames<-c("wealth","wage","employment","propensity.income","propensity.wealth","tax.rate","consumption","expected.income","adaptive.expectation")
fColNames<-c("productivity","price","employee","production")
gColNames<-c("debt","govSpending")

# Creating Matrices
households<-as.data.frame(matrix(NA,nrow=nHH,ncol=length(hhColNames),dimnames=list(NULL,hhColNames)))
firms<-as.data.frame(matrix(NA,nrow=nF,ncol=length(fColNames),dimnames=list(NULL,fColNames)))
government<-as.data.frame(matrix(NA,nrow=1,ncol=length(gColNames),dimnames=list(NULL,gColNames)))

#SIMULATION

# Expectation on income: backward looking + random element, min=0

buildExpectation<-function(households=stop("Need to have households defined!")){
	# Expected income = expected income(-1)+adapative*(expected income(-1)-wage*employment)
	households$expected.income=households$expected.income+households$adaptive.expectation*(households$expected.income-households$employment*households$wage)
	return(households)
}

# Pricing decision = unit costs = wage/productivity

pricingDecision<-function(firms=stop("Need to have firms defined!")){
	firms$price<-...
	return(firms)
}

# Households consumption (with money transfer)

consumption<-function(firms=stop("Need to have firms defined!"),households=stop("Need to have households defined!")){
	...
	return(list(firms=firms,households=households))
}

# Government spending (with money transfer)

governmentSpending<-function(firms=stop("Need to have firms defined!"),government=stop("Need to have government defined!")){
	...
	return(list(firms=firms,government=government))
}

# Hiring decision (random process with mutliple hire allowed, with money transfer)

laborMarket<-function(firms=stop("Need to have firms defined!"),households=stop("Need to have households defined!")){
	...
	return(list(firms=firms,households=households))
}

# Production

production<-function(firms=stop("Need to have firms defined!"),households=stop("Need to have households defined!"),government=stop("Need to have government defined!")){
	...
	return(list(firms=firms,households=households,government=government))
}

# Taxes

taxation<-function(households=stop("Need to have households defined!"),government=stop("Need to have government defined!")){
	...
	return(list(households=households,government=government))
}

#DATA COLLECTION

G = ...
C = ...
WB = ...
Y = ...
N = ...
UR = ...
Q = ...
Tax = ...
YD = ...
H = ...

#Ploting results
layout(matrix(c(1, 4, 7, 2, 5, 8, 3, 6, 9), 3, 3))
layout.show(9)

plot(1:Time, G[1:Time,1],type="l",ylim=range(G),col=1,ylab="Gov Spending",xlab="")

plot(1:Time,C[1:Time,1] + G[1:Time,1],type="l",ylim=range(G+C),col=1,ylab="Output",xlab="")

plot(1:Time,Tax[1:Time,1],type="l",ylim=range(TAXwage+TAXProf),col=1,ylab="Taxes",xlab="")

plot(1:Time,YD[1:Time,1],type="l",ylim=range(YD),col=1,ylab="Disposable income",xlab="")

plot(1:Time,C[1:Time,1],type="l",ylim=range(C),col=1,ylab="Consumption",xlab="")

plot(1:Time,G[1:Time,1] - Tax[1:Time,1],type="l",ylim=range(G-Tax),col=1,ylab="Public Deficit",xlab="")

plot(1:Time,YD[1:Time,1] - C[1:Time,1],type="l",ylim=range(YD-C),col=1,ylab="Households Savings",xlab="")

plot(1:Time,H[1:Time,1],type="l",ylim=range(H),col=1,ylab="Wealth",xlab="")

plot(1:Time,UR[1:Time,1],type="l",ylim=range(UR),col=1,ylab="Unemployment rate",xlab="")
