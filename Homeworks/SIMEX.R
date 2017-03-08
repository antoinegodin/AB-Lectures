#INITIALISATION

#Parameters Initialisation
nHH=100
nF=10

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

# Households consumption (with money transfer)

# Government spending (with money transfer)

# Hiring decision (random process with mutliple hire allowed, with money transfer)

# Production

# Taxes

#DATA COLLECTION