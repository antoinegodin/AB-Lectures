source("Homeworks/SIMEX_exercise.R")

tempRes<-runModel(plot=T)
tempRes$H[100]

tempRes<-runModel(plot=F,nbHH=1)
tempRes$H[100]
avDiff=(tempRes$H[100]-80)/1


tempRes<-runModel(plot=F, nbHH=2)
tempRes$H[100]
avDiff=(tempRes$H[100]-80)/2

tempRes<-runModel(plot=F, nbHH=150)
tempRes$H[100]
avDiff=(tempRes$H[100]-80)/150

resultsTax<-list()
nbScen=5
for(i in 1:nbScen){
	resultsTax[[i]]<-runModel(mc=1,tax.rate = 0.1+0.5*(i-1)/(nbScen-1),Time=100)
	
}
#Sensistivity Analysis

nameTS<-names(resultsTax[[1]])
for(i in 1:nbScen){
	sensitivityResults<-data.frame(matrix(0,nrow=200,ncol=0))
	for(j in 1:length(resultsTax)){
		tsMC<-as.data.frame(t(resultsTax[[j]][[i]]))
		sensitivityResults[paste(nameTS[i],"scen",j,"average",sep=".")]<-rowSums(tsMC)/ncol(tsMC)
		sensitivityResults[paste(nameTS[i],"scen",j,"min",sep=".")]<-apply(tsMC,1,function(x) min(x))
		sensitivityResults[paste(nameTS[i],"scen",j,"max",sep=".")]<-apply(tsMC,1,function(x) max(x))
	}
	#jpeg(filename=paste(nameTS[i],".jpg",sep=""))
	matplot(1:100,sensitivityResults[1:100,],col=rep(1:nbScen,each=3),lwd=rep(c(2,1,1),nbScen),lty=rep(c(1,2,2),nbScen),type='l',main=nameTS[i],ylab="",xlab="")
	grid()
	#legend("center",legend=paste("exp.adapt=",((1:nbScen)-1)/(nbScen-1)),col=1:nbScen,lwd=2,lty=1)
	#dev.off()
}

resultsTax<-list()
nbScen=5
for(i in 1:nbScen){
	resultsTax[[i]]<-runModel(mc=1,tax.rate = 0.1+0.5*(i-1)/(nbScen-1),Time=100)
	
}
#Sensistivity Analysis

nameTS<-names(resultsTax[[1]])
for(i in 1:nbScen){
	sensitivityResults<-data.frame(matrix(0,nrow=200,ncol=0))
	for(j in 1:length(resultsTax)){
		tsMC<-as.data.frame(t(resultsTax[[j]][[i]]))
		sensitivityResults[paste(nameTS[i],"scen",j,"average",sep=".")]<-rowSums(tsMC)/ncol(tsMC)
		sensitivityResults[paste(nameTS[i],"scen",j,"min",sep=".")]<-apply(tsMC,1,function(x) min(x))
		sensitivityResults[paste(nameTS[i],"scen",j,"max",sep=".")]<-apply(tsMC,1,function(x) max(x))
	}
	#jpeg(filename=paste(nameTS[i],".jpg",sep=""))
	matplot(1:100,sensitivityResults[1:100,],col=rep(1:nbScen,each=3),lwd=rep(c(2,1,1),nbScen),lty=rep(c(1,2,2),nbScen),type='l',main=nameTS[i],ylab="",xlab="")
	grid()
	#legend("center",legend=paste("exp.adapt=",((1:nbScen)-1)/(nbScen-1)),col=1:nbScen,lwd=2,lty=1)
	#dev.off()
}

resultsExpect<-list()
nbScen=11
for(i in 1:nbScen){
	resultsExpect[[i]]<-runModel(mc=1,exp.adapt = (i-1)/(nbScen-1),Time=100)
	
}
#Sensistivity Analysis

nameTS<-names(resultsExpect[[1]])
for(i in 1:nbScen){
	sensitivityResults<-data.frame(matrix(0,nrow=200,ncol=0))
	for(j in 1:length(resultsExpect)){
		tsMC<-as.data.frame(t(resultsExpect[[j]][[i]]))
		sensitivityResults[paste(nameTS[i],"scen",j,"average",sep=".")]<-rowSums(tsMC)/ncol(tsMC)
		sensitivityResults[paste(nameTS[i],"scen",j,"min",sep=".")]<-apply(tsMC,1,function(x) min(x))
		sensitivityResults[paste(nameTS[i],"scen",j,"max",sep=".")]<-apply(tsMC,1,function(x) max(x))
	}
	#jpeg(filename=paste(nameTS[i],".jpg",sep=""))
	matplot(1:100,sensitivityResults[1:100,],col=rep(1:nbScen,each=3),lwd=rep(c(2,1,1),nbScen),lty=rep(c(1,2,2),nbScen),type='l',main=nameTS[i],ylab="",xlab="")
	grid()
	#legend("center",legend=paste("exp.adapt=",((1:nbScen)-1)/(nbScen-1)),col=1:nbScen,lwd=2,lty=1)
	#dev.off()
}

