library(beepr)
source("Homeworks/SIMEX_exercise_v2.R")

results<-list()
nbScen=11
for(i in 1:nbScen){
	results[[i]]<-runModel(mc=10,exp.adapt = (i-1)/(nbScen-1),Time=200)
	
}

test<-runModel(mc=1,exp.adapt = 0,Time=300,exp.income = 0.8,plot=T)
beep()
#Sensistivity Analysis

nameTS<-names(results[[1]])
for(i in 1:11){
	sensitivityResults<-data.frame(matrix(0,nrow=200,ncol=0))
	for(j in 1:length(results)){
		tsMC<-as.data.frame(t(results[[j]][[i]]))
		sensitivityResults[paste(nameTS[i],"scen",j,"average",sep=".")]<-rowSums(tsMC)/ncol(tsMC)
		sensitivityResults[paste(nameTS[i],"scen",j,"min",sep=".")]<-apply(tsMC,1,function(x) min(x))
		sensitivityResults[paste(nameTS[i],"scen",j,"max",sep=".")]<-apply(tsMC,1,function(x) max(x))
	}
	jpeg(filename=paste(nameTS[i],".jpg",sep=""))
	matplot(60:200,sensitivityResults[60:200,],col=rep(1:nbScen,each=3),lwd=rep(c(2,1,1),nbScen),lty=rep(c(1,2,2),nbScen),type='l',main=nameTS[i],ylab="",xlab="")
	grid()
	legend("center",legend=paste("exp.adapt=",((1:nbScen)-1)/(nbScen-1)),col=1:nbScen,lwd=2,lty=1)
	dev.off()
}

#Looking at the agents
i=12
sensitivityResults<-data.frame(matrix(0,nrow=200,ncol=0))
meandif<-data.frame(matrix(0,nrow=1,ncol=nbScen))
for(j in 1:length(results)){
	agentsMC<-results[[j]][[i]]
	tempYDe<-data.frame(matrix(0,nrow=100,ncol=0))
	tempYD<-data.frame(matrix(0,nrow=100,ncol=0))
	for(k in 1:length(agentsMC)){
		hhs<-agentsMC[[k]]$households
		YDe<-hhs$expected.income
		YD<-hhs$employment*hhs$wage*(1-hhs$tax.rate)
		tempYDe[k]<-colSums(YDe[60:200,])/nrow(YDe[60:200,])
		tempYD[k]<-colSums(YD[60:200,])/nrow(YD[60:200,])
	}
	sensitivityResults[paste("YDe.scen",j,sep=".")]<-rowSums(tempYDe)/nrow(tempYDe)
	sensitivityResults[paste("YD.scen",j,sep=".")]<-rowSums(tempYD)/nrow(tempYD)
	meandif[j]<-mean(rowSums(tempYDe)/nrow(tempYDe)-rowSums(tempYD)/nrow(tempYD))
	jpeg(filename=paste("expectation.scen.",j,".jpg",sep=""))
	matplot(rowSums(tempYDe)/nrow(tempYDe)-rowSums(tempYD)/nrow(tempYD),col=rep(1:nbScen,each=2),lwd=1,pch=rep(1:2,nbScen),type='p',main=paste("scen.",j),ylab="",xlab="")
	grid()
	lines(1:100,rep(mean(rowSums(tempYDe)/nrow(tempYDe)-rowSums(tempYD)/nrow(tempYD)),100),lty=1,lwd=2)
	dev.off()
}

beep()
#PLOTTING ALL THE RESULTS
matplot(t(results$C)+ t(results$G),type="l",ylim=range(results$G+results$C),col=1:10,lty=1,lwd=2,ylab="Output",xlab="")

matplot(t(results$C),type="l",ylim=range(results$C),col=1:10,ylab="Consumption",xlab="",lty=1,lwd=2)

matplot(t(results$H),type="l",ylim=range(results$H),col=1:10,lwd=2,lty=1,ylab="Wealth",xlab="")

matplot(t(results$UR),type="l",ylim=range(results$UR),col=1:10,lwd=2,lty=1,ylab="Unemployment rate",xlab="")
