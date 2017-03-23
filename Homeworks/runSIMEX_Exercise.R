source("Homeworks/SIMEX_exercise.R")

results<-list()
nbScen=11
for(i in 1:nbScen){
	results[[i]]<-runModel(mc=10,exp.adapt = (i-1)/(nbScen-1))
	
}

#Sensistivity Analysis

nameTS<-names(results[[1]])
for(i in 1:length(results[[1]])){
	sensitivityResults<-data.frame(matrix(0,nrow=100,ncol=0))
	for(j in 1:length(results)){
		tsMC<-as.data.frame(t(results[[j]][[i]]))
		sensitivityResults[paste(nameTS[i],"scen",j,"average",sep=".")]<-rowSums(tsMC)/ncol(tsMC)
		sensitivityResults[paste(nameTS[i],"scen",j,"min",sep=".")]<-apply(tsMC,1,function(x) min(x))
		sensitivityResults[paste(nameTS[i],"scen",j,"max",sep=".")]<-apply(tsMC,1,function(x) max(x))
	}
	jpeg(filename=paste(nameTS[i],".jpg",sep=""))
	matplot(sensitivityResults,col=rep(1:nbScen,each=3),lwd=rep(c(2,1,1),nbScen),lty=rep(c(1,2,2),nbScen),type='l',main=nameTS[i],ylab="",xlab="")
	grid()
	legend("bottomright",legend=paste("exp.adapt=",((1:nbScen)-1)/(nbScen-1)),col=1:nbScen,lwd=2,lty=1)
	dev.off()
}

results<-runModel(mc=1,exp.adapt = 0.9,plot=T)

#PLOTTING ALL THE RESULTS
matplot(t(results$C)+ t(results$G),type="l",ylim=range(results$G+results$C),col=1:10,lty=1,lwd=2,ylab="Output",xlab="")

matplot(t(results$C),type="l",ylim=range(results$C),col=1:10,ylab="Consumption",xlab="",lty=1,lwd=2)

matplot(t(results$H),type="l",ylim=range(results$H),col=1:10,lwd=2,lty=1,ylab="Wealth",xlab="")

matplot(t(results$UR),type="l",ylim=range(results$UR),col=1:10,lwd=2,lty=1,ylab="Unemployment rate",xlab="")
