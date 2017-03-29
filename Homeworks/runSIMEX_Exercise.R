source("Homeworks/SIMEX_exercise.R")

results<-list()
nbScen=11
for(i in 1:nbScen){
	results[[i]]<-runModel(mc=10,exp.adapt = (i-1)/(nbScen-1),Time=200)
	
}

#Sensistivity Analysis

nameTS<-names(results[[1]])
for(i in 1:nbScen){
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
