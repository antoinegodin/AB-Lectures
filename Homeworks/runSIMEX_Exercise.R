source("Homeworks/SIMEX_exercise.R")

m=1
t=500

results<-runModel(Time=t,nHH=20,nF=5)

plot(1:t,results$C[m,1:t] + results$G[m,1:t],type="l",ylim=range(results$G+results$C),col=1,ylab="Output",xlab="")

# plot(1:Time,Tax[1:Time,1],type="l",ylim=range(TAXwage+TAXProf),col=1,ylab="Taxes",xlab="")

# plot(1:Time,YD[1:Time,1],type="l",ylim=range(YD),col=1,ylab="Disposable income",xlab="")

plot(1:t,results$C[m,1:t],type="l",ylim=range(results$C),col=1,ylab="Consumption",xlab="")

# plot(1:Time,G[1:Time,1] - Tax[1:Time,1],type="l",ylim=range(G-Tax),col=1,ylab="Public Deficit",xlab="")

# plot(1:Time,YD[1:Time,1] - C[1:Time,1],type="l",ylim=range(YD-C),col=1,ylab="Households Savings",xlab="")

plot(1:t,results$H[m,1:t],type="l",ylim=range(results$H),col=1,ylab="Wealth",xlab="")

plot(1:t,results$UR[m,1:t],type="l",ylim=range(results$UR),col=1,ylab="Unemployment rate",xlab="")
