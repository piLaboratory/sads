load_all()
N <- 50
s <- 0.8
rzipf2<-function(n, N, s) qzipf(runif(n, min=dzipf(1,N,s), max=1+dzipf(N,N,s)), N, s)
RF <- rzipf2(1e6, N, s)
DF <- dzipf(1:N, N, s) 
DRF <- hist(RF, plot=F, breaks=0:N)$density
barplot(DF, col="gray80", main="rzipf")
barplot(DRF, add=T, col="#FFFF0099")
legend("topright", legend=c("predicted", "generated"), fill=c("gray80", "#ffff00"))
