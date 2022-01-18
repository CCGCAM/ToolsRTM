Jfunc1 <- function(k,l,t){
#	J1 function with avoidance of singularity problem
del <- (k-l)*t
Jout<-c()

greatthan<-which(abs(del) > 1e-3)
lowerthan<-which(abs(del) <= 1e-3)

for (i in c(greatthan)){
 # print(i)
  Jout[i]<-(exp(-l[i]*t) -exp(-k*t))/(k-l[i])

 # print(Jout[i])
}


for (i in c(lowerthan)){
 # print(i)
  Jout[i]<- 0.5*t*(exp(-k*t)+exp(-l[i]*t))*(1-del[i]*del[i]/12)
 # print(Jout[i])
}




Jout <- as.vector(t(Jout))
return(Jout)

}