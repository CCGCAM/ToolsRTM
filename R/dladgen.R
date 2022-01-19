dladgen <- function(a,b,freq){

   # a=1
  #  b=2
litab <- c(5,15,25,35,45,55,65,75,81,83,85,87,89)

for (i1 in c(1:8)){
    t   <- i1*10
    freq <- dcum(a,b,t)
}
for (i2 in c(9:12)){
    t   <- 80+(i2-8)*2
    freq <- dcum(a,b,t)
}

if (freq == 1){
for (i   in seq(13,2,-1)){
    freq[i] <- freq[i]-freq[i-1]
}

}

}