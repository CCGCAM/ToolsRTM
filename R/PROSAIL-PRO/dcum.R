dcum <- function(a,b,t) {

    rd <- pi/180
if (a>=1){
    f <- 1-cos(rd*t)
} 
    else {
        eps <- 1e-8
        delx <- 1
        x <- 2*rd*t
        p <- x
        	while (delx >=  eps){
                y <- a*sin(x)+0.5*b*sin(2.0*x)
                dx <- 0.5*(y-x+p)
                x <- x+dx
                delx <- abs(dx)
            }
    	f <- (2.0*y+p)/pi
        }
}