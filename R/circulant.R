circulant <-
function(n){
    x<-c(1:n)
    c<-matrix(nrow=n,ncol=n)
	fun1<-function(n,x){
        t<-x[n]
        for(i in (n-1):2){
          x[i+1]<-x[i]
       }
    x[2]<-x[1]
    x[1]<-t
    y<-x
    return(y)
    }
    for(i in 1:n){
        c[i,]<-fun1(n,x)
        x<-c[i,]
    }
    return(c)
    
}
