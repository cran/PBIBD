ym2 <-
function(n,t){
    if(n%%2==0){
    v<-n
    b<-v
    r<-v/2-t
    k<-r
    m<-v/2
    l<-c()
    mat<-circulant(n)
    mat1<-mat[1:(v/2-t),]
    print.matrix <- function(m){
        write.table(format(m, justify="centre"),
                    row.names=F, col.names=F, quote=F,sep="  ")
    }
    
    cat("Following is a Youden -",m,"square:\n")
    print.matrix(mat1)
    
    cat("The incomplete columns of the above Youden -",m,"square forms a PBIB design with the following parameters:\n")
    cat("v = ",v,"  b =",b,"  r = ",r,"  k = ",k,"\n")
    l[1]<-r-2
    l[m]<-l[1]+1
    if(m%%2==0){
        for(i in 1:(m/2)){
            l[i+1]<-l[i]-2
            
        }
        
        for(i in (m):((m/2)+1)){
            l[i-1]<-l[i]-2
        }
    }
    else{
        for(i in 1:((m+1)/2)){
            l[i+1]<-l[i]-2
        }
        for(i in m:(((m+1)/2)+1)){
            l[i-1]<-l[i]-2
        }
    }
    
    for(i in 1:m){
        if(l[i]<0)
            l[i]<-0
        cat("lambda_",i," = ",l[i],"  ")
    }
	cat("\n")
    }
    else
        cat("Enter even v\n")
}
