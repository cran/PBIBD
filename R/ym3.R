ym3 <-
function(n,t){
    if(n<7 || n%%2==0){
        cat("Enter v >=7 and odd\n")
    }
    else{
        v<-n
        b<-v
        r<-((v+1)/2)-1-t
        k<-r
        m<-(v-1)/2
        l<-c()
        l[1]<-v-6-(m-4+t)
        for(i in 2:(t+2)){
            l[i]<-0

        }

        if(m>3 && m>(t+2)){
            for(i in (t+3):m){
                l[i]<-l[i-1]+1
            }

        }
        cat("Following is a Youden - ",m,"Square\n")
        mat<-circulant(n)
        print.matrix <- function(m){
            write.table(format(m, justify="centre"),
                        row.names=F, col.names=F, quote=F,sep="  ")
        }
        print.matrix(mat[(t+1):(((v+1)/2)-1),])

        cat("\n")
        cat("The incomplete columns of this Youden - ",m,"square gives the PBIB design with the following parameters:\n")
        cat("v = ",v,"  b = ",b,"  r =",r,"  k = ",k,"\n")
        for(i in 1:m){
            cat("lambda_",i," = ",l[i],"  ")
        }
        cat("\n")
    }
}
