ym1 <-
function(n,t){
    m<-circulant(n)
    x<-c(1:n)
    for(i in 1:t){
        x<-x[-1]
    }
    
    for(i in 1:t){
        l<-length(x)
        
        x<-x[-(l)]
        
    }
    
    print.matrix <- function(mm){
        write.table(format(m[x,], justify="right"),
                    row.names=F, col.names=F, quote=F)
    }
    
    l<-c()
    
    if(n%%2 ==0){
        j<-n/2
        cat("Following is a Youden -",j,"square")
        cat("\n")
        print.matrix(m[x,])
        cat("\n")
        cat("The incomplete columns of the above Youden -",j,"square forms a PBIB design with the following parameters")
        cat("\n")
        cat("v = ",n," = b")
        cat("\n")
        cat("r = ",(n-2*t)," = k")
        cat("\n")
        cat("lambda_1 = ",n-2*(t+1))
        cat("  ")
        for(i in 0:(t-1)){
		l[j-i]<-n-2*t-1-2*i
        }
	  for(i in t:(j-t)){
		l[i]<-n-4*t
        }
	if(t>=3){
	for(i in 2:(t-1)){
	l[i]<-n-2*(t+i)
	}
	}
	for(i in 2:j){
	if(l[i]<0)
		l[i]<-0
	cat("lambda_",i," = ",l[i],"  ")
	}
    }
           
            if(n%%2 == 1){ 
            j<-(n-1)/2  
            cat("Following is a Youden -",j,"square")
            cat("\n")
            print.matrix(m[x,])
            cat("\n")
            cat("The incomplete columns of the above Youden -",j,"square forms a PBIB design with the following parameters")
            cat("\n")
            cat("v = ",n," = b")
            cat("\n")
            cat("r = ",(n-2*t)," = k")
            cat("\n")
            cat("lambda_1 = ",(n-2*t-1))
            cat("  ")
            for(i in 0:(t-1)){
                l[j-i]<-n-2*(t+1)-i
			l[j-(t-1)-i]<-l[j-(t-1)]-i
		}
            for(i in 2:(j-2*t+1)){
			if(j-2*t+2!=0)
                l[i]<-l[j-2*t+2]
			else
			l[i]<-0
			
            }
		for(i in 2:j){
		if(l[i]<0)
		l[i]<-0
		cat("lambda_",i," = ",l[i],"  ")
		}
            }
	cat("\n")
            
    }
