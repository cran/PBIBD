verify <-
function(v,b,r,k,l,n,P){
    count<-0
    co<-0
    z<-0
    nn<-length(n)
    if((v*r)!=(b*k)){
        cat("condition (i) for the existence of a PBIB design i.e. vr = bk does not hold\n")
    }
    else{
        count<-count+1
       
    }
    if(sum(l*n)!=(r*(k-1))){
        cat("\n")
        cat("condition (ii) for the existence of a PBIB design i.e. sum(l*n)= r*(k-1) does not hold\n")
    }
    else{
        count<-count+1
    }

if(sum(n)!=(v-1)){
        cat("\n")
        cat("condition (i) for the existence of association scheme i.e. sum(n)= (v-1) does not hold\n")
    }
    else{
        count<-count+1
    }


    for(i in c(1:nn)){
        for(j in c((i+1):nn)){
           if(j<=nn && sum((n[i]*(P[[i]][j,]))== (n[j]*(P[[j]][i,])))!=nn)
            { 
               z<-z+1
            }
        }
    }
    
    if(z!=0)
    {
        cat("\n")
        cat("condition (iii) for the existence of association scheme i.e. ni(pijk)=nj(pjik) doesnt hold\n")
    }
    else{
        
        count<-count+1
        
    }
    
    for(i in 1:nn){
        c<-0
        for(j in 1:nn){
            for(k in 1:nn){
               if(P[[i]][j,k]!=P[[i]][k,j]){
                   c<-c+1
               } 
            }
        
        }
        if(c>0)
        {
            cat("\n")
            cat("Condition (iv) for the existence of association scheme i.e. P is a symmetric matrix does not hold.\n")
        }
        else{
            count<-count+1
        }
    }
    
    
    for(i in 1:nn){
        g<-0
        for(j in 1:nn){
            if(j==i){
            for(k in 1:nn){
               g<-g+P[[i]][j,k] 
            }
        }
        }
        if(g!=(n[i]-1)){
            cat("\n")
            cat("Condition (ii) for the existence of association scheme i.e. sum(p(ijk)) = ni - 1 for P",i," matrix does not hold\n")
            co<-co+1
        }
    }
    
  
    
    for(i in 1:nn){
        
        for(j in 1:nn){
            gg<-0
            if(i!=j){
                for(k in 1:nn){
                    gg<-gg+P[[i]][j,k] 
                }
            
       
        if(gg!=n[j]){
            cat("\n")
            cat("Condition (ii) for the existence of association scheme i.e. sum(p(ijk)) = nj for P",i," matrix does not hold\n")
            cat(j)
            co<-co+1
        }}}
    }
    
    
    
      
    
   
    
   if((count==(nn+4))&&co==0){
       cat("All the necessary conditions for the existence of PBIB design and association scheme hold")
       cat("\n")
   }
}
