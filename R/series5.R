series5 <-
function(m,n){
v<-m*n
r<-m+n-2
b<-m*n
k<-m+n-2
l<-c((m-2),2,(n-2))
M<-matrix(nrow=m,ncol=n)
B<-matrix(nrow=n,ncol=k)
for(i in 1:m){
for(j in 1:n){
kp<-i+m*(j-1)
M[i,j]<-kp
}
}
for(i in 1:n){
for(j in 1:(m-1)){
B[i,j]<-M[j,i]
}
jj<-m
for(p in 1:n){
if(i!=p){
B[i,jj]<-M[m,p]
jj<-jj+1
}
}
}
cat("The parameters of the design are","\n")
cat("v = b =",v,"\n")
cat("r = k =",k,"\n")
cat("lambda1 =",l[1],"  ","lambda2 =",l[2],"  ","lambda3 =",l[3],"\n")
cat("Following are the blocks")
cat("\n")
for(i in 1:n)
{
matt<-matrix(nrow=m,ncol=k)
for(j in 1:m)
{
matt[j,]<-B[i,]+(j-1)
{
for(ii in 1:(m-1))
{
diff<-0
if(matt[j,ii]>m*i)
{
diff<-matt[j,ii]-m*i
matt[j,ii]<-diff+(i-1)*m
}
}
if(j==1)
{
matt[j,m:k]<-B[i,m:k]
cat("(",matt[j,],")")
cat("\n")
}

if(j==2)
{
matt[2,m:k]<-matt[1,m:k]-(m-1)
cat("(",matt[j,],")")
cat("\n")
}
if(j>=3)
{
matt[j,m:k]<-matt[(j-1),m:k]+1
cat("(",matt[j,],")")
cat("\n")
}
}
}
}
}
