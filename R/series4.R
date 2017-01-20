series4 <-
function(m,n){
v<-m*n
b<-m*n
r<-m+n-1
k<-m+n-1
l<-c(m,2,n)
M<-matrix(nrow=m,ncol=n)
B<-matrix(nrow=n,ncol=k)
for(i in 1:m)
{
for(j in 1:n)
{
kp<-i+m*(j-1)
M[i,j]=kp
}
}
for(i in 1:n){
for(j in 1:m){
B[i,j]<-M[j,i]
}
jj=m+1
for(p in 1:n){
if(i!=p){
B[i,jj]=M[m,p]
jj<-jj+1
}
}
}
cat("\n")
cat("The parameters of the design are","\n")
cat("v = b =",v,"\n")
cat("r = k =",r,"\n")
cat("lambda1 =",m,"  ","lambda2 =",2,"  ","lambda3 =",n,"\n")
cat("Following are the blocks")
cat("\n")
for(i in 1:n)
{
matt<-matrix(nrow=m,ncol=k)
for(j in 1:m)
{
matt[j,]<-B[i,]+(j-1)
{
for(ii in 1:m)
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
matt[j,(m+1):(k)]<-B[i,(m+1):(k)]
cat("(",matt[j,],")")
cat("\n")
}

if(j==2)
{
matt[2,(m+1):(k)]<-matt[(1),(m+1):(k)]-(m-1)
cat("(",matt[j,],")")
cat("\n")
}
if(j>=3)
{
matt[j,(m+1):(k)]<-matt[(j-1),(m+1):(k)]+1
cat("(",matt[j,],")")
cat("\n")
}
}
}
}
}
