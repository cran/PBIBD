series1 <-
function(n){
v<-5*n
b<-5*n
r<-n+4
k<-n+4
l<-c(5,2,5,2,n)
M<-matrix(nrow=5,ncol=n)
B<-matrix(nrow=n,ncol=k)
for(i in 1:5)
{
for(j in 1:n)
{
kp<-i+5*(j-1)
M[i,j]=kp
}
}
for(i in 1:n){
for(j in 1:5){
B[i,j]<-M[j,i]
}
jj=6
for(p in 1:n){
if(i!=p){
B[i,jj]=M[5,p]
jj<-jj+1
}
}
}
cat("The parameters of the design are","\n")
cat("v = b =",5*n,"\n")
cat("r = k =",(n+4),"\n")
cat("lambda1 =",5,"  ","lambda2 =",2,"  ","lambda3 =",5,"  ","lambda 4=",2,"  ","lambda 5=",n,"\n")
cat("Following are the blocks")
cat("\n")
for(i in 1:n)
{
matt<-matrix(nrow=5,ncol=(n+4))
for(j in 1:5)
{
matt[j,]<-B[i,]+(j-1)
{
for(ii in 1:5)
{
diff<-0
if(matt[j,ii]>5*i)
{
diff<-matt[j,ii]-5*i
matt[j,ii]<-diff+(i-1)*5
}
}
if(j==1)
{
matt[j,6:(n+4)]<-B[i,6:(n+4)]
cat("(",matt[j,],")")
cat("\n")
}

if(j==2)
{
matt[2,6:(n+4)]<-matt[(1),6:(n+4)]-4
cat("(",matt[j,],")")
cat("\n")
}
if(j==3 || j==4 || j==5)
{
matt[j,6:(n+4)]<-matt[(j-1),6:(n+4)]+1
cat("(",matt[j,],")")
cat("\n")
}
}
}
}
}
