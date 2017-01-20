series2 <-
function(n){
mat<-matrix(nrow=n,ncol=(n+3))
for(kk in 1:n){
for(i in 1:4)
{
mat[kk,i]<-i+(5*(kk-1))
}
count<-5
for(j in 1:n)
{
if(j!=kk)
{
mat[kk,count]<-4+(j-1)*5
count<-count+1
}
}
}
cat("The parameters of the design are","\n")
cat("v = b =",5*n,"\n")
cat("r = k =",(n+3),"\n")
cat("lambda1 =",3,"  ","lambda2 =",1,"  ","lambda3 =",3,"  ","lambda 4=",2,"  ","lambda 5=",n,"\n")
cat("Following are the blocks")
cat("\n")
for(i in 1:n)
{
matt<-matrix(nrow=5,ncol=(n+3))
cat("(",mat[i,],")")
cat("\n")
for(j in 1:5)
{
matt[j,]<-mat[i,]+(j-1)
{
for(ii in 1:4)
{
diff<-0
if(matt[j,ii]>5*i)
{
diff<-matt[j,ii]-5*i
matt[j,ii]<-diff+(i-1)*5
}
}
if(j==2)
{
matt[j,5:(n+3)]<-matt[(j-1),5:(n+3)]+1
cat("(",matt[j,],")")
cat("\n")
}
if(j==3)
{
matt[3,5:(n+3)]<-matt[(2),5:(n+3)]-4
cat("(",matt[j,],")")
cat("\n")
}
if(j==4 || j==5)
{
matt[j,5:(n+3)]<-matt[(j-1),5:(n+3)]+1
cat("(",matt[j,],")")
cat("\n")
}
}
}}}
