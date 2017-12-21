series6 <-
function(m,n){
v<-m*n
b<-m
r<-(m-1)
k<-(m-1)*n
l<-c((m-2),(m-2),(m-1))
M<-matrix(nrow=m,ncol=n)
mat<-matrix(nrow=m,ncol=k)
B<-c()
for(i in 1:m){
for(j in 1:n){
kp<-i+m*(j-1)
M[i,j]<-kp
}
}
p<-1
for(i in 1:n){
for(j in 1:(m-1)){

B[p]<-M[j,i]
p<-p+1
}
}
cat("The parameters of the design are:")
cat("\n")
cat("v=",v,"\n")
cat("b=",b,"\n")
cat("r=",r,"\n")
cat("k=",k,"\n")
cat("lambda 1=",l[1],"\t","lambda 2=",l[2],"\t","lambda 3=",l[3],"\n")
cat("Following are the blocks of the design:","\n")
mat[1,]<-B

cat("(",mat[1,],")")
cat("\n")
for(i in 2:m)
{
mat[i,]<-mat[(i-1),]+1
for(j in 1:k)
{
if(mat[(i-1),j]%%m==0)
{
mat[i,j]<-mat[(i-1),j]-(m-1)
}
}
cat("(",mat[i,],")")
cat("\n")
}

}
