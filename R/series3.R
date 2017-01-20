series3 <-
function(n){
v<-5*n
b<-5*n
r<-2*(n+1)
k<-2*(n+1)
l<-c((n+2),(n+2),3,2,2*n)
B<-matrix(nrow=n,ncol=k)
M<-matrix(nrow=5,ncol=n)
C<-matrix(nrow=n,ncol=k)
for(i in 1:5){
for(j in 1:n){
M[i,j]<-i+5*(j-1)
}
}
for(i in 1:n){
for(j in 1:4){
B[i,j]<-M[j,i]
}
jj=5
kk<-n+4
for(p in 1:n)
{
if(i!=p){
B[i,jj]<-M[2,p]
B[i,kk]<-M[3,p]
jj<-jj+1
kk<-kk+1
}
}
}
cont<-1
cnt<-1
for(j in 1:(n-1)){
C[,4+cont]<-B[,4+j]
cont<-cont+2
C[,5+cnt]<-B[,n+3+j]
cnt<-cnt+2
}
cat("The Parameters of the design are:","\n")
cat("v = ",v,"b = ",b,"r = ",r,"k = ",k,"\n")
for(i in 1:5)
cat("lambda[",i,"] = ",l[i],"\t")
cat("\n")
cat("The developed blocks are:","\n")
for(i in 1:n)
{
matt<-matrix(nrow=5,ncol=k)
for(j in 1:5)
{
matt[j,]<-B[i,]+(j-1)
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
if(j==1)
{
matt[j,5:k]<-C[i,5:k]
cat("(",matt[j,],")")
cat("\n")
}

if(j==2 || j==3)
{
matt[j,5:k]<-matt[(j-1),5:k]+1
cat("(",matt[j,],")")
cat("\n")
}
count<-1
cnt<-1
if(j==4)
{
for(ij in 1:(n-1)){
matt[j,4+count]<-matt[j-1,4+count]+1
count<-count+2
matt[j,5+cnt]<-matt[j-1,5+cnt]-4
cnt<-cnt+2
}
#print(matt[j,])

cat("(",matt[j,],")")
cat("\n")
}

if(j==5)
{
for(ij in 1:(n-1)){
matt[j,4+count]<-matt[j-1,4+count]-4
count<-count+2
matt[j,5+cnt]<-matt[j-1,5+cnt]+1
cnt<-cnt+2
}
cat("(",matt[j,],")")
cat("\n")
}
}
}
}
}
