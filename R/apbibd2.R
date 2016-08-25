apbibd2 <-
function(v,r,k,l,n,P){
A1<-r*(k-1)+l[2]
A2<-l[2]-l[1]
B1<-A2*P[[2]][1,2]
B2<-A1+(l[2]-l[1])*(P[[1]][1,1]-P[[2]][1,1])
del<-A1*B2-A2*B1
E1<-del/(r*k*(A2+B2))
E2<-del/(r*k*B2)
E<-del*(v-1)/(r*k*((v-1)*B2+n[1]*A2))
lst<-list(Efficiency_E1=E1,Efficiency_E2=E2,Overall_Efficiency_Factor=E)
return(lst)
}
