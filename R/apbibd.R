apbibd <-
function(v,r,k,l,n,P)
{
eff<-list()
m<-length(l)
if(m==length(n)&& m==length(P))
	{
	M<-matrix(nrow=m,ncol=m)
	M[1,1]=r*(k-1)+l[m]
	for(i in 2:m)
		{
		M[1,i]=(l[m]-l[i-1])
		}
	for(i in 2:m)
		{
		sum1=0
		for(j in 1:m)
			{
			sum1=sum1+(l[j]-l[i-1])*P[[m]][(i-1),j]
			}
		M[i,1]=sum1
		}
	for(R in 2:m)
		{
		for(C in 2:m)
			{
			if(C!=R)
				{
				sum2=0
				for(j in 1:(m-1))
					{
					sum2=sum2+(l[m]-l[j])*(P[[C-1]][(R-1),j]-P[[m]][(R-1),j])
					}
				M[R,C]=sum2
				}
			else
				{
				sum3=0
				for(j in 1:(m-1))
					{
					sum3=sum3+(l[m]-l[j])*(P[[(R-1)]][(R-1),j]-P[[m]][(R-1),j])
					}
				M[R,C]=r*(k-1)+l[m]+sum3
				}
			}
		}
	delta=det(M)
	if(m==2)
		{
		eff[1]<-delta/(r*k*(M[-1,-1]+M[-2,-1]))
		eff[2]<-delta/(r*k*(M[-1,-1]))
		eff[3]<-(v-1)*delta/(r*k*((v-1)*M[-1,-1]+n[1]*M[-2,-1]))
            
		}
	else
		{
		for(j in 1:m-1)
			{
			eff[j]<-delta/(r*k*(det(M[-1,-1])+(-1)^(j-1)*det(M[-(j+1),-1])))
			}
		eff[m]<-delta/(r*k*(det(M[-1,-1])))
		sum4=0
		for(i in 1:(m-1))
			{
			sum4=sum4+(-1)^(i-1)*n[i]*det(M[-(i+1),-1])
			}
		eff[m+1]<-(v-1)*delta/(r*k*((v-1)*det(M[-1,-1])+sum4))
		}
	names(eff)<-sprintf("E%s",c(1:m,""))
	return(eff)
	}
else
	{
	print("Enter Valid Data")
	}
}
