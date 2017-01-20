\name{apbibd4}
\alias{apbibd4}
\title{Calculates the efficiencies of any four-associate class Partially Balanced Incomplete Block Designs}
\description{This function calculates the different kinds of efficiencies and overall efficiency factor of any four-associate class Partially Balanced Incomplete Block Design. The total number of treatments i.e. v, replications i.e. r, block size i.e. k, vector l of lambda 1, lambda 2, lambda 3 and lambda 4, vector n of number of associates i.e. n1, n2, n3 and n4, a list P of P-matrices of the association scheme of the design i.e. P1, P2, P3 and P4 are to be supplied as input to the function}
\usage{apbibd4(v, r, k, l, n, P)}
\arguments{
   \item{v}{Total number of treatments of the design}
  \item{r}{Replication of the treatments in the design}
  \item{k}{Block size of the design}
  \item{l}{A vector containing lambda 1, lambda 2, lambda 3 and lambda 4 as its first, second, third and fourth elements}
  \item{n}{A vector containing n1, n2, n3 and n4 as its first, second, third and fourth elements}
  \item{P}{A list containing P-matrices of the association scheme of the design such that P1 is its first element, P2 is second element, P3 is third element and P4 is the fourth element}
}
\value{
Returns a list with five components:
         \item{Efficiency_E1}{Efficiency E1 of the design}
         \item{Efficiency_E2}{Efficiency E2 of the design}
         \item{Efficiency_E3}{Efficiency E3 of the design}
         \item{Efficiency_E4}{Efficiency E4 of the design}
         \item{Overall_Efficiency_Factor}{Overall efficiency factor of the design}
}
\author{Kush Sharma, Parneet Kaur, Davinder Kumar Garg}
\examples{
v<-12
r<-2
k<-3
l<-c(1,0,0,0)
n<-c(4,2,4,1)
P1<-matrix(c(1,1,1,0,1,0,1,0,1,1,1,1,0,0,1,0),nrow=4,ncol=4)
P2<-matrix(c(2,0,2,0,0,0,0,1,2,0,2,0,0,1,0,0),nrow=4,ncol=4)
P3<-matrix(c(1,1,1,1,1,0,1,0,1,1,1,0,1,0,0,0),nrow=4,ncol=4)
P4<-matrix(c(0,0,4,0,0,2,0,0,4,0,0,0,0,0,0,0),nrow=4,ncol=4)
P<-list(P1,P2,P3,P4)
apbibd4(v,r,k,l,n,P)
}
\keyword{Partially Balanced Incomplete Block Designs}
\keyword{Efficiencies}
\keyword{Four-associate class PBIB Design}
