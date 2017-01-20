\name{apbibd5}
\alias{apbibd5}
\title{Calculates the efficiencies of any five-associate class Partially Balanced Incomplete Block Designs}
\description{This function calculates the different kinds of efficiencies and overall efficiency factor of any five-associate class Partially Balanced Incomplete Block Design. The total number of treatments i.e. v, replications i.e. r, block size i.e. k, vector l of lambda 1, lambda 2, lambda 3, lambda 4 and lambda 5, vector n of number of associates i.e. n1, n2, n3, n4 and n5, a list P of P-matrices of the association scheme of the design i.e. P1, P2, P3, P4 and P5 are to be supplied as input to the function}
\usage{apbibd5(v, r, k, l, n, P)}
\arguments{
  \item{v}{Total number of treatments of the design}
  \item{r}{Replication of the treatments in the design}
  \item{k}{Block size of the design}
  \item{l}{A vector containing lambda 1, lambda 2, lambda 3, lambda 4 and lambda 5 as its first, second, third, fourth and fifth elements}
  \item{n}{A vector containing n1, n2, n3, n4 and n5 as its first, second, third, fourth and fifth elements}
  \item{P}{A list containing P-matrices of the association scheme of the design such that P1 is its first element, P2 is second element, P3 is third element, P4 is fourth element and P5 is the fifth element}
}
\value{
Returns a list with six components:
         \item{Efficiency_E1}{Efficiency E1 of the design}
         \item{Efficiency_E2}{Efficiency E2 of the design}
         \item{Efficiency_E3}{Efficiency E3 of the design}
         \item{Efficiency_E4}{Efficiency E4 of the design}
         \item{Efficiency_E5}{Efficiency E5 of the design}
         \item{Overall_Efficiency_Factor}{Overall efficiency factor of the design}
}
\author{Kush Sharma, Parneet Kaur, Davinder Kumar Garg}
\examples{
v<-25
r<-9
k<-9
l<-c(5,2,5,2,5)
n<-c(2,8,2,8,4)
P1<-matrix(c(0,0,1,0,0,0,0,0,4,4,1,0,1,0,0,0,4,0,4,0,0,4,0,0,0),nrow=5,ncol=5)
P2<-matrix(c(0,0,0,1,1,0,0,1,3,3,0,1,0,1,0,1,3,1,3,0,1,3,0,0,0),nrow=5,ncol=5)
P3<-matrix(c(1,0,1,0,0,0,4,0,4,0,1,0,0,0,0,0,4,0,0,4,0,0,0,4,0),nrow=5,ncol=5)
P4<-matrix(c(0,1,0,1,0,1,3,1,3,0,0,1,0,0,1,1,3,0,0,3,0,0,1,3,0),nrow=5,ncol=5)
P5<-matrix(c(0,2,0,0,0,2,6,0,0,0,0,0,0,2,0,0,0,2,6,0,0,0,0,0,3),nrow=5,ncol=5)
P<-list(P1,P2,P3,P4,P5)
apbibd5(v,r,k,l,n,P)
}
\keyword{Partially Balanced Incomplete Block Designs}
\keyword{Efficiencies}
\keyword{Five-associate class PBIB Design}
