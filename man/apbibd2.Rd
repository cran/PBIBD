\name{apbibd2}
\alias{apbibd2}
\title{Calculates the efficiencies of any two-associate class Partially Balanced Incomplete Block Designs}
\description{This function calculates the different kinds of efficiencies and overall efficiency factor of any two-associate class Partially Balanced Incomplete Block Design. The total number of treatments i.e. v, replications i.e. r, block size i.e. k, vector l of lambda 1 and lambda 2, vector n of number of associates i.e. n1 and n2, a list P of P-matrices of the association scheme of the design i.e. P1 and P2 are to be supplied as input to the function}
\usage{apbibd2(v, r, k, l, n, P)}
\arguments{
   \item{v}{Total number of treatments of the design}
  \item{r}{Replication of the treatments in the design}
  \item{k}{Block size of the design}
  \item{l}{A vector containing lambda 1 and lambda 2 as its first and second element}
  \item{n}{A vector containing n1 and n2 as its first and second element}
  \item{P}{A list containing P-matrices of the association scheme of the design such that P1 is its first element and P2 is the second element}
}
\value{
Returns a list with three components:
         \item{Efficiency_E1}{Efficiency E1 of the design}
         \item{Efficiency_E2}{Efficiency E2 of the design}
         \item{Overall_Efficiency_Factor}{Overall efficiency factor of the design}
}
\author{Kush Sharma, Parneet Kaur, Davinder Kumar Garg}
\examples{
v<-10
r<-6
k<-2
l<-c(1,0)
n<-c(6,3)
P1<-matrix(c(3,2,2,1),nrow=2,ncol=2)
P2<-matrix(c(4,2,2,0),nrow=2,ncol=2)
P<-list(P1,P2)
apbibd2(v,r,k,l,n,P)
}
\keyword{Partially Balanced Incomplete Block Designs}
\keyword{Efficiencies}
\keyword{Two-associate class PBIB Design}
