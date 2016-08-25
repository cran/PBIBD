\name{apbibd3}
\alias{apbibd3}
\title{Calculates the efficiencies of any three-associate class Partially Balanced Incomplete Block Designs}
\description{This function calculates the different kinds of efficiencies and overall efficiency factor of any three-associate class Partially Balanced Incomplete Block Design. The total number of treatments i.e. v, replications i.e. r, block size i.e. k, vector l of lambda 1, lambda 2 and lambda 3, vector n of number of associates i.e. n1, n2 and n3, a list P of P-matrices of the association scheme of the design i.e. P1, P2 and P3 are to be supplied as input to the function}
\usage{apbibd3(v, r, k, l, n, P)}
\arguments{
   \item{v}{Total number of treatments of the design}
  \item{r}{Replication of the treatments in the design}
  \item{k}{Block size of the design}
  \item{l}{A vector containing lambda 1, lambda 2 and lambda 3 as its first, second and third element}
  \item{n}{A vector containing n1, n2 and n3 as its first, second and third element}
  \item{P}{A list containing P-matrices of the association scheme of the design such that P1 is its first element, P2 is second element and P3 is the third element}
}
\value{
Returns a list with four components:
         \item{Efficieny_E1}{Efficiency E1 of the design}
         \item{Efficieny_E2}{Efficiency E2 of the design}
         \item{Efficieny_E3}{Efficiency E3 of the design}
         \item{Overall_Efficiency_Factor}{Overall efficiency factor of the design}
}
\references{Rao, C. R. (1947b). General methods of analysis for incomplete block designs, J. Amer. Statist. Assoc. 42, 541-561}
\author{Kush Sharma, Parneet Kaur, Davinder Kumar Garg}
\examples{
v<-15
r<-9
k<-9
l<-c(4,5,6)
n<-c(2,8,4)
P1<-matrix(c(1,0,0,0,4,4,0,4,0),nrow=3,ncol=3)
P2<-matrix(c(0,1,1,1,3,3,1,3,0),nrow=3,ncol=3)
P3<-matrix(c(0,2,0,2,6,0,0,0,3),nrow=3,ncol=3)
P<-list(P1,P2,P3)
apbibd3(v,r,k,l,n,P)
}
\keyword{Partially Balanced Incomplete Block Designs}
\keyword{Efficiencies}
\keyword{Three-associate class PBIB Design}