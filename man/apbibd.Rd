\name{apbibd}
\alias{apbibd}
\title{Calculates the efficiencies of PBIB designs with any number of associate classes.}
\description{This function calculates the different kinds of efficiencies and overall efficiency factor of Partially Balanced Incomplete Block Designs with any number of associate classes. The total number of treatments i.e. v, replications i.e. r, block size i.e. k, vector l of lambda's (lambda i being the ith element of vector l), vector n of number of associates (ni, i.e. number of ith associates, being the ith element of vector n), a list P of P-matrices of the association scheme of the design (Pi being the ith element of the list P) are to be supplied as input to the function.}
\usage{apbibd(v, r, k, l, n, P)}
\arguments{
  \item{v}{Total number of treatments of the design}
  \item{r}{Replication of the treatments in the design}
  \item{k}{Block size of the design}
  \item{l}{A vector containing lambda 1, lambda 2, lambda 3,..., lambda m as its first, second, third,..., mth elements}
  \item{n}{A vector containing n1, n2, n3,..., nm as its first, second, third,..., mth elements}
  \item{P}{A list containing P-matrices of the association scheme of the design such that P1 is its first element, P2 is second element, P3 is third element,..., Pm is the mth element}
}
\value{
Returns a list with (m+1) components:
         \item{E1}{Efficiency E1 of the design}
         \item{E2}{Efficiency E2 of the design and so on...}
         \item{Em}{Efficiency Em of the design}
         \item{E}{Overall efficiency factor of the design}
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
apbibd(v,r,k,l,n,P)
}
\keyword{Partially Balanced Incomplete Block Designs}
\keyword{Efficiencies}
\keyword{m-associate class PBIB Design}
