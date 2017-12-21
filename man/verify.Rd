\name{verify}
\alias{verify}
\title{Verifies all the necessary conditions for the existence of PBIB designs based on any association scheme.}
\description{There exists various necessary conditions for the existence of the PBIB design as well as the association scheme on which the PBIB design is based. This function Verifies all those necessary conditions for the existence of PBIB designs based on any association scheme. The total number of treatments i.e. v, the total number of blocks i.e. b, replications i.e. r, block size i.e. k, vector l of lambda's (lambda i being the ith element of vector l), vector n of number of associates (n i, i.e. number of ith associates, being the ith element of vector n), a list P of P-matrices of the association scheme of the design (Pi being the ith matrix of the list P) are to be supplied as input to the function.}
\usage{verify(v, b, r, k, l, n, P)}
\arguments{
  \item{v}{Total number of treatments of the design}
  \item{b}{Total number of blocks in the design}
  \item{r}{Replication of the treatments in the design}
  \item{k}{Block size of the design}
  \item{l}{A vector containing lambda 1, lambda 2, lambda 3,..., lambda m as its first, second, third,..., mth elements}
  \item{n}{A vector containing n 1, n 2, n 3, ..., n m as its first, second, third,..., mth elements}
  \item{P}{A list containing P-matrices of the association scheme of the design such that P1 is its first element, P2 is second element, P3 is third element,..., Pm is the mth element}
}
\value{The function tells if all the necesary conditions for the existence of PBIB design based on some association scheme hold. If not, it highlights all the conditions which do not hold.}

\author{Kush Sharma, Davinder Kumar Garg}
\examples{
v<-12
b<-12
r<-5
k<-5
l<-c(1,2,2)
n<-c(2,3,6)
P1<-matrix(c(1,0,0,0,0,3,0,3,3),nrow=3,ncol=3)
P2<-matrix(c(0,0,2,0,2,0,2,0,4),nrow=3,ncol=3)
P3<-matrix(c(0,1,1,1,0,2,1,2,2),nrow=3,ncol=3)
P<-list(P1,P2,P3)
verify(v,b,r,k,l,n,P)
}
\keyword{Partially Balanced Incomplete Block Designs}
\keyword{m-associate class Association Scheme}
\keyword{Necessary conditions}
