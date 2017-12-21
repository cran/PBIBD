\name{series3}
\alias{series3}
\title{This function constructs five-associate class PBIB designs}
\description{Let us consider a module M of residue class mod(5) having elements 0, 1, 2, 3, 4 and all the elements of M are assigned to each of the n >= 2 classes. This function constructs PBIB designs with the following parameters:

v = 5n,  b = 5n,  r = 2(n + 1),  k = 2(n + 1)

lambda 1 = n + 2,  lambda 2 = n + 2,  lambda 3 = 3,  lambda 4 = 2,  lambda 5 = 2n
}
\usage{
series3(n)
}
\arguments{
  \item{n}{n is the number of classes to which the elements of Module M are assigned}
}
\value{The function returns the required PBIB design with specified parameters}
\author{Parneet Kaur, Davinder Kumar Garg}
\examples{
series3(5)
}
\keyword{Construction of PBIB designs}
\keyword{Partially Balanced Incomplete Block Designs}
\keyword{five-associate class}
\keyword{Symmetric differences}
