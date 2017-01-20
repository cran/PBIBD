\name{series2}
\alias{series2}
\title{This function constructs five-associate class PBIB designs}
\description{Let us consider a module M of residue class mod(5) having elements 0, 1, 2, 3, 4 and all the elements of M are assigned to each of the n >=2 classes
This function constructs PBIB designs with the following parameters:
v = 5n     b = 5n	     r = n+3	 k = n+3
lambda 1 = 3	 lambda 2 = 1	 lambda 3 = 3	 lambda 4 = 2	 lambda 5 = n
}
\usage{
series2(n)
}
\arguments{
  \item{n}{n is the number of classes to which the elements of Module M are assigned}
}
\value{The function returns the required PBIB design with specified parameters}
\author{Parneet Kaur, Kush Sharma, Davinder Kumar Garg}
\examples{
series2(4)
}
\keyword{Construction of PBIB designs}
\keyword{Partially Balanced Incomplete Block Designs}
\keyword{five-associate class}
\keyword{Symmetric differences}
