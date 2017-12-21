\name{series4}
\alias{series4}
\title{This function constructs three-associate class PBIB designs.}
\description{Let us consider a module M having m elements. To each element of the module, there corresponds n distinct classes, where m >= 5 and n >= 2. With these v = mn treatments following are parameters of the three-associate class PBIB design:

v = mn,  b = mn,  r = (m+n-1),  k = (m+n-1)

lambda 1 = m,  lambda 2 = 2,  lambda 3 = n
}
\usage{
series4(m, n)
}
\arguments{
  \item{m}{Size of the module M.}
  \item{n}{n is the number of classes to which the elements of Module M are assigned.}
}
\value{This function returns the required three-associate class PBIB design.}
\author{Parneet Kaur, Davinder Kumar Garg}
\examples{series4(5,2)}
\keyword{Construction of PBIB designs}
\keyword{Partially Balanced Incomplete Block Designs}
\keyword{three-associate class}
\keyword{Symmetric differences}
