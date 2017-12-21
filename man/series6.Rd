\name{series6}
\alias{series6}
\title{This function constructs three-associate class PBIB designs.}
\description{Consider a module M having m elements and there are n classes corresponding to each element of the module. Thus, we have a total of v = mn treatments (m is odd prime). For these v = mn treatments following are the parameters of the three-associate class PBIB design:

v = mn,  b = m,  r = (m-1),  k = (m-1)n

lambda 1 = m-2,  lambda 2 = m-2,  lambda 3 = m-1
}
\usage{
series6(m, n)
}
\arguments{
 \item{m}{Size of Module M.}
  \item{n}{n is the number of classes to which the elements of Module M are assigned.}
}
\value{The function returns the required three-associate class PBIB design with the parameters specified in the description.}
\author{Kush Sharma, Davinder Kumar Garg}

\examples{series6(5,3)}
\keyword{Construction of PBIB designs}
\keyword{Partially Balanced Incomplete Block designs}
\keyword{Three-associate class association scheme}
\keyword{Initial Blocks}
