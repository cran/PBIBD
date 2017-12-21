\name{circulant}
\alias{circulant}
\title{This function generates circulant matrix of order n.}
\description{Circulant matrix, which is a special kind of Toeplitz matrix, is a square matrix of order n whose rows are obtained by cyclically rotated versions of a list ``l'' of length n such that the first row is obtained by cyclically rotating one element toward right the list ``l'' and each of the other row is the cyclically rotated one element toward the right version of the previous row. This function is used to generate a circulant matrix of order n. The order of the circulant matrix i.e. n is supplied as an argument to the function.}
\usage{circulant(n)}
\arguments{
  \item{n}{n is the order of the circulant matrix we want to generate.}
}
\value{ The function returns a circulant matrix c of order n.}
\author{Kush Sharma, Davinder Kumar Garg}
\examples{
circulant(7)
}
\keyword{Circulant Matrix }
\keyword{Toeplitz Matrix }
