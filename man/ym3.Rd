\name{ym3}
\alias{ym3}
\title{Constructs Youden-m square designs and provides parameters of the corresponding PBIB design}
\description{
This function constructs a Youden-m square by omitting the middle row and an equal number of rows \eqn{t} from both ends of an odd-ordered Circulant matrix of order \eqn{v \geq 7}. The rows retained lie between the middle omitted row and the omitted rows from the top or bottom. This arrangement yields a Youden-m square suitable for two-way elimination of heterogeneity.

The columns of this Youden-m square constitute a PBIB design with the following parameters:

\itemize{
  \item \eqn{v \geq 7} and odd
  \item \eqn{b = v}, \eqn{r = k = \left( \frac{v + 1}{2} \right) - 1 - t}
  \item \eqn{\lambda_1 = v - 6 - (m - 4 + t)}
  \item \eqn{\lambda_i = 0} for \eqn{i = 2, 3, \ldots, t + 2}
  \item If \eqn{m > 3}, then \eqn{\lambda_j = \lambda_{j - 1} + 1} for \eqn{j = t + 3, t + 4, \ldots, m}
}
}
\usage{
ym3(n, t)
}
\arguments{
  \item{n}{Order of the circulant matrix, which is also the number of treatments.}
  \item{t}{Number of rows to omit from both ends of the circulant matrix.}
}
\value{
The function returns the constructed Youden-m square design and the parameters of the PBIB design obtained by treating the incomplete columns of the square as blocks.
}
\author{Kush Sharma, Davinder Kumar Garg}
\examples{
ym3(7, 1)
}
\keyword{Youden-m square}
\keyword{Youden-m square design}
\keyword{Partially Balanced Incomplete Block designs}
\keyword{Row-Column designs}
\keyword{Two-way elimination of heterogeneity}
