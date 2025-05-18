\name{ym1}
\alias{ym1}
\title{Constructs Youden-m square designs and provides parameters of the corresponding PBIB design}
\description{
If the same number of rows, say \eqn{t}, are omitted from the top and bottom of a Circulant matrix such that at least two rows remain, the resulting arrangement forms a Youden-m square.

\strong{(A) For even-ordered Circulant matrices} (order \eqn{v \geq 4} and even), the columns of the resulting Youden-m square constitute a PBIB design with parameters:

\itemize{
  \item \eqn{b = v}, \eqn{r = k = v - 2t}
  \item \eqn{\lambda_1 = v - 2(t + 1)}
  \item \eqn{\lambda_{m - i} = v - 2t - 1 - 2i}, for \eqn{i = 0, 1, \ldots, t - 1}
  \item \eqn{\lambda_t = \lambda_{t+1} = \ldots = \lambda_{m - t} = v - 4t}
  \item If \eqn{t \geq 3}, then \eqn{\lambda_i = v - 2(t + i)} for \eqn{i = 2, 3, \ldots, t - 1}
}

\strong{(B) For odd-ordered Circulant matrices} (order \eqn{v \geq 5} and odd), the columns of the resulting Youden-m square constitute a PBIB design with parameters:

\itemize{
  \item \eqn{b = v}, \eqn{r = k = v - 2t}
  \item \eqn{\lambda_1 = v - 2t - 1}
  \item \eqn{\lambda_{m - i} = v - 2(t + 1) - i}, for \eqn{i = 0, 1, \ldots, t - 1}
  \item \eqn{\lambda_{m - (t - 1) - i} = \lambda_{m - (t - 1)} - i}, for \eqn{i = 0, 1, \ldots, t - 1}
  \item \eqn{\lambda_2 = \lambda_3 = \ldots = \lambda_{m - 2t + 1} = \lambda_{m - 2t + 2}}
}
}
\usage{
ym1(n, t)
}
\arguments{
  \item{n}{Order of the circulant matrix, which is also the number of treatments.}
  \item{t}{Number of rows to omit from both the top and bottom of the circulant matrix.}
}
\value{
The function returns the Youden-m square design and the parameters of the PBIB design formed by taking its incomplete columns as blocks.
}
\author{Kush Sharma, Davinder Kumar Garg}
\examples{
ym1(6, 1)
}
\keyword{Youden-m square}
\keyword{Youden-m square design}
\keyword{Partially Balanced Incomplete Block designs}
\keyword{Row-Column designs}
\keyword{Two-way elimination of heterogeneity}
