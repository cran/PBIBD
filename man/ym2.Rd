\name{ym2}
\alias{ym2}
\title{Constructs Youden-m square designs and provides the parameters of the corresponding PBIB design}
\description{
By omitting the middle \eqn{2t} rows (where \eqn{t = 1, 2, \ldots}) from any even-ordered Circulant matrix with order \eqn{v \geq 6}, and considering only those rows that lie either above or below the omitted \eqn{2t} rows, the resulting arrangement gives a new type of Youden-m square.

The columns of these Youden-m squares constitute \eqn{m}-associate class PBIB designs, with the following parameters:

\itemize{
  \item \eqn{v \geq 6} and even, \eqn{b = v}, \eqn{r = k = \frac{v}{2} - t}
  \item \eqn{\lambda_1 = r - 2}, \eqn{\lambda_m = \lambda_1 + 1}
  \item If \eqn{m} is even, then:
    \itemize{
      \item \eqn{\lambda_{i+1} = \lambda_i - 2} for \eqn{i = 1, 2, \ldots, \frac{m}{2}}
      \item \eqn{\lambda_{i-1} = \lambda_i - 2} for \eqn{i = m, m-1, \ldots, \frac{m}{2} + 1}
    }
  \item If \eqn{m} is odd, then:
    \itemize{
      \item \eqn{\lambda_{i+1} = \lambda_i - 2} for \eqn{i = 1, 2, \ldots, \frac{m+1}{2}}
      \item \eqn{\lambda_{i-1} = \lambda_i - 2} for \eqn{i = m, m-1, \ldots, \left(\frac{m+1}{2}\right) + 1}
    }
}
}
\usage{
ym2(n, t)
}
\arguments{
  \item{n}{\code{n} is the order of the circulant matrix, which is also the number of treatments}
  \item{t}{\code{t} is the number of rows to omit from the middle of the circulant matrix (i.e., \eqn{2t} rows will be omitted)}
}
\value{
The function returns the required Youden-m square design and the parameters of the PBIB design constituted by taking the incomplete columns of the Youden-m square as blocks.
}
\author{Kush Sharma, Davinder Kumar Garg}
\examples{ym2(8, 1)}
\keyword{Youden-m square}
\keyword{Youden-m square design}
\keyword{Partially Balanced Incomplete Block designs}
\keyword{Row-Column designs}
\keyword{Two-way elimination of heterogeneity}
