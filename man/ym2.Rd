\name{ym2}
\alias{ym2}
\title{The function constructs Youden-m square designs. The function provides the parameters of the PBIB design constituted when the incomplete columns of this Youden-m square are taken as blocks.}
\description{By omitting the middle 2t (t = 1, 2, ...) rows of any even ordered Circulant matrix with order v >= 6 and considering only those rows which lie either above or below the omitted 2t rows, the resulting arrangement of rows gives a new type of Youden-m square. The columns of these Youden-m squares constitute m-associate class PBIB designs, with the following parameters:

v >= 6 and even,   b = v,  r = k = (v/2)-t,  lambda 1 = r-2,  lambda m = lambda 1+1 

If m is even, then lambda {i+1} = lambda i - 2; i = 1, 2, ..., m/2 and lambda {i-1} = lambda i - 2; i = m, m-1, ..., (m/2)+1

If m is odd, then lambda {i+1} = lambda i - 2; i = 1, 2, ..., (m+1)/2 and lambda {i-1} = lambda i - 2; i = m, m-1, ...,  ((m+1)/2) + 1
}
\usage{
ym2(n, t)
}
\arguments{
   \item{n}{n is the order of the circulant matrix which is also the number of treatments}
  \item{t}{t is the number of rows you want to omit from both ends of the circulant matrix}
}
\value{The function returns the required Youden-m square design. It also returns the parameters of the PBIB design constituted by taking the incomplete columns of the Youden-m square as blocks.}
\author{Kush Sharma, Davinder Kumar Garg}
\examples{ym2(8,1)}
\keyword{Youden-m square}
\keyword{Youden-m square design}
\keyword{Partially Balanced Incomplete Block designs}
\keyword{Row-Column designs}
\keyword{Two-way eliminationof heterogenity}
