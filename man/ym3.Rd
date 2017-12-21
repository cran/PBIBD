\name{ym3}
\alias{ym3}
\title{The function constructs Youden-m square designs. The function provides the parameters of the PBIB design constituted when the incomplete columns of this Youden-m square are taken as blocks.}
\description{By omitting the middle and equal number of rows, say t rows, from both ends of any odd-ordered Circulant matrix with order v >= 7and considering those rows that lie either between the middle omitted row and omitted rows from the top or between the middle omitted row and the omitted rows from the bottom of the Circulant matrix. Then this arrangement of rows gives us a Youden-m square. These Youden-m square designs are the designs for two-way elimination of heterogenity. The columns of this Youden-m square constitue PBIB design with the following parameters:

v >= 7 and odd,  b = v,   r = k =  ((v+1)/2) -1-t

lambda 1 = v - 6 - (m - 4 + t),  lambda i = 0 ;  i = 2, 3, ..., t+2

if m > 3 then, lambda j = lambda {j-1}+1 ; j = t+3, t+4, ...,m
}
\usage{
ym3(n, t)
}
\arguments{
  \item{n}{n is the order of the circulant matrix which is also the number of treatments}
  \item{t}{t is the number of rows you want to omit from both ends of the circulant matrix}
}

\value{The function returns the required Youden-m square design. It also returns the parameters of the PBIB design constituted by taking the incomplete columns of the Youden-m square as blocks.}
\author{Kush Sharma, Davinder Kumar Garg}

\examples{ym3(7,1)}
\keyword{Youden-m square}
\keyword{Youden-m square design}
\keyword{Partially Balanced Incomplete Block designs}
\keyword{Row-Column designs}
\keyword{Two-way eliminationof heterogenity}
