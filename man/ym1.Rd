\name{ym1}
\alias{ym1}
\title{The function constructs Youden-m square designs. The function provides the parameters of the PBIB design constituted when the incomplete columns of this Youden-m square are taken as blocks.}
\description{If we omit the same number of rows, say t rows, from the top and the bottom of the Circulant matrix, such that we are left with atleast two rows, the resulting arrangement of rows is a Youden-m square.

(A) For even-ordered Circulant matrices with order v >= 4, the columns of the Youden-m squares so obtained constitute the PBIB designs with the following parameters:

v >= 4  and even,  b = v,   r = k = v-2t

lambda 1 = v - 2(t + 1),  lambda {m-i} = v - 2t - 1 - 2i ;  i = 0, 1, ..., t-1

lambda t = lambda {t+1} = ... = lambda {m-t} = v - 4t. If t >=3 then, lambda i = v - 2(t + i); i = 2, 3, ..., t-1

(B)For odd-ordered Circulant matrices with order v >= 5, the columns of the Youden-m squares so obtained constitute the PBIB designs with the following parameters:

v >=5 and odd,  b = v,  r = k = v-2t

lambda 1 = v - 2t - 1, lambda {m-i} = v - 2(t + 1) - i; i = 0, 1, ..., t - 1, lambda {m-(t-1)-i} = lambda {m-(t-1)} - i ; i = 0, 1, 2, ..., t-1

and  lambda 2 = lambda 3= ... = lambda {(m-2t+1)} = lambda {(m-2t+2)}
}
\usage{
ym1(n, t)
}
\arguments{
  \item{n}{n is the order of the circulant matrix which is also the number of treatments}
  \item{t}{t is the number of rows you want to omit from both ends of the circulant matrix}
}
\value{The function returns the required Youden-m square design. It also returns the parameters of the PBIB design constituted by taking the incomplete columns of the Youden-m square as blocks.}
\author{Kush Sharma, Davinder Kumar Garg}
\examples{ym1(6,1)}
\keyword{Youden-m square}
\keyword{Youden-m square design}
\keyword{Partially Balanced Incomplete Block designs}
\keyword{Row-Column designs}
\keyword{Two-way eliminationof heterogenity}
