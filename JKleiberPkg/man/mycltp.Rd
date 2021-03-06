% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mycltp.R
\name{mycltp}
\alias{mycltp}
\title{CLT simulation for Poisson distribution}
\usage{
mycltp(n, iter, lambda = 10, ...)
}
\arguments{
\item{n}{sample size}

\item{iter}{number of iterations}

\item{lambda}{lambda in the poisson distribution}
}
\value{
plots of the sampled distribution with comparisons to the normal
}
\description{
Samples a poisson distribution multiple times in order to approximate the central limit theorem.
}
\examples{
mycltp(20, 10000, lambda=4)


}
