\name{Equality}
\alias{Equality}
\title{Equality when status is passed and mean is TRUE}
\usage{
Equality(n,su)
}
\arguments{
  \item{n}{A vector containg number of people in each observation}

  \item{su}{A vector containg status of observations at each level}
}

\description{
Takes input parameter as n and su or sd and returns the value of equality when mean is set to TRUE, i.e. e
}
\examples{
Equality(c(0,50,25,25), c(0,1,0.5,0.25))
}
