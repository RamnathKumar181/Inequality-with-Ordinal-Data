\name{I_0}
\alias{I_0}
\title{I_0 gives inequality when alpha is zero}
\usage{
I_0(n,s,e)
}
\arguments{
  \item{n}{A vector containg number of people in each observation}

  \item{s}{A vector containg status of each observation}

  \item{e}{A scalar containg equality or reference point}
}

\description{
Function is used to calculate inequality of the data when alpha is set to zero
}
\examples{
I_0(c(0,50,25,25), c(0,1,0.5,0.25), e=1)
}
