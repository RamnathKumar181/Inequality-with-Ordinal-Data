\name{I_alpha}
\alias{I_alpha}
\title{I_alpha gives inequality when alpha is neither zero nor one}
\usage{
I_alpha(n,s,e,alpha)
}
\arguments{
  \item{n}{A vector containg number of people in each observation}

  \item{s}{A vector containg status of each observation}

  \item{e}{A scalar containg equality or reference point}

  \item{alpha}{A scalar containg sensitivity of inequality with status}

}

\description{
Function is used to calculate inequality of the data when alpha is neither zero nor one.
}
\examples{
I_alpha(c(0,50,25,25), c(0,1,0.5,0.25),alpha = 4, e=1)
}
