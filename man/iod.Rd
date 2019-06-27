\name{IOD}
\alias{IOD}
\title{Inequality with ordinal Data}
\description{
Based on the inequality with ordinal data paper written by Cowell. This implements a method to calculate inequality when data is ordinal using only number of observations in that class. Returns a list with su,sd,I_up,I_down.
These results can later be unnamed and used for further purposes. We can access each value using result['su'], result['sd'], etc.
}
\usage{
IOD(n,alpha,e,mean)
}
\arguments{
  \item{n}{A vector containg number of people in each observation}

  \item{mean}{A boolean used to define equality}

  \item{e}{A scalar containg equality or reference point}

  \item{alpha}{A scalar containg sensitivity of inequality with status}

}

\examples{
result <- IOD(c(0,50,25,25),alpha=4,e=1,mean=TRUE)
print(result['su'])
print(result['sd'])
print(result['e'])
print(result['I_up'])
print(result['I_down'])
}
