#'   Inequality with Ordinal Data
#'
#'   @author Ramnath Kumar


S_downward <- function(n) {
  n_sum <- sum(n)
  temp <- integer(length(n))
  for (i in 1:length(n)) {
    sum <- 0
    if (n[i] != 0) {
      for (j in 1:i) {
        sum <- sum(sum,n[j])
      }
    }
    temp[i] <- sum
  }
  return (temp/n_sum)
}

S_upward<- function(n) {
  n_sum <- sum(n)
  temp <- integer(length(n))
  for (i in 1:length(n)) {
    sum <- 0
    if (n[i] != 0) {
      sum <- n[i]
      if (i < length(n)){
        for (j in (i+1):length(n)){
          sum <- sum(sum,n[j])
        }
      }
    }
    temp[i] <- sum
  }
  return (temp/n_sum)
}

Equality <- function(n,su) {
  sum <- 0
  for (i in 1:length(n)) {
    sum <- sum + n[i]*su[i]
  }
  return (sum)
}

I_0 <- function(n,s,e) {
  temp <- log(s)
  sum <- 0
  temp[is.infinite(temp)] <- 0
  for (i in 1:length(n)){
    sum <- sum + n[i]*temp[i]
  }
  temp <- -(sum/sum(n))
  I <- temp + log(e)
  return (I)
}

I_alpha <- function(n,s,e,alpha) {
  sum <- 0
  for (i in 1:length(n)){
    temp <- (s[i]^alpha)
    sum <- sum + n[i]*temp
  }
  temp <- (sum/sum(n))
  I <- temp + (e^alpha)
  I <- I/alpha
  I<- I/(1-alpha)
  return (I)
}

I_one <- function(n,s,e) {
  temp <- log(s)
  sum <- 0
  temp[is.infinite(temp)] <- 0
  for (i in 1:length(n)){
    sum <- sum + n[i]*temp[i]*s[i]
  }
  temp <- (sum/sum(n))
  I <- temp - e*log(e)
  return (I)
}

DP <- function(x) {
  s <- readline(prompt="Enter the order of Ordinal data seperated by comma: ")
  d <- read.table(text = s, sep = ",", as.is = TRUE)
  b <- integer(length(s))
  count <- 1
  for (i in d){
    count3 <- length(which(i == as.character(x)))
    b[count] <- as.integer(count3)
    count <- count +1
  }
  return (b)
}


IOD <- function(n, alpha = 0, e=1, mean = FALSE) {
  n_sum <-  sum(n)
  su <- (S_upward(n))
  sd <- (S_downward(n))
  if (mean== TRUE) {
    e <- Equality(n,su)
    e<- e/n_sum
  }
  if (alpha == 0){
    I_up <- I_0(n,su,e)
    I_down <- I_0(n,sd,e)
  }

  if (alpha ==1 & mean == FALSE){
    I_up <- "+-infi"
    I_down <- "+-infi"
  }

  if (alpha ==1 & mean ==TRUE) {
    I_up<- I_one(n,su,e)
    I_down <- I_one(n,sd,e)
  }

  if (alpha !=1 & alpha !=0) {
    I_up<- I_alpha(n,su,e,alpha)
    I_down <- I_alpha(n,sd,e,alpha)
  }
  result <- vector(mode = "list", length = 5)
  names(result) <- c("su","sd","e","I_up","I_down")
  result[[1]] <- su
  result[[2]] <- sd
  result[[3]] <- e
  result[[4]] <- I_up
  result[[5]] <- I_down
  return(result)
}
