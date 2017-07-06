# Functions ---------------------------------------------------------------



myrange <- function(x){
  return(max(x)-min(x))
}

meanrange <- function(x, k, title=" "){
  require(zoo)
  x1 <- rollapply(as.zoo(x), width=k, mean, by=k)
  x2 <- rollapply(as.zoo(x), width=k, myrange, by=k)
  
  plot(x1, x2, type='p', main = title, 
       xlab="Mean", ylab="Range")
  
}

MCcorr <- function(V){
  M <- matrix(0, ncol=ncol(V), nrow=nrow(V))
  k<-nrow(V)
  for (i in (1:k)){
    for(j in (1:i)){
      M[i,j] = V[i,j]/(sqrt(V[i,i]*V[j,j]))
      M[j,i] = M[i,j]
    } 
  }
  colnames(M) <- colnames(V)
  rownames(M) <- rownames(V)
  return(M)  
}

armatable <- function(mymodel)
{
  M <- matrix(nrow=length(mymodel$coef), ncol=3)
  V <- mymodel$var.coef
  mcoef <- mymodel$coef
  M[,1] <- mcoef
  M[,2] <- sqrt(diag(V))
  M[,3] <- M[,1]/M[,2]
  colnames(M) <- c("Coef", "S.E.", "t-ratio")
  rownames(M) <- names(mcoef)
  M
}

