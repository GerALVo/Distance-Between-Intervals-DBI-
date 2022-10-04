if (!any(installed.packages()[,1]=="docstring")){
  install.packages("docstring")
}
require(docstring, warn.conflicts = F)
dbi <- function (x, diag = FALSE, upper = FALSE) {
  
  #' @title Distance Between Intervals (DBI)
  #' @description This function computes and returns the pairwise distance between intervals (i.e., continuous character coded as a lower and an upper limit).
  #' 
  #' @param x a numeric matrix or data frame with the lower and upper values of the interval in separated columns
  #' @param daig logical value indicating whether the diagonal of the distance matrix should be returned 
  #' @param upper logical value indicating whether the upper triangle of the distance matrix should be returned
  #' 
  #' @details Missing values are allowed returning  \code{\link{NA}} values for the computed distances.
  #' 
  #' @return  \code{\link{dbi()}} returns an object of class \code{\link[stats]{"dist"}}. For more information on the object attributes see \code{\link[stats]{dist()}}.
  #' 
  #' @references Lo Valvo, G. A., Balseiro, D., and Lehmann, O. E. R. Submit. \emph{Distance between intervals (DBI): a novel distance coefficient mixing overlap indices and euclidean distances}.
  #' @examples a<-data.frame(runif(15,0,2))
  #' a[,2]<-a+runif(15)
  #' dbi(a)
  #' dbi(a,diag=T)
  #' dbi(a,diag=T,upper=T)
  #' @author Gerardo A. Lo Valvo, Diego Balseiro, and Oscar E. R. Lehmann
  
  dbi.int <- function (datum, dataset){
    D=NULL
    #RANGE of the group being compared (GBC):
    r1 <- max(datum) - min(datum)
    #RANGES of each group (EG):
    r2 <- pmax(dataset[,1], dataset[,2]) - pmin(dataset[,1], dataset[,2])
    
    #CHARACTER DISTANCE between the lower limit of the GBC and EG:
    D1 <- min(datum) - pmax(dataset[,1], dataset[,2])
    #CHARACTER DISTANCE between the upper limit of the GBC and EG:
    D2 <- pmin(dataset[,1], dataset[,2]) - max(datum)
    #Choosing the highest CHARACTER DISTANCE between the GBC and EG.
    Dtemp <- pmax(D1, D2) #This reflects the relationships between the intervals.
      
    #DISTANCE BETWEEN INTERVALS without overlap:
    D [Dtemp>=0 & !is.na(Dtemp)] <- 1 + (Dtemp[Dtemp>=0 & !is.na(Dtemp)])
    #DISTANCE BETWEEN INTERVALS with overlap:
    D [Dtemp<0 & !is.na(Dtemp)] <- 1 + (Dtemp[Dtemp<0 & !is.na(Dtemp)]
                                        / pmin(r1, r2)[Dtemp<0 & !is.na(Dtemp)])
    #DISTANCE BETWEEN RANGES is 0 when one range is 0 and the value is equal to 
    #one of the limits of the other interval, otherwise it whould be 1 and we
    #consider this case as fully overlapped:
    D [Dtemp==0 & pmin(r1,r2)==0] <- 0
    #Force DISTANCE BETWEEN RANGES as 0 when one interval is embedded in the
    #other, othewise it would be negative.
    D [D<0] <- 0 
    
    #Assignment of the groups' names to the distance:
    names(D) <- rownames(dataset)
    
    
    return (D)}
  
  dbi_out <- as.dist(apply(x,1,dbi.int,dataset=x),diag = diag, upper = upper)

  return(dbi_out)
}
