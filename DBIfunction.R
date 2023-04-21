if (!any(installed.packages()[, 1] == "docstring")) {
  install.packages("docstring")
}
require(docstring, warn.conflicts = F)
dbi <- function (x, diag = FALSE, upper = FALSE, gower = FALSE) {
  #' @title Distance Between Intervals (DBI)
  #' @description This function computes and returns the pairwise distance 
  #' between intervals (i.e., continuous character coded as a lower and an upper
  #'  limit).
  #'
  #' @param x a numeric matrix or data frame with the lower and upper values of
  #'  the interval in separated columns
  #' @param diag logical value indicating whether the diagonal of the distance 
  #' matrix should be returned
  #' @param upper logical value indicating whether the upper triangle of the 
  #' distance matrix should be returned
  #' @param gower logical value indicating whether Gower's standardization 
  #' criteria for quantitative characters is applied
  #'
  #' @details Missing values are allowed returning  \code{\link{NA}} values for
  #'  the computed distances.  
  #'  
  #' The \code{gower} parameter incorporates the standardization criteria suggested by
  #'  Gower for quantitative characters for his coefficient (Gower, 1971 p. 859).
  #'
  #' @return  \code{\link{dbi()}} returns an object of class 
  #' \code{\link[stats]{"dist"}}. For more information on the object attributes
  #'  see \code{\link[stats::dist()]{dist()}}.
  #'  
  #'  If \code{gower = TRUE} the distances will be bounded between 0 and 1
  #'
  #' @references Gower, J. C. 1971. A general coefficient of similarity and some
  #' of its properties. Biometrics 27, 857-871.  
  #' 
  #' This work.
  #' @examples 
  #' a <- data.frame(runif(15, 0, 2))
  #' a[, 2] <- a + runif(15)
  #' dbi(a)
  #' dbi(a, diag = TRUE)
  #' dbi(a, diag = TRUE, upper = TRUE)
  #' dbi(a, gower = TRUE)
  #' @author
  
  dbi.int <- function (datum, dataset) {
    D = NULL
    #RANGE of the group being compared (GBC):
    r1 <- max(datum) - min(datum)
    #RANGES of each group (EG):
    r2 <-
      pmax(dataset[, 1], dataset[, 2]) - pmin(dataset[, 1], dataset[, 2])
    
    #CHARACTER DISTANCE between the lower limit of the GBC and EG:
    D1 <- min(datum) - pmax(dataset[, 1], dataset[, 2])
    #CHARACTER DISTANCE between the upper limit of the GBC and EG:
    D2 <- pmin(dataset[, 1], dataset[, 2]) - max(datum)
    #Choosing the highest CHARACTER DISTANCE between the GBC and EG.
    Dtemp <-
      pmax(D1, D2) #This reflects the relationships between the intervals.
    
    #DISTANCE BETWEEN INTERVALS without overlap:
    D [Dtemp >= 0 & !is.na(Dtemp)] <-
      1 + (Dtemp[Dtemp >= 0 & !is.na(Dtemp)])
    #DISTANCE BETWEEN INTERVALS with overlap:
    D [Dtemp < 0 & !is.na(Dtemp)] <-
      1 + (Dtemp[Dtemp < 0 & !is.na(Dtemp)] /
             pmin(r1, r2)[Dtemp < 0 & !is.na(Dtemp)])
    #DISTANCE BETWEEN RANGES is 0 when one range is 0 and the value is equal to
    #one of the limits of the other interval, otherwise it whould be 1 and we
    #consider this case as fully overlapped:
    D [Dtemp == 0 & pmin(r1, r2) == 0] <- 0
    #Force DISTANCE BETWEEN RANGES as 0 when one interval is embedded in the
    #other, othewise it would be negative.
    D [D < 0] <- 0
    
    #Assignment of the groups' names to the distance:
    names(D) <- rownames(dataset)
    
    
    return (D)
  }
  
  
  dbi_out <-
    as.dist(apply(x, 1, dbi.int, dataset = x),
            diag = diag,
            upper = upper)
  
  if (gower == TRUE) {
    dbi_gow <- dbi_out / max(dbi_out, na.rm = TRUE)
    return(dbi_gow)
  } else{
    return(dbi_out)
  }}
cat("Thank you for using DBI from ---- et al. 202X \nFor more information about 'dbi' please check \nthe function's help using the command '?dbi'")