#' @name handleMissingData
#' @title handle missing data before clustering.
#' @export
#' @description Clustering cannot handle NAs, so we have to get rid of them
#' @param mydata is a dataframe containing the FRET reporter data in each column
#' @return mydata The data without the columns and rows which contains NAs


handleMissingData <- function (mydata){
    
    if (!any(is.na(mydata))) return(mydata)
    
    # remove columns that have more than 10 % missing data 
    include.col = c()
   
    for (icol in 1:ncol(mydata)) {
        if (sum(is.na(mydata[,icol])) < 0.1*nrow(mydata)){
            if (length(include.col)== 0) {include.col = icol}
            else {include.col = c(include.col, icol)}
        }
    }
    mydata2 = subset(mydata,select = include.col)
    
    # remove the rows which have NAs
    return(na.omit(mydata2))
    
    
}