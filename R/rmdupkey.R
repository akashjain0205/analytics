#' Remove observations with duplicate keys from data
#' 
#'  Takes in a data and key, and returns data with duplicate observations by key removed
#'  @param data a data.frame or data.table
#'  @param by a character vector of keys to be used
#'  @return a two element list: unique data and duplicate data
#'  @author Akash Jain
#'  @seealso \code{\link{randomise}}, \code{\link{factorise}}, \code{\link{rmdupobs}}
#'  @examples
#'  # Let's create a data.frame
#' df <- data.frame(x = c(1, 2, 1, 1), y = c(3, 3, 1, 3))
#'
#' # Remove duplicate observations by key from data
#' ltDf <- rmdupkey(data = df, by = c('x'))
#' unqDf <- ltDf$unqData
#' dupDf <- ltDf$dupData
#'  @export
rmdupkey <- function(data, by) {
  if(class(data)[1] != 'data.frame' && class(data)[1] != 'data.table') {
    stop('Invalid input: data should be either data.frame or data.table')
  } else if(class(by) != 'character') {
    stop('Invalid input: by should be a character vector')
  } else if(class(data)[1] == 'data.frame') {
    unqData <- data[!duplicated(data[, by]), ]
    dupData <- data[duplicated(data[, by]), ]
  } else if(class(data)[1] == 'data.table') {
    setkeyv(data, by)
    unqData <- data[!duplicated(data, by = by), ]
    dupData <- data[duplicated(data, by = by), ]
  }
  print(paste('There are', nrow(dupData), 'observations with duplicate keys in the data', sep = ' '))
  return(list(unqData = unqData, 
              dupData = dupData))
}