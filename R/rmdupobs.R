#' Remove duplicate observations from data
#' 
#'  Takes in a data, and returns it with duplicate observations removed
#'  @param data a data.frame or data.table
#'  @return a \code{data} of same class as input with only unique observations
#'  @author Akash Jain
#'  @seealso \code{\link{randomise}}, \code{\link{rmdupkey}}, \code{\link{factorise}}
#'  @examples
#'  # Let's create a data.frame
#' df <- data.frame(x = c(1, 2, 5, 1), y = c(3, 3, 1, 3))
#'
#' # Remove duplicate observations from data
#' dfUnq <- rmdupobs(data = df)
#'  @export
rmdupobs <- function(data) {
  if(class(data)[1] != 'data.frame' && class(data)[1] != 'data.table') {
    stop('Invalid input: data should be either data.frame or data.table')
  } else if(class(data)[1] == 'data.frame') {
    unqData <- data[!duplicated(data), ]
  } else if(class(data)[1] == 'data.table') {
    unqData <- unique(data)
  }
  diff <- nrow(data) - nrow(unqData)
  print(paste('There are', diff, 'duplicate observations in the data', sep = ' '))
  return(unqData)
}