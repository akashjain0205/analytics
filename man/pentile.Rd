% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/pentile.R
\name{pentile}
\alias{pentile}
\title{Create pentiles of a variable}
\usage{
pentile(vector, decreasing = FALSE)
}
\arguments{
\item{vector}{an integer or numeric vector}

\item{decreasing}{a logical input, which if set to \code{FALSE} puts smallest values in
       pentile 1 and if set to \code{TRUE} puts smallest values in pentile 5; \code{FALSE}
       is default}
}
\value{
an integer vector of pentile values
}
\description{
Takes in a vector, and returns a vector of pentiles
}
\details{
\code{pentile} is a convinient function to get integer pentiles of an integer or
 numeric vector. By default, the smallest values are placed in the smallest pentile.

 Sometimes one may want to put smallest values in the biggest pentile, and for that
 the user can set the \code{decreasing} argument to \code{TRUE}; by default it is
 \code{FALSE}.
}
\examples{
# Let's create a vector
scores <- c(1, 4, 7, 10, 15, 21, 25, 27, 32, 35,
            49, 60, 75, 23, 45, 86, 26, 38, 34, 67)

# Create pentiles based on the values of the vector
pentileScores <- pentile(vector = scores)
pentileScores <- pentile(vector = scores, decreasing = TRUE)
}
\author{
Akash Jain
}
\seealso{
\code{\link{decile}}, \code{\link{outliers}}, \code{\link{imputemiss}}
}

