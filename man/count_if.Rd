\name{count_if}
\alias{count_if}
\title{
	Count the occurences in a vector
}
\description{
	This function counts the occurences of a condition within a vector
}
\usage{
count_if(vector, condition)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vector}{
	The vector to count the occurences
}
  \item{condition}{
	The condition to test the vector against

	It must be a function
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	The count of ocurrences in \code{vector} based on the \code{condition}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
	zekrom_vale
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link{count_when}} for using \code{str_detect} to test a vector

	\code{\link{count_if}} for using conditions to test a vector

	\code{\link{count_if}} for counting using a dataframe or tibble, this has access to all columns

	\code{\link{regex}}

	\code{\link{str_detect}}
}
\examples{
count_if(c("1","2","b","c","Z"), function(x)x=="2")
# Returns 1, counting the ones equal to "2"

count_if(c("1","2","b","c","Z"), function(x)str_detect(x,"\\w"))
# Returns 3 as it is counting the values that match \\w

count_if(1:100, function(x)x>50)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
