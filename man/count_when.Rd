\name{count_when}
\alias{count_when}
\title{
	Count if matching a string or regular expresion
}
\description{
	This function allows the ability to count the occurences of a regular expresion or substring in a vector.
}
\usage{
count_when(vector, value)
}
\arguments{
  \item{vector}{
	The vector to count the occurances in
}
  \item{value}{
	The string or regular expression to test each vector against
}
}
\details{
	count_when passes the values through \code{string_detect}, so you can pass \code{value} through \code{regex}
}
\value{
	The count of ocurrences in \code{vector}
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


\seealso{
	\code{\link{count_when}} for using \code{str_detect} to test a vector

	\code{\link{count_if}} for using conditions to test a vector

	\code{\link{count_if}} for counting using a dataframe or tibble, this has access to all columns

	\code{\link{regex}}

	\code{\link{str_detect}}
}
\examples{
count_when(c("1","2","b","c","Z"), "\\w")
# Returns 3, counting the ones with letters
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
