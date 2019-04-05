\name{count_ifs}
\alias{count_ifs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Counts the occurences of the rows
}
\description{
	This function counts the rows that match the given conditions
}
\usage{
count_ifs(df, ..., .group = NULL, .wt = NULL, .sort = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
  The tibble or dataframe to count
}
  \item{\dots}{
  The conditions to test the tibble (\code{df})
}
  \item{.group}{
  How to group the data when counting, if \code{NULL} it will use \code{tally} instead

  Currently not working
}
  \item{.wt}{
  The weight to pass to \code{count} or \code{tally}
}
  \item{.sort}{
  The sort to pass to \code{count} or \code{tally}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	A vector of values in the order of \code{.order} or just a single value if \code{order} is \code{NULL}
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
count_ifs(band_instruments, name=="John")
# 1
count_ifs(band_instruments, name=="Keith"|str_detect(name,"(?i)Pa"))
# 2
count_ifs(band_instruments, name=="Keith"|name=="Paul")
# 2
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
