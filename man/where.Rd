\name{where}
\alias{where}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Filter where the columns match \code{str_detect}
}
\description{
	Uses \code{str_detect} to filter the rows that match the regular expression
}
\usage{
where(df, ..., .negate = FALSE, .op = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
  The tibble or dataframe to use
}
  \item{\dots}{
  The conditions to test each row.  It uses the folowing syntax to filter rows:

  \code{<column> = <regex>}
}
  \item{.negate}{
  Wether to negate the result of \code{str_detect}
}
  \item{.op}{
  The operation to use when evaluating multiple column value pairs.  By defult it is \code{NULL} and acts like \code{`&`}.
}
}
\details{
	Use \code{.op=NULL} instead of \code{.op=`&`} as the later uses the reduce function.
}
\value{
	A tibble that has rows that matches the given conditions
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
	\code{\link{filter_na}} Filters a tibble where the vlaues are NA

	\code{\link{filter_not_na}} Filters a tibble where the vlaues are not NA

	\code{\link{where}} Filters a tibble matching regular expressions
}
\examples{
df=tibble(name=c("ABC", "123", "EDF", "456", "#@!", "GFSD"))
where(df, name="[[:alpha:]]")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
