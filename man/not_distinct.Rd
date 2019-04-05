\name{not_distinct}
\alias{not_distinct}
\title{
	Returns a table of non-distinct elements
}
\description{
	This function returns the elements that are not distinct from a tibble or data frame \code{df}
}
\usage{
not_distinct(df, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
	The tible or data frame to get non-distinct elements from
}
  \item{\dots}{
	The columns to consider to determine distinctness
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	A tibble containing all columns and the rows that are not distinct
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
	\code{\link{not_distinct_}} The legacy function

	\code{\link{distinct}} Removes all but one of non-distinct elements
}
\examples{
df=tibble(x=1:100, y=(1:100)/2, z=as.integer(rnorm(100, 50, 50)))
not_distinct(df,z)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
