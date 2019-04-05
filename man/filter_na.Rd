\name{filter_na}
\alias{filter_na}
\title{
	Filter having NAs
}
\description{
	This function returns a tibble of all the values that are NA
}
\usage{
filter_na(df, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
  The data frame or tibble to process
}
  \item{\dots}{
  The columns that must have NA values
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	A tibble where the columns (\code{...}) are NA
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
y=1:100;
df=tibble(x=y, y=if_else(y>50, NA_integer_, y))
filter_na(df, y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
