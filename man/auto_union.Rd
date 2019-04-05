\name{auto_union}
\alias{auto_union}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Automaticaly Union
}
\description{
	Automaticaly finds identical columns and preforms a union
}
\usage{
auto_union(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  The first tibble or data frame to union
}
  \item{y}{
  The second tibble or data frame to union
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	A tibble that is unioned
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
	\code{\link{union}}
}
\examples{
auto_union(band_instruments, band_members)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
