\name{knn_cv}
\alias{knn_cv}
\alias{knn_cv_}
\title{
K-Nearest Nehbors Cross Validation
}
\description{
	Tests each provided k value and plots it as well as returning the minimum error value and related k-values.  This cross validation method works with more than two colors
}
\usage{
knn_cv(df, ..., color, k=1:200, nfold=10, times=10, na.action="mutate")
}
\arguments{
  \item{df}{
  	The dataframe to test
	}
	\item{...}{
		Columns to determine distance in knn prediction
	}
	\item{color}{
		The predicted column
	}
	\item{k}{
		The k-values to test the data with
	}
	\item{nfold}{
		The ratio of train rows to test rows as well as the times to test
		Must be greater than 1
	}
	\item{times}{
		How many folds to preduce
	}
	\item{na.action}{
		What to do with NA values.\cr
		"mutate": Convert to empty strings\cr
		"remove": Removes all NAs\cr
		"mutate|remove": Applies mutate on color and remove on other columns
		other: No action
	}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	A list of objects:
	$err: A table of all predictions\cr
	$sum: Summary of the data\cr
	$plot: The plot of the knn_cv\cr
	$min: The minumum value(s) as a table
}
\references{
	\link{knn} The function that
}
\author{
	zekrom_vale
}

\examples{
sleep\%>\%knn_cv(extra, ID, color=group, k=1:10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
