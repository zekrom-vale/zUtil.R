\name{knn_cv}
\alias{knn_cv}
\alias{knn_cv_}
\title{
K-Nearest Nehbors Cross Validation
}
\description{
	Tests each provided k value and plots it as well as returning the minimum error value and related k-values.  This cross validation method works with more than two colors.\cr
	\code{knn_cv_} is the alternate and less graphical version
}
\usage{
knn_cv(df, ..., color, k=1:200, nfold=10, times=10, na.action="mutate", l=0, prob=FALSE, use.all=TRUE)
\cr
knn_cv_(df, ..., color,k=1:50, nfold=10, times=1, na.action="mutate", l=0, prob=FALSE, use.all=TRUE)
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
		The ratio of train rows to test rows as well as the times to test\cr
		Must be greater than 1
	}
	\item{times}{
		How many folds to preduce
	}
	\item{na.action}{
		What to do with NA values.\cr
		\code{"mutate"}: Convert to empty strings\cr
		\code{"remove"}: Removes all NAs\cr
		\code{"mutate|remove"}: Applies mutate on color and remove on other columns
		other: No action
	}
	\item{l}{
		minimum vote for definite decision, otherwise \code{doubt}. (More precisely, less than \code{k-l} dissenting votes are allowed, even if k is increased by ties.)
	}
	\item{prob}{
		If this is true, the proportion of the votes for the winning class are returned as attribute \code{prob}.
	}
	\item{use.all}{
		controls handling of ties. If true, all distances equal to the kth largest are included. If false, a random selection of distances equal to the \code{k}th is chosen to use exactly k neighbours.
	}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	A list of objects:\cr
	\code{$err}: A table of all predictions\cr
	\code{$sum}: Summary of the data\cr
	\code{$plot}: The plot of the knn_cv\cr
	\code{$min}: The minumum value(s) as a table
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
