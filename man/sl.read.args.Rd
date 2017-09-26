\name{sl.read.args}
\alias{sl.read.args}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read Named Arguments in Batch Mode
}
\description{
Read arguments provided when a script is called from the command line through \code{Rscript}. Based on \code{R} function \code{\link{commandArgs}}, but allowing to specify named arguments in the style \code{<argname>,<arg>}, returning an accordingly named list.
}
\usage{
sl.read.args(sep = ",", missval = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sep}{
a character specifying the separator between argument names and their arguments. Default is ','.
}
  \item{missval}{
a character or NULL specifying which value is assigned to argument names where no argument follows. Default is NULL.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  A character list with the arguments, named according to the argument names. Returns NULL if no arguments are found.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%
}
\examples{
##    Consider the following two-lines script with file name 'script.R':
## ---
## library(spheRlab)
## sl.read.args()
## ---
##    Invoking this script from the command line with
## ---
## Rscript script.R arg1name,5 arg2name,hello arg3name,3.1415926536
## ---
##    yields
## ---
## $arg1name
## [1] "5"
## 
## $arg2name
## [1] "hello"
## 
## $arg3name
## [1] "3.1415926536"
## ---
##   To convert numeric arguments, use e.g. as.numeric() .
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
