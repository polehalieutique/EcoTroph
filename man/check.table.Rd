\name{check.table}
\alias{check.table}
%- Also NEED an '\alias' for EACH other topic documented here. ????
\title{
Check Ecopath table function
}
\description{
This function enables the verification of input tables based on EwE data and used in the EcoTroph routine. A template is provided in the example: data(ecopath_guinee).
}
\usage{
check.table(ecopath)
}
%- maybe also 'usage' for other objects documented here. ????
\arguments{
\item{ecopath}{
is the input table used in ET. The different variables are the group name, its trophic level, biomass, production, catches, omnivory index and accessibility (fraction of the group that can be catch assuming an infinite fishing effort).
}
}
\details{
The user have the possibility to check its input table, notably the headings and values (no NA).
}
\value{
The check.table function returns notifications related to the input table and its reliability to run EcoTroph (problems in the headings or values).
}
\author{
Colleter Mathieu and Guitton Jerome.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(ecopath_guinee)
check.table(ecopath_guinee)
}
\keyword{ ~EcoTroph }