\name{read.ecopath.model}
\alias{read.ecopath.model}
%- Also NEED an '\alias' for EACH other topic documented here.????
\title{
Input data import function (from an xml file)
}
\description{
This function loads input data from an xml file created by the user, or exported from the EwE EcoTroph plug-in, or from a web service associated to a database populated with parameters of several EwE models.
}
\usage{
read.ecopath.model(filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
is the address of the file the user wants to import.
}
}
\value{
This function returns a data.frame containing all the column needed to run the EcoTroph R package.
}
\author{
Colleter Mathieu and Guitton Jerome.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
check.table to control the reliability of the dataset.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~EcoTroph }

