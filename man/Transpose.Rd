\name{Transpose}
\alias{Transpose}
%- Also NEED an '\alias' for EACH other topic documented here.????
\title{
ET Transpose Function
}
\description{
Transpose enables the conversion of data pertaining to specific taxa or functionnal groups into data by trophic class. Data can represent catches, biomasses or production in order to produce continuous distributions of those variables over trophic levels.
}
\usage{
Transpose(tab_smooth, tab_input, column)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab_smooth}{
is the table returned by the create.smooth function.
}
  \item{tab_input}{
is the input table based on Ecopath data or on independent data. The different variables are the group name, its trophic level, biomass, production on biomass ratio, catches, omnivory index and accessibility (fraction of the group that can be catch assuming an infinite fishing effort) if the input table corresponds to an EwE model. In other case, to simply build trophic spectra, only the group names, their trophic levels and related variables are necessary.
}
  \item{column}{
is the tab_input table column name of the variable the user wants to transpose (for example "biomass" or "catch").
}
}
\value{
The function returns a table of the choosen variable split into the defined trophic classes per group. 
}
\author{
Colleter Mathieu and Guitton Jerome.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
create.smooth function to create the Smooth, plot.smooth to plot the smooth function, plot.Transpose to plot the associated trophic spectra.
}
\examples{
data(ecopath_guinee)
Transpose(create.smooth(ecopath_guinee),ecopath_guinee,"biomass")
Transpose(create.smooth(ecopath_guinee),ecopath_guinee,"catch.1")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~EcoTroph }
\keyword{ ~Transpose }
