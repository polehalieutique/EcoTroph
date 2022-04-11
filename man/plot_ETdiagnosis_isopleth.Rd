\name{plot_ETdiagnosis_isopleth}
\alias{plot_ETdiagnosis_isopleth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Diagnosis Isopleth Plot Function
}
\description{
This function enables to plot the mixed impacts of changes in fishing effort for two fleets (or groups of fleets).
}
\usage{
plot_ETdiagnosis_isopleth(x,fleet1,fleet2,var=NULL,n.level=NULL,
	relative=NULL,name.fleet1=NULL,name.fleet2=NULL,color=NULL,
	ask=interactive())
}
%- maybe also 'usage' for other objects documented here.???
\arguments{
  \item{x}{
is the list object returned by the create.ETdiagnosis function.
}
  \item{fleet1}{
is a character vector of fleets for which fishing efforts are equally changed.
}
  \item{fleet2}{
is a second character vector of fleets for which fishing efforts are equally changed. Fishing efforts remain unchanged for fleets not assigned in fleet1 or fleet2. If fleet2 is NULL, all fleets not assigned in fleet1 are assigned in fleet2. If the argument 'fleet.of.interest' has been assigned in the function create.ETdiagnosis, fleet1=fleet.of.interest and thus fleet2 is composed of the remaining fleet(s) (not assigned in fleet.of.interest).
}
  \item{var}{
is a character vector of plotted variables (TOT_biomass,TOT_biomass_acc,Y,Y_fleet1,
	Y_fleet2,TL_TOT_biomass,TL_TOT_biomass_acc,TL_Y,TL_Y_fleet1,TL_Y_fleet2).
	All the variables are plotted by default.
}
  \item{n.level}{
is a numeric string, specifying the number of plotted isopleth areas (7 is the default value).
}
  \item{relative}{
is a logical string (by default relative=F), specifying if the variables have to be plotted in absolute or relative values (in comparison with reference state, Mul_eff=1). Note that if relative=TRUE, mean trophic level in biomass or catches (TL_TOT_biomass,TL_Y,...) are not plotted.
}
  \item{name.fleet1}{
is a character string used to implement x-axis name. By default name.fleet1='fleet 1'.
}
  \item{name.fleet2}{
is a character string used to implement y-axis name. By default name.fleet2='fleet 2'. If the argument fleet.of.interest has been assigned in the function create.ETdiagnosis, name.fleet1 = 'fleet of interest' and name.fleet2 = 'other fleets'.
}
  \item{color}{
is a vector of colors, the length of this vector should be equal to the value of levels. By default, color=rainbow(n=levels).
}
\item{ask}{
default value is interactive. Parameter used to enable the user to control the display of each graph.
}
}
\details{
Fleets' names used in the arguments 'fleet1' and 'fleet2' are the catch column names of ecopath input dataframe (e.g. 'catch.1' or 'catch.ind').
}
\value{
The function plot.ETdiagnosis_isopleth plots isopleth areas of different variables (biomass, accessible biomass, total catch, mean trophic level of the total biomass...) with fishing effort multipliers for fleet1 in x-axis and for fleet2 in y-axis. This enables to display mixed impacts of changing fishing efforts of two groups of fleets.
}
\author{
Gatti Paul, Colleter Mathieu, Guitton Jerome.
}

\examples{
data(ecopath_guinee)
diagn.list=create.ETdiagnosis(create.ETmain(ecopath_guinee))
\donttest{plot_ETdiagnosis_isopleth(diagn.list,fleet1='catch.1',fleet2='catch.2')}
\donttest{plot_ETdiagnosis_isopleth(diagn.list,fleet1='catch.1',fleet2='catch.2',
	relative=TRUE)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~EcoTroph }
\keyword{ ~Diagnosis }