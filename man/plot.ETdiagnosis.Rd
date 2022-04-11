\name{plot.ETdiagnosis}
\alias{plot.ETdiagnosis}
%- Also NEED an '\alias' for EACH other topic documented here.???
\title{
Plot Diagnosis Function
}
\description{
This function enables the creation of the principle graphics resulting from the create.ETdiagnosis function.  
}
\usage{
\method{plot}{ETdiagnosis}(x,scale=NULL,maxrange=NULL,legend.cex=NULL,ask=interactive(),...)
}
%- maybe also 'usage' for other objects documented here.???
\arguments{
  \item{x}{
is the list object returned by the create.ETdiagnosis function.
}
  \item{scale}{
is the scale parameter of the Biomass Trophic Spectra, can be log or by default the standard scale of results.
}
\item{maxrange}{
is the maximum TL wanted for the x-axis. By default maxrange = 5.5.
}
  \item{legend.cex}{
defines the value of the cex for the legend.
}
\item{ask}{
default value is interactive. Parameter used to enable the user to control the display of each graph.
}
\item{\dots}{plot other arguments}
}
\details{
The scale parameter controls the scale of the BTS y-axis, it corresponds by default to the scale of results, but it's usually more practical to use a log scale.
}
\value{
The function returns the principal plots of the global ET-Diagnosis routine: the graphics of the biomass, accessible biomass...rates for the different effort multipliers, the Biomass Trophic Spectra (BTS) for the different effort multipliers, the B/Bref(mE=1) and Y/Yref graphs for the main TL classes and the Catch Trophic Spectra (CTS) (global and per fleet).
}
\author{
Colleter Mathieu, Guitton Jerome and Gatti Paul.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(ecopath_guinee)
diagn.list<-create.ETdiagnosis(create.ETmain(ecopath_guinee),same.mE=TRUE)
plot(diagn.list)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~EcoTroph }
\keyword{ ~Diagnosis }