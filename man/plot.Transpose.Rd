\name{plot.Transpose}
\alias{plot.Transpose}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Transpose Function
}
\description{
This function returns the two principal plots of the Transpose function : a plot by group and the associated Trophic Spectra (CTS, BTS...).
}
\usage{
\method{plot}{Transpose}(x,title = NULL,scale = NULL,legend.cex,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
is the table returned by the Transpose function.
}
  \item{title}{
defines the title of the graph.
}
  \item{scale}{
defines the scale of the y-axis: can be log or not.
}
  \item{legend.cex}{
defines the value of the cex for the legend.
}
\item{\dots}{plot other arguments}
}
\value{
The function returns the principal plots of the Transpose function : the different Trophic Spectra (CTS, BTS...) depending on the transposed column, and a plot by group.
}
\author{
Colleter Mathieu and Guitton Jerome.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
create.smooth function to create the Smooth, plot.smooth to plot the smooth function, Transpose to calculate the data transposition into trophic spectra.
}
\examples{
data(ecopath_guinee)
smoothed<-create.smooth(ecopath_guinee)
plot(Transpose(smoothed,ecopath_guinee,"biomass"),scale=log)
plot(Transpose(smoothed,ecopath_guinee,"catch.1"),
	title="Small Scale Fishery Catch")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~EcoTroph }
\keyword{ ~Transpose }

