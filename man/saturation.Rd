\name{saturation}
\alias{saturation}
%- Also NEED an '\alias' for EACH other topic documented here. ????
\title{
Sigma Saturation Function
}
\description{
This function enables an other calculation for the sigma of the create.smooth function. Sigma is calculated on the base of a saturation function reflecting a biological reasoning about the variability of the TL within trophic classes: the variability increases with the TL and reaches a plateau after a certain TL.
}
\usage{
saturation(sigma_inf = NULL, coeff = NULL, pas = NULL)
}
%- maybe also 'usage' for other objects documented here.??
\arguments{
  \item{sigma_inf}{
defines the value of the curve's plateau.
}
  \item{coeff}{
defines the value of the slope.
}
  \item{pas}{
defines the splitting of the TL classes.
}
}
\details{
By default sigma is constant. This function enables an other user choice reflecting a different reasoning.
}
\value{
saturation returns a vector of values for the sigma used in the create.smooth function.
}
\author{
Colleter Mathieu and Guitton Jerome.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
create.smooth function to create the Smooth, plot.smooth to plot the smooth function.
}
\examples{
plot(saturation())
lines(saturation(0.2))
text(48,0.18,"sigma_inf=0.2")
lines(saturation(coeff=0.5))
text(48,0.35,"coeff=0.5")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~EcoTroph }
\keyword{ ~Smooth }
