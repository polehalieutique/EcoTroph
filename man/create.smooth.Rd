\name{create.smooth}
\alias{create.smooth}
%- Also NEED an '\alias' for EACH other topic documented here. ????
\title{
Create Smooth Function
}
\description{
create.smooth is used to create a smooth function. This function enables the conversion of data pertaining to specific taxa or functional groups into data by trophic classes. The main assumption of this Smooth function is that the distribution of the biomass (or catch...) of a trophic group around its mean trophic level follows a lognormal curve. The curve is defined by a mean (the mean TL of the trophic group) and a standart deviation (sigma), which is a measure of the trophic level variability within the group. The distribution is then defined by the lognormal function LN(mean TL, sigma).
}
\usage{
create.smooth(tab_input, smooth_type=NULL, sigmaLN_cst=NULL,
	pas=NULL, shift=NULL, smooth_param=NULL)
}
%- maybe also 'usage' for other objects documented here. ????
\arguments{
  \item{tab_input}{
is the input table based on Ecopath data or on independent data. The different variables are the group name, its trophic level, biomass, production on biomass ratio, catches, omnivory index and accessibility (fraction of the group that can be catch assuming an infinite fishing effort) if the input table corresponds to an EwE model. In other case, to simply build trophic spectra, only the group names and their trophic levels are necessary.
}
  \item{smooth_type}{
defines the type of sigma calculation for the lognormal distribution. Values of this parameter are 1, 2 or 3. By default smooth_type=1, this defines a constant sigma. By choosing smooth_type=2, the user has the possibility to implement a sigmaLN=smooth_param*ln(TL-0.05), with the parameter smooth_param=0.07 and shift=0.95 by default. Smooth_type=3 corresponds to the use of the omnivory index (OI) in the sigmaLN calculation (sigmaLN=OI/TL). 
}
  \item{sigmaLN_cst}{
defines the value of the constant sigma of the lognormal distribution in case of smooth_type=1. By default, sigmaLN_cst=0.12.  
}
  \item{pas}{
defines the splitting of the TL classes. By default, pas=0.1.
}
  \item{shift}{
defines the beginning of the smooth function and allows the substraction of 0.05 in the sigma calculation accounting for the half interval range of the trophic class. By default, with a constant sigmaLN (smooth_type=1), shift=1.8; with a function defined sigmaLN (smooth_type=2), shift=0.95; and with sigmaLN=OI/TL (smooth_type=3), shift=0.
}
  \item{smooth_param}{
defines the slope of the log-linear increase of the TL variability with the mean trophic level of the group. 
SigmaLN(TL) is thus defined as sigmaLN(TL)=smooth_param*ln(TL-0.05). By default, smooth_param=0.07.
}
}
\details{
The user has the possibility to define sigmaLN for each trophic group and also adjust the LN distribution with the smooth_type, sigmaLN_cst, smooth_param, shift and pas parameters. Different choices are available : a constant sigma, a function defined sigma (log-linear increase) , or a sigma equal to the omnivory index divided by the associated mean TL.
}
\value{
create.smooth returns a table of the TL distribution within a trophic class. This table enables the calculation of Trophic Spectra, it is used in the Transpose function.
}
\author{
Colleter Mathieu and Guitton Jerome.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
plot.smooth to plot the Smooth function, Transpose to build trophic spectra, plot.Transpose to plot the trophic spectra.
}
\examples{
data(ecopath_guinee)
create.smooth(ecopath_guinee)
create.smooth(ecopath_guinee,sigmaLN_cst=0.11)
create.smooth(ecopath_guinee,smooth_type=2,pas=0.2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~EcoTroph }
\keyword{ ~Smooth }
