\name{CTSA.catch.input}
\alias{CTSA.catch.input}
%- Also NEED an '\alias' for EACH other topic documented here. ????
\title{
Catch input for CTSA
}
\description{
CTSA.catch.input is used to create inputs for the CTSA.forward function. It is a list of data.frames referring to catches per fleet formatted with TL classes in rows and trophic groups in columns.
}
\usage{
CTSA.catch.input(catch.group,smooth_type=NULL,sigmaLN_cst=NULL,
	pas=NULL,shift=NULL,smooth_param=NULL)
}
%- maybe also 'usage' for other objects documented here. ????
\arguments{
  \item{catch.group}{
is a data.frame containing: a column group_name, column(s) referring to the catches of each fleet (named 'catch.1', 'catch.2'...), a column TL specifying the mean TL of each group, and optionally a column OI (omnivory index) used for smooth_type=3.
}
  \item{smooth_type}{
is a parameter of the create.smooth function. It defines the type of sigma calculation for the lognormal distribution. Values of this parameter are 1, 2 or 3. By default smooth_type=1, this defines a constant sigma. By choosing smooth_type=2, the user has the possibility to implement a sigmaLN=smooth_param*ln(TL-0.05), with the parameter smooth_param=0.07 and shift=0.95 by default. Smooth_type=3 corresponds to the use of the omnivory index (OI) in the sigmaLN calculation (sigmaLN=OI/TL). 
}
  \item{sigmaLN_cst}{
is a parameter of the create.smooth function. It defines the value of the constant sigma of the lognormal distribution in case of smooth_type=1. By default, sigmaLN_cst=0.12.  
}
  \item{pas}{
is a parameter of the create.smooth function. It defines the splitting of the TL classes. By default, pas=0.1.
}
  \item{shift}{
is a parameter of the create.smooth function. It defines the beginning of the smooth function and allows the substraction of 0.05 in the sigma calculation accounting for the half interval range of the trophic class. By default, with a constant sigmaLN (smooth_type=1), shift=1.8; with a function defined sigmaLN (smooth_type=2), shift=0.95; and with sigmaLN=OI/TL (smooth_type=3), shift=0.
}
  \item{smooth_param}{
is a parameter of the create.smooth function. It defines the slope of the log-linear increase of the TL variability with the mean trophic level of the group. SigmaLN(TL) is thus defined as sigmaLN(TL)=smooth_param*ln(TL-0.05). By default, smooth_param=0.07.
}
}
\value{
CTSA.catch.input returns a list of data.frames, referring to catches per fleet formatted with TL classes in rows and trophic groups in columns.
}
\author{
Gatti Paul, Colleter Mathieu, Guitton Jerome.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
create.smooth, Transpose and CTSA.forward.
}
\examples{
data(ecopath_guinee)
catch.group=ecopath_guinee[,c("group_name","TL","catch.1","catch.2")]
Y_test <- CTSA.catch.input(catch.group)
Y_test
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~EcoTroph }
\keyword{ ~CTSA }
