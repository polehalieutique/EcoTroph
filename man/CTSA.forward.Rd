\name{CTSA.forward}
\alias{CTSA.forward}
\title{
Catch Trophic Spectrum Analysis (CTSA) - Forward
}
\description{
CTSA.forward enables to estimate biomass, biomass flow, kinetic of an ecosystem starting from a biomass or biomass flow at TL=1, catches, transfer efficiencies, and kinetics at virgin state.
}
\usage{
CTSA.forward(catch,Prod.TL1=NULL,Biomass.TL1=NULL,TE=NULL,
	Kin=NULL,Kin_acc=NULL,temp=NULL,Selec=NULL,TL50=NULL,
	asymptote=NULL,slope=NULL,TopD=NULL,FormD=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{catch}{
is a formatted list of catch per fleet. It should be a data.frame with TL classes in rows and trophic groups in columns.
}
  \item{Prod.TL1}{
is the value of biomass flow at TL=1 used to initiate CTSA computations.
}
  \item{Biomass.TL1}{
is the value of biomass at TL=1 used to initiate CTSA computations. Only one of the arguments Biomass.TL1/Prod.TL1 should be implemented.
}
  \item{TE}{
is the value of transfer efficiency (in %) (Natural loss rate = -log(TE/100)). The user can specify a numeric value which is applied to each TL class, or a numeric vector (of the same length as TL classes), i.e. a value for each TL class.
}
  \item{Kin}{
is the kinetic of biomass flow at virgin state.
}
  \item{Kin_acc}{
is the kinetic of accessible biomass flow at virgin state.
}
  \item{temp}{
is the mean ecosytem temperature used to estimate Kin and/or Kin_acc, if unknown, based on empirical equations function of temperature and TL (see Gascuel et al. (2008)).
}
  \item{Selec}{
is the selectivity vector used to calculate accessible biomass from total biomass. If unknown, a selectivity curve can be computed using a logistic function which takes as input parameters TL50, asymptote and slope.
}
  \item{TL50}{
is the TL at first catch, parameter of the logistic selectivity curve.
}
  \item{asymptote}{
is the asymptote value of the logistic selectivity curve.
}
  \item{slope}{
is the slope value of the logistic selectivity curve.
}
  \item{TopD}{
is a coefficient expressing the top-down control, i.e. the fraction of the natural mortality depending on predator abundance. It varies between 0 and 1. The user can specify a numeric value, which is applied to each TL class, or a numeric vector (of the same length as TL classes), i.e. a value for each different TL class (default=0.4).
}
  \item{FormD}{
is a shape parameter varying between 0 and 1. It defines the functional relationship between prey and predators. The value 1 refers to a situation where predators abundance has a linear effect on the speed of the flow of their preys. The user can specify a numeric value, which is applied to each TL class, or a numeric vector (of the same length as TL classes), i.e. a different value for each TL class (default=0.5).
}
}
\value{
This function returns a list object containing the catches per fleet, the ET-Main table containing all the variables characterizing the ecosystem state. This list object can be used as an input for the create.ETdiagnosis function in order to test fishing impacts on the "reconstituted ecosystem".
}
\author{
Gatti Paul, Colleter Mathieu, Guitton Jerome.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{CTSA.catch.input to create input data for the CTSA.forward function, and create.ETdiagnosis to run the ET-Diagnosis routine.
}
\examples{
data(ecopath_guinee)
catch.group=ecopath_guinee[,c("group_name","TL","catch.1","catch.2")]
Y_test <- CTSA.catch.input(catch.group)
B.TL1=create.ETmain(ecopath_guinee)$ET_Main$B[1]
results <- CTSA.forward(Y_test,Biomass.TL1=B.TL1,
	asymptote=.7,TL50=3,slope=5,temp=28,TE=10)
results
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~EcoTroph }
\keyword{ ~CTSA }