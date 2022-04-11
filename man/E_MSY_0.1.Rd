\name{E_MSY_0.1}
\alias{E_MSY_0.1}
\title{
E_MSY_0.1: Exploitation indices
}
\description{
E_MSY_0.1 computes two indices of exploitation: Emsy or Fmsy (maximum sustainable yield), and E0.1 or F0.1 ("start" of full exploitation) per TL class.
}
\usage{
E_MSY_0.1(data, Mul_eff=NULL, B.Input=NULL, Beta=NULL,
	TopD=NULL, FormD=NULL, TLpred=NULL, maxTL=NULL)
}
\arguments{
  \item{data}{
is the list object returned by the create.ETmain function.
}
  \item{Mul_eff}{
is a parameter of the create.ETdiagnosis function. It is a vector of fishing effort multipliers that the user wants to test. Mul_eff must contain the value 1 (reference state). By default, the function simulates a range of fishing effort multipliers from 0 to 5 for each fleet.
}
  \item{B.Input}{
is a parameter of the create.ETdiagnosis function. It is a logical argument (default=F), if TRUE the "Biomass input control" equation is accounted for in EcoTroph equations.
}
  \item{Beta}{
is a parameter of the create.ETdiagnosis function. It is a coefficient expressing the extent of the biomass input control. Beta=0 refers to an ecosystem where all secondary production originates from grazing on primary producers, and Beta=1 to an ecosystem where detritus and/or recruitment contribute to a major part of the biomass input (default=0.2).
}
  \item{TopD}{
is a parameter of the create.ETdiagnosis function. It is a coefficient expressing the top-down control, i.e. the fraction of the natural mortality depending on predator abundance. It varies between 0 and 1. The user can specify a numeric value, which is applied to each trophic level, or a numeric vector (of the same length as TL classes), i.e. a value for each TL (default=0.4).
}
  \item{FormD}{
is a parameter of the create.ETdiagnosis function. It is a shape parameter varying between 0 and 1. It defines the functional relationship between prey and predators. The value 1 refers to a situation where predators abundance has a linear effect on the speed of the flow of their preys. The user can specify a numeric value, which is applied to each trophic level, or a numeric vector (of the same length as TL classes), i.e. a value for each TL (default=0.5).
}
  \item{TLpred}{
is a parameter of the create.ETdiagnosis function. It is the trophic level that the user considers to be the "predator" trophic classes start. The default value is 3.5.
}
  \item{maxTL}{
is a numeric string indicating the maximum TL for which indices are computed.
}
}
\details{
For any TL class, if E0.1 and/or Emsy value(s) is(are) equal to the maximum effort multiplier tested (max(Mul_eff)), then E/F0.1 and/or E/Fmsy are set equal to NA.
}
\value{
The E_MSY_0.1 function returns a data.frame containing Fmsy, Emsy, F0.1 and E0.1 per TL class.
}
\author{
Gatti Paul, Colleter Mathieu, Guitton Jerome.
}

\examples{
data(ecopath_guinee)
\donttest{E_MSY_0.1(create.ETmain(ecopath_guinee))}
}

\keyword{ ~EcoTroph }
\keyword{ ~Diagnosis }