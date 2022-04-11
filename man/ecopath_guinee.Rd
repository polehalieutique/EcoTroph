\name{ecopath_guinee}
\alias{ecopath_guinee}
\docType{data}
\title{
EcoTroph example dataset: Guinean data
}
\description{
This example dataset is extracted from the 2004 Guinean Ecopath model (Gascuel et al., 2009). It provides a template for the input table formatting, the wanted variables names and the different capabilities of this package (used in function's examples).
}
\usage{data(ecopath_guinee)}
\format{
  A data.frame with 35 observations on the following 8 variables.
  \describe{
    \item{\code{group_name}}{a character vector corresponding to the names of the trophic groups used in the Ecopath model. Has obligatory to be written 'group_name'.}
    \item{\code{TL}}{a numeric vector corresponding to the trophic level of the associated trophic groups. Has obligatory to be written 'TL'.}
    \item{\code{biomass}}{a numeric vector corresponding to the biomass of the associated trophic groups. Has obligatory to be written 'biomass'.}
    \item{\code{prod}}{a numeric vector corresponding to the production on biomass ratio. For the Detritus groups (no P/B value entered in Ecopath), put 0 as a value. Has obligatory to be written 'prod'.}
    \item{\code{catch.1}}{a numeric vector corresponding to the catch of the artisanal fleet. A value must be entered for all groups, with a 0-value if no catch are made. Has obligatory to be written 'catch.something'.}
    \item{\code{catch.2}}{a numeric vector corresponding to the catch of the industrial fleet. A value must be entered for all groups, with a 0-value if no catch are made. Has obligatory to be written 'catch.something'}
    \item{\code{accessibility}}{a numeric vector corresponding to the fraction of the trophic group that can be catch assuming an infinite fishing effort. Has obligatory to be written 'accessibility'.}
    \item{\code{OI}}{a numeric vector corresponding to the omnivory index calculated by Ecopath for each trophic group. Has obligatory to be written 'OI'.}
  }
}
\details{
No NA are accepted in the dataset (0 for the P/B of the detritus groups, 0 for the catch...). Follow the instructions stated in the variables descriptions. Different fleets can be entered in the model using the following system: catch.1, catch.2, catch.whatyouwant... If there is only one fleet, you just have to put catch as a variable name.
}
\source{
Gascuel et al. (2009) Impact de la peche sur l'ecosysteme marin de Guinee - Modelisation EwE 1985/2005 - 
}
\examples{
data(ecopath_guinee)
ecopath_guinee
names(ecopath_guinee)
}
\keyword{ ~EcoTroph }
\keyword{ ~datasets }
