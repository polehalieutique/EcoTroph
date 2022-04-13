#' @title Sigma Saturation Function
#' @usage saturation(sigma_inf = NULL, coeff = NULL, pas = NULL)
#' @description This function enables an other calculation for the sigma of the create.smooth function. Sigma is calculated on the base of a saturation function reflecting a biological reasoning about the variability of the TL within trophic classes: the variability increases with the TL and reaches a plateau after a certain TL.
#' @param sigma_inf defines the value of the curve's plateau.
#' @param coeff defines the value of the slope.
#' @param pas  defines the splitting of the TL classes.
#' @details By default sigma is constant. This function enables an other user choice reflecting a different reasoning.
#' @return saturation returns a vector of values for the sigma used in the create.smooth function.
#' @seealso create.smooth function to create the Smooth, plot.smooth to plot the smooth function.
#' @examples
#' plot(saturation())
#' lines(saturation(0.2))
#' text(48,0.18,"sigma_inf=0.2")
#' lines(saturation(coeff=0.5))
#' text(48,0.35,"coeff=0.5")
#' @export

saturation <-
function(sigma_inf=NULL,coeff=NULL,pas=NULL)
{
if (is.null(sigma_inf)) sigma_inf <- 0.4                 ## default values
if (is.null(coeff)) coeff <- 1
if (is.null(pas)) pas <- 0.1

TL<- seq(from=2, to=7, by=pas)
sigma<-sigma_inf*(1-exp(-coeff*(TL-2)))
return(sigma)
}

