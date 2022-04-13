#'  plot.smooth is used to plot the Smooth function. This function enables the user to see the TL distributions around their mean trophic levels.
#' @param x is the table returned by the create.smooth function.
#' @param \dots plot other arguments
#' @seealso create.smooth function to create the Smooth, Transpose to calculate the data transposition into trophic spectra.
#' @examples
#' data(ecopath_guinee)
#' plot(create.smooth(ecopath_guinee))
#' plot(create.smooth(ecopath_guinee,smooth_type=2))
#' @export

plot.smooth <-
function(x,...)
{
tab_smooth <- x[-1,]
plot(rownames(tab_smooth),rep(0,length(rownames(tab_smooth))),type="l",ylab="Smooth",xlab="TL",col="blue",ylim=range(0,max(tab_smooth)))

for (compteur in 3:ncol(tab_smooth))
lines(rownames(tab_smooth),tab_smooth[,compteur],type="l",col="blue")
}

