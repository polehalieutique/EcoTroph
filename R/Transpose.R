#'  Transpose enables the conversion of data pertaining to specific taxa or functionnal groups into data by trophic class. Data can represent catches, biomasses or production in order to produce continuous distributions of those variables over trophic levels.
#' @param tab_smooth is the table returned by the create.smooth function.
#' @param tab_input  is the input table based on Ecopath data or on independent data. The different variables are the group name, its trophic level, biomass, production on biomass ratio, catches, omnivory index and accessibility (fraction of the group that can be catch assuming an infinite fishing effort) if the input table corresponds to an EwE model. In other case, to simply build trophic spectra, only the group names, their trophic levels and related variables are necessary.
#' @param column is the tab_input table column name of the variable the user wants to transpose (for example "biomass" or "catch").
#' @seealso create.smooth function to create the Smooth, plot.smooth to plot the smooth function, plot.Transpose to plot the associated trophic spectra.
#' @examples
#' data(ecopath_guinee)
#' Transpose(create.smooth(ecopath_guinee),ecopath_guinee,"biomass")
#' Transpose(create.smooth(ecopath_guinee),ecopath_guinee,"catch.1")
#' @export
Transpose <-
function(tab_smooth,tab_input,column)
{
if(missing(tab_smooth))
cat("tab_smooth is missing\n")
if(missing(tab_input))
cat("tab_input is missing\n")
if(missing(column))
cat("column is missing\n")

tab_Trans <- array(dim=c(length(rownames(tab_smooth)),length(tab_input$group_name)),dimnames=list(rownames(tab_smooth),tab_input$group_name))
pas <- round(0.00+(as.double(rownames(tab_smooth)[length(rownames(tab_smooth))])-as.double(rownames(tab_smooth)[length(rownames(tab_smooth))-1])),3)       ## recalculation of the 'pas', argument of the create.smooth function

for(groupe in tab_input$group_name)
{
tmp_tl=tab_input[tab_input$group_name==groupe,]$TL
tmp_tl <- seq(0,7,pas)[as.numeric(cut(tmp_tl+0.0000000000001,seq(0,7,pas),right=FALSE))]  ## assignment of a trophic class to the trophic groups
tab_Trans[,groupe]=tab_input[tab_input$group_name==groupe,column]*tab_smooth[,as.character(tmp_tl)]
}

class(tab_Trans)<-"Transpose"
return (tab_Trans)
}

