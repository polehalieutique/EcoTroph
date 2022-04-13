#' @title Create Smooth Function
#' @description  create.smooth is used to create a smooth function. This function enables the conversion of data pertaining to specific taxa or functional groups into data by trophic classes. The main assumption of this Smooth function is that the distribution of the biomass (or catch...) of a trophic group around its mean trophic level follows a lognormal curve. The curve is defined by a mean (the mean TL of the trophic group) and a standart deviation (sigma), which is a measure of the trophic level variability within the group. The distribution is then defined by the lognormal function LN(mean TL, sigma).
#' @usage create.smooth(tab_input, smooth_type=NULL, sigmaLN_cst=NULL,
#' @usage pas=NULL, shift=NULL, smooth_param=NULL)
#' @param tab_input is the input table used in ET (possibly based on Ecopath data). The different variables are the group name, its trophic level, biomass, production, catches, omnivory index and accessibility (fraction of the group that can be catch assuming an infinite fishing effort).
#' @param smooth_type defines the type of sigma calculation for the lognormal distribution. Values of this parameter are 1, 2 or 3. By default smooth_type=1, this defines a constant sigma. By choosing smooth_type=2, the user has the possibility to implement a sigmaLN=smooth_param*ln(TL-0.05), with the parameter smooth_param=0.07 and shift=0.95 by default. Smooth_type=3 corresponds to the use of the omnivory index (OI) in the sigmaLN calculation (sigmaLN=OI/TL).
#' @param sigmaLN_cst defines the value of the constant sigma of the lognormal distribution in case of smooth_type=1. By default, sigmaLN_cst=0.12.
#' @param pas defines the splitting of the TL classes. By default, pas=0.1.
#' @param shift defines the beginning of the smooth function and allows the substraction of 0.05 in the sigma calculation accounting for the half interval range of the trophic class. By default, with a constant sigmaLN (smooth_type=1), shift=1.8; with a function defined sigmaLN (smooth_type=2), shift=0.95; and with sigmaLN=OI/TL (smooth_type=3), shift=0.
#' @param smooth_param defines the slope of the log-linear increase of the TL variability with the mean trophic level of the group. SigmaLN(TL) is thus defined as sigmaLN(TL)=smooth_param*ln(TL-0.05). By default, smooth_param=0.07.
#' @details The user has the possibility to define sigmaLN for each trophic group and also adjust the LN distribution with the smooth_type, sigmaLN_cst, smooth_param, shift and pas parameters. Different choices are available : a constant sigma, a function defined sigma (log-linear increase) , or a sigma equal to the omnivory index divided by the associated mean TL.
#' @return create.smooth returns a table of the TL distribution within a trophic class. This table enables the calculation of Trophic Spectra, it is used in the Transpose function.
#' @seealso plot.smooth to plot the Smooth function, Transpose to build trophic spectra, plot.Transpose to plot the trophic spectra.
#'
#' @examples
#' data(ecopath_guinee)
#' create.smooth(ecopath_guinee)
#' create.smooth(ecopath_guinee,sigmaLN_cst=0.11)
#' create.smooth(ecopath_guinee,smooth_type=2,pas=0.2)
#' @export

create.smooth <-
function(tab_input,smooth_type=NULL,sigmaLN_cst=NULL,pas=NULL,shift=NULL,smooth_param=NULL)
{
shift_init <- shift
if (is.null(shift)) shift <- 1.8               ##default values
if (is.null(pas)) pas <- 0.1
if (is.null(smooth_param)) smooth_param <- 0.07
if (is.null(sigmaLN_cst)) sigmaLN_cst <- 0.12
if (is.null(smooth_type)) smooth_type <- 1

Troph_round <- tab_input$TL
for (i in 1:length(tab_input$group_name)){
tmp_tl=tab_input[i,]$TL
Troph_round[i] <- seq(0,7,pas)[as.numeric(cut(tmp_tl+0.0000000000001,seq(0,7,pas),right=FALSE))]  ## assignment of a trophic class to the trophic groups
}

TL_out <- c(1,seq(from=2, to=7, by=pas))
tab_smooth <- array(dim=c(length(TL_out),length(tab_input$group_name)),dimnames=list(TL_out,Troph_round))

toto <- c(1,2,3)
if (is.na(match(smooth_type,toto))){
cat(paste("You didn't choose a right value for smooth_type. Type 1, 2 or 3.\n"))
}

else{
if (smooth_type==1){                                  ## constant sigma
sigmaLN <- rep(sigmaLN_cst,length(Troph_round))
}
if (smooth_type==2){
sigmaLN <- smooth_param*log(Troph_round-0.05)        ##lognormal sigma
if (is.null(shift_init)) shift <- 0.95               ##default values
}
if (smooth_type==3){
sigmaLN <- tab_input$OI/Troph_round
if (is.null(shift_init)) shift <- 0
}

#Handling of the zero-values in sigmaLN (notably for sigmaLN=OI)
for (i in 1:length(tab_input$group_name)){
if(sigmaLN[i]==0){
sigmaLN[i]<- 0.01
cat(paste("the value of the sigmaLN was 0 for the group", tab_input$group_name[i],". We change it for a value of 0.01. If you want to correct it, change your dataset (fix() or else).\n"))
}}

for (i in 1:length(tab_input$group_name))
for (j in 2:length(TL_out))
if (colnames(tab_smooth)[i]>=2)
tab_smooth[j,i]<-exp(-1/2*((log(TL_out[j]-shift)-log(Troph_round[i]-shift))/sigmaLN[i])^2)/((TL_out[j]-shift)*sigmaLN[i]*((2*pi)^(1/2)))


for(i in 1:length(Troph_round))
if (colnames(tab_smooth)[i]==1)
tab_smooth[1,i]<-1

for (i in 1:length(Troph_round))
if (colnames (tab_smooth)[i]>1 & colnames (tab_smooth)[i]<2){
tab_smooth[,i] <- 0
tab_smooth[1,i] <- 2- as.numeric(colnames(tab_smooth)[i])
tab_smooth[2,i] <- as.numeric(colnames(tab_smooth)[i])-1
}

tab_smooth[is.na(tab_smooth)]<-0

sumcol <- colSums(tab_smooth)
for (i in 1:length(tab_input$group_name))
tab_smooth[,i]<- tab_smooth[,i]/sumcol[i]
class(tab_smooth)<-"smooth"
return(tab_smooth)
}
}

