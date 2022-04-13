#'  This function enables the creation of the ET-Main table (summarizing the principal results/variables in function of the TL classes) and other intermediate tables of the ET-Transpose routine. It provides a picture of an ecosystem under a given fishing mortality.
#' @usage create.ETmain(ecopath, smooth_type=NULL, sigmaLN_cst=NULL,
#' @usage pas=NULL, shift=NULL, smooth_param=NULL)
#' @param ecopath is the input table used in ET (possibly based on Ecopath data). The different variables are the group name, its trophic level, biomass, production, catches, omnivory index and accessibility (fraction of the group that can be catch assuming an infinite fishing effort).
#' @param smooth_type is a parameter of the create.smooth function. It defines the type of sigma calculation for the lognormal distribution. The value for this parameter is 1, 2 or 3. By default smooth_type=1, this defines a constant sigma. By choosing smooth_type=2, the user has the possibility to put a sigmaLN=smooth_param*ln(TL-0.05), with smooth_param=0.07 and shift=0.95 by default. Smooth_type=3 corresponds to the use of the calculated Omnivory Index (OI) divided by the associated mean TL as sigmaLN.
#' @param sigmaLN_cst is a parameter of the create.smooth function. It defines the value of the constant sigma of the lognormal distribution for smooth_type=1. By default, sigmaLN_cst=0.12.
#' @param pas is a parameter of the create.smooth function. It defines the splitting of the TL classes.
#' @param shift is a parameter of the create.smooth function. It defines the beginning of the smooth function and allows the substraction of 0.05 in the sigma calculation accounting for the half interval range of the trophic class.
#' @param smooth_param is a parameter of the create.smooth function. It defines the slope of the log-linearly increase of the TL variability with the mean trophic level of the group for smooth_type=2. SigmaLN(TL) is thus defined as sigmaLN(TL)=smooth_param*ln(TL-0.05).
#' @return This function returns a list containing: the ET-Main table, intermediate matrices (biomass, accessible biomass, flowP...) and a list of matrices corresponding to the different fisheries catches.
#' @seealso plot.ETmain to create the principle graphics resulting from the create.ETmain function, create.smooth to create the Smooth table used in this function, Transpose to convert data referring to groups into data referring to TL classes.
#' @examples
#' data(ecopath_guinee)
#'create.ETmain(ecopath_guinee)
#'#Use of the second smooth type
#'create.ETmain(ecopath_guinee,smooth_type=2)
#' @export

create.ETmain <- function (ecopath, smooth_type = NULL, sigmaLN_cst = NULL, pas = NULL, shift = NULL, smooth_param = NULL)
{
B <- apply(biomass <- Transpose(tab_smooth <- create.smooth(tab_input = ecopath, smooth_type, sigmaLN_cst, pas, shift, smooth_param), ecopath, "biomass"), 1, sum)
B_acc <- apply(biomass_acc <- sweep(biomass, 2, ecopath$accessibility, FUN = "*"), 1, sum)
P <- apply(flowP <- sweep(biomass, 2, ecopath$prod, FUN = "*"), 1, sum)
P_acc <- apply(flowP_acc <- sweep(flowP, 2, ecopath$accessibility, FUN = "*"), 1, sum)
Kin <- P/B
Kin_acc <- P_acc/B_acc

Y <- list()
somme_pecheries <- biomass
somme_pecheries[] <- 0
for (pecheries in colnames(ecopath)[grep("catch", colnames(ecopath))])
{
Y[[paste(pecheries)]] <- Transpose(tab_smooth, ecopath, pecheries)
somme_pecheries <- somme_pecheries + Y[[paste(pecheries)]]
}
Y_tot <- apply(somme_pecheries, 1, sum)

F_loss <- Y_tot/P
F_loss_acc <- Y_tot/P_acc
V <- as.numeric(rownames(biomass))[-1] - as.numeric(rownames(biomass))[-nrow(biomass)]
N_loss <- c(log(P[-length(P)]/P[-1])/V - F_loss[-length(F_loss)], NA)
N_loss[1]=log(P[1]/P[2]*V[2])-F_loss[1]

Fish_mort <- Y_tot/B
Fish_mort_acc <- Fish_mort/(B_acc/B)
Selec <- B_acc/B
Time <- cumsum(c(0, V/Kin[-length(Kin)]))
N_loss_acc <- c(log(P_acc[-length(P_acc)]/P_acc[-1])/V - F_loss_acc[-length(F_loss_acc)], NA)
N_loss_acc[1]=log(P_acc[1]/P_acc[2]*V[2])-F_loss_acc[1]
ET_Main <- cbind(B, B_acc, P, P_acc, Kin, Kin_acc, Y_tot, F_loss, F_loss_acc, N_loss, Fish_mort, Fish_mort_acc, Selec, Time, N_loss_acc)
retour<-list(ET_Main = as.data.frame(ET_Main), biomass = biomass, biomass_acc = biomass_acc, prod = flowP, prod_acc = flowP_acc, tab_smooth = tab_smooth,Y=Y)

class(retour)<-"ETmain"
return(retour)
}
