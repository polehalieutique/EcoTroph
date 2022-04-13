#' @title Catch input for CTSA
#' @description   CTSA.catch.input is used to create inputs for the CTSA.forward function. It is a list of data.frames referring to catches per fleet formatted with TL classes in rows and trophic groups in columns.
#' @usage CTSA.catch.input(catch.group,smooth_type=NULL,sigmaLN_cst=NULL,
#' @usage pas=NULL,shift=NULL,smooth_param=NULL)
#' @param catch.group is a data.frame containing: a column group_name, column(s) referring to the catches of each fleet (named 'catch.1', 'catch.2'...), a column TL specifying the mean TL of each group, and optionally a column OI (omnivory index) used for smooth_type=3.
#' @param smooth_type is a parameter of the create.smooth function. It defines the type of sigma calculation for the lognormal distribution. The value for this parameter is 1, 2 or 3. By default smooth_type=1, this defines a constant sigma. By choosing smooth_type=2, the user has the possibility to put a sigmaLN=smooth_param*ln(TL-0.05), with smooth_param=0.07 and shift=0.95 by default. Smooth_type=3 corresponds to the use of the calculated Omnivory Index (OI) divided by the associated mean TL as sigmaLN.
#' @param sigmaLN_cst is a parameter of the create.smooth function. It defines the value of the constant sigma of the lognormal distribution for smooth_type=1. By default, sigmaLN_cst=0.12.
#' @param pas is a parameter of the create.smooth function. It defines the splitting of the TL classes.
#' @param shift is a parameter of the create.smooth function. It defines the beginning of the smooth function and allows the substraction of 0.05 in the sigma calculation accounting for the half interval range of the trophic class.
#' @param smooth_param is a parameter of the create.smooth function. It defines the slope of the log-linearly increase of the TL variability with the mean trophic level of the group for smooth_type=2. SigmaLN(TL) is thus defined as sigmaLN(TL)=smooth_param*ln(TL-0.05).
#' @seealso create.smooth, Transpose and CTSA.forward.
#' @return CTSA.catch.input returns a list of data.frames, referring to catches per fleet formatted with TL classes in rows and trophic groups in columns.
#' @examples
#' data(ecopath_guinee)
#' catch.group=ecopath_guinee[,c("group_name","TL","catch.1","catch.2")]
#' Y_test <- CTSA.catch.input(catch.group)
#' Y_test
#' @export
#'
CTSA.catch.input=function(catch.group,smooth_type=NULL,sigmaLN_cst=NULL,pas=NULL,shift=NULL,smooth_param=NULL){
  nm=colnames(catch.group)
  fleet=nm[substring(nm,1,6)=='catch.'];n.fleet=length(fleet)
  catch=list()
  for(i in 1:n.fleet){catch[[fleet[i]]]=Transpose(create.smooth(catch.group,smooth_type=smooth_type,sigmaLN_cst=sigmaLN_cst,pas=pas,shift=shift,smooth_param=smooth_param),catch.group,fleet[i])}
  return(catch)
}
