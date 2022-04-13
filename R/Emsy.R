#'  E_MSY
#' @param TL Trophic level
#' @param Y Catches
#' @param Fish_mort Fishing mortality
#' @export
Emsy=function(TL,Y,Fish_mort){
  y=Y[as.character(TL),];ff=Fish_mort[as.character(TL),]
  x=cbind(y,ff)
  if(length(y[!y==0])==0){emsy=c(NA,NA)}else{
    emsy=c(x[x[,1]==max(y)[1],'ff'][1],names(y[y==max(y)[1]])[1])}
  return(as.numeric(emsy))
}

