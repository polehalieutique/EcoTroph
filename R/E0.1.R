#'  E0.1
#' @param TL Trophic level
#' @param Y Catches
#' @param Fish_mort Fishing mortality
#' @export

E0.1<-function(TL,Y,Fish_mort){
  y=Y[as.character(TL),];ff=Fish_mort[as.character(TL),]
  if(length(y[!y==0])==0){f.01=c(NA,NA)}else{
    #pente.origine=(y[2]-y[1])/(ff[2]-ff[1])
    pente=(y[2:length(y)]-y[1:(length(y)-1)])/(ff[2:length(y)]-ff[1:(length(ff)-1)])
    names(pente)=names(y[1:(length(y)-1)])
    dif=abs(pente-0.1*pente[1])# pente[1]: pente ? l'origine
    n=names(dif[dif==min(dif)]) ; if(length(n)>1){n=n[1]}
    f.01=c(ff[names(ff)==as.numeric(n)],n)}
  return(as.numeric(f.01))
}
