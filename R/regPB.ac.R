#'  function used to compute pB for the higest trophic levels and accessible biomass
#'  @param compteur
#'  @param pb.mf
#'  @param TL_out
#'  @param range.highTL
#' @export
regPB.ac=function(compteur,pb.mf.ac,TL_out,range.highTL){
  x. <- TL_out[(range.highTL[1]):(range.highTL[2])]
  y <- log(pb.mf.ac[(range.highTL[1]):(range.highTL[2])])
  #if(fast){reg <- coef(fastLm(y ~ x.))}else{
  reg <- coef(lm(y ~ x.))
  reg.ac<- exp(reg[1] + reg[2] * TL_out[compteur])
  return(reg.ac)
}
