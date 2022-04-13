
a13.eq=function(compteur,ET_Main,biom.mf,fish.m,TopD,FormD,range.TLpred){
  kin=(ET_Main$Kin[compteur] - ET_Main$Fish_mort[compteur]) * (1 + TopD[compteur] * (sum(biom.mf[(compteur + range.TLpred[1]):(compteur + range.TLpred[2])])^(FormD[compteur]) - sum(ET_Main$B[(compteur+ range.TLpred[1]):(compteur + range.TLpred[2])])^(FormD[compteur]))/(sum(ET_Main$B[(compteur + range.TLpred[1]):(compteur + range.TLpred[2])])^(FormD[compteur]))) +  fish.m[compteur]
  return(kin)
}
