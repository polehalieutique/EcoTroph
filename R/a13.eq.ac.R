a13.eq.ac=function(compteur,ET_Main,biom.mf,fish.m.ac,TopD,FormD,range.TLpred){
  kin.ac=(ET_Main[compteur, "Kin_acc"] - ET_Main[compteur, "Fish_mort_acc"]) * (1 + TopD[compteur] * (sum(biom.mf[(compteur + range.TLpred[1]):(compteur + range.TLpred[2])])^(FormD[compteur]) - sum(ET_Main[(compteur+ range.TLpred[1]):(compteur + range.TLpred[2]), "B"])^(FormD[compteur]))/(sum(ET_Main[(compteur + range.TLpred[1]):(compteur + range.TLpred[2]), "B"])^(FormD[compteur]))) +  fish.m.ac[compteur]
  return(kin.ac)
}
