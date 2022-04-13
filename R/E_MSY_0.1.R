#'  E_MSY_0.1 computes two indices of exploitation: Emsy or Fmsy (maximum sustainable yield), and E0.1 or F0.1 ("start" of full exploitation) per TL class.
#' @usage E_MSY_0.1(data, Mul_eff=NULL, B.Input=NULL, Beta=NULL,TopD=NULL,
#' @usage  FormD=NULL, TLpred=NULL, maxTL=NULL)
#' @param data is the list object returned by the create.ETmain function.
#' @param Mul_eff is a parameter of the create.ETdiagnosis function. It is a vector of fishing effort multipliers that the user wants to test. Mul_eff must contain the value 1 (reference state). By default, the function simulates a range of fishing effort multipliers from 0 to 5 for each fleet.
#' @param B.Input is a parameter of the create.ETdiagnosis function. It is a logical argument (default=F), if TRUE the "Biomass input control" equation is accounted for in EcoTroph equations.
#' @param Beta is a parameter of the create.ETdiagnosis function. It is a coefficient expressing the extent of the biomass input control. Beta=0 refers to an ecosystem where all secondary production originates from grazing on primary producers, and Beta=1 to an ecosystem where detritus and/or recruitment contribute to a major part of the biomass input (default=0.2).
#' @param TopD is a parameter of the create.ETdiagnosis function. It is a coefficient expressing the top-down control, i.e. the fraction of the natural mortality depending on predator abundance. It varies between 0 and 1. The user can specify a numeric value, which is applied to each trophic level, or a numeric vector (of the same length as TL classes), i.e. a value for each TL (default=0.4).
#' @param FormD is a parameter of the create.ETdiagnosis function. It is a shape parameter varying between 0 and 1. It defines the functional relationship between prey and predators. The value 1 refers to a situation where predators abundance has a linear effect on the speed of the flow of their preys. The user can specify a numeric value, which is applied to each trophic level, or a numeric vector (of the same length as TL classes), i.e. a value for each TL (default=0.5).
#' @param TLpred is a parameter of the create.ETdiagnosis function. It is the trophic level that the user considers to be the "predator" trophic classes start. The default value is 3.5.
#' @param maxTL is a numeric string indicating the maximum TL for which indices are computed.
#' @return The E_MSY_0.1 function returns a data.frame containing Fmsy, Emsy, F0.1 and E0.1 per TL class.
#' @details For any TL class, if E0.1 and/or Emsy value(s) is(are) equal to the maximum effort multiplier tested (max(Mul_eff)), then E/F0.1 and/or E/Fmsy are set equal to NA.

#' @examples
#' data(ecopath_guinee)
#' \donttest{E_MSY_0.1(create.ETmain(ecopath_guinee))}
#' @export
E_MSY_0.1<-function(data,Mul_eff=NULL,B.Input=NULL,Beta=NULL,TopD=NULL,FormD=NULL,TLpred=NULL,maxTL=NULL){
  #data=create.ETmain(ecopath_guinee)
  #Mul_eff=NULL;B.Input=NULL;Beta=NULL;TopD=NULL;FormD=NULL;TLpred=NULL;maxTL=NULL
  n.TL=nrow(data$ET_Main)
  if (is.null(Mul_eff)){Mul_eff=seq(0,10,.1)}
  if (is.null(B.Input)){B.Input <- FALSE}
  if (is.null(Beta)){Beta <- 0.2}
  if (is.null(TopD)){TopD <- rep(.4,n.TL)}else{if(length(TopD)==1){TopD=rep(TopD,n.TL)}}
  if (is.null(FormD)){FormD <- rep(.5,n.TL)}else{if(length(FormD)==1){FormD=rep(FormD,n.TL)}}
  if (is.null(TLpred)){TLpred <- 3.5}
  if (is.null(maxTL)){maxTL=5.5}

  diagn.list=create.ETdiagnosis(data=data,Mul_eff=Mul_eff,B.Input=B.Input,Beta=Beta,TopD=TopD,FormD=FormD,TLpred=TLpred,same.mE=T)
  fleet.of.interest=diagn.list[['fleet.of.interest']]
  if(!is.null(fleet.of.interest)){diagn.list=diagn.list[-length(diagn.list)]}
  names(diagn.list)=Mul_eff

  for(i in 1:length(Mul_eff)){
   fm=diagn.list[[as.character(Mul_eff[i])]][['Fish_mort']]
   y=diagn.list[[as.character(Mul_eff[i])]][['Catches.tot']]
  if(i==1){Fish_mort=fm;Y=y}else{Fish_mort=cbind(Fish_mort,fm);Y=cbind(Y,y)}
  }
  colnames(Y)=Mul_eff;colnames(Fish_mort)=Mul_eff

  E_MSY=sapply(row.names(Y),Emsy,Y,Fish_mort)
  E_0.1=sapply(row.names(Y),E0.1,Y,Fish_mort)
  MSY_0.1=cbind(t(E_MSY),t(E_0.1))
  colnames(MSY_0.1)=c('F_MSY','E_MSY','F_0.1','E_0.1')
  MSY_0.1=as.data.frame(MSY_0.1)

  # Control, if E_MSY=max(Mul_eff)=> NA
  msy.na=rownames(MSY_0.1[MSY_0.1$E_MSY==max(Mul_eff) & !is.na(MSY_0.1$E_MSY),])
  if(!length(msy.na)==0){MSY_0.1[row.names(MSY_0.1)%in%msy.na,c(1,2)]=matrix(data=NA,ncol=2,nrow=length(msy.na))}
  # Control E_0.1
  m01.na=rownames(MSY_0.1[MSY_0.1$E_0.1==max(Mul_eff) & !is.na(MSY_0.1$E_0.1),])
  if(!length(m01.na)==0){MSY_0.1[row.names(MSY_0.1)%in%m01.na,c(1,2)]=matrix(data=NA,ncol=2,nrow=length(m01.na))}

  MSY_0.1=MSY_0.1[as.numeric(row.names(MSY_0.1))<=maxTL,]
return(MSY_0.1)}
