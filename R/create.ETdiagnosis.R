#'  ET-Transpose provides a picture of an ecosystem under a given fishing mortality. ET-Diagnosis is a routine simulating how this baseline ecosystem would be impacted by increasing or decreasing fishing effort. Fishing effort can be modified per fleet and/or trophic group. Ecosystem-wide effects of altering fishing effort include potential changes in biomass, accessible biomass, production, kinetics and catch trophic spectra, as well as impacts on the mean trophic level of the catch and biomass. Additionally, ET-Diagnosis constitutes a useful exploratory tool for ecosystem-based management. It simulates how reducing or increasing fishing effort and/or preferentially targeting different trophic levels could improve yield at the ecosystem scale. Lastly, ET-Diagnosis allows to view how different assumptions on ecosystem functioning (biomass input control, top-down effect) affect both trophic level specific and ecosystem-wide properties in relation to fishing.
#' @usage create.ETdiagnosis(data, Mul_eff = NULL, Group = NULL, fleet.of.interest = NULL,
#' @usage same.mE = NULL, B.Input=NULL, Beta = NULL, TopD = NULL, FormD = NULL, TLpred = NULL)
#' @param data is the list object returned by the create.ETmain function.
#' @param Mul_eff is a vector of fishing effort multipliers that the user wants to test. Mul_eff must contain the value 1 (reference state). By default, the function simulates a range of fishing effort multipliers from 0 to 5 for each fleet.
#' @param Group is a character vector of trophic groups that the user specifically wants to impact by changing associated fishing efforts. By default, all trophic groups are equally impacted.
#' @param fleet.of.interest is a character vector of fleet(s) that the user specifically wants to impact by changing associated fishing efforts (default =NULL). This argument is of particular interest if there are more than two fleets because it limits the mE combinations to be tested, and thus the associated computation time.
#' @param same.mE is a logical argument (default=F), if TRUE the same effort multipliers are simultaneously applied to all fleets.
#' @param B.Input is a logical argument (default=F), if TRUE the "Biomass input control" equation is accounted for in EcoTroph equations.
#' @param Beta is a coefficient expressing the extent of the biomass input control. Beta=0 refers to an ecosystem where all secondary production originates from grazing on primary producers, and Beta=1 to an ecosystem where detritus and/or recruitment contribute to a major part of the biomass input (default=0.2).
#' @param TopD is a coefficient expressing the top-down control, i.e. the fraction of the natural mortality depending on predator abundance. It varies between 0 and 1. The user can specify a numeric value, which is applied to each trophic level, or a numeric vector (of the same length as TL classes), i.e. a value for each TL (default=0.4).
#' @param FormD is a shape parameter varying between 0 and 1. It defines the functional relationship between prey and predators. The value 1 refers to a situation where predators abundance has a linear effect on the speed of the flow of their preys. The user can specify a numeric value, which is applied to each trophic level, or a numeric vector (of the same length as TL classes), i.e. a value for each TL (default=0.5).
#' @param TLpred is the trophic level that the user considers to be the "predator" trophic classes start. The default value is 3.5.
#' @return This function returns a list of elements referring to each simulated combination of fishing effort multipliers. Each element is a list of two types of results:
#' @return -	Variables characterizing the state and functioning of the modeled ecosystem: biomass, flow, kinetic, catches (total and per fleet) and fishing mortality per trophic level.
#' @return -	Summary statistics (contained in the ET_Main_diagnose): absolute and relative (in comparison with the reference state) total biomass, flow, catches.
#' @details Fleets' names used in the argument 'fleet.of.interest' are the catch column names of the Ecopath input data.frame (e.g. 'catch.1' or 'catch.ind').
#' @seealso plot.ETdiagnosis and plot.ETdiagnosis_isopleth to plot the principle graphics resulting from the create.ETdiagnosis function, create.ETmain to create a list of tables used as input in the create.ETdiagnosis function.
#' @examples
#' data(ecopath_guinee)
#' #Impacts of global changes in fishing efforts multipliers (in the range 0-5)
#' create.ETdiagnosis(create.ETmain(ecopath_guinee),same.mE=TRUE)
#' #Test of all the combinations of fishing effort multipliers per fleet
#' #(in the range 0-5)
#' \donttest{create.ETdiagnosis(create.ETmain(ecopath_guinee))}
#' #With biomass input control
#' \donttest{create.ETdiagnosis(create.ETmain(ecopath_guinee),B.Input=TRUE)}
#' #Impacts of changing fishing effort against Barracudas+ and Carangids groups
#' \donttest{
#' create.ETdiagnosis(create.ETmain(ecopath_guinee),
#' Mul_eff=(seq(0,5,.1)),Group=c('Barracudas+','Carangids'))
#' }
#'
#' @export

create.ETdiagnosis <- function(data, Mul_eff = NULL, Group = NULL, fleet.of.interest = NULL, same.mE = NULL, B.Input=NULL,
                      Beta = NULL, TopD = NULL, FormD = NULL, TLpred = NULL)
{

  ET_Main=data$ET_Main

  TL_out <- as.numeric(rownames(ET_Main))
  names(TL_out)=1:length(TL_out)
  n.TL=length(TL_out)

  #Initialization

  #Calculates the number of fleets
  fleet=names(data$Y) ; n.fleet=length(fleet)

  if (is.null(same.mE)){same.mE <- FALSE}
  if(!is.null(fleet.of.interest)){same.mE <- FALSE}
  if (is.null(Mul_eff)){Mul_eff.=list()
    for(i in 1:n.fleet){Mul_eff.[[i]]=c(0, 0.2, 0.4, 0.7, 1, 1.5, 2, 2.5, 3, 4, 5)}
    #names(Mul_eff)=paste('mF',1:n.fleet,sep='')
    if(same.mE){Mul_eff.=c(0, 0.2, 0.4, 0.7, 1, 1.5, 2, 2.5, 3, 4, 5)}
  }else{Mul_eff.=list()
      for(i in 1:n.fleet){Mul_eff.[[i]]=Mul_eff}
      #names(Mul_eff)=paste('mF',1:n.fleet,sep='')
      if(same.mE){Mul_eff.=Mul_eff}
  }
  if (is.null(B.Input)){B.Input <- FALSE}
  if (is.null(Beta)){Beta <- .2}
  if (is.null(TopD)){TopD <- rep(.4,n.TL)}else{if(length(TopD)==1){TopD=rep(TopD,n.TL)}}
  if (is.null(FormD)){FormD <- rep(.5,n.TL)}else{if(length(FormD)==1){FormD=rep(FormD,n.TL)}}
  if (is.null(TLpred)){TLpred <- 3.5}

  #Computes reference (initial state) fishing mortality coefficients per fleet and per TL classes
  Fish_mort_ref=list()
  Fish_mort_acc_ref=list()
  for(i in 1:n.fleet){
    Fish_mort_ref[[fleet[i]]]=apply(data[['Y']][[fleet[i]]],1,sum)/ET_Main$B
    Fish_mort_acc_ref[[fleet[i]]]=apply(data[['Y']][[fleet[i]]],1,sum)/ET_Main$B_acc
  }
  for(i in 1:n.fleet){Fish_mort_acc_ref[[i]][is.nan(Fish_mort_acc_ref[[i]])]=0}

  #Computes reference (initial state) fishing mortality coefficients per fleet,per TL classes and groups
  if(!is.null(Group)){
    Fish_mort_gp_ref=list()
    Fish_mort_acc_gp_ref=list()
    for(i in 1:n.fleet){
      # Gasche et Gascuel (2013) (7) Fg,i,t=Yg,i,t/Bt
      Fish_mort_gp_ref[[fleet[i]]]=data$Y[[fleet[i]]]/ET_Main$B
      Fish_mort_gp_ref[[fleet[i]]][is.nan(Fish_mort_gp_ref[[fleet[i]]])]=0
      Fish_mort_acc_gp_ref[[fleet[i]]]=data$Y[[fleet[i]]]/ET_Main$B_acc
      Fish_mort_acc_gp_ref[[fleet[i]]][is.nan(Fish_mort_acc_gp_ref[[fleet[i]]])]=0
    }
  }

#Creates a list with for each combination of effort multipliers containing all variables (F,Flow,Biom,Kin)

if(!same.mE){
  ff=expand.grid(Mul_eff.)
  if(is.null(fleet.of.interest)){
    for(n in 1:n.fleet){
      if(n==1){FF=ff[,n]}else{FF=paste(FF,'_',ff[,n],sep='')}}
  }else{
    colnames(ff)=c('interest','other')
      for(n in 1:n.fleet){
        if(fleet[n]%in%fleet.of.interest){ff.=ff[,'interest']}else{ff.=ff[,'other']}
        if(n==1){FF=ff.}else{FF=paste(FF,'_',ff.,sep='')}}
  }
}else{
  for(n in 1:n.fleet){
    if(n==1){FF=Mul_eff.}else{FF=paste(FF,'_',Mul_eff.,sep='')}}
}
 FF=unique(FF)
 comb=as.list(FF)
 names(comb)=FF

  for(i in 1:length(comb)){

    comb[[i]]=list()
    #
    comb[[i]][['mf']]=as.numeric(unlist(strsplit(names(comb)[i],'_')))
    names(comb[[i]][['mf']])=paste('mf',1:n.fleet,sep='')
    # F & F_acc
    mf=as.numeric(comb[[i]][['mf']])
    if(is.null(Group)){
      for(j in 1:n.fleet){
        ff=mf[j]*Fish_mort_ref[[j]]
        if(j==1){ff.=ff}else{ff.=ff.+ff}
      }
    }else{
      for(j in 1:n.fleet){
        ff=Fish_mort_gp_ref[[fleet[j]]]
        if(j==1){f.=ff}else{f.=f.+ff}
      }
      for(g in Group){
        for(j in 1:n.fleet){
          ff=mf[j]*Fish_mort_gp_ref[[fleet[j]]][,g]
          if(j==1){f..=ff}else{f..=f..+ff}
        }
        f.[,g]=f..
      }
      ff.=apply(f.,1,sum)
    }

    comb[[i]][['Fish_mort']]=ff.
    comb[[i]][['Fish_mort_acc']]=ff./ET_Main$Selec

    # Kin & Kin_acc
    comb[[i]][['TEMP_Kin']]=ET_Main[,'Kin']-ET_Main[,'Fish_mort']+comb[[i]][['Fish_mort']]
    comb[[i]][['TEMP_Kin_acc']]=ET_Main[,'Kin_acc']-ET_Main[,'Fish_mort_acc']+comb[[i]][['Fish_mort_acc']]
    comb[[i]][['Kin_MF']]=comb[[i]][['TEMP_Kin']]
    comb[[i]][['Kin_MF_acc']]=comb[[i]][['TEMP_Kin_acc']]

    # FLOW_MF & FLOW_MF_acc
    comb[[i]][['Prod_MF']]=ET_Main[,'P']
    comb[[i]][['Prod_MF_acc']]=ET_Main[,'P_acc']

    # BIOM_MF & BIOM_MF_acc
    comb[[i]][['BIOM_MF']]=ET_Main[,'B']
    comb[[i]][['BIOM_MF_acc']]=ET_Main[,'B_acc']
  }

  # Other arguments of mf.diagnosis
  tll=names(TL_out[TL_out>=2.8 & TL_out<=3.3])
  range.TLpred=as.numeric(c(tll[1],tll[length(tll)]))-2
  high.tl=abs(TL_out-5.6)
  lim.high.TL=as.numeric(names(high.tl[high.tl==min(high.tl)[1]]))
  #range.highTL=abs(as.numeric(names(TL_out[TL_out %in%
  #range(TL_out[TL_out>=(TL_out[lim.high.TL]-.9) & TL_out<=round(TL_out[lim.high.TL]-.2,1)])]))-lim.high.TL)
  tlll=names(TL_out[TL_out>=(TL_out[lim.high.TL]-0.5) & TL_out<=(TL_out[lim.high.TL])])
  range.highTL=as.numeric(c(tlll[1],tlll[length(tlll)]))

  # Computation runned on each list element
  diagn.list=lapply(comb,mf.diagnosis,ET_Main,data$Y,TL_out,fleet,n.fleet,Fish_mort_ref,Fish_mort_acc_ref,B.Input,
                    Beta,TopD,FormD,TLpred,n.TL,range.TLpred,lim.high.TL,range.highTL)
  # mf.diagnosis(comb[[10]],ET_Main,TL_out,fleet,n.fleet,Fish_mort_ref,Fish_mort_acc_ref,Beta,TopD,FormD,TLpred)
  names(diagn.list)=names(comb)
  diagn.list[['fleet.of.interest']]=fleet.of.interest
  class(diagn.list)<-"ETdiagnosis"
  return(diagn.list)
}
