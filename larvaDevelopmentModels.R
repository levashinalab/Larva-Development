##### Definition of the models used in the project "larva Development"

#### 1. Simple density-dependent growth----
larva_growth<-function(t,state,parms){
  with(as.list(c(state,parms)), {
    if(t<=tau)
      gamma = 0
    dL <- -delta*L -gamma*L*(1-(L/(L+h)))
    dP<- gamma*L*(1-(L/(L+h)))
    return(list(c(dL,dP)))
  })
}
larva_growth_delay<-function(t,state,parms){
  with(as.list(c(state,parms)), {

    if(t<=tau)
      gamma = 0
    dL <- -delta*L*(1+(L/(L+h2))) -gamma*L*(1-(L/(L+h)))
    dP<- gamma*L*(1-(L/(L+h)))
    return(list(c(dL,dP)))
  })
}