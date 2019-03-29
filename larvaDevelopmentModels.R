library(dMod)  
##### Definition of the models used in the project "larva Development"

#### 1. Density-dependent development: Type I functional response----
f1<-eqnvec(
  L = "- delta_L*L - gamma*L*(1-(L/(L+h)))",
  P = "gamma*L*(1-(L/(L+h)))",
  gamma = 0
)

#### 2. Density-dependent development. Type II functional response----
f2<-eqnvec(
  L = "- delta_L*L - gamma*L*(1-(L^2/(L^2+h^2)))",
  P = "gamma*L*(1-(L^2/(L+h^2)))",
  gamma = 0
)

#### 3. DMOD style Density-dependent development: exponential functional response----
f3<-eqnvec(
  L = "- delta_L*L - gamma*L*(exp(-log(2)*L/h))",
  P = "gamma*L*(exp(-log(2)*L/h))",
  gamma = 0
)

#### 4. Simple density-dependent death: Type I----
f4<-eqnvec(
  L = "- delta_L*L*(1+(L/(L+h))) - gamma*L",
  P = "gamma*L",
  gamma = 0
)

#### 5. Density-dependent death (type I) and growth (exp)----
f5<-eqnvec(
  L = "- delta_L*L*(1-(L/(L+h1))) - gamma*L*(exp(-log(2)*L/h))",
  P = "gamma*L*(exp(-log(2)*L/h))",
  gamma = 0
)

#### 6. Simple density-dependent death: exp----
f6<-eqnvec(
  L = "- delta_L*L*(1-exp(-log(2)*L/h)) - gamma*L",
  P = "gamma*L",
  gamma = 0
)


###7. "whale" like effect: minimum density required for successful growth -----
f7<-eqnvec(
  L = "-gamma*L*(L/(h+L))*(1-L/(2*K)) -delta_L*L",
  P = "gamma*L*(L/(h+L))*(1-L/(2*K))",
  gamma = 0
)

###8. "whale" like effect: minimum density required for successful growth + hill growth with 2 exponent -----
f8<-eqnvec(
  L = "-gamma*L*(L/(h+L))*(1-(L/(K+L))) -delta_L*L",
  P = "gamma*L*(L/(h+L))*(1-(L/(K+L)))",
  gamma = 0
)

## FME style: Density-dependent development: exponential functional response----
larva_growth<-function(t, state, parms) {
  with(as.list(c(state,parms)), {
    
    if(t<=7) gamma =0
    dL<- -delta_L*L - gamma*L*(exp(-log(2)*L/h))
    dP<- gamma*L*(exp(-log(2)*L/h))
    
    return(list(c(dL, dP)))  
  }) 
}
  

larva_growth_temp_density<-function(t, state, parms) {
  with(as.list(c(state,parms)), {
    
    epsilon = exp(-log(2)*L/h)
    sigma =
    if(t<=7) gamma =0
    dL<- -delta_L*L - gamma*L*(1-epsilon)*(1-sigma)
    dP<- gamma*L*(1-epsilon)*(1-sigma)
    
    return(list(c(dL, dP)))  
  }) 
}
