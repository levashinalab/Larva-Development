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

#### 3. Density-dependent development: exponential functional response----
f3<-eqnvec(
  L = "- delta_L*L - gamma*L*(exp(-log(2)*L/h))",
  P = "gamma*L*(1-(L/(L+h)))",
  gamma = 0
)

#### 4. Simple density-dependent death----
f4<-eqnvec(
  L = "- delta_L*L*(1-(L/(L+h))) - gamma*L",
  P = "gamma*L",
  gamma = 0
)
