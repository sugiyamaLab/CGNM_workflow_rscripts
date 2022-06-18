## Here we try model written in ODE.
## One can use CGNM with any ODE solver of users choice but here show an example using RxODE
## Make sure it produces the similar result as Tutorial 1.
## RxODE compiles the model at "model=RxODE(model_text)" stage and if the compiler is not linked properly to the RxODE it may produce an error.
## "Compile" can be thought as a process of translation from the ODE written as a text in "model_text" into the language that computer can understand easily so that the computation can be done quickly.  This process depends on the computer so compiler needs to be installed and linked properly to each individual computer.

library(CGNM)
library(ggplot2)
library(RxODE)

##lip-flop kinetics (an example known to have two distinct solutions)

model_text="
d/dt(X_1)=-ka*X_1
d/dt(C_2)=(ka*X_1-CL_2*C_2)/V1"

model=RxODE(model_text)
#define nonlinearFunction
model_function=function(x){
  
  observation_time=c(0.1,0.2,0.4,0.6,1,2,3,6,12)
  
  theta <- c(ka=10^x[1],V1=10^x[2],CL_2=10^x[3])
  ev <- eventTable()
  ev$add.dosing(dose = 1000, start.time =0)
  ev$add.sampling(observation_time)
  odeSol=model$solve(theta, ev)
  log10(odeSol[,"C_2"])
  
}


observation_time=c(0.1,0.2,0.4,0.6,1,2,3,6,12)
observation=log10(c(4.91, 8.65, 12.4, 18.7, 24.3, 24.5, 18.4, 4.66, 0.238))

CGNM_result=Cluster_Gauss_Newton_method(
  nonlinearFunction=model_function,
  targetVector = observation, initial_lowerRange = c(-2,-2,-2), initial_upperRange =  c(2,2,2))

ParameterNames=c("log10(ka)", "V1", "CL_2")
ParameterDefinition=c("x1", "10^x2", "10^x3")

plot_paraDistribution_byViolinPlots(CGNM_result, ParameterNames = ParameterNames, ReparameterizationDef = ParameterDefinition)
plot_paraDistribution_byHistogram(CGNM_result, ParameterNames = ParameterNames, ReparameterizationDef = ParameterDefinition)

plot_goodnessOfFit(CGNM_result,independentVariableVector = observation_time)+ylab("Concentration")+xlab("Time")

