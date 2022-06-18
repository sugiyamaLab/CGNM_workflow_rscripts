## This is a minimum example for CGNM where we do not use any ODE solver.
## Make sure this runs and also the plots get produced.  If not revisit installation of CGNM and ggplot.

library(CGNM)
library(ggplot2)

##lip-flop kinetics (an example known to have two distinct solutions)

model_analytic_function=function(x){

  observation_time=c(0.1,0.2,0.4,0.6,1,2,3,6,12)
  Dose=1000
  F=1

  ka=10^x[1]
  V1=10^x[2]
  CL_2=10^x[3]
  t=observation_time

  Cp=ka*F*Dose/(V1*(ka-CL_2/V1))*(exp(-CL_2/V1*t)-exp(-ka*t))

  log10(Cp)
}

observation_time=c(0.1,0.2,0.4,0.6,1,2,3,6,12)
observation=log10(c(4.91, 8.65, 12.4, 18.7, 24.3, 24.5, 18.4, 4.66, 0.238))

CGNM_result=Cluster_Gauss_Newton_method(
  nonlinearFunction=model_analytic_function,
  targetVector = observation, initial_lowerRange = c(-2,-2,-2), initial_upperRange =  c(2,2,2))

ParameterNames=c("log10(ka)", "V1", "CL_2")
ParameterDefinition=c("x1", "10^x2", "10^x3")

plot_paraDistribution_byViolinPlots(CGNM_result, ParameterNames = ParameterNames, ReparameterizationDef = ParameterDefinition)
plot_paraDistribution_byHistogram(CGNM_result, ParameterNames = ParameterNames, ReparameterizationDef = ParameterDefinition)

plot_goodnessOfFit(CGNM_result,independentVariableVector = observation_time)+ylab("Concentration")+xlab("Time")

