flogL <- function(sims,data,data_s)
{ 
logLi <- (-1.*0.5)*((sims-data)/data_s)^2 - 0.5*log(2*pi) - log(data_s)

sum(logLi)
}
