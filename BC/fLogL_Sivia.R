flogL <- function(sims,data,data_s)
{ 
Ri         <- (sims - data) / data_s
i0         <- which( abs(Ri)<1.e-08 )

logLi      <- log(1-exp(-0.5*Ri^2)) - log(Ri^2) - 0.5*log(2*pi) - log(data_s)
logLi[i0]  <- -0.5*log(2*pi) - log(2*data_s[i0])

sum(logLi)
}
