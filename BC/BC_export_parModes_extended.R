# Find the positions in the default parameter list of the calibrated parameters
iBC <- match( dataframe_parameters_BC[[1]], row.names(dataframe_parameters) )

# Extend the prior mode parameter vector with default values for non-calibrated parameters
parModePrior_extended       <- parameters_default
parModePrior_extended[iBC]  <- parmod

# Extend the parMAP parameter vector with default values for non-calibrated parameters
parMAP_extended             <- parameters_default
parMAP_extended[iBC]        <- parMAP * abs(parmod)

# Extend the parMaxL parameter vector with default values for non-calibrated parameters
parMaxL_extended            <- parameters_default
parMaxL_extended[iBC]       <- parMaxL * abs(parmod)

# ADDITIONAL OUTPUT (FROM AUG 2013)
  p                            <- colMeans(pChain)
  p2                           <- colMeans(pChain^2)
parMeanPosterior               <-  p         * abs(parmod)
parVarPosterior                <- (p2 - p^2) * abs(parmod)^2
parCVPosterior                 <- sqrt(parVarPosterior) / abs(parMeanPosterior)
parMinPosterior                <- apply(pChain,2,min) * abs(parmod) 
parMaxPosterior                <- apply(pChain,2,max) * abs(parmod) 

parMeanPosterior_extended      <- rep(NA,length(parameters_default))
parVarPosterior_extended       <- rep(NA,length(parameters_default))
parCVPosterior_extended        <- rep(NA,length(parameters_default))
parMinPosterior_extended       <- rep(NA,length(parameters_default))
parMaxPosterior_extended       <- rep(NA,length(parameters_default))

parMeanPosterior_extended[iBC] <- parMeanPosterior
parVarPosterior_extended[iBC]  <- parVarPosterior
parCVPosterior_extended[iBC]   <- parCVPosterior
parMinPosterior_extended[iBC]  <- parMinPosterior
parMaxPosterior_extended[iBC]  <- parMaxPosterior

# Make a dataframe with the parameter names in column 1, and the values of the
# prior and posterior modes and the maximum likelihood in columns 2, 3 and 4.
# Additional columns: posterior mean, variance, CV, min, max.
dataframe_parModes_extended <- data.frame(row.names(dataframe_parameters),
                                          parModePrior_extended,
                                          parMAP_extended,
                                          parMaxL_extended,
                                          parMeanPosterior_extended,
                                          parVarPosterior_extended,
                                          parCVPosterior_extended,
                                          parMinPosterior_extended,
                                          parMaxPosterior_extended)

# Write the dataframe to txt-file so values can be copied into 'parameters_default.txt' if desired.
write.table(dataframe_parModes_extended,"BASGRA_parModes_extended.txt",sep="\t",row.names=F)