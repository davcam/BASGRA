params_BC_ModePrior     <- parmod_BC
params_BC_MAP           <- scparMAP_BC                               * sc
params_BC_MaxL          <- scparMaxL_BC                              * sc
params_BC_MeanPosterior <-  colMeans(pChain)                         * sc
params_BC_VarPosterior  <- (colMeans(pChain^2) - colMeans(pChain)^2) * sc^2
params_BC_CVPosterior   <- sqrt(params_BC_VarPosterior) / abs(params_BC_MeanPosterior)
params_BC_MinPosterior  <- apply(pChain,2,min)                       * sc 
params_BC_MaxPosterior  <- apply(pChain,2,max)                       * sc 

df_parModes <- data.frame( row.names(df_params) )
for (s in 1:nSites) {
  params               <- list_params[[s]]
  params_ModePrior     <- params
  params_MAP           <- params
  params_MaxL          <- params
  params_MeanPosterior <- params
  params_VarPosterior  <- rep( 0, length(params) )
  params_CVPosterior   <- rep( 0, length(params) )
  params_MinPosterior  <- params
  params_MaxPosterior  <- params
  ip_BC_s                       <- ip_BC_site      [[s]]
  icol_pChain_s                 <- icol_pChain_site[[s]]
  params_ModePrior    [ip_BC_s] <- params_BC_ModePrior    [icol_pChain_s]
  params_MAP          [ip_BC_s] <- params_BC_MAP          [icol_pChain_s]
  params_MaxL         [ip_BC_s] <- params_BC_MaxL         [icol_pChain_s]
  params_MeanPosterior[ip_BC_s] <- params_BC_MeanPosterior[icol_pChain_s]
  params_VarPosterior [ip_BC_s] <- params_BC_VarPosterior [icol_pChain_s]
  params_CVPosterior  [ip_BC_s] <- params_BC_CVPosterior  [icol_pChain_s]
  params_MinPosterior [ip_BC_s] <- params_BC_MinPosterior [icol_pChain_s]
  params_MaxPosterior [ip_BC_s] <- params_BC_MaxPosterior [icol_pChain_s]
  nsign <- 4
  p1 <- data.frame(signif(params_ModePrior    ,nsign)); colnames(p1) = paste("ModePrior_",s,sep="")
  p2 <- data.frame(signif(params_MAP          ,nsign)); colnames(p2) = paste("MAP_"      ,s,sep="")
  p3 <- data.frame(signif(params_MaxL         ,nsign)); colnames(p3) = paste("MaxL_"     ,s,sep="")
  p4 <- data.frame(signif(params_MeanPosterior,nsign)); colnames(p4) = paste("MeanPost_" ,s,sep="")
  p5 <- data.frame(signif(params_VarPosterior ,nsign)); colnames(p5) = paste("VarPost_"  ,s,sep="")
  p6 <- data.frame(signif(params_CVPosterior  ,nsign)); colnames(p6) = paste("CVPost_"   ,s,sep="")
  p7 <- data.frame(signif(params_MinPosterior ,nsign)); colnames(p7) = paste("MinPost_"  ,s,sep="")
  p8 <- data.frame(signif(params_MaxPosterior ,nsign)); colnames(p8) = paste("MaxPost_"  ,s,sep="")
  # Update dataframe
  df_parModes <- cbind( df_parModes, p1, p2, p3, p4, p5, p6, p7, p8 ) 
}
  
# Write the dataframe to txt-file
write.table( df_parModes,
			 paste('BASGRA_parModes',format(Sys.time(),"_%H_%M.txt"),sep=""),
			 sep="\t", row.names=F )
