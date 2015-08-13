## 1. INITIALISE MCMC ##
   source('BC/BC_BASGRA_MCMC_init_Gri_SAERHEIM_00_0102.R')

## 2. RUNNING THE MCMC ##
   source('BC/BC_BASGRA_MCMC.R')

## 3. PRINTING AND PLOTTING ##
   source('BC/BC_export_parModes.R')
#   source('BC/BC_plot_parameters_traceplots.R')
   source('BC/BC_plot_parameters_priorbeta_histograms.R')
   source('BC/BC_plot_outputs_data.R')

## 4. SAVING WORKSPACE
#    imagefilename <- paste( "BC_Gri_SAERHEIM_00_0102",
#                            format(Sys.time(),"%H_%M.Rdata"), sep="" )
#    save.image(file=imagefilename)
