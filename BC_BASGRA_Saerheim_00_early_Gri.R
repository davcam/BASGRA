## 1. INITIALISE MODEL ##
   source('initialise_BASGRA_Saerheim_00_early_Gri.R')
   
## 2. INITIALISE MCMC ##
   source('BC/BC_BASGRA_MCMC_init_Saerheim_00_early_Gri.R')

## 3. RUNNING THE MCMC ##
   source('BC/BC_BASGRA_MCMC.R')

## 4. PRINTING AND PLOTTING ##
   source('BC/BC_export_parModes_extended.R')
   source('BC/BC_plot_parameters_traceplots.R')
   source('BC/BC_plot_parameters_priorbeta_histograms.R')
   source('BC/BC_plot_outputs_data.R')