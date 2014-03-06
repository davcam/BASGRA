### 1. INITIALISE MODEL ##
   source('initialise_BASGRA_Oensingen_early.R')
   
## 2. INITIALISE MCMC ##
   source('BC/BC_BASGRA_MCMC_init_Oensingen.R')

## 3. RUNNING THE MCMC ##
   source('BC/BC_BASGRA_MCMC_AM.R')

## Calculate model output for the MAP parameter vector
#   .Fortran( 'set_params_BC', parMAP * abs(parmod) )
#   outputMAP       <- .Fortran('BASGRA', NDAYS,NOUT,y)[[3]]

#   .Fortran( 'set_params_BC', parMaxL * abs(parmod) )
#   outputMaxL       <- .Fortran('BASGRA', NDAYS,NOUT,y)[[3]]

## 4. PRINTING AND PLOTTING ##
   #source('BC/BC_export_parModes_extended.R')
   source('BC/BC_plot_parameters_traceplots.R')
   source('BC/BC_plot_parameters_priorbeta_histograms.R')
   source('BC/BC_plot_outputs_data.R')
