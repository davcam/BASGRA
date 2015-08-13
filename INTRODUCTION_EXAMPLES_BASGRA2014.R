################################################################################
### 0. Introduction
################################################################################
# This file shows examples of working with BASGRA (v. 2014-09-19).

# In comparison with the 2012 version of the model, much has changed.
# The model is still operated from R, and the BASGRA-code itself is still in
# FORTRAN, but some specific tasks have shifted to R. There are no longer
# FORTRAN subroutines for setting parameter values and weather conditions.
# Parameters, weather and harvesting days are now defined in R and then
# they are given as input to the main model subroutine called 'BASGRA',
# which we find in file 'BASGRA.f90'.

# The subdirectory tree-structure containing all files has changed too. 

# The new set-up has allowed us to write several R-functions that make
# working with the model easier. The most important function is 'run_model()',
# which is the R-function that runs BASGRA itself. Examples are given in
# section 1. below. But there are many other routines (for plotting, making
# tables, sensitivity analysis, checking weather data etc.) that are explained in
# this file as well.

# One important thing to realise is that at the beginning of an R-session,
# R begins with a clean slate, i.e. it does not know the functions we defined.
# So we always have to begin by running a so-called 'initialisation file'
# which will make the functions known to R. We have made site-specific
# initialisation files, with names like
# 'initialise_BASGRA_Saerheim_00_early_Gri.R', which can be used for that
# purpose. Each of those site-specific initialisation files includes a call
# to the R-script 'initialisation_general.R' where the functions are defined.

# The site-specific initialisation files are also the place where we set the
# parameter values, weather data and harvest dates for the runs that we are
# going to do.

# So, the standard way of working with BASGRA involves at least two steps:
#   1. run an initialisation file,
#   2. use R-functions to run the model and do analyses.

# The rest of this file gives examples of working with this set-up.

################################################################################
### 1. Initialise and run the model for a site
################################################################################

# We initialise the model for Saerheim 2000, early cutting treatment
source("initialisation/initialise_BASGRA_Saerheim_00_early_Gri.R")
# This will define the four inputs to the model: (1) parameter values,
# (2) weather data, (3) harvesting calendar, (4) length of simulation period.
# These inputs will be known to R as: 'params', 'matrix_weather',
# 'days_harvest' and 'NDAYS'.

# We run BASGRA by calling the function 'run_model'.
# We run the model with the above four inputs, and put the results in "output":
output <- run_model( p=params, w=matrix_weather, h=days_harvest, n=NDAYS )

# If we do not change the order of the inputs, the "p=" etc. are not needed:
output <- run_model( params, matrix_weather, days_harvest, NDAYS )

# By default, the 'run_model' function expects arguments called 'params',
# 'matrix_weather', 'days_harvest' and 'NDAYS', so in this case we could
# have left those out too, and still got the same output!
output <- run_model()

# We can show the output of BASGRA on screen:
plot_output(output)

# By default, the 'plot_output' function expects its first argument to
# be called 'output' so we could have shortened the last command:
plot_output()

# We can also export the output to a pdf-file with graphs as follows:
pdf( "output.pdf" )
plot_output()
dev.off()

# We can also request a txt-file with some tabulated results:
table_output()

# If we only want tabulated output for specific variables, we can give:
table_output( vars=c("DM","TILTOT"),
              file_table="example_output_DM_TILTOT.txt" )

# If we want both plot to screen and table to file, we can use a shorter command
export_output( )

################################################################################
### 2. Comparing the output from multiple runs
################################################################################
# If you run the model multiple times, e.g. with different parameter values or
# harvest times, then the outputs from the different runs can easily be compared
# with each other.

source("initialisation/initialise_BASGRA_Saerheim_00_early_Gri.R")
output_early <- run_model()
source("initialisation/initialise_BASGRA_Saerheim_00_late_Gri.R")
output_late  <- run_model()
plot_output( list(output_early,output_late) )

# The example shows that if you put the outputs from different runs in one
# list, then the 'plot-output()' function knows how to plot the results in one
# graph.

# For changing parameters, the sensitivity analysis function 'SA()' is
# usually more convenient, see the next section.

################################################################################
### 3. Sensitivity analysis
################################################################################
source("initialisation/initialise_BASGRA_general.R")

# If we want to see the impact of changing a single parameter, we can
# use the 'SA' function. We tell SA by how much the parameter should be
# multiplied in the different runs. For example:
SA( "HAGERE", c(1,0.5,0.25) )
# The results of this sensitivity analysis are written to two files,
# one with plots (called 'SA_[].pdf'), and one with a table ('SA_[].txt').

# Let's use a different parameter:
SA( "TILTOTI", c(0.5,1,2) )

# By default, the 'SA' function varies parameter "TILTOTI", and by default
# it also uses multiplication values 0.5, 1 and 2. So we get the same with:
SA()

# A full specification of the 'SA()' function, using all the defaults (so it
# gives the same results as above), is as follows:
pmult     <- 2^(-1:1)
vars      <- outputNames[-(1:3)]
nrow_plot <- ceiling( sqrt((length(vars)+1) * 8/11) )
SA( parname_SA = "TILTOTI",
    pmult      = pmult,
    vars       = vars,
    leg_title  = parname_SA,
    nrow_plot  = nrow_plot,
    ncol_plot  = ceiling( (length(vars)+1)/nrow_plot ),
    lty        = rep(1,length(pmult)),
    lwd        = rep(3,length(pmult)),
    file_init  = "initialisation/initialise_BASGRA_Saerheim_00_early_Gri.R" )

# So the function can take nine arguments, which we can change as we see fit.
# For example, we can reduce the number of output variables:
SA( "TILTOTI", vars=c("DM","TILTOT","LAI") )

# or:

SA( "K", vars="TILTOT")

################################################################################
### 4. Bayesian Calibration (BC)
################################################################################

# The files for BC have changed much since previous versions of the model.
# We only give a brief introduction here.

# An example of a single-site BC is the following (if you just want to see
# a short first test, make sure that 'nChain' is set to a low number like 1000
# in file "BC/BC_BASGRA_MCMC_init_Saerheim_00_early_Gri")
source("BC_BASGRA_Saerheim_00_early_Gri.R")
# The results of the BC are written to three files:
# 1. 'BASGRA_parModes_[].txt': parameter values
# 2. 'BC_parameters_priorbeta_histograms_[].pdf': parameter distributions
# 3. 'BC_outputs_data_[].pdf': model outputs and the calibration data.

# Multi-site BC can be done as in the following example:
source("BC_BASGRA_Gri_SAERHEIM_00_0102.R")
# Note that it is now possible for every parameter to specify its own degree of
# 'site-specificity'. That is done in the last column in the file
# 'parameters_BC_[].txt'. There you can specify whether the parameter should be
# calibrated generically for all of the sites, or that it should get different
# values for different subsets of the sites.

# After the BC, we can run the model for the MAP parameter vector and do a SA.
# We first find the name of the parameter-modes file that was produced last.
# [CAREFUL: we only included the hour&minute time in the file names, and not
#  the day. So the next two lines only look at the hour-minute time, and not 
#  the day-number, to determine which file is the most recent one!]
files_parModes <- list.files( pattern="BASGRA_parModes" )
file_parModes  <- tail( files_parModes, 1 )
# Now we do the SA, starting from the MAP parameter vector
SA_BC( "SIMAX1T", file_init_BC = "BC/BC_BASGRA_MCMC_init_Gri_SAERHEIM_00_0102.R", 
                  file_par     = file_parModes,
                  partype      = "MAP" )
# The 'SA_BC' function produces three files:
# 1. 'SA_BC_outputs_[].pdf': model outputs for all variables.
# 2. 'SA_BC_outputs_data_[].pdf': model outputs for observed variables with the BC-data
# 3. 'SA_BC_likelihoods_[].pdf': impact of parameter changes on likelihoods
# Note that SA_BC() does the sensitivity analysis for each of the sites that
# was included in the BC. So if the BC was for 11 different sites, SA_BC()
# will produce results for each of those sites, and it even takes into account
# that some parameters are site-specific and others are generic.

################################################################################
### 5. Running with weather data from the weather generator
################################################################################

# If you want to use artificial weather data generated by LARS-WG,
# then a different BASGRA DLL is required, namely BASGRA_WG.DLL. An example
# of an initialisation file that calls that DLL is the following:
source('initialisation/initialise_BASGRA_Saerheim_early_Gri_WG.R')

# Once we initialised, we can run the model and make plots as usual:
plot_output ( run_model() )
