
devtools::install_github("bsvars/bvarPANELs")           # install the package

library(bvarPANELs)                                     # load the package
data(ilo_dynamic_panel)                                 # load the data
ilo_dynamic_panel$COL                                   # show the data for Colombia

spec = specify_bvarPANEL$new(                           # specify the model
  ilo_dynamic_panel,                                    # data
  exogenous = ilo_exogenous_variables,                  # exogenous variables
  stationary = c(FALSE, FALSE, FALSE, TRUE),            # stationarity (determines prior mean)
  type = c("real", "rate", "rate", "rate")              # variable types
)

burn = estimate(spec, S = 10000, show_progress = FALSE) # run the burn-in
post = estimate(burn, S = 10000)                        # estimate the model

fore = forecast(                                        # forecast the model 
  post,                                                 # estimation output
  horizon = 6,                                          # forecast horizon
  exogenous_forecast = ilo_exogenous_forecasts,         # forecasts for exogenous variables
  conditional_forecast = ilo_conditional_forecasts      # gdp projections
)

plot(fore, "COL", main = "Forecasts for Colombia")
summary(fore, "COL")$variable2

post |>                                                 # estimation output
  compute_variance_decompositions(horizon = 6) |>       # compute variance decompositions
  plot(which_c = "COL")                                 # plot variance decompositions
