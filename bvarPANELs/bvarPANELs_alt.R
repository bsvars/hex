
############################################################
# Reproduction of the IRF from Woźniak, Sanchez-Martinez (2024, ILO)
############################################################

# estimate the model using bvarPANELs v0.1
############################################################
# devtools::install_github("bsvars/bvarPANELs")           # install the package
# library(bvarPANELs)                                     # load the package
# 
# spec = specify_bvarPANEL$new(                           # specify the model
#   ilo_dynamic_panel,                                    # data
#   exogenous = ilo_exogenous_variables,                  # exogenous variables
#   stationary = c(FALSE, FALSE, FALSE, TRUE),            # stationarity (determines prior mean)
#   type = c("real", "rate", "rate", "rate")              # variable types
# )
# 
# burn = estimate(spec, S = 10000, show_progress = FALSE) # run the burn-in
# post = estimate(burn, S = 10000)                        # estimate the model
# 
# fore = forecast(                                        # forecast the model 
#   post,                                                 # estimation output
#   horizon = 6,                                          # forecast horizon
#   exogenous_forecast = ilo_exogenous_forecasts,         # forecasts for exogenous variables
#   conditional_forecast = ilo_conditional_forecasts      # gdp projections
# )
# save(spec, post, fore, file = "bvarPANELs/ilo_forecasts.rda")

# plot(fore, "COL", main = "Forecasts for Colombia")

# sticker properties
############################################################
# Define colors
bsyell = "#1A003F"
bspink = "#1614B1"
bsyell_trans  = rgb(t(col2rgb(bsyell, alpha = F)), alpha=170, maxColorValue=255)

stickerColor = bspink

# forecasts
############################################################
load("bvarPANELs/ilo_forecasts.rda")

data_period = 16:32
COL_ur   = fore$COL$Y[2, data_period]
COL_med  = apply(fore$COL$forecasts[2,,], 1, median)
COL_hdi  = apply(fore$COL$forecasts[2,,], 1, HDInterval::hdi, credMass = 0.8)

POL_ur   = fore$POL$Y[2, data_period]
POL_med  = apply(fore$POL$forecasts[2,,], 1, median)
POL_hdi  = apply(fore$POL$forecasts[2,,], 1, HDInterval::hdi, credMass = 0.8)

NEW_ur   = fore$BGD$Y[2, data_period]
NEW_med  = apply(fore$BGD$forecasts[2,,], 1, median)
NEW_hdi  = apply(fore$BGD$forecasts[2,,], 1, HDInterval::hdi, credMass = 0.95)


svg(file = "bvarPANELs/fore.svg",
    width = 1.1 * 9,
    height = 0.95 * 6.5
)
par(
  bg = bspink,
  mar = c(2, 2, 0, 0)
)
plot(
  x = 1:length(c(COL_ur, COL_med)),
  y = c(COL_ur, COL_med),
  ylim = range(COL_ur, POL_ur, COL_hdi, POL_hdi),
  type = "l",
  col = bsyell,
  lwd = 20,
  lend = 2,
  axes = FALSE
)
lines(
  x = 1:length(c(POL_ur, POL_med)),
  y = c(POL_ur, POL_med),
  col = bspink,
  lwd = 28,
  lend = 2,
)
lines(
  x = 1:length(c(POL_ur, POL_med)),
  y = c(POL_ur, POL_med),
  col = bsyell,
  lwd = 20,
  lend = 2,
)
# lines(
#   x = 1:length(c(NEW_ur, NEW_med)),
#   y = c(NEW_ur, NEW_med),
#   col = bsyell,
#   lwd = 20,
#   lend = 2,
# )
polygon(
  x = c(17:length(c(COL_ur, COL_med)), rev(17:length(c(COL_ur, COL_med)))),
  y = c(tail(COL_ur, 1), COL_hdi[1,], rev(COL_hdi[2,]), tail(COL_ur, 1)),
  col = bsyell_trans,
  border = NA
)
polygon(
  x = c(17:length(c(POL_ur, POL_med)), rev(17:length(c(POL_ur, POL_med)))),
  y = c(tail(POL_ur, 1), POL_hdi[1,], rev(POL_hdi[2,]), tail(POL_ur, 1)),
  col = bsyell_trans,
  border = NA
)
# polygon(
#   x = c(17:length(c(NEW_ur, COL_med)), rev(17:length(c(NEW_ur, NEW_med)))),
#   y = c(tail(NEW_ur, 1), NEW_hdi[1,], rev(NEW_hdi[2,]), tail(NEW_ur, 1)),
#   col = bsyell_trans,
#   border = NA
# )
abline(
  v = 23.3,
  col = bspink,
  lwd = 32
)
ticks_vertical      = c(seq(from = 7, to = 15, by = 0.05), -1) + 1
axis(2, 
     ticks_vertical, 
     rep("",length(ticks_vertical)), 
     col = bsyell, 
     lwd = 12, 
     lwd.ticks = 12,
     tcl = -1
)
ticks_horizontal    = c(1, 6, 11.5,
                        seq(from = 17.5, to = 22.8, by = 0.05))
axis(1, 
     ticks_horizontal, 
     rep("", length(ticks_horizontal)), 
     col = bsyell, 
     lwd = 12, 
     lwd.ticks = 12,
     tcl = -1
)
dev.off()

# image formattiing and including
img <- magick::image_read_svg("bvarPANELs/fore.svg", width = 1 * 1080, height = 1 * 840)
# img |> magick::image_crop(geometry = "1450x950+200+240")  -> img

# font adjustments
## Loading Google fonts (http://www.google.com/fonts)
sysfonts::font_add_google("Baloo 2", "font_fam")
# various options I tried for the first argument above:
# "Quicksand" v
# "Comfortaa" vv
# "Rajdhani"
# "Montserrat Alternates" v
# "Mitr" v
# "Baloo 2" vv

## Automatically use showtext to render text for future devices
showtext::showtext_auto()


final_res <- hexSticker::sticker(img,
                                package = "bvarPANELs",
                                p_size = 80,
                                p_family = "font_fam",
                                p_fontface = "bold",
                                p_y = 1.35,
                                p_color = bsyell,
                                s_x = 1,
                                s_y = 0.84,
                                s_width = 1.1,
                                s_height = 1.2,
                                filename = "bvarPANELs/bvarPANELs.png",
                                h_fill = bspink,
                                h_color = bsyell,
                                h_size = 1.3,
                                dpi = 1200)

plot(final_res)

# system("cp bsvars/bsvars.png /Users/twozniak/Research/bsvars/")
# system("cp bsvars/bsvars.png /Users/twozniak/Research/bsvars/bsvars.github.io/")
# system("cp bsvars/bsvars.png /Users/twozniak/Research/donotdespair/")
# system("cp bsvars/bsvars.png /Users/twozniak/Research/bsvars/presentations/2024-05-bsvars-mcxs/")
# system("cp bsvars/bsvars_ukr.png /Users/twozniak/Research/bsvars/presentations/2024-08-bsvars-ukr/")

# contribute to the README of the hexSticker on GH: https://github.com/GuangchuangYu/hexSticker?tab=readme-ov-file