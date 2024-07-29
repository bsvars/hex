
############################################################
# Reproduction of the IRF from Arias, Rubio-Ramírez, and Waggoner (2018)
############################################################

# estimate the model
############################################################
# devtools::install_github("bsvars/bsvars")
# devtools::install_github("bsvars/bsvarSIGNs")
# library(bsvarSIGNs)
# set.seed(123)
# data(optimism)
# sign_irf       = matrix(c(0, 1, rep(NA, 23)), 5, 5)
# specification  = specify_bsvarSIGN$new(optimism * 100,
#                                        p        = 4,
#                                        sign_irf = sign_irf)
# specification$prior$Ysoc = matrix(0, nrow(specification$prior$Ysoc), 0)
# specification$prior$Xsoc = matrix(0, nrow(specification$prior$Xsoc), 0)
# specification$prior$Ysur = matrix(0, nrow(specification$prior$Ysur), 0)
# specification$prior$Xsur = matrix(0, nrow(specification$prior$Xsur), 0)
# posterior      = estimate(specification, S = 200000)
# 
# save(posterior, specification, sign_irf, file = "bsvarSIGNs/bsvarSIGNs.rda")
load("bsvarSIGNs/bsvarSIGNs.rda")

# impulse responses
#######################################################
irfs    = compute_impulse_responses(posterior, horizon = 20)
i = 5; j = 1
irfs    = irfs[i,j,, ]
irf_med       = apply(irfs, 1, median)
irf_hdi       = apply(irfs, 1, HDInterval::hdi, credMass = 0.7)


# sticker properties
############################################################
# Define colors
bsblu   = "#001D31"
bspin   = "#F500BD"

stickerColor = bsblu



# figure
#######################################################
svg(file = "bsvarSIGNs/irf.svg",
    width = 1.2 * 9,
    height = 1.2 * 6.5
)
par(
  bg = bsblu,
  mar = c(2, 2, 0, 0)
)
graphics::plot(
  x = 1:length(irf_med), 
  y = irf_med,
  ylim = c(-0.5, 1.5),
  # ylim = range(irf_hdi),
  type = "n",
  col = bspin,
  lwd = 32,
  ylab = "",
  xlab = "",
  lend = 2,
  axes = FALSE
)
polygon(
  x = c(1:length(irf_med), rev(1:length(irf_med))),
  y = c(irf_hdi[1,], rev(irf_hdi[2,])),
  col = bspin,
  border = NA
)
graphics::lines(
  x = 1:length(irf_med), 
  y = irf_med,
  # ylim = c(-0.065, 0.06),
  col = bsblu,
  lwd = 32,
  lend = 2
)
abline(
  v = 21.3,
  col = bsblu,
  lwd = 30
)
abline(
  v = 0.7,
  col = bsblu,
  lwd = 30
)
ticks_vertical      = c(
  seq(from = 0, to = 5, by = 0.05),
  seq(from = 10, to = 15, by = 0.05),
  20) + 1
ticks_horizontal    = c(
  seq(from = -0.5, to = 0, by = 0.005),
  seq(from = 0.5, to = 1, by = 0.005),
  1.5
)
axis(1, 
     ticks_vertical, 
     rep("",length(ticks_vertical)), 
     col = bspin, 
     lwd = 12, 
     lwd.ticks = 12,
     tcl = -1
)
axis(2, 
     ticks_horizontal, 
     rep("", length(ticks_horizontal)), 
     col = bspin, 
     lwd = 12, 
     lwd.ticks = 12,
     tcl = -1
)
dev.off()

# image formattiing and including
img <- magick::image_read_svg("bsvarSIGNs/irf.svg", width = 2 * 1.2 * 1080, height = 2 * 1.1 * 840)
# img |> magick::image_crop(geometry = "1450x950+200+240")  -> img

# font adjustments
## Loading Google fonts (http://www.google.com/fonts)
sysfonts::font_add_google("Baloo 2", "font_fam")

## Automatically use showtext to render text for future devices
showtext::showtext_auto()


final_res <- hexSticker::sticker(img,
                                package = "bsvarSIGNs",
                                p_size = 80,
                                p_family = "font_fam",
                                p_fontface = "bold",
                                p_y = 1.4,
                                p_color = bspin,
                                s_x = 1,
                                s_y = 0.87,
                                s_width = 1.1,
                                s_height = 1.0,
                                filename = "bsvarSIGNs/bsvarSIGNs.png",
                                h_fill = bsblu,
                                h_color = bspin,
                                h_size = 1.3,
                                dpi = 1200)

plot(final_res)

# system("cp bsvarSIGNs/bsvarSIGNs.png /Users/twozniak/Research/bsvars/")

# contribute to the README of the hexSticker on GH: https://github.com/GuangchuangYu/hexSticker?tab=readme-ov-file