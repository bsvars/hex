
############################################################

# sticker properties
############################################################
# Define colors
# bspink = "#ffd700"
bspink = "white"
bsyell = "#ff69b4"
bsyell_trans  = rgb(t(col2rgb(bsyell, alpha = F)), alpha=50, maxColorValue=255)

stickerColor = bspink

# Reproduction of the IRFs
############################################################
# library(bvars)
# set.seed(12)
# spec = specify_bvar$new(
#   us_macro_chan,
#   p = 4,
# )
# burn = estimate(spec, 1000)
# post = estimate(burn, 1000)
# fore = forecast(post, horizon = 12)
# fore$forecast_mean
# save(fore, file = "bvars/fore.rda")


h    = 8

load("bvars/fore.rda")
gdp_me = apply(fore$forecasts[2,1:h,], 1, mean)
gdp_sd = apply(fore$forecasts[2,1:h,], 1, sd)

limits.1    = range(gdp_me[h]+3*gdp_sd[h], gdp_me[h]-3*gdp_sd[h])
point.f     = gdp_me
interval.f  = rbind(point.f + qnorm(.975)*gdp_sd, 
                    point.f + qnorm(.025)*gdp_sd)


x      = seq(
  from=limits.1[2], 
  to=limits.1[1], 
  length.out=200
)
z      = matrix(NA,200,h)
for (i in 1:h){
  z[,i] = dnorm(
    x=x, 
    mean=point.f[i],
    sd=gdp_sd[i]
  )
}
yy      = 1:h


svg(
  file = "bvars/fore.svg",
  width = 1 * 9,
  height = 1 * 6.5,
  bg = "transparent"
)
par(
  mar = c(0, 0, 0, 0)
)

# theta = 146
# phi   = 22

theta = 152
phi   = 14

f4    = plot3D::persp3D(
  x=x, 
  y=yy, 
  z=z, 
  phi=phi, 
  theta=theta, 
  xlab="", 
  ylab="", 
  zlab="", 
  shade=NA, 
  border=NA, 
  ticktype="detailed", 
  nticks=3,
  cex.lab=1, 
  col=NA,
  plot=FALSE
)
plot3D::perspbox(
  x=x, 
  y=yy, 
  z=z, 
  bty="n", 
  col.axis="white", 
  phi=phi, 
  theta=theta, 
  xlab="", 
  ylab="", 
  zlab="", 
  ticktype="detailed", 
  nticks=3,
  cex.lab=1, 
  col = NULL, 
  plot = TRUE
)
plot3D::polygon3D(
  x=c(interval.f[1,],interval.f[2,h:1]),
  y=c(1:h,h:1), z=rep(0,2*h),
  col = bsyell_trans,
  NAcol = "white",
  border = NA,
  add = TRUE,
  plot = TRUE
)
f4.l1 = trans3d(
  x=point.f,
  y=yy,
  z=0,
  pmat=f4
)
lines(
  f4.l1,
  lwd=8,
  lend = "butt",
  col= bsyell
)
for (i in 1:h){
  f4.l = trans3d(
    x=x, 
    y=yy[i], 
    z=z[,i], 
    pmat=f4
  )
  lines(
    f4.l, 
    lwd=6, 
    col= bsyell
  )
}
dev.off()

# image formattiing and including
img <- magick::image_read_svg("bvars/fore.svg", width = 1 * 1080, height = 1 * 840)
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
                                package = "bvars",
                                p_size = 60,
                                p_family = "font_fam",
                                p_fontface = "bold",
                                p_y = 1.4,
                                p_color = bsyell,
                                s_x = 1.15,
                                s_y = 1,
                                s_width = 2.1,
                                s_height = 1.65,
                                filename = "bvars/bvars.png",
                                h_fill = bspink,
                                h_color = bsyell,
                                h_size = 1.3,
                                dpi = 600)

plot(final_res)

# system("cp bsvars/bsvars.png /Users/twozniak/Research/bsvars/")
# system("cp bsvars/bsvars.png /Users/twozniak/Research/bsvars/bsvars.github.io/")
# system("cp bsvars/bsvars.png /Users/twozniak/Research/donotdespair/")
# system("cp bsvars/bsvars.png /Users/twozniak/Research/bsvars/presentations/2024-05-bsvars-mcxs/")
# system("cp bsvars/bsvars_ukr.png /Users/twozniak/Research/bsvars/presentations/2024-08-bsvars-ukr/")

# contribute to the README of the hexSticker on GH: https://github.com/GuangchuangYu/hexSticker?tab=readme-ov-file