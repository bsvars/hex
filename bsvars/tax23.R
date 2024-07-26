
# bsvars package installed from the developer's repo using:
# devtools::install_git("https://github.com/bsvars/bsvars.git")

library(bsvars)
data("us_fiscal_lsuw")
data("us_fiscal_ex")

set.seed(1234)
B         = matrix(TRUE, 3, 3)
spec      = specify_bsvar_sv$new(
              data = us_fiscal_lsuw,
              p    = 4,
              exogenous = us_fiscal_ex,
              B    = B
            )

burn      = estimate(spec, 3e5, thin = 1e4)
post      = estimate(burn, 6e5, thin = 10)

sddr      = verify_volatility(post)

save(spec, post, sddr, file = paste0("tax23.rda"))


load(paste0("spartan/results/tax23.rda"))
B0hat_PM          = post$last_draw$starting_values$B
qq_normL_PM       = normalise_jaro_bsvar_sv(post$posterior, B0hat_PM)
post              = bsvars::specify_posterior_bsvar_sv$new(post$last_draw, qq_normL_PM)
post$set_normalised()
sddr              = bsvars::verify_volatility(post)
save(post, spec, sddr, B0hat_PM, file = paste0("tax23nPM.rda"))
