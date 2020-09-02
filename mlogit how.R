#how to use the mlogit package...

#library and data
library(mlogit)
data("Heating") #look at the format the data is in

data("Fishing", package = "mlogit")
library("zoo")
Fish <- mlogit.data(Fishing, varying = c(2:9), shape = "wide", choice = "mode")
m <- mlogit(mode ~ price | income | catch, data = Fish)
# compute a data.frame containing the mean value of the covariates in
# the sample
z <- with(Fish, data.frame(price = tapply(price, index(m)$alt, mean),
                           catch = tapply(catch, index(m)$alt, mean),
                           income = mean(income)))

library(brms)
N <- 15
dat <- data.frame(
  y1 = rbinom(N, 10, 0.3), y2 = rbinom(N, 10, 0.5), 
  y3 = rbinom(N, 10, 0.7), x = rnorm(N)
)
dat$size <- with(dat, y1 + y2 + y3)
dat$y <- with(dat, cbind(y1, y2, y3))

library(catdata)
data(addiction)

prior <- prior(normal(0, 10), "b", dpar = muy2) +
  prior(cauchy(0, 1), "Intercept") +
  prior(normal(0, 2), "Intercept", dpar = muy3)
fit <- brm(bf(y | trials(size)  ~ 1, muy2 ~ x), data = dat, 
           family = multinomial(), prior = prior)

nelsonrep_long <- nelsonrep_long %>% replace_na(list(abo = 0, a_bo = 0, ab_o = 0))

cat_fit <- brm(replaced ~ 1 + overall_deflection + actor_deflection + behavior_deflection + object_deflection +
                          abo + a_bo + ab_o, family = categorical(link = "logit"), data = nelsonrep_long)

m3 <- mlogit(replaced ~ def + h | overall_deflection, rep)

stargazer(m3, title = "Model 3")

z <- with(rep2, data.frame(def = tapply(def, index(m3)$alt, mean),
                           h = tapply(h, index(m3)$alt, mean),
                           overall_deflection = mean(overall_deflection)))


me_m3 <- effects(m3, covariate = "overall_deflection", data = z)
me_m3 <- as.data.frame(me_m3)
colnames(me_m3) <- c("Marginal Effect")

kable(me_m3, caption = "Marginal Effects for the Overall Deflection", digits = 3)

# Model 3 includes a choice specific predictor - the overall deflection produced by the situation - meaning that it could impact the likelihood of selecting the behavior or object to replace differently. I included this, because I think it is likely that how "odd" the situation seems could have an effect on the respondents' probability of choosing each objet to replace.  
# 
# 
# Indeed, in Model 3, if the deflection increases by a point, the odds of choosing to replace the Object versus the Actor increase by 12.5%. However, the marginal effects of the increased overall deflection on the choice of outcome is actually a decrease of 3% for the Object. 
