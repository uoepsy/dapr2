library(tidyverse)

# eseed=round(runif(1,1e3,1e5))
set.seed(29330)
N = 600
age = round(runif(N,18,60))

# think of these as latent "propensities for anxiety, positive and negative affect"
# based on distributions i can find, gad and panas_neg should be skewed right. 
# panas_pos fairly centered.  
# it feels sensible to make gad and panas_neg positively correlated, 
# and they are both negatively correlated with panas_pos
pGPN = MASS::mvrnorm(N, mu=c(-1.5,-.5,0), 
                     Sigma = matrix(c(
                         1,.4,-.3,
                         .4,1,-.5,
                         -.3,-.5,1
                     ), nrow=3))

# observed scores (sneaky way of using binomial to sim likert like data)
anxiety = sapply(pGPN[,1], \(x) sum(replicate(7, rbinom(1,3,plogis(x)))))
negative_affect= sapply(pGPN[,2], \(x) sum(replicate(10, rbinom(1,4,plogis(x))+1)))
positive_affect = sapply(pGPN[,3], \(x) sum(replicate(10, rbinom(1,4,plogis(x))+1)))

# distributions:  
par(mfrow=c(3,1))
hist(anxiety);hist(positive_affect);hist(negative_affect)
pairs(cbind(anxiety,positive_affect,negative_affect))

# experimental grid
expgrid = tidyr::expand_grid(
    tilt_condition = c("up","down","none"),
    occlusion_condition = c("upper","lower"),
    n = 1:100
) 

# data 
dat = data.frame(
    anxiety,negative_affect,positive_affect,
    expgrid[,-3]
)
dat$tilt_condition = factor(dat$tilt_condition, levels=c("none","down","up"))

# model matrix for outcome
Xmat = model.matrix(rnorm(N) ~ scale(anxiety) + scale(negative_affect) + scale(positive_affect) + tilt_condition * occlusion_condition, 
                    data = dat)

# we need coefficients for population model, in this order:
dimnames(Xmat)[[2]]

# linear predictor: 
lp = Xmat %*% c(0, .1, .1, 0, 
                .4, .4, 0,
                -.2, .1)
# outcome, then scaled somehow
dat$dominance = rnorm(N, lp, 1)

# dominanceinance is based on https://www.sciencedirect.com/science/article/pii/S1090513810000267#app1
# which is apparently 8 x 7-point likerts, so range 8-56
dat$dominance = round(28.4 + scale(dat$dominance)[,1]*6.2)
range(dat$dominance)

# make sure data is randominancely shuffled 
# (otherwise students will panick with DWT of autocorrelation)
dat = dat[sample(1:N), ]
row.names(dat) <- NULL
dat$PID = paste0("PPT_",1:N)
head(dat)

dat$age <- age

dat <- dat %>% relocate(PID, .before=anxiety)
dat <- dat %>% relocate(age, .before=anxiety)

# test:
lm(dominance ~ anxiety + negative_affect + positive_affect + tilt_condition * occlusion_condition, 
   data = dat) |>
    summary()
