library(INLA)
data(Seeds)
?Seeds
head(Seeds)
# - r is the number of seed germinated (successes)
# - n is the number of seeds attempted (trials)
# - x1 is the type of seed
# - x2 is the type of root extract
# - plate is the numbering of the plates/experiments

# All the covariates are 0/1 factors, and the numbering of the plates are arbitrary. We do not re-scale any covariates. The observations are integers, so we do not re-scale these either.

df = data.frame(y = Seeds$r, Ntrials = Seeds$n, Seeds[, 3:5])
# Explore data
summary(df)
table(df$x1)
table(df$x2)
plot(df)

##########################
##### Likehood
##########################
family1 = "binomial"
control.family1 = list(control.link=list(model="logit"))
# number of trials is df$Ntrials

# This specifies which likelihood we are going to use for the observations. The binomial distribution is defined with a certain number of trials, in INLA known as Ntrials. If there were hyper-parameters in our likelihood, we would specify the priors on these in control.family
# 这指定了我们将使用哪种可能性进行观测。二项分布由一定数量的试验定义，在INLA中称为Ntrials。如果在我们的可能性中有超参数，我们会在control.family中指定这些先验


##########################
##   
#########################
fisher.dat <- readRDS(system.file("demodata/data_fisher2.rds", package
                                  = "INLA"))
fisher.dat$id1 <- fisher.dat$id
fisher.dat$dist_cent <- scale(fisher.dat$dist_cent)
formula.inla <- y ~ sex + landuse + dist_cent +
  f(stratum,model="iid",hyper=list(theta = list(initial=log(1e-6),fixed=T))) +
  f(id1,dist_cent, model="iid")
r.inla <- inla(formula.inla, family ="Poisson", data=fisher.dat)
summary(r.inla)


formula.inla <- y ~ sex + landuse + dist_cent + stratum
r.inla <- inla(formula.inla, family ="binomial", data=fisher.dat)
summary(r.inla)


###########################
####
###########################
n=100
a = 1
b = 1
z = rnorm(n)
eta = a + b*z
Ntrials = sample(c(1,5,10,15), size=n, replace=TRUE)
prob = exp(eta)/(1 + exp(eta))
y = rbinom(n, size=Ntrials, prob = prob)
data = list(y=y,z=z)
formula = y ~ 1+z
result = inla(formula, family = "binomial", data = data, Ntrials=Ntrials)
summary(result)





hyper = list(
  prec = list(
    prior = "normal",
    param = c(0,0.00001)
  )
)

















