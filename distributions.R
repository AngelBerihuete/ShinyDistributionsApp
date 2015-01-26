library(shiny)

# To be called from server.R
####################################################
# Continuous distributions
####################################################
# F-distribution
f.func <- function(df1, df2, p_or_c){
  ncp <- 0
  if(p_or_c == "p"){
    func <- function(x) df(x, df1, df2, ncp)
  } else {
    func <- function(x) pf(x, df1, df2, ncp)
  }
  return(func)
}

# Noncentral F-distribution
ncf.func <- function(df1, df2, ncp, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) df(x, df1, df2, ncp)
  } else {
    func <- function(x) pf(x, df1, df2, ncp)
  }
  return(func)
}

# Chi-squared distribution
chisq.func <- function(df, p_or_c){
  ncp <- 0
  if(p_or_c == "p"){
    func <- function(x) dchisq(x, df, ncp)
  } else {
    func <- function(x) pchisq(x, df, ncp)
  }
  return(func)
}

# Noncentral chi-squared distribution Noncentralカイ二乗 distribution
ncChisq.func <- function(df, ncp, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dchisq(x, df, ncp)
  } else {
    func <- function(x) pchisq(x, df, ncp)
  }
  return(func)
}

# Gamma distribution
gamma.func <- function(shape, scale, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dgamma(x, shape, scale)
  } else {
    func <- function(x) pgamma(x, shape, scale)
  }
  return(func)
}

# Cauchy distribution
cauchy.func <- function(location, scale, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dcauchy(x, location, scale)
  } else {
    func <- function(x) pcauchy(x, location, scale)
  }
  return(func)
}

# Exponential distribution
exp.func <- function(rate, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dexp(x, rate)
  } else {
    func <- function(x) pexp(x, rate)
  }
  return(func)
}

# Normal distribution
norm.func <- function(mean, sd, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dnorm(x, mean, sd)
  } else {
    func <- function(x) pnorm(x, mean, sd)
  }
  return(func)
}

# Log-normal distribution
lnorm.func <- function(meanlog, sdlog, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dlnorm(x, meanlog, sdlog)
  } else {
    func <- function(x) plnorm(x, meanlog, sdlog)
  }
  return(func)
}

# t-distribution
t.func <- function(df, p_or_c){
  ncp <- 0
  if(p_or_c == "p"){
    func <- function(x) dt(x, df, ncp)
  } else {
    func <- function(x) pt(x, df, ncp)
  }
  return(func)
}

# Noncentral t-distribution
nct.func <- function(df, ncp, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dt(x, df, ncp)
  } else {
    func <- function(x) pt(x, df, ncp)
  }
  return(func)
}

# Beta distribution
beta.func <- function(shape1, shape2, p_or_c){
  ncp <- 0
  if(p_or_c == "p"){
    func <- function(x) dbeta(x, shape1, shape2, ncp)
  } else {
    func <- function(x) pbeta(x, shape1, shape2, ncp)
  }
  return(func)
}

# Noncentral beta distribution
ncbeta.func <- function(shape1, shape2, ncp, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dbeta(x, shape1, shape2, ncp)
  } else {
    func <- function(x) pbeta(x, shape1, shape2, ncp)
  }
  return(func)
}

# Uniform distribution
unif.func <- function(min, max, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dunif(x, min, max)
  } else {
    func <- function(x) punif(x, min, max)
  }
  return(func)
}

# Logistic distribution
logis.func <- function(location, scale, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dlogis(x, location, scale)
  } else {
    func <- function(x) plogis(x, location, scale)
  }
  return(func)
}

# Weibull distribution
weibull.func <- function(shape, scale, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dweibull(x, shape, scale)
  } else {
    func <- function(x) pweibull(x, shape, scale)
  }
  return(func)
}

####################################################
# Discrete distribution
####################################################
# Geometric distribution
geom.func <- function(prob, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dgeom(x, prob)
  } else {
    func <- function(x) pgeom(x, prob)
  }
  return(func)
}

# Hypergeometric distribution
hyper.func <- function(m, n, k, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dhyper(x, m, n, k)
  } else {
    func <- function(x) phyper(x, m, n, k)
  }
  return(func)
}

# Binomial distribution
binom.func <- function(size, prob, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dbinom(x, size, prob)
  } else {
    func <- function(x) pbinom(x, size, prob)
  }
  return(func)
}

# Negative binomial distribution
nbinom.func <- function(size, prob, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dnbinom(x, size, prob)
  } else {
    func <- function(x) pnbinom(x, size, prob)
  }
  return(func)
}

# Poisson distribution
pois.func <- function(lambda, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dpois(x, lambda)
  } else {
    func <- function(x) ppois(x, lambda)
  }
  return(func)
}

# # Discrete uniform distribution
# dunif.func <- function(min, max, p_or_c){
#   if(p_or_c == "p"){
#     func <- function(x) dunif(x, min, max)
#   } else {
#     func <- function(x) punif(x, min, max)
#   }
#   return(func)
# }

