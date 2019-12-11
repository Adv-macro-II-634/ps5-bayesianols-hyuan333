data("card")
attach(card)

#1
OLS<-lm(lwage~exper+educ+smsa+black+south, x=TRUE, y=TRUE)
summary(OLS)


beta_hat<-coef(OLS)
sigma_hat<-sigma(OLS)
theta_hat<-c(beta_hat, sigma_hat)
std_beta_hat<-c(0.06,0.0022,0.0035,0.0157,0.0178,0.0152)
std_sigma_hat<-0.377

x<-cbind(exper,educ,smsa,black,south)
y<-lwage

likelihood <- function(param){
  a = param[1]
  b = param[2]
  c = param[3]
  d = param[4]
  e = param[5]
  f = param[6]
  
  sd = param[7]
  pred = a+b*x[,1]+c*x[,2]+d*x[,3]+e*x[,4]+f*x[,5]
  singlelikelihoods = dnorm(y, mean = pred, sd = 0.15*sd, log = T)
  sumll = sum(singlelikelihoods)
  return(sumll)   
}


# Prior distribution
prior <- function(param){
  a = param[1]
  b = param[2]
  c = param[3]
  d = param[4]
  e = param[5]
  f = param[6]
  sd = param[7]
  # CHANGE THE NEXT 3 LINES TO CHANGE THE PRIOR, log is True, so these are log density/likelihood
  aprior = dnorm(0, sd = 1, log = T)
  bprior = dnorm(0, sd = 1, log = T)
  cprior = dnorm(0, sd = 1, log = T)
  dprior = dnorm(0, sd = 1, log = T)
  eprior = dnorm(0, sd = 1, log = T)
  fprior = dnorm(0, sd = 1, log = T)
  sdprior = dnorm(0, sd = 1, log = T)
  return(aprior+bprior+cprior+dprior+eprior+fprior+sdprior)
}
# Posterior
posterior <- function(param){
  return (likelihood(param) + prior(param))
}
######## Metropolis algorithm ################

proposalfunction <- function(param){
  return(rnorm(7,mean = param, sd= 0.15*c(0.06,0.0022,0.0035,0.0157,0.0178,0.0152,0.377)))
}

run_metropolis_MCMC <- function(startvalue, iterations){
  chain = array(dim = c(iterations+1,7))
  chain[1,] = startvalue
  for (i in 1:iterations){
    proposal = proposalfunction(chain[i,])
    
    probab = exp(posterior(proposal) - posterior(chain[i,]))
    if (runif(1) < probab){
      chain[i+1,] = proposal
    }else{
      chain[i+1,] = chain[i,]
    }
  }
  return(chain)
}
startvalue = c(1,0.5,0.1,0.1,0.1,0.1,0.2)
chain = run_metropolis_MCMC(startvalue, 30000)
 
burnIn = 5000
acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))


par(mfrow = c(2,4))
hist(chain[-(1:burnIn),1],prob=TRUE, main="Posterior of a(intercept)")
hist(chain[-(1:burnIn),2],prob=TRUE, main="Posterior of b(exper)")
hist(chain[-(1:burnIn),3],prob=TRUE, main="Posterior of c(educ)")
hist(chain[-(1:burnIn),4],prob=TRUE, main="Posterior of d(smsa)")
hist(chain[-(1:burnIn),5],prob=TRUE, main="Posterior of e(black)")
hist(chain[-(1:burnIn),6],prob=TRUE, main="Posterior of f(south)")
hist(0.15*chain[-(1:burnIn),7],prob=TRUE, main="Posterior of residual")
