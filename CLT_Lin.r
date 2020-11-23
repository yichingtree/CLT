####################################################
#  2019.10.28 Biostat Lab: Central Limit Theory
#                          Sampling distribution
#
####################################################

## Creating a population follows a normal distribution (n=1000, mean=0, sd=1)

par(mfrow=c(2,2))

set.seed(25)
pop=rnorm(1000,0,1)

## Calculating population mean and standard deviation
n=length(pop)
hist(pop)
pop.miu=mean(pop)
pop.sigma=(sum((pop-pop.miu)^2)/n)^0.5

## Creating a distribution of sample means when n=4

n1=4
xbar=numeric()

n.perm=1000

for (i in 1:n.perm)
{
  samp=sample(pop, n1, replace=TRUE)
  xbar[i]=mean(samp)
  
}

hist(xbar, main="Normal distribution, n=4")

# Mean and standard deviation of sample means
grand.mean.4=mean(xbar)
grand.sigma.4=(sum((xbar-grand.mean.4)^2)/n.perm)^0.5

## Creating a distribution of sample means when n=8

n2=8
xbar2=numeric()
n.perm=1000

for (i in 1:n.perm)
{
  samp=sample(pop, n2, replace=TRUE)
  xbar2[i]=mean(samp)
  
}

hist(xbar2, main="Normal distribution, n=8")

# Mean and standard deviation of sample means
grand.mean.8=mean(xbar2) 
grand.sigma.8=(sum((xbar2-grand.mean.8)^2)/n.perm)^0.5

## Creating a distribution of sample means when n=100

n3=100
xbar3=numeric()

for (i in 1:n.perm)
{
  samp=sample(pop, n3, replace=TRUE)
  xbar3[i]=mean(samp)
  
}

hist(xbar3, main="Normal distribution, n=100")

# Mean and standard deviation of sample means
grand.mean.100=mean(xbar3) 
grand.sigma.100=(sum((xbar3-grand.mean.100)^2)/n.perm)^0.5

#########################################################
#
# Central Limit Theorem for a non-normal distribution 
# (Exponential distribution)
#
##########################################################
par(mfrow=c(2,2))

## Creating a population of an exponential distribution
set.seed(25)
pop.ex=rexp(1000,5)

## Calculating the mean and standard deviation of the population

hist(pop.ex, main="Poulation of Exponential Distribution")
pop.ex.miu=mean(pop.ex)
pop.ex.sigma=(sum((pop.ex-pop.ex.miu)^2)/1000)^0.5



## Creating a distribution of sample means when n=4
n1=4
xbar.ex=numeric()
n.perm=1000

for (i in 1:n.perm)
{
  samp=sample(pop.ex, n1, replace=TRUE)
  xbar.ex[i]=mean(samp)
  
}

hist(xbar.ex, main="Exponential Distribution, n=4")

# Calculating mean and standard deviation of sample means
grand.mean.ex.4=mean(xbar.ex) 
grand.sigma.ex.4=(sum((xbar.ex-grand.mean.ex.4)^2)/n.perm)^0.5

## Creating a distribution of sample means when n=8

n2=8
xbar.ex.2=numeric()
n.perm=1000

for (i in 1:n.perm)
{
  samp=sample(pop.ex, n2, replace=TRUE)
  xbar.ex.2[i]=mean(samp)
}

hist(xbar.ex.2, main="Exponential Distribution, n=8")

## Calculating mean and standard deviation of sample means
grand.mean.ex.8=mean(xbar.ex.2) 
grand.sigma.ex.8=(sum((xbar.ex.2-grand.mean.ex.8)^2)/n.perm)^0.5

## Creating a distribution of sample means when n=100

n3=100
xbar.ex.3=numeric()
n.perm=1000

for (i in 1:n.perm)
{
  samp=sample(pop.ex, n3, replace=TRUE)
  xbar.ex.3[i]=mean(samp)
  
}

hist(xbar.ex.3, main="Exponential Distribution, n=100")

## Calculating mean and standard deviation of sample means
grand.mean.ex.100=mean(xbar.ex.3) 
grand.sigma.ex.100=(sum((xbar.ex.3-grand.mean.ex.100)^2)/n.perm)^0.5
