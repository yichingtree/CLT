## Create a normal population

n.pop=1000
pop=rnorm(1000,160,20)

hist(pop, xlim=c(100,250))

pop.mean=mean(pop)

pop.sd=sqrt(sum((pop-mean(pop))^2)/n.pop)

pop.sd

### Sampling ditribution

sample.mean=vector()

nperm=10000


n=10

for (i in 1:nperm)
{
temp=sample(pop, n)
sample.mean[i]=mean(temp)
}

hist(sample.mean, xlim=c(100,250))

mean.sam.mean=mean(sample.mean)
sam.sd=sqrt(sum((sample.mean-mean(sample.mean))^2)/length(sample.mean))



mean.sam.mean
sam.sd

20/sqrt(10)


### 

par(mfrow=c(2,1))
hist(pop, xlim=c(100,250))
hist(sample.mean, xlim=c(100,250))
