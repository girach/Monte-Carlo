alg1=function(n,alpha)
{
algo1=vector(length=n)
i=1;count=0
while(i<=n)
{
a=runif(2)
count=count+1
X=-2*log(1-(a[1]^(1/alpha)))
if (X!=0){
if (a[2]<=((X^(alpha-1))*(exp(-X/2)))/
((2*(1-exp(-1*X/2)))^(alpha-1)))
{
algo1[i]=X
i=i+1
}}
}
integrand=function(x){x^(alpha)*exp(-1*x)}
c=integrate(integrand,lower=0,upper=Inf)$value
cat("Theoritical acceptance via alg1:",c/2^(alpha),"\n")
#print(1/c)
cat("Acceptance Obtained:",n/count,"\n")
png("image3.png")
hist(algo1,breaks=100,col="darkgreen")
dev.off()
#cat((n/count))
}
alg1(1000,0.9)
