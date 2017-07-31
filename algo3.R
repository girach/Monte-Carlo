alg3=function(n,alpha)
{
algo3=vector(length=n)
i=1;count=0
d=1.0334-0.0766*exp(2.2942*(alpha))
b=alpha*(d^(alpha-1))*exp(-1*d)
#print(b)
a=(2*(1-exp(-0.5*d)))^(alpha)
#print(a)
c=a+b
while(i<=n)
{
arr=runif(2)
count=count+1
if (arr[1]<=a/c) 
{
X=-2*log(1-((c*arr[1])^(1/alpha))/2)
}
else 
{
X=-log((c*(1-arr[1]))/(alpha*(d^(alpha-1))))
}
if (X<=d && X!=0 && (arr[2]<=((X^(alpha-1))*
(exp(-X/2)))/((2*(1-exp(-1*X/2)))^(alpha-1))))
{
algo3[i]=X
i=i+1
}
if (X>d && arr[2]<(d/X)^(1-alpha))
{
algo3[i]=X
i=i+1
}
}
integrand=function(x){x^(alpha)*exp(-1*x)}
d=integrate(integrand,lower=0,upper=Inf)$value
cat("Theoritical acceptance via alg3:",d/c,"\n")
#print(1/c)
cat("Acceptance Obtained:",n/count,"\n")
png("image5.png")
hist(algo3,breaks=100,col="darkgreen")
dev.off()
}
alg3(1000,0.9)
