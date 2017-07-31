alg2=function(n,alpha)
{
algo2=vector(length=n)
i=1;count=0
b=((1-exp(-0.5))^(alpha))+((alpha)*exp(-1))/2^(alpha)
#print(b)
a=((1-exp(-0.5))^(alpha))/b
#print(a)
while(i<=n)
{
arr=runif(2)
count=count+1
if (arr[1]<=a) 
{
X=-2*log(1-(arr[1]*b)^(1/alpha))
}
else 
{
X=-1*log(((2^(alpha))/(alpha))*b*(1-arr[1]))
}
if (X<=1 && X!=0 && (arr[2]<=((X^(alpha-1))*
(exp(-X/2)))/((2*(1-exp(-1*X/2)))^(alpha-1))))
{
algo2[i]=X
i=i+1
}
if (X>1 && arr[2]<=(X^(alpha-1)))
{
algo2[i]=X
i=i+1
}
}
integrand=function(x){x^(alpha)*exp(-1*x)}
c=integrate(integrand,lower=0,upper=Inf)$value
cat("Theoritical Acceptance via alg2:",
c/((2^(alpha))*b),"\n")
#print(1/c)
cat("Acceptance Obtained",n/count,"\n")
png("image4.png")
hist(algo2,breaks=100,col="darkgreen")
dev.off()
}
alg2(1000,0.9)
