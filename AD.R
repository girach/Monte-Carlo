AD=function(n,alpha)
{
AD=vector(length=n)
i=1;count=0
e=exp(1)
#print(e/(e+alpha))
k=(e+alpha)/(e*alpha)
#print(k)
while(i<=n)
{
arr=runif(2)
count=count+1
if (arr[1]<=e/(e+alpha)) 
{
X=((arr[1]*(e+alpha))/e)^(1/alpha)
}
else 
{
X=-log(k*(1-arr[1]))
}
#cat(arr[1],arr[2],X,"\n")
if (X<=1 && X!=0 && arr[2]<=exp(-X))
{
AD[i]=X
i=i+1
}
if (X>1 && arr[2]<=X^(alpha-1))
{
AD[i]=X
i=i+1
}
}
integrand=function(x){x^(alpha)*exp(-1*x)}
c=integrate(integrand,lower=0,upper=Inf)$value
cat("Theoritical Acceptance via AD method:",
e*c/(e+alpha),"\n")
#print(1/c)
cat("Acceptance Obtained:",n/count,"\n")
png("image1.png")
hist(AD,breaks=100,col="darkgreen")
dev.off()
#cat((n/count))

}
AD(1000,0.9)
