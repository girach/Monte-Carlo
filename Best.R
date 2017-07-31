Best=function(n,alpha)
{
best=vector(length=n)
i=1;count=0
d=0.07+0.75*((1-alpha)^(0.5))
b=1+exp(-1*d)*alpha/d
while(i<=n)
{
arr=runif(2)
count=count+1
if (arr[1]<=1/b) 
{
X=((b*arr[1])^(1/alpha))*d
}
else 
{
X=-log(exp(-1*d)+(d-b*d*arr[1])/(alpha))
}
#cat(arr[1],arr[2],X,"\n")
if (X<=d && X!=0 && arr[2]<=exp(-X))
{
best[i]=X
i=i+1
}
if (X>d && arr[2]<=(X/d)^(alpha-1))
{
best[i]=X
i=i+1
}
}
integrand=function(x){x^(alpha)*exp(-1*x)}
c=integrate(integrand,lower=0,upper=Inf)$value
cat("Theoritical Acceptance via Best method:",
c/((d+exp(-1*d)*alpha)*(d^(alpha-1))),"\n")
#print(1/c)
cat("Acceptance Obtained:",n/count,"\n")
png("image2.png")
hist(best,breaks=100,col="darkgreen")
dev.off()
#cat((n/count))

}
Best(1000,0.9)
