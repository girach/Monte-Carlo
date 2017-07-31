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
return (n/count)
}
alg2=function(n,alpha)
{
algo2=vector(length=n)
i=1;count=0
b=((1-exp(-0.5))^(alpha))+((alpha)*exp(-1))/2^(alpha)
#print(b)
a=((1-exp(-0.5))^(alpha))/b
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
return (n/count)
}
alg3=function(n,alpha)
{
algo3=vector(length=n)
i=1;count=0
d=1.0334-0.0766*exp(2.2942*(alpha))
b=alpha*(d^(alpha-1))*exp(-1*d)
a=(2*(1-exp(-0.5*d)))^(alpha)
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
return (n/count)
}
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
return (n/count)
}
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
return (n/count)
}
v=10
ADans=vector(length=v)
Bestans=vector(length=v)
Algo1ans=vector(length=v)
Algo2ans=vector(length=v)
Algo3ans=vector(length=v)
i=1
while(i<=v)
{

n=10000;alpha=0.1*i
ADans[i]=AD(n,alpha)
Bestans[i]=Best(n,alpha)
Algo1ans[i]=alg1(n,alpha)
Algo2ans[i]=alg2(n,alpha)
Algo3ans[i]=alg3(n,alpha)
i=i+1
}
x=seq(0,0.999,0.1)
png("Main.png")
plot(x,ADans,type="l",ylab="Acception Probability",
ylim=c(0,1),col=5)
lines(x,Bestans,col=2,type="l",lty=2)
lines(x,Algo1ans,col=3,type="l",lty=3)
lines(x,Algo2ans,col=4,type="l",lty=4)
lines(x,Algo3ans,col=1,type="l",lty=5)
legend('topleft',legend=c("ADans","Bestans","Algo1ans",
"Algo2ans","Algo3ans"),lty=1:2:3:4:5,col=5:2:3:4:1,
bty='n')
dev.off()
