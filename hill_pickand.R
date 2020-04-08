data<-read.delim2("Natixis stock.txt",header=F)

head(data)

n<-as.numeric(dim(data)[1])

rend <- c(0)

i=1
while(i < n)
{
  r <- (data[i+1,2]-data[i,2])/data[i,2]
  rend[i] <- r
  i = i + 1
}
head(rend)
summary(rend)

rend_s<-sort(rend)

pickand<-c(0)

for(k in 1:(n/4))
{
  pickand[k]=(1/log(2))*log((rend_s[n-k+1]-rend_s[n-2*k+1])/(rend_s[n-2*k+1]-rend_s[n-4*k+1]))
}
plot(pickand,type='l',ylim=c(-3,3))

hill_a<-c(0)
x_a<-numeric(400)
rend_a<-tail(rend_s,400)

for(k in 1:400)
{
  for(i in 1:(400-k+1))
  {
    x_a[k]=x_a[k]+log(rend_a[i]/rend_a[400-k+1])
  }
  hill_a[k]<-(1/k)*x_a[k]
}
plot(hill_a,type='l')

hill_b<-c(0)
x_b<-numeric(400)
rend_b<-abs(head(rend_s,400))
for(k in 1:400)
{
  for(i in 1:(400-k+1))
  {
    x_b[k]=x_b[k]+log(rend_b[i]/rend_b[400-k+1])
  }
  hill_b[k]<-(1/k)*x_b[k]
}
plot(hill_b,type='l')
