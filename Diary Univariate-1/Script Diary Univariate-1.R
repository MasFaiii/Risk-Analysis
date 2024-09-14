#PETUNJUK PRAKTIKUM UNIVARIATE DISTRIBUTION

#Figure I.3.11

x = seq(-7.5, 7, by=0.01)
u = dnorm(x)
v = dnorm(x,1,2)
plot(x,u,type="l")
lines(x,v,lty=3)

dnorm(0)
#[1] 0.3989423 # titik puncak distribusi N(0,1) adalah (0, dnorm(0))
dnorm(1,1,2)
#[1] 0.1994711 # titik puncak distribusi N(1,4) adalah (1, dnorm(1,1,2))
1/sqrt(2*pi)
#[1] 0.3989423
1/sqrt(8*pi)
#[1] 0.1994711

#membuat simulasi data berdasarkan informasi Example I.3.5

simulasi = function(n,B)
{
  has = numeric(B)
  for (i in 1:B)
  {
    x = rnorm(n,0.1,0.2)
    has[i] = sum(x<0)/n
  }
  has
}

hasil = simulasi(100, 10000)
hist(hasil)
summary(hasil)

#Example

qnorm(0.95)
pnorm(1.6449)
qnorm(0.01)
pnorm(-2.3263)
pnorm(3,1,2)
pnorm(-2,1,3)

#membuat simulasi data berdasarkan informasi Example I.3.6

simulasi = function(n)
{
  x = rnorm(n,0.24, 0.2)
  y = rnorm(n,0.16, 0.1)
  has = 0.25*x + 0.75*y
  has
}

hasil = simulasi(10000)
hist(hasil)
summary(hasil)

#Probabilitas mempunyai return negatif #Example I.3.6

simulasi = function(n,B)
{
  
  has = numeric(B)
  for (i in 1:B)
  {
    R = rnorm(n, 0.18, 0.109) 
    has[i] = sum(R<0)/n
  }
  has
}

hasil = simulasi(100, 10000)
hist(hasil)
summary(hasil)

#Probabilitas mempunyai return lebih dari 20 %

simulasi = function(n,B)
{
  
  has = numeric(B)
  for (i in 1:B)
  {
    R = rnorm(n, 0.18, 0.109) 
    has[i] = sum(R>0.2)/n
  }
  has
}

hasil = simulasi(100, 10000)
hist(hasil)
summary(hasil)

#membuat simulasi data berdasarkan informasi Example I.3.7

simulasi = function(n)
{
  x = rnorm(n,0.1, 0.25)
  y = rnorm(n,0.08, 0.15)
  has = x-y
  has
}

hasil = simulasi(10000)
hist(hasil)
summary(hasil)


simulasi = function(n,B)
{
  
  has = numeric(B)
  for (i in 1:B)
  {
    R = rnorm(n, 0.02, 0.1696) 
    has[i] = sum(R<0)/n
  }
  has
}

hasil = simulasi(100, 10000)
hist(hasil)
summary(hasil)


#LOG NORMAL DISTRIBUTION

u = seq(0,10,by=0.01)
v = dlnorm(u)
plot(u,v,type="l")
w = rlnorm(1000)
summary(w)

simulasi = function(n, B)
{
  has = numeric(B)
  for (i in 1:B)
  {
    has[i] = mean(rlnorm(n))
  }
  has
}

hasil = simulasi(100,10000)
hist(hasil)
summary(hasil)

simulasi = function(n, B)
{
  has = numeric(B)
  for (i in 1:B)
  {
    has[i] = var(rlnorm(n))
  }
  has
}

hasil = simulasi(100,10000)
hist(hasil)
summary(hasil)

#normal mixture

p = 0.3
normal.mixture = function(B, p)
{
  has = numeric(B)
  for (i in 1:B)
  {
    x = rnorm(B)
    y = rnorm(B, 0, 2)
    u = runif(B)
    if (u[i] < p) has[i] = x[i] else has[i] = y[i]
  }
  has
}

hasil = normal.mixture(10000, p)
hist(hasil)
summary(hasil)
sd(hasil)

p = 0.3
normal.mixture = function(B, p)
{
  x = rnorm(B)
  y = rnorm(B, 0, 2)
  u = runif(B)
  has = x
  v = which(u>0.3)
  has[v] = y[v]
  has
}

hasil1 = normal.mixture(10000, p)
hist(hasil1)
summary(hasil1)
summary(hasil)
sd(hasil)
sd(hasil1)

#Example I.3.8

p = 0.5
normal.mixture = function(B, p)
{
  x = rnorm(B, 0, 3)
  y = rnorm(B, 0, 4)
  u = runif(B)
  has = x
  v = which(u>p)
  has[v] = y[v]
  has
}

hasil1 = normal.mixture(10000, p)
hist(hasil1)
summary(hasil1)
var(hasil1)

#Figure I.3.13

u = seq(-7, 7, by = 0.01)
x = dnorm(u)
y = dnorm(u, 0, 2)
v = 0.3*dnorm(u) + 0.7*dnorm(u,0, 2)
plot(u,x,type="l")
lines(u, y, lty=2)
lines(u,v,lty=3)

#Example I.3.9

normal.mixture = function(B, p)
{
  x = rnorm(B, 0, 0.04)
  y = rnorm(B, 0, 0.03)
  u = runif(B)
  has = x
  v = which(u>p)
  has[v] = y[v]
  has
}

B = 10000
p = 0.5
hasil1 = normal.mixture(B, p)
hist(hasil1)
summary(hasil1)



peluang = sum(hasil1 < -0.1)/B
peluang

normal.mixture2 = function(B, p)
{
  has = numeric(B)
  for (i in 1:B)
  {
    x = rnorm(B, 1, 4)
    y = rnorm(B, -2, 3)
    u = runif(B)
    if (u[i] < p) has[i] = x[i] else has[i] = y[i]
  }
  has
}

B = 10000
p = 0.25
hasil2 = normal.mixture2(B, p)
hist(hasil2)
summary(hasil2)
peluang2 = sum(hasil2< -1)/B
peluang2


normal.mixture = function(B, p)
{
  x = rnorm(B, 1, 4)
  y = rnorm(B, -2, 3)
  u = runif(B)
  has = x
  v = which(u>p)
  has[v] = y[v]
  has
}

B = 20000
p = 0.25
hasil1 = normal.mixture(B, p)
hist(hasil1)
summary(hasil1)
peluang2 = sum(hasil1 < -1)/B
peluang2
