---
title: "Chapter 6 HW"
author: "Esteban"
date: "May 29, 2019"
output: 
  html_document: 
    keep_md: yes
---





## 6H1

```r
divorce_dag <- dagitty( "dag {
    S -> A -> D
    S -> M -> D
    S -> W -> D
    A -> M
}")
coordinates( divorce_dag ) <- list( x=c(S=0, W=1, M=0.5, A=0, D=1),
                                  y=c(S=0, W=0, M=0.5,   A=1, D=1) )
plot(divorce_dag)
```

![](Chapter6HW_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
d$D <- scale(d$Divorce)
d$W <- scale(d$WaffleHouses)
d$S <- scale((d$South +1))
d$A <- scale(d$MedianAgeMarriage)
d$M <- scale(d$Marriage)

divorce_modS <- quap(
  alist(
    D ~ dnorm( mean=mu, sd=sigma ),
    mu <- a + bW*W + bS*S,
    a ~ dnorm( 0 , 0.2 ) ,
        bW ~ dnorm( 0 , 0.5 ) ,
        bS ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data = d )
precis(divorce_modS)
```

```
##                mean         sd        5.5%     94.5%
## a     -2.331003e-06 0.10911680 -0.17439205 0.1743874
## bW     5.244379e-02 0.16588020 -0.21266481 0.3175524
## bS     2.891714e-01 0.16594792  0.02395454 0.5543882
## sigma  9.206689e-01 0.09087258  0.77543701 1.0659009
```

## 6H2

```r
impliedConditionalIndependencies(divorce_dag)
```

```
## A _||_ W | S
## D _||_ S | A, M, W
## M _||_ W | S
```

```r
divorce_mod1 <- quap(
  alist(
    A ~ dnorm( mean=mu, sd=sigma ),
    mu <- a + bS*S + bW*W,
    a ~ dnorm( 0 , 0.2 ) ,
        c(bW, bS) ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data = d )
precis(divorce_mod1)
```

```
##                mean        sd       5.5%        94.5%
## a     -2.198131e-08 0.1113628 -0.1779792 0.1779791935
## bW     6.347922e-02 0.1700737 -0.2083314 0.3352897961
## bS    -2.717138e-01 0.1701365 -0.5436249 0.0001971974
## sigma  9.480115e-01 0.0935687  0.7984706 1.0975523374
```

```r
divorce_mod2 <- quap(
  alist(
    D ~ dnorm( mean=mu, sd=sigma ),
    mu <- a + bS*S + bA*A + bM*M + bW*W,
    a ~ dnorm( 0 , 0.2 ) ,
        c(bW, bS, bA, bM) ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data = d )
precis(divorce_mod2)
```

```
##                mean         sd        5.5%      94.5%
## a      1.244386e-10 0.09456290 -0.15112978  0.1511298
## bW     8.659403e-02 0.14050156 -0.13795460  0.3111426
## bS     1.451643e-01 0.14437791 -0.08557951  0.3759081
## bA    -5.512406e-01 0.15155108 -0.79344849 -0.3090327
## bM    -3.628275e-02 0.14737955 -0.27182373  0.1992582
## sigma  7.588397e-01 0.07520257  0.63865146  0.8790279
```

```r
divorce_mod3 <- quap(
  alist(
    M ~ dnorm( mean=mu, sd=sigma ),
    mu <- a + bW*W + bS*S ,
    a ~ dnorm( 0 , 0.2 ) ,
        c(bW, bS) ~ dnorm( 0 , 0.5 ),
        sigma ~ dexp( 1 )
    ) , data = d )
precis(divorce_mod3)
```

```
##                mean         sd       5.5%     94.5%
## a     -3.197606e-07 0.11363582 -0.1816123 0.1816117
## bW    -3.892094e-02 0.17424379 -0.3173962 0.2395543
## bS     1.006656e-01 0.17425222 -0.1778231 0.3791543
## sigma  9.764513e-01 0.09626031  0.8226087 1.1302938
```
## Question 1
1. Use a model to infer the total causal influence of area on weight. Would
increasing the area available to each fox make it heavier (healthier)? You
might want to standardize the variables. Regardless, use prior predictive
simulation to show that your model’s prior predictions stay within the pos-
sible outcome range.

```r
data(foxes)
f <- foxes

fox_dag <- dagitty( "dag{
                    area -> avgfood -> weight
                    avgfood -> groupsize -> weight
                    }")
coordinates( fox_dag ) <- list( x=c(area=0.5, avgfood=0, groupsize=1, weight=0.5),
                                  y=c(area=0, avgfood=0.5, groupsize=0.5, weight=1) )

plot(fox_dag)
```

![](Chapter6HW_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
adjustmentSets( fox_dag, exposure="area", outcome="weight" )
```

```
##  {}
```
There are no back-door paths from area to weight.


```r
f$W <- standardize(f$weight)
f$A <- standardize(f$area)

fox_mod <- quap(
  alist(
    W ~ dnorm( mean=mu, sd=sigma ),
    mu <- a + bA*A,
    a ~ dnorm( 0 , 0.2 ) ,
        bA ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data = f )
precis(fox_mod)
```

```
##                mean         sd       5.5%     94.5%
## a     -3.681273e-06 0.08360568 -0.1336217 0.1336143
## bA     1.883458e-02 0.09089201 -0.1264284 0.1640976
## sigma  9.912231e-01 0.06465949  0.8878847 1.0945614
```

```r
plot(coeftab(fox_mod))
```

![](Chapter6HW_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
# plot prior parameters
par(mfrow=c(3,1))
curve( dnorm( x, 0 , 0.2 ), from=-1, to=1, ylab="prior for a" )
curve( dnorm( x, 0 , 0.5 ), from=-2, to=2, ylab="prior for bA" )
curve( dexp( x, 1), from=-0.01, to=5, ylab="prior for sigma" )
```

![](Chapter6HW_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

```r
# prior predictive simulation
N <- 200
sim.a <- rnorm( N, 0, 0.2 ) 
sim.bA <- rnorm( N, 0, 0.5 )
sim.A <- seq( from=min(f$A) , to=max(f$A), length.out = N )
sim.mu <- sim.a + sim.bA * sim.A
sim.sigma <- rexp(N, 1) 
prior.W <- rnorm(N, sim.mu, sim.sigma)

par(mfrow=c(1,1))
plot( NULL , xlim=range(f$A) , ylim=c(-4,4) , xlab="area" , ylab="weight" )
for ( i in 1:N ) curve( sim.a[i] + sim.bA[i]*x ,
  from=min(f$A) , to=max(f$A) , add=TRUE ,
  col=col.alpha("black",0.2) )
```

![](Chapter6HW_files/figure-html/unnamed-chunk-6-3.png)<!-- -->

```r
dens( prior.W)
```

![](Chapter6HW_files/figure-html/unnamed-chunk-6-4.png)<!-- -->

## Question 2

```r
f$F <- standardize(f$avgfood)

fox_mod2 <- quap(
  alist(
    W ~ dnorm( mean=mu, sd=sigma ),
    mu <- a + bF*F,
    a ~ dnorm( 0 , 0.2 ) ,
        bF ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data = f )
precis(fox_mod2)
```

```
##                mean         sd       5.5%     94.5%
## a     -1.129047e-06 0.08360006 -0.1336102 0.1336079
## bF    -2.421799e-02 0.09088488 -0.1694696 0.1210336
## sigma  9.911424e-01 0.06465833  0.8878059 1.0944789
```

```r
plot(coeftab(fox_mod2))
```

![](Chapter6HW_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

## Question 3

```r
f$G <-  standardize(f$groupsize)
fox_mod3 <- quap(
  alist(
    W ~ dnorm( mean=mu, sd=sigma ),
    mu <- a + bF*F + bG*G,
    a ~ dnorm( 0 , 0.2 ) ,
        c( bF, bG ) ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data = f )
precis(fox_mod3)
```

```
##                mean         sd       5.5%      94.5%
## a      1.331851e-05 0.08013281 -0.1280544  0.1280810
## bF     4.771055e-01 0.17911369  0.1908472  0.7633638
## bG    -5.733853e-01 0.17913211 -0.8596730 -0.2870976
## sigma  9.419703e-01 0.06174051  0.8432970  1.0406435
```

```r
plot(coeftab(fox_mod3))
```

![](Chapter6HW_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

