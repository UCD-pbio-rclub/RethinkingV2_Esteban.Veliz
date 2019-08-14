---
title: "Ch8HW"
author: "Esteban"
date: "July 26, 2019"
output: 
  html_document: 
    keep_md: yes
---



##8E1. For each of the causal relationships below, name a hypothetical third variable that would lead to an interaction effect.  
(1) Bread dough rises because of yeast.  
> * temperature, [sugar]   

(2) Education leads to higher income.   
> * family
* local environment (i.e., opportunities e.g. city vs. rural)  
* nutrition  
* choice of major  

(3) Gasoline makes a car go.  
> * spark plugs  

##8M3. In parts of North America, ravens depend upon wolves for their food. This is because ravens
are carnivorous but cannot usually kill or open carcasses of prey. Wolves however can and do kill
and tear open animals, and they tolerate ravens co-feeding at their kills. This species relationship
is generally described as a “species interaction.” Can you invent a hypothetical set of data on raven
population size in which this relationship would manifest as a statistical interaction? Do you think
the biological interaction could be linear? Why or why not?  


```r
# Number of samples
N <- 1000

# Prey
prey <- rnorm(N, mean=0, sd=1)

# Wolf
m.wolf <- 0.5*prey
wolf <- rnorm( N, mean=m.wolf, sd=1 )

# Raven
m.raven <- 0.1*prey + 0.3*wolf*prey 
raven <- rnorm( N, mean=m.raven, sd=1 )

# Make data frame
df.raven <- data.frame(raven, prey, wolf)
```


```r
# Raven-wolf-prey model
m8M3 <- quap(
  alist(
    raven ~ dnorm( mu, sigma ),
    mu <- a + bP*prey + bW*wolf + bPW*prey*wolf,
    a ~ dnorm( 0, 1 ),
    c( bP, bW, bPW ) ~ dnorm( 0, 1 ),
    sigma ~ dunif( 0, 3 )
  ),
  data = df.raven)

precis(m8M3)
```

```
##              mean         sd        5.5%       94.5%
## a      0.04375093 0.03480270 -0.01187051 0.099372377
## bP     0.10931949 0.03578352  0.05213051 0.166508470
## bW    -0.04730403 0.03299906 -0.10004290 0.005434833
## bPW    0.26128825 0.02544122  0.22062827 0.301948227
## sigma  1.01251999 0.02264095  0.97633538 1.048704603
```




##8H3. Consider again the data(rugged) data on economic development and terrain ruggedness,
examined in this chapter. One of the African countries in that example, Seychelles, is far outside
the cloud of other nations, being a rare country with both relatively high GDP and high ruggedness.
Seychelles is also unusual, in that it is a group of islands far from the coast of mainland Africa, and
its main economic activity is tourism.  

One might suspect that this one nation is exerting a strong influence on the conclusions. In
this problem, I want you to drop Seychelles from the data and re-evaluate the hypothesis that the
relationship of African economies with ruggedness is different from that on other continents.  

(a) Begin by using map to fit just the interaction model:  
$y_i \sim Normal(\mu_i, \sigma)$    
$\mu_i = \alpha + \beta_A A_i + \beta_R R_i + \beta_{AR} A_i R_i$

where y is log GDP per capita in the year 2000 (log of rgdppc_2000); A is cont_africa, the dummy
variable for being an African nation; and R is the variable rugged. Choose your own priors. Compare
the inference from this model fit to the data without Seychelles to the same model fit to the full data. Does it still seem like the effect of ruggedness depends upon continent? How much has the expected relationship changed? 


```r
## R code 8.1
data(rugged)
d <- rugged

# make log version of outcome
d$log_gdp <- log( d$rgdppc_2000 )

# extract countries with GDP data
dd <- d[ complete.cases(d$rgdppc_2000) , ]

# rescale variables
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)

# split countries into Africa and not-Africa
d.A1 <- dd[ dd$cont_africa==1 , ] # Africa
d.A0 <- dd[ dd$cont_africa==0 , ] # not Africa

########################
  # Sí Sey ------------#
m8H3 <- quap(
    alist(
        log_gdp_std ~ dnorm( mu , sigma ) ,
        mu <- a + bA*cont_africa +  bR*rugged_std + bAR*cont_africa*rugged_std ,
        a ~ dnorm( 1 , 0.1 ) ,
        c(bA, bR, bAR) ~ dnorm( 0 , 0.3 ) ,
        sigma ~ dexp( 1 )
    ) ,
    data=dd )
precis(m8H3, depth=2)
```

```
##             mean          sd       5.5%       94.5%
## a      1.0772648 0.015681604  1.0522026  1.10232706
## bA    -0.2191388 0.025493197 -0.2598819 -0.17839575
## bR    -0.1275420 0.053526310 -0.2130873 -0.04199658
## bAR    0.2475690 0.089589613  0.1043875  0.39075051
## sigma  0.1095439 0.005942651  0.1000464  0.11904144
```

```r
  # No Sey ------------#
dd.noSey <- dd %>% subset( country!="Seychelles" )
# dd.noSey <- dd[ dd$country!="Seychelles", ]

m8H3.noSey <- quap(
    alist(
        log_gdp_std ~ dnorm( mu , sigma ) ,
        mu <- a + bA*cont_africa +  bR*rugged_std + bAR*cont_africa*rugged_std ,
        a ~ dnorm( 1 , 0.1 ) ,
        c(bA, bR, bAR) ~ dnorm( 0 , 0.3 ) ,
        sigma ~ dexp( 1 )
    ) ,
    data=dd.noSey )
precis(m8H3.noSey, depth=2)
```

```
##             mean          sd        5.5%       94.5%
## a      1.0778406 0.015563568  1.05296699  1.10271416
## bA    -0.2125369 0.025516182 -0.25331673 -0.17175716
## bR    -0.1299655 0.053131459 -0.21487979 -0.04505112
## bAR    0.1819514 0.094776579  0.03048013  0.33342269
## sigma  0.1086620 0.005910078  0.09921657  0.11810746
```
> There is less of an interaction effect, indicated by the slope bAR going from 0.25 to 0.18.  

(b) Now plot the predictions of the interaction model, with and without Seychelles. Does it still
seem like the effect of ruggedness depends upon continent? How much has the expected relationship
changed?  


```r
par(mfrow = c(2, 2))
####------ With Seychelles -------###
d.A1.noSey <- subset(d.A1, country!="Seychelles")

## R code 8.17
# plot Africa - cont_africa=1
plot( d.A1$rugged_std , d.A1$log_gdp_std , pch=16 , col=rangi2 ,
    xlab="ruggedness (standardized)" , ylab="log GDP (as proportion of mean)" ,
    xlim=c(0,1) )

rugged_seq <- seq( from=-0.1 , to=1.1 , length.out=30 )
mu <- link( m8H3 , data=data.frame( cont_africa=1 , rugged_std=rugged_seq ) )
mu_mean <- apply( mu , 2 , mean )
mu_ci <- apply( mu , 2 , PI , prob=0.97 )

lines( rugged_seq , mu_mean , lwd=2 )
shade( mu_ci , rugged_seq , col=col.alpha(rangi2,0.3) )
mtext("African nations")

# plot non-Africa - cont_africa=0
plot( d.A0$rugged_std , d.A0$log_gdp_std , pch=1 , col="black" ,
    xlab="ruggedness (standardized)" , ylab="log GDP (as proportion of mean)" ,
    xlim=c(0,1) )
mu <- link( m8H3 , data=data.frame( cont_africa=0 , rugged_std=rugged_seq ) )
mu_mean <- apply( mu , 2 , mean )
mu_ci <- apply( mu , 2 , PI , prob=0.97 )
lines( rugged_seq , mu_mean , lwd=2 )
shade( mu_ci , rugged_seq )
mtext("Non-African nations")


###--------- Without Seychelles ------###
d.A1.noSey <- subset( d.A1, country!="Seychelles" )

## R code 8.17
# plot Africa - cont_africa=1
plot( d.A1.noSey$rugged_std , d.A1.noSey$log_gdp_std , pch=16 , col=rangi2 ,
    xlab="ruggedness (standardized)" , ylab="log GDP (as proportion of mean)" ,
    xlim=c(0,1) )

rugged.seq <- seq( from=-0.1 , to=1.1 , length.out=30 )
mu <- link( m8H3.noSey , data=data.frame( cont_africa=1 , rugged_std=rugged_seq ) )
mu_mean <- apply( mu , 2 , mean )
mu_ci <- apply( mu , 2 , PI , prob=0.97 )

lines( rugged_seq , mu_mean , lwd=2 )
shade( mu_ci , rugged_seq , col=col.alpha(rangi2,0.3) )
mtext("African nations minus Seychelles")

# plot non-Africa - cont_africa=0
plot( d.A0$rugged_std , d.A0$log_gdp_std , pch=1 , col="black" ,
    xlab="ruggedness (standardized)" , ylab="log GDP (as proportion of mean)" ,
    xlim=c(0,1) )
mu <- link( m8H3.noSey , data=data.frame( cont_africa=0 , rugged_std=rugged_seq ) )
mu_mean <- apply( mu , 2 , mean )
mu_ci <- apply( mu , 2 , PI , prob=0.97 )
lines( rugged_seq , mu_mean , lwd=2 )
shade( mu_ci , rugged_seq )
mtext("Non-African nations (minus Seychelles in model)")
```

![](Ch8HW_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

> The slope for African nations' gdp v. ruggedness has decreased.  


(c) Finally, conduct a model comparison analysis, using WAIC. Fit three models to the data without Seychelles:  
Model 1 : $y_i \sim Normal(\mu_i, \sigma)$  
$\mu_i = \alpha + \beta_R R_i$  
Model 2 : $y_i \sim Normal(\mu_i, \sigma)$  
$\mu_i = \alpha + \beta_A A_i + \beta_R R_i$  
Model 3 : $y_i \sim Normal(\mu_i, \sigma)$  
$\mu_i = \alpha + \beta_A A_i + \beta_R R_i + \beta_{AR} A_i R_i$  
Use whatever priors you think are sensible. Plot the model-averaged predictions of this model set.
Do your inferences differ from those in (b)? Why or why not?

```r
m8H3.C1 <- quap(
    alist(
        log_gdp_std ~ dnorm( mu , sigma ) ,
        mu <- a + bR*rugged_std,
        a ~ dnorm( 1 , 0.1 ) ,
        bR ~ dnorm( 0, 0.3 ) ,
        sigma ~ dexp( 1 )
    ) ,
    data=dd.noSey )

m8H3.C2 <- quap(
    alist(
        log_gdp_std ~ dnorm( mu , sigma ) ,
        mu <- a + bA*cont_africa +  bR*rugged_std,
        a ~ dnorm( 1 , 0.1 ) ,
        c(bA, bR) ~ dnorm( 0 , 0.3 ) ,
        sigma ~ dexp( 1 )
    ) ,
    data=dd.noSey )

m8H3.C3 <- quap(
    alist(
        log_gdp_std ~ dnorm( mu , sigma ) ,
        mu <- a + bA*cont_africa +  bR*rugged_std + bAR*cont_africa*rugged_std ,
        a ~ dnorm( 1 , 0.1 ) ,
        c(bA, bR, bAR) ~ dnorm( 0 , 0.3 ) ,
        sigma ~ dexp( 1 )
    ) ,
    data=dd.noSey )

compare(m8H3.C1, m8H3.C2, m8H3.C3)
```

```
##              WAIC    pWAIC     dWAIC       weight       SE      dSE
## m8H3.C3 -260.9233 4.488683  0.000000 8.211202e-01 15.11646       NA
## m8H3.C2 -257.8754 3.987058  3.047911 1.788798e-01 14.42363  3.30031
## m8H3.C1 -187.7830 2.712010 73.140275 1.076956e-16 13.41949 15.59175
```
> The model with A and R is equivalent to the interaction model.


##Plus, use the tomato.csv (attached) data set and evaluate whether hypocotyl length ("hyp") is affected by shade ("trt"), species ("species") and their interaction.

```r
tomato <- read.csv("Tomato.csv")
tomato <- subset(tomato, select = c(hyp, trt, species))

tomato$h <- tomato$hyp / max(tomato$hyp)
tomato$s <- tomato$species %>% as.numeric()
tomato$t <- tomato$trt %>% as.numeric() - 1

head(tomato)
```

```
##     hyp trt         species         h s t
## 1 19.46   H    S. pennellii 0.2608579 4 0
## 2 31.28   H   S. peruvianum 0.4193029 5 0
## 3 56.65   H   S. peruvianum 0.7593834 5 0
## 4 35.18   H     S. chilense 0.4715818 1 0
## 5 35.32   H     S. chilense 0.4734584 1 0
## 6 28.74   H S. chmielewskii 0.3852547 2 0
```

```r
str(tomato)
```

```
## 'data.frame':	1008 obs. of  6 variables:
##  $ hyp    : num  19.5 31.3 56.6 35.2 35.3 ...
##  $ trt    : Factor w/ 2 levels "H","L": 1 1 1 1 1 1 1 1 1 1 ...
##  $ species: Factor w/ 5 levels "S. chilense",..: 4 5 5 1 1 2 3 4 5 5 ...
##  $ h      : num  0.261 0.419 0.759 0.472 0.473 ...
##  $ s      : num  4 5 5 1 1 2 3 4 5 5 ...
##  $ t      : num  0 0 0 0 0 0 0 0 0 0 ...
```


```r
mtomato <- quap(
    alist(
        h ~ dnorm( mu , sigma ) ,
        mu <- a[s] + b*t ,
        a[s] ~ dnorm( 0 , 1 ) ,
        b ~ dnorm( 0 , 1 ) ,
        sigma ~ dexp( 1 )
    ) ,
    data=tomato )


mtomato.int <- quap(
    alist(
        h ~ dnorm( mu , sigma ) ,
        mu <- a[s] + b[s]*t ,
        a[s] ~ dnorm( 0 , 1 ) ,
        b[s] ~ dnorm( 0 , 1 ) ,
        sigma ~ dexp( 1 )
    ) ,
    data=tomato )

precis(mtomato, depth=2)
```

```
##             mean          sd       5.5%      94.5%
## a[1]  0.42970908 0.009248681 0.41492790 0.44449026
## a[2]  0.39307161 0.008956938 0.37875670 0.40738653
## a[3]  0.38278168 0.008813543 0.36869593 0.39686742
## a[4]  0.34679690 0.011312659 0.32871709 0.36487672
## a[5]  0.47937907 0.009100202 0.46483519 0.49392295
## b     0.07131956 0.007623059 0.05913644 0.08350268
## sigma 0.12088346 0.002691110 0.11658254 0.12518437
```

```r
precis(mtomato.int, depth=2)
```

```
##             mean          sd       5.5%      94.5%
## a[1]  0.42257976 0.011878896 0.40359499 0.44156453
## a[2]  0.40976986 0.011491239 0.39140464 0.42813508
## a[3]  0.39488117 0.010997942 0.37730434 0.41245801
## a[4]  0.31591192 0.015486656 0.29116125 0.34066258
## a[5]  0.47291017 0.011708015 0.45419850 0.49162184
## b[1]  0.08536664 0.016678305 0.05871148 0.11202179
## b[2]  0.03905774 0.015970380 0.01353399 0.06458149
## b[3]  0.04575529 0.015983003 0.02021136 0.07129921
## b[4]  0.12793771 0.020968078 0.09442667 0.16144875
## b[5]  0.08385067 0.016296360 0.05780594 0.10989540
## sigma 0.11998790 0.002671408 0.11571847 0.12425732
```

```r
compare(mtomato, mtomato.int)
```

```
##                  WAIC     pWAIC    dWAIC     weight       SE      dSE
## mtomato.int -1390.673 12.030182 0.000000 0.97030382 58.33124       NA
## mtomato     -1383.700  7.952946 6.973182 0.02969618 57.78985 7.133239
```

###=====================================   
###******** SECOND PART  ***************   
###=====================================    

7M1, 7M2
7H1, 7H2, 7H4

##7M1. Recall the tulips example from the chapter. Suppose another set of treatments adjusted the temperature in the greenhouse over two levels: cold and hot. The data in the chapter were collected at the cold temperature. You find none of the plants grown under the hot temperature developed any blooms at all, regardless of the water and shade levels. Can you explain this result in terms of interactions between water, shade, and temperature?  
> Light and water produce blooms provided temperature doesn't rise above a threshold. 


##7M2. Can you invent a regression equation that would make the bloom size zero, whenever the temperature is hot?  
$\mu_i = \gamma_{h/c}[ \alpha + \beta_w w_i + \beta_s s_i + (\beta_w + \beta_s) w_i s_i]$  
$\gamma_h = 0$  
$\gamma_c = 1$
 

##7H1. Return to the data(tulips) example in the chapter. Now include the bed variable as a predictor in the interaction model. Don’t interact bed with the other predictors; just include it as a main effect. Note that bed is categorical. So to use it properly, you will need to either construct dummy variables or rather an index variable, as explained in Chapter ??.

```r
library(rethinking)
data(tulips)
d <- tulips
str(d)
```

```
## 'data.frame':	27 obs. of  4 variables:
##  $ bed   : Factor w/ 3 levels "a","b","c": 1 1 1 1 1 1 1 1 1 2 ...
##  $ water : int  1 1 1 2 2 2 3 3 3 1 ...
##  $ shade : int  1 2 3 1 2 3 1 2 3 1 ...
##  $ blooms: num  0 0 111 183.5 59.2 ...
```

```r
d$blooms_std <- d$blooms / max(d$blooms)
d$water_cent <- d$water - mean(d$water)
d$shade_cent <- d$shade - mean(d$shade)

summary(d)
```

```
##  bed       water       shade       blooms         blooms_std    
##  a:9   Min.   :1   Min.   :1   Min.   :  0.00   Min.   :0.0000  
##  b:9   1st Qu.:1   1st Qu.:1   1st Qu.: 71.11   1st Qu.:0.1966  
##  c:9   Median :2   Median :2   Median :111.04   Median :0.3070  
##        Mean   :2   Mean   :2   Mean   :128.99   Mean   :0.3567  
##        3rd Qu.:3   3rd Qu.:3   3rd Qu.:190.30   3rd Qu.:0.5262  
##        Max.   :3   Max.   :3   Max.   :361.66   Max.   :1.0000  
##    water_cent   shade_cent
##  Min.   :-1   Min.   :-1  
##  1st Qu.:-1   1st Qu.:-1  
##  Median : 0   Median : 0  
##  Mean   : 0   Mean   : 0  
##  3rd Qu.: 1   3rd Qu.: 1  
##  Max.   : 1   Max.   : 1
```

```r
apply( d[ -1 ], 2, sd ) %>% round(2)
```

```
##      water      shade     blooms blooms_std water_cent shade_cent 
##       0.83       0.83      92.68       0.26       0.83       0.83
```

```r
## Modified R code 8.24
m.tulip <- quap(
    alist(
        blooms_std ~ dnorm( mu , sigma ) ,
        mu <- a[bed] + bw*water_cent + bs*shade_cent + bws*water_cent*shade_cent ,
        a[bed] ~ dnorm( 0.5 , 0.25 ) ,
        bw ~ dnorm( 0 , 0.25 ) ,
        bs ~ dnorm( 0 , 0.25 ) ,
        bws ~ dnorm( 0 , 0.25 ) ,
        sigma ~ dexp( 1 )
    ) ,
    data=d )

precis(m.tulip, depth=2)
```

```
##             mean         sd       5.5%       94.5%
## a[1]   0.2732623 0.03571264  0.2161866  0.33033803
## a[2]   0.3964035 0.03569523  0.3393557  0.45345142
## a[3]   0.4091094 0.03569414  0.3520633  0.46615557
## bw     0.2074518 0.02537319  0.1669006  0.24800307
## bs    -0.1138480 0.02536865 -0.1543920 -0.07330399
## bws   -0.1438799 0.03099389 -0.1934141 -0.09434569
## sigma  0.1081799 0.01469191  0.0846994  0.13166041
```

```r
## Is creating an ID necessary? -- NO. Factor levels already incorporated in original variable.
#
# d$bed_id <- as.numeric(d$bed)
# m.tulipID <- quap(
#     alist(
#         blooms_std ~ dnorm( mu , sigma ) ,
#         mu <- a[bed_id] + bw*water_cent + bs*shade_cent + bws*water_cent*shade_cent ,
#         a[bed_id] ~ dnorm( 0.5 , 0.25 ) ,
#         bw ~ dnorm( 0 , 0.25 ) ,
#         bs ~ dnorm( 0 , 0.25 ) ,
#         bws ~ dnorm( 0 , 0.25 ) ,
#         sigma ~ dexp( 1 )
#     ) ,
#     data=d )
# 
# precis(m.tulipID, depth=2)
```


##7H2. Use WAIC to compare the model from 7H1 to a model that omits bed. What do you infer from this comparison? Can you reconcile the WAIC results with the posterior distribution of the bed coefficients?

```r
## R code 8.24
m.tulip.OG <- quap(
    alist(
        blooms_std ~ dnorm( mu , sigma ) ,
        mu <- a + bw*water_cent + bs*shade_cent + bws*water_cent*shade_cent ,
        a ~ dnorm( 0.5 , 0.25 ) ,
        bw ~ dnorm( 0 , 0.25 ) ,
        bs ~ dnorm( 0 , 0.25 ) ,
        bws ~ dnorm( 0 , 0.25 ) ,
        sigma ~ dexp( 1 )
    ) ,
    data=d )

plot( coeftab( m.tulip, m.tulip.OG ) )
```

![](Ch8HW_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
compare( m.tulip, m.tulip.OG )
```

```
##                 WAIC    pWAIC    dWAIC    weight        SE      dSE
## m.tulip    -23.89298 9.490916 0.000000 0.7497979  9.791993       NA
## m.tulip.OG -21.69791 6.787611 2.195069 0.2502021 10.676781 7.670055
```
> The model that includes bed has a slightly lower WAIC score, although given that it is smaller than the dSE, the WAIC scores are effectively the same. Bed A seems to have overall lower blooms on average than the other two beds, which are similar to each other. When we condition on bed, giving each its own intercept, we include information which contributes to our predictive power. The other model takes an average of the intercepts for each bed. The other model expects tulip in bed A to yield less blooms on average, holding all other variables at their mean. It expects slightly more in beds B and C.   

##7H4. The values in data(nettle) are data on language diversity in 74 nations. The meaning of each column is given below.  
(1) country: Name of the country
(2) num.lang: Number of recognized languages spoken
(3) area: Area in square kilometers
(4) k.pop: Population, in thousands
(5) num.stations: Number of weather stations that provided data for the next two columns
(6) mean.growing.season: Average length of growing season, in months
(7) sd.growing.season: Standard deviation of length of growing season, in months

Use these data to evaluate the hypothesis that language diversity is partly a product of food security. The notion is that, in productive ecologies, people don’t need large social networks to buffer them against risk of food shortfalls. This means ethnic groups can be smaller and more self-sufficient, leading to more languages per capita. In contrast, in a poor ecology, there is more subsistence risk, and so human societies have adapted by building larger networks of mutual obligation to provide food insurance. This in turn creates social forces that help prevent languages from diversifying. Specifically, you will try to model the number of languages per capita as the outcome variable:

Use the logarithm of this new variable as your regression outcome. (A count model would be better
here, but you’ll learn those later, in Chapter 11.) This problem is open ended, allowing you to decide how you address the hypotheses and the uncertain advice the modeling provides. If you think you need to use WAIC anyplace, please do. If you think you need certain priors, argue for them. If you think you need to plot predictions in a certain way, please do. Just try to honestly evaluate the main effects of both mean.growing.season and sd.growing.season, as well as their two-way interaction, as outlined in parts (a), (b), and (c) below. If you are not sure which approach to use, try several.  


```r
data(nettle)
d <- nettle 

# R code 8.27
d$lang.per.cap <- d$num.lang / d$k.pop

# log transform lang per cap and area
d$log.lang.pcap <- log( d$lang.per.cap )
d$log.area <- log( d$area )

# center variables
d$log.lang.cent <- d$log.lang.pcap - mean( d$log.lang.pcap )
d$mean.growing.cent <- d$mean.growing.season - mean( d$mean.growing.season )
d$log.area.cent <- d$log.area - mean( d$log.area )
d$sd.growing.cent <- d$sd.growing.season - mean( d$sd.growing.season )


# mean, sd, and range
summary(d)
```

```
##        country      num.lang           area             k.pop       
##  Algeria   : 1   Min.   :  1.00   Min.   :  12189   Min.   :   102  
##  Angola    : 1   1st Qu.: 17.25   1st Qu.: 167708   1st Qu.:  3829  
##  Australia : 1   Median : 40.00   Median : 434796   Median :  9487  
##  Bangladesh: 1   Mean   : 89.73   Mean   : 880698   Mean   : 33574  
##  Benin     : 1   3rd Qu.: 93.75   3rd Qu.:1080316   3rd Qu.: 24745  
##  Bolivia   : 1   Max.   :862.00   Max.   :8511965   Max.   :849638  
##  (Other)   :68                                                      
##   num.stations    mean.growing.season sd.growing.season
##  Min.   :  1.00   Min.   : 0.000      Min.   :0.0000   
##  1st Qu.: 10.00   1st Qu.: 5.348      1st Qu.:0.9375   
##  Median : 20.50   Median : 7.355      Median :1.6900   
##  Mean   : 37.91   Mean   : 7.041      Mean   :1.6992   
##  3rd Qu.: 44.75   3rd Qu.: 9.283      3rd Qu.:2.1075   
##  Max.   :272.00   Max.   :12.000      Max.   :5.8700   
##                                                        
##   lang.per.cap       log.lang.pcap        log.area      log.lang.cent     
##  Min.   :0.0000931   Min.   :-9.2814   Min.   : 9.408   Min.   :-3.82475  
##  1st Qu.:0.0019901   1st Qu.:-6.2196   1st Qu.:12.029   1st Qu.:-0.76302  
##  Median :0.0041066   Median :-5.4952   Median :12.981   Median :-0.03858  
##  Mean   :0.0206464   Mean   :-5.4566   Mean   :12.935   Mean   : 0.00000  
##  3rd Qu.:0.0100059   3rd Qu.:-4.6111   3rd Qu.:13.892   3rd Qu.: 0.84547  
##  Max.   :0.6809816   Max.   :-0.3842   Max.   :15.957   Max.   : 5.07239  
##                                                                           
##  mean.growing.cent log.area.cent      sd.growing.cent    
##  Min.   :-7.0415   Min.   :-3.52662   Min.   :-1.699189  
##  1st Qu.:-1.6940   1st Qu.:-0.90595   1st Qu.:-0.761689  
##  Median : 0.3135   Median : 0.04564   Median :-0.009189  
##  Mean   : 0.0000   Mean   : 0.00000   Mean   : 0.000000  
##  3rd Qu.: 2.2410   3rd Qu.: 0.95742   3rd Qu.: 0.408311  
##  Max.   : 4.9585   Max.   : 3.02207   Max.   : 4.170811  
## 
```

```r
apply( d[-1], 2, sd ) %>% round(2)
```

```
##            num.lang                area               k.pop 
##              144.16          1399199.16           102226.25 
##        num.stations mean.growing.season   sd.growing.season 
##               50.88                3.14                1.07 
##        lang.per.cap       log.lang.pcap            log.area 
##                0.08                1.52                1.27 
##       log.lang.cent   mean.growing.cent       log.area.cent 
##                1.52                3.14                1.27 
##     sd.growing.cent 
##                1.07
```

```r
# apply( d[-1], 2, range ) %>% diff()
```


(a) Evaluate the hypothesis that language diversity, as measured by log(lang.per.cap), is positively associated with the average length of the growing season, mean.growing.season. Consider log(area) in your regression(s) as a covariate (not an interaction). Interpret your results.  


```r
# qplot(mean.growing.season, log.lang.pcap, data=d)
qplot(mean.growing.cent, log.lang.cent, data=d)
```

![](Ch8HW_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
qplot(sd.growing.cent, log.lang.cent, data=d)
```

![](Ch8HW_files/figure-html/unnamed-chunk-12-2.png)<!-- -->



```r
m.A <- quap(
    alist(
        log.lang.cent ~ dnorm( mu , sigma ) ,
        mu <- a + bG*mean.growing.cent,
        a ~ dnorm( 0 , 0.85 ) ,
        bG ~ dnorm( 0 , 0.4 ) ,
        sigma ~ dexp( 1 )
    ) ,
    data=d )

## Checck prior
prior <- extract.prior( m.A )

# set up the plot dimensions
plot( NULL , xlim=c( -8, 5.5 ) , ylim=c( -4, 6 ) ,
    xlab="mean growing season" , ylab="log language per capita, centered" )
abline( h=min(d$log.lang.cent) , lty=2 )
abline( h=max(d$log.lang.cent) , lty=2 )

# draw 50 lines from the prior
plot_seq <- seq( from=-8.5 , to=6 , length.out=100 )
mu <- link( m.A , post=prior , data=data.frame(mean.growing.cent=plot_seq) )
for ( i in 1:100 ) lines( plot_seq , mu[i,] , col=col.alpha("black",0.3) )
```

![](Ch8HW_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


```r
m.A_area <- quap(
    alist(
        log.lang.cent ~ dnorm( mu , sigma ) ,
        mu <- a + bG*mean.growing.cent + bA*log.area.cent,
        a ~ dnorm( 0 , 0.85 ) ,
        c(bG, bA) ~  dnorm( 0 , 0.4 ) ,
        sigma ~ dexp( 1 )
    ) ,
    data=d )

plot( coeftab( m.A, m.A_area ) ) 
```

![](Ch8HW_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
compare( m.A, m.A_area )
```

```
##              WAIC    pWAIC      dWAIC   weight       SE      dSE
## m.A      267.8916 3.691896 0.00000000 0.507703 15.51259       NA
## m.A_area 267.9532 4.898465 0.06162925 0.492297 16.13365 3.415406
```
> In both models, there is a slight positive association between growing season and language diversity. Area does not contribute much to WAIC in comparison with the simple model, but when included is slightly negatively associated with language diversity.

(b) Now evaluate the hypothesis that language diversity is negatively associated with the standard deviation of length of growing season, sd.growing.season. This hypothesis follows from uncertainty in harvest favoring social insurance through larger social networks and therefore fewer languages. Again, consider log(area) as a covariate (not an interaction). Interpret your results.  

```r
m.B <- quap(
    alist(
        log.lang.cent ~ dnorm( mu , sigma ) ,
        mu <- a + bG*mean.growing.cent + bS*sd.growing.cent, 
        a ~ dnorm( 0 , 0.85 ) ,
        c( bG, bS ) ~  dnorm( 0 , 0.4 ) ,
        sigma ~ dexp( 1 )
    ) ,
    data=d )

m.B_area <- quap(
    alist(
        log.lang.cent ~ dnorm( mu , sigma ) ,
        mu <- a + bG*mean.growing.cent + bS*sd.growing.cent + bA*log.area.cent,
        a ~ dnorm( 0 , 0.85 ) ,
        c( bG, bS, bA ) ~  dnorm( 0 , 0.4 ) ,
        sigma ~ dexp( 1 )
    ) ,
    data=d )

plot( coeftab( m.A, m.A_area, m.B, m.B_area ), cex=0.7 )
```

![](Ch8HW_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

```r
compare( m.A, m.A_area, m.B, m.B_area )
```

```
##              WAIC    pWAIC    dWAIC     weight       SE      dSE
## m.B      264.1193 4.769037 0.000000 0.67114459 15.92878       NA
## m.B_area 266.8471 6.194427 2.727893 0.17157803 16.28879 0.909272
## m.A_area 268.3465 5.049188 4.227278 0.08107265 16.13542 3.374433
## m.A      268.4704 3.999235 4.351122 0.07620472 15.64187 4.546683
```
> There is a stronger negative assocaition between language diversity and the sd of the growing season.

(c) Finally, evaluate the hypothesis that mean.growing.season and sd.growing.season interact to synergistically reduce language diversity. The idea is that, in nations with longer average growing seasons, high variance makes storage and redistribution even more important than it would be otherwise. That way, people can cooperate to preserve and protect windfalls to be used during the droughts. These forces in turn may lead to greater social integration and fewer languages.]

```r
m.C <- quap(
    alist(
        log.lang.cent ~ dnorm( mu , sigma ) ,
        mu <- a + bG*mean.growing.cent + bS*sd.growing.cent + 
          bGS*mean.growing.cent*sd.growing.cent, 
        a ~ dnorm( 0 , 0.85 ) ,
        c( bG, bS, bGS ) ~  dnorm( 0 , 0.4 ) ,
        sigma ~ dexp( 1 )
    ) ,
    data=d )

m.C_area <- quap(
    alist(
        log.lang.cent ~ dnorm( mu , sigma ) ,
        mu <- a + bG*mean.growing.cent + bS*sd.growing.cent + 
          bA*log.area.cent + bGS*mean.growing.cent*sd.growing.cent,
        a ~ dnorm( 0 , 0.85 ) ,
        c( bG, bS, bA, bGS ) ~  dnorm( 0 , 0.4 ) ,
        sigma ~ dexp( 1 )
    ) ,
    data=d )

plot( coeftab( m.A, m.A_area, m.B, m.B_area, m.C, m.C_area ), cex=0.7 )
```

![](Ch8HW_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

```r
compare( m.A, m.A_area, m.B, m.B_area, m.C, m.C_area )
```

```
##              WAIC    pWAIC    dWAIC     weight       SE       dSE
## m.C      260.8223 5.586585 0.000000 0.58384185 16.16609        NA
## m.C_area 262.9000 6.610388 2.077695 0.20659963 16.54950 0.7195513
## m.B      263.7725 4.597807 2.950184 0.13355828 15.72076 4.8323214
## m.B_area 266.1339 5.822696 5.311611 0.04101036 16.16000 4.9372532
## m.A      267.5528 3.524758 6.730452 0.02017419 15.48677 6.5273171
## m.A_area 268.1702 4.978804 7.347887 0.01481569 16.20521 5.6332110
```
> The interaction model has the best WAIC score, dSE above the other models. Although growing season still has a positive interaction, both sd of growing season and the interaction with growing season have a negative impact on language diversity. [Can we say anything about sigma being smaller in this model as we do in ANOVA--that is does sigma decrease as our model encompasses more predictor variables?]
