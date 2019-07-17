---
title: "Chapter7.2"
author: "Esteban"
date: "July 10, 2019"
output: 
  html_document: 
    keep_md: yes
---



##7M1. Write down and compare the definitions of AIC, DIC, and WAIC. Which of these criteria is most general? Which assumptions are required to transform a more general criterion into a less general one?
> ###AIC  
$AIC = - 2lppd + 2p$  
Assumes:  
(1) Flat priors or priors overwhelmed by the likelihood.   
(2) Approximately multivariate Gaussian posterior distribution.  
(3) Sample size $N >> k$ number of parameters.  

> ###DIC
$DIC = ?$  
 * accomodates informative priors, but still assumes that the posterior is multivariate Gaussian and that sample size $N >> k$ parameters.  

> ###WAIC 
 $WAIC = -2(lppd - p_WAIC)$  
 $p_WAIC = \Sigma  var_\theta  log  p(y_i|\theta)$  
 * Most general  
 * Makes no assumption about the shape of the posterior.  
 * Provides an approximation of the out-of-sample deviance.  

##7M2. Explain the difference between model selection and model averaging. What information is lost under model selection? What information is lost under model averaging?

> (p. 221)   
* Model selection picks the single best performing model by some criterion and discards the others, whereas model averaging uses all the models and computes an average that is weighted by some criterion.   
* Selection discards the information about relative model accuracy contained in the differences among the LOOCV/LOOIS/WAIC values. This provides how confident we are about the models, conditional on what models we compare.  
* I would imagine averaging would wash the signal away, losing some predictive precision.  

##7M3. When comparing models with an information criterion, why must all models be fit to exactly the same observations? What would happen to the information criterion values, if the models were fit to different numbers of observations? Perform some experiments, if you are not sure.
> All models must be fit to the same observations because we must compare like with like, and information criteria are sensitive to different observation as well as different numbers of observations. Adding observations would increase deviance, since we are summing log probabilities.


##7M4. What happens to the effective number of parameters, as measured by DIC or WAIC, as a prior becomes more concentrated? Why? Perform some experiments, if you are not sure.
>The effective number of parameters will decrease because a concentrated prior in practice will reduce the influence of the less informative parameters in the model.

##7M5. Provide an informal explanation of why informative priors reduce overfitting.
> Informative priors can reduce the effect of the extreme, irregular parameter values and hone in on the regular features. Informative priors can be like a noise-canceling device for the model that is driven by feedforward instead of feedback.

##7M6. Provide an information explanation of why overly informative priors result in underfitting.
> Overly informative priors can overrun the actual data and thus lose information in the process. The data is given too small of a voice in the posterior, therefore the model can't actually learn from the data.
