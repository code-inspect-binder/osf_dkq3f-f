#### hurdle_gamma  ####
## hurdle_gamma  -> hu = logit (p0)
##                  b_ = log(mu gamma)

rm(list=ls()) 
graphics.off() 
library(rstudioapi) 
dirs<-rstudioapi::getSourceEditorContext()$path 
setwd(dirname(dirs))
library(brms)

load('eda_df.Rdata')

DF = eda_df

DF$emotion = relevel( DF$emotion , ref = 'angry' )


## fitted mu function
getCAT_fitted_mean = function(FIT , CAT_name , CAT_val , scale = 'response' ){
    newdata = FIT$data
    for( c in 1:length(CAT_name) ){
        newdata[,CAT_name[c]] = CAT_val[c]
    }
    PRED = fitted( FIT , newdata , re_formula = NA , summary = FALSE , scale = scale) 
    PRED
}


## trend sei
getCONT_fitted_trend = function(FIT , CONT_name ,  DELTA , CAT_name = NULL , CAT_val = NULL , scale = 'response' ){
    newdata = FIT$data
    for( c in 1:length(CAT_name) ){
        newdata[,CAT_name[c]] = CAT_val[c] 
    }
    PRED0 = fitted( FIT , newdata , re_formula = NA , summary = FALSE , scale = scale )
    newdata[,CONT_name] = newdata[,CONT_name] + DELTA 
    PRED1 = fitted( FIT , newdata , re_formula = NA , summary = FALSE , scale = scale)
    (PRED1 - PRED0) 
}

## FIT ####
## cda.scr = gamma, hu : if cda.scr = 0
formula_fit <- bf( cda.scr  ~ emotion*distance*sei_c + SUBJgender + AVATgender + (1|ID|Subject)
                  , hu  ~ emotion*distance*sei_c + SUBJgender + AVATgender + (1|ID|Subject)
                  , shape ~  (1|ID|Subject) )


FIT = brm( formula = formula_fit
           , data = DF
           , family = hurdle_gamma
           , prior = prior( normal(0,1) , class = "b" ) # prior specification 
           , cores = parallel::detectCores()
           , iter = 4000
           , warmup = 2000
           , file = 'eda_fit'
)


summary( FIT )
stancode(FIT)
prior_summary( FIT ) # infos priors

## plot MCMC chains
plot(FIT, trace = "cda.scr")
library(bayesplot)
pp_check(FIT, nreps =100)

stanplot(FIT, pars = c("b_") , prob_outer = 0.95)
stanplot(FIT, pars = c("^r_Subject") , prob_outer = 0.95)
stanplot(FIT, pars = c("cor_") , prob_outer = 0.95)
stanplot(FIT , pars = "b_emo" , prob_outer = 0.95 )

# plot fit model
# marginal_effects( FIT )

##compute marginal means and average marginal effects ####
mu_happy = rowMeans(getCAT_fitted_mean(FIT , 'emotion' , 'happy'  ))
mu_angry = rowMeans(getCAT_fitted_mean(FIT , 'emotion' , 'angry'  ))
mu_neutral = rowMeans(getCAT_fitted_mean(FIT , 'emotion' , 'neutral'  ))

contrast_mu_angry_neutral = mu_angry-mu_neutral
contrast_mu_happy_neutral = mu_happy-mu_neutral
contrast_mu_angry_happy = mu_angry - mu_happy

## MM: marginal mean
POST_MM = data.frame( mu_happy = mu_happy
                      , mu_angry = mu_angry
                      , mu_neutral = mu_neutral
)
## AME: average marginal effect
POST_AME = data.frame( contrast_mu_angry_neutral = contrast_mu_angry_neutral
                       , contrast_mu_happy_neutral = contrast_mu_happy_neutral
                       , contrast_mu_angry_happy = contrast_mu_angry_happy
)


## subj gender
mu_SUBJm = rowMeans(getCAT_fitted_mean(FIT , 'SUBJgender' , 'm'  ))
mu_SUBJf = rowMeans(getCAT_fitted_mean(FIT , 'SUBJgender' , 'f'  ))
contrast_mu_SUBJ_m_f = mu_SUBJm - mu_SUBJf

POST_MM$mu_SUBJm = mu_SUBJm
POST_MM$mu_SUBJf = mu_SUBJf
POST_AME$contrast_mu_SUBJ_m_f = contrast_mu_SUBJ_m_f
## avat gender
mu_AVATm = rowMeans(getCAT_fitted_mean(FIT , 'AVATgender' , 'm'  ))
mu_AVATf = rowMeans(getCAT_fitted_mean(FIT , 'AVATgender' , 'f'  ))
contrast_mu_AVAT_m_f = mu_AVATm - mu_AVATf

POST_MM$mu_AVATm = mu_AVATm
POST_MM$mu_AVATf = mu_AVATf
POST_AME$contrast_mu_AVAT_m_f = contrast_mu_AVAT_m_f


## distance
mu_m8 = rowMeans(getCAT_fitted_mean(FIT , 'distance' , '-8'  ))
mu_32 = rowMeans(getCAT_fitted_mean(FIT , 'distance' , '32'  ))
mu_72 = rowMeans(getCAT_fitted_mean(FIT , 'distance' , '72'  ))
contrast_mu_m8_32 = mu_m8 - mu_32
contrast_mu_32_72 = mu_32 - mu_72
contrast_mu_m8_72 = mu_m8 - mu_72

POST_MM$mu_m8 = mu_m8
POST_MM$mu_32 = mu_32
POST_MM$mu_72 = mu_72
POST_AME$contrast_mu_m8_32 = contrast_mu_m8_32
POST_AME$contrast_mu_32_72 = contrast_mu_32_72
POST_AME$contrast_mu_m8_72 = contrast_mu_m8_72


trend_sei = rowMeans(getCONT_fitted_trend( FIT , 'sei_c'  , 0.2 ))
POST_AME$trend_sei = trend_sei


## mu emo & contrastes -8cm
mu_happy_m8 = rowMeans(getCAT_fitted_mean(FIT , c('emotion','distance') , c('happy','-8')  ))
mu_angry_m8 = rowMeans(getCAT_fitted_mean(FIT , c('emotion','distance') , c('angry','-8')  ))
mu_neutral_m8 = rowMeans(getCAT_fitted_mean(FIT , c('emotion','distance') , c('neutral','-8')  ))
contrast_mu_angry_neutral_m8 = mu_angry_m8-mu_neutral_m8
contrast_mu_happy_neutral_m8 = mu_happy_m8-mu_neutral_m8
contrast_mu_angry_happy_m8 = mu_angry_m8 - mu_happy_m8
## cast to post MM & AME
POST_MM$mu_happy_m8 = mu_happy_m8
POST_MM$mu_angry_m8 = mu_angry_m8
POST_MM$mu_neutral_m8 = mu_neutral_m8
POST_AME$contrast_mu_angry_neutral_m8 = contrast_mu_angry_neutral_m8
POST_AME$contrast_mu_happy_neutral_m8 = contrast_mu_happy_neutral_m8
POST_AME$contrast_mu_angry_happy_m8 = contrast_mu_angry_happy_m8

## mu emo & contrasts 32cm
mu_happy_32 = rowMeans(getCAT_fitted_mean(FIT , c('emotion','distance') , c('happy','32')  ))
mu_angry_32 = rowMeans(getCAT_fitted_mean(FIT , c('emotion','distance') , c('angry','32')  ))
mu_neutral_32 = rowMeans(getCAT_fitted_mean(FIT , c('emotion','distance') , c('neutral','32')  ))
contrast_mu_angry_neutral_32 = mu_angry_32-mu_neutral_32
contrast_mu_happy_neutral_32 = mu_happy_32-mu_neutral_32
contrast_mu_angry_happy_32 = mu_angry_32 - mu_happy_32
## cast to post MM & AME
POST_MM$mu_happy_32 = mu_happy_32
POST_MM$mu_angry_32 = mu_angry_32
POST_MM$mu_neutral_32 = mu_neutral_32
POST_AME$contrast_mu_angry_neutral_32 = contrast_mu_angry_neutral_32
POST_AME$contrast_mu_happy_neutral_32 = contrast_mu_happy_neutral_32
POST_AME$contrast_mu_angry_happy_32 = contrast_mu_angry_happy_32


## mu & contrasts 72 cm
mu_happy_72 = rowMeans(getCAT_fitted_mean(FIT , c('emotion','distance') , c('happy','72')  ))
mu_angry_72 = rowMeans(getCAT_fitted_mean(FIT , c('emotion','distance') , c('angry','72')  ))
mu_neutral_72 = rowMeans(getCAT_fitted_mean(FIT , c('emotion','distance') , c('neutral','72')  ))
contrast_mu_angry_neutral_72 = mu_angry_72-mu_neutral_72
contrast_mu_happy_neutral_72 = mu_happy_72-mu_neutral_72
contrast_mu_angry_happy_72 = mu_angry_72 - mu_happy_72
## cast to post MM & AME
POST_MM$mu_happy_72 = mu_happy_72
POST_MM$mu_angry_72 = mu_angry_72
POST_MM$mu_neutral_72 = mu_neutral_72
POST_AME$contrast_mu_angry_neutral_72 = contrast_mu_angry_neutral_72
POST_AME$contrast_mu_happy_neutral_72 = contrast_mu_happy_neutral_72
POST_AME$contrast_mu_angry_happy_72 = contrast_mu_angry_happy_72

## contrasts per emo -8 - 32 cm
contrast_mu_angry_m8_32 = mu_angry_m8-mu_angry_32
contrast_mu_happy_m8_32 = mu_happy_m8-mu_happy_32
contrast_mu_neutral_m8_32 = mu_neutral_m8-mu_neutral_32
## cast to post AME
POST_AME$contrast_mu_angry_m8_32 = contrast_mu_angry_m8_32
POST_AME$contrast_mu_happy_m8_32 = contrast_mu_happy_m8_32
POST_AME$contrast_mu_neutral_m8_32 = contrast_mu_neutral_m8_32
## contrasts per emo -8 - 72 cm
contrast_mu_angry_m8_72 = mu_angry_m8-mu_angry_72
contrast_mu_happy_m8_72 = mu_happy_m8-mu_happy_72
contrast_mu_neutral_m8_72 = mu_neutral_m8-mu_neutral_72
## cast to post AME
POST_AME$contrast_mu_angry_m8_72 = contrast_mu_angry_m8_72
POST_AME$contrast_mu_happy_m8_72 = contrast_mu_happy_m8_72
POST_AME$contrast_mu_neutral_m8_72 = contrast_mu_neutral_m8_72
## contrasts per emo 32 - 72 cm
contrast_mu_angry_32_72 = mu_angry_32-mu_angry_72
contrast_mu_happy_32_72 = mu_happy_32-mu_happy_72
contrast_mu_neutral_32_72 = mu_neutral_32-mu_neutral_72
## cast to post AME
POST_AME$contrast_mu_angry_32_72 = contrast_mu_angry_32_72
POST_AME$contrast_mu_happy_32_72 = contrast_mu_happy_32_72
POST_AME$contrast_mu_neutral_32_72 = contrast_mu_neutral_32_72


## get sei per emotion then contrast emotions
trend_sei_happy = rowMeans(getCONT_fitted_trend( FIT , 'sei_c'  , 0.2 , 'emotion' , 'happy'))
trend_sei_angry = rowMeans(getCONT_fitted_trend( FIT , 'sei_c'  , 0.2 , 'emotion' , 'angry'))
trend_sei_neutral = rowMeans(getCONT_fitted_trend( FIT , 'sei_c'  , 0.2 , 'emotion' , 'neutral'))
contrast_trend_sei_angry_happy = trend_sei_angry-trend_sei_happy
contrast_trend_sei_angry_neutral = trend_sei_angry-trend_sei_neutral
contrast_trend_sei_happy_neutral = trend_sei_happy-trend_sei_neutral
## cast to AME
POST_AME$trend_sei_happy = trend_sei_happy
POST_AME$trend_sei_angry = trend_sei_angry
POST_AME$trend_sei_neutral = trend_sei_neutral
POST_AME$contrast_trend_sei_angry_happy = contrast_trend_sei_angry_happy
POST_AME$contrast_trend_sei_angry_neutral = contrast_trend_sei_angry_neutral
POST_AME$contrast_trend_sei_happy_neutral = contrast_trend_sei_happy_neutral
## get sei per distance then contrast distances
trend_sei_72 = rowMeans(getCONT_fitted_trend( FIT , 'sei_c'  , 0.2 , 'distance' , '72'))
trend_sei_m8 = rowMeans(getCONT_fitted_trend( FIT , 'sei_c'  , 0.2 , 'distance' , '-8'))
trend_sei_32 = rowMeans(getCONT_fitted_trend( FIT , 'sei_c'  , 0.2 , 'distance' , '32'))
contrast_trend_sei_m8_72 = trend_sei_m8-trend_sei_72
contrast_trend_sei_m8_32 = trend_sei_m8-trend_sei_32
contrast_trend_sei_72_32 = trend_sei_72-trend_sei_32
## cast to AME
POST_AME$trend_sei_72 = trend_sei_72
POST_AME$trend_sei_m8 = trend_sei_m8
POST_AME$trend_sei_32 = trend_sei_32
POST_AME$contrast_trend_sei_m8_72 = contrast_trend_sei_m8_72
POST_AME$contrast_trend_sei_m8_32 = contrast_trend_sei_m8_32
POST_AME$contrast_trend_sei_72_32 = contrast_trend_sei_72_32
## bayes plot #####
library( bayesplot )
mcmc_intervals( POST_MM , prob_outer = 0.95 )
mcmc_intervals( POST_AME , prob_outer = 0.95 )


## export csv ####
## df Marginal Mean with mu estimates
DF_MM = data.frame( estimate = parnames(POST_MM) )
DF_MM$mean = sapply(POST_MM, mean)
DF_MM$sd = sapply(POST_MM, sd)
DF_MM$'CI_2.5' = sapply(POST_MM, FUN = function(x){ sort(x)[length(x)*0.025] } )
DF_MM$'CI_97.5' = sapply(POST_MM, FUN = function(x){ sort(x)[length(x)*0.975] } )

# file <- paste0( substring( FIT$file , 1, nchar( FIT$file)-4 ), "_marginal_means_summary.csv")
# write.csv( DF_MM , file = file )

## df Average Marginal Effect with contrats estimates
DF_AME = data.frame( estimate = parnames(POST_AME) )
DF_AME$mean = sapply(POST_AME, mean)
DF_AME$sd = sapply(POST_AME, sd)
DF_AME$'CI_2.5' = sapply(POST_AME, FUN = function(x){ sort(x)[length(x)*0.025] } )
DF_AME$'CI_97.5' = sapply(POST_AME, FUN = function(x){ sort(x)[length(x)*0.975] } )
DF_AME$'P>0' = sapply(POST_AME, FUN = function(x){ mean( x > 0 ) } )
alpha = 0.025
DF_AME[, paste0('out_of_CI')] = DF_AME$'P>0' > (1-alpha) | DF_AME$'P>0' < alpha

# file <- paste0( substring( FIT$file , 1, nchar( FIT$file)-4 ), "_marginal_effects_summary.csv")
# write.csv( DF_AME , file = file )
