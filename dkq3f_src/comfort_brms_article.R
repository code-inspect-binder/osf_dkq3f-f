#### FIT RESP EMOx DIST ####
rm(list=ls())
graphics.off()
library(rstudioapi) 
dirs<-rstudioapi::getSourceEditorContext()$path # ac
setwd(dirname(dirs))
library(brms)

load('comfort_df_article.Rdata')
DF = comfort_df_article

DF$emotion = relevel( DF$emotion , ref = 'neutral' )


#### FIT ####
## fit for population ###
FIT = brm( formula = bf(Nyes | trials(Ntrial)  ~ emotion*sei_c*distance_m*AVATgender*SUBJgender+ (emotion*sei_c*distance_m|ID|Subject))
           , data = DF
           , family = binomial("logit")
           , prior = c( prior( "normal(0,1)" , class = "b" )
                        , prior( "student_t(3,0,10)" , class = "b" , coef = "distance_m") )
           , cores = parallel::detectCores()
           , iter = 4000
           , warmup = 2000
           , file = 'comfort_fit_pop'
)
## fit for subject ###
FITsubj = brm( formula = bf(Nyes | trials(Ntrial)  ~ emotion*sei_c*distance_m+ (emotion*sei_c*distance_m|ID|Subject))
           , data = DF
           , family = binomial("logit")
           , prior = c( prior( "normal(0,1)" , class = "b" )
                        , prior( "student_t(3,0,10)" , class = "b" , coef = "distance_m") )
           , cores = parallel::detectCores()
           , iter = 4000
           , warmup = 2000
           , file = 'comfort_fit'
)

summary(FIT)

bayes_R2(FIT)
bayes_R2(FITsubj)

####stanplot ####
stanplot( FIT, pars = c("b_") , prob_outer = 0.95) # b_ = fixed effects
stanplot( FIT, pars = c("^r_sub") , prob_outer = 0.95) # random
stanplot( FIT, pars = c("cor") , prob_outer = 0.95) # correlations

#### CI 2.5 & 97.5
DFtmp = DF
DFtmp$Ntrial = 1
fitted_yesProp = fitted( FIT , newdata = DFtmp )
DF$fitted_yesProp = fitted_yesProp[,1]
DF$fitted_cilow_yesProp = fitted_yesProp[,3]
DF$fitted_cihigh_yesProp = fitted_yesProp[,4]



#### ggplot individual ####
library(ggplot2)
ggplot( DF , aes( distance_m , yesProp , col = emotion) ) +
    geom_point( position = position_jitter( width = 0.05 , height = 0 )) +
    geom_line( aes(distance_m , fitted_yesProp)) +
    geom_ribbon( aes( ymin = fitted_cilow_yesProp , ymax = fitted_cihigh_yesProp ) , alpha = 0.1 ) +
    facet_wrap( ~Subject )


##### functions no rnd = population -> mu threshold ####
getTHRESH_fromFit_no_RND = function(FIT , newdata){
    newdata$distance_m = 0 # get alpha x= 0 -> 0*beta
    ALPHA = fitted( FIT , newdata = newdata , re_formula = NA , scale = 'linear' , summary = FALSE )
    newdata$distance_m = 1 # x = 1 -> 1*beta -> rget beta
    BETA = fitted( FIT , newdata = newdata , re_formula = NA , scale = 'linear' , summary = FALSE ) - ALPHA

    -ALPHA/BETA # value of x for y = 50% -> threshold
}




#### mu slope ####
getSLOPE_fromFit_no_RND = function(FIT , newdata){
    newdata$distance_m = 0
    ALPHA = fitted( FIT , newdata = newdata , re_formula = NA , scale = 'linear' , summary = FALSE )
    newdata$distance_m = 1
    BETA = fitted( FIT , newdata = newdata , re_formula = NA , scale = 'linear' , summary = FALSE ) - ALPHA

    BETA/4 # slope ? 50% = beta/4
}



#### effet sei ####
getEMO_seitrend_on_THRESH = function(FIT , newdata , DELTA , EMO_GRP = NULL){
  if( !is.null(EMO_GRP) ){
      newdata$emotion = EMO_GRP
  }

  THRESH0 = getTHRESH_fromFit_no_RND( FIT , newdata) 
  newdata$sei_c = newdata$sei_c + DELTA # add delta on continuous -> 0.2
  THRESH1 = getTHRESH_fromFit_no_RND( FIT , newdata)

  (THRESH1 - THRESH0) # get DELTA
}


#### interaction emotion * sei  ####
getEMO_seitrend_diff_on_THRESH = function(FIT , newdata , EMO_GRPs , DELTA){

    newdata$emotion = EMO_GRPs[1]
    THRESH0 = getEMO_seitrend_on_THRESH( FIT , newdata , DELTA)
    newdata$emotion = EMO_GRPs[2]
    THRESH1 = getEMO_seitrend_on_THRESH( FIT , newdata , DELTA )

    (THRESH0 - THRESH1)
}

#### sei on slope ####
getEMO_seitrend_on_SLOPE = function(FIT , newdata , DELTA , EMO_GRP = NULL){
  if( !is.null(EMO_GRP) ){
    newdata$emotion = EMO_GRP
  }
    SLOPE0 = getSLOPE_fromFit_no_RND( FIT , newdata)
    newdata$sei_c = newdata$sei_c + DELTA
    SLOPE1 = getSLOPE_fromFit_no_RND( FIT , newdata)

    (SLOPE1 - SLOPE0)
}


## interaction emotion * sei on thresh
getEMO_seitrend_diff_on_SLOPE = function(FIT , newdata , EMO_GRPs , DELTA){

    newdata$emotion = EMO_GRPs[1]
    SLOPE0 = getEMO_seitrend_on_SLOPE( FIT , newdata , DELTA)
    newdata$emotion = EMO_GRPs[2]
    SLOPE1 = getEMO_seitrend_on_SLOPE( FIT , newdata , DELTA )

    (SLOPE0 - SLOPE1)
}


#### get posterior EMOTION ####
## slope & thresh for mu happy
DFtmp = DF[ DF$emotion == 'happy' ,]
mean_THRESH_happy = rowMeans( getTHRESH_fromFit_no_RND( FIT , DFtmp  ) )
mean_SLOPE_happy = rowMeans( getSLOPE_fromFit_no_RND( FIT , DFtmp  ) )
## slope & thresh for mu neutral
DFtmp = DF[ DF$emotion == 'neutral' ,]
mean_THRESH_neutral = rowMeans( getTHRESH_fromFit_no_RND( FIT , DFtmp  ) )
mean_SLOPE_neutral = rowMeans( getSLOPE_fromFit_no_RND( FIT , DFtmp  ) )
## slope & thresh for mu angry
DFtmp = DF[ DF$emotion == 'angry' ,]
mean_THRESH_angry = rowMeans( getTHRESH_fromFit_no_RND( FIT , DFtmp  ) )
mean_SLOPE_angry = rowMeans( getSLOPE_fromFit_no_RND( FIT , DFtmp  ) )

## contrasts
contrast_mean_THRESH_angry_neutral = mean_THRESH_angry - mean_THRESH_neutral
contrast_mean_THRESH_happy_neutral = mean_THRESH_happy - mean_THRESH_neutral
contrast_mean_THRESH_angry_happy = mean_THRESH_angry - mean_THRESH_happy

contrast_mean_SLOPE_angry_neutral = mean_SLOPE_angry - mean_SLOPE_neutral
contrast_mean_SLOPE_happy_neutral = mean_SLOPE_happy - mean_SLOPE_neutral
contrast_mean_SLOPE_angry_happy = mean_SLOPE_angry - mean_SLOPE_happy

#### get posterior GENDER #### 
## subject gender
DFtmp = DF[ DF$SUBJgender == 'm' ,]
mean_THRESH_subj_male = rowMeans( getTHRESH_fromFit_no_RND( FIT , DFtmp  ) )
DFtmp = DF[ DF$SUBJgender == 'f' ,]
mean_THRESH_subj_female = rowMeans( getTHRESH_fromFit_no_RND( FIT , DFtmp  ) )
contrast_mean_THRESH_subj_male_female = mean_THRESH_subj_male - mean_THRESH_subj_female

## stimulus gender
DFtmp = DF[ DF$AVATgender == 'm' ,]
mean_THRESH_male = rowMeans( getTHRESH_fromFit_no_RND( FIT , DFtmp  ) )
mean_SLOPE_male = rowMeans( getSLOPE_fromFit_no_RND( FIT , DFtmp  ) )
DFtmp = DF[ DF$AVATgender == 'f' ,]
mean_THRESH_female = rowMeans( getTHRESH_fromFit_no_RND( FIT , DFtmp  ) )
mean_SLOPE_female = rowMeans( getSLOPE_fromFit_no_RND( FIT , DFtmp  ) )

contrast_mean_THRESH_male_female = mean_THRESH_male - mean_THRESH_female
contrast_slope_THRESH_male_female = mean_SLOPE_male - mean_SLOPE_female
## stimulus gender * emo
## gender * angry
DFtmp = DF[ DF$AVATgender == 'm' & DF$emotion=='angry',]
mean_THRESH_male_angry = rowMeans( getTHRESH_fromFit_no_RND( FIT , DFtmp  ) )
DFtmp = DF[ DF$AVATgender == 'f' & DF$emotion=='angry',]
mean_THRESH_female_angry = rowMeans( getTHRESH_fromFit_no_RND( FIT , DFtmp  ) )
contrast_mean_THRESH_angry_male_female = mean_THRESH_male_angry - mean_THRESH_female_angry
## gender * neutral
DFtmp = DF[ DF$AVATgender == 'm' & DF$emotion=='neutral',]
mean_THRESH_male_neutral = rowMeans( getTHRESH_fromFit_no_RND( FIT , DFtmp  ) )
DFtmp = DF[ DF$AVATgender == 'f' & DF$emotion=='neutral',]
mean_THRESH_female_neutral = rowMeans( getTHRESH_fromFit_no_RND( FIT , DFtmp  ) )
contrast_mean_THRESH_neutral_male_female = mean_THRESH_male_neutral - mean_THRESH_female_neutral
## gender * happy
DFtmp = DF[ DF$AVATgender == 'm' & DF$emotion=='happy',]
mean_THRESH_male_happy = rowMeans( getTHRESH_fromFit_no_RND( FIT , DFtmp  ) )
DFtmp = DF[ DF$AVATgender == 'f' & DF$emotion=='happy',]
mean_THRESH_female_happy = rowMeans( getTHRESH_fromFit_no_RND( FIT , DFtmp  ) )
contrast_mean_THRESH_happy_male_female = mean_THRESH_male_happy - mean_THRESH_female_happy


## posterior distribution
POST = data.frame( mean_THRESH_angry = mean_THRESH_angry
                   , mean_THRESH_neutral = mean_THRESH_neutral
                   , mean_THRESH_happy = mean_THRESH_happy
                   , contrast_mean_THRESH_angry_happy = contrast_mean_THRESH_angry_happy
                   , contrast_mean_THRESH_angry_neutral = contrast_mean_THRESH_angry_neutral
                   , contrast_mean_THRESH_happy_neutral = contrast_mean_THRESH_happy_neutral
                   , mean_SLOPE_happy = mean_SLOPE_happy
                   , mean_SLOPE_neutral = mean_SLOPE_neutral
                   , mean_SLOPE_angry = mean_SLOPE_angry
                   , contrast_mean_SLOPE_angry_happy = contrast_mean_SLOPE_angry_happy
                   , contrast_mean_SLOPE_angry_neutral = contrast_mean_SLOPE_angry_neutral
                   , contrast_mean_SLOPE_happy_neutral = contrast_mean_SLOPE_happy_neutral
                   , contrast_mean_THRESH_subj_male_female = contrast_mean_THRESH_subj_male_female
                   , mean_THRESH_male = mean_THRESH_male
                   , mean_THRESH_female = mean_THRESH_female
                   , contrast_mean_THRESH_male_female = contrast_mean_THRESH_male_female
                   , contrast_mean_THRESH_angry_male_female = contrast_mean_THRESH_angry_male_female
                   , contrast_mean_THRESH_neutral_male_female = contrast_mean_THRESH_neutral_male_female
                   , contrast_mean_THRESH_happy_male_female = contrast_mean_THRESH_happy_male_female
)

## posterior for sei
## 0.2 because sei_c [-1 1] -> back sei [0 10]
DFtmp = DF
mean_THRESH_trend_sei = rowMeans( getEMO_seitrend_on_THRESH( FIT , DFtmp , 0.2 ) )
POST$mean_THRESH_trend_sei = mean_THRESH_trend_sei

DFtmp = DF
mean_THRESH_trend_sei_happy = rowMeans( getEMO_seitrend_on_THRESH( FIT , DFtmp , 0.2 , 'happy' ) )
POST$mean_THRESH_trend_sei_happy = mean_THRESH_trend_sei_happy

DFtmp = DF
mean_THRESH_trend_sei_angry = rowMeans( getEMO_seitrend_on_THRESH( FIT , DFtmp , 0.2 , 'angry' ) )
POST$mean_THRESH_trend_sei_angry = mean_THRESH_trend_sei_angry

DFtmp = DF
mean_THRESH_trend_sei_neutral = rowMeans( getEMO_seitrend_on_THRESH( FIT , DFtmp , 0.2 , 'neutral' ) )
POST$mean_THRESH_trend_sei_neutral = mean_THRESH_trend_sei_neutral


DFtmp = DF
contrast_THRESH_trend_sei_happy_angry = rowMeans( getEMO_seitrend_diff_on_THRESH( FIT , DFtmp , c('happy' , 'angry') , 0.2) )
POST$contrast_THRESH_trend_sei_happy_angry = contrast_THRESH_trend_sei_happy_angry

DFtmp = DF
contrast_THRESH_trend_sei_happy_neutral = rowMeans( getEMO_seitrend_diff_on_THRESH( FIT , DFtmp , c('happy' , 'neutral') , 0.2) )
POST$contrast_THRESH_trend_sei_happy_neutral = contrast_THRESH_trend_sei_happy_neutral

DFtmp = DF
contrast_THRESH_trend_sei_neutral_angry = rowMeans( getEMO_seitrend_diff_on_THRESH( FIT , DFtmp , c('neutral' , 'angry') , 0.2) )
POST$contrast_THRESH_trend_sei_neutral_angry = contrast_THRESH_trend_sei_neutral_angry




DFtmp = DF
mean_SLOPE_trend_sei = rowMeans( getEMO_seitrend_on_SLOPE( FIT , DFtmp , 0.2 ) )
POST$mean_SLOPE_trend_sei = mean_SLOPE_trend_sei

DFtmp = DF
mean_SLOPE_trend_sei_happy = rowMeans( getEMO_seitrend_on_SLOPE( FIT , DFtmp , 0.2 , 'happy' ) )
POST$mean_SLOPE_trend_sei_happy = mean_SLOPE_trend_sei_happy

DFtmp = DF
mean_SLOPE_trend_sei_angry = rowMeans( getEMO_seitrend_on_SLOPE( FIT , DFtmp , 0.2 , 'angry' ) )
POST$mean_SLOPE_trend_sei_angry = mean_SLOPE_trend_sei_angry

DFtmp = DF
mean_SLOPE_trend_sei_neutral = rowMeans( getEMO_seitrend_on_SLOPE( FIT , DFtmp , 0.2 , 'neutral' ) )
POST$mean_SLOPE_trend_sei_neutral = mean_SLOPE_trend_sei_neutral

DFtmp = DF
contrast_SLOPE_trend_sei_happy_angry = rowMeans( getEMO_seitrend_diff_on_SLOPE( FIT , DFtmp , c('happy' , 'angry') , 0.2) )
POST$contrast_SLOPE_trend_sei_happy_angry = contrast_SLOPE_trend_sei_happy_angry

DFtmp = DF
contrast_SLOPE_trend_sei_happy_neutral = rowMeans( getEMO_seitrend_diff_on_SLOPE( FIT , DFtmp , c('happy' , 'neutral') , 0.2) )
POST$contrast_SLOPE_trend_sei_happy_neutral = contrast_SLOPE_trend_sei_happy_neutral

DFtmp = DF
contrast_SLOPE_trend_sei_neutral_angry = rowMeans( getEMO_seitrend_diff_on_SLOPE( FIT , DFtmp , c('neutral' , 'angry') , 0.2) )
POST$contrast_SLOPE_trend_sei_neutral_angry = contrast_SLOPE_trend_sei_neutral_angry



#### bayes plots ####
library( bayesplot )
mcmc_intervals( POST , regex_pars = '^mean_THRESH' , prob_outer = 0.95 )
mcmc_intervals( POST , regex_pars = '^contrast.*THRESH' , prob_outer = 0.95 )
mcmc_intervals( POST , regex_pars = '^mean_SLOPE' , prob_outer = 0.95 )
mcmc_intervals( POST , regex_pars = '^contrast.*SLOPE' , prob_outer = 0.95 )

#### CI pop ####
DFpop = data.frame( estimate = parnames(POST) )
DFpop$mean = sapply(POST, mean)
DFpop$sd = sapply(POST, sd)
DFpop$'CI_2.5' = sapply(POST, FUN = function(x){ sort(x)[length(x)*0.025] } )
DFpop$'CI_97.5' = sapply(POST, FUN = function(x){ sort(x)[length(x)*0.975] } )
DFpop$'P>0' = sapply(POST, FUN = function(x){ mean( x > 0 ) } )


# file <- paste0( substring( FIT$file , 1, nchar( FIT$file)-4 ), "_population_summary.csv")
# write.csv( DFpop , file = file )


##### Get THRESHOLD estimates for each stim/Subject ####
##### function with_rnd = individual threshold ####
getTHRESH_fromFit_with_RND = function(FITsubj , newdata){
  newdata$distance_m = 0
  ALPHA = fitted( FITsubj , newdata = newdata , re_formula = NULL , scale = 'linear' , summary = FALSE )
  newdata$distance_m = 1
  BETA = fitted( FITsubj , newdata = newdata , re_formula = NULL , scale = 'linear' , summary = FALSE ) - ALPHA
  
  -ALPHA/BETA
}

#### individual slope ####
getSLOPE_fromFit_with_RND = function(FITsubj , newdata){
  newdata$distance_m = 0
  ALPHA = fitted( FITsubj , newdata = newdata , re_formula = NULL , scale = 'linear' , summary = FALSE )
  newdata$distance_m = 1
  BETA = fitted( FITsubj , newdata = newdata , re_formula = NULL , scale = 'linear' , summary = FALSE ) - ALPHA
  
  BETA/4
}



DFperStim = unique( DF[c('Subject', 'stim' , 'emotion' , 'sei_c' )] )
DFperStim$Ntrial = 1
THRESH_SUBJ_STIM = getTHRESH_fromFit_with_RND( FITsubj , newdata = DFperStim )
DFperStim$THRESH_mean = colMeans(THRESH_SUBJ_STIM)
THRESH_SUBJ_STIM_sort = apply( THRESH_SUBJ_STIM , 2 , sort )
DFperStim$'THRESH_CI_2.5' = THRESH_SUBJ_STIM_sort[nrow(THRESH_SUBJ_STIM_sort)*0.025,]
DFperStim$'THRESH_CI_97.5' = THRESH_SUBJ_STIM_sort[nrow(THRESH_SUBJ_STIM_sort)*0.975,]
DFperStim$THRESH_sd = apply( THRESH_SUBJ_STIM , 2 , sd )
## Get THRESHOLD estimates for each stim/Subject
SLOPE_SUBJ_STIM = getSLOPE_fromFit_with_RND( FITsubj , newdata = DFperStim )
DFperStim$SLOPE_mean = colMeans(SLOPE_SUBJ_STIM)
SLOPE_SUBJ_STIM_sort = apply( SLOPE_SUBJ_STIM , 2 , sort )
DFperStim$'SLOPE_CI_2.5' = SLOPE_SUBJ_STIM_sort[nrow(SLOPE_SUBJ_STIM_sort)*0.025,]
DFperStim$'SLOPE_CI_97.5' = SLOPE_SUBJ_STIM_sort[nrow(SLOPE_SUBJ_STIM_sort)*0.975,]
DFperStim$SLOPE_sd = apply( SLOPE_SUBJ_STIM , 2 , sd )

DFperStim$Ntrial = NULL
rownames( DFperStim ) = NULL
#file <- paste0( substring( FIT$file , 1, nchar( FIT$file)-4 ), "_perStim_summary.csv")
#write.csv( DFperStim , file = file )


#######################################################################################
##### Get THRESHOLD estimates for cond/subject (for 32 cm%) ####
resp_pre_thresh = unique( DF[c('Subject' , 'emotion'  )] )
resp_pre_thresh$Ntrial = 1
resp_pre_thresh$sei_c = 0 # to get 1 thresh per emotion, arousal = same
library(stringr)
resp_pre_thresh$cond= factor(paste(str_sub(resp_pre_thresh$emotion,1,1),"32", sep="_"))


thresh_subj = getTHRESH_fromFit_with_RND( FITsubj , newdata = resp_pre_thresh )
resp_pre_thresh$THRESH_mean = colMeans(thresh_subj)
thresh_subj_sort = apply( thresh_subj , 2 , sort )
resp_pre_thresh$'THRESH_CI_2.5' = thresh_subj_sort[nrow(thresh_subj_sort)*0.025,]
resp_pre_thresh$'THRESH_CI_97.5' = thresh_subj_sort[nrow(thresh_subj_sort)*0.975,]
resp_pre_thresh$THRESH_sd = apply( thresh_subj , 2 , sd )
## Get THRESHOLD estimates for each stim/subject
slope_subj = getSLOPE_fromFit_with_RND( FITsubj , newdata = resp_pre_thresh )
resp_pre_thresh$SLOPE_mean = colMeans(slope_subj)
slope_subj_sort = apply( slope_subj , 2 , sort )
resp_pre_thresh$'SLOPE_CI_2.5' = slope_subj_sort[nrow(slope_subj_sort)*0.025,]
resp_pre_thresh$'SLOPE_CI_97.5' = slope_subj_sort[nrow(slope_subj_sort)*0.975,]
resp_pre_thresh$SLOPE_sd = apply( slope_subj , 2 , sd )
resp_pre_thresh$cond= factor(paste(str_sub(resp_pre_thresh$emotion,1,1),"32", sep="_"))

resp_pre_thresh$Ntrial = NULL
rownames( resp_pre_thresh ) = NULL
# file <- paste0( substring( FIT$file , 1, nchar( FIT$file)-4 ), "_per_cond.csv")
# write.csv( resp_pre_thresh , file = file )
# save(resp_pre_thresh, file = "resp_pre_thresh.RData")

