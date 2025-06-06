#### FIT RESP EMOx DIST ####
rm(list=ls()) 
graphics.off() 
library(rstudioapi)
dirs<-rstudioapi::getSourceEditorContext()$path
setwd(dirname(dirs))
library(brms)

load("behavioural_df.Rdata")
df= behavioural_df
library(stringr)


## Descriptives ####
obs_d=aggregate(response ~ distance, df, mean)
obs_d$response<-(obs_d$response)*100   # convert to %
obs_d
obs = aggregate(response ~ emotion + distance, df, mean) 
obs$response=(obs$response)*100  # convert to %
library(stringr)
obs$stim <- factor(paste(str_sub(obs$emotion,1,1),obs$distance, sep="_"))
obs

## density
library(ggplot2)
ggplot(df, aes(x=response, fill=emotion)) + geom_density(alpha=0.4)


## FIT ####
library(brms)
FIT_bh= brm( response ~ emotion*distance   + (1  |Subject)
             , data = df 
             , family = "bernoulli"
             , prior = prior( normal(0,1) , class = "b" ) 
             , cores = parallel::detectCores()
             , iter = 4000
             , warmup = 2000
             , file = 'bh_FIT'
)

summary(FIT_bh)
stanplot(FIT_bh)
plot(FIT_bh) # posterior coef & mcmc
prior_summary(FIT_bh) # prior info
library(bayesplot)
pp_check(FIT_bh) + xlab("response") # posterior predictive check, Y= distrib of Y, Yrep = simulated data 


## Plotting model predictions
brms::marginal_effects(FIT_bh)

## get fitted values
newdata=data.frame(obs [c('emotion', 'distance')])
newdata$stim <- factor(paste(str_sub(obs$emotion,1,1),obs$distance, sep="_"))

fit = fitted(FIT_bh, newdata = newdata, re_formula = NA) *100  # convert to %
colnames(fit) = c('fit', 'se', 'lwr', 'upr')
df_plot = cbind(newdata, fit)
df_plot


## FIT1 ####
fit1 = as.data.frame(fitted(FIT_bh, newdata = newdata, re_formula = NA, scale='response', summary = FALSE))
colnames(fit1) = newdata$stim
head(fit1)
nrow(fit1)

## df mu MM ####
mu_m8= (fit1$'a_-8' + fit1$'h_-8' + fit1$'n_-8')/3
mu_32= (fit1$'a_32' + fit1$'h_32' + fit1$'n_32')/3
mu_72= (fit1$'a_72' + fit1$'h_72' + fit1$'n_72')/3

mu_angry= (fit1$'a_-8' + fit1$'a_32' + fit1$'a_72')/3
mu_happy= (fit1$'h_-8' + fit1$'h_32' + fit1$'h_72')/3
mu_neutral= (fit1$'n_-8' + fit1$'n_32' + fit1$'n_72')/3

mu_neutral_32= (fit1$'n_32')
mu_angry_32= (fit1$'a_32')
mu_happy_32= (fit1$'h_32')

## POST df mu ####
POST_MM = data.frame( mu_m8 = mu_m8
                      , mu_32 = mu_32
                      , mu_72 = mu_72
                      , mu_angry = mu_angry
                      , mu_happy= mu_happy
                      , mu_neutral = mu_neutral
                      , mu_angry_32 = mu_angry_32
                      , mu_happy_32 = mu_happy_32
                      , mu_neutral_32 = mu_neutral_32
)




## contrasts distances ####
d_32_8 = (fit1$'a_32' + fit1$'h_32' + fit1$'n_32')/3 - (fit1$'a_-8' + fit1$'h_-8' + fit1$'n_-8')/3
d_72_8 = (fit1$'a_72' + fit1$'h_72' + fit1$'n_72')/3 - (fit1$'a_-8' + fit1$'h_-8' + fit1$'n_-8')/3
d_32_72 = (fit1$'a_32' + fit1$'h_32' + fit1$'n_32')/3 - (fit1$'a_72' + fit1$'h_72' + fit1$'n_72')/3

## contrasts emotions ####
a_h = (fit1$'a_-8' + fit1$'a_32' + fit1$'a_72')/3 - (fit1$'h_-8' + fit1$'h_32' + fit1$'h_72')/3
a_n =  (fit1$'a_-8' + fit1$'a_32' + fit1$'a_72')/3 - (fit1$'n_-8' + fit1$'n_32' + fit1$'n_72')/3
n_h = (fit1$'n_-8' + fit1$'n_32' + fit1$'n_72')/3 - (fit1$'h_-8' + fit1$'h_32' + fit1$'h_72')/3

## contrasts per dist * emo ####
## -8 
m8_happy_neutral = fit1$'h_-8' - fit1$'n_-8'
m8_happy_angry = fit1$'h_-8' - fit1$'a_-8'
m8_neutral_angry = fit1$'n_-8' - fit1$'a_-8'

## 32
d32_happy_neutral = fit1$h_32 - fit1$n_32
d32_happy_angry = fit1$'h_32' - fit1$'a_32'
d32_neutral_angry = fit1$'n_32' - fit1$'a_32'

## 72
d72_happy_neutral = fit1$h_72 - fit1$n_72
d72_happy_angry = fit1$'h_72' - fit1$'a_72'
d72_neutral_angry = fit1$'n_72' - fit1$'a_72'

## contrasts emo per distance ####
## angry
angry_32_m8 = fit1$'a_32' - fit1$'a_-8'
angry_32_72 = fit1$'a_32' - fit1$'a_72'
angry_72_m8 = fit1$'a_72' - fit1$'a_-8'

## happy
happy_32_m8 = fit1$'h_32' - fit1$'h_-8'
happy_32_72 = fit1$'h_32' - fit1$'h_72'
happy_72_m8 = fit1$'h_72' - fit1$'h_-8'

## neutral
neutral_32_m8 = fit1$'n_32' - fit1$'n_-8'
neutral_32_72 = fit1$'n_32' - fit1$'n_72'
neutral_72_m8 = fit1$'n_72' - fit1$'n_-8'

## df contrasts AME ####
POST_AME = data.frame( d_32_8 = d_32_8
                       , d_72_8 = d_72_8
                       , d_32_72 = d_32_72
                       , a_h = a_h
                       , a_n = a_n
                       , n_h = n_h
                       , m8_happy_neutral = m8_happy_neutral
                       , m8_happy_angry = m8_happy_angry
                       , m8_neutral_angry = m8_neutral_angry
                       , d32_happy_neutral = d32_happy_neutral 
                       , d32_happy_angry = d32_happy_angry
                       , d32_neutral_angry = d32_neutral_angry
                       , d72_happy_neutral = d72_happy_neutral
                       , d72_happy_angry = d72_happy_angry
                       , d72_neutral_angry = d72_neutral_angry
                       , angry_32_m8 = angry_32_m8
                       , angry_32_72 = angry_32_72
                       , angry_72_m8 = angry_72_m8
                       , happy_32_m8 = happy_32_m8
                       , happy_32_72 = happy_32_72
                       , happy_72_m8 = happy_72_m8
                       , neutral_32_m8 = neutral_32_m8
                       , neutral_32_72 = neutral_32_72
                       , neutral_72_m8 = neutral_72_m8
)

## export csv ####
## df Marginal Mean with mu estimates
DF_MM_bh = data.frame( estimate = parnames(POST_MM) )
DF_MM_bh$mean = sapply(POST_MM, mean)
DF_MM_bh$sd = sapply(POST_MM, sd)
DF_MM_bh$'CI_2.5' = sapply(POST_MM, FUN = function(x){ sort(x)[length(x)*0.025] } )
DF_MM_bh$'CI_97.5' = sapply(POST_MM, FUN = function(x){ sort(x)[length(x)*0.975] } )

## df Average Marginal Effect with contrats estimates
DF_AME_bh = data.frame( estimate = parnames(POST_AME) )
DF_AME_bh$mean = sapply(POST_AME, mean)
DF_AME_bh$sd = sapply(POST_AME, sd)
DF_AME_bh$'CI_2.5' = sapply(POST_AME, FUN = function(x){ sort(x)[length(x)*0.025] } )
DF_AME_bh$'CI_97.5' = sapply(POST_AME, FUN = function(x){ sort(x)[length(x)*0.975] } )

## recap df
df_estimate_bh<-rbind(DF_MM_bh,DF_AME_bh)

# file <- paste0( substring( FIT$file , 1, nchar( FIT$file)-4 ), "_marginal_means_summary.csv")
# write.csv( df_estimate_bh , file = "bh_marginal.csv" )                


####################################################
#### DF for eq5 % ~cm  per Subject at 32cm #####
cond = aggregate(response ~ Subject + emotion + distance, df, mean)
cond$cond <- factor(paste(str_sub(cond$emotion,1,1),cond$distance, sep="_"))
cond = subset(cond, distance =="32")


df$cond= factor(paste(str_sub(df$emotion,1,1),df$distance, sep="_"))
DFperSubj = unique( df[c('Subject', 'cond', 'emotion','distance' )] )
DFperSubj= subset(DFperSubj, distance =='32')
fit_subj = fitted( FIT_bh , newdata = DFperSubj,  re_formula = NULL) * 100 # null =random


colnames(fit_subj) = c('fit_p_y', 'se_p_y', 'lwr_p_y', 'upr_p_y')
response_pre_df = cbind(DFperSubj, fit_subj)
save(response_pre_df, file = "response_pre_df.Rdata")

# write.csv(df_plot, file = "marginal_subj_bh.csv")

