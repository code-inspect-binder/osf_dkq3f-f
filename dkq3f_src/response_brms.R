## % ~ threshold ####
rm(list=ls()) 
graphics.off()
library(rstudioapi) 
dirs<-rstudioapi::getSourceEditorContext()$path 
setwd(dirname(dirs)) 
library(brms)

load('response_pre_df.Rdata')
load('response_pre_threshold.Rdata')


df=merge(response_pre_df, response_pre_threshold, by = c("Subject", "emotion", "cond"))
df$prop_yes=df$fit/100


df$Subject<-factor(df$Subject)

## outliers
df<-subset(df, THRESH_mean >0)
df<-subset(df, THRESH_mean <1.5)
df<-subset(df, THRESH_sd <1)

table(df$Subject)
df=subset(df, Subject !="s26") # only 1 data

## plots pre fit ####
library(ggplot2)
ggplot( df, aes(prop_yes , THRESH_mean, col = emotion ))+
  geom_point() + 
  geom_smooth(method='lm') 
  # facet_wrap( ~ Subject )

#### FIT ####
FIT =brm(formula = bf(prop_yes ~  me(THRESH_mean,  THRESH_sd)+ (1|Subject)) 
           , data = df
           , family = gaussian
           , prior = prior( normal(0,1) , class = "b" )
           , control = list(adapt_delta = 0.95 , max_treedepth = 15 )
           , cores = parallel::detectCores()
           , iter = 4000
           , warmup = 2000
           , file = 'rep_thresh_fit'
)

summary(FIT)
# plot(FIT)
fixef(FIT)
# marginal_effects(FIT)


bayes_R2(FIT)


## plot fit #####
newdata=data.frame(df [c('emotion', 'Subject', 'prop_yes', 'THRESH_mean', 'THRESH_sd')])
newdata$THRESH_sd = mean(newdata$THRESH_sd) # fix THRESH_sd to its mean
post = fitted(FIT, newdata = newdata, re_formula = NA) 


colnames(post) = c('fit', 'se', 'lwr', 'upr')
df_plot = cbind(newdata, post)
df_plot$threshold=df_plot$THRESH_mean*100

library(ggplot2)
library(extrafont)
font_import(pattern="times.ttf")
loadfonts(device="win") 

p=ggplot(df_plot, aes(x=threshold, y= prop_yes))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
       text=element_text(size=10),
       axis.ticks =  element_line(colour = "black"))+
  geom_point(aes(shape = emotion))+
  geom_line(aes(threshold , fit), size =1) +
  geom_ribbon( aes( ymin = lwr , ymax = upr ) , linetype =0,  alpha = 0.2 ) +
  geom_hline(yintercept=.50, linetype="dashed", color = "grey", size =0.5, alpha =0.5)+
  xlab("Comfort Threshold (cm)")+
  ylab("Proportion of 'Comfortable' Responses at 32cm")+
  labs(shape="Facial \nExpression")+
  scale_shape_manual(values =c(17, 1, 4),
                     labels=c("Angry", "Happy", "Neutral")) 

p

### to save ####
library(Cairo)

Cairo(file="CartaudFig5.pdf",
      type="pdf",
      units="px",
      width=7000,
      height=5000,
      pointsize=7,
      dpi=300)

p

dev.off()

