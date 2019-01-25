library(tidyverse)
library(psych)
library(sjPlot)
library(lmerTest)
library(fitdistrplus)

#data entry ----
library(googlesheets)
gs_auth()
gs_ls()
gs_spreadsheet <- gs_title("OpenShift Benchmark Data")
gs_data <- gs_read(gs_spreadsheet)
df <- gs_data



#widening for airtable



df %>%
  select(par,task,time,comp,sat) -> dft

cT <- tribble(
  #~
)

clip<- pipe("pbcopy", "w")      

dft %>% 
  nest(comp, sat, time, .key = 'value_col') %>%
  spread(key = task, value = value_col) %>%
  unnest(.sep = '_') %>% write.table(., file=clip)                               
close(clip)


#data
df <- read.csv(file="/Users/cjpearson/Documents/r_github/Openshift-sum-git/final-combined-data.csv",header=T)

#make sure variables are the correct type for analyses
df_adv <- df %>% 
  select(group,par,task,time,comp,sat) %>%
  mutate(group=as.factor(group)) %>%
  mutate(par=as.factor(par)) %>%
  mutate(task=as.factor(task)) %>%
  mutate(time=as.numeric(as.character((time)))) %>% mutate(group=as.factor(recode(group,"2"="3.11","1"="3.5")))

df_adv$time[df_adv$comp==0] <- NA #remove time values for failures


#completion
lme4::glmer(comp ~
              (1|par), 
            data = df_adv, 
            family = binomial
) -> null
summary(null)
lme4::glmer(comp ~ group*task +
              (1|par), 
            data = df_adv, 
            family = binomial,
            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000))
                                 
) -> full
summary(full)

#Singular fit glm so can't do completion

#linear
#descriptives
describe(df_adv)[4:6,]
psych::describeBy(df_adv$sat,group = df_adv$task)

#sat
sat_null <- lmerTest::lmer(sat ~ (1|par), data=df_adv)
summary(sat_null)
sjPlot::tab_model(sat_null,
                  show.adj.icc=TRUE,
                  show.intercept = T,
                  show.est = T,
                  p.style="asterisk",
                  show.stat = T,
                  show.obs = F)

plot(sat_null)

sat_one <- lmerTest::lmer(sat ~ group + (1|par), data=df_adv)
summary(sat_one)
sjPlot::tab_model(sat_null,sat_one,
                  show.adj.icc=TRUE,
                  show.intercept = T,
                  show.est = T,
                  p.style="asterisk",
                  show.stat = T,
                  show.obs = F)
sjPlot::plot_model(sat_one)


sat_full <- lmerTest::lmer(sat ~ group*task + (1|par), data=df_adv)
summary(sat_full)
sjPlot::tab_model(sat_null,sat_one,sat_full,
                  show.adj.icc=TRUE,
                  show.intercept = T,
                  show.est = T,
                  p.style="asterisk",
                  show.stat = T,
                  show.obs = F)
sjPlot::plot_model(sat_full)

sjPlot::plot_model(sat_full,type = "std")

sjPlot::plot_model(sat_full,
                   type = "int",
                   grid=T,
                   show.p=F,
                   show.intercept = F,
                   show.data = F
) -> sat_plot

#model comparison
anova(sat_null,sat_full)

plot(sat_full)

#check residuals
fit.norm <-fitdist(data = resid(sat_full), dist="norm")
plot(fit.norm)


plot_model(sat_full, type = "pred", terms = c("task", "group")) -> sat2_plot
sat2_plot + geom_line() + scale_fill_manual(values = c("#f57670","#1fbec3"),aesthetics = "color")

ggsave("openshift_adv_sat.png",
       device = "png",
       bg =  "transparent",
       width = 6,
       height = 5)

#time

time_null <- lme4::glmer(time ~ (1|par), family = Gamma(link = log),data=df_adv_time)
summary(time_null)
sjPlot::tab_model(time_null,
                  show.adj.icc=TRUE,
                  show.intercept = T,
                  show.est = T,
                  p.style="asterisk",
                  show.stat = T,
                  show.obs = F)


time_one <- lme4::glmer(time ~ group + (1|par),family = Gamma(link = log), data=df_adv_time)
summary(time_one)
sjPlot::tab_model(time_one,
                  show.adj.icc=TRUE,
                  show.intercept = T,
                  show.est = T,
                  p.style="asterisk",
                  show.stat = T,
                  show.obs = F)
sjPlot::plot_model(time_one)


#real start after null is singular
time_norm <- lmerTest::lmer(time ~ group*task + (1|par), data=df_adv)
time_gamma <- glmer(time ~ group*task + (1|par),family = Gamma(link = log), data=df_adv)
fit.nor = fitdistrplus::fitdist(data = resid(time_norm), dist="norm")
plot(fit.nor)
fit.gam = fitdistrplus::fitdist(data = resid(time_gamma), dist="norm")
plot(fit.gam)

time_wei <- gamlss::gamlss(time ~ group*task + (1|as.integer(par)),
                           family = "WEI",
                           data = na.omit(df_adv)
) 
fit.w= fitdistrplus::fitdist(data = resid(time_wei), dist="norm")
plot(fit.w)


plot(time_full)
plot(time_wei)


fitdistrplus::descdist(df_adv_time$time, discrete = F,boot = 10000) 

sjPlot::tab_model(time_gamma,
                  show.adj.icc=TRUE,
                  show.intercept = T,
                  show.est = T,
                  p.style="asterisk",
                  show.stat = T,
                  show.obs = F)

plot_model(time_gamma, type = "pred", terms = c("task", "group")) -> time2_plot
time2_plot + geom_line()+ scale_fill_manual(values = c("#f57670","#1fbec3"),aesthetics = "color") + scale_y_reverse() 

ggsave("openshift_adv_time.png",
       device = "png",
       bg =  "transparent",
       width = 6,
       height = 5)

