## ---- packages
library(tidyverse)   # for data processing
library(rstan)       # for underlying Bayesian modelling framework
library(brms)        # for Bayesian GLMM
library(emmeans)     # for marginal means
library(glmmTMB)     # for frequentist GLMM
library(ggridges)    # for ridge plots
library(broom)       # for tidy outputs
library(broom.mixed) # for additional tidy methods
library(patchwork)   # for arranging plots
library(DHARMa)      # for residual diagnostics
library(tidybayes)   # for tidying Bayesian outputs
library(INLA)        # Bayesian approximation
library(INLAutils)   # inla diagnostics
## ----end

## ---- loadData 
load(file='../data/FullData.RData')
glimpse(FullData)
## ----end

## ---- processData

FullData = FullData %>%
    #filter(!Subject %in% c('NS1881','PY2111')) %>% droplevels %>% 
    mutate(fYear=as.factor(Year),
           total_achievement = total_achievement/100,
           total_achievement = ifelse(total_achievement==0,0.01,total_achievement),
           on_course = on_course/100,
           on_course = ifelse(on_course==0, 0.01, on_course),
           exam = exam/100,
           exam = ifelse(exam==0, 0.01, exam),
           pracvivaosce = pracvivaosce/100,
           pracvivaosce = ifelse(pracvivaosce==0, 0.01, ifelse(pracvivaosce==1,0.99,pracvivaosce)),
           uninvigilated = uninvigilated/100,
           uninvigilated = ifelse(uninvigilated==0, 0.01, uninvigilated),
           ftask_change=as.factor(task_change),
           fYear_level = factor(gsub('[A-Z]{2}([0-9]).*','\\1',Subject)))
FullData.all = FullData

FullData = FullData %>% 
    filter(!is.na(task_change),
           #!is.na(total_achievement),
           DNS==0) %>%
    droplevels

## ----end


# FullData %>%
#     ggplot(aes(y=total_achievement, x=Year)) +
#     geom_point()
# 
# Subjects = c('HS1111','NS1771','NS2216','PY3101','NS2220',
#              'NS2221','NS2771','NS2881','NS3228','NS3363','NS3771','NS3881',
#              'OT1011','OT3001','OT3011','OT3012',
#              'PS1001','PS2004','PS3001','PS3002','PS3003','PS3006',
#              'PY3101','PY3102','PY3104',
#              'SL1001','SL1002','SL2006',
#              'SP2007','SP2200','SP3006','SP3011','SP3015','SP4103',
#              'SS1111','SS1113')
# # 'PY2018' - 2019
# # 'SP1001' - 2020
# Subjects <- FullData %>% pull(Subject) %>% unique()

## ---- EDA1

FullData %>%
    #filter(Subject %in% Subjects) %>%
    ggplot(aes(y=total_achievement, x=fYear, color=factor(task_change))) +
    scale_color_discrete('Task Change\nCategory') +
    scale_y_continuous('Total Achievement (%)', labels=function(x) x*100) +
    geom_point() +
    facet_wrap(~Subject) +
    theme(axis.title.x=element_blank())

## ----end

## ---- EDA1a
FullData %>%
    #filter(Subject %in% Subjects) %>%
    ggplot(aes(y=total_achievement, x=fYear, color=factor(fYear_level))) +
    scale_color_discrete('Task Change\nCategory') +
    scale_y_continuous('Total Achievement (%)', labels=function(x) x*100) +
    geom_point() +
    facet_wrap(~Subject) +
    theme(axis.title.x=element_blank())
## ----end

## ---- EDA2
FullData %>%
    #filter(Subject %in% Subjects) %>%
    ggplot(aes(y=exam, x=fYear, color=factor(task_change))) +
    scale_color_discrete('Task Change\nCategory') +
    scale_y_continuous('Exam score (%)', labels=function(x) x*100) +
    geom_point() +
    facet_wrap(~Subject) +
    theme(axis.title.x=element_blank())
## ----end

## ---- EDA2a
FullData %>%
    #filter(Subject %in% Subjects) %>%
    ggplot(aes(y=exam, x=fYear, color=factor(fYear_level))) +
    scale_color_discrete('Task Change\nCategory') +
    scale_y_continuous('Exam score (%)', labels=function(x) x*100) +
    geom_point() +
    facet_wrap(~Subject) +
    theme(axis.title.x=element_blank())
## ----end

## ---- EDA3
FullData %>%
    #filter(Subject %in% Subjects) %>%
    ggplot(aes(y=on_course, x=fYear, color=factor(task_change))) +
    scale_color_discrete('Task Change\nCategory') +
    scale_y_continuous('On course score (%)', labels=function(x) x*100) +
    geom_point() +
    facet_wrap(~Subject) +
    theme(axis.title.x=element_blank())
## ----end
## ---- EDA3a
FullData %>%
    #filter(Subject %in% Subjects) %>%
    ggplot(aes(y=on_course, x=fYear, color=factor(fYear_level))) +
    scale_color_discrete('Task Change\nCategory') +
    scale_y_continuous('On course score (%)', labels=function(x) x*100) +
    geom_point() +
    facet_wrap(~Subject) +
    theme(axis.title.x=element_blank())
## ----end

## ---- EDA4
FullData %>%
    #filter(Subject %in% Subjects) %>%
    ggplot(aes(y=pracvivaosce, x=fYear, color=factor(task_change))) +
    scale_color_discrete('Task Change\nCategory') +
    scale_y_continuous('Prac vivaosce score (%)', labels=function(x) x*100, limits=c(0,1)) +
    geom_point() +
    facet_wrap(~Subject) +
    theme(axis.title.x=element_blank())
## ----end
## ---- EDA4a
FullData %>%
    #filter(Subject %in% Subjects) %>%
    ggplot(aes(y=pracvivaosce, x=fYear, color=factor(fYear_level))) +
    scale_color_discrete('Task Change\nCategory') +
    scale_y_continuous('Prac vivaosce score (%)', labels=function(x) x*100, limits=c(0,1)) +
    geom_point() +
    facet_wrap(~Subject) +
    theme(axis.title.x=element_blank())
## ----end

## ---- EDA5
FullData.all %>% group_by(Subject, fYear, task_change, DNS) %>% count() %>%
    #filter(Subject %in% Subjects) %>%
    ggplot(aes(y=DNS, x=fYear, color=factor(task_change))) +
    scale_color_discrete('Task Change\nCategory') +
    scale_y_continuous('DNS', labels=function(x) x*100, limits=c(0,1)) +
    geom_point(aes(size=n)) +
    facet_wrap(~Subject) +
    theme(axis.title.x=element_blank())
## ----end



## ---- EDA1b
FullData %>%
    ggplot() +
    geom_density_ridges(aes(x=total_achievement, y=fYear, fill=factor(task_change)), alpha=0.6) +
    facet_wrap(~Subject) +
    scale_fill_discrete('Task Change\nCategory') +
    scale_x_continuous('Total achievement (%)', labels=function(x) x*100, limits=c(0,1)) +
    facet_wrap(~Subject) +
    theme(axis.title.y=element_blank())
## ----end

## ---- EDA2b
FullData %>% 
    ggplot() +
    geom_density_ridges(aes(x=exam, y=fYear, fill=factor(task_change)), alpha=0.6) +
    facet_wrap(~Subject) +
    scale_fill_discrete('Task Change\nCategory') +
    scale_x_continuous('Exam score (%)', labels=function(x) x*100, limits=c(0,1)) +
    theme(axis.title.y=element_blank())
## ----end

## ---- EDA3b
FullData %>%
    ggplot() +
    geom_density_ridges(aes(x=on_course, y=fYear, fill=factor(task_change)), alpha=0.6) +
    facet_wrap(~Subject) +
    scale_fill_discrete('Task Change\nCategory') +
    scale_x_continuous('On course score (%)', labels=function(x) x*100, limits=c(0,1)) +
    facet_wrap(~Subject) +
    theme(axis.title.y=element_blank())
## ----end

## ---- EDA4b
FullData %>%
    ggplot() +
    geom_density_ridges(aes(x=pracvivaosce, y=fYear, fill=factor(task_change)), alpha=0.6) +
    facet_wrap(~Subject) +
    scale_fill_discrete('Task Change\nCategory') +
    scale_x_continuous('Prac vivaosce score (%)', labels=function(x) x*100, limits=c(0,1)) +
    facet_wrap(~Subject) +
    theme(axis.title.y=element_blank())
## ----end


## Modelling #####################################################################################

## TA -------------------------------------------------------------

## ---- processTA
FullData.TA = FullData %>% filter(!is.na(total_achievement)) %>% droplevels
## ----end

## glmmTMB %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## ---- fitModels-TA-glmmTMB
mod.TA <- glmmTMB(total_achievement ~ fYear*ftask_change + (1|Subject) + (1|id),
               data=FullData.TA,
               family='beta_family')
save(mod.TA, file='../data/mod.TA.RData')
## ----end

## ---- fitModel-TA-glmmTMB-diagnostics

load(file='../data/mod.TA.RData')
mod.resid<-DHARMa::simulateResiduals(mod.TA)
ggsave(filename=paste0('../output/mod.TA.diag.png'), plot(mod.resid), width=10, height=5, dpi=300)
DHARMa::testDispersion(mod.resid)

## ----end
## ---- fitModel-TA-glmmTMB-summary
load(file='../data/mod.TA.RData')
broom::tidy(mod.TA) %>% knitr::kable()  
## ----end
## ---- fitModel-TA-glmmTMB-contrasts1a
load(file='../data/mod.TA.RData')
em <- emmeans(mod.TA, ~fYear)
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
em.contr %>% 
    tidy(conf.int=TRUE) %>% 
    knitr::kable() 
## ----end
## ---- fitModel-TA-glmmTMB-contrasts1figA
g1 <- em.means %>% as.data.frame() %>%
    ggplot() + 
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Total achievement (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % total achievement', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2 
## ----end
## ---- fitModel-TA-glmmTMB-contrasts1b
em <- emmeans(mod.TA, ~fYear, weights='proportional')
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
em.contr %>% 
    tidy(conf.int=TRUE) %>% 
    knitr::kable() 
## ----end
## ---- fitModel-TA-glmmTMB-contrasts1figB
g1 <- em.means %>% as.data.frame() %>%
    ggplot() + 
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Total achievement (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % total achievement', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2 
## ----end
## ---- fitModel-TA-glmmTMB-contrasts2
em <- emmeans(mod.TA, ~fYear*ftask_change) %>% regrid

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.CL, ymax=upper.CL)) +
    facet_wrap(~ftask_change) + 
    theme_bw()

g2 <- em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE)) %>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=ftask_change, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % total achievement', labels=function(x) x*100) +
    scale_x_discrete('Task change code') +
    coord_flip() + 
    theme_bw()
g1 + g2
 
em %>% tidy(conf.int=TRUE) %>% knitr::kable()
em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE)) %>% 
    tidy(conf.int=TRUE) %>% 
    dplyr::select(-term,-null.value) %>%
    knitr::kable()

cellmeans.TA.glmmTMB <- em %>% as.data.frame() %>% mutate(Response='Total Achievement')
save(cellmeans.TA.glmmTMB, file='../data/cellmeans.TA.glmmTMB.RData')
effects.TA.glmmTMB <- em %>%
    contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE)) %>%
    as.data.frame()  %>% mutate(Response='Total Achievement')
save(effects.TA.glmmTMB, file='../data/effects.TA.glmmTMB.RData')
## ----end
## ---- fitModel-TA-glmmTMB-contrasts3
xmat = em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change') %>% coef()
 
em.contr <- contrast(em, method=list(
    #'0'=xmat[,3],
    #'5'=xmat[,6],
    #'6'=xmat[,7],
    '0+6'=rowMeans(xmat[,c(3,7)]),
    #'21'=xmat[,9],
    #'31'=xmat[,12],
    '21+31'=rowMeans(xmat[,c(9,12)]),
    #'22'=xmat[,10],
    #'3'=xmat[,4],
    #'13'=xmat[,8],
    #'33'=xmat[,13],
    '3+13+33'=rowMeans(xmat[,c(4,8,13)]),
    #'4'=xmat[,5],
    #'24'=xmat[,11],
    '4+24'=rowMeans(xmat[,c(5,11)]),
    '21+31+22+3+13+33'=rowMeans(xmat[,c(9,12,10,4,8,13)]),
    '4+24+5'=rowMeans(xmat[,c(4,11,6)]),
    '21+31+22+3+13+33 vs 4+24+5'=rowMeans(xmat[,c(9,12,10,4,8,13)]) - 
        rowMeans(xmat[,c(4,11,6)])
),
infer=c(TRUE,TRUE)
)

em.contr %>% tidy(conf.int=TRUE) %>% dplyr::select(-term,-null.value) %>% knitr::kable()
em.contr %>% 
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % total achievement', labels=function(x) x*100) +
    scale_x_discrete('Contrast') +
    coord_flip() + 
    theme_bw()
## ----end

## ---- fitModels-TA-glmmTMBa
mod.TAa <- glmmTMB(total_achievement ~ fYear*fYear_level + (1|Subject) + (1|id),
               data=FullData.TA,
               family='beta_family')
save(mod.TAa, file='../data/mod.TAa.RData')
## ----end
## ---- fitModel-TA-glmmTMBa-diagnostics
load(file='../data/mod.TAa.RData')
mod.resid<-DHARMa::simulateResiduals(mod.TAa,plot = TRUE)
DHARMa::testDispersion(mod.resid) 
## ----end
## ---- fitModel-TA-glmmTMBa-summary
load(file='../data/mod.TAa.RData')
broom::tidy(mod.TAa) %>% knitr::kable()  
## ----end
## ---- fitModel-TA-glmmTMBa-contrasts2
load(file='../data/mod.TAa.RData')
em <- emmeans(mod.TAa, ~fYear*fYear_level) %>% regrid

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.CL, ymax=upper.CL)) +
    facet_wrap(~fYear_level) + 
    theme_bw()


g2 <- em %>% contrast(method=list(fYear=c(-1,1)), by='fYear_level', infer=c(TRUE,TRUE)) %>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=fYear_level, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % total achievement', labels=function(x) x*100) +
    scale_x_discrete('Year level') +
    coord_flip() + 
    theme_bw()
g1 + g2
 
em %>% tidy(conf.int=TRUE) %>% knitr::kable()
em %>% contrast(method=list(fYear=c(-1,1)), by='fYear_level', infer=c(TRUE,TRUE)) %>% 
    tidy(conf.int=TRUE) %>% 
    dplyr::select(-term,-null.value) %>%
    knitr::kable()
## ----end

## ---- initialTesting


FullData.TA %>% group_by(fYear) %>% summarise(Mean=mean(total_achievement, na.rm=TRUE))
FullData.TA %>% group_by(fYear, ftask_change) %>% summarise(Mean=mean(on_course, na.rm=TRUE), N=n()) %>% arrange(ftask_change) %>% as.data.frame


mod1 <- glmmTMB(on_course ~ fYear,
                    data=FullData.TA,
                family='beta_family')
emmeans(mod1, ~fYear, type='response')
mod1 <- glmmTMB(on_course ~ fYear,
                    data=FullData.TA,
                    family='gaussian')
emmeans(mod1, ~fYear, type='response')
mod2 <- glmmTMB(on_course ~ fYear*ftask_change,
                    data=FullData.TA,
                    family='beta_family')
emmeans(mod2, ~fYear, type='response')
emmeans(mod2, ~fYear, type='response', weights='proportional')

mod2 <- glmmTMB(on_course ~ fYear*ftask_change,
                    data=FullData.TA,
                    family='gaussian')
emmeans(mod2, ~fYear, type='response')
emmeans(mod2, ~fYear, type='response', weights='equal')
emmeans(mod2, ~fYear, type='response', weights='proportional')


mod2 <- glmmTMB(I(on_course^2) ~ fYear*ftask_change + (1|Subject) + (1|id),
                    data=FullData.TA,
                    family='gaussian')
emmeans(mod2, ~fYear, type='response')
simulateResiduals(mod2, plot=TRUE)


emmeans(mod.OC, ~fYear|ftask_change, type='response')
emmeans(mod.OC, ~fYear, type='response')
emmeans(mod.OCa, ~fYear|fYear_level, type='response')
emmeans(mod.OCa, ~fYear, type='response')
## ----end

## inla %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## ---- fitModels-TA-inla
newdata <- with(FullData.TA, expand.grid(fYear=levels(fYear),
                                      ftask_change=levels(ftask_change),
                                      Subject=NA, id=NA,
                                      total_change=NA))
# xmat = model.matrix(~fYear*ftask_change, newdata)
# newdata.xmat=cbind(newdata, xmat)
# n1 <- (newdata.xmat %>% group_by(fYear) %>% summarise(across(is.numeric, mean)))[,-1] %>% as.matrix
# n2 = n1[2,]-n1[1,]
# lincomb <- inla.make.lincombs(as.data.frame(rbind(n1,n2, xmat[1,])))
newdata.pred = FullData.TA %>%
    bind_rows(newdata) %>%
    bind_rows(data.frame(fYear=levels(FullData.TA$fYear))) 
mod.TA.inla <- inla(total_achievement ~ fYear*ftask_change + f(Subject, model='iid') + f(id, model='iid'),
                    data=newdata.pred,
                    #lincomb=lincomb,
                    family='beta',
                    control.predictor=list(link=1, compute=TRUE),
                    #control.inla=list(lincomb.derived.only=TRUE),
                    control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, config=TRUE)
                    )
#mod.TA.inla = inla.hyperpar(mod.TA.inla)
save(mod.TA.inla, file='../data/mod.TA.inla.RData')
save(newdata, file='../data/newdata.TA.inla.RData')

## ----end
## ---- oldInlaTests



#tidyMCMC(cellmeans)



# 
# 
# 
# 
# a=inla.posterior.sample(10,mod.TA.inla)
# (a[[1]]$latent)[(nrow(FullData.TA)+1):nrow(newdata),]
# b=a[[1]]$latent
# 
# 
# a=sapply(
#     mod.TA.inla$marginals.fitted.values[(nrow(FullData.TA)+1):(nrow(FullData.TA)+2)],
#     function(x) inla.posterior.sample(1000,x))
#     
# 
# 
# mod.TA.inla$summary.fitted.values[(nrow(FullData.TA)+1):(nrow(FullData.TA)+2),]
# mod.TA.inla$summary.lincomb.derived
# summary(mod.TA.inla)
# 
# head(mod.TA.inla$summary.lincomb.derived)
# a=inla.posterior.sample(10,mod.TA.inla)
# a[[1]]$latent %>% tail(24)
# 
# inla.emarginal(function(x) sum(x>0)/length(x),mod.TA.inla$marginals.lincomb.derived[[3]])
# inla.emarginal(function(x)(x>0),mod.TA.inla$marginals.lincomb.derived[[3]])
# 
# alpha = mod.TA.inla$marginals.lincomb.derived[[3]]
# 1-inla.pmarginal(0,alpha)
# inla.emarginal(function(x) x>=0.1,alpha)
# 1-inla.pmarginal(0.1,alpha)
# 
# sapply(mod.TA.inla$marginals.lincomb.derived, function(x) 1-inla.pmarginal(0,plogis(x)))
# sapply(mod.TA.inla$marginals.lincomb.derived, function(x) inla.emarginal(function(d) d>=0, x))
# sapply(mod.TA.inla$marginals.lincomb.derived, function(x) inla.emarginal(function(d) d, x))
# 
# sapply(mod.TA.inla$marginals.lincomb.derived[1:2], 
#        function(x) inla.emarginal(function(d) d, inla.tmarginal(function(f) plogis(f), x)))
# 
#        
# ggplot(data.frame(inla.smarginal(alpha)), aes(y=y,x=x)) + 
#     geom_line()

## ----end
## ---- fitModel-TA-inla-summary
load(file='../data/mod.TA.inla.RData')
summary(mod.TA.inla)$fixed %>% knitr::kable()
summary(mod.TA.inla)$hyperpar %>% knitr::kable()
## ----end
## ---- fitModel-TA-inla-diagnostics
load(file='../data/mod.TA.inla.RData')
load(file='../data/newdata.TA.inla.RData')
preds <- lapply(1:nrow(FullData.TA), function(x) inla.rmarginal(250, mod.TA.inla$marginals.fitted.values[[x]]))
preds = do.call('cbind', preds)
resids <- createDHARMa(simulatedResponse = t(preds),
                       observedResponse = FullData.TA$total_achievement,
                       fittedPredictedResponse = apply(preds, 2, median),
                       integerResponse = FALSE)
plot(resids)
DHARMa::testDispersion(resids)

autoplot(mod.TA.inla)
ggplot_inla_residuals(mod.TA.inla, FullData.TA$total_achievement)
ggplot_inla_residuals2(mod.TA.inla, FullData.TA$total_achievement)
## ----end
## ---- fitModel-TA-inla-contrasts1a
load(file='../data/mod.TA.inla.RData')
load(file='../data/newdata.TA.inla.RData')
xmat = model.matrix(~fYear*ftask_change, newdata)
samples = t(sapply(inla.posterior.sample(1000,mod.TA.inla), function(x) tail(x$latent,nrow(newdata))))

cellmeans = plogis(samples %*% t(xmat))
Xmat = diag(ncol(cellmeans))
newdata.Xmat = cbind(newdata,Xmat)
Xmat.year=newdata.Xmat %>% group_by(fYear) %>% summarise(across(is.numeric, mean)) %>%
    dplyr::select(-fYear) %>%
    as.matrix()
MCMCstats <- function(x) {
    hpd = median_hdci(x)
    p=sum(x>0)/length(x)
    data.frame(hpd, 'P>0'=p, 'P<0'=1-p)
}
em.means <- newdata.Xmat %>% dplyr::select(fYear) %>% distinct %>% bind_cols(
    do.call('rbind',apply(as.mcmc(cellmeans %*% (t(Xmat.year))),2,
                          MCMCstats))) %>%
    dplyr::rename(`P>0`=P.0, `P<0`=P.0.1)
em.means %>% as.tibble() %>% knitr::kable()
## Xmat.year.contrast = Xmat.year[2,]-Xmat.year[1,]
## em.contr <- tibble(Contrast='2020-2019',
##                        do.call('rbind',apply(as.mcmc(cellmeans %*% cbind(Xmat.year.contrast)),2,
##                           MCMCstats)) %>%
##     dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))

## em.contr %>% as.tibble() %>% 
##     knitr::kable()

cellmeans.2 = plogis(samples %*% t(xmat) %*% t(Xmat.year))
em.contr <- tibble(Contrast='2020-2019',
                       do.call('rbind',apply(cbind(apply(cellmeans.2, 1, diff)),2,MCMCstats)) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))
em.contr %>% as.tibble() %>% 
    knitr::kable()
## ----end
## ---- fitModel-TA-inla-contrasts1figA
g1 <- em.means %>% as.data.frame() %>%
    ggplot() + 
    geom_pointrange(aes(y=y, x=fYear, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Total achievement (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=Contrast, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % total achievement', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-TA-inla-contrasts1b
w=FullData.TA %>% group_by(ftask_change,fYear) %>% count() %>% ungroup %>% group_by(fYear) %>% mutate(w=n/sum(n)) %>% ungroup %>% dplyr::select(fYear,ftask_change,w) %>% mutate(w1=w*c(1,0), w2=w*c(0,1)) %>% dplyr::select(w1,w2) %>% as.matrix() %>% t

em.means <- newdata.Xmat %>% dplyr::select(fYear) %>% distinct %>% bind_cols(
    #do.call('rbind',apply(as.mcmc(cellmeans %*% (t(Xmat.year))),2,
    do.call('rbind',apply(as.mcmc(cellmeans %*% (t(w))),2,
                          MCMCstats))) %>%
    dplyr::rename(`P>0`=P.0, `P<0`=P.0.1)
em.means %>% as.tibble() %>% knitr::kable()
## Xmat.year.contrast = w[2,]-w[1,]
## em.contr <- tibble(Contrast='2020-2019',
##                        do.call('rbind',apply(as.mcmc(cellmeans %*% cbind(Xmat.year.contrast)),2,
##                           MCMCstats)) %>%
##     dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))

## em.contr %>% as.tibble() %>% 
##     knitr::kable()

cellmeans.2 = plogis(samples %*% t(xmat) %*% t(w))
em.contr <- tibble(Contrast='2020-2019',
                       do.call('rbind',apply(cbind(apply(cellmeans.2, 1, diff)),2,MCMCstats)) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))

em.contr %>% as.tibble() %>% 
    knitr::kable()
## ----end
## ---- fitModel-TA-inla-contrasts1figB
g1 <- em.means %>% as.data.frame() %>%
    ggplot() + 
    geom_pointrange(aes(y=y, x=fYear, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Total achievement (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=Contrast, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % total achievement', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-TA-inla-contrasts2
load(file='../data/mod.TA.inla.RData')
em <- newdata.Xmat %>% dplyr::select(fYear,ftask_change) %>% distinct %>% bind_cols(
    do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat)),2,
                          MCMCstats))) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=y, x=fYear, ymin=ymin, ymax=ymax)) +
    facet_wrap(~ftask_change) + 
    theme_bw()

Xmat.year.by.task = newdata.Xmat %>% 
    group_by(ftask_change) %>% 
    summarise(across(is.numeric, function(x) x[2]-x[1])) %>%
    dplyr::select(-ftask_change) %>%
    as.matrix()
## em.contr <- newdata.Xmat %>% dplyr::select(ftask_change) %>% distinct %>% bind_cols(
##     do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat.year.by.task)),2,
##                           MCMCstats))
## ) %>%
##     dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

cellmeans.2 = plogis(samples %*% t(xmat) %*% t(Xmat))
Xmat.contr = diag(ncol(cellmeans.2))
Xmat.contr = Xmat.contr[seq(2,ncol(cellmeans.2), by=2),] - Xmat.contr[seq(1,ncol(cellmeans.2), by=2),]
em.contr <- newdata.Xmat %>%
    dplyr::select(ftask_change) %>%
    distinct %>%
    bind_cols(
        do.call('rbind', apply(cellmeans.2 %*% t(Xmat.contr),2,MCMCstats))) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

g2 <- em.contr%>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=ftask_change, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % total achievement', labels=function(x) x*100) +
    scale_x_discrete('Task change code') +
    coord_flip() + 
    theme_bw()
g1 + g2

em  %>% knitr::kable()
em.contr %>% knitr::kable() 
## ----end
## ---- fitModel-TA-inla-contrasts3
load(file='../data/mod.TA.inla.RData') 
Xmat.combs = rbind(
    #'0'=Xmat.year.by.task[1,],
    #'5'=Xmat.year.by.task[4,],
    #'6'=Xmat.year.by.task[5,],
    '0+6'=colMeans(Xmat.year.by.task[c(1,5),]),
    #'21'=Xmat.year.by.task[7,],
    #'31'=Xmat.year.by.task[10,],
    '21+31'=colMeans(Xmat.year.by.task[c(7,10),]),
    #'22'=Xmat.year.by.task[8,],
    #'3'=Xmat.year.by.task[2,],
    #'13'=Xmat.year.by.task[6,],
    #'33'=Xmat.year.by.task[11,],
    '3+13+33'=colMeans(Xmat.year.by.task[c(2,6,11),]),
    #'4'=Xmat.year.by.task[3,],
    #'24'=Xmat.year.by.task[9,],
    #'34'=Xmat.year.by.task[12,],
    '4+24'=colMeans(Xmat.year.by.task[c(3,9),]),
    '21+31+22+3+13+33'=colMeans(Xmat.year.by.task[c(7,10,8,2,6,11),]),
    '4+24+5'=colMeans(Xmat.year.by.task[c(3,9,4),]),
    '21+31+22+3+13+33 vs 4+24+5'=colMeans(Xmat.year.by.task[c(7,10,8,2,6,11),]) - colMeans(Xmat.year.by.task[c(3,9,4),])
)

em.combs <- 
    do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat.combs)),2,
                          MCMCstats))
em.combs <- em.combs %>% mutate(Contrast=rownames(.)) %>%
        dplyr::rename('P>0'=P.0, 'P<0'=P.0.1) %>%
        dplyr::select(Contrast, everything()) %>% 
    as.tibble()
em.combs %>% knitr::kable()

em.combs %>% 
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=Contrast, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % total achievement', labels=function(x) x*100) +
    scale_x_discrete('Contrast') +
    coord_flip() + 
    theme_bw()
## ----end

## ---- fitModels-TAa-inla
newdata <- with(FullData.TA, expand.grid(fYear=levels(fYear),
                                      fYear_level=levels(fYear_level),
                                      Subject=NA, id=NA,
                                      total_change=NA))
# xmat = model.matrix(~fYear*ftask_change, newdata)
# newdata.xmat=cbind(newdata, xmat)
# n1 <- (newdata.xmat %>% group_by(fYear) %>% summarise(across(is.numeric, mean)))[,-1] %>% as.matrix
# n2 = n1[2,]-n1[1,]
# lincomb <- inla.make.lincombs(as.data.frame(rbind(n1,n2, xmat[1,])))
newdata.pred = FullData.TA %>% bind_rows(newdata)
mod.TAa.inla <- inla(total_achievement ~ fYear*fYear_level + f(Subject, model='iid') + f(id, model='iid'),
                    data=newdata.pred,
                    #lincomb=lincomb,
                    family='beta',
                    control.predictor=list(link=1, compute=TRUE),
                    #control.inla=list(lincomb.derived.only=TRUE),
                    control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, config=TRUE)
                    )
#mod.TA.inla = inla.hyperpar(mod.TA.inla)
save(mod.TAa.inla, file='../data/mod.TAa.inla.RData')
save(newdata, file='../data/newdata.TAa.inla.RData')
## ----end
## ---- fitModel-TAa-inla-contrasts1
load(file='../data/mod.TAa.inla.RData')
load(file='../data/newdata.TAa.inla.RData')
xmat = model.matrix(~fYear*fYear_level, newdata)
samples = t(sapply(inla.posterior.sample(1000,mod.TAa.inla), function(x) tail(x$latent,nrow(newdata))))

cellmeans = plogis(samples %*% t(xmat))
Xmat = diag(dim(cellmeans)[2])
newdata.Xmat = cbind(newdata,Xmat)
Xmat.year=newdata.Xmat %>% group_by(fYear) %>% summarise(across(is.numeric, mean)) %>%
    dplyr::select(-fYear) %>%
    as.matrix()
MCMCstats <- function(x) {
    hpd = median_hdci(x)
    p=sum(x>0)/length(x)
    data.frame(hpd, 'P>0'=p, 'P<0'=1-p)
}
em.means <- newdata.Xmat %>% dplyr::select(fYear) %>% distinct %>% bind_cols(
    do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat.year)),2,
                          MCMCstats))) %>%
    dplyr::rename(`P>0`=P.0, `P<0`=P.0.1)
em.means %>% as.tibble() %>% knitr::kable()

## Xmat.year.contrast = Xmat.year[2,]-Xmat.year[1,]
## em.contr <- tibble(Contrast='2020-2019',
##                        do.call('rbind',apply(as.mcmc(cellmeans %*% cbind(Xmat.year.contrast)),2,
##                           MCMCstats)) %>%
##     dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))

## em.contr %>% as.tibble() %>% 
##     knitr::kable()


cellmeans.2 = plogis(samples %*% t(xmat) %*% t(Xmat.year))

em.contr <- tibble(Contrast='2020-2019',
                       do.call('rbind',apply(cbind(apply(cellmeans.2, 1, diff)),2,MCMCstats)) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))

em.contr %>% as.tibble() %>% 
    knitr::kable()
## ----end
## ---- fitModel-TAa-inla-contrasts2
load(file='../data/mod.TAa.inla.RData')
load(file='../data/newdata.TAa.inla.RData')
em <- newdata.Xmat %>% dplyr::select(fYear,fYear_level) %>% distinct %>% bind_cols(
    do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat)),2,
                          MCMCstats))) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=y, x=fYear, ymin=ymin, ymax=ymax)) +
    facet_wrap(~fYear_level) + 
    theme_bw()


## Xmat.year.by.yearlevel = newdata.Xmat %>% 
##     group_by(fYear_level) %>% 
##     summarise(across(is.numeric, function(x) x[2]-x[1])) %>%
##     dplyr::select(-fYear_level) %>%
##     as.matrix()
## em.contr <- newdata.Xmat %>% dplyr::select(fYear_level) %>% distinct %>% bind_cols(
##     do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat.year.by.yearlevel)),2,
##                           MCMCstats))
## ) %>%
##     dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

cellmeans.2 = plogis(samples %*% t(xmat) %*% t(Xmat))
Xmat.contr = diag(ncol(cellmeans.2))
Xmat.contr = Xmat.contr[seq(2,ncol(cellmeans.2), by=2),] - Xmat.contr[seq(1,ncol(cellmeans.2), by=2),]
em.contr <- newdata.Xmat %>%
    dplyr::select(fYear_level) %>%
    distinct %>%
    bind_cols(
        do.call('rbind', apply(cellmeans.2 %*% t(Xmat.contr),2,MCMCstats))) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

g2 <- em.contr%>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=fYear_level, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % total achievement', labels=function(x) x*100) +
    scale_x_discrete('Year level') +
    coord_flip() + 
    theme_bw()
g1 + g2

em  %>% knitr::kable()
em.contr %>% knitr::kable()
## ----end

## brms %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## ---- fitModel-TA-brm
mod.brm<- brm(total_achievement ~ fYear*ftask_change + (1|Subject) + (1|id),
               data=FullData.TA,
              family='beta',
              iter=3000,
              warmup=1500,
              thin=5,
              chains=3,
              cores=3,
              prior=c(prior(normal(0,5), class='b'),
                      prior(normal(0.5,5), class='Intercept')
                      ),
              control=list(adapt_delta=0.99))
save(mod.brm, file='../data/mod.brm.RData')
## ----end
## ---- fitModel-TA-brm-summary
load(file='../data/mod.brm.RData')
broom::tidy(mod.brm) %>% dplyr::select(-effect,-component,-group) %>% knitr::kable()  
## ----end
## ---- fitModel-TA-brm-MCMCdiagnostics
load(file='mod.brm.RData')
rstan::stan_trace(mod.brm$fit)
rstan::stan_ac(mod.brm$fit)
rstan::stan_rhat(mod.brm$fit)
rstan::stan_ess(mod.brm$fit)
## ----end
## ---- fitModel-TA-brm-diagnostics
load(file='mod.brm.RData')
preds <- posterior_predict(mod.brm,  nsamples=250,  summary=FALSE)
mod.resids <- createDHARMa(simulatedResponse = t(preds),
                           observedResponse = FullData.TA$total_achievement,
                           fittedPredictedResponse = apply(preds, 2, median),
                           integerResponse = 'beta')
plot(mod.resids)
DHARMa::testDispersion(mod.resids)
## ----end
## ---- fitModel-TA-brm-summary1
broom::tidy(mod.TA) %>% knitr::kable() 
## ----end
## ---- fitModel-TA-brm-contrasts1a
load(file='../data/mod.brm.RData')
em <- emmeans(mod.brm, ~fYear)
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())
em.contr %>% 
    tidy(conf.int=TRUE) %>%
    left_join(P) %>%
    knitr::kable() 
## ----end
## ---- fitModel-TA-brm-contrasts1figA
g1 <- em.means %>% as.data.frame() %>% 
    ggplot() + 
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Total achievement (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % total achievement', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-TA-brm-contrasts1b
load(file='../data/mod.brm.RData')
em <- emmeans(mod.brm, ~fYear, weights='proportional')
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())
em.contr %>% 
    tidy(conf.int=TRUE) %>%
    left_join(P) %>%
    knitr::kable() 
## ----end
## ---- fitModel-TA-brm-contrasts1figB
g1 <- em.means %>% as.data.frame() %>%
    ggplot() + 
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Total achievement (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % total achievement', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-TA-brm-contrasts2
load(file='../data/mod.brm.RData')
em <- emmeans(mod.brm, ~fYear*ftask_change) %>% regrid

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD, ymax=upper.HPD)) +
    facet_wrap(~ftask_change) + 
    theme_bw()
em.contr <- em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE))
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast, ftask_change) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())

g2 <- em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE)) %>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=ftask_change, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % total achievement', labels=function(x) x*100) +
    scale_x_discrete('Task change code') +
    coord_flip() + 
    theme_bw()
g1 + g2

em %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr %>% 
    tidy(conf.int=TRUE) %>% 
    left_join(P) %>%
    dplyr::select(-term,-null.value) %>%
    knitr::kable()

cellmeans.TA.brm <- em %>% as.data.frame() %>% mutate(Response='Total Achievement')
save(cellmeans.TA.brm, file='../data/cellmeans.TA.brm.RData')
effects.TA.brm <- em %>%
    contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE)) %>%
    as.data.frame()  %>% mutate(Response='Total Achievement')
save(effects.TA.brm, file='../data/effects.TA.brm.RData')
## ----end
## ---- fitModel-TA-brm-contrasts3
load(file='../data/mod.brm.RData')
xmat = em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change') %>% coef()
em.contr <- contrast(em, method=list(
    #'0'=xmat[,3],
    #'5'=xmat[,6],
    #'6'=xmat[,7],
    '0+6'=rowMeans(xmat[,c(3,7)]),
    #'21'=xmat[,9],
    #'31'=xmat[,12],
    '21+31'=rowMeans(xmat[,c(9,12)]),
    #'22'=xmat[,10],
    #'3'=xmat[,4],
    #'13'=xmat[,8],
    #'33'=xmat[,13],
    '3+13+33'=rowMeans(xmat[,c(4,8,13)]),
    #'4'=xmat[,5],
    #'24'=xmat[,11],
    #'34'=xmat[,14],
    '4+24'=rowMeans(xmat[,c(5,11)]),
    '21+31+22+3+13+33'=rowMeans(xmat[,c(9,12,10,4,8,13)]),
    '4+24+5'=rowMeans(xmat[,c(4,11,6)]),
    '21+31+22+3+13+33 vs 4+24+5'=rowMeans(xmat[,c(9,12,10,4,8,13)]) - 
        rowMeans(xmat[,c(4,11,6)])
),
infer=c(TRUE,TRUE)
)

P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())
em.contr %>% 
    tidy(conf.int=TRUE) %>% 
    left_join(P) %>%
    dplyr::select(-term,-null.value) %>% 
    knitr::kable()
em.contr %>% 
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % total achievement', labels=function(x) x*100) +
    scale_x_discrete('Contrast') +
    coord_flip() + 
    theme_bw()
## ----end

## ---- fitModels-TA-brma
mod.TAa.brm<- brm(total_achievement ~ fYear*fYear_level + (1|Subject) + (1|id),
               data=FullData.TA,
              family='beta',
              iter=3000,
              warmup=1500,
              thin=5,
              chains=3,
              cores=3,
              control=list(adapt_delta=0.95),
              prior=c(
                  prior(normal(0,5), class='b'),
                  prior(normal(0.5,5), class='Intercept')
              )
              )
save(mod.TAa.brm, file='../data/mod.TAa.brm.RData')
## ----end
## ---- fitModel-TA-brma-diagnostics
load(file='../data/mod.TAa.brm.RData')
mod.resid<-DHARMa::simulateResiduals(mod.TAa,plot = TRUE)
DHARMa::testDispersion(mod.resid) 
## ----end
## ---- fitModel-TA-brma-summary
load(file='../data/mod.TAa.brm.RData')
broom::tidy(mod.TAa.brm) %>% knitr::kable()  
## ----end
## ---- fitModel-TA-brma-contrasts2
load(file='../data/mod.TAa.brm.RData')
em <- emmeans(mod.TAa.brm, ~fYear*fYear_level) %>% regrid

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD, ymax=upper.HPD)) +
    facet_wrap(~fYear_level) + 
    theme_bw()

em.contr <- em %>% contrast(method=list(fYear=c(-1,1)), by='fYear_level', infer=c(TRUE,TRUE))
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast, fYear_level) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())

g2 <- em %>% contrast(method=list(fYear=c(-1,1)), by='fYear_level', infer=c(TRUE,TRUE)) %>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=fYear_level, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % total achievement', labels=function(x) x*100) +
    scale_x_discrete('Year level') +
    coord_flip() + 
    theme_bw()
g1 + g2
 
em %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr %>%
    tidy(conf.int=TRUE) %>%
    left_join(P) %>% 
    dplyr::select(-term,-null.value) %>%
    knitr::kable()
## ----end

## Exam scores -------------------------------------------------------------
## ---- processEXAM
FullData.EXAM = FullData %>% filter(!is.na(exam)) %>% droplevels
## ----end
## glmmTMB %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## ---- fitModels-EXAM-glmmTMB
mod.EXAM <- glmmTMB(exam ~ fYear*ftask_change + (1|Subject) + (1|id),
                  data=FullData.EXAM,
                  family='beta_family')
save(mod.EXAM, file='../data/mod.EXAM.RData')
## ----end
## ---- fitModel-EXAM-glmmTMB-diagnostics
load(file='../data/mod.EXAM.RData')
mod.resid<-DHARMa::simulateResiduals(mod.EXAM,plot = TRUE)
DHARMa::testDispersion(mod.resid)
## ----end
## ---- fitModel-EXAM-glmmTMB-summary
load(file='../data/mod.EXAM.RData')
broom::tidy(mod.EXAM) %>% knitr::kable() 
## ----end
## ---- fitModel-EXAM-glmmTMB-contrasts1a
load(file='../data/mod.EXAM.RData')
em <- emmeans(mod.EXAM, ~fYear)
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
em.contr %>% 
    tidy(conf.int=TRUE) %>% 
    knitr::kable() 
## ----end
## ---- fitModel-EXAM-glmmTMB-contrasts1figA
g1 <- em.means %>% as.data.frame() %>% 
    ggplot() + 
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Exam score (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % exam score', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-EXAM-glmmTMB-contrasts1b
load(file='../data/mod.EXAM.RData') 
em <- emmeans(mod.EXAM, ~fYear, weights='proportional')
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
em.contr %>% 
    tidy(conf.int=TRUE) %>% 
    knitr::kable()
## ----end
## ---- fitModel-EXAM-glmmTMB-contrasts1figB
g1 <- em.means %>% as.data.frame() %>% 
    ggplot() + 
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Exam score (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % exam score', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-EXAM-glmmTMB-contrasts2
em <- emmeans(mod.EXAM, ~fYear*ftask_change) %>% regrid
g1 <- em %>% as.data.frame %>%  
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.CL, ymax=upper.CL)) +
    facet_wrap(~ftask_change) + 
    theme_bw()

g2 <- em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE)) %>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=ftask_change, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % exam score', labels=function(x) x*100) +
    scale_x_discrete('Task change code') +
    coord_flip() + 
    theme_bw()
g1 + g2
em %>% tidy(conf.int=TRUE) %>% knitr::kable()
em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE)) %>% 
    tidy(conf.int=TRUE) %>% 
    dplyr::select(-term,-null.value) %>%
    knitr::kable()

cellmeans.EXAM.glmmTMB <- em %>% as.data.frame() %>% mutate(Response='Exam scores')
save(cellmeans.EXAM.glmmTMB, file='../data/cellmeans.EXAM.glmmTMB.RData')
effects.EXAM.glmmTMB <- em %>%
    contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE)) %>%
    as.data.frame()  %>% mutate(Response='Exam scores')
save(effects.EXAM.glmmTMB, file='../data/effects.EXAM.glmmTMB.RData')
## ----end
## ---- fitModel-EXAM-glmmTMB-contrasts3
xmat = em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change') %>% coef()
 
em.contr <- contrast(em, method=list(
    #'0'=xmat[,3],
    #'5'=xmat[,6],
    #'21'=xmat[,8],
    #'31'=xmat[,11],
    '21+31'=rowMeans(xmat[,c(8,11)]),
    #'22'=xmat[,9],
    #'3'=xmat[,4],
    #'13'=xmat[,7],
    #'33'=xmat[,12],
    '3+13+33'=rowMeans(xmat[,c(4,7,12)]),
    #'4'=xmat[,5],
    #'24'=xmat[,10],
    #'34'=xmat[,13],
    '4+24'=rowMeans(xmat[,c(5,10)]),
    '21+31+22+3+13+33'=rowMeans(xmat[,c(8,11,9,4,7,12)]),
    '4+24+5'=rowMeans(xmat[,c(4,10,6)]),
    '21+31+22+3+13+33 vs 4+24+5'=rowMeans(xmat[,c(8,11,9,4,7,12)]) - 
        rowMeans(xmat[,c(4,10,6)])
),
infer=c(TRUE,TRUE)
)

em.contr %>% tidy(conf.int=TRUE) %>% dplyr::select(-term,-null.value) %>% knitr::kable()
em.contr %>% 
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % exam score', labels=function(x) x*100) +
    scale_x_discrete('Contrast') +
    coord_flip() + 
    theme_bw() 
## ----end

## ---- fitModels-EXAM-glmmTMBa
mod.EXAMa <- glmmTMB(exam ~ fYear*fYear_level + (1|Subject) + (1|id),
               data=FullData.EXAM,
               family='beta_family')
save(mod.EXAMa, file='../data/mod.EXAMa.RData') 
## ----end
## ---- fitModel-EXAM-glmmTMBa-diagnostics
load(file='../data/mod.EXAMa.RData')  
mod.resid<-DHARMa::simulateResiduals(mod.EXAMa,plot = TRUE)
DHARMa::testDispersion(mod.resid) 
## ----end
## ---- fitModel-EXAM-glmmTMBa-summary
load(file='../data/mod.EXAMa.RData') 
broom::tidy(mod.EXAMa) %>% knitr::kable()   
## ----end
## ---- fitModel-EXAM-glmmTMBa-contrasts2
load(file='../data/mod.EXAMa.RData')  
em <- emmeans(mod.EXAMa, ~fYear*fYear_level) %>% regrid

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.CL, ymax=upper.CL)) +
    facet_wrap(~fYear_level) + 
    theme_bw()


g2 <- em %>% contrast(method=list(fYear=c(-1,1)), by='fYear_level', infer=c(TRUE,TRUE)) %>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=fYear_level, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % total achievement', labels=function(x) x*100) +
    scale_x_discrete('Year level') +
    coord_flip() + 
    theme_bw()
g1 + g2
 
em %>% tidy(conf.int=TRUE) %>% knitr::kable()
em %>% contrast(method=list(fYear=c(-1,1)), by='fYear_level', infer=c(TRUE,TRUE)) %>% 
    tidy(conf.int=TRUE) %>% 
    dplyr::select(-term,-null.value) %>%
    knitr::kable()
## ----end

## inla %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## ---- fitModels-EXAM-inla
FullData.EXAM.exam = FullData.EXAM %>% filter(!is.na(exam)) %>% droplevels
newdata <- with(FullData.EXAM.exam, expand.grid(fYear=levels(fYear),
                                      ftask_change=levels(ftask_change),
                                      Subject=NA, id=NA,
                                      exam=NA))
newdata.pred = FullData.EXAM.exam %>% bind_rows(newdata)
mod.EXAM.inla <- inla(exam ~ fYear*ftask_change + f(Subject, model='iid') + f(id, model='iid'),
                    data=newdata.pred,
                    family='beta',
                    control.predictor=list(link=1, compute=TRUE),
                    control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, config=TRUE)
                    )
save(mod.EXAM.inla, file='../data/mod.EXAM.inla.RData')
save(newdata, file='../data/newdata.EXAM.inla.RData')
save(FullData.EXAM.exam, file='../data/FullData.EXAM.exam.RData')
## ----end
## ---- fitModel-EXAM-inla-summary
load(file='../data/mod.EXAM.inla.RData')
summary(mod.EXAM.inla)$fixed %>% knitr::kable()
summary(mod.EXAM.inla)$hyperpar %>% knitr::kable()
## ----end
## ---- fitModel-EXAM-inla-diagnostics
load(file='../data/mod.EXAM.inla.RData')
load(file='../data/newdata.EXAM.inla.RData')
load(file='../data/FullData.EXAM.exam.RData')
preds <- lapply(1:nrow(FullData.EXAM.exam), function(x) inla.rmarginal(250, mod.EXAM.inla$marginals.fitted.values[[x]]))
preds = do.call('cbind', preds)
resids <- createDHARMa(simulatedResponse = t(preds),
                       observedResponse = FullData.EXAM.exam$exam,
                       fittedPredictedResponse = apply(preds, 2, median),
                       integerResponse = FALSE)
plot(resids)
DHARMa::testDispersion(resids)

autoplot(mod.EXAM.inla)
ggplot_inla_residuals(mod.EXAM.inla, FullData.EXAM.exam$exam)
ggplot_inla_residuals2(mod.EXAM.inla, FullData.EXAM.exam$exam)
## ----end
## ---- fitModel-EXAM-inla-contrasts1a
load(file='../data/mod.EXAM.inla.RData')
load(file='../data/newdata.EXAM.inla.RData')
xmat = model.matrix(~fYear*ftask_change, newdata)
samples = t(sapply(inla.posterior.sample(1000,mod.EXAM.inla), function(x) tail(x$latent,nrow(newdata))))

cellmeans = plogis(samples %*% t(xmat))
Xmat = diag(dim(cellmeans)[2])
newdata.Xmat = cbind(newdata,Xmat)
Xmat.year=newdata.Xmat %>% group_by(fYear) %>% summarise(across(is.numeric, mean)) %>%
    dplyr::select(-fYear) %>%
    as.matrix()
MCMCstats <- function(x) {
    hpd = median_hdci(x)
    p=sum(x>0)/length(x)
    data.frame(hpd, 'P>0'=p, 'P<0'=1-p)
}
em.means <- newdata.Xmat %>% dplyr::select(fYear) %>% distinct %>% bind_cols(
    do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat.year)),2,
                          MCMCstats))) %>%
    dplyr::rename(`P>0`=P.0, `P<0`=P.0.1)
em.means %>% as.tibble() %>% knitr::kable()
## Xmat.year.contrast = Xmat.year[2,]-Xmat.year[1,]
## em.contr <- tibble(Contrast='2020-2019',
##                        do.call('rbind',apply(as.mcmc(cellmeans %*% cbind(Xmat.year.contrast)),2,
##                           MCMCstats)) %>%
##     dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))

## em.contr %>% as.tibble() %>% 
##     knitr::kable()

cellmeans.2 = plogis(samples %*% t(xmat) %*% t(Xmat.year))
em.contr <- tibble(Contrast='2020-2019',
                       do.call('rbind',apply(cbind(apply(cellmeans.2, 1, diff)),2,MCMCstats)) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))
em.contr %>% as.tibble() %>% 
    knitr::kable()
## ----end
## ---- fitModel-EXAM-inla-contrasts1figA
g1 <- em.means %>% as.data.frame() %>%
    ggplot() + 
    geom_pointrange(aes(y=y, x=fYear, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Exam score (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=Contrast, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % exam score', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-EXAM-inla-contrasts1b
w=FullData.EXAM %>% filter(!is.na(exam)) %>% droplevels %>% group_by(ftask_change,fYear) %>% count() %>% ungroup %>% group_by(fYear) %>% mutate(w=n/sum(n)) %>% ungroup %>% dplyr::select(fYear,ftask_change,w) %>% mutate(w1=w*c(1,0), w2=w*c(0,1)) %>% dplyr::select(w1,w2) %>% as.matrix() %>% t

em.means <- newdata.Xmat %>% dplyr::select(fYear) %>% distinct %>% bind_cols(
    #do.call('rbind',apply(as.mcmc(cellmeans %*% (t(Xmat.year))),2,
    do.call('rbind',apply(as.mcmc(cellmeans %*% (t(w))),2,
                          MCMCstats))) %>%
    dplyr::rename(`P>0`=P.0, `P<0`=P.0.1)
em.means %>% as.tibble() %>% knitr::kable()
## Xmat.year.contrast = w[2,]-w[1,]
## em.contr <- tibble(Contrast='2020-2019',
##                        do.call('rbind',apply(as.mcmc(cellmeans %*% cbind(Xmat.year.contrast)),2,
##                           MCMCstats)) %>%
##     dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))

## em.contr %>% as.tibble() %>% 
##     knitr::kable()

cellmeans.2 = plogis(samples %*% t(xmat) %*% t(w))
em.contr <- tibble(Contrast='2020-2019',
                       do.call('rbind',apply(cbind(apply(cellmeans.2, 1, diff)),2,MCMCstats)) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))

em.contr %>% as.tibble() %>% 
    knitr::kable()
## ----end
## ---- fitModel-EXAM-inla-contrasts1figB
g1 <- em.means %>% as.data.frame() %>%
    ggplot() + 
    geom_pointrange(aes(y=y, x=fYear, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Exam score (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=Contrast, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % exam score', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-EXAM-inla-contrasts2
load(file='../data/mod.EXAM.inla.RData')
em <- newdata.Xmat %>% dplyr::select(fYear,ftask_change) %>% distinct %>% bind_cols(
    do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat)),2,
                          MCMCstats))) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=y, x=fYear, ymin=ymin, ymax=ymax)) +
    facet_wrap(~ftask_change) + 
    theme_bw()


Xmat.year.by.task = newdata.Xmat %>% 
    group_by(ftask_change) %>% 
    summarise(across(is.numeric, function(x) x[2]-x[1])) %>%
    dplyr::select(-ftask_change) %>%
    as.matrix()
## em.contr <- newdata.Xmat %>% dplyr::select(ftask_change) %>% distinct %>% bind_cols(
##     do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat.year.by.task)),2,
##                           MCMCstats))
## ) %>%
##     dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

cellmeans.2 = plogis(samples %*% t(xmat) %*% t(Xmat))
Xmat.contr = diag(ncol(cellmeans.2))
Xmat.contr = Xmat.contr[seq(2,ncol(cellmeans.2), by=2),] - Xmat.contr[seq(1,ncol(cellmeans.2), by=2),]
em.contr <- newdata.Xmat %>%
    dplyr::select(ftask_change) %>%
    distinct %>%
    bind_cols(
        do.call('rbind', apply(cellmeans.2 %*% t(Xmat.contr),2,MCMCstats))) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

g2 <- em.contr%>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=ftask_change, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % exam score', labels=function(x) x*100) +
    scale_x_discrete('Task change code') +
    coord_flip() + 
    theme_bw()
g1 + g2

em  %>% knitr::kable()
em.contr %>% knitr::kable()
## ----end
## ---- fitModel-EXAM-inla-contrasts3
load(file='../data/mod.EXAM.inla.RData')
Xmat.combs = rbind(
    #'0'=Xmat.year.by.task[1,],
    #'5'=Xmat.year.by.task[4,],
    #'6'=Xmat.year.by.task[5,],
    #'0+6'=colMeans(Xmat.year.by.task[c(1,5),]),
    #'21'=Xmat.year.by.task[6,],
    #'31'=Xmat.year.by.task[9,],
    '21+31'=colMeans(Xmat.year.by.task[c(6,9),]),
    #'22'=Xmat.year.by.task[7,],
    #'3'=Xmat.year.by.task[2,],
    #'13'=Xmat.year.by.task[5,],
    #'33'=Xmat.year.by.task[10,],
    '3+13+33'=colMeans(Xmat.year.by.task[c(2,5,10),]),
    #'4'=Xmat.year.by.task[3,],
    #'24'=Xmat.year.by.task[8,],
    #'34'=Xmat.year.by.task[11,],
    '4+24'=colMeans(Xmat.year.by.task[c(3,8),]),
    '21+31+22+3+13+33'=colMeans(Xmat.year.by.task[c(6,9,7,2,5,10),]),
    '4+24+5'=colMeans(Xmat.year.by.task[c(3,8,4),]),
    '21+31+22+3+13+33 vs 4+24+5'=colMeans(Xmat.year.by.task[c(6,9,7,2,5,10),]) - colMeans(Xmat.year.by.task[c(3,8,4),])
)

em.combs <- 
    do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat.combs)),2,
                          MCMCstats))
em.combs <- em.combs %>% mutate(Contrast=rownames(.)) %>%
        dplyr::rename('P>0'=P.0, 'P<0'=P.0.1) %>%
        dplyr::select(Contrast, everything()) %>% 
    as.tibble()
em.combs %>% knitr::kable()

em.combs %>% 
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=Contrast, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % exam score', labels=function(x) x*100) +
    scale_x_discrete('Contrast') +
    coord_flip() + 
    theme_bw()
## ----end

## ---- fitModels-EXAMa-inla
newdata <- with(FullData.EXAM, expand.grid(fYear=levels(fYear),
                                      fYear_level=levels(fYear_level),
                                      Subject=NA, id=NA,
                                      exam=NA)) 
# xmat = model.matrix(~fYear*ftask_change, newdata)
# newdata.xmat=cbind(newdata, xmat)
# n1 <- (newdata.xmat %>% group_by(fYear) %>% summarise(across(is.numeric, mean)))[,-1] %>% as.matrix
# n2 = n1[2,]-n1[1,]
# lincomb <- inla.make.lincombs(as.data.frame(rbind(n1,n2, xmat[1,])))
newdata.pred = FullData.EXAM %>% bind_rows(newdata)
mod.EXAMa.inla <- inla(exam ~ fYear*fYear_level + f(Subject, model='iid') + f(id, model='iid'),
                    data=newdata.pred,
                    #lincomb=lincomb,
                    family='beta',
                    control.predictor=list(link=1, compute=TRUE),
                    #control.inla=list(lincomb.derived.only=TRUE),
                    control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, config=TRUE)
                    )
#mod.EXAM.inla = inla.hyperpar(mod.EXAM.inla)
save(mod.EXAMa.inla, file='../data/mod.EXAMa.inla.RData')
save(newdata, file='../data/newdata.EXAMa.inla.RData')
## ----end
## ---- fitModel-EXAMa-inla-contrasts1
load(file='../data/mod.EXAMa.inla.RData') 
load(file='../data/newdata.EXAMa.inla.RData')
xmat = model.matrix(~fYear*fYear_level, newdata)
samples = t(sapply(inla.posterior.sample(1000,mod.EXAMa.inla), function(x) tail(x$latent,nrow(newdata))))

cellmeans = plogis(samples %*% t(xmat))
Xmat = diag(dim(cellmeans)[2])
newdata.Xmat = cbind(newdata,Xmat)
Xmat.year=newdata.Xmat %>% group_by(fYear) %>% summarise(across(is.numeric, mean)) %>%
    dplyr::select(-fYear) %>%
    as.matrix()
MCMCstats <- function(x) {
    hpd = median_hdci(x)
    p=sum(x>0)/length(x)
    data.frame(hpd, 'P>0'=p, 'P<0'=1-p)
}
em.means <- newdata.Xmat %>% dplyr::select(fYear) %>% distinct %>% bind_cols(
    do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat.year)),2,
                          MCMCstats))) %>%
    dplyr::rename(`P>0`=P.0, `P<0`=P.0.1)
em.means %>% as.tibble() %>% knitr::kable()
## Xmat.year.contrast = Xmat.year[2,]-Xmat.year[1,]
## em.contr <- tibble(Contrast='2020-2019',
##                        do.call('rbind',apply(as.mcmc(cellmeans %*% cbind(Xmat.year.contrast)),2,
##                           MCMCstats)) %>%
##     dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))

## em.contr %>% as.tibble() %>% 
##     knitr::kable()

cellmeans.2 = plogis(samples %*% t(xmat) %*% t(Xmat.year))

em.contr <- tibble(Contrast='2020-2019',
                       do.call('rbind',apply(cbind(apply(cellmeans.2, 1, diff)),2,MCMCstats)) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))

em.contr %>% as.tibble() %>% 
    knitr::kable()
## ----end
## ---- fitModel-EXAMa-inla-contrasts2
load(file='../data/mod.EXAMa.inla.RData') 
load(file='../data/newdata.EXAMa.inla.RData')
em <- newdata.Xmat %>% dplyr::select(fYear,fYear_level) %>% distinct %>% bind_cols(
    do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat)),2,
                          MCMCstats))) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=y, x=fYear, ymin=ymin, ymax=ymax)) +
    facet_wrap(~fYear_level) + 
    theme_bw()


## Xmat.year.by.yearlevel = newdata.Xmat %>% 
##     group_by(fYear_level) %>% 
##     summarise(across(is.numeric, function(x) x[2]-x[1])) %>%
##     dplyr::select(-fYear_level) %>%
##     as.matrix()
## em.contr <- newdata.Xmat %>% dplyr::select(fYear_level) %>% distinct %>% bind_cols(
##     do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat.year.by.yearlevel)),2,
##                           MCMCstats))
## ) %>%
##     dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

cellmeans.2 = plogis(samples %*% t(xmat) %*% t(Xmat))
Xmat.contr = diag(ncol(cellmeans.2))
Xmat.contr = Xmat.contr[seq(2,ncol(cellmeans.2), by=2),] - Xmat.contr[seq(1,ncol(cellmeans.2), by=2),]
em.contr <- newdata.Xmat %>%
    dplyr::select(fYear_level) %>%
    distinct %>%
    bind_cols(
        do.call('rbind', apply(cellmeans.2 %*% t(Xmat.contr),2,MCMCstats))) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

g2 <- em.contr%>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=fYear_level, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % total achievement', labels=function(x) x*100) +
    scale_x_discrete('Year level') +
    coord_flip() + 
    theme_bw()
g1 + g2

em  %>% knitr::kable()
em.contr %>% knitr::kable()
## ----end

## brms %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## ---- fitModel-EXAM-brm
mod.EXAM.brm<- brm(exam ~ fYear*ftask_change + (1|Subject) + (1|id),
               data=FullData.EXAM,
              family='beta',
              iter=3000,
              warmup=1500,
              thin=5,
              chains=3,
              cores=3,
              prior=c(prior(normal(0,5), class='b'),
                      prior(normal(0.5,5), class='Intercept')
              ),
              control=list(adapt_delta=0.99))
save(mod.EXAM.brm, file='../data/mod.EXAM.brm.RData')
## ----end
## ---- fitModel-EXAM-brm-summary
load(file='../data/mod.EXAM.brm.RData')
broom::tidy(mod.EXAM.brm) %>% dplyr::select(-effect,-component,-group) %>% knitr::kable()  
## ----end
## ---- fitModel-EXAM-brm-MCMCdiagnostics
load(file='../data/mod.EXAM.brm.RData')
rstan::stan_trace(mod.EXAM.brm$fit)
rstan::stan_ac(mod.EXAM.brm$fit)
rstan::stan_rhat(mod.EXAM.brm$fit)
rstan::stan_ess(mod.EXAM.brm$fit)
## ----end
## ---- fitModel-EXAM-brm-diagnostics
load(file='../data/mod.EXAM.brm.RData')
preds <- posterior_predict(mod.EXAM.brm,  nsamples=250,  summary=FALSE)
mod.resids <- createDHARMa(simulatedResponse = t(preds),
                           observedResponse = FullData.EXAM$exam,
                           fittedPredictedResponse = apply(preds, 2, median),
                           integerResponse = 'beta')
plot(mod.resids)
DHARMa::testDispersion(mod.resids)
## ----end
## ---- fitModel-EXAM-brm-summary1
broom::tidy(mod.EXAM.brm) %>% knitr::kable() 
## ----end
## ---- fitModel-EXAM-brm-contrasts1a
load(file='../data/mod.EXAM.brm.RData')
em <- emmeans(mod.EXAM.brm, ~fYear)
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())
em.contr %>% 
    tidy(conf.int=TRUE) %>%
    left_join(P) %>%
    knitr::kable() 
## ----end
## ---- fitModel-EXAM-brm-contrasts1figA
g1 <- em.means %>% as.data.frame() %>% 
    ggplot() + 
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Exam (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % total achievement', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-EXAM-brm-contrasts1b
load(file='../data/mod.EXAM.brm.RData')
em <- emmeans(mod.EXAM.brm, ~fYear, weights='proportional')
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())
em.contr %>% 
    tidy(conf.int=TRUE) %>%
    left_join(P) %>%
    knitr::kable() 
## ----end
## ---- fitModel-EXAM-brm-contrasts1figB
g1 <- em.means %>% as.data.frame() %>% 
    ggplot() + 
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Exam (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % total achievement', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-EXAM-brm-contrasts2
load(file='../data/mod.EXAM.brm.RData')
em <- emmeans(mod.EXAM.brm, ~fYear*ftask_change) %>% regrid

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD, ymax=upper.HPD)) +
    facet_wrap(~ftask_change) + 
    theme_bw()
em.contr <- em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE))
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast, ftask_change) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())

g2 <- em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE)) %>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=ftask_change, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % exam score', labels=function(x) x*100) +
    scale_x_discrete('Task change code') +
    coord_flip() + 
    theme_bw()
g1 + g2

em %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr %>% 
    tidy(conf.int=TRUE) %>% 
    left_join(P) %>%
    dplyr::select(-term,-null.value) %>%
    knitr::kable()

cellmeans.EXAM.brm <- em %>% as.data.frame() %>% mutate(Response='Total Achievement')
save(cellmeans.EXAM.brm, file='../data/cellmeans.EXAM.brm.RData')
effects.EXAM.brm <- em %>%
    contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE)) %>%
    as.data.frame()  %>% mutate(Response='Total Achievement')
save(effects.EXAM.brm, file='../data/effects.EXAM.brm.RData')

## ----end
## ---- fitModel-EXAM-brm-contrasts3
load(file='../data/mod.EXAM.brm.RData')
xmat = em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change') %>% coef()
em.contr <- contrast(em, method=list(
    #'0'=xmat[,3],
    #'5'=xmat[,6],
    #'6'=xmat[,7],
    #'0+6'=rowMeans(xmat[,c(3,7)]),
    #'21'=xmat[,9],
    #'31'=xmat[,12],
    '21+31'=rowMeans(xmat[,c(8,11)]),
    #'22'=xmat[,10],
    #'3'=xmat[,4],
    #'13'=xmat[,8],
    #'33'=xmat[,13],
    '3+13+33'=rowMeans(xmat[,c(4,7,12)]),
    #'4'=xmat[,5],
    #'24'=xmat[,11],
    #'34'=xmat[,14],
    '4+24'=rowMeans(xmat[,c(5,10)]),
    '21+31+22+3+13+33'=rowMeans(xmat[,c(8,11,9,4,7,12)]),
    '4+24+5'=rowMeans(xmat[,c(4,10,6)]),
    '21+31+22+3+13+33 vs 4+24+5'=rowMeans(xmat[,c(8,11,9,4,7,12)]) - 
        rowMeans(xmat[,c(4,10,6)])
),
infer=c(TRUE,TRUE)
)

P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())
em.contr %>% 
    tidy(conf.int=TRUE) %>% 
    left_join(P) %>%
    dplyr::select(-term,-null.value) %>% 
    knitr::kable()
em.contr %>% 
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % exam score', labels=function(x) x*100) +
    scale_x_discrete('Contrast') +
    coord_flip() + 
    theme_bw()
## ----end

## ---- fitModels-EXAM-brma
mod.EXAMa.brm<- brm(exam ~ fYear*fYear_level + (1|Subject) + (1|id),
               data=FullData.EXAM,
              family='beta',
              iter=3000,
              warmup=1500,
              thin=5,
              chains=3,
              cores=3,
              control=list(adapt_delta=0.95),
              prior=c(
                  prior(normal(0,5), class='b'),
                  prior(normal(0.5,5), class='Intercept')
              )
              )
save(mod.EXAMa.brm, file='../data/mod.EXAMa.brm.RData')
## ----end
## ---- fitModel-EXAM-brma-diagnostics
load(file='../data/mod.EXAMa.brm.RData')
mod.resid<-DHARMa::simulateResiduals(mod.EXAMa,plot = TRUE)
DHARMa::testDispersion(mod.resid) 
## ----end
## ---- fitModel-EXAM-brma-summary
load(file='../data/mod.EXAMa.brm.RData')
broom::tidy(mod.EXAMa.brm) %>% knitr::kable()  
## ----end
## ---- fitModel-EXAM-brma-contrasts2
load(file='../data/mod.EXAMa.brm.RData')
em <- emmeans(mod.EXAMa.brm, ~fYear*fYear_level) %>% regrid

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD, ymax=upper.HPD)) +
    facet_wrap(~fYear_level) + 
    theme_bw()

em.contr <- em %>% contrast(method=list(fYear=c(-1,1)), by='fYear_level', infer=c(TRUE,TRUE))
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast, fYear_level) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())

g2 <- em %>% contrast(method=list(fYear=c(-1,1)), by='fYear_level', infer=c(TRUE,TRUE)) %>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=fYear_level, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % exam score', labels=function(x) x*100) +
    scale_x_discrete('Year level') +
    coord_flip() + 
    theme_bw()
g1 + g2
 
em %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr %>%
    tidy(conf.int=TRUE) %>%
    left_join(P) %>% 
    dplyr::select(-term,-null.value) %>%
    knitr::kable()
## ----end

## On course -------------------------------------------------------------
## ---- processOC
FullData.OC = FullData %>% filter(!is.na(on_course)) %>% droplevels
## ----end
## glmmTMB %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## ---- fitModels-OC-glmmTMB
mod.OC <- glmmTMB(on_course ~ fYear*ftask_change + (1|Subject) + (1|id),
                    data=FullData.OC,
                    family='beta_family')
save(mod.OC, file='../data/mod.OC.RData')
## ----end
## ---- fitModel-OC-glmmTMB-diagnostics
load(file='../data/mod.OC.RData')
mod.resid<-DHARMa::simulateResiduals(mod.OC,plot = TRUE)
DHARMa::testDispersion(mod.resid)
## ----end
## ---- fitModel-OC-glmmTMB-summary
load(file='../data/mod.OC.RData')
broom::tidy(mod.OC) %>% knitr::kable() 
## ----end
## ---- fitModel-OC-glmmTMB-contrasts1a
load(file='../data/mod.OC.RData')
em <- emmeans(mod.OC, ~fYear)
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
em.contr %>% 
    tidy(conf.int=TRUE) %>% 
    knitr::kable()
## ----end
## ---- fitModel-OC-glmmTMB-contrasts1figA
g1 <- em.means %>% as.data.frame() %>%
    ggplot() + 
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('On course score (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % on course score', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-OC-glmmTMB-contrasts1b
load(file='../data/mod.OC.RData')
em <- emmeans(mod.OC, ~fYear, weights='proportional')
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
em.contr %>% 
    tidy(conf.int=TRUE) %>% 
    knitr::kable()
## ----end
## ---- fitModel-OC-glmmTMB-contrasts1figB
g1 <- em.means %>% as.data.frame() %>%
    ggplot() + 
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('On course score (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % on course score', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-OC-glmmTMB-contrasts2
em <- emmeans(mod.OC, ~fYear*ftask_change) %>% regrid
g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.CL, ymax=upper.CL)) +
    facet_wrap(~ftask_change) + 
    theme_bw()

g2 <- em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE)) %>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=ftask_change, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % on course score', labels=function(x) x*100) +
    scale_x_discrete('Task change code') +
    coord_flip() + 
    theme_bw()
g1 + g2
em %>% tidy(conf.int=TRUE) %>% knitr::kable()
em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE)) %>% 
    tidy(conf.int=TRUE) %>% 
    dplyr::select(-term,-null.value) %>%
    knitr::kable()

cellmeans.OC.glmmTMB <- em %>% as.data.frame() %>% mutate(Response='On course scores')
save(cellmeans.OC.glmmTMB, file='../data/cellmeans.OC.glmmTMB.RData')
effects.OC.glmmTMB <- em %>%
    contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE)) %>%
    as.data.frame()  %>% mutate(Response='On course scores')
save(effects.OC.glmmTMB, file='../data/effects.OC.glmmTMB.RData')
## ----end
## ---- fitModel-OC-glmmTMB-contrasts3
xmat = em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change') %>% coef()
em.contr <- contrast(em, method=list(
    #'0'=xmat[,3],
    #'5'=xmat[,6],
    #'6'=xmat[,7],
    '0+6'=rowMeans(xmat[,c(3,7)]),
    #'21'=xmat[,9],
    #'31'=xmat[,12],
    '21+31'=rowMeans(xmat[,c(9,12)]),
    #'22'=xmat[,10],
    #'3'=xmat[,4],
    #'13'=xmat[,8],
    #'33'=xmat[,13],
    '3+13+33'=rowMeans(xmat[,c(4,8,13)]),
    #'4'=xmat[,5],
    #'24'=xmat[,11],
    #'34'=xmat[,14],
    '4+24'=rowMeans(xmat[,c(5,11)]),
    '21+31+22+3+13+33'=rowMeans(xmat[,c(9,12,10,4,8,13)]),
    '4+24+5'=rowMeans(xmat[,c(4,11,6)]),
    '21+31+22+3+13+33 vs 4+24+5'=rowMeans(xmat[,c(9,12,10,4,8,12)]) - 
        rowMeans(xmat[,c(4,11,6)])
),
infer=c(TRUE,TRUE)
)


em.contr %>% tidy(conf.int=TRUE) %>% dplyr::select(-term,-null.value) %>% knitr::kable()
em.contr %>% 
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % on course score', labels=function(x) x*100) +
    scale_x_discrete('Contrast') +
    coord_flip() + 
    theme_bw() 
## ----end

## ---- fitModels-OC-glmmTMBa
mod.OCa <- glmmTMB(on_course ~ fYear*fYear_level + (1|Subject) + (1|id),
               data=FullData.OC,
               family='beta_family')
save(mod.OCa, file='../data/mod.OCa.RData')
## ----end
## ---- fitModel-OC-glmmTMBa-diagnostics
load(file='../data/mod.OCa.RData')
mod.resid<-DHARMa::simulateResiduals(mod.OCa,plot = TRUE)
DHARMa::testDispersion(mod.resid) 
## ----end
## ---- fitModel-OC-glmmTMBa-summary
load(file='../data/mod.OCa.RData')
broom::tidy(mod.OCa) %>% knitr::kable()  
## ----end
## ---- fitModel-OC-glmmTMBa-contrasts2
load(file='../data/mod.OCa.RData')
em <- emmeans(mod.OCa, ~fYear*fYear_level) %>% regrid

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.CL, ymax=upper.CL)) +
    facet_wrap(~fYear_level) + 
    theme_bw()


g2 <- em %>% contrast(method=list(fYear=c(-1,1)), by='fYear_level', infer=c(TRUE,TRUE)) %>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=fYear_level, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % total achievement', labels=function(x) x*100) +
    scale_x_discrete('Year level') +
    coord_flip() + 
    theme_bw()
g1 + g2
 
em %>% tidy(conf.int=TRUE) %>% knitr::kable()
em %>% contrast(method=list(fYear=c(-1,1)), by='fYear_level', infer=c(TRUE,TRUE)) %>% 
    tidy(conf.int=TRUE) %>% 
    dplyr::select(-term,-null.value) %>%
    knitr::kable()
## ----end

## inla %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## ---- fitModels-OC-inla
FullData.OC.on_course = FullData.OC %>% filter(!is.na(on_course)) %>% droplevels
newdata <- with(FullData.OC.on_course, expand.grid(fYear=levels(fYear),
                                      ftask_change=levels(ftask_change),
                                      Subject=NA, id=NA,
                                      on_course=NA))
newdata.pred = FullData.OC.on_course %>% bind_rows(newdata)
mod.OC.inla <- inla(on_course ~ fYear*ftask_change + f(Subject, model='iid') + f(id, model='iid'),
                    data=newdata.pred,
                    family='beta',
                    control.predictor=list(link=1, compute=TRUE),
                    control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, config=TRUE)
                    ) 
save(mod.OC.inla, file='../data/mod.OC.inla.RData')
save(newdata, file='../data/newdata.OC.inla.RData')
save(FullData.OC.on_course, file='../data/FullData.OC.on_course.RData')
## ----end
## ---- fitModel-OC-inla-summary
load(file='../data/mod.OC.inla.RData') 
summary(mod.OC.inla)$fixed %>% knitr::kable()
summary(mod.OC.inla)$hyperpar %>% knitr::kable()
## ----end
## ---- fitModel-OC-inla-diagnostics
load(file='../data/mod.OC.inla.RData') 
load(file='../data/newdata.OC.inla.RData')
load(file='../data/FullData.OC.on_course.RData')
preds <- lapply(1:nrow(FullData.OC.on_course), function(x) inla.rmarginal(250, mod.OC.inla$marginals.fitted.values[[x]]))
preds = do.call('cbind', preds)
resids <- createDHARMa(simulatedResponse = t(preds),
                       observedResponse = FullData.OC.on_course$on_course,
                       fittedPredictedResponse = apply(preds, 2, median),
                       integerResponse = FALSE)
plot(resids)
DHARMa::testDispersion(resids)

autoplot(mod.OC.inla)
ggplot_inla_residuals(mod.OC.inla, FullData.OC.on_course$on_course)
ggplot_inla_residuals2(mod.OC.inla, FullData.OC.on_course$on_course)
## ----end
## ---- fitModel-OC-inla-contrasts1a
load(file='../data/mod.OC.inla.RData')   
load(file='../data/newdata.OC.inla.RData')
xmat = model.matrix(~fYear*ftask_change, newdata)
samples = t(sapply(inla.posterior.sample(1000,mod.OC.inla), function(x) tail(x$latent,nrow(newdata))))

cellmeans = plogis(samples %*% t(xmat))
Xmat = diag(dim(cellmeans)[2])
newdata.Xmat = cbind(newdata,Xmat)
Xmat.year=newdata.Xmat %>% group_by(fYear) %>% summarise(across(is.numeric, mean)) %>%
    dplyr::select(-fYear) %>%
    as.matrix()
MCMCstats <- function(x) {
    hpd = median_hdci(x)
    p=sum(x>0)/length(x)
    data.frame(hpd, 'P>0'=p, 'P<0'=1-p)
}
em.means <- newdata.Xmat %>% dplyr::select(fYear) %>% distinct %>% bind_cols(
    do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat.year)),2,
                          MCMCstats))) %>%
    dplyr::rename(`P>0`=P.0, `P<0`=P.0.1)
em.means %>% as.tibble() %>% knitr::kable()
## Xmat.year.contrast = Xmat.year[2,]-Xmat.year[1,]
## em.contr <- tibble(Contrast='2020-2019',
##                        do.call('rbind',apply(as.mcmc(cellmeans %*% cbind(Xmat.year.contrast)),2,
##                           MCMCstats)) %>%
##     dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))

## em.contr %>% as.tibble() %>% 
##     knitr::kable()

cellmeans.2 = plogis(samples %*% t(xmat) %*% t(Xmat.year))
em.contr <- tibble(Contrast='2020-2019',
                       do.call('rbind',apply(cbind(apply(cellmeans.2, 1, diff)),2,MCMCstats)) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))
em.contr %>% as.tibble() %>% 
    knitr::kable()
## ----end
## ---- fitModel-OC-inla-contrasts1figA
g1 <- em.means %>% as.data.frame() %>% 
    ggplot() + 
    geom_pointrange(aes(y=y, x=fYear, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('On course score (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=Contrast, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % on course score', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-OC-inla-contrasts1b
w=FullData.OC %>% filter(!is.na(on_course)) %>% droplevels %>% group_by(ftask_change,fYear) %>% count() %>% ungroup %>% group_by(fYear) %>% mutate(w=n/sum(n)) %>% ungroup %>% dplyr::select(fYear,ftask_change,w) %>% mutate(w1=w*c(1,0), w2=w*c(0,1)) %>% dplyr::select(w1,w2) %>% as.matrix() %>% t

em.means <- newdata.Xmat %>% dplyr::select(fYear) %>% distinct %>% bind_cols(
    #do.call('rbind',apply(as.mcmc(cellmeans %*% (t(Xmat.year))),2,
    do.call('rbind',apply(as.mcmc(cellmeans %*% (t(w))),2,
                          MCMCstats))) %>%
    dplyr::rename(`P>0`=P.0, `P<0`=P.0.1)
em.means %>% as.tibble() %>% knitr::kable()
## Xmat.year.contrast = w[2,]-w[1,]
## em.contr <- tibble(Contrast='2020-2019',
##                        do.call('rbind',apply(as.mcmc(cellmeans %*% cbind(Xmat.year.contrast)),2,
##                           MCMCstats)) %>%
##     dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))

## em.contr %>% as.tibble() %>% 
##     knitr::kable()
cellmeans.2 = plogis(samples %*% t(xmat) %*% t(w))
em.contr <- tibble(Contrast='2020-2019',
                       do.call('rbind',apply(cbind(apply(cellmeans.2, 1, diff)),2,MCMCstats)) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))

em.contr %>% as.tibble() %>% 
    knitr::kable()
## ----end
## ---- fitModel-OC-inla-contrasts1figB
g1 <- em.means %>% as.data.frame() %>% 
    ggplot() + 
    geom_pointrange(aes(y=y, x=fYear, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('On course score (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=Contrast, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % on course score', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-OC-inla-contrasts2
load(file='../data/mod.OC.inla.RData')  
em <- newdata.Xmat %>% dplyr::select(fYear,ftask_change) %>% distinct %>% bind_cols(
    do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat)),2,
                          MCMCstats))) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=y, x=fYear, ymin=ymin, ymax=ymax)) +
    facet_wrap(~ftask_change) + 
    theme_bw()


Xmat.year.by.task = newdata.Xmat %>% 
    group_by(ftask_change) %>% 
    summarise(across(is.numeric, function(x) x[2]-x[1])) %>%
    dplyr::select(-ftask_change) %>%
    as.matrix()
## em.contr <- newdata.Xmat %>% dplyr::select(ftask_change) %>% distinct %>% bind_cols(
##     do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat.year.by.task)),2,
##                           MCMCstats))
## ) %>%
##     dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)
cellmeans.2 = plogis(samples %*% t(xmat) %*% t(Xmat))
Xmat.contr = diag(ncol(cellmeans.2))
Xmat.contr = Xmat.contr[seq(2,ncol(cellmeans.2), by=2),] - Xmat.contr[seq(1,ncol(cellmeans.2), by=2),]
em.contr <- newdata.Xmat %>%
    dplyr::select(ftask_change) %>%
    distinct %>%
    bind_cols(
        do.call('rbind', apply(cellmeans.2 %*% t(Xmat.contr),2,MCMCstats))) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)


g2 <- em.contr%>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=ftask_change, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % on course score', labels=function(x) x*100) +
    scale_x_discrete('Task change code') +
    coord_flip() + 
    theme_bw()
g1 + g2

em  %>% knitr::kable()
em.contr %>% knitr::kable()
## ----end
## ---- fitModel-OC-inla-contrasts3
load(file='../data/mod.OC.inla.RData')  
Xmat.combs = rbind(
    #'0'=Xmat.year.by.task[1,],
    #'5'=Xmat.year.by.task[4,],
    #'6'=Xmat.year.by.task[5,],
    '0+6'=colMeans(Xmat.year.by.task[c(1,5),]),
    #'21'=Xmat.year.by.task[7,],
    #'31'=Xmat.year.by.task[10,],
    '21+31'=colMeans(Xmat.year.by.task[c(7,10),]),
    #'22'=Xmat.year.by.task[8,],
    #'3'=Xmat.year.by.task[2,],
    #'13'=Xmat.year.by.task[6,],
    #'33'=Xmat.year.by.task[11,],
    '3+13+33'=colMeans(Xmat.year.by.task[c(2,6,11),]),
    #'4'=Xmat.year.by.task[3,],
    #'24'=Xmat.year.by.task[9,],
    #'34'=Xmat.year.by.task[12,],
    '4+24'=colMeans(Xmat.year.by.task[c(3,9),]),
    '21+31+22+3+13+33'=colMeans(Xmat.year.by.task[c(7,10,8,2,6,11),]),
    '4+24+5'=colMeans(Xmat.year.by.task[c(3,9,4),]),
    '21+31+22+3+13+33 vs 4+24+5'=colMeans(Xmat.year.by.task[c(7,10,8,2,6,11),]) - colMeans(Xmat.year.by.task[c(3,9,4),])
)

em.combs <- 
    do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat.combs)),2,
                          MCMCstats))
em.combs <- em.combs %>% mutate(Contrast=rownames(.)) %>%
        dplyr::rename('P>0'=P.0, 'P<0'=P.0.1) %>%
        dplyr::select(Contrast, everything()) %>% 
    as.tibble()
em.combs %>% knitr::kable()

em.combs %>% 
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=Contrast, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % exam score', labels=function(x) x*100) +
    scale_x_discrete('Contrast') +
    coord_flip() + 
    theme_bw()
## ----end

## ---- fitModels-OCa-inla
newdata <- with(FullData.OC, expand.grid(fYear=levels(fYear),
                                      fYear_level=levels(fYear_level),
                                      Subject=NA, id=NA,
                                      on_course=NA))
# xmat = model.matrix(~fYear*ftask_change, newdata)
# newdata.xmat=cbind(newdata, xmat)
# n1 <- (newdata.xmat %>% group_by(fYear) %>% summarise(across(is.numeric, mean)))[,-1] %>% as.matrix
# n2 = n1[2,]-n1[1,]
# lincomb <- inla.make.lincombs(as.data.frame(rbind(n1,n2, xmat[1,])))
newdata.pred = FullData.OC %>% bind_rows(newdata)
mod.OCa.inla <- inla(on_course ~ fYear*fYear_level + f(Subject, model='iid') + f(id, model='iid'),
                    data=newdata.pred,
                    #lincomb=lincomb,
                    family='beta',
                    control.predictor=list(link=1, compute=TRUE),
                    #control.inla=list(lincomb.derived.only=TRUE),
                    control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, config=TRUE)
                    )
#mod.OC.inla = inla.hyperpar(mod.OC.inla)
save(mod.OCa.inla, file='../data/mod.OCa.inla.RData')
save(newdata, file='../data/newdata.OCa.inla.RData')
## ----end
## ---- fitModel-OCa-inla-contrasts1
load(file='../data/mod.OCa.inla.RData') 
load(file='../data/newdata.OCa.inla.RData')
xmat = model.matrix(~fYear*fYear_level, newdata)
samples = t(sapply(inla.posterior.sample(1000,mod.OCa.inla), function(x) tail(x$latent,nrow(newdata))))

cellmeans = plogis(samples %*% t(xmat))
Xmat = diag(dim(cellmeans)[2])
newdata.Xmat = cbind(newdata,Xmat)
Xmat.year=newdata.Xmat %>% group_by(fYear) %>% summarise(across(is.numeric, mean)) %>%
    dplyr::select(-fYear) %>%
    as.matrix()
MCMCstats <- function(x) {
    hpd = median_hdci(x)
    p=sum(x>0)/length(x)
    data.frame(hpd, 'P>0'=p, 'P<0'=1-p)
}
em.means <- newdata.Xmat %>% dplyr::select(fYear) %>% distinct %>% bind_cols(
    do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat.year)),2,
                          MCMCstats))) %>%
    dplyr::rename(`P>0`=P.0, `P<0`=P.0.1)
em.means %>% as.tibble() %>% knitr::kable()
## Xmat.year.contrast = Xmat.year[2,]-Xmat.year[1,]
## em.contr <- tibble(Contrast='2020-2019',
##                        do.call('rbind',apply(as.mcmc(cellmeans %*% cbind(Xmat.year.contrast)),2,
##                           MCMCstats)) %>%
##     dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))

## em.contr %>% as.tibble() %>% 
##     knitr::kable()
cellmeans.2 = plogis(samples %*% t(xmat) %*% t(Xmat.year))

em.contr <- tibble(Contrast='2020-2019',
                       do.call('rbind',apply(cbind(apply(cellmeans.2, 1, diff)),2,MCMCstats)) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))

em.contr %>% as.tibble() %>% 
    knitr::kable()
## ----end
## ---- fitModel-OCa-inla-contrasts2
load(file='../data/mod.OCa.inla.RData') 
load(file='../data/newdata.OCa.inla.RData')
em <- newdata.Xmat %>% dplyr::select(fYear,fYear_level) %>% distinct %>% bind_cols(
    do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat)),2,
                          MCMCstats))) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=y, x=fYear, ymin=ymin, ymax=ymax)) +
    facet_wrap(~fYear_level) + 
    theme_bw()


## Xmat.year.by.yearlevel = newdata.Xmat %>% 
##     group_by(fYear_level) %>% 
##     summarise(across(is.numeric, function(x) x[2]-x[1])) %>%
##     dplyr::select(-fYear_level) %>%
##     as.matrix()
## em.contr <- newdata.Xmat %>% dplyr::select(fYear_level) %>% distinct %>% bind_cols(
##     do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat.year.by.yearlevel)),2,
##                           MCMCstats))
## ) %>%
##     dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

cellmeans.2 = plogis(samples %*% t(xmat) %*% t(Xmat))
Xmat.contr = diag(ncol(cellmeans.2))
Xmat.contr = Xmat.contr[seq(2,ncol(cellmeans.2), by=2),] - Xmat.contr[seq(1,ncol(cellmeans.2), by=2),]
em.contr <- newdata.Xmat %>%
    dplyr::select(fYear_level) %>%
    distinct %>%
    bind_cols(
        do.call('rbind', apply(cellmeans.2 %*% t(Xmat.contr),2,MCMCstats))) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

g2 <- em.contr%>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=fYear_level, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % total achievement', labels=function(x) x*100) +
    scale_x_discrete('Year level') +
    coord_flip() + 
    theme_bw()
g1 + g2

em  %>% knitr::kable()
em.contr %>% knitr::kable()
## ----end

## brms %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## ---- fitModel-OC-brm
mod.OC.brm<- brm(on_course ~ fYear*ftask_change + (1|Subject) + (1|id),
               data=FullData.OC,
              family='beta',
              iter=3000,
              warmup=1500,
              thin=5,
              chains=3,
              cores=3,
              prior=c(
                  prior(normal(0,5), class='b'),
                  prior(normal(0.5,5), class='Intercept')
              ),
              control=list(adapt_delta=0.99))
save(mod.OC.brm, file='../data/mod.OC.brm.RData')
## ----end
## ---- fitModel-OC-brm-summary
load(file='../data/mod.OC.brm.RData')
broom::tidy(mod.OC.brm) %>% dplyr::select(-effect,-component,-group) %>% knitr::kable()  
## ----end
## ---- fitModel-OC-brm-MCMCdiagnostics
load(file='../data/mod.OC.brm.RData')
rstan::stan_trace(mod.OC.brm$fit)
rstan::stan_ac(mod.OC.brm$fit)
rstan::stan_rhat(mod.OC.brm$fit)
rstan::stan_ess(mod.OC.brm$fit)
## ----end
## ---- fitModel-OC-brm-diagnostics
load(file='../data/mod.OC.brm.RData')
preds <- posterior_predict(mod.OC.brm,  nsamples=250,  summary=FALSE)
mod.resids <- createDHARMa(simulatedResponse = t(preds),
                           observedResponse = FullData.OC$on_course,
                           fittedPredictedResponse = apply(preds, 2, median),
                           integerResponse = 'beta')
plot(mod.resids)
DHARMa::testDispersion(mod.resids)
## ----end
## ---- fitModel-OC-brm-summary1
broom::tidy(mod.OC.brm) %>% knitr::kable() 
## ----end
## ---- fitModel-OC-brm-contrasts1a
load(file='../data/mod.OC.brm.RData')
em <- emmeans(mod.OC.brm, ~fYear)
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())
em.contr %>% 
    tidy(conf.int=TRUE) %>%
    left_join(P) %>%
    knitr::kable() 
## ----end
## ---- fitModel-OC-brm-contrasts1figA
g1 <- em.means %>% as.data.frame() %>% 
    ggplot() + 
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('On_Course (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % total achievement', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-OC-brm-contrasts1b
load(file='../data/mod.OC.brm.RData')
em <- emmeans(mod.OC.brm, ~fYear, weights='proportional')
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())
em.contr %>% 
    tidy(conf.int=TRUE) %>%
    left_join(P) %>%
    knitr::kable() 
## ----end
## ---- fitModel-OC-brm-contrasts1figB
g1 <- em.means %>% as.data.frame() %>% 
    ggplot() + 
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('On_Course (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % total achievement', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-OC-brm-contrasts2
load(file='../data/mod.OC.brm.RData') 
em <- emmeans(mod.OC.brm, ~fYear*ftask_change) %>% regrid

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD, ymax=upper.HPD)) +
    facet_wrap(~ftask_change) + 
    theme_bw()
em.contr <- em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE))
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast, ftask_change) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())

g2 <- em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE)) %>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=ftask_change, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % on_course score', labels=function(x) x*100) +
    scale_x_discrete('Task change code') +
    coord_flip() + 
    theme_bw()
g1 + g2

em %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr %>% 
    tidy(conf.int=TRUE) %>% 
    left_join(P) %>%
    dplyr::select(-term,-null.value) %>%
    knitr::kable()

cellmeans.OC.brm <- em %>% as.data.frame() %>% mutate(Response='Total Achievement')
save(cellmeans.OC.brm, file='../data/cellmeans.OC.brm.RData')
effects.OC.brm <- em %>%
    contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE)) %>%
    as.data.frame()  %>% mutate(Response='Total Achievement')
save(effects.OC.brm, file='../data/effects.OC.brm.RData')
## ----end
## ---- fitModel-OC-brm-contrasts3
load(file='../data/mod.OC.brm.RData') 
xmat = em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change') %>% coef()
em.contr <- contrast(em, method=list(
    #'0'=xmat[,3],
    #'5'=xmat[,6],
    #'6'=xmat[,7],
    '0+6'=rowMeans(xmat[,c(3,7)]),
    #'21'=xmat[,9],
    #'31'=xmat[,12],
    '21+31'=rowMeans(xmat[,c(9,12)]),
    #'22'=xmat[,10],
    #'3'=xmat[,4],
    #'13'=xmat[,8],
    #'33'=xmat[,13],
    '3+13+33'=rowMeans(xmat[,c(4,8,13)]),
    #'4'=xmat[,5],
    #'24'=xmat[,11],
    #'34'=xmat[,14],
    '4+24'=rowMeans(xmat[,c(5,11)]),
    '21+31+22+3+13+33'=rowMeans(xmat[,c(9,12,10,4,8,13)]),
    '4+24+5'=rowMeans(xmat[,c(4,11,6)]),
    '21+31+22+3+13+33 vs 4+24+5'=rowMeans(xmat[,c(9,12,10,4,8,13)]) - 
        rowMeans(xmat[,c(4,11,6)])
),
infer=c(TRUE,TRUE)
)

P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())
em.contr %>% 
    tidy(conf.int=TRUE) %>% 
    left_join(P) %>%
    dplyr::select(-term,-null.value) %>% 
    knitr::kable()
em.contr %>% 
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % on_course score', labels=function(x) x*100) +
    scale_x_discrete('Contrast') +
    coord_flip() + 
    theme_bw()
## ----end

## ---- fitModels-OC-brma
mod.OCa.brm<- brm(on_course ~ fYear*fYear_level + (1|Subject) + (1|id),
               data=FullData.OC,
              family='beta',
              iter=3000,
              warmup=1500,
              thin=5,
              chains=3,
              cores=3,
              control=list(adapt_delta=0.95),
              prior=c(
                  prior(normal(0,5), class='b'),
                  prior(normal(0.5,5), class='Intercept')
              )
              )
save(mod.OCa.brm, file='../data/mod.OCa.brm.RData')
## ----end
## ---- fitModel-OC-brma-diagnostics
load(file='../data/mod.OCa.brm.RData')
mod.resid<-DHARMa::simulateResiduals(mod.OCa,plot = TRUE)
DHARMa::testDispersion(mod.resid) 
## ----end
## ---- fitModel-OC-brma-summary
load(file='../data/mod.OCa.brm.RData')
broom::tidy(mod.OCa.brm) %>% knitr::kable()  
## ----end
## ---- fitModel-OC-brma-contrasts2
load(file='../data/mod.OCa.brm.RData')
em <- emmeans(mod.OCa.brm, ~fYear*fYear_level) %>% regrid

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD, ymax=upper.HPD)) +
    facet_wrap(~fYear_level) + 
    theme_bw()

em.contr <- em %>% contrast(method=list(fYear=c(-1,1)), by='fYear_level', infer=c(TRUE,TRUE))
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast, fYear_level) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())

g2 <- em %>% contrast(method=list(fYear=c(-1,1)), by='fYear_level', infer=c(TRUE,TRUE)) %>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=fYear_level, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % on_course score', labels=function(x) x*100) +
    scale_x_discrete('Year level') +
    coord_flip() + 
    theme_bw()
g1 + g2
 
em %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr %>%
    tidy(conf.int=TRUE) %>%
    left_join(P) %>% 
    dplyr::select(-term,-null.value) %>%
    knitr::kable()
## ----end

## Prac Vivaoesce -------------------------------------------------------------
## ---- processPR
FullData.PR = FullData %>% filter(!is.na(pracvivaosce)) %>% droplevels
## ----end
## glmmTMB %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## ---- fitModels-PR-glmmTMB
FullData.PR = FullData.PR %>% filter(!is.na(pracvivaosce)) %>% droplevels
mod.PR <- glmmTMB(pracvivaosce ~ fYear*ftask_change + (1|Subject) + (1|id),
                    data=FullData.PR,
                    family='beta_family')
save(mod.PR, file='../data/mod.PR.RData') 
save(FullData.PR, file='../data/FullData.PR.RData')
## ----end
## ---- fitModel-PR-glmmTMB-diagnostics
load(file='../data/mod.PR.RData') 
load(file='../data/FullData.PR.pracvivaosce.RData')
mod.resid<-DHARMa::simulateResiduals(mod.PR,plot = TRUE)
DHARMa::testDispersion(mod.resid)
## ----end
## ---- fitModel-PR-glmmTMB-summary
load(file='../data/mod.PR.RData') 
broom::tidy(mod.PR) %>% knitr::kable() 
## ----end
## ---- fitModel-PR-glmmTMB-contrasts1a
load(file='../data/mod.PR.RData') 
em <- emmeans(mod.PR, ~fYear)
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
em.contr %>% 
    tidy(conf.int=TRUE) %>% 
    knitr::kable()
## ----end
## ---- fitModel-PR-glmmTMB-contrasts1figA
g1 <- em.means %>% as.data.frame() %>% 
    ggplot() + 
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Prac vivaosce score (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % prac vivaosce score', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-PR-glmmTMB-contrasts1b
load(file='../data/mod.PR.RData') 
em <- emmeans(mod.PR, ~fYear, weights='proportional')
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
em.contr %>% 
    tidy(conf.int=TRUE) %>% 
    knitr::kable()
## ----end
## ---- fitModel-PR-glmmTMB-contrasts1figB
g1 <- em.means %>% as.data.frame() %>% 
    ggplot() + 
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Prac vivaosce score (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % prac vivaosce score', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-PR-glmmTMB-contrasts2
em <- emmeans(mod.PR, ~fYear*ftask_change) %>% regrid 
g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.CL, ymax=upper.CL)) +
    facet_wrap(~ftask_change) + 
    theme_bw()

g2 <- em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE)) %>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=ftask_change, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % prac vivaosce score', labels=function(x) x*100) +
    scale_x_discrete('Task change code') +
    coord_flip() + 
    theme_bw()
g1 + g2

em %>% tidy(conf.int=TRUE) %>% knitr::kable()
em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE)) %>% 
    tidy(conf.int=TRUE) %>% 
    dplyr::select(-term,-null.value) %>%
    knitr::kable()

cellmeans.PR.glmmTMB <- em %>% as.data.frame() %>% mutate(Response='Prac vivaosce scores')
save(cellmeans.PR.glmmTMB, file='../data/cellmeans.PR.glmmTMB.RData')
effects.PR.glmmTMB <- em %>%
    contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE)) %>%
    as.data.frame()  %>% mutate(Response='Prac vivaosce scores')
save(effects.PR.glmmTMB, file='../data/effects.PR.glmmTMB.RData')
## ----end
## ---- fitModel-PR-glmmTMB-contrasts3
xmat = em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change') %>% coef() 
em.contr <- contrast(em, method=list(
     '6'=xmat[,3],
     #'31'=xmat[,7],
     #'22'=xmat[,5],
     #'13'=xmat[,4],
     #'33'=xmat[,8],
     '13+33'=rowMeans(xmat[,c(4,7)]),
     '31+22+13+33'=rowMeans(xmat[,c(6,5,4,7)])
     ),
     infer=c(TRUE,TRUE)
)

em.contr %>% tidy(conf.int=TRUE) %>% dplyr::select(-term,-null.value) %>% knitr::kable()
em.contr %>% 
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % prac vivaosce score', labels=function(x) x*100) +
    scale_x_discrete('Contrast') +
    coord_flip() + 
    theme_bw() 
## ----end

## ---- fitModels-PR-glmmTMBa
mod.PRa <- glmmTMB(pracvivaosce ~ fYear*fYear_level + (1|Subject) + (1|id),
               data=FullData.PR,
               family='beta_family')
save(mod.PRa, file='../data/mod.PRa.RData')
## ----end
## ---- fitModel-PR-glmmTMBa-diagnostics
load(file='../data/mod.PRa.RData')
mod.resid<-DHARMa::simulateResiduals(mod.PRa,plot = TRUE)
DHARMa::testDispersion(mod.resid) 
## ----end
## ---- fitModel-PR-glmmTMBa-summary
load(file='../data/mod.PRa.RData')
broom::tidy(mod.PRa) %>% knitr::kable()  
## ----end
## ---- fitModel-PR-glmmTMBa-contrasts2
load(file='../data/mod.PRa.RData')
em <- emmeans(mod.PRa, ~fYear*fYear_level) %>% regrid

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.CL, ymax=upper.CL)) +
    facet_wrap(~fYear_level) + 
    theme_bw()


g2 <- em %>% contrast(method=list(fYear=c(-1,1)), by='fYear_level', infer=c(TRUE,TRUE)) %>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=fYear_level, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % total achievement', labels=function(x) x*100) +
    scale_x_discrete('Year level') +
    coord_flip() + 
    theme_bw()
g1 + g2
 
em %>% tidy(conf.int=TRUE) %>% knitr::kable()
em %>% contrast(method=list(fYear=c(-1,1)), by='fYear_level', infer=c(TRUE,TRUE)) %>% 
    tidy(conf.int=TRUE) %>% 
    dplyr::select(-term,-null.value) %>%
    knitr::kable()
## ----end

## inla %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## ---- fitModels-PR-inla
newdata <- with(FullData.PR, expand.grid(fYear=levels(fYear),
                                      ftask_change=levels(ftask_change),
                                      Subject=NA, id=NA,
                                      pracvivaosce=NA))
newdata.pred = FullData.PR %>% bind_rows(newdata)
mod.PR.inla <- inla(pracvivaosce ~ fYear*ftask_change + f(Subject, model='iid') + f(id, model='iid'),
                    data=newdata.pred,
                    family='beta',
                    control.predictor=list(link=1, compute=TRUE),
                    control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, config=TRUE)
                    )
save(mod.PR.inla, file='../data/mod.PR.inla.RData')
save(newdata, file='../data/newdata.PR.inla.RData')
## ----end
## ---- fitModel-PR-inla-summary
load(file='../data/mod.PR.inla.RData')
summary(mod.PR.inla)$fixed %>% knitr::kable()
summary(mod.PR.inla)$hyperpar %>% knitr::kable()
## ----end
## ---- fitModel-PR-inla-diagnostics
load(file='../data/mod.PR.inla.RData')
load(file='../data/newdata.PR.inla.RData')
load(file='../data/FullData.PR.RData')
preds <- lapply(1:nrow(FullData.PR), function(x) inla.rmarginal(250, mod.PR.inla$marginals.fitted.values[[x]]))
preds = do.call('cbind', preds)
resids <- createDHARMa(simulatedResponse = t(preds),
                       observedResponse = FullData.PR$pracvivaosce,
                       fittedPredictedResponse = apply(preds, 2, median),
                       integerResponse = FALSE)
plot(resids)
DHARMa::testDispersion(resids)

autoplot(mod.PR.inla)
ggplot_inla_residuals(mod.PR.inla, FullData.PR$pracvivaosce)
ggplot_inla_residuals2(mod.PR.inla, FullData.PR$pracvivaosce)
## ----end
## ---- fitModel-PR-inla-contrasts1a
load(file='../data/mod.PR.inla.RData')
load(file='../data/newdata.PR.inla.RData')
xmat = model.matrix(~fYear*ftask_change, newdata)
samples = t(sapply(inla.posterior.sample(1000,mod.PR.inla), function(x) tail(x$latent,nrow(newdata))))

cellmeans = plogis(samples %*% t(xmat))
Xmat = diag(dim(cellmeans)[2])
newdata.Xmat = cbind(newdata,Xmat)
Xmat.year=newdata.Xmat %>% group_by(fYear) %>% summarise(across(is.numeric, mean)) %>%
    dplyr::select(-fYear) %>%
    as.matrix()
MCMCstats <- function(x) {
    hpd = median_hdci(x)
    p=sum(x>0)/length(x)
    data.frame(hpd, 'P>0'=p, 'P<0'=1-p)
}
em.means <- newdata.Xmat %>% dplyr::select(fYear) %>% distinct %>% bind_cols(
    do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat.year)),2,
                          MCMCstats))) %>%
    dplyr::rename(`P>0`=P.0, `P<0`=P.0.1)
em.means %>% as.tibble() %>% knitr::kable()
## Xmat.year.contrast = Xmat.year[2,]-Xmat.year[1,]
## em.contr <- tibble(Contrast='2020-2019',
##                        do.call('rbind',apply(as.mcmc(cellmeans %*% cbind(Xmat.year.contrast)),2,
##                           MCMCstats)) %>%
##     dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))

## em.contr %>% as.tibble() %>% 
##     knitr::kable()
cellmeans.2 = plogis(samples %*% t(xmat) %*% t(Xmat.year))
em.contr <- tibble(Contrast='2020-2019',
                       do.call('rbind',apply(cbind(apply(cellmeans.2, 1, diff)),2,MCMCstats)) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))
em.contr %>% as.tibble() %>% 
    knitr::kable()
## ----end
## ---- fitModel-PR-inla-contrasts1figA
g1 <- em.means %>% as.data.frame() %>%
    ggplot() + 
    geom_pointrange(aes(y=y, x=fYear, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('prac vivaosce score (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=Contrast, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % prac vivaosce score', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-PR-inla-contrasts1b
w=FullData.PR %>% filter(!is.na(pracvivaosce)) %>% droplevels %>% group_by(ftask_change,fYear) %>% count() %>% ungroup %>% group_by(fYear) %>% mutate(w=n/sum(n)) %>% ungroup %>% dplyr::select(fYear,ftask_change,w) %>% mutate(w1=w*c(1,0), w2=w*c(0,1)) %>% dplyr::select(w1,w2) %>% as.matrix() %>% t

em.means <- newdata.Xmat %>% dplyr::select(fYear) %>% distinct %>% bind_cols(
    #do.call('rbind',apply(as.mcmc(cellmeans %*% (t(Xmat.year))),2,
    do.call('rbind',apply(as.mcmc(cellmeans %*% (t(w))),2,
                          MCMCstats))) %>%
    dplyr::rename(`P>0`=P.0, `P<0`=P.0.1)
em.means %>% as.tibble() %>% knitr::kable()
## Xmat.year.contrast = w[2,]-w[1,]
## em.contr <- tibble(Contrast='2020-2019',
##                        do.call('rbind',apply(as.mcmc(cellmeans %*% cbind(Xmat.year.contrast)),2,
##                           MCMCstats)) %>%
##     dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))

## em.contr %>% as.tibble() %>% 
##     knitr::kable()
cellmeans.2 = plogis(samples %*% t(xmat) %*% t(w))
em.contr <- tibble(Contrast='2020-2019',
                       do.call('rbind',apply(cbind(apply(cellmeans.2, 1, diff)),2,MCMCstats)) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))

em.contr %>% as.tibble() %>% 
    knitr::kable()
## ----end
## ---- fitModel-PR-inla-contrasts1figB
g1 <- em.means %>% as.data.frame() %>% 
    ggplot() + 
    geom_pointrange(aes(y=y, x=fYear, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Prac vivaosce score (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=Contrast, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % prac vivaosce score', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-PR-inla-contrasts2
load(file='../data/mod.PR.inla.RData')
em <- newdata.Xmat %>% dplyr::select(fYear,ftask_change) %>% distinct %>% bind_cols(
    do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat)),2,
                          MCMCstats))) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=y, x=fYear, ymin=ymin, ymax=ymax)) +
    facet_wrap(~ftask_change) + 
    theme_bw()


Xmat.year.by.task = newdata.Xmat %>% 
    group_by(ftask_change) %>% 
    summarise(across(is.numeric, function(x) x[2]-x[1])) %>%
    dplyr::select(-ftask_change) %>%
    as.matrix()
## em.contr <- newdata.Xmat %>% dplyr::select(ftask_change) %>% distinct %>% bind_cols(
##     do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat.year.by.task)),2,
##                           MCMCstats))
## ) %>%
##     dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

cellmeans.2 = plogis(samples %*% t(xmat) %*% t(Xmat))
Xmat.contr = diag(ncol(cellmeans.2))
Xmat.contr = Xmat.contr[seq(2,ncol(cellmeans.2), by=2),] - Xmat.contr[seq(1,ncol(cellmeans.2), by=2),]
em.contr <- newdata.Xmat %>%
    dplyr::select(ftask_change) %>%
    distinct %>%
    bind_cols(
        do.call('rbind', apply(cellmeans.2 %*% t(Xmat.contr),2,MCMCstats))) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

g2 <- em.contr%>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=ftask_change, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % prac vivaosce score', labels=function(x) x*100) +
    scale_x_discrete('Task change code') +
    coord_flip() + 
    theme_bw()
g1 + g2

em  %>% knitr::kable()
em.contr %>% knitr::kable()
## ----end
## ---- fitModel-PR-inla-contrasts3
load(file='../data/mod.PR.inla.RData')
Xmat.combs = rbind(
    '6'=Xmat.year.by.task[1,],
    #'31'=Xmat.year.by.task[5,],
    #'22'=Xmat.year.by.task[3,],
    #'13'=Xmat.year.by.task[2,],
    #'33'=Xmat.year.by.task[6,],
    '13+33'=colMeans(Xmat.year.by.task[c(2,5),]),
    '31+22+13+33'=colMeans(Xmat.year.by.task[c(4,3,2,5),])
)
em.combs <- 
    do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat.combs)),2,
                          MCMCstats))
em.combs <- em.combs %>% mutate(Contrast=rownames(.)) %>%
        dplyr::rename('P>0'=P.0, 'P<0'=P.0.1) %>%
        dplyr::select(Contrast, everything()) %>% 
    as.tibble()
em.combs %>% knitr::kable()

em.combs %>% 
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=Contrast, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % prac vivaoesce score', labels=function(x) x*100) +
    scale_x_discrete('Contrast') +
    coord_flip() + 
    theme_bw()
## ----end

## ---- fitModels-PRa-inla
newdata <- with(FullData.PR, expand.grid(fYear=levels(fYear),
                                      fYear_level=levels(fYear_level),
                                      Subject=NA, id=NA,
                                      pracvivaosce=NA))
# xmat = model.matrix(~fYear*ftask_change, newdata)
# newdata.xmat=cbind(newdata, xmat)
# n1 <- (newdata.xmat %>% group_by(fYear) %>% summarise(across(is.numeric, mean)))[,-1] %>% as.matrix
# n2 = n1[2,]-n1[1,]
# lincomb <- inla.make.lincombs(as.data.frame(rbind(n1,n2, xmat[1,])))
newdata.pred = FullData.PR %>% bind_rows(newdata)
mod.PRa.inla <- inla(pracvivaosce ~ fYear*fYear_level + f(Subject, model='iid') + f(id, model='iid'),
                    data=newdata.pred,
                    #lincomb=lincomb,
                    family='beta',
                    control.predictor=list(link=1, compute=TRUE),
                    #control.inla=list(lincomb.derived.only=TRUE),
                    control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, config=TRUE)
                    )
#mod.PR.inla = inla.hyperpar(mod.PR.inla)
save(mod.PRa.inla, file='../data/mod.PRa.inla.RData')
save(newdata, file='../data/newdata.PRa.inla.RData')
## ----end
## ---- fitModel-PRa-inla-contrasts1
load(file='../data/mod.PRa.inla.RData') 
load(file='../data/newdata.PRa.inla.RData')
xmat = model.matrix(~fYear*fYear_level, newdata)
samples = t(sapply(inla.posterior.sample(1000,mod.PRa.inla), function(x) tail(x$latent,nrow(newdata))))

cellmeans = plogis(samples %*% t(xmat))
Xmat = diag(dim(cellmeans)[2])
newdata.Xmat = cbind(newdata,Xmat)
Xmat.year=newdata.Xmat %>% group_by(fYear) %>% summarise(across(is.numeric, mean)) %>%
    dplyr::select(-fYear) %>%
    as.matrix()
MCMCstats <- function(x) {
    hpd = median_hdci(x)
    p=sum(x>0)/length(x)
    data.frame(hpd, 'P>0'=p, 'P<0'=1-p)
}
em.means <- newdata.Xmat %>% dplyr::select(fYear) %>% distinct %>% bind_cols(
    do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat.year)),2,
                          MCMCstats))) %>%
    dplyr::rename(`P>0`=P.0, `P<0`=P.0.1)
em.means %>% as.tibble() %>% knitr::kable()
## Xmat.year.contrast = Xmat.year[2,]-Xmat.year[1,]
## em.contr <- tibble(Contrast='2020-2019',
##                        do.call('rbind',apply(as.mcmc(cellmeans %*% cbind(Xmat.year.contrast)),2,
##                           MCMCstats)) %>%
##     dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))

## em.contr %>% as.tibble() %>% 
##     knitr::kable()
cellmeans.2 = plogis(samples %*% t(xmat) %*% t(Xmat.year))

em.contr <- tibble(Contrast='2020-2019',
                       do.call('rbind',apply(cbind(apply(cellmeans.2, 1, diff)),2,MCMCstats)) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))

em.contr %>% as.tibble() %>% 
    knitr::kable()
## ----end
## ---- fitModel-PRa-inla-contrasts2
load(file='../data/mod.PRa.inla.RData') 
load(file='../data/newdata.PRa.inla.RData')
em <- newdata.Xmat %>% dplyr::select(fYear,fYear_level) %>% distinct %>% bind_cols(
    do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat)),2,
                          MCMCstats))) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=y, x=fYear, ymin=ymin, ymax=ymax)) +
    facet_wrap(~fYear_level) + 
    theme_bw()


## Xmat.year.by.yearlevel = newdata.Xmat %>% 
##     group_by(fYear_level) %>% 
##     summarise(across(is.numeric, function(x) x[2]-x[1])) %>%
##     dplyr::select(-fYear_level) %>%
##     as.matrix()
## em.contr <- newdata.Xmat %>% dplyr::select(fYear_level) %>% distinct %>% bind_cols(
##     do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat.year.by.yearlevel)),2,
##                           MCMCstats))
## ) %>%
##     dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

cellmeans.2 = plogis(samples %*% t(xmat) %*% t(Xmat))
Xmat.contr = diag(ncol(cellmeans.2))
Xmat.contr = Xmat.contr[seq(2,ncol(cellmeans.2), by=2),] - Xmat.contr[seq(1,ncol(cellmeans.2), by=2),]
em.contr <- newdata.Xmat %>%
    dplyr::select(fYear_level) %>%
    distinct %>%
    bind_cols(
        do.call('rbind', apply(cellmeans.2 %*% t(Xmat.contr),2,MCMCstats))) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

g2 <- em.contr%>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=fYear_level, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % prac vivaosce', labels=function(x) x*100) +
    scale_x_discrete('Year level') +
    coord_flip() + 
    theme_bw()
g1 + g2

em  %>% knitr::kable()
em.contr %>% knitr::kable()
## ----end

## brms %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## ---- fitModel-PR-brm
mod.PR.brm<- brm(pracvivaosce ~ fYear*ftask_change + (1|Subject) + (1|id),
               data=FullData.PR,
              family='beta',
              iter=3000,
              warmup=1500,
              thin=5,
              chains=3,
              cores=3,
              prior=c(
                  prior(normal(0,5), class='b'),
                  prior(normal(0.5,5), class='Intercept')
              ),
              control=list(adapt_delta=0.99))
save(mod.PR.brm, file='../data/mod.PR.brm.RData')
## ----end
## ---- fitModel-PR-brm-summary
load(file='../data/mod.PR.brm.RData')
broom::tidy(mod.PR.brm) %>% dplyr::select(-effect,-component,-group) %>% knitr::kable()  
## ----end
## ---- fitModel-PR-brm-MCMCdiagnostics
load(file='../data/mod.PR.brm.RData')
rstan::stan_trace(mod.PR.brm$fit)
rstan::stan_ac(mod.PR.brm$fit)
rstan::stan_rhat(mod.PR.brm$fit)
rstan::stan_ess(mod.PR.brm$fit)
## ----end
## ---- fitModel-PR-brm-diagnostics
load(file='../data/mod.PR.brm.RData')
preds <- posterior_predict(mod.PR.brm,  nsamples=250,  summary=FALSE)
mod.resids <- createDHARMa(simulatedResponse = t(preds),
                           observedResponse = FullData.PR$pracvivaosce,
                           fittedPredictedResponse = apply(preds, 2, median),
                           integerResponse = 'beta')
plot(mod.resids)
DHARMa::testDispersion(mod.resids)
## ----end
## ---- fitModel-PR-brm-summary1
broom::tidy(mod.PR.brm) %>% knitr::kable() 
## ----end
## ---- fitModel-PR-brm-contrasts1a
load(file='../data/mod.PR.brm.RData')
em <- emmeans(mod.PR.brm, ~fYear)
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())
em.contr %>% 
    tidy(conf.int=TRUE) %>%
    left_join(P) %>%
    knitr::kable() 
## ----end
## ---- fitModel-PR-brm-contrasts1figA
g1 <- em.means %>% as.data.frame() %>% 
    ggplot() + 
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Prac vivaosce score (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % prac vivaosce score', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-PR-brm-contrasts1b
load(file='../data/mod.PR.brm.RData')
em <- emmeans(mod.PR.brm, ~fYear, weights='proportional')
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())
em.contr %>% 
    tidy(conf.int=TRUE) %>%
    left_join(P) %>%
    knitr::kable() 
## ----end
## ---- fitModel-PR-brm-contrasts1figB
g1 <- em.means %>% as.data.frame() %>%
    ggplot() + 
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Prac vivaoesce score (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % prac vivaoesce score', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-PR-brm-contrasts2
load(file='../data/mod.PR.brm.RData')
em <- emmeans(mod.PR.brm, ~fYear*ftask_change) %>% regrid

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD, ymax=upper.HPD)) +
    facet_wrap(~ftask_change) + 
    theme_bw()
em.contr <- em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE))
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast, ftask_change) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())

g2 <- em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE)) %>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=ftask_change, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % on_course score', labels=function(x) x*100) +
    scale_x_discrete('Task change code') +
    coord_flip() + 
    theme_bw()
g1 + g2

em %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr %>% 
    tidy(conf.int=TRUE) %>% 
    left_join(P) %>%
    dplyr::select(-term,-null.value) %>%
    knitr::kable()

cellmeans.PR.brm <- em %>% as.data.frame() %>% mutate(Response='Total Achievement')
save(cellmeans.PR.brm, file='../data/cellmeans.PR.brm.RData')
effects.PR.brm <- em %>%
    contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE)) %>%
    as.data.frame()  %>% mutate(Response='Total Achievement')
save(effects.PR.brm, file='../data/effects.PR.brm.RData')
## ----end
## ---- fitModel-PR-brm-contrasts3
load(file='../data/mod.PR.brm.RData')
xmat = em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change') %>% coef()
em.contr <- contrast(em, method=list(
    #'0'=xmat[,3],
    #'5'=xmat[,6],
    '6'=xmat[,3],
    #'0+6'=rowMeans(xmat[,c(3,7)]),
    #'21'=xmat[,9],
    #'31'=xmat[,12],
    #'21+31'=rowMeans(xmat[,c(9,12)]),
    #'22'=xmat[,10],
    #'3'=xmat[,4],
    #'13'=xmat[,8],
    #'33'=xmat[,13],
    '13+33'=rowMeans(xmat[,c(4,7)]),
    #'4'=xmat[,5],
    #'24'=xmat[,11],
    #'34'=xmat[,14],
    #'4+24'=rowMeans(xmat[,c(5,11)]),
    '31+22+13+33'=rowMeans(xmat[,c(6,5,4,7)])
    #'4+24+5'=rowMeans(xmat[,c(4,11,6)]),
    #'21+31+22+3+13+33 vs 4+24+5'=rowMeans(xmat[,c(9,12,10,4,8,13)]) - 
    #    rowMeans(xmat[,c(4,11,6)])
),
infer=c(TRUE,TRUE)
)

P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())
em.contr %>% 
    tidy(conf.int=TRUE) %>% 
    left_join(P) %>%
    dplyr::select(-term,-null.value) %>% 
    knitr::kable()
em.contr %>% 
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % on_course score', labels=function(x) x*100) +
    scale_x_discrete('Contrast') +
    coord_flip() + 
    theme_bw()
## ----end

## ---- fitModels-PR-brma
mod.PRa.brm<- brm(pracvivaosce ~ fYear*fYear_level + (1|Subject) + (1|id),
               data=FullData.PR,
              family='beta',
              iter=3000,
              warmup=1500,
              thin=5,
              chains=3,
              cores=3,
              control=list(adapt_delta=0.95),
              prior=c(
                  prior(normal(0,5), class='b'),
                  prior(normal(0.5,5), class='Intercept')
              )
              )
save(mod.PRa.brm, file='../data/mod.PRa.brm.RData')
## ----end
## ---- fitModel-PR-brma-diagnostics
load(file='../data/mod.PRa.brm.RData')
mod.resid<-DHARMa::simulateResiduals(mod.PRa,plot = TRUE)
DHARMa::testDispersion(mod.resid) 
## ----end
## ---- fitModel-PR-brma-summary
load(file='../data/mod.PRa.brm.RData')
broom::tidy(mod.PRa.brm) %>% knitr::kable()  
## ----end
## ---- fitModel-PR-brma-contrasts2
load(file='../data/mod.PRa.brm.RData')
em <- emmeans(mod.PRa.brm, ~fYear*fYear_level) %>% regrid

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD, ymax=upper.HPD)) +
    facet_wrap(~fYear_level) + 
    theme_bw()

em.contr <- em %>% contrast(method=list(fYear=c(-1,1)), by='fYear_level', infer=c(TRUE,TRUE))
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast, fYear_level) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())

g2 <- em %>% contrast(method=list(fYear=c(-1,1)), by='fYear_level', infer=c(TRUE,TRUE)) %>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=fYear_level, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % pracvivaosce score', labels=function(x) x*100) +
    scale_x_discrete('Year level') +
    coord_flip() + 
    theme_bw()
g1 + g2
 
em %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr %>%
    tidy(conf.int=TRUE) %>%
    left_join(P) %>% 
    dplyr::select(-term,-null.value) %>%
    knitr::kable()
## ----end


## DNS -------------------------------------------------------------

## ---- prepare4DNS
FullData.subject = FullData.all %>% group_by(Subject,Year,task_change,fYear,ftask_change,fYear_level) %>%
  summarise(DNS=sum(DNS), N=n()) %>%
  ungroup %>%
  mutate(DS=N-DNS,
         Perc=DS/N) %>%
  filter(!is.na(task_change),
         !ftask_change %in% c(99)) %>%  #5,31
  droplevels
## ----end

## glmmTMB %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## ---- fitModels-DNS-glmmTMBold
## mod.DNS <- glmmTMB(DNS ~ fYear*ftask_change + (1|Subject) + (1|id),
##                   data=FullData.all,
##                   family='betabinomial')
## save(mod.DNS, file='../data/mod.DNS.RData')
## ----end

## ---- fitModels-DNS-glmmTMB
mod.DNS <- glmmTMB(cbind(DNS, DS) ~ fYear*ftask_change + (1|Subject),
                  data=FullData.subject,
                  family='binomial')
save(mod.DNS, file='../data/mod.DNS.RData')
## ----end
## ---- fitModel-DNS-glmmTMB-diagnostics
load(file='../data/mod.DNS.RData')
mod.resid<-DHARMa::simulateResiduals(mod.DNS,plot = TRUE)
DHARMa::testDispersion(mod.resid)
## ----end
## ---- fitModel-DNS-glmmTMB-summary
load(file='../data/mod.DNS.RData')
broom::tidy(mod.DNS) %>% knitr::kable() 
## ----end
## ---- fitModel-DNS-glmmTMB-contrasts1a
load(file='../data/mod.DNS.RData')
em <- emmeans(mod.DNS, ~fYear)
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
em.contr %>% 
    tidy(conf.int=TRUE) %>% 
    knitr::kable() 
## ----end
## ---- fitModel-DNS-glmmTMB-contrasts1figA
g1 <- em.means %>% as.data.frame() %>% 
    ggplot() + 
    geom_pointrange(aes(y=prob, x=fYear, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('DNS (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % DNS', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-DNS-glmmTMB-contrasts1b
load(file='../data/mod.DNS.RData') 
em <- emmeans(mod.DNS, ~fYear, weights='proportional')
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
em.contr %>% 
    tidy(conf.int=TRUE) %>% 
    knitr::kable()
## ----end
## ---- fitModel-DNS-glmmTMB-contrasts1figB
g1 <- em.means %>% as.data.frame() %>% 
    ggplot() + 
    geom_pointrange(aes(y=prob, x=fYear, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('DNS (%)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % DNS', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-DNS-glmmTMB-contrasts2
em <- emmeans(mod.DNS, ~fYear*ftask_change) %>% regrid
g1 <- em %>% as.data.frame %>%  
    ggplot() +
    geom_pointrange(aes(y=prob, x=fYear, ymin=lower.CL, ymax=upper.CL)) +
    facet_wrap(~ftask_change) + 
    theme_bw()

g2 <- em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE)) %>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=ftask_change, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % DNS', labels=function(x) x*100) +
    scale_x_discrete('Task change code') +
    coord_flip() + 
    theme_bw()
g1 + g2
em %>% tidy(conf.int=TRUE) %>% knitr::kable()
em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE)) %>% 
    tidy(conf.int=TRUE) %>% 
    dplyr::select(-term,-null.value) %>%
    knitr::kable()
## ----end
## ---- fitModel-DNS-glmmTMB-contrasts3
xmat = em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change') %>% coef()
 
em.contr <- contrast(em, method=list(
    #'0'=xmat[,3],
    #'5'=xmat[,6],
    #'21'=xmat[,8],
    #'31'=xmat[,11],
    '21+31'=rowMeans(xmat[,c(8,11)]),
    #'22'=xmat[,9],
    #'3'=xmat[,4],
    #'13'=xmat[,7],
    #'33'=xmat[,12],
    '3+13+33'=rowMeans(xmat[,c(4,7,12)]),
    #'4'=xmat[,5],
    #'24'=xmat[,10],
    #'34'=xmat[,13],
    '4+24+34'=rowMeans(xmat[,c(5,10,13)]),
    '21+31+22+3+13+33'=rowMeans(xmat[,c(8,11,9,4,7,12)]),
    '4+24+34+5'=rowMeans(xmat[,c(4,10,13,6)]),
    '21+31+22+3+13+33 vs 4+24+34+5'=rowMeans(xmat[,c(8,11,9,4,7,12)]) - 
        rowMeans(xmat[,c(4,10,13,6)])
),
infer=c(TRUE,TRUE)
)

em.contr %>% tidy(conf.int=TRUE) %>% dplyr::select(-term,-null.value) %>% knitr::kable()
em.contr %>% 
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % DNS', labels=function(x) x*100) +
    scale_x_discrete('Contrast') +
    coord_flip() + 
    theme_bw() 
## ----end

## ---- fitModels-DNS-glmmTMBa
mod.DNSa <- glmmTMB(cbind(DNS,DS) ~ fYear*fYear_level + (1|Subject),
               data=FullData.subject,
               family='binomial')
save(mod.DNSa, file='../data/mod.DNSa.RData') 
## ----end
## ---- fitModel-DNS-glmmTMBa-diagnostics
load(file='../data/mod.DNSa.RData') 
mod.resid<-DHARMa::simulateResiduals(mod.DNSa,plot = TRUE)
DHARMa::testDispersion(mod.resid) 
## ----end
## ---- fitModel-DNS-glmmTMBa-summary
load(file='../data/mod.DNSa.RData')
broom::tidy(mod.DNSa) %>% knitr::kable()   
## ----end
## ---- fitModel-DNS-glmmTMBa-contrasts2
load(file='../data/mod.DNSa.RData') 
em <- emmeans(mod.DNSa, ~fYear*fYear_level) %>% regrid
#em <- emmeans(mod.DNSa, ~fYear*fYear_level, type='response')

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=prob, x=fYear, ymin=lower.CL, ymax=upper.CL)) +
    facet_wrap(~fYear_level) + 
    theme_bw()


g2 <- em %>% contrast(method=list(fYear=c(-1,1)), by='fYear_level', infer=c(TRUE,TRUE)) %>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=fYear_level, ymin=lower.CL, ymax=upper.CL)) +
    scale_y_continuous('Change in % DNS', labels=function(x) x*100) +
    scale_x_discrete('Year level') +
    coord_flip() + 
    theme_bw()
g1 + g2
 
em %>% tidy(conf.int=TRUE) %>% knitr::kable()
em %>% contrast(method=list(fYear=c(-1,1)), by='fYear_level', infer=c(TRUE,TRUE)) %>% 
    tidy(conf.int=TRUE) %>% 
    dplyr::select(-term,-null.value) %>%
    knitr::kable()
## ----end


## ---- testing


FullData.subject = FullData.all %>% group_by(Subject,Year,task_change,fYear,ftask_change,fYear_level) %>%
    summarise(DNS=sum(DNS), N=n()) %>%
    ungroup %>%
    mutate(DS=N-DNS,
           Perc=DS/N)
FullData.subject

mod.DNS.subject <- glmmTMB(cbind(DNS,DS) ~ fYear*ftask_change + (1|Subject),
                           data=FullData.subject, 
                           family='betabinomial')
save(mod.DNS.subject, file='../data/mod.DNS.subject.RData')

mod.resid<-DHARMa::simulateResiduals(mod.DNS.subject,plot = TRUE)
DHARMa::testDispersion(mod.resid)

em <- emmeans(mod.DNS.subject, ~fYear)
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
em.contr %>% 
    tidy(conf.int=TRUE) %>% 
    knitr::kable() 

FullData.all %>% group_by(fYear) %>%
    summarise(p=sum(DNS==1)/n())

em <- emmeans(mod.DNS.subject, ~fYear, weights='proportional')
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
em.contr %>% 
    tidy(conf.int=TRUE) %>% 
    knitr::kable()

em <- emmeans(mod.DNS.subject, ~fYear*ftask_change) %>% regrid()
em

(emmeans(mod.DNS.subject, ~fYear|ftask_change) %>% pairs() %>% as.glht())[[1]]$linfct

a=(emmeans(mod.DNS.subject, ~fYear) %>% pairs() %>% as.glht())$linfct %>% as.vector()
a
a=(emmeans(mod.DNS.subject, ~fYear, weights='proportional') %>% pairs() %>% as.glht())$linfct %>% as.vector()
a

(emmeans(mod.DNS.subject, ~fYear)  %>% as.glht())$linfct
emmeans(mod.DNS.subject, ~fYear) %>% regrid()
emmeans(mod.DNS.subject, ~fYear, weights='proportional') %>% regrid()
emmeans(mod.DNS.subject, ~fYear) 
(emmeans(mod.DNS.subject, ~fYear) %>% as.glht())$linfct
## ----end
## inla %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## ---- fitModels-DNS-inla
#FullData.DNS = FullData.all %>% filter(!is.na(DNS), !is.na(ftask_change)) %>% droplevels
newdata <- with(FullData.subject, expand.grid(fYear=levels(fYear),
                                      ftask_change=levels(ftask_change),
                                      Subject=NA, id=NA,
                                      DNS=NA, DS=NA,N=NA))
newdata.pred = FullData.subject %>% bind_rows(newdata)
mod.DNS.inla <- inla(DNS ~ fYear*ftask_change + f(Subject, model='iid'),
                    data=newdata.pred,
                    family='binomial',
                    Ntrials=newdata.pred$N,
                    control.predictor=list(link=1, compute=TRUE),
                    control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, config=TRUE),
                    control.fixed=list(mean=0, prec=0.01, mean.intercept=0, prec.intercept=0.001)
                    )
save(mod.DNS.inla, file='../data/mod.DNS.inla.RData')
save(newdata, file='../data/newdata.DNS.inla.RData')
save(FullData.subject, file='../data/FullData.DNS.RData')
## ----end
## ---- fitModel-DNS-inla-summary
load(file='../data/mod.DNS.inla.RData')
summary(mod.DNS.inla)$fixed %>% knitr::kable()
summary(mod.DNS.inla)$hyperpar %>% knitr::kable()
## ----end
## ---- fitModel-DNS-inla-diagnostics
load(file='../data/mod.DNS.inla.RData')
load(file='../data/newdata.DNS.inla.RData')
load(file='../data/FullData.DNS.RData')
## preds <- lapply(1:nrow(FullData.DNS), function(x) inla.rmarginal(250, mod.DNS.inla$marginals.fitted.values[[x]]))
## preds = do.call('cbind', preds)
## resids <- createDHARMa(simulatedResponse = t(preds),
##                        observedResponse = FullData.DNS$DNS,
##                        fittedPredictedResponse = apply(preds, 2, median),
##                        integerResponse = FALSE)
## plot(resids)
## DHARMa::testDispersion(resids)

autoplot(mod.DNS.inla)
## ggplot_inla_residuals(mod.DNS.inla, FullData.DNS$DNS)
ggplot_inla_residuals2(mod.DNS.inla, FullData.DNS$DNS)
## ----end
## ---- fitModel-DNS-inla-contrasts1a
load(file='../data/mod.DNS.inla.RData')
load(file='../data/newdata.DNS.inla.RData')
xmat = model.matrix(~fYear*ftask_change, newdata)
samples = t(sapply(inla.posterior.sample(1000,mod.DNS.inla), function(x) tail(x$latent,nrow(newdata))))

cellmeans = samples %*% t(xmat)
Xmat = diag(dim(cellmeans)[2])
newdata.Xmat = cbind(newdata,Xmat)
Xmat.year=newdata.Xmat %>% group_by(fYear) %>% summarise(across(is.numeric, mean)) %>%
    dplyr::select(-fYear) %>%
    as.matrix()
MCMCstats <- function(x) {
    hpd = median_hdci(x)
    p=sum(x>0)/length(x)
    data.frame(hpd, 'P>0'=p, 'P<0'=1-p)
}
em.means <- newdata.Xmat %>% dplyr::select(fYear) %>% distinct %>% bind_cols(
    do.call('rbind',apply(as.mcmc(plogis(cellmeans %*% t(Xmat.year))),2,
                          MCMCstats))) %>%
    dplyr::rename(`P>0`=P.0, `P<0`=P.0.1)
em.means %>% as.tibble() %>% knitr::kable()
cellmeans.2 = plogis(cellmeans %*% t(Xmat.year))

em.contr <- tibble(Contrast='2020-2019',
                       do.call('rbind',apply(cbind(apply(cellmeans.2, 1, diff)),2,MCMCstats)) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))

em.contr %>% as.tibble() %>% 
    knitr::kable()
## ----end
## ---- fitModel-DNS-inla-contrasts1figA
g1 <- em.means %>% as.data.frame() %>%
    ggplot() + 
    geom_pointrange(aes(y=y, x=fYear, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('DNS rate (% per subject)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=Contrast, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % DNS rate', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-DNS-inla-contrasts1b
#w=FullData %>%
w=FullData.subject %>%
    filter(!is.na(DNS)) %>%
    droplevels %>%
    group_by(ftask_change,fYear) %>%
    count() %>%
    ungroup %>%
    group_by(fYear) %>%
    mutate(w=n/sum(n)) %>%
    ungroup %>%
    dplyr::select(fYear,ftask_change,w) %>%
    mutate(w1=w*c(1,0), w2=w*c(0,1)) %>%
    dplyr::select(w1,w2) %>%
    as.matrix() %>%
    t

em.means <- newdata.Xmat %>% dplyr::select(fYear) %>% distinct %>% bind_cols(
    #do.call('rbind',apply(as.mcmc(cellmeans %*% (t(Xmat.year))),2,
    do.call('rbind',apply(as.mcmc(plogis(cellmeans %*% (t(w)))),2,
                          MCMCstats))) %>%
    dplyr::rename(`P>0`=P.0, `P<0`=P.0.1)
em.means %>% as.tibble() %>% knitr::kable()
cellmeans.2 = plogis(cellmeans %*% t(w))

em.contr <- tibble(Contrast='2020-2019',
                       do.call('rbind',apply(cbind(apply(cellmeans.2, 1, diff)),2,MCMCstats)) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))

em.contr %>% as.tibble() %>% 
    knitr::kable()
## ----end
## ---- fitModel-DNS-inla-contrasts1figB
g1 <- em.means %>% as.data.frame() %>%
    ggplot() + 
    geom_pointrange(aes(y=y, x=fYear, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('DNS rate (% per subject)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=Contrast, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % DNS rate', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-DNS-inla-contrasts2
load(file='../data/mod.DNS.inla.RData')
em <- newdata.Xmat %>% dplyr::select(fYear,ftask_change) %>% distinct %>% bind_cols(
    do.call('rbind',apply(as.mcmc(plogis(cellmeans %*% t(Xmat))),2,
                          MCMCstats))) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=y, x=fYear, ymin=ymin, ymax=ymax)) +
    facet_wrap(~ftask_change) + 
    scale_y_continuous('DNS rate (% per subject)', labels=function(x) x*100) +
    theme_bw()

Xmat.year.by.task = newdata.Xmat %>% 
    group_by(ftask_change) %>% 
    summarise(across(is.numeric, function(x) x[2]-x[1])) %>%
    dplyr::select(-ftask_change) %>%
    as.matrix()
cellmeans.2 = plogis(cellmeans %*% t(Xmat))
Xmat.contr = diag(ncol(cellmeans.2))
Xmat.contr = Xmat.contr[seq(2,ncol(cellmeans.2), by=2),] - Xmat.contr[seq(1,ncol(cellmeans.2), by=2),]
em.contr <- newdata.Xmat %>%
    dplyr::select(ftask_change) %>%
    distinct %>%
    bind_cols(
        do.call('rbind', apply(cellmeans.2 %*% t(Xmat.contr),2,MCMCstats))) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

g2 <- em.contr%>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=ftask_change, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % DNS rate', labels=function(x) x*100) +
    scale_x_discrete('Task change code') +
    coord_flip() + 
    theme_bw()
g1 + g2

em  %>% knitr::kable()
em.contr %>% knitr::kable()
## ----end
## ---- fitModel-DNS-inla-contrasts3
load(file='../data/mod.DNS.inla.RData')
Xmat.combs = rbind(
    #'0'=Xmat.year.by.task[1,],
    #'5'=Xmat.year.by.task[4,],
    #'6'=Xmat.year.by.task[5,],
    #'0+6'=colMeans(Xmat.year.by.task[c(1,5),]),
    #'21'=Xmat.year.by.task[6,],
    #'31'=Xmat.year.by.task[9,],
    '21+31'=colMeans(Xmat.year.by.task[c(6,9),]),
    #'22'=Xmat.year.by.task[7,],
    #'3'=Xmat.year.by.task[2,],
    #'13'=Xmat.year.by.task[5,],
    #'33'=Xmat.year.by.task[10,],
    '3+13+33'=colMeans(Xmat.year.by.task[c(2,5,10),]),
    #'4'=Xmat.year.by.task[3,],
    #'24'=Xmat.year.by.task[8,],
    #'34'=Xmat.year.by.task[11,],
    '4+24'=colMeans(Xmat.year.by.task[c(3,8),]),
    '21+31+22+3+13+33'=colMeans(Xmat.year.by.task[c(6,9,7,2,5,10),]),
    '4+24+5'=colMeans(Xmat.year.by.task[c(3,8,4),]),
    '21+31+22+3+13+33 vs 4+24+5'=colMeans(Xmat.year.by.task[c(6,9,7,2,5,10),]) - colMeans(Xmat.year.by.task[c(3,8,4),])
)

em.combs <- 
    do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat.combs)),2,
                          MCMCstats))
em.combs <- em.combs %>% mutate(Contrast=rownames(.)) %>%
        dplyr::rename('P>0'=P.0, 'P<0'=P.0.1) %>%
        dplyr::select(Contrast, everything()) %>% 
    as.tibble()
em.combs %>% knitr::kable()

em.combs %>% 
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=Contrast, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % DNS score', labels=function(x) x*100) +
    scale_x_discrete('Contrast') +
    coord_flip() + 
    theme_bw()
## ----end

## ---- fitModels-DNSa-inla
newdata <- with(FullData.subject, expand.grid(fYear=levels(fYear),
                                      fYear_level=levels(fYear_level),
                                      Subject=NA, id=NA,
                                      DNS=NA, DS=NA,N=NA))
newdata.pred = FullData.subject %>% bind_rows(newdata)
mod.DNSa.inla <- inla(DNS ~ fYear*fYear_level + f(Subject, model='iid'),# + f(id, model='iid'),
                    data=newdata.pred,
                    #lincomb=lincomb,
                    family='binomial',
                    Ntrials=newdata.pred$N,
                    control.predictor=list(link=1, compute=TRUE),
                    #control.inla=list(lincomb.derived.only=TRUE),
                    control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, config=TRUE)
                    )
#mod.DNS.inla = inla.hyperpar(mod.DNS.inla)
save(mod.DNSa.inla, file='../data/mod.DNSa.inla.RData')
save(newdata, file='../data/newdata.DNSa.inla.RData')
## ----end
## ---- fitModel-DNSa-inla-contrasts1
load(file='../data/mod.DNSa.inla.RData') 
load(file='../data/newdata.DNSa.inla.RData')
xmat = model.matrix(~fYear*fYear_level, newdata)
samples = t(sapply(inla.posterior.sample(1000,mod.DNSa.inla), function(x) tail(x$latent,nrow(newdata))))

cellmeans = (samples %*% t(xmat))
Xmat = diag(dim(cellmeans)[2])
newdata.Xmat = cbind(newdata,Xmat)
Xmat.year=newdata.Xmat %>% group_by(fYear) %>% summarise(across(is.numeric, mean)) %>%
    dplyr::select(-fYear) %>%
    as.matrix()
MCMCstats <- function(x) {
    hpd = median_hdci(x)
    p=sum(x>0)/length(x)
    data.frame(hpd, 'P>0'=p, 'P<0'=1-p)
}
em.means <- newdata.Xmat %>% dplyr::select(fYear) %>% distinct %>% bind_cols(
    do.call('rbind',apply(as.mcmc(plogis(cellmeans %*% t(Xmat.year))),2,
                          MCMCstats))) %>%
    dplyr::rename(`P>0`=P.0, `P<0`=P.0.1)
em.means %>% as.tibble() %>% knitr::kable()


cellmeans.2 = plogis(cellmeans %*% t(Xmat.year))

em.contr <- tibble(Contrast='2020-2019',
                       do.call('rbind',apply(cbind(apply(cellmeans.2, 1, diff)),2,MCMCstats)) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))

em.contr %>% as.tibble() %>% 
    knitr::kable()

## ----end
## ---- fitModel-DNSa-inla-contrasts2
load(file='../data/mod.DNSa.inla.RData') 
load(file='../data/newdata.DNSa.inla.RData')
em <- newdata.Xmat %>% dplyr::select(fYear,fYear_level) %>% distinct %>% bind_cols(
    do.call('rbind',apply(as.mcmc(plogis(cellmeans %*% t(Xmat))),2,
                          MCMCstats))) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=y, x=fYear, ymin=ymin, ymax=ymax)) +
    facet_wrap(~fYear_level) + 
    scale_y_continuous('DNS rate (% per subject)', labels=function(x) x*100) +
    theme_bw()

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=y, x=fYear, ymin=ymin, ymax=ymax)) +
    facet_wrap(~fYear_level) + 
    theme_bw()

cellmeans.2 = plogis(cellmeans %*% t(Xmat))
Xmat.contr = diag(ncol(cellmeans.2))
Xmat.contr = Xmat.contr[seq(2,ncol(cellmeans.2), by=2),] - Xmat.contr[seq(1,ncol(cellmeans.2), by=2),]
em.contr <- newdata.Xmat %>%
    dplyr::select(fYear_level) %>%
    distinct %>%
    bind_cols(
        do.call('rbind', apply(cellmeans.2 %*% t(Xmat.contr),2,MCMCstats))) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1)

g2 <- em.contr%>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=y, x=fYear_level, ymin=ymin, ymax=ymax)) +
    scale_y_continuous('Change in % DNS rate', labels=function(x) x*100) +
    scale_x_discrete('Year level') +
    coord_flip() + 
    theme_bw()
g1 + g2

em  %>% knitr::kable()
em.contr %>% knitr::kable()

## ----end


## brms %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## ---- fitModels-DNS-brm
beta_binomial2 <- custom_family(
  "beta_binomial2", dpars = c("mu", "phi"),
  links = c("logit", "log"), lb = c(NA, 0),
  type = "int", vars = "vint1[n]"
)
stan_funs <- "
  real beta_binomial2_lpmf(int y, real mu, real phi, int T) {
    return beta_binomial_lpmf(y | T, mu * phi, (1 - mu) * phi);
  }
  int beta_binomial2_rng(real mu, real phi, int T) {
    return beta_binomial_rng(T, mu * phi, (1 - mu) * phi);
  }
"
stanvars <- stanvar(scode = stan_funs, block = "functions")
mod.DNS.brm <- brm(DNS|vint(N) ~ fYear*ftask_change + (1|Subject),
                   family=beta_binomial2,
                   data=FullData.subject,
                   stanvars = stanvars,
                   iter=3000,
                   warmup=1500,
                   chains=3,
                   thin=5,
                   cores=3,
                   control=list(adapt_delta=0.95),
                   prior=c(prior(normal(0,5), class='b'),
                           prior(normal(0.5,5), class='Intercept')
                           )
                   )
save(mod.DNS.brm, file='../data/mod.DNS.brm.RData')
## ----end

## ---- fitModel-DNS-brm-summary
load(file='../data/mod.DNS.brm.RData')
broom::tidy(mod.DNS.brm) %>% dplyr::select(-effect,-component,-group) %>% knitr::kable()  
## ----end

## ---- fitModel-DNS-brm-MCMCdiagnostics
load(file='../data/mod.DNS.brm.RData')
rstan::stan_trace(mod.DNS.brm$fit)
rstan::stan_ac(mod.DNS.brm$fit)
rstan::stan_rhat(mod.DNS.brm$fit)
rstan::stan_ess(mod.DNS.brm$fit)
## ----end
## ---- fitModel-DNS-brm-diagnostics
load(file='../data/mod.DNS.brm.RData')
preds <- posterior_predict(mod.DNS.brm,  nsamples=250,  summary=FALSE)
mod.resids <- createDHARMa(simulatedResponse = t(preds),
                           observedResponse = FullData.subject$DNS,
                           fittedPredictedResponse = apply(preds, 2, median),
                           integerResponse = 'beta')
plot(mod.resids)
DHARMa::testDispersion(mod.resids)
## ----end
## ---- fitModel-DNS-brm-summary1
broom::tidy(mod.DNS.brm) %>% knitr::kable() 
## ----end
## ---- fitModel-DNS-brm-contrasts1a
load(file='../data/mod.DNS.brm.RData')
em <- emmeans(mod.DNS.brm, ~fYear)
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())
em.contr %>% 
    tidy(conf.int=TRUE) %>%
    left_join(P) %>%
    knitr::kable() 
## ----end
## ---- fitModel-DNS-brm-contrasts1figA
g1 <- em.means %>% as.data.frame() %>% 
    ggplot() + 
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('DNS rate (% per subject)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % DNS per subject', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end

## ---- fitModel-DNS-brm-contrasts1b
load(file='../data/mod.DNS.brm.RData')
em <- emmeans(mod.DNS.brm, ~fYear, weights='proportional')
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())
em.contr %>% 
    tidy(conf.int=TRUE) %>%
    left_join(P) %>%
    knitr::kable() 
## ----end
## ---- fitModel-DNS-brm-contrasts1figB
g1 <- em.means %>% as.data.frame() %>%
    ggplot() + 
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('DNS rate (% per subject)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % DNS rate per subject', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-DNS-brm-contrasts2
load(file='../data/mod.DNS.brm.RData')
em <- emmeans(mod.DNS.brm, ~fYear*ftask_change) %>% regrid

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD, ymax=upper.HPD)) +
    facet_wrap(~ftask_change) + 
    theme_bw()
em.contr <- em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE))
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast, ftask_change) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())

g2 <- em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change', infer=c(TRUE,TRUE)) %>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=ftask_change, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % on_course score', labels=function(x) x*100) +
    scale_x_discrete('Task change code') +
    coord_flip() + 
    theme_bw()
g1 + g2

em %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr %>% 
    tidy(conf.int=TRUE) %>% 
    left_join(P) %>%
    dplyr::select(-term,-null.value) %>%
    knitr::kable()
## ----end
## ---- fitModel-DNS-brm-contrasts3
load(file='../data/mod.DNS.brm.RData')
xmat = em %>% contrast(method=list(fYear=c(-1,1)), by='ftask_change') %>% coef()
em.contr <- contrast(em, method=list(
    #'0'=xmat[,3],
    #'5'=xmat[,6],
    #'6'=xmat[,7],
    '0+6'=rowMeans(xmat[,c(3,7)]),
    #'21'=xmat[,9],
    #'31'=xmat[,12],
    '21+31'=rowMeans(xmat[,c(9,12)]),
    #'22'=xmat[,10],
    #'3'=xmat[,4],
    #'13'=xmat[,8],
    #'33'=xmat[,13],
    '3+13+33'=rowMeans(xmat[,c(4,8,13)]),
    #'4'=xmat[,5],
    #'24'=xmat[,11],
    #'34'=xmat[,14],
    '4+24'=rowMeans(xmat[,c(5,11)]),
    '21+31+22+3+13+33'=rowMeans(xmat[,c(9,12,10,4,8,13)]),
    '4+24+5'=rowMeans(xmat[,c(4,11,6)]),
    '21+31+22+3+13+33 vs 4+24+5'=rowMeans(xmat[,c(9,12,10,4,8,13)]) - 
        rowMeans(xmat[,c(4,11,6)])
),
infer=c(TRUE,TRUE)
)

P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())
em.contr %>% 
    tidy(conf.int=TRUE) %>% 
    left_join(P) %>%
    dplyr::select(-term,-null.value) %>% 
    knitr::kable()
em.contr %>% 
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % on_course score', labels=function(x) x*100) +
    scale_x_discrete('Contrast') +
    coord_flip() + 
    theme_bw()
## ----end

## ---- fitModels-DNS-brma
mod.DNSa.brm <- brm(DNS|vint(N) ~ fYear*fYear_level + (1|Subject),
                   family=beta_binomial2,
                   data=FullData.subject,
                   stanvars = stanvars,
                   iter=3000,
                   warmup=1500,
                   chains=3,
                   thin=5,
                   cores=3,
                   control=list(adapt_delta=0.95),
                   prior=c(prior(normal(0,5), class='b'),
                           prior(normal(0.5,5), class='Intercept')
                           )
                   )
save(mod.DNSa.brm, file='../data/mod.DNSa.brm.RData')
## ----end
## ---- fitModel-DNS-brma-diagnostics
load(file='../data/mod.DNSa.brm.RData')
mod.resid<-DHARMa::simulateResiduals(mod.DNSa,plot = TRUE)
DHARMa::testDispersion(mod.resid) 
## ----end
## ---- fitModel-DNS-brma-summary
load(file='../data/mod.DNSa.brm.RData')
broom::tidy(mod.DNSa.brm) %>% knitr::kable()  
## ----end
## ---- fitModel-DNS-brma-contrasts2
load(file='../data/mod.DNSa.brm.RData')
em <- emmeans(mod.DNSa.brm, ~fYear*fYear_level) %>% regrid

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD, ymax=upper.HPD)) +
    facet_wrap(~fYear_level) + 
    theme_bw()

em.contr <- em %>% contrast(method=list(fYear=c(-1,1)), by='fYear_level', infer=c(TRUE,TRUE))
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast, fYear_level) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())

g2 <- em %>% contrast(method=list(fYear=c(-1,1)), by='fYear_level', infer=c(TRUE,TRUE)) %>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=fYear_level, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % pracvivaosce score', labels=function(x) x*100) +
    scale_x_discrete('Year level') +
    coord_flip() + 
    theme_bw()
g1 + g2
 
em %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr %>%
    tidy(conf.int=TRUE) %>%
    left_join(P) %>% 
    dplyr::select(-term,-null.value) %>%
    knitr::kable()
## ----end

## ---- brms_beta_binomial_Testing


summary(mod.brm)
library(rstan)
stan_trace(mod.brm$fit)
stan_rhat(mod.brm$fit)
stan_ess(mod.brm$fit)

emmeans(mod.brm, ~fYear) %>% regrid()
emmeans(mod.brm, ~fYear) %>% regrid() %>% pairs()
emmeans(mod.brm, ~fYear, weights='proportional') %>% regrid() %>% pairs()
emmeans(mod.brm, ~fYear*ftask_change) %>% regrid() %>% as.data.frame %>%
    ggplot(aes(y=prob, x=fYear)) +
    geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD)) +
    facet_wrap(~ftask_change)


beta_binomial2 <- custom_family(
  "beta_binomial2", dpars = c("mu", "phi"),
  links = c("logit", "log"), lb = c(NA, 0),
  type = "int", vars = "vint1[n]"
)
stan_funs <- "
  real beta_binomial2_lpmf(int y, real mu, real phi, int T) {
    return beta_binomial_lpmf(y | T, mu * phi, (1 - mu) * phi);
  }
  int beta_binomial2_rng(real mu, real phi, int T) {
    return beta_binomial_rng(T, mu * phi, (1 - mu) * phi);
  }
"
stanvars <- stanvar(scode = stan_funs, block = "functions")
mod.brm2 <- brm(
    DNS | vint(N) ~ fYear*ftask_change + (1|Subject),
    data = FullData.subject, 
    family = beta_binomial2,
    stanvars = stanvars,
    iter=1000,
    chains=3,
    cores=3,
    warmup=500,
    prior=c(prior(normal(0,5), class='b'),
            prior(normal(0.5,5), class='Intercept')
            )
)
summary(mod.brm2)
emmeans(mod.brm2, ~fYear) %>% regrid()
emmeans(mod.brm, ~fYear) %>% regrid() %>% pairs()
emmeans(mod.brm, ~fYear, weights='proportional') %>% regrid() %>% pairs()
emmeans(mod.brm, ~fYear*ftask_change) %>% regrid() %>% as.data.frame %>%
    ggplot(aes(y=prob, x=fYear)) +
    geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD)) +
    facet_wrap(~ftask_change)
## ----end


## Compilation figures -------------------------------------------------------------
## ---- compilationFigures

load(file='../data/cellmeans.TA.glmmTMB.RData')
load(file='../data/cellmeans.EXAM.glmmTMB.RData')
load(file='../data/cellmeans.OC.glmmTMB.RData')
load(file='../data/cellmeans.PR.glmmTMB.RData')
load(file='../data/effects.TA.glmmTMB.RData')
load(file='../data/effects.EXAM.glmmTMB.RData')
load(file='../data/effects.OC.glmmTMB.RData')
load(file='../data/effects.PR.glmmTMB.RData')


cellmeans.all = cellmeans.TA.glmmTMB %>%
    bind_rows(cellmeans.EXAM.glmmTMB) %>%
    bind_rows(cellmeans.OC.glmmTMB) %>%
    bind_rows(cellmeans.PR.glmmTMB)

effects.all = effects.TA.glmmTMB %>%
    bind_rows(effects.EXAM.glmmTMB) %>%
    bind_rows(effects.OC.glmmTMB) %>%
    bind_rows(effects.PR.glmmTMB)

g1 <- cellmeans.all %>%  
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.CL,
                        ymax=upper.CL, fill=Response, color=Response),
                    position=position_dodge(width=0.5),
                    shape=21) +
    scale_fill_manual('',
                      breaks=c('Exam scores','On course scores', 'Prac vivaosce scores','Total Achievement'),
                      values=c('white','gray','black','black')) + 
    scale_color_manual('',
                      breaks=c('Exam scores','On course scores', 'Prac vivaosce scores','Total Achievement'),
                      values=c('black','black','gray','black')) + 
    facet_wrap(~ftask_change, ncol=4) + 
    theme_bw() +
    theme(legend.position=c(1,0), legend.justification=c(1,0))
g1

g2 <- effects.all %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=ftask_change, ymin=lower.CL, ymax=upper.CL,
                        fill=Response, color=Response),
                    position=position_dodge(width=0.5),
                    shape=21) +
    scale_fill_manual('',
                      breaks=c('Exam scores','On course scores', 'Prac vivaosce scores','Total Achievement'),
                      values=c('white','gray','black','black')) + 
    scale_color_manual('',
                      breaks=c('Exam scores','On course scores', 'Prac vivaosce scores','Total Achievement'),
                      values=c('black','black','gray','black')) + 
    scale_y_continuous('Change in % DNS', labels=function(x) x*100) +
    scale_x_discrete('Task change code') +
    coord_flip() + 
    theme_bw()
g2

## ----end
## ---- compilationFigures1
load(file='../data/cellmeans.TA.brm.RData')
load(file='../data/cellmeans.EXAM.brm.RData')
load(file='../data/cellmeans.OC.brm.RData')
load(file='../data/cellmeans.PR.brm.RData')
load(file='../data/effects.TA.brm.RData')
load(file='../data/effects.EXAM.brm.RData')
load(file='../data/effects.OC.brm.RData')
load(file='../data/effects.PR.brm.RData')

relabel <- function(df) {
    df %>% mutate(ftask_change=recode_factor(ftask_change,
                                      `0`='1.0.0',
                                      `6`='1.0.1',
                                      `21`='2.2.0',
                                      `31`='2.0.2',
                                      `22`='3.3.0',
                                      `3`='4.0.0',
                                      `13`='4.0.1',
                                      `33`='4.4.1',
                                      `4`='5.0.0',
                                      `24`='5.5.0',
                                      `5`='6.0.0')) 
        
}

cellmeans.TA.brm = cellmeans.TA.brm %>% relabel
cellmeans.EXAM.brm = cellmeans.EXAM.brm %>% relabel
cellmeans.OC.brm = cellmeans.OC.brm %>% relabel
cellmeans.PR.brm = cellmeans.PR.brm %>% relabel
effects.TA.brm = effects.TA.brm %>% relabel() %>%
    mutate(ftask_change=factor(ftask_change, levels=rev(levels(ftask_change))))
effects.EXAM.brm = effects.EXAM.brm %>% relabel() %>%
    mutate(ftask_change=factor(ftask_change, levels=rev(levels(ftask_change))))
effects.OC.brm = effects.OC.brm %>% relabel() %>%
    mutate(ftask_change=factor(ftask_change, levels=rev(levels(ftask_change))))
effects.PR.brm = effects.PR.brm %>% relabel() %>%
    mutate(ftask_change=factor(ftask_change, levels=rev(levels(ftask_change))))

g1a <- cellmeans.TA.brm %>%  
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD,
                        ymax=upper.HPD)) +
    facet_wrap(~ftask_change, ncol=4) +
    scale_x_discrete('') +
    scale_y_continuous('Score (%)', labels=function(x) x*100) +
    theme_bw()+
    ggtitle('a) Total achievement')
#g1a
g1b <- cellmeans.TA.brm %>%  
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD,
                        ymax=upper.HPD)) +
    facet_wrap(~ftask_change, ncol=4) + 
    scale_x_discrete('') +
    scale_y_continuous('Score (%)', labels=function(x) x*100) +
    theme_bw() +
    ggtitle('b) Exam score')
#g1b
g1c <- cellmeans.OC.brm %>%  
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD,
                        ymax=upper.HPD)) +
    facet_wrap(~ftask_change, ncol=4) + 
    scale_x_discrete('') +
    scale_y_continuous('Score (%)', labels=function(x) x*100) +
    theme_bw() +
    ggtitle('c) On-course score')
#g1c
g1d <- cellmeans.PR.brm %>%  
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD,
                        ymax=upper.HPD)) +
    facet_wrap(~ftask_change, ncol=4) + 
    scale_x_discrete('') +
    scale_y_continuous('Score (%)', labels=function(x) x*100) +
    theme_bw() +
    ggtitle('d) Prac viva OSCE score')
#g1d


g2a <- effects.TA.brm %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=ftask_change, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % score (2020 - 2019)', labels=function(x) x*100, limits=c(-0.15,0.25)) +
    scale_x_discrete('Task change code') +
    coord_flip() + 
    theme_bw() +
    ggtitle('e) Total achievement effect size')
#g2a
g2b <- effects.EXAM.brm %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=ftask_change, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % score (2020 - 2019)', labels=function(x) x*100, limits=c(-0.15,0.25)) +
    scale_x_discrete('Task change code') +
    coord_flip() + 
    theme_bw() +
    ggtitle('f) Exam score effect size')
#g2b
g2c <- effects.OC.brm %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=ftask_change, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % score (2020 - 2019)', labels=function(x) x*100, limits=c(-0.15,0.25)) +
    scale_x_discrete('Task change code') +
    coord_flip() + 
    theme_bw() +
    ggtitle('g) On-course score effect size')
#g2c
g2d <- effects.PR.brm %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=ftask_change, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % score (2020 - 2019)', labels=function(x) x*100, limits=c(-0.15,0.25)) +
    scale_x_discrete('Task change code') +
    coord_flip() + 
    theme_bw() +
    ggtitle('h) Prac viva OSCE score effect size')
#g2d


g <- (g1a | g1b | g1c | g1d)/(g2a | g2b | g2c | g2d) + plot_layout(heights=c(2,1))

## ((g1a | g2a) / (g1b | g2b) / (g1c | g2c) / (g1d | g2d)) + plot_layout(widths=c(2,1))

## (g1a/g1b/g1c/g1d) | (g2a/g2b/g2c/g2d)

#g1 = (g1a + g2a) + plot_layout(widths=c(2,1))
#g2 = (g1b + g2b) + plot_layout(widths=c(2,1))
#g3 = (g1c + g2c) + plot_layout(widths=c(2,1))
#g4 = (g1d + g2d) + plot_layout(widths=c(2,1))
#g <- g1/g2/g3/g4

ggsave(filename='../output/CompilationFigure.pdf', g, width=15, height=12)
ggsave(filename='../output/CompilationFigure.png', g, width=15, height=12, dpi=300)

## ----end




## ---- moretesting


mod.DNS.subject <- glmmTMB(cbind(DNS,DS) ~ fYear*ftask_change + (1|Subject),
                           data=FullData.subject %>% filter(ftask_change %in% c(0,3,6)), 
                           family='binomial')
summary(mod.DNS.subject)
emmeans(mod.DNS.subject, ~fYear*ftask_change) %>% regrid()


FD=FullData.subject %>% filter(ftask_change %in% c(0,3,6)) %>% droplevels
mod.DNS.inla <- inla(DNS ~ fYear*ftask_change + f(Subject, model='iid'),
                     data=FD,
                     ## Ntrials=newdata.pred$N,
                     Ntrials=FD$N,
                    family='binomial',
                    control.predictor=list(link=1, compute=TRUE),
                    control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, config=TRUE)#,
                    #control.fixed=list(mean=0,prec=0.001)
                    )
summary(mod.DNS.inla)

FullData.DNS = FullData.all %>% filter(!is.na(DNS),!is.na(task_change)) %>% droplevels
FullData.subject = FullData.subject %>% filter(!is.na(DNS),!is.na(task_change)) %>% droplevels

newdata <- with(FD, expand.grid(fYear=levels(fYear),
                                      ftask_change=levels(ftask_change),
                                      Subject=NA, id=NA,
                                      DNS=NA, DS=NA, N=NA))
newdata.pred = FD %>% bind_rows(newdata)
#FD = FullData.subject %>% filter(ftask_change %in% c(0,3,6,13,33,24,4,21,22)) %>% droplevels()
mod.DNS.inla <- inla(DNS ~ fYear*ftask_change + f(Subject, model='iid'),
                     data=newdata.pred,
                     #data=FD,
                     Ntrials=newdata.pred$N,
                     #Ntrials=FD$N,
                    family='binomial',
                    control.predictor=list(link=1, compute=TRUE),
                    control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, config=TRUE)#,
                    #control.fixed=list(mean=0,prec=0.001)
                    )
save(mod.DNS.inla, file='../data/mod.DNS.inla.RData')
save(newdata, file='../data/newdata.DNS.inla.RData')
save(FullData.DNS, file='../data/FullData.DNS.RData')

autoplot(mod.DNS.inla)
## ggplot_inla_residuals(mod.DNS.inla, FullData.DNS$DNS)
ggplot_inla_residuals2(mod.DNS.inla, FullData.DNS$DNS)

xmat = model.matrix(~fYear*ftask_change, newdata)
samples = t(sapply(inla.posterior.sample(1000,mod.DNS.inla), function(x) tail(x$latent,nrow(newdata))))

cellmeans = plogis(samples %*% t(xmat))
colMeans(cellmeans)
Xmat = diag(dim(cellmeans)[2])
newdata.Xmat = cbind(newdata,Xmat)
#newdata.Xmat = cbind(newdata,xmat)
Xmat.year=newdata.Xmat %>% group_by(fYear) %>% summarise(across(is.numeric, mean)) %>%
    dplyr::select(-fYear) %>%
    as.matrix()
MCMCstats <- function(x) {
    hpd = median_hdci(x)
    p=sum(x>0)/length(x)
    data.frame(hpd, 'P>0'=p, 'P<0'=1-p)
}
em.means <- newdata.Xmat %>% dplyr::select(fYear) %>% distinct %>% bind_cols(
    do.call('rbind',apply(as.mcmc(cellmeans %*% t(Xmat.year)),2,
                          MCMCstats))) %>%
    dplyr::rename(`P>0`=P.0, `P<0`=P.0.1)
em.means %>% as.tibble() %>% knitr::kable()
Xmat.year.contrast = Xmat.year[2,]-Xmat.year[1,]
em.contr <- tibble(Contrast='2020-2019',
                       do.call('rbind',apply(as.mcmc(cellmeans %*% cbind(Xmat.year.contrast)),2,
                          MCMCstats)) %>%
    dplyr::rename('P>0'=P.0, 'P<0'=P.0.1))

em.contr %>% as.tibble() %>% 
    knitr::kable()

colMeans(cellmeans)

## ----end

## ---- originalBayesian

rstan::stan_trace(mod.brm$fit)
rstan::stan_ac(mod.brm$fit)
rstan::stan_rhat(mod.brm$fit)
rstan::stan_ess(mod.brm$fit)

pp_check(mod.brm, type='dens_overlay')
pp_check(mod.brm, type='error_scatter_avg')

library(ggeffects)
library(gridExtra)
a=mod.brm %>% ggpredict() %>% plot(add.data=TRUE) 
do.call('grid.arrange', a)

library(DHARMa)
preds <- posterior_predict(mod.brm,  nsamples=250,  summary=FALSE)
mod.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = FullData$total_achievement,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = 'beta')
plot(mod.resids)
DHARMa::testDispersion(mod.resids)
##seems to be underdispersed - less variance than would be expected by beta
## this is very difficult to address, nevertheless, the tests will be more
## concervative...

DHARMa::testZeroInflation(mod.resids)

library(broom.mixed)
tidy(mod.brm) %>% as.data.frame

tidyMCMC(mod.brm$fit, estimate.method='median',  conf.int=TRUE,  conf.method='HPDinterval',  rhat=TRUE, ess=TRUE)

mcmc <- as.matrix(mod.brm)


library(tidybayes)
em <- emmeans(mod.brm, ~fYear|ftask_change) %>%
    gather_emmeans_draws() %>%
    mutate(fit=plogis((.value)))
a=em %>%
    pivot_wider(id_cols=.draw,names_from = c(fYear,ftask_change),values_from = fit) %>%
    dplyr::select(-.draw) %>% as.matrix()

a %>% head

# 0, 3, 4, 5, 6, 13, 21, 22, 24, 31, 33, 34
Xmat = cbind(`0`=c(1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
             `3`=c(0,0, 1,-1, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
             `4`=c(0,0, 0,0, 1,-1, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
             `5`=c(0,0, 0,0, 0,0, 1,-1, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
             `6`=c(0,0, 0,0, 0,0, 0,0, 1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
             `13`=c(0,0, 0,0, 0,0, 0,0, 0,0, 1,-1, 0,0,0,0,0,0,0,0,0,0,0,0),
             `21`=c(0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 1,-1, 0,0, 0,0, 0,0, 0,0, 0,0),
             `22`=c(0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 1,-1, 0,0, 0,0, 0,0, 0,0),
             `24`=c(0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 1,-1, 0,0, 0,0, 0,0),
             `31`=c(0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 1,-1, 0,0, 0,0),
             `33`=c(0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 1,-1, 0,0),
             `34`=c(0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 1,-1)
             )
a %*% Xmat %>% 
    as.data.frame() %>%
    pivot_longer(cols=everything()) %>%
    arrange(name) %>%
    group_by(name) %>%
    median_hdci(value)

Xmat2 = cbind(`0vs6`=Xmat[,'0']-Xmat[,'6'])
Xmat2 = cbind(`0+6`=rowMeans(Xmat[,c('0','6')]),
              `21+31`=rowMeans(Xmat[,c('21','31')]),
              `22`=Xmat[,c('22')],
              `3+13+33`=rowMeans(Xmat[,c('3','13','33')]),
              `4+24+34`=rowMeans(Xmat[,c('4','24','34')]),
              `5`=Xmat[,c('5')],
              `21+31+22+3+13+33+ vs 4+24+34+5`=rowMeans(Xmat[,c('21','31','22','3','13','33')]) - rowMeans(Xmat[,c('4','24','34','5')]),
              `0+6 vs others`=rowMeans(Xmat[,c('0','6')]) - rowMeans(Xmat[,c('3','4','5','13','21','22','24','31','33','34')])
              )
a1=a %*% Xmat2 %>% 
    as.data.frame() %>%
    #pivot_longer(c(`0`,`6`,`0vs6`)) %>%
    pivot_longer(cols=everything()) %>%
    arrange(name) %>%
    group_by(name) 
a2=a1 %>%
    median_hdci(value)
a3=a1 %>% 
    summarise(Bayes.P=sum(value>0)/n(),
              Bayes.P2=sum(value<0)/n())
a4=a1 %>%
    do({
        x=.
        data.frame(Empirical.P=mcmcpvalue(x$value))
    })
a5 = log(a) %*% Xmat2 %>%
    as.data.frame() %>%
    #pivot_longer(c(`0`,`6`,`0vs6`)) %>%
    pivot_longer(cols=everything()) %>%
    arrange(name) %>%
    mutate(value=exp(value),
           value=100*(value-1)) %>%
    group_by(name) %>%
    median_hdci(value) %>%
    dplyr::select(name, perc.value=value, .perc.lower=.lower, .perc.upper=.upper)
a2 %>% full_join(a5) %>% full_join(a3) %>% full_join(a4) %>% as.data.frame


mcmcpvalue <- function(samp)
{
    ## elementary version that creates an empirical p-value for the
    ## hypothesis that the columns of samp have mean zero versus a
    ## general multivariate distribution with elliptical contours.
    
    ## differences from the mean standardized by the observed
    ## variance-covariance factor
    
    ## Note, I put in the bit for single terms
    if (length(dim(samp))==0) {
        std <- backsolve(chol(var(samp)),cbind(0, t(samp)) - mean(samp),transpose = TRUE)
        sqdist <- colSums(std * std)
        sum(sqdist[-1] > sqdist[1])/length(samp)
    }
    else {
        std <- backsolve(chol(var(samp)),cbind(0, t(samp)) - colMeans(samp),transpose = TRUE)
        sqdist <- colSums(std * std)
        sum(sqdist[-1] > sqdist[1])/nrow(samp)
    }
    
}


emmeans(mod.brm, pairwise~fYear, at=list(ftask_change = c('0')),type='response')
emmeans(mod.brm, pairwise~fYear, at=list(ftask_change = c('0')),tran=TRUE)
em

summary(mod.brm)
emmeans(mod.brm, pairwise~fYear, type='response')
emmeans(mod.brm, pairwise~fYear|ftask_change, type='response')$emmeans %>% plot
emmeans(mod.brm, pairwise~fYear|ftask_change, tran=TRUE)

## ----end


## ---- originalTests

# 
# contrast(regrid(em),method = 'pairwise') %>% tidy(conf.int=TRUE) %>% knitr::kable()
# em <- emmeans(mod.TA, pairwise~fYear, tran=TRUE)
# regrid(em$contrasts,transform = 'unlink')
# tidy(em$emmeans, conf.int=TRUE)
# tidy(em$contrasts, conf.int=TRUE)
# 
# 
# mod <- glmmTMB(total_achievement ~ fYear*ftask_change + (1|Subject) + (1|id),
#                data=FullData %>% filter(Subject %in% Subjects),
#                family='beta_family')
# mod <- glmmTMB(on_course ~ fYear*ftask_change + (1|Subject) + (1|id),
#                data=FullData %>% filter(Subject %in% Subjects),
#                family='beta_family')
# mod <- glmmTMB(on_course ~ fYear*ftask_change + (1|Subject) + (1|id),
#                data=FullData %>% filter(Subject %in% Subjects),
#                family='beta_family')
# mod <- glmmTMB(pracvivaosce ~ fYear*ftask_change + (1|Subject) + (1|id),
#                data=FullData %>% filter(Subject %in% Subjects),
#                family='beta_family')
# mod <- glmmTMB(uninvigilated ~ fYear*ftask_change + (1|Subject) + (1|id),
#                data=FullData %>% filter(Subject %in% Subjects),
#                family='beta_family')
# save(mod, file='mod.RData')
# #mod <- glmmTMB(total_achievement ~ fYear*ftask_change + (1|Subject) ,
# #               data=FullData,
# #               family='beta_family')
# #mod <- glmmTMB(total_achievement ~ fYear*ftask_change + (1|Subject) ,
# #               data=FullData,
# #               family='gaussian')
# 
# #mod <- lme(total_achievement ~fYear+ftask_change, random=~1|Subject/id,
# #           data=FullData)
# #mod <- lme(total_achievement ~fYear*ftask_change, random=~1|Subject,
# #           data=FullData %>% filter(Subject %in% Subjects))
# 
# load(file='mod.RData')
# mod.resid<-DHARMa::simulateResiduals(mod,plot = TRUE)
# DHARMa::testDispersion(mod.resid)
# 
# mod <- glmmTMB(total_achievement ~ fYear*ftask_change + (fYear*ftask_change|Subject) + (1|id),
#                data=FullData %>% filter(Subject %in% Subjects),
#                family='beta_family')
# save(mod, file='mod_randomInterceptSlope.RData')
# summary(mod)
# emmeans(mod, pairwise~fYear, type='response')
# emmeans(mod, pairwise~fYear, trans=TRUE)
# emmeans(mod, ~fYear, type='response') %>%
#     as.data.frame %>%
#     ggplot() +
#     geom_pointrange(aes(y=response, x=fYear, ymin=lower.CL, ymax=upper.CL))
# 
# 
# 
# emmeans(mod, pairwise~fYear|ftask_change, type='response')
# emmeans(mod, pairwise~fYear|ftask_change, tran=TRUE) %>% plot()
# emmeans(mod, pairwise~fYear|ftask_change, tran=TRUE)$contrast %>% plot() + geom_vline(xintercept=0)
# emmeans(mod, pairwise~fYear|ftask_change, tran=TRUE, infer=c(TRUE,TRUE))$contrast %>% as.data.frame %>%
#     ggplot() + 
#     geom_pointrange(aes(y=-1*estimate, x=ftask_change, ymin=-1*lower.CL, ymax=-1*upper.CL)) +
#     geom_hline(yintercept=0, linetype='dashed') +
#     scale_y_continuous('Difference between 2020 and 2019 scores') +
#     coord_flip() +
#     theme_bw()
# 
# emmeans(mod, ~fYear|ftask_change, type='response') %>%
#     as.data.frame %>%
#     ggplot() +
#     geom_pointrange(aes(y=response, x=fYear, ymin=lower.CL, ymax=upper.CL)) +
#     facet_wrap(~ftask_change)
# 
# ## Contrasts
# emmeans(mod, pairwise~fYear, at=list(ftask_change = c('0','6')),type='response')
# a=emmeans(mod, pairwise~fYear, at=list(ftask_change = c('0','6')),type='response')
# b = as.glht(a)$linfct
# 
# ## 0, 6
# emmeans(mod, pairwise~fYear, at=list(ftask_change = c('0','6')),tran=TRUE)
# emmeans(mod, pairwise~fYear, at=list(ftask_change = c('0','6')),tran=TRUE)$contrast %>% plot
# ## 1, 11, 21, 31
# emmeans(mod, pairwise~fYear, at=list(ftask_change = c('21','31')),tran=TRUE)
# emmeans(mod, pairwise~fYear, at=list(ftask_change = c('21','31')),tran=TRUE)$contrast %>% plot
# ## 2, 12, 22
# emmeans(mod, pairwise~fYear, at=list(ftask_change = c('22')),tran=TRUE)
# emmeans(mod, pairwise~fYear, at=list(ftask_change = c('22')),tran=TRUE)$contrast %>% plot
# ## 3, 13, 23
# emmeans(mod, pairwise~fYear, at=list(ftask_change = c('3','13','33')),tran=TRUE)
# emmeans(mod, pairwise~fYear, at=list(ftask_change = c('3','13')),tran=TRUE)$contrast %>% plot
# ## 4, 24
# emmeans(mod, pairwise~fYear, at=list(ftask_change = c('4','24','34')),tran=TRUE)
# emmeans(mod, pairwise~fYear, at=list(ftask_change = c('4','24','34')),tran=TRUE)$contrast %>% plot
# 
# a1 = emmeans(mod, pairwise~fYear, at=list(ftask_change = c('21','31','22','3','13','34')),tran=TRUE) %>% as.glht()
# a1=a1$linfct
# a1 = a1[2,]-a1[1,]
# a2 = emmeans(mod, pairwise~fYear, at=list(ftask_change = c('4','24','34','5')),tran=TRUE) %>% as.glht()
# a2=a2$linfct
# a2 = a2[2,]-a2[1,]
# 
# a3 = a2-a1
# emmeans(mod, c('fYear','ftask_change'), contr=list(a1))
# 
# a1 = emmeans(mod, pairwise~fYear, at=list(ftask_change = c('0','6')),tran=TRUE) %>% as.glht()
# a1=a1$linfct
# a1 = a1[2,]-a1[1,]
# a2 = emmeans(mod, pairwise~fYear, at=list(ftask_change = c('3','4','5','13','21','22','24','31','33','34')),tran=TRUE) %>% as.glht()
# a2=a2$linfct
# a2 = a2[2,]-a2[1,]
# 
# a3 = a2-a1
# emmeans(mod, c('fYear','ftask_change'), contr=list(a1))

## ----end


## Withdrawals -------------------------------------------------------------
## ---- readWithdrawals
withdrawals <- read.csv('../data/withdrawals.csv')
glimpse(withdrawals)
head(withdrawals)
## ----end

## ---- processWithdrawals
withdrawals <- withdrawals %>%
    dplyr::select(-X2019,-matches('.end.subject')) %>% 
    pivot_longer(-Subject,
                 names_to=c('Year','By','Junk'),
                 names_pattern = 'X([0-9]{4})\\.(.*)\\.(.*)') %>%
    mutate(YearLevel = as.integer(gsub('..(.).*','\\1',Subject)),
           fYear=factor(Year),
           Year=as.integer(Year),
           fYearLevel=factor(YearLevel),
           YearLevel=as.integer(YearLevel),
           Subject=factor(Subject)) %>%
    dplyr::select(-Junk) %>%
    pivot_wider(values_from=value, names_from=By) %>%
    mutate(start=ifelse(start<census, census, start))
withdrawals %>% head()
## ----end

## brms %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Census ..................................................................
## ---- FitModelWithdrawals_glmmTMB.census
withdrawals %>% ggplot(aes(y=(start-census)/start, x=fYear)) +
    geom_point() +
    geom_line(aes(x=as.numeric(fYear))) +
    facet_wrap(~fYearLevel+Subject)


withdrawals.census.glmmTMB.1 <- glmmTMB(cbind(start-census,census) ~ fYear*fYearLevel + (1|Subject) ,
                                        data=withdrawals,
#                                        weights=withdrawals$start,
                                      family='betabinomial')
withdrawals.census.glmmTMB <- glmmTMB(cbind(start-census,census) ~ fYear + (1|Subject) ,
                                      data=withdrawals,
                                      family='betabinomial')

AIC(withdrawals.census.glmmTMB,withdrawals.census.glmmTMB.1)
summary(withdrawals.census.glmmTMB)

emmeans(withdrawals.census.glmmTMB.1, ~fYear, weights='proportional') %>% regrid() %>% pairs()
emmeans(withdrawals.census.glmmTMB, ~fYear) %>% regrid() %>% contrast(method=list(fYear=c(-1,1)))
emmeans(withdrawals.census.glmmTMB, ~fYear) %>% regrid() %>% contrast(method=list(fYear=c(-1,1))) %>% confint()


emmeans(withdrawals.census.glmmTMB.1, ~fYear|fYearLevel) %>% regrid() %>% pairs() %>% confint()
emmeans(withdrawals.census.glmmTMB.1, ~fYear|fYearLevel) %>% regrid() %>% contrast(method=list(fYear=c(-1,1))) %>% confint() %>% as.data.frame() %>%
    ggplot(aes(y=estimate, ymin=lower.CL, ymax=upper.CL, x=fYearLevel)) +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange() +
    coord_flip() +
    theme_bw()

## ----end
## ---- FitModelWithdrawals_glmmTMB.last
withdrawals %>% ggplot(aes(y=(start-last)/start, x=fYear)) +
    geom_point() +
    geom_line(aes(x=as.numeric(fYear))) +
    facet_wrap(~fYearLevel+Subject)


withdrawals.last.glmmTMB.1 <- glmmTMB(cbind(start-last,last) ~ fYear*fYearLevel + (1|Subject) ,
                                        data=withdrawals,
#                                        weights=withdrawals$start,
                                      family='binomial')
withdrawals.last.glmmTMB <- glmmTMB(cbind(start-last,last) ~ fYear + (1|Subject) ,
                                      data=withdrawals,
                                      family='binomial')

AIC(withdrawals.last.glmmTMB,withdrawals.last.glmmTMB.1)
summary(withdrawals.last.glmmTMB)

emmeans(withdrawals.last.glmmTMB.1, ~fYear, weights='proportional') %>% regrid() %>% pairs()
emmeans(withdrawals.last.glmmTMB, ~fYear) %>% regrid() %>% contrast(method=list(fYear=c(-1,1)))
emmeans(withdrawals.last.glmmTMB, ~fYear) %>% regrid() %>% contrast(method=list(fYear=c(-1,1))) %>% confint()
emmeans(withdrawals.last.glmmTMB.1, ~fYear) %>% regrid() %>% contrast(method=list(fYear=c(-1,1)))
emmeans(withdrawals.last.glmmTMB.1, ~fYear, weights='proportional') %>% regrid() %>% contrast(method=list(fYear=c(-1,1)))
emmeans(withdrawals.last.glmmTMB.1, ~fYear, weights='outer') %>% regrid() %>% contrast(method=list(fYear=c(-1,1)))


emmeans(withdrawals.last.glmmTMB.1, ~fYear|fYearLevel) %>% regrid() %>% pairs() 
emmeans(withdrawals.last.glmmTMB.1, ~fYear|fYearLevel) %>% regrid() %>% pairs() %>% confint()
emmeans(withdrawals.last.glmmTMB.1, ~fYear|fYearLevel) %>% regrid() %>% contrast(method=list(fYear=c(-1,1))) %>% confint() %>% as.data.frame() %>%
    ggplot(aes(y=estimate, ymin=lower.CL, ymax=upper.CL, x=fYearLevel)) +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange() +
    coord_flip() +
    theme_bw()

## ----end

## ---- fitModels-census-brm
beta_binomial2 <- custom_family(
  "beta_binomial2", dpars = c("mu", "phi"),
  links = c("logit", "log"), lb = c(NA, 0),
  type = "int", vars = "vint1[n]"
)
stan_funs <- "
  real beta_binomial2_lpmf(int y, real mu, real phi, int T) {
    return beta_binomial_lpmf(y | T, mu * phi, (1 - mu) * phi);
  }
  int beta_binomial2_rng(real mu, real phi, int T) {
    return beta_binomial_rng(T, mu * phi, (1 - mu) * phi);
  }
"
stanvars <- stanvar(scode = stan_funs, block = "functions")

withdrawals.census.brm.1 <- brm(I(start-census)|vint(start) ~ fYear*fYearLevel + (1|Subject) ,
                                data=withdrawals,
                                        #                                        weights=withdrawals$start,
                                family=beta_binomial2,
                                        # family='binomial',
                                stanvars = stanvars,
                                iter=3000,
                                warmup=1500,
                                chains=3,
                                thin=5,
                                cores=3,
                                control=list(adapt_delta=0.95),
                                prior=c(prior(normal(0,5), class='b'),
                                        prior(normal(0.5,5), class='Intercept')
                                        )
                                )

save(withdrawals.census.brm.1, file='../data/withdrawals.census.brm.1.RData')
## ----end
## ---- fitModel-census-brm-summary
load(file='../data/withdrawals.census.brm.1.RData')
broom::tidy(withdrawals.census.brm.1) %>% dplyr::select(-effect,-component,-group) %>% knitr::kable()  
## ----end
## ---- fitModel-census-brm-MCMCdiagnostics
load(file='../data/withdrawals.census.brm.1.RData')
rstan::stan_trace(withdrawals.census.brm.1$fit)
rstan::stan_ac(withdrawals.census.brm.1$fit)
rstan::stan_rhat(withdrawals.census.brm.1$fit)
rstan::stan_ess(withdrawals.census.brm.1$fit)
## ----end
## ---- fitModel-census-brm-diagnostics
load(file='../data/withdrawals.census.brm.1.RData')
preds <- posterior_predict(withdrawals.census.brm.1,  nsamples=250,  summary=FALSE)
mod.resids <- createDHARMa(simulatedResponse = t(preds),
                           observedResponse = withdrawals$start - withdrawals$census,
                           fittedPredictedResponse = apply(preds, 2, median),
                           integerResponse = 'binomial')
plot(mod.resids)
DHARMa::testDispersion(mod.resids)
## ----end
## ---- fitModels-census-brm2
withdrawals.census.brm <- brm(I(start-census)|vint(start) ~ fYear + (1|Subject) ,
                                data=withdrawals,
                                        #                                        weights=withdrawals$start,
                                #family='binomial',
                                family=beta_binomial2,
                                stanvars = stanvars,
                                iter=3000,
                                warmup=1500,
                                chains=3,
                                thin=5,
                                cores=3,
                                control=list(adapt_delta=0.95),
                                prior=c(prior(normal(0,5), class='b'),
                                        prior(normal(0.5,5), class='Intercept')
                                        )
                                )

save(withdrawals.census.brm, file='../data/withdrawals.census.brm.RData')
## ----end
## ---- fitModel-census-brm-MCMCdiagnostics2
load(file='../data/withdrawals.census.brm.1.RData')
rstan::stan_trace(withdrawals.census.brm.1$fit)
rstan::stan_ac(withdrawals.census.brm.1$fit)
rstan::stan_rhat(withdrawals.census.brm.1$fit)
rstan::stan_ess(withdrawals.census.brm.1$fit)
## ----end
## ---- fitModel-census-brm-diagnostics2
load(file='../data/withdrawals.census.brm.RData')
preds <- posterior_predict(withdrawals.census.brm,  nsamples=250,  summary=FALSE)
mod.resids <- createDHARMa(simulatedResponse = t(preds),
                           observedResponse = withdrawals$start - withdrawals$census,
                           fittedPredictedResponse = apply(preds, 2, median),
                           integerResponse = 'binomial')
plot(mod.resids)
DHARMa::testDispersion(mod.resids)
## ----end
                                
## ---- fitModel-census-brm-contrasts1a
load(file='../data/withdrawals.census.brm.1.RData')
em <- emmeans(withdrawals.census.brm.1, ~fYear)
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())
em.contr %>% 
    tidy(conf.int=TRUE) %>%
    left_join(P) %>%
    knitr::kable() 
## ----end
## ---- fitModel-census-brm-contrasts1a2
load(file='../data/withdrawals.census.brm.RData')
em <- emmeans(withdrawals.census.brm, ~fYear)
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())
em.contr %>% 
    tidy(conf.int=TRUE) %>%
    left_join(P) %>%
    knitr::kable()
## ----end
## ---- fitModel-census-brm-contrasts1figA
g1 <- em.means %>% as.data.frame() %>% 
    ggplot() + 
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Withdrawal rate (% per subject)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % withdrawal rate per subject', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-census-brm-contrasts2
load(file='../data/withdrawals.census.brm.1.RData')
em <- emmeans(withdrawals.census.brm.1, ~fYear*fYearLevel) %>% regrid

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD, ymax=upper.HPD)) +
    facet_wrap(~fYearLevel) + 
    theme_bw()

em.contr <- em %>% contrast(method=list(fYear=c(-1,1)), by='fYearLevel', infer=c(TRUE,TRUE))
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast, fYearLevel) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())

g2 <- em %>% contrast(method=list(fYear=c(-1,1)), by='fYearLevel', infer=c(TRUE,TRUE)) %>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=fYearLevel, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % withdrawal rate', labels=function(x) x*100) +
    scale_x_discrete('Year level') +
    coord_flip() + 
    theme_bw()
g1 + g2
 
em %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr %>%
    tidy(conf.int=TRUE) %>%
    left_join(P) %>% 
    dplyr::select(-term,-null.value) %>%
    knitr::kable()
## ----end

## Last ..................................................................
## ---- FitModelWithdrawals_glmmTMB.last
withdrawals %>% ggplot(aes(y=(start-last)/start, x=fYear)) +
    geom_point() +
    geom_line(aes(x=as.numeric(fYear))) +
    facet_wrap(~fYearLevel+Subject)

withdrawals.last.glmmTMB.1 <- glmmTMB(cbind(start-last,last) ~ fYear*fYearLevel + (1|Subject) ,
                                        data=withdrawals,
                                      family='betabinomial')
withdrawals.last.glmmTMB <- glmmTMB(cbind(start-last,last) ~ fYear + (1|Subject) ,
                                      data=withdrawals,
                                      family='betabinomial')

AIC(withdrawals.last.glmmTMB,withdrawals.last.glmmTMB.1)
summary(withdrawals.last.glmmTMB)

emmeans(withdrawals.last.glmmTMB.1, ~fYear, weights='proportional') %>% regrid() %>% pairs()
emmeans(withdrawals.last.glmmTMB, ~fYear) %>% regrid() %>% contrast(method=list(fYear=c(-1,1)))
emmeans(withdrawals.last.glmmTMB, ~fYear) %>% regrid() %>% contrast(method=list(fYear=c(-1,1))) %>% confint()


emmeans(withdrawals.last.glmmTMB.1, ~fYear|fYearLevel) %>% regrid() %>% pairs() %>% confint()
emmeans(withdrawals.last.glmmTMB.1, ~fYear|fYearLevel) %>% regrid() %>% contrast(method=list(fYear=c(-1,1))) %>% confint() %>% as.data.frame() %>%
    ggplot(aes(y=estimate, ymin=lower.CL, ymax=upper.CL, x=fYearLevel)) +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange() +
    coord_flip() +
    theme_bw()

## ----end

## ---- fitModels-last-brm
beta_binomial2 <- custom_family(
  "beta_binomial2", dpars = c("mu", "phi"),
  links = c("logit", "log"), lb = c(NA, 0),
  type = "int", vars = "vint1[n]"
)
stan_funs <- "
  real beta_binomial2_lpmf(int y, real mu, real phi, int T) {
    return beta_binomial_lpmf(y | T, mu * phi, (1 - mu) * phi);
  }
  int beta_binomial2_rng(real mu, real phi, int T) {
    return beta_binomial_rng(T, mu * phi, (1 - mu) * phi);
  }
"
stanvars <- stanvar(scode = stan_funs, block = "functions")

withdrawals.last.brm.1 <- brm(I(start-last)|vint(start) ~ fYear*fYearLevel + (1|Subject) ,
                                data=withdrawals,
                                        #                                        weights=withdrawals$start,
                                family=beta_binomial2,
                                        # family='binomial',
                                stanvars = stanvars,
                                iter=3000,
                                warmup=1500,
                                chains=3,
                                thin=5,
                                cores=3,
                                control=list(adapt_delta=0.95),
                                prior=c(prior(normal(0,5), class='b'),
                                        prior(normal(0.5,5), class='Intercept')
                                        )
                                )

save(withdrawals.last.brm.1, file='../data/withdrawals.last.brm.1.RData')
## ----end
## ---- fitModel-last-brm-summary
load(file='../data/withdrawals.last.brm.1.RData')
broom::tidy(withdrawals.last.brm.1) %>% dplyr::select(-effect,-component,-group) %>% knitr::kable()  
## ----end
## ---- fitModel-last-brm-MCMCdiagnostics
load(file='../data/withdrawals.last.brm.1.RData')
rstan::stan_trace(withdrawals.last.brm.1$fit)
rstan::stan_ac(withdrawals.last.brm.1$fit)
rstan::stan_rhat(withdrawals.last.brm.1$fit)
rstan::stan_ess(withdrawals.last.brm.1$fit)
## ----end
## ---- fitModel-last-brm-diagnostics
load(file='../data/withdrawals.last.brm.1.RData')
preds <- posterior_predict(withdrawals.last.brm.1,  nsamples=250,  summary=FALSE)
mod.resids <- createDHARMa(simulatedResponse = t(preds),
                           observedResponse = withdrawals$start - withdrawals$last,
                           fittedPredictedResponse = apply(preds, 2, median),
                           integerResponse = 'binomial')
plot(mod.resids)
DHARMa::testDispersion(mod.resids)
## ----end
## ---- fitModels-last-brm2
withdrawals.last.brm <- brm(I(start-last)|vint(start) ~ fYear + (1|Subject) ,
                                data=withdrawals,
                                        #                                        weights=withdrawals$start,
                                #family='binomial',
                                family=beta_binomial2,
                                stanvars = stanvars,
                                iter=3000,
                                warmup=1500,
                                chains=3,
                                thin=5,
                                cores=3,
                                control=list(adapt_delta=0.95),
                                prior=c(prior(normal(0,5), class='b'),
                                        prior(normal(0.5,5), class='Intercept')
                                        )
                                )

save(withdrawals.last.brm, file='../data/withdrawals.last.brm.RData')
## ----end
## ---- fitModel-last-brm-MCMCdiagnostics2
load(file='../data/withdrawals.last.brm.1.RData')
rstan::stan_trace(withdrawals.last.brm.1$fit)
rstan::stan_ac(withdrawals.last.brm.1$fit)
rstan::stan_rhat(withdrawals.last.brm.1$fit)
rstan::stan_ess(withdrawals.last.brm.1$fit)
## ----end
## ---- fitModel-last-brm-diagnostics2
load(file='../data/withdrawals.last.brm.RData')
preds <- posterior_predict(withdrawals.last.brm,  nsamples=250,  summary=FALSE)
mod.resids <- createDHARMa(simulatedResponse = t(preds),
                           observedResponse = withdrawals$start - withdrawals$last,
                           fittedPredictedResponse = apply(preds, 2, median),
                           integerResponse = 'binomial')
plot(mod.resids)
DHARMa::testDispersion(mod.resids)
## ----end
                                
## ---- fitModel-last-brm-contrasts1a
load(file='../data/withdrawals.last.brm.1.RData')
em <- emmeans(withdrawals.last.brm.1, ~fYear)
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())
em.contr %>% 
    tidy(conf.int=TRUE) %>%
    left_join(P) %>%
    knitr::kable() 
## ----end
## ---- fitModel-last-brm-contrasts1a2
load(file='../data/withdrawals.last.brm.RData')
em <- emmeans(withdrawals.last.brm, ~fYear)
em.means <- regrid(em) 
em.means %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr <- contrast(regrid(em),method = list('2020 - 2019'=c(-1,1)), infer=c(TRUE,TRUE)) 
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())
em.contr %>% 
    tidy(conf.int=TRUE) %>%
    left_join(P) %>%
    knitr::kable()

## ----end
## ---- fitModel-last-brm-contrasts1figA
g1 <- em.means %>% as.data.frame() %>% 
    ggplot() + 
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Withdrawal rate (% per subject)', labels=function(x) x*100) +
    scale_x_discrete('') +
    theme_bw()
g2 <- em.contr %>% as.data.frame() %>%
    ggplot() +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=contrast, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % withdrawal rate per subject', labels=function(x) x*100) +
    scale_x_discrete('') +
    coord_flip() + 
    theme_bw()
g1 + g2
## ----end
## ---- fitModel-last-brm-contrasts2
load(file='../data/withdrawals.last.brm.1.RData')
em <- emmeans(withdrawals.last.brm.1, ~fYear*fYearLevel) %>% regrid

g1 <- em %>% as.data.frame %>% 
    ggplot() +
    geom_pointrange(aes(y=response, x=fYear, ymin=lower.HPD, ymax=upper.HPD)) +
    facet_wrap(~fYearLevel) + 
    theme_bw()

em.contr <- em %>% contrast(method=list(fYear=c(-1,1)), by='fYearLevel', infer=c(TRUE,TRUE))
P=em.contr %>% gather_emmeans_draws() %>% 
    group_by(contrast, fYearLevel) %>% 
    summarise('P>0'=sum(.value>0)/n(), 'P<0'=sum(.value<0)/n())

g2 <- em %>% contrast(method=list(fYear=c(-1,1)), by='fYearLevel', infer=c(TRUE,TRUE)) %>%
    as.data.frame %>% 
    ggplot()+
    geom_hline(yintercept=0, linetype='dashed') +
    geom_pointrange(aes(y=estimate, x=fYearLevel, ymin=lower.HPD, ymax=upper.HPD)) +
    scale_y_continuous('Change in % withdrawal rate', labels=function(x) x*100) +
    scale_x_discrete('Year level') +
    coord_flip() + 
    theme_bw()
g1 + g2
 
em %>% tidy(conf.int=TRUE) %>% knitr::kable()
em.contr %>%
    tidy(conf.int=TRUE) %>%
    left_join(P) %>% 
    dplyr::select(-term,-null.value) %>%
    knitr::kable()
## ----end
