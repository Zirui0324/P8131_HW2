# Fit logistic models

# EX1: Show/No-show
# Investigating the relation between show/no-show and appointment lag.

data1 = read.table('E:/P8131Spring2023/Bin_2023/Code/Lagtime_1.csv',header=TRUE,sep=',')
table(data1[,1]) # 521 show, 125 no-show
dim(data1) # 645*2
names(data1)
levels(factor(data1$Internal.Status)) 

fit=glm(factor(Internal.Status)~Appointment.Lag,family=binomial(link='logit'),data=data1)
summary(fit) 

exp(fit$coefficients)[2] # odds ratio of no-shdataow with one day increase in lag 


# GoF (ungrouped data) check Hosmer-Lemeshow for ungrouped data
library(ResourceSelection)
hoslem.test(fit$y, fitted(fit), g=10)  # fitted: returns \hat{pi}
#fails to reject, fit is ok

#95% CI for beta
vcov(fit) # inverse of fisher information matrix
CI1=fit$coefficients + kronecker(t(c(0,qnorm(0.025),-qnorm(0.025))),t(t(sqrt(diag(vcov(fit))))))
out=cbind(exp(CI1)[-1,,drop=FALSE],coef(summary(fit))[-1,4,drop=FALSE])
colnames(out)=c('OR','95% CI','95% CI','p-value')
rownames(out)=c('Lag')
out


# EX2: Pub
# Peer reviewed publication: Comparing urology fellows with and without time off in terms of
# their proportions of urological publications.

data2 = read.table('E:/P8131Spring2023/Bin_2023/Code/MedEd Stats.csv',header=TRUE,sep=',')
sum(data2$timeoff) # 26 timeoff, 162 no timeoff

# fit model
# (#pr, #total-#pr) ~ timeoff
resp=cbind(data2$Urology.Publication,data2$Total.Publications-data2$Urology.Publication)
pred=data2$timeoff

fit2=glm(resp~pred,family=binomial(link='logit'))
summary(fit2)

exp(fit2$coefficients)[2] # odds ratio of urology pub with timeoff vs no-timeoff


# GoF
sum(residuals(fit,type='pearson')^2) # pearson chisq

dev=sum(residuals(fit,type='deviance')^2);dev # deviance (or obtain from summary(glm_logit))

# compare with chisq(188-2)
pval=1-pchisq(dev,186);pval # fit is not good, later will see why (over dispersion; lack of covariate)


# equivalently
uropub=xtabs(data2$Urology.Publication~data2$timeoff)
allpub=xtabs(data2$Total.Publications~data2$timeoff)
resp1=cbind(uropub,allpub-uropub)
fit3=glm(resp1~c(0,1),family=binomial(link='logit'))
summary(fit3) 
exp(fit3$coefficients)[2] 


# 95% CI
CI1=fit$coefficients + kronecker(t(c(0,qnorm(0.025),-qnorm(0.025))),t(t(sqrt(diag(vcov(fit))))))
out=cbind(exp(CI1)[-1,,drop=FALSE],coef(summary(fit))[-1,4,drop=FALSE])
colnames(out)=c('OR','95% CI','95% CI','p-value')
rownames(out)=c('timeoff')
out 

