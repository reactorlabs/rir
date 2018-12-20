options(continue="  ", width=60)
options(SweaveHooks=list(fig=function() par(mar=c(4.1, 4.1, .3, 1.1))))
pdf.options(pointsize=8) #text in graph about the same as regular text
options(contrasts=c("contr.treatment", "contr.poly")) #reset default
require(survival)
mfit <- coxph(Surv(futime, death) ~ sex + pspline(age, df=4), data=mgus)
mfit
termplot(mfit, term=2, se=TRUE, col.term=1, col.se=1)
ptemp <- termplot(mfit, se=TRUE, plot=FALSE)
attributes(ptemp)
ptemp$age[1:4,]
ageterm <- ptemp$age  # this will be a data frame
center <- with(ageterm, y[x==50])
ytemp <- ageterm$y + outer(ageterm$se, c(0, -1.96, 1.96), '*')
matplot(ageterm$x, exp(ytemp - center), log='y',
        type='l', lty=c(1,2,2), col=1, 
        xlab="Age at diagnosis", ylab="Relative death rate")
fit <- coxph(Surv(futime, death) ~ age + pspline(hgb, 4), mgus2)
fit
termplot(fit, se=TRUE, term=2, col.term=1, col.se=1,
         xlab="Hemoglobin level")
termplot(fit, se=TRUE, col.term=1, col.se=1, term=2,
         xlab="Hemoglobin level", ylim=c(-.4, 1.3))
df <- c(3, 2.5, 2)
for (i in 1:3) {
    tfit <- coxph(Surv(futime, death) ~ age + 
                  pspline(hgb, df[i], nterm=8), mgus2)
    temp <- termplot(tfit, se=FALSE, plot=FALSE, term=2)
    lines(temp$hgb$x, temp$hgb$y, col=i+1, lwd=2)
}
