# AssortativeMating.R
# 8 July 2021

# scrub memory for fresh start
rm(list=ls())

# bring in the data
setwd("C:/Users/brett.sandercock/OneDrive - NINA/Documents/Papers/Rae_Broadbills")
pairs = read.csv("AssortativeMating.csv", header=T)
pairs$Pair1 = factor(pairs$Pair1)
str(pairs)

pairs = pairs[pairs$Pair1!="20", ] # drop female-female pair on two separate lines
pairs = pairs[pairs$Pair1!="34", ] # drop remated pair same as pair 30
pairs$Pair1 = factor(pairs$Pair1)
levels(pairs$Pair1)
length(levels(pairs$Pair1))

# coding for morphometrics: 1 = female, 2 = male
pairs = subset(pairs, select=c(wing1, foot1, bill1, head1, mass1, wing2, foot2, bill2, head2, mass2))

# correlations
wing.m = cor.test(pairs$wing1,pairs$wing2, method='pearson')
foot.m = cor.test(pairs$foot1,pairs$foot2, method='pearson')
bill.m = cor.test(pairs$bill1,pairs$bill2, method='pearson')
head.m = cor.test(pairs$head1,pairs$head2, method='pearson', use='complete.obs')
mass.m = cor.test(pairs$mass1,pairs$mass2, method='pearson')

# regression to get the SDI values for males of different size
lm(pairs$bill1 ~ pairs$bill2)
# Coefficients:
# (Intercept)  pairs$bill2  
#     40.1551      -0.2494  

# mar= bottom, left, top, right
windows()
png("AssortativeMating.png", width = 21, height = 21, units = 'cm', res = 600)
par(mfrow=c(2,2), mar = c(5,5,0.5,0.5))

# bill
x.range = c(min(pairs$bill2)*0.98, max(pairs$bill2)*1.02)
y.range = c(min(pairs$bill1)*0.98, max(pairs$bill1)*1.02)
plot(pairs$bill1 ~ pairs$bill2, xlim=x.range, ylim=y.range, pch=16, cex=1.5, xlab='Male bill (mm)', ylab='Female bill (mm)', cex.lab=1.5, cex.axis=1.5)
label = paste("r=", round(bill.m$estimate,3), "p=", round(bill.m$p.value,3))
text(30,35.5, labels=label, cex=1.5)
clip(min(pairs$bill2), max(pairs$bill2), min(pairs$bill1), max(pairs$bill1))
abline(lm(pairs$bill1 ~ pairs$bill2), lwd=2)
#abline(lm(pairs.rand$bill1 ~ pairs.rand$bill2), lty=2, lwd=2)
#abline(a=0, b=1, lty=2)

# head
#x.range = c(min(pairs$head2, na.rm = TRUE)*0.98, max(pairs$head2, na.rm = TRUE)*1.02)
#y.range = c(min(pairs$head1, na.rm = TRUE)*0.98, max(pairs$head1, na.rm = TRUE)*1.02)
#plot(pairs$head1 ~ pairs$head2, xlim=x.range, ylim=y.range, pch=16, cex=1.5, xlab='Male head (mm)', ylab='Female head (mm)', cex.lab=1.5, cex.axis=1.5)
#label = paste("r=", round(head.m$estimate,3), "p=", round(head.m$p.value,3))
#text(54,58, labels=label, cex=1.5)
#clip(min(pairs$head2, na.rm = T), max(pairs$head2, na.rm = T), min(pairs$head1, na.rm = T), max(pairs$head1, na.rm = T))
#abline(lm(pairs$head1 ~ pairs$head2), lwd=2)
#abline(lm(pairs.rand$head1 ~ pairs.rand$head2), lty=2, lwd=2)
#abline(a=0, b=1, lty=2)

# wing
x.range = c(min(pairs$wing2)*0.98, max(pairs$wing2)*1.02)
y.range = c(min(pairs$wing1)*0.98, max(pairs$wing1)*1.02)
plot(pairs$wing1 ~ pairs$wing2, xlim=x.range, ylim=y.range, pch=16, cex=1.5, xlab='Male wing (mm)', ylab='Female wing (mm)', cex.lab=1.5, cex.axis=1.5)
label = paste("r=", round(wing.m$estimate,3), "p=", round(wing.m$p.value,3))
text(108.5,116, labels=label, cex=1.5)
clip(min(pairs$wing2), max(pairs$wing2), min(pairs$wing1), max(pairs$wing1))
abline(lm(pairs$wing1 ~ pairs$wing2), lwd=2)
#abline(lm(pairs.rand$wing1 ~ pairs.rand$wing2), lty=2, lwd=2)
#abline(a=0, b=1, lty=2)

# foot
x.range = c(min(pairs$foot2)*0.98, max(pairs$foot2)*1.02)
y.range = c(min(pairs$foot1)*0.98, max(pairs$foot1)*1.02)
plot(pairs$foot1 ~ pairs$foot2, xlim=x.range, ylim=y.range, pch=16, cex=1.5, xlab='Male foot (mm)', ylab='Female foot (mm)', cex.lab=1.5, cex.axis=1.5)
label = paste("r=", round(foot.m$estimate,3), "p=", round(foot.m$p.value,3))
text(43,47.4, labels=label, cex=1.5)
clip(min(pairs$foot2), max(pairs$foot2), min(pairs$foot1), max(pairs$foot1))
abline(lm(pairs$foot1 ~ pairs$foot2), lwd=2)
#abline(lm(pairs.rand$foot1 ~ pairs.rand$foot2), lty=2, lwd=2)
#abline(a=0, b=1, lty=2)

# mass
x.range = c(min(pairs$mass2)*0.95, max(pairs$mass2)*1.05)
y.range = c(min(pairs$mass1)*0.95, max(pairs$mass1)*1.05)
plot(pairs$mass1 ~ pairs$mass2, xlim=x.range, ylim=y.range, pch=16, cex=1.5, xlab='Male mass (g)', ylab='Female mass (g)', cex.lab=1.5, cex.axis=1.5)
label = paste("r=", round(mass.m$estimate,3), "p=", round(mass.m$p.value,3))
text(41,45, labels=label, cex=1.5)
clip(min(pairs$mass2), max(pairs$mass2), min(pairs$mass1), max(pairs$mass1))
abline(lm(pairs$mass1 ~ pairs$mass2), lwd=2)
#abline(lm(pairs.rand$mass1 ~ pairs.rand$mass2), lty=2, lwd=2)
#abline(a=0, b=1, lty=2)

# save to png file
dev.off()


# generate random pairs
males = subset(pairs, select=c(wing1, foot1, bill1, head1, mass1))
females = subset(pairs, select=c(wing2, foot2, bill2, head2, mass2))
set.seed(NULL) # different random set each time
females$random = runif(nrow(females),0,1) # add a random value
females = females[order(females$random),] # sort by random value
pairs.rand = cbind(males, females)


# tests versus random
# significance of tests varies widely because of small samples
# bill
fit = lm(pairs$bill1 ~ pairs$bill2)
sfit<- summary(fit)
rfit = lm(pairs.rand$bill1 ~ pairs.rand$bill2)
x = unname(rfit$coefficients[2])
# Compute t-student H0: slope = x. The estimation of coefficients and their s.d. are in sfit$coefficients
tstats <- (x-sfit$coefficients[2,1])/sfit$coefficients[2,2]
# Calculates two tailed probability
pval<- 2 * pt(abs(tstats), df = df.residual(fit), lower.tail = FALSE)
print(pval)

# wing
fit = lm(pairs$wing1 ~ pairs$wing2)
sfit<- summary(fit)
rfit = lm(pairs.rand$wing1 ~ pairs.rand$wing2)
x = unname(rfit$coefficients[2])
# Compute t-student H0: slope = x. The estimation of coefficients and their s.d. are in sfit$coefficients
tstats <- (x-sfit$coefficients[2,1])/sfit$coefficients[2,2]
# Calculates two tailed probability
pval<- 2 * pt(abs(tstats), df = df.residual(fit), lower.tail = FALSE)
print(pval)

# foot
fit = lm(pairs$foot1 ~ pairs$foot2)
sfit<- summary(fit)
rfit = lm(pairs.rand$foot1 ~ pairs.rand$foot2)
x = unname(rfit$coefficients[2])
# Compute t-student H0: slope = x. The estimation of coefficients and their s.d. are in sfit$coefficients
tstats <- (x-sfit$coefficients[2,1])/sfit$coefficients[2,2]
# Calculates two tailed probability
pval<- 2 * pt(abs(tstats), df = df.residual(fit), lower.tail = FALSE)
print(pval)

# mass
fit = lm(pairs$mass1 ~ pairs$mass2)
sfit<- summary(fit)
rfit = lm(pairs.rand$mass1 ~ pairs.rand$mass2)
x = unname(rfit$coefficients[2])
# Compute t-student H0: slope = x. The estimation of coefficients and their s.d. are in sfit$coefficients
tstats <- (x-sfit$coefficients[2,1])/sfit$coefficients[2,2]
# Calculates two tailed probability
pval<- 2 * pt(abs(tstats), df = df.residual(fit), lower.tail = FALSE)
print(pval)

