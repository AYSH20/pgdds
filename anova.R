##One way anova

setwd("F:/PGDDS/PGDDS DAY10 ACTIVITY DATA SETS")

satlevel <- read.csv("One way anova.csv")

anovatable <- aov(formula = satindex~dept, data = satlevel)
summary(anovatable)
anovatable
boxplot(satlevel$satindex~satlevel$dept)


#sales <-c(8,12.5,9.2,6.7,9.4,5.9,7.7,6.9
#,10.2,9.3,9.9,8.7,9.1,10.2,9.5,10,5.3
#,5.8,6,7.1,7,6.1,6.3,7.3)
#cbind(North,West,South)

write.csv(sales,file.choose())

market_campaign <- data.frame(North,West,South)
market_campaign <- read.csv("marketcam.csv")


anovatableformarketcampaign <- aov(formula = sales~region,data = market_campaign )
summary(anovatableformarketcampaign)

aggregate(sales~region, data = market_campaign, FUN = sum)


## Two way anova

twoway <- read.csv("Two way anova.csv")
annovatable_Twowayanova <- aov(data = twoway, formula = satindex~dept*exp)

summary(annovatable_Twowayanova)

setwd("F:/PGDDS/PGDDS DAY11 ACTIVITY DATA SETS")
campaign_test <- read.csv("Three Way Campaign Data.csv")
anova3 <- aov(formula = growth~campaign*region*size,data = campaign_test)
anova3
summary(anova3)


#Visualize Interactions:

attach(campaign_test)
par(mfrow=c(1,1))
interaction.plot(campaign,region,growth)
interaction.plot(size,region,growth)

detach(campaign_test)
aptitude <- read.csv("Mann Whitney U test.csv")
wilcox.test(formula=aptscore~Group,data = aptitude)

signed <- read.csv("Wilcoxon Signed Rank test.csv")
attach(signed)
wilcox.test(Before,After,paired = T,alternative = "less")

kruskal <- read.csv("Kruskal Wallis Test.csv")
kruskal.test(formula = aptscore~Group , data=kruskal)


chi <- read.csv("chi square association.csv")
library(gmodels)
CrossTable(chi$performance,chi$source,chisq = T)
chisq.test(chi$performance,chi$source)
chisq.test()


#TO TEST NORMALITY
setwd("F:/PGDDS/PGDDS DAY12 ACTIVITY DATA SETS")
testData <- read.csv("Normality Testing Data.csv")
attach(testData)
par(mfrow=c(1,2))
boxplot(csi,col="blue")
boxplot(billamt,col = "maroon")

#1. CHECKING  SKEWNESS
library(e1071)
skewness(csi)
skewness(billamt)

#2. QUANTILE QUANTILE PLOT

qqnorm(csi,col="blue")
qqline(csi,col="red") #draws a 45 deg line 

qqnorm(billamt,col="green")
qqline(billamt,col="red")

shapiro.test(csi)
shapiro.test(billamt)

install.packages("nortest")

#post hoc test

#1. using ANOVA
apttest <- read.csv("Post Hoc Tests.csv")
attach(apttest)
anova4 <- aov(formula = aptscore~grade,data = apttest)
summary(anova4)

#2. using pairwise without bonf var.

pairwise.t.test(aptscore,grade,p.adjust.method = "none")

#3. using bonf var.

pairwise.t.test(aptscore,grade,p.adjust.method = "bonf")

boxplot()

TukeyHSD(anova4,"grade")
#lwr and upr are lower and upper bounds


anova4hd <- read.csv("Four Factor Anova in Marketing Analytics.csv")
summary(anova4hd)
anova5 <- aov(formula = growth~campaign*region*store*size, data = anova4hd)
anova5
summary(anova5)
