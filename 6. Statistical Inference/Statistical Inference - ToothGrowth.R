data(ToothGrowth)

head(ToothGrowth)

summary(ToothGrowth)

str(ToothGrowth)

dim(ToothGrowth)

table(ToothGrowth[,2:3])

library(ggplot2)

g <-  ggplot(ToothGrowth, aes(supp, len)) +
        geom_boxplot(aes(fill = supp)) + geom_jitter(width = 0.1) +
        facet_wrap(~ dose, nrow = 1, ncol = 3) +
        labs(title = "Toothgrowth as function of dose (mg) and supplement") +
        labs(x = "Supplement", y ="Tooth Length")
        
LENOJHALF <- ToothGrowth$len[(ToothGrowth$supp=="OJ" & ToothGrowth$dose==0.5)]
LENVCHALF <- ToothGrowth$len[(ToothGrowth$supp=="VC" & ToothGrowth$dose==0.5)]
LENOJONE <- ToothGrowth$len[(ToothGrowth$supp=="OJ" & ToothGrowth$dose==1)]
LENVCONE <- ToothGrowth$len[(ToothGrowth$supp=="VC" & ToothGrowth$dose==1)]
LENOJTWO <- ToothGrowth$len[(ToothGrowth$supp=="OJ" & ToothGrowth$dose==2)]
LENVCTWO <- ToothGrowth$len[(ToothGrowth$supp=="VC" & ToothGrowth$dose==2)]

t.test(LENOJHALF, LENVCHALF, alternative = "two.sided", mu=0, paired=FALSE, var.equal=FALSE, conf.level=0.95)
t.test(LENOJONE, LENVCONE, alternative = "two.sided", mu=0, paired=FALSE, var.equal=FALSE, conf.level=0.95)
t.test(LENOJTWO, LENVCTWO, alternative = "two.sided", mu=0, paired=FALSE, var.equal=FALSE, conf.level=0.95)
