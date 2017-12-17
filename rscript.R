##Load Libraries
library(readxl)
library(car)
library(ggpubr)

##QUEST0
quest0 <- read_excel("~/Desktop/NOVA IMS/Statistics/ANOVA - R/dir_anova_stats/quest0.xls")
#Descriptive statistics - dataset
summary(quest0)
gender_freq<-prop.table(table(quest0$gender))
round(gender_freq, 2)           

#Mean and SD of the differences of weight 
mean <- tapply(quest0$Diff, quest0$Diet, mean)
sdev <- tapply(quest0$Diff, quest0$Diet, sd)
exp_quest0 <- cbind (mean, sdev)
exp_quest0

#Recoding/Renaming Diets
quest0$Diet[quest0$Diet=="1"] <- "Dieta 1"
quest0$Diet[quest0$Diet=="2"] <- "Dieta 2"
quest0$Diet[quest0$Diet=="3"] <- "Dieta 3"
View(quest0)

#Exploratory data from weight loss
round(exp_quest0, 3)

#Boxplot
#col nova: col="#c0d630") 
boxplot(quest0$Diff ~ quest0$Diet, data=quest0, col=(c("#c0d630", "#5d666d")), 
        ylab= "Peso Perdido em 6 Semanas (kg)", xlab= "ID Dieta") 

##QUEST1 (femVSMas)
quest1 <- read_excel("~/Desktop/NOVA IMS/Statistics/ANOVA - R/dir_anova_stats/quest1.xlsx")
gender_freq<-prop.table(table(quest0$gender))
round(gender_freq, 2)  
#summary
summary(quest1)
#SDEV
sd(quest1$`Peso Homens`, na.rm = TRUE)
sd(quest1$`Peso Mulheres`, na.rm = TRUE)
#Boxplot Function
boxplotHM <- function(column_name1, column_name2)
{return(boxplot(c(quest1[column_name1], quest1[column_name2]), 
                  col=(c("#5d666d", "#c0d630")), main="Pesos Originais", xlab=("Sexo")))}
boxplotHM('Peso Homens', 'Peso Mulheres')

#Normality
#Levene Test (assumption of homogeneity of variance)
leveneTest(y = quest0$`pre,weight`, group = quest0$gender)


##QUEST2 (pesos_iniciais_age)
quest2 <- read_excel("~/Desktop/NOVA IMS/Statistics/ANOVA - R/dir_anova_stats/quest2.xlsx")
summary(quest2)
sd(quest2$`15-29a`, na.rm = TRUE)
sd(quest2$`30-44a`, na.rm = TRUE)
sd(quest2$`45-60a`, na.rm = TRUE)

boxplot_age <- function(column_name1, column_name2, column_name3)
{return(boxplot(c(quest2[column_name1], quest2[column_name2], quest2[column_name3]), 
                  col=(c("#c0d630", "#5d666d", "#c0d630")), main="Pesos por Idade"))}
boxplot_age("15-29a", "30-44a", "45-60a")


##Questão 2 - Pesos Iniciais/Age
#Anova
anova_ages <- aov(quest0$Diff ~ quest0$Age_Group)
summary(anova_ages)

#Assumption of normality: shapiro residuals and qq plot from diff(ages)
#shapiro
shapiro.test(quest0$`pre,weight`)
res_ages <- anova_ages$residuals
hist(res_ages, main = "Residuos por Grupo Etario", xlab = "Peso Perdido", col=(c("#c0d630", "#5d666d")))
#QQPlot for pre-weight
ggqqplot(quest0$`pre,weight`,col=(c("#c0d630")), main="Q-Q Plot do Peso Inicial")

#C:Residuals and qqplot seem to be normally destributed

#Levene Test (assumption of homogeneity of variance)
leveneTest(y = quest0$`pre,weight`, group = quest0$Age_Group)
#Levene test shows that pr(>F)=0.514. Because this p-value(0,51) is bigger than 0.05, equal variances could be assumed

##Questão 3 
#Anova

#QQPlot for Diff
ggqqplot(quest0$Diff,col=(c("#c0d630")), main="Q-Q Plot do Peso Perdido")


#T.TEST
summary(quest1)

tt1<- t.test(quest1$`Peso Homens`, quest1$`Peso Mulheres`, alternative = "less", paired=FALSE, conf.level = 0.95)
tt2<- t.test(quest1$`Peso Homens`, quest1$`Peso Mulheres`, alternative = "less", paired=FALSE, conf.level = 0.95, var.equal = TRUE)

tt1
tt2

#Testes de homocedasticidade para a questão 3
#Levene
leveneTest(quest0$Diff ~ interaction(quest0$Diet, quest0$Age_Group))
#Barlett
bartlett.test(quest0$Diff ~ interaction(quest0$Diet, quest0$Age_Group))
