library(plyr)
library(readxl)

library(tidyverse)
library(survival)
library(emmeans)

Aphid <- read_excel("C:/Users/adriv/Downloads/Data of parasitation and infestation with graphs.xlsx", 
                    sheet = "Data (2)")


#Tile map

Map <-  ggplot(Aphid, aes(x = pl, y = pass,  fill = Presence ))+
  geom_tile(aes(x=as.numeric(pl), y=as.numeric(pass), fill = Presence)) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8))+
  scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))

Map

#Statistical analysis 

#ANOVA2 : can PL and PS explain the "Presence of fungus"
ano <- lm(Presence~as.factor(pl)+as.factor(pass), data = Aphid)
anova(ano)
#emmeans of the anova
Malade <- emmeans(ano, specs = "pl")

#anova: nb of aphid explaining the presence of fungus
NbAphid <- as.numeric(Aphid$`Number of aphids per pertri dish`)
pl <- as.factor(Aphid$pl)
NbAphid <- unlist(NbAphid) 
anova2 <- lm(NbAphid~pl , data = Aphid)
anova(anova2)


Effectif <-emmeans(anova2, specs = "pl")
Malade <- as.data.frame(Malade)[,2]
Effectif <-as.data.frame(Effectif)[,2]

#Anova: to see if sample taken from the same position ie. repetitions impact the presence of fungus

Combin <- paste0(Aphid$pl, Aphid$pass)
lm(Aphid$Presence ~ Combin)
anova(lm(Aphid$Presence ~ Combin))#taux de presence du champignon est corrélé à la répétition R² pas tres significatif
summary(lm(Aphid$Presence ~ Combin))

#Anova: to see if sample taken from the same position ie. repetitions impact the number of aphids sampled

Combin <- paste0(Aphid$pl, Aphid$pass)
lm(Aphid$`Number of aphids per pertri dish` ~ Combin)
anova(lm(Aphid$`Number of aphids per pertri dish` ~ Combin))#taux de presence du champignon est corrélé à la répétition R² pas tres significatif
summary(lm(Aphid$`Number of aphids per pertri dish` ~ Combin)) #


#find a link between the presence of fungus and the number of aphids per sample 
plot(Aphid$"Number of aphids per pertri dish", Aphid$Presence)
max(Aphid$Presence, na.rm = T)
lm0 <- lm(Aphid$Presence ~ log10(NbAphid) + pl )#using log transformation to have more accurate model

# anova of presence of fungus explained by the number of aphids per sample corrected for the sampling effect
anova(lm0)
emmeans(lm0, specs = "pl")
#find the coefficient linking the relationship
correlation <- cor(Aphid$"Number of aphids per pertri dish", Aphid$Presence, use = "pairwise.complete.obs")
#can a logistic model explain that relationship 
logistic <- glm(Aphid$Presence ~ Aphid$`Number of aphids per pertri dish`, data = Aphid, family = "poisson")
summary(logistic)


#Survival analysis
path <- "C:/Users/adriv/Desktop/JRL 2022/DataExperimentAphid.xlsx"
s.names <- excel_sheets(path)
cinetic <- data.frame()
for(i in 1:length(s.names)) {
  d0 <- read_excel(path, sheet = i, range = cell_cols("A:F"))
  cinetic <- rbind(d0, cinetic)
}

ToD <- na.omit(cinetic) 
SoP <- read_excel(path, sheet = 7, range = cell_cols("A:F"))

SoP$`Dead or alive` <- as.numeric(SoP$`Dead or alive`)
msurv = Surv(SoP$Timeofdeath, SoP$`Dead or alive`)
mfit = survfit(msurv ~SoP$Treatment)
plot(mfit, col = c("red", "green", "blue"), ylab = ("Probability of an aphid being"),xlab = ("Days"))
legend("topright", c("Control","F1","F2"), lty=1, col=c("red", "green", "blue"))

survdiff(msurv ~ SoP$Treatment)
