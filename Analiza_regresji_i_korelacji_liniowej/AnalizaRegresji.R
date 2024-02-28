library("tidyverse")
library("ggplot2")
library("scatterplot3d")

#Zad1
tannin <- read.delim("Tannins.txt", header = TRUE)
summary(tannin)
#2.1
ggplot(data = tannin, mapping = aes(x = TC,
                                    y = LM)) +
  geom_point() + theme_classic() + 
  theme(text = element_text(size = 14)) +
  labs(x = "Masa gasienic [mg]", y = "Procenowa zawartotanin [%]")

#2.2
#funkcja lm() tworzy model regresji (Y~X)

tannin_lm <- lm(formula = LM~TC, data = tannin)
tannin_lm

#Wartosci wspoczynnika regrasji a i b 
#-1.217 - a
#-1.217  - b (nachylenie)

#Dla wartosci resztowych mozliwe jest wykonanie testu poniewaz ich rozklad jest normalny 
shapiro.test(tannin_lm$residuals)

hist(residuals(tannin_lm))

plot(tannin_lm, 1)
#Punkty sa symetrycznie rozmieszczone. Rozrzut wartosci (wariancja) jest rownomierny 

plot(tannin_lm, 2)
#Punkty sa rozrzucone prawie idealnie dla normalnego rozkladu 

plot(tannin_lm, 3)

plot(tannin_lm, 4)
#Ocena jak bardzo konkretne wartosci wplywaja na wynik ostateczny 

summary(tannin_lm)

#2.5
ggplot(data = tannin, mapping = aes(x = TC,
                                    y = LM)) +
  geom_point() + theme_classic() + stat_smooth(method = lm, se=TRUE) + 
  theme(text = element_text(size = 14)) +
  labs(x = "Masa gasienic [mg]", y = "Procenowa zawartosc tanin [%]")



#Zad2-----------------------------------------------------------------------------------
RMR_Rodents <- read.delim(file = "RMR_Rodents.txt", header = TRUE)
summary(RMR_Rodents)

ggplot(data = RMR_Rodents, mapping = aes(x = M,
                                      y = RMR)) +
  geom_point() + theme_classic() + 
  theme(text = element_text(size = 14)) +
  labs(x = "Masa ciala (g)", y = "Spoczynkowe tempo metabolizmu (mlO2/h) ")



RMR_Rodents_log <- RMR_Rodents
RMR_Rodents_log$RMR <- log(RMR_Rodents_log$RMR, 10)
RMR_Rodents_log$M <- log(RMR_Rodents_log$M, 10)
summary(RMR_Rodents_log)

ggplot(data = RMR_Rodents_log, mapping = aes(x = M,
                                         y = RMR)) +
  geom_point() + theme_classic() + 
  theme(text = element_text(size = 14)) +
  labs(x = "Masa ciala (g)", y = "Spoczynkowe tempo metabolizmu (mlO2/h)")

#2.2
RMR_Rodents_log_lm <- lm(formula = RMR~M, data = RMR_Rodents_log)
RMR_Rodents_log_lm

#2.3
plot(RMR_Rodents_log_lm, 1)
plot(RMR_Rodents_log_lm, 2)
plot(RMR_Rodents_log_lm, 3)
plot(RMR_Rodents_log_lm, 4)

#2.4
summary(RMR_Rodents_log_lm)

#2.5
RMR_of2na3 <- lm(formula = RMR~M+offset((2/3)*M), data = RMR_Rodents_log)
summary(RMR_of2na3)

confint(RMR_of2na3)

RMR_of3na4 <- lm(formula = RMR~M+offset((3/4)*M), data = RMR_Rodents_log)
summary(RMR_of3na4)

confint(RMR_of3na4)

#2.6

ggplot(data = RMR_Rodents_log_lm, mapping = aes(x = M,
                                    y = RMR)) +
  geom_point() + theme_classic() + stat_smooth(method = lm, se=TRUE) + 
  theme(text = element_text(size = 14)) +
  labs(x = "Masa cia�a [mg]", y = "Spoczynkowe tempo metabolizmu [mlO2/h]")

#zad 3---------------------------------------------------------------------


Monkeys <- read.delim(file = "Monkeys.txt", header = TRUE)
summary(Monkeys)



ggplot(data = Monkeys, mapping = aes(x = DomRank,
                                    y = Eggs)) +
  geom_point() + theme_classic() + 
  theme(text = element_text(size = 14)) +
  labs(x = "Pozycja w rankingu dominacji", y = "Liczba jaj nicienia na 1g odchod�w danego osobnika")

MonkeysLog <- Monkeys
MonkeysLog$Eggs <- log(MonkeysLog$Eggs, 10)
MonkeysLog$DomRank <- log(MonkeysLog$DomRank,10)
summary(MonkeysLog)


ggplot(data = MonkeysLog, mapping = aes(x = DomRank,
                                     y = Eggs)) +
  geom_point() + theme_classic() + 
  theme(text = element_text(size = 14)) +
  labs(x = "Pozycja w rankingu dominacji", y = "Liczba jaj nicienia na 1g odchod�w danego osobnika")


#2.3

Monkeys_output <- cor.test(MonkeysLog$Eggs, MonkeysLog$DomRank, method = "spearman")


#2.5
ggplot(data = MonkeysLog, mapping = aes(x = DomRank,
                                       y = Eggs)) +
  geom_point() + theme_classic() + stat_smooth(method = lm, se=TRUE) + 
  theme(text = element_text(size = 14)) +
  labs(x = "Pozycja w rankingu dominacji", y = " Liczba jaj nicienia na 1g odchod�w danego osobnika")

#Zad4 -----------------------------------------------------------------------

Deer <- read.delim(file = "Deer.txt", header = TRUE)
summary(Deer)

ggplot(data = Deer, mapping = aes(x = BM,
                                    y = AM)) +
  geom_point() + theme_classic() + 
  theme(text = element_text(size = 14)) +
  labs(x = "Masa ciala [kg]", y = "Masa poroza [kg]")

ggplot(data = Deer, mapping = aes(x = Age,
                                  y = BM)) +
  geom_point() + theme_classic() + 
  theme(text = element_text(size = 14)) +
  labs(x = "Masa ciala [kg]", y = "Masa poroza [kg]")

scatterplot3d(x = Deer$BM, y = Deer$Age, z = Deer$AM, , xlab = "Masa ciala [kg]", ylab = "", zlab = "Masa poroza [kg]", pch=16)
dims <- par("usr")
x <- dims[1]+ 0.9*diff(dims[1:2])
y <- dims[3]+ 0.08*diff(dims[3:4])
text(x,y,expression("Wiek [lata]"),srt=35)


#2

Deer_AM_BM <- lm(formula = AM~BM, data = Deer)
Deer_AM_BM

Deer_AM_Age <- lm(formula = AM~Age, data = Deer)
Deer_AM_Age

Deer_AM_Age_BM <- lm(formula = AM~BM+Age, data=Deer)
Deer_AM_Age_BM

#3
plot(Deer_AM_Age_BM, 1)
plot(Deer_AM_Age_BM, 2)
plot(Deer_AM_Age_BM, 3)
plot(Deer_AM_Age_BM, 4)

#4
summary(Deer_AM_BM)
summary(Deer_AM_Age)
summary(Deer_AM_Age_BM)

confint(Deer_AM_Age_BM)
#5
scatterplot3d(x = Deer$BM, y = Deer$Age, z = Deer$AM, , xlab = "Masa ciala [kg]", ylab="", zlab = "Masa poroza [kg]", pch=16)$plane3d(Deer_AM_Age_BM, draw_polygon=TRUE, draw_lines = FALSE)
dims <- par("usr")
x <- dims[1]+ 0.9*diff(dims[1:2])
y <- dims[3]+ 0.08*diff(dims[3:4])
text(x,y,expression("Wiek [lata]"),srt=35)



