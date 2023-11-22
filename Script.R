# Test sur les depenses energetiques 

# Depenses energetiques activitE resting :
#  H0 : Pas de difference significative entre la periode sans mais et avec mais
#  H1 : Difference significative

library(readr)
library(car)

DepE <- read_delim("./data/DepE.csv", delim = ";", 
                   escape_double = FALSE, trim_ws = TRUE)
SMER=c(DepE$Er[1:12])
MER=c(DepE$Er[13:20])

# Créer le graphique Q-Q
qqnorm(SMER, main="Graphique Q-Q", xlab="Quantiles théoriques", ylab="Quantiles observés")

# Ajoute la ligne de référence
qqline(SMER, col="red")

# Créer le graphique Q-Q
qqnorm(MER, main="Graphique Q-Q", xlab="Quantiles théoriques", ylab="Quantiles observés")

# Ajoute la ligne de référence
qqline(MER, col="red")

# Distribution normale ?
shapiro.test(SMER)

shapiro.test(MER)

# p-value > 0.05 --> distribution normale des donnees

# Egalité des variances
DepER = c(SMER,MER)
group = factor(c(rep(1, length(SMER)), rep(2, length(MER))))
leveneTest(DepER, group, center=median)
# --> egalité des variances

#Test de student, comparaison des moyennes

t.test(SMER,MER, var.equal = TRUE)

# p-value > 0.05
# Pas de rejet de H0 --< Pas de difference significative de depenses energetiques resting entre la periode avec mais et la periode sans mais

# ----------------------------------------------------------------------------------------

# Depenses energetiques activite eating :
#  H0 : Pas de difference significative entre la periode sans mais et avec mais
#  H1 : Difference significative

SMEE=c(DepE$Ee[1:12])
MEE=c(DepE$Ee[13:20])
hist(SMEE)
hist(MEE)

# Tests de shapiro (distribution normale ?)
shapiro.test(SMEE)

shapiro.test(MEE)

qqnorm(SMEE, main="Graphique Q-Q", xlab="Quantiles théoriques", ylab="Quantiles observés")

# Ajouter la ligne de référence
qqline(SMEE, col="red")

qqnorm(MEE, main="Graphique Q-Q", xlab="Quantiles théoriques", ylab="Quantiles observés")

# Ajouter la ligne de référence, presence d'un point extreme aberrant
qqline(MEE, col="red")

#p-value > 0.05 --> distributions normales des donnees
# Egalité des variances
DepEE = c(SMEE,MEE)
group = factor(c(rep(1, length(SMEE)), rep(2, length(MEE))))
leveneTest(DepEE, group, center=median)

t.test(SMEE,MEE, var.equal = TRUE)

# p-value > 0.05
# Pas de rejet de H0

# --------------------------------------------------------------------------------------------
  
# Depenses energetique activite Social
  #  H0 : Pas de difference significative entre la periode sans mais et avec mais
  #  H1 : Difference significative
  
SMES=c(DepE$Es[1:12])
MES=c(DepE$Es[13:20])

# Tests de shapiro
shapiro.test(SMES)
qqnorm(SMES, main="Graphique Q-Q", xlab="Quantiles théoriques", ylab="Quantiles observés")

# Ajouter la ligne de référence
qqline(SMES, col="red")
  
shapiro.test(MES)

qqnorm(MES, main="Graphique Q-Q", xlab="Quantiles théoriques", ylab="Quantiles observés")

# Ajouter la ligne de référence
qqline(MES, col="red")
  # une p-value < 0.05, --> Test de wilcoxon

wilcox.test(SMES,MES)

  
  #p-value > 0.05 --> pas de rejet de H0
  
  # --------------------------------------------------------------------------------------------
  
  # Depenses energetique activite Travelling
  #  H0 : Pas de difference significative entre la periode sans mais et avec mais
  #  H1 : Difference significative
  
SMET=c(DepE$Etwo[1:12])
MET=c(DepE$Etwo[13:20])
shapiro.test(SMET)
  
  # p-value < 0.05 --> les donnees ne sont pas distribuees selon une loi normale --> test non-parametrique 
  # de Wilcoxon
  
  hist(SMET)
  hist(MET)
  wilcox.test(SMET,MET)
  # p-value > 0.05, on ne rejette pas H0
  
  
  # --------------------------------------------------------------------------------------------
  
  # Depenses energetique activite Climbing
  #  H0 : Pas de difference significative entre la periode sans mais et avec mais
  #  H1 : Difference significative
  
SMEC=c(DepE$Ecwo[1:12])
MEC=c(DepE$Ecwo[13:20])
hist(SMEC)
hist(MEC)
shapiro.test(SMEC)
wilcox.test(SMEC,MEC)
  
# p-value > 0.05 --> on ne rejette pas H0
  
  
  # Depenses energetique totales
  #  H0 : Pas de difference significative entre la periode sans mais et avec mais
  #  H1 : Difference significative
  
SMET=c(DepE$TDEEwo[1:12])
MET=c(DepE$TDEEwo[13:20])

# Tests de shapiro
shapiro.test(SMET)

shapiro.test(MET)
  
  
#p-value > 0.05 --> distributions normales des donnees
qqnorm(SMET, main="Graphique Q-Q", xlab="Quantiles théoriques", ylab="Quantiles observés")

# Ajouter la ligne de référence
qqline(SMET, col="red")

qqnorm(MET, main="Graphique Q-Q", xlab="Quantiles théoriques", ylab="Quantiles observés")

# Ajouter la ligne de référence
qqline(MET, col="red")
# Egalité des variances
DepET = c(SMET,MET)
group = factor(c(rep(1, length(SMET)), rep(2, length(MET))))
leveneTest(DepET, group, center=median)

t.test(SMET,MET, var.equal = TRUE)
  
  # -----------------------------------------------------------------------------------------
SMDT=c(DepE$D.T[1:12])
MDT=c(DepE$D.T[13:20])
# Ho : Pas de difference significative entre le nombre de metres parcourus en periode d'absence de mais
# et en periode de presence de cultures de mais
# Test de shapiro
shapiro.test(SMDT)


shapiro.test(MDT)


#p-value > 0.05 --> distribution normale des donnees --> test de student
wilcox.test(SMDT,MDT)

# p-value > 0.05, non rejet de H0
  # -----------------------------------------------------------------------------------------
# Temps passe au sol

SMTT=c(DepE$T.Time[1:12])
MTT=c(DepE$T.Time[13:20])
shapiro.test(SMTT)

shapiro.test(MTT)

#p-value > 0.05 --> distributions normales des donnees
qqnorm(SMTT, main="Graphique Q-Q", xlab="Quantiles théoriques", ylab="Quantiles observés")

# Ajouter la ligne de référence
qqline(SMTT, col="red")

qqnorm(MTT, main="Graphique Q-Q", xlab="Quantiles théoriques", ylab="Quantiles observés")

# Ajouter la ligne de référence
qqline(MTT, col="red")

DepTT= c(SMTT,MTT)
group = factor(c(rep(1, length(SMTT)), rep(2, length(MTT))))
leveneTest(DepTT, group, center=median)

# p-value > 0.05 --> Distribution normale
t.test(SMTT,MTT, var.equal = TRUE)

# Pas de rejet de H0


# FAI (disponibilite alimentaire), chi2 de conformite

RF <- read_csv("./data/FAI.csv", col_names = TRUE)
RF = RF[,-1]
colnames(RF) = c("Octobre", "Novembre", "Decembre", "Janvier")
RF = as.matrix(RF)
RFO=c(RF[,"Octobre"])
RFN=c(RF[,"Novembre"])
RFD=c(RF[,"Decembre"])
RFJ=c(RF[,"Janvier"])
RFDJ=rbind(RFD,RFJ)
RFDJ=t(RFDJ)
RFON=rbind(RFO,RFN)
RFON=t(RFON)
RFSM=rowSums(RFON)
RFAM=rowSums(RFDJ)
RFT=rbind(RFSM,RFAM)
RFT=t(RFT)
chisq.test(RFT)
 
# Pearson's Chi-squared test

 #data:  RFT
#X-squared = 148, df = 8, p-value < 2.2e-16

print(chisq.test(RFT)$expected)

# Critere de cochran non respecte --> Test exact de Fisher
fisher.test(RFON,workspace = 200000,hybrid = T)

# Fisher's Exact Test for Count Data hybrid using asym.chisq. iff (exp=5, perc=80, Emin=1)

#data:  RFON
#p-value = 0.0001841
#alternative hypothesis: two.sided

# p-value < 0.05, difference significative entre 
# la disponibilite alimentaire en periode sans culture de mais et avec cultures de mais  

