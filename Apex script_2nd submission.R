library(ggplot2)
library(dplyr)
library(cowplot)
library(ggpubr)
library(lme4)
library(glmm)
library(lmerTest)
library(MuMIn)
library(bestNormalize)
library(MASS)
library(moments)
library(jtools)
library(devtools)
library(ggcorrplot)
library(sjPlot)
library(car)
library(jtools)
library(coefplot)

#############################################
#  OVERALL MARRIAGE PATTERNS (no specific age cohorts)
#############################################

#-------------------------------------------
# marriage rate
#-------------------------------------------

df1<- read.csv("overall_marriage_rate.csv",header = TRUE, sep = ",", dec = ".",na.strings = "NA")
df1.1 <- df1[c(1:18)]
df1.1$country_code <- as.factor(df1.1$country_code)
df1.1$religion <- as.factor(df1.1$Main.religion)
df1.1$year <- as.numeric(df1.1$year)
df1.1$Continent <- as.factor(df1.1$Continent)
df1.1_na <- na.omit(df1.1)
str(df1.1_na)

# 2.1.2 Data visualisation
hist(df1.1_na$men.marriage.rate,xlab ="marriage rate in men", main = NULL)
hist(df1.1_na$women.marriage.rate, xlab ="marriage rate in women", main = NULL)
hist(df1.1_na$ASR,xlab ="Adult sex ratio", main = NULL)

# data transform box cox 
bestNormalize::boxcox(df1.1_na$men.marriage.rate)
# lambda = 1.999958 

bestNormalize::boxcox(df1.1_na$women.marriage.rate)
# lambda = 1.999958 

bestNormalize::boxcox(df1.1_na$ASR)
# lambda = -0.9999576 

df1.1_na$ASR_bc <- (((df1.1_na$ASR)^-0.9999576)-1)/-0.9999576 
hist(df1.1_na$ASR_bc)

df1.1_na$MRM_bc <- (((df1.1_na$men.marriage.rate)^1.999958)-1)/1.999958 
hist(df1.1_na$MRM_bc)

df1.1_na$MRF_bc <- (((df1.1_na$women.marriage.rate)^1.999958 )-1)/1.999958 
hist(df1.1_na$MRF_bc)

## Female marriage rate-----------------------------------------------------

# Models
tab_model(
  
  # univariable model
  lmer(MRF_bc ~ ASR_bc  + scale(year) + (1|Continent) + (1|country_code),
       REML = TRUE, data = df1.1_na, na.action = na.exclude),
  
  # multivariable model
  lmer(MRF_bc ~ ASR_bc + scale(year) + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
          (1|Continent) + (1|country_code),
       REML = TRUE, data = df1.1_na, na.action = na.exclude),
  
  show.aic = T,
  collapse.ci = T,
  file = "MRF.doc")


## Male marriage rate -----------------------------------------------------

# Models
tab_model(
  
  # univariable model
  lmer(MRM_bc ~ ASR_bc  +  scale(year) + (1|Continent) + (1|country_code),
       REML = TRUE, data = df1.1_na, na.action = na.exclude),
  
  # multivariable model
  lmer(MRM_bc ~ ASR_bc + scale(year) + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
          (1|Continent) + (1|country_code),
       REML = TRUE, data = df1.1_na, na.action = na.exclude), 
  
  show.aic = T,
  collapse.ci = T,
  file = "MRM.doc")


#------------------------------------------
# divorce rate
#-------------------------------------------

df2<- read.csv("overall_divorce_rate.csv",header = TRUE, sep = ",", dec = ".",na.strings = "NA")
df2.1 <- df2[c(1:18)]
df2.1$country_code <- as.factor(df2.1$country_code)
df2.1$religion <- as.factor(df2.1$Main.religion)
df2.1$year <- as.numeric(df2.1$year)
df2.1$Continent <- as.factor(df2.1$Continent)
df2.1_na <- na.omit(df2.1)

# Data visualisation
hist(df2.1_na$men.divorce.rate, xlab="divorce rate in men", main = NULL)
hist(log(df2.1_na$men.divorce.rate))
hist(df2.1_na$women.divorce.rate, xlab="divorce rate in women", main = NULL)
hist(log(df2.1_na$women.divorce.rate))
hist(df2.1_na$ASR)
hist(log(df2.1_na$ASR))

# data transform box cox 
bestNormalize::boxcox(df2.1_na$men.divorce.rate)
# lambda = 0.1789885

bestNormalize::boxcox(df2.1_na$women.divorce.rate)
# lambda = 0.321432

bestNormalize::boxcox(df2.1_na$ASR)
# lambda = -0.9999576 

df2.1_na$ASR_bc <- (((df2.1_na$ASR)^-0.9999576)-1)/-0.9999576 
hist(df2.1_na$ASR_bc)

df2.1_na$DRM_bc <- (((df2.1_na$men.divorce.rate)^0.1789885)-1)/0.1789885
hist(df2.1_na$DRM_bc)

df2.1_na$DRF_bc <- (((df2.1_na$women.divorce.rate)^0.321432)-1)/0.321432
hist(df2.1_na$DRF_bc)


## female divorce rate -----------------------------------------------------

# Models
tab_model(
  
  # univariable model
  lmer(DRF_bc ~ ASR_bc + scale(year) + (1|Continent) + (1|country_code),
       REML = TRUE, data = df2.1_na, na.action = na.exclude),
  
  # multivariable model
  lmer(DRF_bc ~ ASR_bc + scale(year) + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
         (1|Continent) + (1|country_code),
        REML = TRUE, data = df2.1_na, na.action = na.exclude), 
  
  show.aic = T,
  collapse.ci = T,
  file = "DRF.doc")


## Male divorce rate  -----------------------------------------------------

# Models
tab_model(
  
  # univariable model
  lmer(DRM_bc ~ ASR_bc + scale(year) + (1|Continent) + (1|country_code),
       REML = TRUE, data = df2.1_na, na.action = na.exclude),
  
  # multivariable model
  lmer(DRM_bc ~ ASR_bc + scale(year) + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity")+
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df2.1_na, na.action = na.exclude), 
  
  show.aic = T,
  collapse.ci = T,
  file = "DRM.doc")


#############################################
#  15-49 AGE COHORT MARRIAGE PATTERNS
#############################################

#-------------------------------------------
# marriage rate
#-------------------------------------------

df3.1<- read.csv("MR(15-49)_full.csv",header = TRUE, sep = ",", dec = ".",na.strings = "NA")
df3.2 <- df3.1
df3.2$country_code <- as.factor(df3.1$country_code)
df3.2$religion <- as.factor(df3.1$Main.religion)
df3.2$year <- as.numeric(df3.1$year)
df3.2$Continent <- as.factor(df3.1$Continent)
df3.2_na <- na.omit(df3.2)

# Data visualisation
hist(df3.2_na$MR.men.15.49,xlab ="marriage rate in males (15-49 age cohort)", main = NULL)
hist(df3.2_na$MR.women.15.49, xlab ="marriage rate in females (15-49 age cohort)", main = NULL)
hist(df3.2_na$ASR.15.49,xlab ="Adult sex ratio (15-49 age cohort)", main = NULL)


# data transform box cox 
bc1 <- boxcox(df3.2_na$ASR.15.49)
# lambda = -0.9999576 

bc2 <- boxcox(df3.2_na$MR.men.15.49)
# lambda = 1.999941

bc3 <- boxcox(df3.2_na$MR.women.15.49)
# lambda = 1.524434

df3.2_na$ASR_bc <- (((df3.2_na$ASR.15.49)^-0.9999576)-1)/-0.9999576 
hist(df3.2_na$ASR_bc)

df3.2_na$MRM_bc <- (((df3.2_na$MR.men.15.49)^1.999941)-1)/1.999941
hist(df3.2_na$MRM_bc)

df3.2_na$MRF_bc <- (((df3.2_na$MR.women.15.49)^1.524434)-1)/1.524434
hist(df3.2_na$MRF_bc)


## Female marriage rate 15-49-----------------------------------------------------


# Models
tab_model(
  
  # univariable model
  lmer(MRF_bc ~ ASR_bc + scale(year) + (1|Continent) +(1|country_code),
       REML = TRUE, data = df3.2_na, na.action = na.exclude),
  
  # multivariable model
  lmer(MRF_bc ~ ASR_bc + scale(year) + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full +  relevel(religion, ref="Christianity") +
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df3.2_na, na.action = na.exclude),
  
  show.aic = T,
  collapse.ci = T,
  file = "cMRF.doc")

## Male marriage rate -----------------------------------------------------

# Models
tab_model(
  
  # univariable model
  lmer(MRM_bc ~ ASR_bc + scale(year) + (1|Continent) +(1|country_code),
       REML = TRUE, data = df3.2_na, na.action = na.exclude),
  
  # multivariable model
  lmer(MRM_bc ~ ASR_bc + scale(year) + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
          (1|Continent) + (1|country_code),
       REML = TRUE, data = df3.2_na, na.action = na.exclude), 
  
  show.aic = T,
  collapse.ci = T,
  file = "cMRM.doc")


#-------------------------------------------
# divorce rate
#-------------------------------------------

df3.3<- read.csv("DR(15-49)_full.csv",header = TRUE, sep = ",", dec = ".",na.strings = "NA")
df3.4 <- df3.3
df3.4$country_code <- as.factor(df3.4$country_code)
df3.4$religion <- as.factor(df3.4$Main.religion)
df3.4$year <- as.numeric(df3.4$year)
df3.4$Continent <- as.factor(df3.4$Continent)
df3.4_na <- na.omit(df3.4)

# Data visualisation
hist(df3.4_na$DR.men.15.49, xlab="divorce rate in males (15-49 age cohort)", main = NULL)
hist(df3.4_na$DR.women.15.49, xlab="divorce rate in females (15-49 age cohort)", main = NULL)
hist(df3.4_na$ASR.15.49)


# data transform box cox 
bestNormalize::boxcox(df3.4_na$DR.men.15.49)
# lambda = 0.1778075

bestNormalize::boxcox(df3.4_na$DR.women.15.49)
# lambda = 0.332582

bestNormalize::boxcox(df3.4_na$ASR.15.49)
# lambda = -0.9999576 

df3.4_na$ASR_bc <- (((df3.4_na$ASR)^-0.9999576)-1)/-0.9999576 
hist(df3.4_na$ASR_bc)

df3.4_na$DRM_bc <- (((df3.4_na$DR.men.15.49)^0.1778075)-1)/0.1778075
hist(df3.4_na$DRM_bc)

df3.4_na$DRF_bc <- (((df3.4_na$DR.women.15.49)^0.332582)-1)/0.332582
hist(df3.4_na$DRF_bc)

## Female divorce rate -----------------------------------------------------

# Models
tab_model(
  
  # univariable model
  lmer(DRF_bc ~ ASR_bc + scale(year) + (1|Continent) + (1|country_code),
       REML = TRUE, data = df3.4_na, na.action = na.exclude),
  
  # multivariable model
  lmer(DRF_bc ~ ASR_bc + scale(year) + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full +  relevel(religion, ref="Christianity") +
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df3.4_na, na.action = na.exclude),
  
  show.aic = T,
  collapse.ci = T,
  file = "cDRF.doc")

## Male divorce rate -----------------------------------------------------

# Models
tab_model(
  
  # univariable model
  lmer(DRM_bc ~ ASR_bc + scale(year) + (1|Continent) + (1|country_code),
       REML = TRUE, data = df3.4_na, na.action = na.exclude),
  
  # multivariable model
  lmer(DRM_bc ~ ASR_bc + scale(year) + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
          (1|Continent) + (1|country_code),
         REML = TRUE, data = df3.4_na, na.action = na.exclude),
  
  show.aic = T,
  collapse.ci = T,
  file = "cDRM.doc")


ASR_temp_4<- lm(df3.4_na$ASR.15.49 ~ df3.4_na$year)
summary(ASR_temp_4)

#############################################
# FAMILY STRUCTURES
#############################################

df4<- read.csv("family_structure_full.csv",header = TRUE, sep = ",", dec = ".",na.strings = "NA")
df4$relative_parental_care_1 <- df4$relative_parental_care*(-1)
df4.1 <- df4
df4.1$country_code <- as.factor(df4.1$country_code)
df4.1$religion <- as.factor(df4.1$Main.religion)
df4.1$Continent <- as.factor(df4.1$Continent)
df4.1$year <- as.numeric(df4.1$year)
df4.1_na <- na.omit(df4.1)

# data visulisation
hist(df4.1_na$ASR.15.49)
hist(df4.1_na$Single.mother.with.children)
hist(df4.1_na$Single.father.with.children)


bc1 <- boxcox(df4.1_na$ASR.15.49)
bc1
#lambda = -0.9999576 

df4.1_na$ASR_bc_1 <- (((df4.1_na$ASR.15.49)^-0.9999576)-1)/-0.9999576 
hist(df4.1_na$ASR_bc_1)


# share of single-mother family models
tab_model(
  
  # univariable model
  lmer(Single.mother.with.children ~ ASR_bc_1 + scale(year) +(1|Continent) + (1|country_code),
       REML = TRUE, data = df4.1_na, na.action = na.exclude),

  # multivariable model
  lmer(Single.mother.with.children  ~ ASR_bc_1 + scale(year) + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
          (1|Continent) + (1|country_code),
       REML = TRUE, data = df4.1_na, na.action = na.exclude,control = lmerControl(optimizer ="Nelder_Mead")),
  
  show.aic = T,
  collapse.ci = T,
  file = "SMF.doc")

# share of single-father family models
tab_model(
  
  # univariable model
  lmer(Single.father.with.children ~ ASR_bc_1 + scale(year) + (1|Continent) + (1|country_code),
       REML = TRUE, data = df4.1_na, na.action = na.exclude),
  
  # multivariable model
  lmer(Single.father.with.children  ~ ASR_bc_1 + scale(year) + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df4.1_na, na.action = na.exclude),
  
  show.aic = T,
  collapse.ci = T,
  file = "SFF.doc")



#############################################
# REPRODUCTIVE TIMING
#############################################

df5<- read.csv("Mean age of women at childbirth_full.csv",header = TRUE, sep = ",", dec = ".",na.strings = "NA")
df5.1 <- df5[c(1:16,19,21:22)]
df5.1$country_code <- as.factor(df5.1$country_code)
df5.1$religion <- as.factor(df5.1$Main.religion)
df5.1$Year <- as.numeric(df5.1$Year)
df5.1$Continent<- as.factor(df5.1$Continent)
df5.1_na <- na.omit(df5.1)


# histogram
hist(df5.1_na$age_of_women_at_childbirth, xlab = "average age of women at chilbirth", main = NULL)
hist(df5.1_na$age_of_women_at_first_childbirth, xlab = "average age of women at first chilbirth", main = NULL)

hist(df5.1_na$ASR.15.49.)


# Models
tab_model(
  
  # univariaBLE model
  lmer(scale(age_of_women_at_first_childbirth) ~ scale(ASR.15.49.) + scale(Year)  + (1|Continent) + (1|country_code),
       REML = TRUE, data = df5.1_na, na.action = na.exclude,control = lmerControl(optimizer ="Nelder_Mead")),
  
  # multivariable model
  lmer(scale(age_of_women_at_first_childbirth) ~ scale(ASR.15.49.) + scale(Year)  + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df5.1_na, na.action = na.exclude),
  
  show.aic = T,
  collapse.ci = T,
  file = "RT.doc")


#############################################
# FIGURES
#############################################


# Overall marriage rate
mod1.1 <- lmer(MRM_bc ~ ASR_bc  +  scale(year) + (1|Continent) + (1|country_code),
               REML = TRUE, data = df1.1_na, na.action = na.exclude)
summary(mod1.1)


mod1.2 <-lmer(MRM_bc ~ ASR_bc + scale(year) + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
                  (1|Continent) + (1|country_code), REML = TRUE, data = df1.1_na, na.action = na.exclude)
summary(mod1.2)


multiplot(mod1.1, mod1.2, intercept=FALSE,
          innerCI=0,
          title = "(a) Marriage rate in males",
          xlab = "Estimates",
          ylab = NULL,
          names = c("Minimal model","Full model"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text = element_text(size=10,face="bold"),axis.text.y = element_text(size=10,colour="black",face="bold"),axis.text.x = element_text(size=10,colour="black",face="bold"),
        axis.line = element_line(colour = "black",size = 0.7))+ xlim(-0.2,.2) + 
  scale_y_discrete(labels=c("ASR","Year","GDP per capita","Mean years of schooling","Female to male unemployment rate","Median age","Buddhism",
                            "Hinduism","Islam","Judaism","Unaffiliated religions"))+
  theme(legend.position = "bottom")+
  annotate(geom="text", x=c(-5.919e-02, -0.040840,-3.106e-04,-0.002185,1.452e-04,-1.492e-03,-3.073e-03,-2.593e-04,1.159e-02, 
                            5.090e-02, 1.284e-02,1.955e-02, 1.249e-02), 
           y=c(1, 1.5, 2, 2.5, 3.3,4.3,5.3,6.3,7.3,8.3,9.3,10.3,11.3),
           label=c( "-0.059", "-0.041", "0.000", "-0.002", "0.000","-0.001", "-0.003",  "0.000", "0.012", "0.051**", "0.013", "0.020","0.012" ), color="black",size = 4)


mod1.3 <- lmer(MRF_bc ~ ASR_bc  + scale(year) + (1|Continent) + (1|country_code),
               REML = TRUE, data = df1.1_na, na.action = na.exclude)
summary(mod1.3)

mod1.4 <- lmer(MRF_bc ~ ASR_bc + scale(year) + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
                  (1|Continent) + (1|country_code),
               REML = TRUE, data = df1.1_na, na.action = na.exclude)
summary(mod1.4)


multiplot(mod1.3, mod1.4, intercept=FALSE,
          innerCI=0,
          title = "(b) Marriage rate in females",
          xlab = "Estimates",
          ylab = NULL,
          names = c("Minimal model","Full model"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text = element_text(size=10,face="bold"),axis.text.y = element_text(size=10,colour="black",face="bold"),axis.text.x = element_text(size=10,colour="black",face="bold"),
        axis.line = element_line(colour = "black",size = 0.7))+ xlim(-0.3,.3) + 
  scale_y_discrete(labels= c("ASR","Year","GDP per capita","Mean years of schooling","Female to male unemployment rate","Median age","Buddhism",
                             "Hinduism","Islam","Judaism","Unaffiliated religions"))+
  annotate(geom="text", x=c(7.113e-02,  0.134756, 1.270e-03,  -0.003418, 2.477e-03, -3.203e-03, -5.584e-03, -1.293e-03, 2.319e-02, 7.208e-02,  3.738e-02, 2.402e-02, 2.101e-02), y=c(1, 1.5, 2, 2.5, 3.3,4.3,5.3,6.3,7.3,8.3,9.3,10.3,11.3),
           label=c("0.071", "0.135*", "0.001", "-0.003**","0.002", "-0.003*", "-0.006", "-0.001", "0.023", "0.072***", "0.037***", "0.024", "0.021"), color="black",size = 4)+theme(legend.position="bottom")


# Overall divorce rate
mod2.1 <-lmer(DRM_bc ~ ASR_bc + scale(year) + (1|Continent) + (1|country_code),
              REML = TRUE, data = df2.1_na, na.action = na.exclude)
summary(mod2.1)


mod2.2 <-  lmer(DRM_bc ~ ASR_bc + scale(year) + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity")+
                  (1|Continent) + (1|country_code),
                REML = TRUE, data = df2.1_na, na.action = na.exclude)
summary(mod2.2)

multiplot(mod2.1, mod2.2, intercept=FALSE,
          innerCI=0,
          title = "(c) Divorce rate in males",
          xlab = "Estimates",
          ylab = NULL,
          names = c("Minimal model","Full model"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text = element_text(size=10,face="bold"),axis.text.y = element_text(size=10,colour="black",face="bold"),axis.text.x = element_text(size=10,colour="black",face="bold"),
        axis.line = element_line(colour = "black",size = 0.7))+ xlim(-7,7) + 
  scale_y_discrete(labels=c("ASR","Year","GDP per capita","Mean years of schooling","Female to male unemployment rate","Median age","Buddhism",
                            "Hinduism","Islam","Judaism","Unaffiliated religions"))+
  theme(legend.position = "bottom")+ 
  annotate(geom="text", x=c( -1.50291, -3.76172,0.02961, 0.21143, 0.05918, -0.03422, -0.101864, 0.09490, -0.57855,
                             -1.48040, -0.18153, 0.70196, -0.18181), y=c(1, 1.5, 2, 2.5, 3.3,4.3,5.3,6.3,7.3,8.3,9.3,10.3,11.3),
           label=c("-1.503", "-3.762*", "0.030", "0.211***","0.059", "-0.034", "-0.102",  "0.095***", "-0.579", "-1.480**", "-0.182", "0.702", "-0.182"), color="black",size = 4)


mod2.3 <-lmer(DRF_bc ~ ASR_bc + scale(year) + (1|Continent) + (1|country_code),
              REML = TRUE, data = df2.1_na, na.action = na.exclude)
summary(mod2.3)

mod2.4 <- lmer(DRF_bc ~ ASR_bc + scale(year) + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
                  (1|Continent) + (1|country_code),
               REML = TRUE, data = df2.1_na, na.action = na.exclude)
summary(mod2.4)


multiplot(mod2.3, mod2.4, intercept=FALSE,
          innerCI=0,
          title = "(d) Divorce rate in females",
          xlab = "Estimates",
          ylab = NULL,
          names = c("Minimal model","Full model"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text = element_text(size=10,face="bold"),axis.text.y = element_text(size=10,colour="black",face="bold"),axis.text.x = element_text(size=10,colour="black",face="bold"),
        axis.line = element_line(colour = "black",size = 0.7))+ xlim(-9,9) + 
  scale_y_discrete(labels=c("ASR","Year","GDP per capita","Mean years of schooling","Female to male unemployment rate","Median age","Buddhism",
                            "Hinduism","Islam","Judaism","Unaffiliated religions"))+
  theme(legend.position = "bottom")+
  annotate(geom="text", x=c(-3.57968, -4.66764, 0.04280, 0.23159, 0.17552, -0.04294, -0.13105, 0.08629, -0.59450, -1.76634, 0.40352, 1.07543,-0.27036 ), y=c(1, 1.5, 2, 2.5, 3.3,4.3,5.3,6.3,7.3,8.3,9.3,10.3,11.3),
           label=c("-3.580","-4.668*", "0.043", "0.232***", "0.176*", "-0.043", "-0.131", "0.086***", "-0.595", "-1.766**", "0.404", "1.075", "-0.270"), color="black",size = 4)


# Cohort marraige rate

mod3.1 <-  lmer(MRM_bc ~ ASR_bc + scale(year) + (1|Continent) +(1|country_code),
                REML = TRUE, data = df3.2_na, na.action = na.exclude)
summary(mod3.1)


mod3.2 <- lmer(MRM_bc ~ ASR_bc + scale(year) + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
                  (1|Continent) + (1|country_code),
               REML = TRUE, data = df3.2_na, na.action = na.exclude)
summary(mod3.2)


multiplot(mod3.1, mod3.2, intercept=FALSE,
          innerCI=0,
          title = "(a) Cohort-based marriage rate in males",
          xlab = "Estimates",
          ylab = NULL,
          names = c("Minimal model","Full model"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text = element_text(size=10,face="bold"),axis.text.y = element_text(size=10,colour="black",face="bold"),axis.text.x = element_text(size=10,colour="black",face="bold"),
        axis.line = element_line(colour = "black",size = 0.7))+ xlim(-.3,.3) + 
  scale_y_discrete(labels=c("Cohort-based ASR","Year","GDP per capita","Mean years of schooling","Female to male unemployment rate","Median age","Buddhism",
                            "Hinduism","Islam","Judaism","Unaffiliated religions"))+
  theme(legend.position = "bottom")+ 
  annotate(geom="text", x=c(-1.146e-01, -0.113486, -1.483e-03, -0.005411, -1.798e-03, -4.532e-04, -6.140e-03, -1.514e-03,  1.348e-02, 5.464e-02, 6.950e-03, 9.584e-03, 4.202e-03), y=c(1, 1.5, 2, 2.5, 3.3,4.3,5.3,6.3,7.3,8.3,9.3,10.3,11.3),
           label=c("-0.115", "-0.113", "-0.001", "-0.005***",  "-0.002", "0.000", "-0.006", "-0.002*",  "0.013",  "0.055**",  "0.007",  "0.010",  "0.004"), 
           color="black",size = 4)


mod3.3 <- lmer(MRF_bc ~ ASR_bc + scale(year) + (1|Continent) +(1|country_code),
               REML = TRUE, data = df3.2_na, na.action = na.exclude)
summary(mod3.3)

mod3.4 <-lmer(MRF_bc ~ ASR_bc +  scale(year) + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full +  relevel(religion, ref="Christianity") +
                (1|Continent) + (1|country_code),
              REML = TRUE, data = df3.2_na, na.action = na.exclude)
summary(mod3.4)


multiplot(mod3.3, mod3.4, intercept=FALSE,
          innerCI=0,
          title = "(b) Cohort-based marriage rate in females",
          xlab = "Estimates",
          ylab = NULL,
          names = c("Minimal model","Full model"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text = element_text(size=10,face="bold"),axis.text.y = element_text(size=10,colour="black",face="bold"),axis.text.x = element_text(size=10,colour="black",face="bold"),
        axis.line = element_line(colour = "black",size = 0.7))+ xlim(-.3,.3) + 
  scale_y_discrete(labels=c("Cohort-based ASR","Year","GDP per capita","Mean years of schooling","Female to male unemployment rate","Median age","Buddhism",
                            "Hinduism","Islam","Judaism","Unaffiliated religions"))+
  theme(legend.position = "bottom")+
  annotate(geom="text", x=c(8.026e-04, -0.051215, -1.426e-03,-0.010198,  -2.079e-03, -4.491e-03,-1.166e-02, -2.023e-03, 2.937e-02, 1.031e-01, 5.604e-02, 3.788e-02, 1.886e-02), y=c(1, 1.5, 2, 2.5, 3.3,4.3,5.3,6.3,7.3,8.3,9.3,10.3,11.3),
           label=c("0.001", "-0.051", "-0.001","-0.010***",  "-0.002", "-0.004", "-0.012", "-0.002", "0.029", "0.103**", "0.056***", "0.038", "0.019"), color="black",size = 4)



# Cohort divorce rate

mod4.1 <- lmer(DRM_bc ~ ASR_bc + scale(year) + (1|Continent) + (1|country_code),
               REML = TRUE, data = df3.4_na, na.action = na.exclude)
summary(mod4.1)

mod4.2 <-lmer(DRM_bc ~ ASR_bc + scale(year) + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
                (1|Continent) + (1|country_code),
              REML = TRUE, data = df3.4_na, na.action = na.exclude)
summary(mod4.2)

multiplot(mod4.1, mod4.2, intercept=FALSE,
          innerCI=0,
          title = "(c) Cohort-based divorce rate in males",
          xlab = "Estimates",
          ylab = NULL,
          names = c("Minimal model","Full model"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text = element_text(size=10,face="bold"),axis.text.y = element_text(size=10,colour="black",face="bold"),axis.text.x = element_text(size=10,colour="black",face="bold"),
        axis.line = element_line(colour = "black",size = 0.7))+ xlim(-3,3) + 
  scale_y_discrete(labels=c("Cohort-based ASR", "Year", "GDP per capita","Mean years of schooling","Female to male unemployment rate","Median age","Buddhism",
                            "Hinduism","Islam","Judaism","Unaffiliated religions"))+
  theme(legend.position = "bottom")+
  annotate(geom="text", x=c(2.624e-01, 0.53088, -2.647e-02,  0.03751, -2.911e-02, -7.874e-04, -5.215e-02,  3.989e-02, -1.673e-01, -5.282e-01, -9.350e-02, 2.487e-01, 1.845e-02), y=c(1, 1.5, 2, 2.5, 3.3,4.3,5.3,6.3,7.3,8.3,9.3,10.3,11.3),
           label=c("0.262", "0.531", "-0.026 ", "0.038*",  "-0.029", "-0.001", "-0.052",  "0.040***", "-0.167", "-0.528*", "-0.094",  "0.249",  "0.018"), color="black",size = 4)



mod4.3 <- lmer(DRF_bc ~ ASR_bc + scale(year) + (1|Continent) + (1|country_code),
               REML = TRUE, data = df3.4_na, na.action = na.exclude)
summary(mod4.3)

mod4.4 <-lmer(DRF_bc ~ ASR_bc + scale(year) +scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full +  relevel(religion, ref="Christianity") +
                (1|Continent) + (1|country_code),
              REML = TRUE, data = df3.4_na, na.action = na.exclude)
summary(mod4.4)

multiplot(mod4.3, mod4.4, intercept=FALSE,
          innerCI=0,
          title = "(d) Cohort-based divorce rate in females",
          xlab = "Estimates",
          ylab = NULL,
          names = c("Minimal model","Full model"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text = element_text(size=10,face="bold"),axis.text.y = element_text(size=10,colour="black",face="bold"),axis.text.x = element_text(size=10,colour="black",face="bold"),
        axis.line = element_line(colour = "black",size = 0.7))+ xlim(-1.5,1.5) + 
  scale_y_discrete(labels=c("Cohort-based ASR","Year","GDP per capita","Mean years of schooling","Female to male unemployment rate","Median age","Buddhism",
                            "Hinduism","Islam","Judaism","Unaffiliated religions"))+
  theme(legend.position = "bottom")+
  annotate(geom="text", x=c(-0.088523, 0.036484, -0.015286, 0.015345, -0.014555, -0.006039, -0.031343, 0.022109, -0.099703, -0.297322, 0.0359761, 0.243608, 0.001578), y=c(1, 1.5, 2, 2.5, 3.3,4.3,5.3,6.3,7.3,8.3,9.3,10.3,11.3),
           label=c("-0.089",  "0.036", "-0.015",  "0.015", "-0.015", "-0.006", "-0.031",  "0.022***", "-0.100", "-0.297*",  "0.036",  "0.244",  "0.002"), color="black",size = 4)


# Family sturcture
mod5.1 <-lmer(Single.father.with.children ~ ASR_bc_1 + scale(year) + (1|Continent) + (1|country_code),
              REML = TRUE, data = df4.1_na, na.action = na.exclude)
summary(mod5.1)

mod5.2 <-lmer(Single.father.with.children  ~ ASR_bc_1 + scale(year) + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
                (1|Continent) + (1|country_code),REML = TRUE, data = df4.1_na, na.action = na.exclude)
summary(mod5.2)

multiplot(mod5.1, mod5.2, intercept=FALSE,
          innerCI=0,
          title = "(a) Share of single-father families",
          xlab = "Estimates",
          ylab = NULL,
          names = c("Minimal model","Full model"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text = element_text(size=10,face="bold"),axis.text.y = element_text(size=10,colour="black",face="bold"),axis.text.x = element_text(size=10,colour="black",face="bold"),
        axis.line = element_line(colour = "black",size = 0.7))+ xlim(-4,4) + 
  scale_y_discrete(labels=c("Cohort-based ASR","Year","GDP per capita","Mean years of schooling","Female to male unemployment rate","Median age","Buddhism","Folk religion",
                            "Hinduism","Islam","Judaism","Unaffiliated religions"))+
  theme(legend.position = "bottom")+
  annotate(geom="text", x=c(1.549018, 1.02275, 0.045932,  0.04589, -0.026090, 0.021453, -0.120994, -0.009412, -0.009417, -0.464375, -0.452972, -0.684022, -0.614368,  0.239755), y=c(1, 1.5, 2, 2.5, 3.3,4.3,5.3,6.3,7.3,8.3,9.3,10.3,11.3,12.3),
           label=c("1.549",  "1.023",  "0.046*",  "0.046*", "-0.026",  "0.021", "-0.121**", "-0.009", "-0.009", "-0.464", "-0.453", "-0.684***", "-0.614",  "0.240"), color="black",size = 4)


mod5.3 <-lmer(Single.mother.with.children ~ ASR_bc_1 + scale(year) +(1|Continent) + (1|country_code),
              REML = TRUE, data = df4.1_na, na.action = na.exclude)
summary(mod5.3)

mod5.4 <- lmer(Single.mother.with.children  ~ ASR_bc_1 + scale(year) + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
                 (1|Continent) + (1|country_code),REML = TRUE, data = df4.1_na, na.action = na.exclude,
               control = lmerControl(optimizer ="Nelder_Mead"))
summary(mod5.4)

multiplot(mod5.3, mod5.4, intercept=FALSE,
          innerCI=0,
          title = "(b) Share of single-mother families",
          xlab = "Estimates",
          ylab = NULL,
          names = c("Minimal model","Full model"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text = element_text(size=10,face="bold"),axis.text.y = element_text(size=10,colour="black",face="bold"),axis.text.x = element_text(size=10,colour="black",face="bold"),
        axis.line = element_line(colour = "black",size = 0.7))+ xlim(-22,22) + 
  scale_y_discrete(labels=c("Cohort-based ASR","Year","GDP per capita","Mean years of schooling","Female to male unemployment rate","Median age","Buddhism","Folk religion",
                            "Hinduism","Islam","Judaism","Unaffiliated religions"))+
  theme(legend.position = "bottom")+
  annotate(geom="text", x=c( -12.535560, -12.50100 , 0.485316, 0.50039, -0.105232, 0.047411, -0.284425, 0.001094, -1.972351, -0.582814, -1.169625, -2.431897, -1.233825, -3.504921), y=c(1, 1.5, 2, 2.5, 3.3,4.3,5.3,6.3,7.3,8.3,9.3,10.3,11.3,12.3),
           label=c("-12.536**", "-12.501**",  "0.485 ***",  "0.500***",  "-0.105",  "0.047", "-0.284",  "0.001", "-1.972", "-0.583", "-1.170", "-2.432***", "-1.234", "-3.505"), color="black",size = 4)


# Reproductive age

mod6.1 <- lmer(scale(age_of_women_at_first_childbirth) ~ scale(ASR.15.49.) + scale(Year)  + (1|Continent) + (1|country_code),
               REML = TRUE, data = df5.1_na, na.action = na.exclude, control = lmerControl(optimizer ="Nelder_Mead"))
summary(mod6.1)  
isSingular(mod6.1, tol = 1e-4)

mod6.2 <- lmer(scale(age_of_women_at_first_childbirth) ~ scale(ASR.15.49.) + scale(Year)  + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
                 (1|Continent) + (1|country_code),
               REML = TRUE, data = df5.1_na, na.action = na.exclude)
summary(mod6.2)  

multiplot(mod6.1, mod6.2, intercept=FALSE,
          innerCI=0,
          title = "(c) Avearge age of women at first childbirth",
          xlab = "Estimates",
          ylab = NULL,
          names = c("Minimal model","Full model"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text = element_text(size=10,face="bold"),axis.text.y = element_text(size=10,colour="black",face="bold"),axis.text.x = element_text(size=10,colour="black",face="bold"),
        axis.line = element_line(colour = "black",size = 0.7))+  
  scale_y_discrete(labels=c("Cohort-based ASR","Year","GDP per capita","Mean years of schooling","Female to male unemployment rate","Median age",
                            "Judaism","Unaffiliated religions"))+
  theme(legend.position = "bottom")+
  annotate(geom="text", x=c(0.121695, 0.14502, 0.458264, 0.58949,  -0.091933, 0.037962, 0.045501, 0.068074, -0.154915, -0.299329), y=c(1, 1.4, 2, 2.4, 3.3,4.3,5.3,6.3,7.3,8.3),
           label=c("0.122***", "0.145***", "0.458***", "0.589***", "-0.092***",  "0.038*",  "0.046",  "0.068***", "-0.155", "-0.299"), color="black",size = 4)


######################################
# Temporal changes plots
######################################

# female marriage rate temporal change
mod1.3_predicted=predict(mod1.3)

reg_model_1.3 <- lm(mod1.3_predicted ~ df1.1_na$year, data = df1.1_na)
summary(reg_model_1.3)

fig1 <- plot(mod1.3_predicted ~ df1.1_na$year, xlab = "Year", ylab = "Marriage rate in females",
             cex.lab = 1.2, cex.axis = 1.2)
abline(reg_model_1.3, col="steelblue", lwd = 1.5)
title("(a) Temporal change of marriage rate in females", adj = 0, cex.main = 1)

# male divorce rate temporal change
mod2.1_predicted=predict(mod2.1)

reg_model_2.1 <- lm(mod2.1_predicted ~ df2.1_na$year, data = df2.1_na)
summary(reg_model_2.1)

fig1 <- plot(mod2.1_predicted ~ df2.1_na$year, xlab = "Year", ylab = "Divorce rate in males",
             cex.lab = 1.2, cex.axis = 1.2)
abline(reg_model_2.1, col="steelblue", lwd = 1.5)
title("(b) Temporal change of divorce rate in males", adj = 0, cex.main = 1)

# female divorce rate temporal change
mod2.3_predicted=predict(mod2.3)

reg_model_2.3 <- lm(mod2.3_predicted ~ df2.1_na$year, data = df2.1_na)
summary(reg_model_2.3)

fig1 <- plot(mod2.3_predicted ~ df2.1_na$year, xlab = "Year", ylab = "Divorce rate in females",
             cex.lab = 1.2, cex.axis = 1.2)
abline(reg_model_2.3, col="steelblue", lwd = 1.5)
title("(c) Temporal change of divorce rate in females", adj = 0,cex.main = 1)


# 15-49 male marriage rate temporal change

mod3.1_predicted=predict(mod3.1)

reg_model_3.1 <- lm(mod3.1_predicted ~ df3.2_na$year, data = df3.2_na)
summary(reg_model_3.1)

fig1 <- plot(mod3.1_predicted ~ df3.2_na$year, xlab = "Year", ylab = "Cohort-based marriage rate in males",
             cex.lab = 1.2, cex.axis = 1.2)
abline(reg_model_3.1, col="steelblue", lwd = 1.5)
title("(d) Temporal change of cohort-based marriage rate in males", adj = 0, cex.main = 1)


# 15-49 female marriage rate temporal change

mod3.3_predicted=predict(mod3.3)

reg_model_3.3 <- lm(mod3.3_predicted ~ df3.2_na$year, data = df3.2_na)
summary(reg_model_3.3)

fig1 <- plot(mod3.3_predicted ~ df3.2_na$year, xlab = "Year", ylab = "Cohort-based marriage rate in females",
             cex.lab = 1.2, cex.axis = 1.2)
abline(reg_model_3.3, col="steelblue", lwd = 1.5)
title("(e) Temporal change of cohort-based marriage rate in females", adj = 0,cex.main = 1)


# 15-49 male divorce rate temporal change

mod4.1_predicted=predict(mod4.1)

reg_model_4.1 <- lm(mod4.1_predicted ~ df3.4_na$year, data = df3.4_na)
summary(reg_model_4.1)

fig1 <- plot(mod4.1_predicted ~ df3.4_na$year, xlab = "Year", ylab = "Cohort-based divorce rate in males",
             cex.lab = 1.2, cex.axis = 1.2)
abline(reg_model_4.1, col="steelblue", lwd = 1.5)
title("(f) Temporal change of cohort-based divorce rate in males", adj = 0, cex.main = 1)



# family structure temporal change

mod5.1_predicted=predict(mod5.1)

reg_model_5.1 <- lm(mod5.1_predicted ~ df4.1_na$year, data = df4.1_na)
summary(reg_model_5.1)

fig1 <- plot(mod5.1_predicted ~ df4.1_na$year, xlab = "Year", ylab = "Share of single-father families",
             cex.lab = 1.2, cex.axis = 1.2)
abline(reg_model_5.1, col="steelblue", lwd = 1.5)
title("(g) Temporal change of the share of single-father families", adj = 0, cex.main = 1)


mod5.3_predicted=predict(mod5.3)

reg_model_5.3<- lm(mod5.3_predicted ~ df4.1_na$year, data = df4.1_na)
summary(reg_model_5.3)

fig1 <- plot(mod5.3_predicted ~ df4.1_na$year, xlab = "Year", ylab = "Share of single-mother families",
             cex.lab = 1.2, cex.axis = 1.2)
abline(reg_model_5.3, col="steelblue", lwd = 1.5)
title("(h) Temporal change of the share of single-mother families", adj = 0, cex.main = 1)


# reproductive age temporal change

reg_model_6.1 <- lm(df5.1_na$age_of_women_at_first_childbirth ~ df5.1_na$Year, data = df5.1_na)
summary(reg_model_6.1)

fig1 <- plot(df5.1_na$age_of_women_at_first_childbirth ~ df5.1_na$Year, xlab = "Year", ylab = "Age of women at first childbirth",
             cex.lab = 1.2, cex.axis = 1.2)
abline(reg_model_6.1, col="steelblue", lwd = 1.5)
title("(i)Temporal change of age of women at first childbirth", adj = 0, cex.main = 1)


######################################
# ASR plots
######################################

# marriage rate in males
mod1.3_predicted=predict(mod1.3)

predict_mod_1.3<- lm(mod1.3_predicted ~ df1.1_na$ASR_bc, data = df1.1_na)
summary(predict_mod_1.3)

fig1 <- plot(mod1.3_predicted ~ df1.1_na$ASR_bc, xlab = "Adult sex ratio", ylab = "Divorce rate in males",
             cex.lab = 1.2, cex.axis = 1.2)
abline(predict_mod_1.3, col="steelblue", lwd = 1.5)
title("(a) Adult sex ratio in relation to marriage rate in males ", adj = 0,cex.main = 1)

# divorce rate in males
mod2.1_predicted=predict(mod2.1)

predict_mod_2.1<- lm(mod2.1_predicted ~ df2.1_na$ASR_bc, data = df2.1_na)
summary(predict_mod_2.1)

fig1 <- plot(mod2.1_predicted ~ df2.1_na$ASR_bc, xlab = "Adult sex ratio", ylab = "Divorce rate in males",
             cex.lab = 1.2, cex.axis = 1.2)
abline(predict_mod_2.1, col="steelblue", lwd = 1.5)
title("(b) Adult sex ratio in relation to divorce rate in males ", adj = 0,cex.main = 1)


# divorce rate in females
mod2.3_predicted=predict(mod2.3)

predict_mod_2.3<- lm(mod2.3_predicted ~ df2.1_na$ASR_bc, data = df2.1_na)
summary(predict_mod_2.3)

fig1 <- plot(mod2.3_predicted ~ df2.1_na$ASR_bc, xlab = "Adult sex ratio", ylab = "Divorce rate in males",
             cex.lab = 1.2, cex.axis = 1.2)
abline(predict_mod_2.3, col="steelblue", lwd = 1.5)
title("(c) Adult sex ratio in relation to divorce rate in females ", adj = 0,cex.main = 1)


# family structures

mod5.4_predicted=predict(mod5.4)

predict_mod_5.4<- lm(mod5.4_predicted ~ df4.1_na$ASR_bc_1, data = df4.1_na)
summary(predict_mod_5.4)

fig1 <- plot(mod5.4_predicted ~ df4.1_na$ASR_bc_1, xlab = "Cohort-based adult sex ratio", ylab = "Share of single-mother families",
             cex.lab = 1.2, cex.axis = 1.2)
abline(predict_mod_5.4, col="steelblue", lwd = 1.5)
title("(d) Adult sex ratio in relation to share of single-mother families", adj = 0, cex.main = 1)


# reproductive age

reg_model_6.1 <- lm(scale(df5.1_na$age_of_women_at_first_childbirth) ~ scale(df5.1_na$ASR.15.49.), data = df5.1_na)
summary(reg_model_6.1)

fig1 <- plot(scale(df5.1_na$age_of_women_at_first_childbirth) ~ scale(df5.1_na$ASR.15.49.), xlab = "Cohort-based adult sex ratio", ylab = "Age of women at first childbirth",
             cex.lab = 1.2, cex.axis = 1.2)
abline(reg_model_6.1, col="steelblue", lwd = 1.5)
title("(e) Adult sex ratio in relation to the age of women at first childbirth", adj = 0, cex.main = 1)


######################################
# Median age plots
######################################

# divorce in males
predict_mod_2.2<- lm(mod2.1_predicted ~ df2.1_na$median.age_full, data = df2.1_na)
summary(predict_mod_2.2)

fig1 <- plot(mod2.1_predicted ~ df2.1_na$median.age_full, xlab = "Median age", ylab = "Divorce rate in males",
             cex.lab = 1.2, cex.axis = 1.2)
abline(predict_mod_2.2, col="steelblue", lwd = 1.5)
title("(a) Median age in relation to divorce rate in males", adj = 0, cex.main = 1)

#divorce in females
predict_mod_2.4<- lm(mod2.3_predicted ~ df2.1_na$median.age_full, data = df2.1_na)
summary(predict_mod_2.4)

fig1 <- plot(mod2.3_predicted ~ df2.1_na$median.age_full, xlab = "Median age", ylab = "Divorce rate in females",
             cex.lab = 1.2, cex.axis = 1.2)
abline(predict_mod_2.4, col="steelblue", lwd = 1.5)
title("(b) Median age in relation to divorce rate in females", adj = 0, cex.main = 1)


# 15-49 marriage rate in males
mod3.1_predicted=predict(mod3.1)

reg_model_3.2 <- lm(mod3.1_predicted ~ df3.2_na$median.age_full, data = df3.2_na)
summary(reg_model_3.2)

fig1 <- plot(mod3.1_predicted ~ df3.2_na$median.age_full, xlab = "Median age", ylab = "Cohort-based marriage rate in males",
             cex.lab = 1.2, cex.axis = 1.2)
abline(reg_model_3.2, col="steelblue", lwd = 1.5)
title("(c) Median age in relation to cohort-based marriage rate in males", adj = 0, cex.main = 1)


# 15-49 male divorce rate 

mod4.1_predicted=predict(mod4.1)

reg_model_4.2 <- lm(mod4.1_predicted ~ df3.4_na$median.age_full, data = df3.4_na)
summary(reg_model_4.2)

fig1 <- plot(mod4.1_predicted ~ df3.4_na$median.age_full, xlab = "Median age", ylab = "Cohort-based divorce rate in males",
             cex.lab = 1.2, cex.axis = 1.2)
abline(reg_model_4.2, col="steelblue", lwd = 1.5)
title("(d) Median age in relation to cohort-based divorce rate in males", adj = 0, cex.main = 1)

# 15-49 female divorce rate 

mod4.3_predicted=predict(mod4.3)

reg_model_4.4 <- lm(mod4.3_predicted ~ df3.4_na$median.age_full, data = df3.4_na)
summary(reg_model_4.4)

fig1 <- plot(mod4.3_predicted ~ df3.4_na$median.age_full, xlab = "Median age", ylab = "Cohort-based divorce rate in females",
             cex.lab = 1.2, cex.axis = 1.2)
abline(reg_model_4.4, col="steelblue", lwd = 1.5)
title("(e) Median age in relation to cohort-based divorce rate in females", adj = 0,cex.main = 1)

# reproductive age 

reg_model_6.2 <- lm(df5.1_na$age_of_women_at_first_childbirth ~ df5.1_na$median.age_full, data = df5.1_na)
summary(reg_model_6.2)

fig1 <- plot(df5.1_na$age_of_women_at_first_childbirth ~ df5.1_na$median.age_full, xlab = "Median age", ylab = "Age of women at first childbirth",
             cex.lab = 1.2, cex.axis = 1.2)
abline(reg_model_6.2, col="steelblue", lwd = 1.5)
title("(f) Median age in relation to age of women at first childbirth", adj = 0, cex.main = 1)



######################################
# Religion plots
######################################


fig_s1 <- plot(df1.1_na$MRM_bc~df1.1_na$religion, xlab = "Religion", ylab = "Marriage rate in males",
               cex.lab = 1.2, cex.axis = 1.2)
title("(a) Religions in relation to marriage rate in males", adj = 0, cex.main = 1)


fig_s2 <- plot(df1.1_na$MRF_bc~df1.1_na$religion, xlab = "Religion", ylab = "Marriage rate in females",
               cex.lab = 1.2, cex.axis = 1.2)
title("(b) Religions in relation to marriage rate in females", adj = 0, cex.main = 1)

fig_s3 <- plot(df2.1_na$DRM_bc~df2.1_na$religion, xlab = "Religion", ylab = "Divorce rate in males",
               cex.lab = 1.2, cex.axis = 1.2)
title("(c) Religions in relation to divorce rate in males", adj = 0, cex.main = 1)

fig_s4 <- plot(df2.1_na$DRF_bc~df2.1_na$religion, xlab = "Religion", ylab = "Divorce rate in females",
               cex.lab = 1.2, cex.axis = 1.2)
title("(d) Religions in relation to divorce rate in females", adj = 0, cex.main = 1)


fig_s5 <- plot(df3.2_na$MRM_bc~df3.2_na$religion, xlab = "Religion", ylab = "Cohort-based marriage rate in males",
               cex.lab = 1.2, cex.axis = 1.2)
title("(e) Religions in relation to cohort-based marriage rate in males", adj = 0, cex.main = 1)

fig_s6 <- plot(df3.2_na$MRF_bc~df3.2_na$religion, xlab = "Religion", ylab = "Cohort-based marriage rate in females",
               cex.lab = 1.2, cex.axis = 1.2)
title("(f) Religions in relation to cohort-based marriage rate in females", adj = 0, cex.main = 1)

fig_s7 <- plot(df3.4_na$DRM_bc~df3.4_na$religion, xlab = "Religion", ylab = "Cohort-based divorce rate in males",
               cex.lab = 1.2, cex.axis = 1.2)
title("(g) Religions in relation to cohort-based divorce rate in males", adj = 0, cex.main = 1)

fig_s8 <- plot(df3.4_na$DRF_bc~df3.4_na$religion, xlab = "Religion", ylab = "Cohort-based divorce rate in females",
               cex.lab = 1.2, cex.axis = 1.2)
title("(h) Religions in relation to cohort-based divorce rate in females", adj = 0, cex.main = 1)



fig_s9 <- plot(df4.1_na$Single.father.with.children~df4.1_na$religion, xlab = "Religion", ylab = "Share of single-father families",
               cex.lab = 1.2, cex.axis = 1.2)
title("(i) Religions in relation to the share of single-father families", adj = 0, cex.main = 1)

fig_s10 <- plot(df4.1_na$Single.mother.with.children~df4.1_na$religion, xlab = "Religion", ylab = "Share of single-mother families",
               cex.lab = 1.2, cex.axis = 1.2)
title("(j) Religions in relation to the share of single-mother families", adj = 0, cex.main = 1)

fig_s11 <- plot(df5.1_na$age_of_women_at_first_childbirth~df5.1_na$religion, xlab = "Religion", ylab = "Age of women at first childbirth",
               cex.lab = 1.2, cex.axis = 1.2)
title("(k) Religions in relation to age of women at first childbirth", adj = 0, cex.main = 1)




