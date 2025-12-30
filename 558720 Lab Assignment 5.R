############################################################
# Lab Assignment 5 
# Course: Introduction to Econometrics, Spring 2025
# Submission instructions:
# - Submit ONE .R script only.

# Team members:
# 1) LE HONG TRA MY (558720)
############################################################
# Setup ----------------------------------------------------
library(tidyverse)
library(car)
library(modelsummary)   
library(broom)

# Load data ------------------------------------------------
library(wooldridge)
data("loanapp")

############################################################
# Exercise 1
# Variable descriptions
# approve (binary outcome) : =1 if application is approved, 0 otherwise
# white (explainatory) : =1 if white, 0 if black or hispanic
#mortage approval: approve(i) = β0 + β1white(i) + X′(i)γ + u(i)
#where X includes a set of borrower and loan characteristics.

#### (a) ---------------------------------------------------------
#Q:Estimate a Linear Probability Model (LPM) of approve on white only. Report and 
#interpret the coefficient on white. What does it suggest about the difference in approval rates
# between white and nonwhite applicants?

#LPM: approve white only 
#Model: approve = β0 + β1 white + u 
md1 <- lm(approve ~ white, data = loanapp)
summary(md1)
#Interpretation 
#Intercept β0 = 0.71 (highly statistically significant at 0.01% level)
# 0.71 is the predicted probability for mortage application approved for nonwhite people (approve=1,white=0)
# The approval probability is approximately 71% for nonwhite people on average (ceteris paribus)

#Coefficient β1 white = 0.2 (highly statistically significant 0.01% level)
#for white people (=1), the approval probability is approximately 20 percentage point 
#higher than nonwhite people on average (ceteris paribus) 

#Difference between white and nonwhite 
#nonwhite (=0) -> intercept = 0.71 -> the approval probability for nonwhite people is 71% 
#white (=1) -> intercept + β1 = 0.71 + 0.2 = 0.91 -> the probability for white people is 91%
#white people have a higher probability of approval, which around 21% higher 

#### (b) ---------------------------------------------------------
#Q:Add the following controls to the LPM: hrat, obrat, loanprc, unem, male, married,
#dep, sch, cosign, chist, pubrec, mortlat1, mortlat2, vr. 
#How does the coefficienton white change? 
#Is there still evidence consistent with discrimination after including these borrower and loan characteristics?

#Model: approve = β0 + β1 white + β2 hrat + β3 obrat + β4 loanprc + β5 unem + β6 male + β7 married 
       # + β8 dep + β9 sch + β10 cosign + β11 chist + β12 pubrec + β13 mortlat1 + β14 mortlat2 + β15 vr

md2 <- lm( approve ~ white + hrat + obrat + loanprc + unem + male + married +
                     dep + sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr,
           data = loanapp)
summary(md2)

#1: How does the coefficient on white change? 
#After adding controls variables, the coefficient of white decreases to β1 = 0.13 
#The coefficient on white decreased by approxiamtely 7 percentage point 

#2: Is there still evidence consistent with discrimination after including these 
#borrower and loan characteristics?
#Yes, after including the borrower and loan characteristics, the magnitude of white slightly decreased
#However, the coefficient remains positive and p_value extremely small (highly statistically significant at 0.01% level)

#### (c) ---------------------------------------------------------
#Q:Re-estimate the model from part (b) using: a probit model and a logit model. 
md_probit <- glm(approve ~ white + hrat + obrat + loanprc + unem + male + 
                 married + dep + sch + cosign + chist + pubrec + 
                 mortlat1 + mortlat2 + vr, 
                 family = binomial(link = "probit"), data = loanapp)
summary(md_probit)

md_logit <- glm(approve ~ white + hrat + obrat + loanprc + unem + male + 
                married + dep + sch + cosign + chist + pubrec + 
                mortlat1 + mortlat2 + vr, 
                family = binomial(link = "logit"), data = loanapp)
summary(md_logit)

#1: Report the coefficient on white
summary(md_probit)$coefficients["white", ]
summary(md_logit)$coefficients["white", ]
#probit: β1 white = 0.52 (highly statistically significant at 0.01% level)
#logit: β1 white = 0.94 (highly statistically significant at 0.01% level)

#2: Compute the average marginal effect (AME) of white
library(margins)
ame_probit <- margins(md_probit, variables = "white")
ame_logit <- margins(md_logit, variables = "white")
ame_probit; ame_logit
#AME(probit) of white = 0.086
#AME(logit) of white = 0.083

#3: Compare the AME to the corresponding LPM coefficient. Are the substantive conclusions similar?
#Comparision: 
#LPM (question b) = 0.13
#AME(probit) = 0.086 
#AME(logit) = 0.082

#All 3 models for white people show the positive and highly statistically significant 
#For white people, under 3 models, they always have 8.2% - 13% higher in approval probability than nonwhite people
#Strong evidence consistent for discrimination (substantive conclusions similar)

############################################################
# BONUS
#Up to this point, your analysis uses data on individuals who applied for a mortgage loan. 
#Consider the following questions carefully:
#### (a) ---------------------------------------------------------
#Q: Is the group of people who apply for a loan necessarily representative of everyone who might want a mortgage? 
#Discuss factors such as expectations, information, prior experiences, or constraints.

#No, the group of applicants is NOT representative of grought which might want a mortgage
#becuase of the "selection" problem, influenced by factors:

#1: Expectation: people who worried that they might be reject due to credit history, 
#being discrimitaed -> maybe discorage from applying
#2: Informatic Asymmetric: ot all the applicants have equal access to information. 
#Less information, people feel less likely to apply 
#3: Other constraints: time constraints, complicated documentation procedures, etc 

#because the dataset only includes people who applied their documents for a mortgage, 
#implying  the applicant pool is understated, may not reflect the true distribution of mortgage demand

#### (b) ---------------------------------------------------------
#Q:If the applicant pool is systematically different across racial or socioeconomic groups, how
#might this influence the estimates from parts (i)–(iii)? Is the direction of potential bias obvious or ambiguous?

#If the applicant pool is systematically different across racial (white and nonwhite) 
#or socioeconomic groups, the decision to apply for a mortgage is not random.
#Instead, it may depend on actors mentioned in bonus question (a)
#Therefore, the sample of applicants may no longer represent all individuals who want a mortgage. 
#As a result, the estimated effect of racial discrimination in parts (a)–(c) may change, 
#and the direction of the bias is ambiguous

#### (c) ---------------------------------------------------------
#Q:Briefly describe, in general terms, how an econometric approach could address situations where
#outcomes are observed only for a selected subset of the population.

#To deal with this situation, we need to explicitly model how individuals enter the observed sample, 
#instead of unreasonably assuming that the observed data are random.
#People must first make a decision to be included in the data. 
#By taking decision into account before analyzing the final outcome, 
#the analysis can reduce the bias caused by observing results only for certain individuals 
#and provide more reliable estimates

############################################################
# End of script
############################################################

