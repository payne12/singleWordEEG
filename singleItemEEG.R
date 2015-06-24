#################################################################################
#Code supplement for Appendix A in Payne, Lee, and Federmeier (2015).
#Submitted to Psychophysiology
#Linear Mixed-Effects Models 1 and 2. 
#################################################################################
#load required packages
require(lme4) #Linear Mixed-Effect Modeling. 
#Documentation: http://cran.r-project.org/web/packages/lme4/index.html

#load EEG data file from GitHub Repository using RIO. 
#Documentation:http://cran.r-project.org/web/packages/rio/index.html
#Note: must install "devtools" to download rio from github
library("devtools")
  install_github("leeper/rio")
  library("rio")
centroParietal_OpenClass<-import("https://raw.githubusercontent.com/payne12/singleWordEEG/master/centroParietal_OpenClass.csv")
summary(centroParietal_OpenClass) #summarize data set

#############
#Variable IDs
#############
#meanAmpEEG = Mean Amplitude of Single-Item Event-Related EEG from 300-500 ms (N400)
#zWordOrder = Ordinal Word Position, standardized N ~(0,1)
#C1 = Sentence Context Contrast: Congruent vs. Syntactic Prose
#C2 = Sentence Context Contrast: Congruent vs. Random
#chan = channel ID
#SubID = Subject ID
#words = individual word ID
#zLog_Freq_HAL = log transformed HAL Word Frequency, standardized N ~(0,1)
#zOLD20 = Orthgraphic Levenshtein Distance (20 nearest neighbors), standardized N ~(0,1)
#zSentLength = Overall Sentence Length, standardized N ~(0,1)
#zLength = word length, standardized N ~(0,1)
#zConc= Rated Concreteness of meaning, standardized N ~(0,1)

########################################
#Model 1: Word Position Effects 
#(see Appendix, pg X)
########################################
lmeModel1 <-lmer(meanAmpEEG ~ zWordOrder + C1 + C2 
                 + zWordOrder:C1 + zWordOrder:C2  
                 + (1 | SubID)
                 + (1 | chan)
                 + (1 | words)
                 + (0 + zWordOrder:C1 | SubID)
                 + (0 + zWordOrder:C2 | SubID),  
                 data = centroParietal_OpenClass)
summary(lmeModel1) 

#Profile Likelihood Confidence Intervals
c(names(getME(lmeModel1,c("theta"))), names(fixef(lmeModel1))) #integer position of fixed-parameters
fixed <- c(7:12) #parms vector
CI_lmeModel1<-confint(lmeModel1,fixed, method = "profile" ) #profile confidence intervals for fixed-effects. 
#Note profile likelihood method is computationally intensive and takes time. method = Wald gives fast approximate CIs.  

###################################################
#Model 2: Effects of Context on Lexical Processing 
# (see Appendix p X)
###################################################
lmeModel2 <- lmer(meanAmpEEG ~  zWordOrder + zLog_Freq_HAL + zOLD20  #lexical 
                  + C1 + C2  #sentence context factors
                  + zWordOrder:(zLog_Freq_HAL + zOLD20) #two-way interactions
                  + C1:(zWordOrder + zLog_Freq_HAL + zOLD20) 
                  + C2:(zWordOrder + zLog_Freq_HAL + zOLD20)
                  + zLog_Freq_HAL:zOLD20 
                  + zWordOrder:C1:zLog_Freq_HAL #three-way interactions
                  + zWordOrder:C2:zLog_Freq_HAL 
                  + zWordOrder:C1:zOLD20  
                  + zWordOrder:C2:zOLD20 
                  + zLength + zConc + zSentLength #covariates
                  + (1 | SubID) #random effect structure
                  + (1 | chan)
                  + (1 | words)
                  + (0 + zWordOrder:C1:zLog_Freq_HAL| SubID)
                  + (0 + zWordOrder:C2:zLog_Freq_HAL| SubID)
                  + (0 + zWordOrder:C1:zOLD20| SubID)
                  + (0 + zWordOrder:C2:zOLD20 | SubID),
                  data = centroParietal_OpenClass) 
summary(lmeModel2)
#Profile Likelihood Confidence Intervals
c(names(getME(lmeModel2,c("theta"))), names(fixef(lmeModel2))) #integer position of fixed-parameters
fixed2 <- c(9:30) #parms vector
CI_lmeModel2<-confint(lmeModel2,fixed2, method = "profile" ) #profile confidence intervals for fixed-effects. 
#Note profile likelihood method is computationally intensive and takes time. method = Wald gives fast approximate CIs  


####Model Output: June 24 2015####
# sessionInfo()
# R version 3.1.2 (2014-10-31)
# Platform: x86_64-apple-darwin10.8.0 (64-bit)
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] splines   grid      stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] MASS_7.3-35                  pbkrtest_0.4-2               languageR_1.4.1              Hmisc_3.15-0                
# [5] Formula_1.2-0                survival_2.37-7              lattice_0.20-29              mgcv_1.8-4                  
# [9] nlme_3.1-119                 LMERConvenienceFunctions_2.5 plyr_1.8.1                   psych_1.4.8.11              
# [13] lme4_1.1-7                   Rcpp_0.11.3                  Matrix_1.1-4                 ggplot2_1.0.0               
# 
# loaded via a namespace (and not attached):
#   [1] acepack_1.3-3.3     cluster_1.15.3      colorspace_1.2-4    digest_0.6.4        foreign_0.8-61      gtable_0.1.2       
# [7] latticeExtra_0.6-26 minqa_1.2.4         munsell_0.4.2       nloptr_1.0.4        nnet_7.3-8          parallel_3.1.2     
# [13] proto_0.3-10        RColorBrewer_1.0-5  reshape2_1.4        rpart_4.1-8         scales_0.2.4        stringr_0.6.2      
# [19] tools_3.1.2        

#
# summary(lmeModel1)
# Linear mixed model fit by REML ['lmerMod']
# Formula: meanAmpEEG ~ zWordOrder + C1 + C2 + zWordOrder:C1 + zWordOrder:C2 +  
#   (1 | SubID) + (1 | chan) + (1 | words) + (0 + zWordOrder:C1 |      SubID) + (0 + zWordOrder:C2 | SubID)
# Data: centroParietal_OpenClass
# 
# REML criterion at convergence: 1449761
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.3256 -0.6614 -0.0019  0.6566  3.3481 
# 
# Random effects:
#   Groups   Name          Variance Std.Dev.
# words    (Intercept)    3.70050 1.9237  
# SubID    zWordOrder:C2  0.11948 0.3457  
# SubID.1  zWordOrder:C1  0.19703 0.4439  
# SubID.2  (Intercept)    0.57796 0.7602  
# chan     (Intercept)    0.01818 0.1348  
# Residual               53.51642 7.3155  
# Number of obs: 212268, groups:  words, 895; SubID, 28; chan, 8
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)    0.62046    0.16730   3.709
# zWordOrder     0.55298    0.03156  17.519
# C1            -0.63189    0.03950 -15.998
# C2            -0.61269    0.03924 -15.614
# zWordOrder:C1 -0.45994    0.09390  -4.898
# zWordOrder:C2 -0.50095    0.07751  -6.463
# 
# Correlation of Fixed Effects:
#   (Intr) zWrdOr C1     C2     zWO:C1
# zWordOrder  -0.006                            
# C1          -0.117  0.016                     
# C2          -0.118  0.018  0.503              
# zWrdOrdr:C1  0.000 -0.294 -0.011 -0.005       
# zWrdOrdr:C2  0.001 -0.370 -0.008 -0.003  0.123

# CI_lmeModel1
#                   2.5 %     97.5 %
# (Intercept)    0.2892547  0.9515667
# zWordOrder     0.4910954  0.6148421
# C1            -0.7093152 -0.5544835
# C2            -0.6896071 -0.5357894
# zWordOrder:C1 -0.6459808 -0.2733374
# zWordOrder:C2 -0.6542603 -0.3474391

# CI_lmeModel1
#                     2.5 %     97.5 %
# (Intercept)    0.2892547  0.9515667
# zWordOrder     0.4910954  0.6148421
# C1            -0.7093152 -0.5544835
# C2            -0.6896071 -0.5357894
# zWordOrder:C1 -0.6459808 -0.2733374
# zWordOrder:C2 -0.6542603 -0.3474391
# > summary(lmeModel2)
# Linear mixed model fit by REML ['lmerMod']
# Formula: meanAmpEEG ~ zWordOrder + zLog_Freq_HAL + zOLD20 + C1 + C2 +  
#   zWordOrder:(zLog_Freq_HAL + zOLD20) + C1:(zWordOrder + zLog_Freq_HAL +  
#                                               zOLD20) + C2:(zWordOrder + zLog_Freq_HAL + zOLD20) + zLog_Freq_HAL:zOLD20 +  
#   zWordOrder:C1:zLog_Freq_HAL + zWordOrder:C2:zLog_Freq_HAL +      zWordOrder:C1:zOLD20 + zWordOrder:C2:zOLD20 + zLength + zConc +  
#   zSentLength + (1 | SubID) + (1 | chan) + (1 | words) + (0 +  
#                                                             zWordOrder:C1:zLog_Freq_HAL | SubID) + (0 + zWordOrder:C2:zLog_Freq_HAL |  
#                                                                                                       SubID) + (0 + zWordOrder:C1:zOLD20 | SubID) + (0 + zWordOrder:C2:zOLD20 |      SubID)
# Data: centroParietal_OpenClass
# 
# REML criterion at convergence: 1448841
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -2.96430 -0.66060 -0.00192  0.65822  2.97387 
# 
# Random effects:
#   Groups   Name                        Variance Std.Dev.
# words    (Intercept)                  3.44775 1.8568  
# SubID    zWordOrder:C2:zOLD20         0.21434 0.4630  
# SubID.1  zWordOrder:C1:zOLD20         0.16504 0.4063  
# SubID.2  zWordOrder:C2:zLog_Freq_HAL  0.24270 0.4926  
# SubID.3  zWordOrder:C1:zLog_Freq_HAL  0.36901 0.6075  
# SubID.4  (Intercept)                  0.57371 0.7574  
# chan     (Intercept)                  0.01817 0.1348  
# Residual                             53.24778 7.2971  
# Number of obs: 212268, groups:  words, 895; SubID, 28; chan, 8
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                  0.738147   0.170153   4.338
# zWordOrder                   0.703975   0.033846  20.799
# zLog_Freq_HAL               -0.207005   0.095903  -2.158
# zOLD20                       0.189629   0.085600   2.215
# C1                          -0.702608   0.039973 -17.577
# C2                          -0.666154   0.039595 -16.824
# zLength                      0.040550   0.023838   1.701
# zConc                       -0.323842   0.076033  -4.259
# zSentLength                 -0.109941   0.021611  -5.087
# zWordOrder:zLog_Freq_HAL    -0.548812   0.035994 -15.247
# zWordOrder:zOLD20           -0.057298   0.035775  -1.602
# zWordOrder:C1               -0.535151   0.042480 -12.598
# zLog_Freq_HAL:C1             0.537880   0.045303  11.873
# zOLD20:C1                    0.178799   0.044660   4.004
# zWordOrder:C2               -0.563952   0.042169 -13.374
# zLog_Freq_HAL:C2             0.555302   0.044800  12.395
# zOLD20:C2                    0.102871   0.044928   2.290
# zLog_Freq_HAL:zOLD20         0.007082   0.063506   0.112
# zWordOrder:zLog_Freq_HAL:C1  0.396009   0.123997   3.194
# zWordOrder:zLog_Freq_HAL:C2  0.552062   0.103956   5.311
# zWordOrder:zOLD20:C1         0.013106   0.090885   0.144
# zWordOrder:zOLD20:C2         0.026112   0.100018   0.261
# 
# Correlation matrix not shown by default, as p = 22 > 20.
# Use print(...., correlation=TRUE)  or
# vcov(....)	 if you need it

# CI_lmeModel2
#                                     2.5 %      97.5 %
# (Intercept)                  0.404653807  1.07164057
# zWordOrder                   0.637637862  0.77031233
# zLog_Freq_HAL               -0.394971683 -0.01903876
# zOLD20                       0.021855526  0.35740302
# C1                          -0.780952890 -0.62426216
# C2                          -0.743758211 -0.58854981
# zLength                     -0.006171547  0.08727228
# zConc                       -0.472863364 -0.17481987
# zSentLength                 -0.152298967 -0.06758373
# zWordOrder:zLog_Freq_HAL    -0.619359632 -0.47826484
# zWordOrder:zOLD20           -0.127415696  0.01282067
# zWordOrder:C1               -0.618411162 -0.45189161
# zLog_Freq_HAL:C1             0.449088321  0.62667142
# zOLD20:C1                    0.091268178  0.26633041
# zWordOrder:C2               -0.646601934 -0.48130145
# zLog_Freq_HAL:C2             0.467494980  0.64310825
# zOLD20:C2                    0.014812655  0.19092891
# zLog_Freq_HAL:zOLD20        -0.117386821  0.13155143
# zWordOrder:zLog_Freq_HAL:C1  0.152978784  0.63903828
# zWordOrder:zLog_Freq_HAL:C2  0.348312292  0.75581092
# zWordOrder:zOLD20:C1        -0.165025370  0.19123662
# zWordOrder:zOLD20:C2        -0.169920766  0.22214418