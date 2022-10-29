# Packages
library(lmerTest) # packageVersion("lmerTest") 3.1.1
library(psych) # packageVersion("psych") 1.8.4
library(plyr) # packageVersion("plyr") 1.8.4
library(ICC) # packageVersion("ICC") 2.3.0
library(lmSupport) # packageVersion("lmSupport") 2.9.13
library(moments) # packageVersion("moments") .14
library(PerformanceAnalytics) # packageVersion("PerformanceAnalytics") 1.5.2
library(tidyverse)
# install.packages("parameters")
# Change WD
setwd("/Users/joshuaupshaw/Documents/A_Projects/First Year Project/Data/R project")
# Read in data set
BigTech_Final_Excludes <- read.csv("BigTech_Final_Excludes_72p.csv")
str(BigTech_Final_Excludes)
# Read in Demographics data ####
DemoScaleInfo <- read.csv("DemoScaleInfo_72p.csv")
BigTech_Final_Excludes2 <- merge(DemoScaleInfo, BigTech_Final_Excludes,by="PartID")
BigTech_Final_Excludes2 <- mutate(BigTech_Final_Excludes2, SAPS_Split = ifelse(SAPS_MedSplit_Coded == "0", "Low SAPS", "High SAPS"))
str(BigTech_Final_Excludes2)
# Create new dataset without mower trials#####
BigTech_FinalExcludes_NoMow <- BigTech_Final_Excludes2[!BigTech_Final_Excludes2$Sound == "mower", ]
# Changing Square sound to control sound ####
BigTech_FinalExcludes_NoMow <- mutate(BigTech_FinalExcludes_NoMow, 
                                      Sound = as.factor(ifelse(Sound == "phone", "phone",
                                                     ifelse(Sound == "mower", "mower", "control"))))

# Create factor block order variable ####
BigTech_FinalExcludes_NoMow$Block_Ord.f <- as.factor(BigTech_FinalExcludes_NoMow$Block_Ord)                                                                          
# Half variable for SAPS ####
# cut(BigTech_FinalExcludes_NoMow$SAPS_Ave, 2)
# (2.6+1.2)/2
# low <- c(1.2, (1.2 + .7))
# low#[1.2 1.9]
# high <- c(1.9, (1.9 + .7))
# high#[1.9 2.6]
# describeBy(BigTech_FinalExcludes_NoMow$RT.ms, BigTech_FinalExcludes_NoMow$SAPS_Split)
# describe(BigTech_FinalExcludes_NoMow$SAPS_Ave)

# RTby_SAPSsplit <- ddply(BigTech_FinalExcludes_NoMow,
#                         .(SAPS_Split, PartID),
#                         summarise,
#                         M.RT.ms = mean(RT.ms),
#                         SD.RT.ms = sd(RT.ms),
#                         n.RT.ms = length(RT.ms),
#                         M.SAPS = mean(SAPS_Ave)) 
# RTby_SAPSsplit

# Create Thirds variable for SAPS ####
# cut(BigTech_FinalExcludes_NoMow$SAPS_Ave, 3)
# (2.6-1.2)/3
# low <- c(1.2, (1.2 + .47))
# #[1.20 1.67]
# mid <- c(1.67, (1.67 + .47))
# #[1.67 2.14]
# high <- c(2.14, (2.14 + .47))
# #[2.14 2.61]
BigTech_FinalExcludes_NoMow <- mutate(BigTech_FinalExcludes_NoMow, SAPS_thirds = as.factor(ifelse(SAPS_Ave < 1.67, "Low Third",
                                                                                        ifelse(SAPS_Ave > 2.14, "High Third", "Mid Third"))))
# BigTech_FinalExcludes_NoMow$SAPS_thirds <- cut(BigTech_FinalExcludes_NoMow$SAPS_Ave, 3, labels = c("Low Third", "Mid Third", "High Third"))
# str(BigTech_FinalExcludes_NoMow$SAPS_thirds)
# levels(BigTech_FinalExcludes_NoMow$SAPS_thirds)
# describeBy(BigTech_FinalExcludes_NoMow$RT.ms, BigTech_FinalExcludes_NoMow$SAPS_thirds)
# RTby_SAPSthird <- ddply(BigTech_FinalExcludes_NoMow,
#                    .(SAPS_thirds, PartID),
#                    summarise,
#                    M.RT.ms = mean(RT.ms),
#                    SD.RT.ms = sd(RT.ms),
#                    n.RT.ms = length(RT.ms),
#                    M.SAPS = mean(SAPS_Ave)) 
# RTby_SAPSthird

# RT Means for each part overall#####
RTMeans <- aggregate(data = BigTech_FinalExcludes_NoMow, RT.ms ~ PartID, mean)
names(RTMeans)[2] <- "ParticipantM"
BigTech_FinalExcludes_NoMow <- merge(BigTech_FinalExcludes_NoMow, RTMeans, by = 'PartID')
# Halfway variable#####
BigTech_FinalExcludes_NoMow <- mutate(BigTech_FinalExcludes_NoMow, Halfway = as.factor(ifelse(Block_Ord <= 8, "First Half", "Second Half")))
# Block Quarters variable#####
BigTech_FinalExcludes_NoMow <- mutate(BigTech_FinalExcludes_NoMow, BlockQuarter = as.factor(ifelse(Block_Ord <= 4, "First Qrt",
                                                                                                   ifelse(Block_Ord < 4 & Block_Ord <= 8, "Second Qrt",
                                                                                                          ifelse(Block_Ord < 8 & Block_Ord <= 12, "Third Qrt", "Fourth Qrt")))))

# Create coded sound variable 0 = control, 1 = phone ####
BigTech_FinalExcludes_NoMow <- mutate(BigTech_FinalExcludes_NoMow, Sound.coded = as.numeric(ifelse(Sound == "control", 0, 1)))
# Create coded trial type variable 0 = frequent, 1 = rare ####
BigTech_FinalExcludes_NoMow <- mutate(BigTech_FinalExcludes_NoMow, TrialType.coded = as.numeric(ifelse(Code_Good == "Frequent", 0, 1)))

# Save Dataset####
# write_csv(BigTech_FinalExcludes_NoMow, "BigTech_FinalExcludes_NoMow_72p_2.csv")
# Read in data set #############3
BigTech_FinalExcludes_NoMow <- read.csv("BigTech_FinalExcludes_NoMow_72p_2.csv")
control <- lmerControl(check.nobs.vs.nRE="ignore", calc.derivs=FALSE)
# Outlier Detection ####
# create a variable to filter outliers
# BigTech_FinalExcludes_NoMow$outlier <- 0 # to start out, nothing is an outlier, all rows get a zero
# # find outlier cutoffs for overall RT mean
# meanRT <- mean(BigTech_FinalExcludes_NoMow$RT.ms) # mean of rt of "good" subjects without errors
# sdRT <- sd(BigTech_FinalExcludes_NoMow$RT.ms)
# highcut <- meanRT + 2.5*sdRT
# lowcut <- meanRT - 2.5*sdRT
# # identify outliers
# BigTech_FinalExcludes_NoMow$outlier[ ((BigTech_FinalExcludes_NoMow$RT.ms < lowcut) | (BigTech_FinalExcludes_NoMow$RT.ms > highcut)) ] <- 1
# # count outliers
# xtabs(~BigTech_FinalExcludes_NoMow$outlier[ (BigTech_FinalExcludes_NoMow$outlier == 1)])
# xtabs(~BigTech_FinalExcludes_NoMow$outlier == 1)
# 739 overall RT outliers
# New dataset without overall RT outliers ####
# BigTech_NoMow_outliersremoved <- BigTech_FinalExcludes_NoMow[!BigTech_FinalExcludes_NoMow$outlier == 1, ]
# Save Dataset ####
# write_csv(BigTech_NoMow_outliersremoved, "BigTech_NoMow_outliersremoved_72p.csv")
# Read in data set 
# BigTech_NoMow_outliersremoved <- read.csv("BigTech_NoMow_outliersremoved_72p.csv")
######################################
# Demos ####
unique(BigTech_FinalExcludes_NoMow$PartID)
Demos <- summarise(BigTech_FinalExcludes_NoMow,
                   M.age = mean(Demo_Age),
                   SD.age = sd(Demo_Age),
                   Prct.Female = mean(Coded_Sex.0.male.1.female.),
                   Prct.NonWhite = mean(Race.0.White.1.Non.white.),
                   n = length(unique(BigTech_FinalExcludes_NoMow$PartID)))
Demos
##### Overall findings Models without SAPS ######
# ICC of RT within participants
ICCbare(data = BigTech_FinalExcludes_NoMow, PartID, RT.ms) # 0.1481515
# ICC value indicates that a portion of variance in overall
# RT can be attributed to RT differences within participants
ICCest(data = BigTech_FinalExcludes_NoMow, PartID, RT.ms)
# LowerCI = 0.1103725, UpperCI = 0.2067649

# Overall RT
RTOverall <- describe(BigTech_FinalExcludes_NoMow$RT.ms)
#write.table(RTOverall, file = "RTOverall_72p.txt", sep = ",", quote = FALSE, row.names = F)

# ggplot(BigTech_FinalExcludes_NoMow, aes(RT.ms)) +
#   geom_histogram(binwidth = 5) +
#   geom_vline(aes(xintercept = mean(BigTech_FinalExcludes_NoMow$RT.ms)))
# dev.off()

# RT by participant
# RTbyPartID <- ddply(BigTech_FinalExcludes_NoMow,
#                        .(PartID),
#                        summarise,
#                        M.RT.ms = mean(RT.ms),
#                        SD.RT.ms = sd(RT.ms),
#                        n.RT.ms = length(RT.ms)) 
# RTbyPartID
# write.table(RTbyPartID, file = "RTbyPartID_72p.txt", sep = ",", quote = FALSE, row.names = F)

##### Overall interaction Oddball Analyses #####
# Descriptives #####
RTbyTrialType <- ddply(BigTech_FinalExcludes_NoMow,
                       .(Code_Good),
                       summarise,
                       M.RT.ms = mean(RT.ms),
                       SD.RT.ms = sd(RT.ms),
                       SE.RT.ms = plotrix::std.error(RT.ms),
                       n.RT.ms = length(RT.ms)) 
RTbyTrialType


# BigTech_FinalExcludes_NoMow %>% 
#   ggplot(aes(x = Code_Good, y = RT.ms))+
#   theme_classic()+
#   geom_bar(stat = "identity")
#   Code_Good  M.RT.ms  SD.RT.ms n.RT.ms
# 1  Frequent 428.6612  87.96733   29370
# 2      Rare 480.3593 115.81611    2904
#write.table(RTbyTrialType, file = "RTbyTrialType_72p.txt", sep = ",", quote = FALSE, row.names = F)

# Overall Oddball effect
OddballEffect <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare"]) - mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent"])
OddballEffect # 50.17053

# Histogram
# ggplot(BigTech_FinalExcludes_NoMow, aes(RT.ms)) +
#   geom_density(kernel = "gaussian") +
#   geom_histogram(binwidth = 5, aes(color= BigTech_FinalExcludes_NoMow$Code_Good))

# Create a trellis plot with mean lines created for each Participant 
# this suggests that modeling intercepts is a good idea
# Trellice plot
# library(lattice)
# xyplot(BigTech_FinalExcludes_NoMow$RT.ms ~ BigTech_FinalExcludes_NoMow$Code_Good | BigTech_FinalExcludes_NoMow$PartID,
#        type = c("r", "p"),
#        panel = function (x, y){
#          panel.xyplot(x, y)
#          panel.abline(h = mean(y))
#        }
# )
# Trellis Plot for slope differences
# xyplot(BigTech_FinalExcludes_NoMow$RT.ms ~ BigTech_FinalExcludes_NoMow$Code_Good | BigTech_FinalExcludes_NoMow$PartID,
#        type = c("p", "r"), col = 'blue')

# Anova/regression model #####
# mod.OverallOddball_lm <- lm(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + Code_Good)
# summary(mod.OverallOddball_lm)
# # Coefficients:
# # Estimate Std. Error t value Pr(>|t|)    
# # (Intercept)    428.661      0.530  808.86   <2e-16 ***
# # Code_GoodRare   51.698      1.767   29.26   <2e-16 ***
# # 
# # Residual standard error: 90.82 on 32272 degrees of freedom
# # Multiple R-squared:  0.02585,	Adjusted R-squared:  0.02582 
# # F-statistic: 856.3 on 1 and 32272 DF,  p-value: < 2.2e-16
# 
# modelEffectSizes(mod.OverallOddball_lm)
# # Coefficients
# # SSR df pEta-sqr dR-sqr
# # (Intercept) 5396748812  1   0.9530     NA
# # Code_Good      7063128  1   0.0258 0.0258
# # 
# # Sum of squared errors (SSE): 266203736.8
# # Sum of squared total  (SST): 273266864.8
# confint(mod.OverallOddball_lm)
# #                    2.5 %    97.5 %
# # (Intercept)   427.62241 429.69989
# # Code_GoodRare  48.23525  55.16097
# 
# # Visualizing non independence of data within participants 
# boxplot(resid(mod.OverallOddball_lm) ~ BigTech_FinalExcludes_NoMow$PartID, xlab = "Participant", ylab = "RT", main = "ANOVA Model: Residual RTs predicted by Frequent and Rare Trials across Participants"); abline(0,0)
# 
# 
# # Exploring normality of residuals
# as.data.frame(resid(mod.OverallOddball_lm)) -> res
# # renaming the residuals column
# colnames(res)[1] <- "e"
# 
# plot(density(res$e))
# qqnorm(res$e); qqline(res$e)
# skewness(res$e) # 0.5204232 
# # homoscedasticity 
# plot(mod.OverallOddball_lm)
# # linearity
# residPlot <- data.frame(fitted(mod.OverallOddball_lm), resid(mod.OverallOddball_lm))
# names(residPlot) <- c("predicted", "residual")
# # now create the scatterplot with a LOESS smoother added
# ggplot(residPlot,
#        aes(x = predicted,
#            y = residual)) +
#   theme_bw() +
#   geom_point(shape = 1) +
#   geom_smooth(method = "lm", se = F)

# All suggesting normal distribution of RT residuals for Frequent and Rare Stimuli

# Mixed model to account for variance in RT within particpants #####
mod.OverallOddball_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + Code_Good + (Code_Good|PartID), REML = F)
summary(mod.OverallOddball_lmer)
# Summary
# Random effects:
# Groups   Name        Variance Std.Dev.
# PartID   (Intercept) 1243     35.25   
# Residual             7007     83.71   
# Number of obs: 32274, groups:  PartID, 59
# 
# Fixed effects:
#                 Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)     428.550      4.616    59.117   92.85   <2e-16 ***
# Code_GoodRare    52.238      1.629 32215.607   32.06   <2e-16 ***

# Model Effect Sizes
as.data.frame(VarCorr(mod.OverallOddball_lmer)) -> vars.mod.OverallOddball_lmer
confint(mod.OverallOddball_lmer)
# create base model 
basemodel <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ (1|PartID), REML = F)
as.data.frame(VarCorr(basemodel)) -> vars.basemodel
pseudoR2 <- 1 - ((vars.mod.OverallOddball_lmer[1,4] + vars.mod.OverallOddball_lmer[2,4]) / (vars.basemodel[1,4] + vars.basemodel[2,4]))
pseudoR2 # 0.7710194
# 77.10% of additional variance is explained by the mixed model compared to the intercept only model
# Exploring normality of residuals
as.data.frame(resid(mod.OverallOddball_lmer)) -> res
# renaming the residuals column
colnames(res)[1] <- "e"
plot(density(res$e))
qqnorm(res$e); qqline(res$e)
skewness(res$e) # 0.6699011 
# homoscedasticity 
plot(mod.OverallOddball_lmer)
# linearity
residPlot <- data.frame(fitted(mod.OverallOddball_lmer), resid(mod.OverallOddball_lmer))
names(residPlot) <- c("predicted", "residual")
# now create the scatterplot with a LOESS smoother added
ggplot(residPlot,
       aes(x = predicted,
           y = residual)) +
  theme_bw() +
  geom_point(shape = 1) +
  geom_smooth(method = "lm", se = F)

# Comparing models#####
boxplot(resid(mod.OverallOddball_lmer) ~ BigTech_FinalExcludes_NoMow$PartID, xlab = "Participant", ylab = "RT", main = "Mixed Model: Residual RTs Predicted by Frequent and Rare Trials across Participants"); abline(0,0)

anova(mod.OverallOddball_lmer, mod.OverallOddball_lm)
#                         Df    AIC    BIC  logLik deviance  Chisq Chi Df
# mod.OverallOddball_lm    3 382635 382660 -191314   382629                         
# mod.OverallOddball_lmer  4 377645 377679 -188819   377637 4991.5      1  < 2.2e-16 ***
# Improved fit of lmer model

##### RT differences across sounds #####
# Descritpives #####
RTbySounds <- ddply(BigTech_FinalExcludes_NoMow,
                    .(Sound),
                    summarise,
                    M.RT.ms = mean(RT.ms),
                    SE.RT.ms = plotrix::std.error(RT.ms),
                    SD.RT.ms = sd(RT.ms),
                    n.RT.ms = length(RT.ms)) 
RTbySounds
#     Sound  M.RT.ms SD.RT.ms n.RT.ms
# 1 control 431.4619 91.43978   16095
# 2   phone 435.1543 92.55617   16179
write.table(RTbySounds, file = "RTbySounds_72p.txt", sep = ",", quote = FALSE, row.names = F)

# Histogram
ggplot(BigTech_FinalExcludes_NoMow, aes(RT.ms)) +
  geom_histogram(binwidth = 5, aes(color= BigTech_FinalExcludes_NoMow$Sound)) +
  geom_vline(aes(xintercept = mean(BigTech_FinalExcludes_NoMow$RT.ms))) +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(angle = 0, color="blue",size=15, face=3)) +
  theme(axis.title = element_text(angle = 0, color="black", size=15, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  theme(legend.key = element_rect(fill = "white", colour = "black")) +
  theme(legend.text = element_text(size = 15, colour = "Black")) +
  theme(legend.title = element_text(face = "bold")) #+
  theme(legend.position = c(.25, .15),
        legend.justification = c("right", "top"),
        legend.box.just = "center",
        legend.margin = margin(6, 6, 6, 6))

# Sound RT plot
RTMeans4 <- aggregate(data = BigTech_FinalExcludes_NoMow, RT.ms ~ Sound, mean)
RTMeans4 <- mutate(RTMeans4, Group = c(1:2))
RTMeansAll <- spread(BigTech_FinalExcludes_NoMow, ParticipantM)
library(reshape2)
RTMeansAll <- dcast(BigTech_FinalExcludes_NoMow, PartID ~ Sound, value.var="ParticipantM")
# RTMeansAll1 <- dcast(BigTech_FinalExcludes_NoMow, Sound ~ PartID, value.var="ParticipantM")
# RTMeansAll1 <- spread(BigTech_FinalExcludes_NoMow, ParticipantM)

RTMeansAll2 <- melt(RTMeansAll, id.vars=c("PartID"), measure.vars=c("control", "phone"), 
                    variable.name="Sound", value.name="RTMean")
str(RTMeans4)
library(plotrix)
ggplot(BigTech_FinalExcludes_NoMow, aes(y=RT.ms, x=Sound, color = Sound, group = Sound)) +
  theme_classic(base_size = 14)+
  set.seed(69) +
  geom_jitter(aes(fill = Sound), color = "black", alpha = .35, size= 3, width = .3, shape = 21) +
  # geom_errorbar(aes(ymin = mean(RT.ms) - plotrix::std.error(RT.ms), ymax = mean(RT.ms) + plotrix::std.error(RT.ms), 
  #                   group = BigTech_FinalExcludes_NoMow$Sound), color="black", width=0.70, size= 1.5) +
  stat_summary(fun.y = mean, geom = "point", color="black", size = 20, shape = 21, fill = "grey95")+
  stat_summary(fun.data = mean_se, geom = "errorbar", color="black", width=0.50, size= 1.75) +
  scale_fill_manual("Sound", values=c("grey45", "grey90")) +
  #scale_colour_manual("Sound", values = c("grey 17", "grey 54"))+
  #geom_hline(yintercept = RTMeans4$RT.ms) +
  labs(y = "RT (ms)", size = 12, face = "bold") +
  #coord_cartesian(ylim=c(400.00, 440.00), expand = T) +
  #scale_y_continuous(breaks = seq(400.00, 440.00, 10)) +
  scale_x_discrete(labels = c("control" = "Control Sound","phone" = "Smartphone Sound")) +
  theme(legend.position = "none") +
  theme(axis.text.y = element_text(angle = 0, color="black",size=30)) +
  theme(axis.text.x = element_text(angle = 0, color="black", size=40, face = "bold")) +
  theme(axis.title.y = element_text(size = 40, face = "bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.line.y = element_blank())

#theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  #theme(legend.key = element_rect(fill = "white", colour = "black")) +
  #theme(legend.text = element_text(size = 15, colour = "Black")) +
  #theme(legend.title = element_text(face = "bold"))
#geom_point(data = aes(mean(BigTech_FinalExcludes_NoMow$RT.ms)), size = 7, shape = 19, col = "black", fill = "Black") +
#geom_errorbar(data = BigTech_FinalExcludes_NoMow, aes(ymin= mean(RT.ms) - sd(RT.ms), ymax = mean(RT.ms) + sd(RT.ms)), color = "black", group = BigTech_FinalExcludes_NoMow$Sound)+

# Create a trellis plot with mean lines created for each Participant 
# this suggests that modeling intercepts is a good idea
# Trellice plot

xyplot(BigTech_FinalExcludes_NoMow$RT.ms ~ BigTech_FinalExcludes_NoMow$Sound | BigTech_FinalExcludes_NoMow$PartID,
       type = c("r", "p"),
       panel = function (x, y){
         panel.xyplot(x, y)
         panel.abline(h = mean(y))
       }
)
# Trellis Plot for slope differences
xyplot(BigTech_FinalExcludes_NoMow$RT.ms ~ BigTech_FinalExcludes_NoMow$Sound | BigTech_FinalExcludes_NoMow$PartID,
       type = c("p", "r"), col = 'blue')
str(BigTech_FinalExcludes_NoMow$Sound)
# ANOVA/regression model #####
BigTech_FinalExcludes_NoMow$Sound <- relevel(BigTech_FinalExcludes_NoMow$Sound, ref="phone")
contrasts(BigTech_FinalExcludes_NoMow$Sound)
mod.RTSounds_lm <- lm(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + Sound)
summary(mod.RTSounds_lm)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  435.1543     0.7233 601.625  < 2e-16 ***
# Soundcontrol  -3.6924     1.0242  -3.605 0.000313 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 92 on 32272 degrees of freedom
# Multiple R-squared:  0.0004026,	Adjusted R-squared:  0.0003716 
# F-statistic:    13 on 1 and 32272 DF,  p-value: 0.0003126
confint(mod.RTSounds_lm)

# Visualizing non independence of data within participants 
boxplot(resid(mod.RTSounds_lm) ~ BigTech_FinalExcludes_NoMow$PartID, xlab = "Participant", ylab = "RT", main = "ANOVA Model: Residual RTs Predicted by Sounds Across Participants"); abline(0,0)

# Exploring residuals
as.data.frame(resid(mod.RTSounds_lm)) -> res
# renaming the residuals column
colnames(res)[1] <- "e"
# looking at normality of residuals
plot(density(res$e))
qqnorm(res$e); qqline(res$e)
skewness(res$e) # 0.6043542
# Suggests minor deviation from normal distribution of RT residuals for Sound Stimuli
# homoscedasticity 
plot(mod.RTSounds_lm)
# linearity
residPlot <- data.frame(fitted(mod.RTSounds_lm), resid(mod.RTSounds_lm))
names(residPlot) <- c("predicted", "residual")
# now create the scatterplot with a LOESS smoother added
ggplot(residPlot,
       aes(x = predicted,
           y = residual)) +
  theme_bw() +
  geom_point(shape = 1) +
  geom_smooth(method = "lm", se = F)
# Mixed Model #####
BigTech_FinalExcludes_NoMow$Sound <- relevel(BigTech_FinalExcludes_NoMow$Sound, ref="phone")
mod.RTbySounds_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + Sound + (Sound|PartID), REML = F)
contrasts(BigTech_FinalExcludes_NoMow$Sound)
summary(mod.RTbySounds_lmer)
# Summary
# Random effects:
# Groups   Name        Variance Std.Dev.
# PartID   (Intercept) 1239     35.19   
# Residual             7227     85.01   
# Number of obs: 32274, groups:  PartID, 59
# 
# Fixed effects:
#             Estimate Std. Error         df t value  Pr(>|t|)    
# (Intercept)    435.2440     4.6302    60.2440  94.000   < 2e-16 ***
# Soundcontrol    -4.0049     0.9466 32215.2606  -4.231 0.0000233 ***
  
# Model Effect Sizes
as.data.frame(VarCorr(mod.RTbySounds_lmer)) -> vars.mod.RTbySounds_lmer
pseudoR2 <- 1 - ((vars.mod.RTbySounds_lmer[1,4] + vars.mod.RTbySounds_lmer[4,4]) / (vars.basemodel[1,4] + vars.basemodel[2,4]))
pseudoR2 # 0.002074977, Little variation exists between sounds
confint(mod.RTbySounds_lmer)

# Exploring residuals
as.data.frame(resid(mod.RTbySounds_lmer)) -> res
colnames(res)[1] <- "e"
plot(density(res$e))
qqnorm(res$e); qqline(res$e)
skewness(res$e) # 0.7570144
# homoscedasticity 
plot(mod.RTbySounds_lmer)
# linearity
residPlot <- data.frame(fitted(mod.RTbySounds_lmer), resid(mod.RTbySounds_lmer))
names(residPlot) <- c("predicted", "residual")
# now create the scatterplot with a LOESS smoother added
ggplot(residPlot,
       aes(x = predicted,
           y = residual)) +
  theme_bw() +
  geom_point(shape = 1) +
  geom_smooth(method = "lm", se = F)
# Comparing Models ####
boxplot(resid(mod.RTbySounds_lmer) ~ BigTech_FinalExcludes_NoMow$PartID, xlab = "Participant", ylab = "RT", main = "Mixed Model: Residual RTs Predicted by Sounds Across Participants"); abline(0,0)

anova(mod.RTbySounds_lmer, mod.RTSounds_lm)
#                     Df    AIC    BIC  logLik deviance  Chisq Chi Df
#mod.RTSounds_lm      3 383467 383492 -191730   383461                             
#mod.RTbySounds_lmer  4 378639 378673 -189316   378631 4829.4      1  < 2.2e-16 ***
# Improved fit of lmer model
##### RT differences across blocks #####
# Descritpives #####
RTbyBlockOrder <- ddply(BigTech_FinalExcludes_NoMow,
                        .(Block_Ord),
                        summarise,
                        M.RT.ms = mean(RT.ms),
                        SD.RT.ms = sd(RT.ms),
                        n.RT.ms = length(RT.ms)) 
RTbyBlockOrder
write.table(RTbyBlockOrder, file = "RTbyBlockOrder_72p.txt", sep = ",", quote = FALSE, row.names = F)

# Boxplot
ggplot(BigTech_FinalExcludes_NoMow, aes(Block_Ord, RT.ms, group = Block_Ord)) +
  geom_boxplot(size = 2) +
  geom_hline(aes(yintercept = mean(BigTech_FinalExcludes_NoMow$RT.ms))+
  xlab("Block")) #+
  # scale_x_continuous(breaks = seq(1,16))) 
# Across blocks
ggplot(BigTech_FinalExcludes_NoMow, aes(x = Block_Ord, y = RT.ms, group = Block_Ord)) +
  geom_boxplot(size = 2) +
  geom_abline(intercept = 462.6910, slope = -2.78, color = "Blue", size = 1.5) +
  scale_color_brewer(palette = "Set1") +
  geom_hline(yintercept = mean(BigTech_FinalExcludes_NoMow$RT.ms), color = "Red", size = 1.5) +
  labs(title = "Reaction Time Across Experiment Blocks", x = "Block", y = "RT (ms)", size = 12) +
  scale_x_continuous(breaks = seq(1,16)) +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(angle = 0, color="blue",size=15, face=3)) +
  theme(axis.title = element_text(angle = 0, color="black", size=15, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  theme(legend.key = element_rect(fill = "white", colour = "black")) +
  theme(legend.text = element_text(size = 15, colour = "Black")) +
  theme(legend.title = element_text(face = "bold"))


# Trial Type Code_Good across blocks
ggplot(BigTech_FinalExcludes_NoMow, aes(x = Block_Ord, y = RT.ms, color = Code_Good)) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  scale_color_brewer(palette = "Set1") +
  geom_hline(yintercept = mean(BigTech_NoMow_outliersremoved$RT.ms)) +
  labs(title = "Reaction Time Across Experiment Blocks", x = "Block", y = "RT (ms)", size = 12) +
  scale_x_continuous(breaks = seq(1,16)) +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.position = "bottom")
# SAPS_Split plot####
ggplot(BigTech_FinalExcludes_NoMow, aes(x = Block_Ord, y = RT.ms, color = SAPS_Split)) +
  geom_jitter(width = .2, alpha = .5, size = 1) +
  geom_smooth(method = lm, se = F) +
  scale_color_brewer(palette = "Set1") +
  geom_hline(yintercept = mean(BigTech_FinalExcludes_NoMow$RT.ms)) +
  labs(title = "Reaction Time Across Experiment Blocks", x = "Block", y = "RT (ms)", size = 12) +
  scale_x_continuous(breaks = seq(1,16)) +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(angle = 0, color="blue",size=15, face=3)) +
  theme(axis.title = element_text(angle = 0, color="black", size=15, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  theme(legend.key = element_rect(fill = "white", colour = "black")) +
  theme(legend.text = element_text(size = 15, colour = "Black")) +
  theme(legend.title = element_text(face = "bold")) +
  theme(legend.position = c(.25, .15),
        legend.justification = c("right", "top"),
        legend.box.just = "center",
        legend.margin = margin(6, 6, 6, 6))
# Sound plot ####
ggplot(BigTech_FinalExcludes_NoMow, aes(x = , y = RT.ms, color = Sound)) +
  geom_jitter(width = .1) +
  geom_smooth(method = lm, se = F) +
  scale_color_brewer(palette = "Set1") +
  geom_hline(yintercept = mean(BigTech_FinalExcludes_NoMow$RT.ms)) +
  labs(title = "Reaction Time Across Experiment Blocks", x = "Block", y = "RT (ms)", size = 12) +
  scale_x_continuous(breaks = seq(1,16)) +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.position = "bottom")
# Create a trellis plot with mean lines created for each Participant 
# this suggests that modeling intercepts is a good idea
# Trellice plot

xyplot(BigTech_FinalExcludes_NoMow$RT.ms ~ BigTech_FinalExcludes_NoMow$Block_Ord | BigTech_FinalExcludes_NoMow$PartID,
       type = c("r", "p"),
       panel = function (x, y){
         panel.xyplot(x, y)
         panel.abline(h = mean(y))
       }
)
# Trellis Plot for slope differences
xyplot(BigTech_FinalExcludes_NoMow$RT.ms ~ BigTech_FinalExcludes_NoMow$Block_Ord | BigTech_FinalExcludes_NoMow$PartID,
       type = c("p", "r"), col = 'blue')
str(BigTech_FinalExcludes_NoMow$Sound)
# ANOVA/regression model #####
mod.BlockOrd_lm <- lm(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + Block_Ord)
summary(mod.BlockOrd_lm)
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 457.5541     1.0725  426.63   <2e-16 ***
#   Block_Ord    -2.8332     0.1105  -25.65   <2e-16 ***
# Residual standard error: 91.1 on 32272 degrees of freedom
# Multiple R-squared:  0.01998,	Adjusted R-squared:  0.01995 
# F-statistic:   658 on 1 and 32272 DF,  p-value: < 2.2e-16
confint(mod.BlockOrd_lm)
boxplot(resid(mod.BlockOrd_lm) ~ BigTech_FinalExcludes_NoMow$PartID, xlab = "Participant", ylab = "RT", main = "ANOVA model: Residual RTs Predicted by Block Order Across Participants"); abline(0,0)
# Exploring residuals
as.data.frame(resid(mod.BlockOrd_lm)) -> res
colnames(res)[1] <- "e"
plot(density(res$e))
qqnorm(res$e); qqline(res$e)
skewness(res$e) # 0.6286023
# homoscedasticity 
plot(mod.BlockOrd_lm)
# linearity
residPlot <- data.frame(fitted(mod.BlockOrd_lm), resid(mod.BlockOrd_lm))
names(residPlot) <- c("predicted", "residual")
# now create the scatterplot with a LOESS smoother added
ggplot(residPlot,
       aes(x = predicted,
           y = residual)) +
  theme_bw() +
  geom_point(shape = 1) +
  geom_smooth(method = "lm", se = F)
# Mixed Model #####
mod.BlockOrd_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + Block_Ord + (1|PartID), REML = F)
summary(mod.BlockOrd_lmer)
options(scipen = 4)
# Random effects:
# Groups   Name        Variance Std.Dev.
# PartID   (Intercept) 1238     35.18   
# Residual             7062     84.03   
# Number of obs: 32274, groups:  PartID, 59
# 
# Fixed effects:
#             Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   457.4763     4.6863    63.3049   97.62   <2e-16 ***
#   Block_Ord   -2.8319     0.1019 32215.2366  -27.79   <2e-16 ***

# Factored block order
mod.BlockOrd.f_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + Block_Ord.f + (1|PartID), REML = F)
summary(mod.BlockOrd.f_lmer)
# Peusdo R squared
as.data.frame(VarCorr(mod.BlockOrd.f_lmer)) -> vars.mod.BlockOrd.f_lmer
pseudoR2 <- 1 - ((vars.mod.BlockOrd.f_lmer[1,4] + vars.mod.BlockOrd.f_lmer[2,4]) / (vars.basemodel[1,4] + vars.basemodel[2,4]))
pseudoR2 # 0.02547444, 
confint(mod.BlockOrd_lmer)
# Exploring residuals
as.data.frame(resid(mod.BlockOrd.f_lmer)) -> res
colnames(res)[1] <- "e"
plot(density(res$e))
qqnorm(res$e); qqline(res$e)
skewness(res$e) # 0.7730038
# homoscedasticity 
plot(mod.BlockOrd.f_lmer)
# linearity
residPlot <- data.frame(fitted(mod.BlockOrd.f_lmer), resid(mod.BlockOrd.f_lmer))
names(residPlot) <- c("predicted", "residual")
# now create the scatterplot with a LOESS smoother added
ggplot(residPlot,
       aes(x = predicted,
           y = residual)) +
  theme_bw() +
  geom_point(shape = 1) +
  geom_smooth(method = "lm", se = F)
# Comparing Models ####
boxplot(resid(mod.BlockOrd.f_lmer) ~ BigTech_FinalExcludes_NoMow$PartID, xlab = "Participant", ylab = "RT", main = "Mixed Model: Residual RTs Predicted by Block Order Across Participants"); abline(0,0)

anova(mod.BlockOrd.f_lmer, mod.BlockOrd_lm)
#                   Df    AIC    BIC  logLik deviance  Chisq Chi Df
# mod.BlockOrd_lm      3 382828 382854 -191411   382822                             
#mod.BlockOrd.f_lmer  4 377894 377928 -188943   377886 4936.2      1  < 2.2e-16 ***
# Improved fit of lmer model
#######################################
##### Sound x Trial Condition interaction Oddball Analyses #####
# Descriptives ####
RTbySound_Trial <- ddply(BigTech_FinalExcludes_NoMow,
                         .(Sound, Code_Good),
                         summarise,
                         M.RT.ms = mean(RT.ms),
                         SD.RT.ms = sd(RT.ms),
                         n.RT.ms = length(RT.ms)) 
RTbySound_Trial
write.table(RTbySound_Trial, file = "RTbySound_Trial_72p.txt", sep = ",", quote = FALSE, row.names = F)
# Histogram
ggplot(BigTech_FinalExcludes_NoMow, aes(Sound, RT.ms, group = Code_Good, color = Code_Good)) +
  geom_boxplot(size = 2) +
  geom_hline(aes(yintercept = mean(BigTech_FinalExcludes_NoMow$RT.ms))) +
  theme(legend.position = "bottom")

ggplot(BigTech_FinalExcludes_NoMow, aes(x = Code_Good, y = RT.ms, color = Sound)) +
  geom_boxplot(shape = 1, size = 1, alpha = .8) +
  geom_smooth(aes(as.numeric(Code_Good), RT.ms, color = Sound), method = lm, se = F, show.legend = NA, linetype = "twodash", size = 1, fullrange = T) +
  scale_color_brewer(palette = "Set1") +
  geom_hline(yintercept = mean(BigTech_FinalExcludes_NoMow$RT.ms)) +
  labs(title = "Reaction Time Across Sound and Trial Type", x = "Trial Type", y = "RT (ms)", size = 12) +
  scale_y_continuous(breaks = seq(0, 1000, 100)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(angle = 0, color="blue",size=15, face=3)) +
  theme(axis.title = element_text(angle = 0, color="black", size=15, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  theme(legend.key = element_rect(fill = "white", colour = "black")) +
  theme(legend.text = element_text(size = 15, colour = "Black")) +
  theme(legend.title = element_text(face = "bold"))
# RT.ms ~ Sound * Code_Good plot   
RTMeans3 <- aggregate(data = BigTech_FinalExcludes_NoMow, RT.ms ~ Sound * Code_Good, mean)
RTMeans3 <- mutate(RTMeans3, Group = c(1:4))
RTMeans3 <- mutate(RTMeans3, TrialType = ifelse(Code_Good == "Frequent", 1, 2))
str(RTMeans3)
ggplot(BigTech_FinalExcludes_NoMow, aes(y=RT.ms, x=Sound)) +
  geom_jitter(aes(color=Code_Good),alpha = .5, size=.7, width = .2, fill = "black") +
  set.seed(69) +
  geom_point(data = RTMeans3, col = "black", size = 7, shape = 23, fill = RTMeans3$TrialType) +
  scale_colour_manual("Trial Type", values=c("red", "black")) +
  geom_line(data = RTMeans3, aes(y = RT.ms, group = Code_Good, linetype=Code_Good), alpha = .9, col = "black", size = 1) +
  scale_fill_manual(values=c("black", "red")) +
  geom_hline(yintercept = mean(RTMeans3$RT.ms)) +
  labs(title = "Change in RT Oddball between Sounds", x = "Trial Type", y = "RT (ms)", size = 12) +
  scale_y_continuous(limits = c(400, 525), breaks = seq(400, 525, 25)) +
  scale_x_discrete(labels = c("control" = "Control","phone" = "Phone")) +
  theme_classic(base_size = 14)+
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(angle = 0, color="blue",size=15, face=3)) +
  theme(axis.title = element_text(angle = 0, color="black", size=15, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  theme(legend.key = element_rect(fill = "white", colour = "black")) +
  theme(legend.text = element_text(size = 15, colour = "Black")) +
  theme(legend.title = element_text(face = "bold"))
# Overall Oddball effect for High and Low SAPS
# OddballEffect phone
M_RT_RareTrials_phone <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Sound == "phone" & BigTech_FinalExcludes_NoMow$Code_Good == "Rare"])
M_RT_FreqTrials_phone <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Sound == "phone" & BigTech_FinalExcludes_NoMow$Code_Good == "Frequent"])
M_RT_RareTrials_phone-M_RT_FreqTrials_phone # 51.55498
# OddballEffect control
M_RT_RareTrials_control <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Sound == "control" & BigTech_FinalExcludes_NoMow$Code_Good == "Rare"])
M_RT_FreqTrials_control <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Sound == "control" & BigTech_FinalExcludes_NoMow$Code_Good == "Frequent"])
M_RT_RareTrials_control-M_RT_FreqTrials_control # 51.79527

# Anova/regression model ####
mod.SoundOddball_lm <- lm(data = BigTech_FinalExcludes_NoMow, RT.ms ~ Sound * Code_Good)
summary(mod.SoundOddball_lm)
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                430.4574     0.7488 574.830  < 2e-16 ***
# Soundcontrol                -3.5973     1.0597  -3.395 0.000688 ***
# Code_GoodRare               51.5550     2.4809  20.780  < 2e-16 ***
# Soundcontrol:Code_GoodRare   0.2403     3.5333   0.068 0.945779    
# 
# Residual standard error: 90.81 on 32270 degrees of freedom
# Multiple R-squared:  0.02622,	Adjusted R-squared:  0.02613 
# F-statistic: 289.7 on 3 and 32270 DF,  p-value: < 2.2e-16
confint(mod.SoundOddball_lm)
boxplot(resid(mod.SoundOddball_lm) ~ BigTech_FinalExcludes_NoMow$PartID, xlab = "Participant", ylab = "RT", main = "Anova Model: Residual RTs Predicted by Sound and Trial Type Across Participants"); abline(0,0)
# Exploring residuals
as.data.frame(resid(mod.SoundOddball_lm)) -> res
colnames(res)[1] <- "e"
plot(density(res$e))
qqnorm(res$e); qqline(res$e)
skewness(res$e) # 0.5200602
# Mixed Model ####
mod.SoundOddball_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ Code_Good * Sound + (Code_Good + Sound|PartID), REML = F, control = control)
summary(mod.SoundOddball_lmer)
contrasts(BigTech_FinalExcludes_NoMow$Sound)
BigTech_FinalExcludes_NoMow$Sound <- relevel(BigTech_FinalExcludes_NoMow$Sound, ref="phone")
contrasts(BigTech_FinalExcludes_NoMow$Sound) = contr.treatment(2)
contrasts(BigTech_FinalExcludes_NoMow$Code_Good)

# Peusdo R squared
as.data.frame(VarCorr(mod.SoundOddball_lmer)) -> vars.mod.SoundOddball_lmer
pseudoR2 <- 1 - ((vars.mod.SoundOddball_lmer[1,4] + vars.mod.SoundOddball_lmer[2,4]) / (vars.basemodel[1,4] + vars.basemodel[2,4]))
pseudoR2 # 0.02620618, Mixed Model with Sound x Trial type interaction explains and additional 2.8% of variability in RT
confint(mod.SoundOddball_lmer)
# Exploring residuals
as.data.frame(resid(mod.SoundOddball_lmer)) -> res
colnames(res)[1] <- "e"
plot(density(res$e))
qqnorm(res$e); qqline(res$e)
skewness(res$e) # 
library(sjPlot)
plot_model(mod.SoundOddball_lmer, type = "eff", terms = c("Sound", "Code_Good"), 
           title = "RT predicted from Sounds and Trial Frequency", wrap.legend.title = 5,
           show.values = TRUE, show.p = TRUE) + 
  geom_smooth(method = lm) +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  geom_hline(yintercept = mean(BigTech_FinalExcludes_NoMow$RT.ms)) 
# Comparing Models ####
boxplot(resid(mod.SoundOddball_lmer) ~ BigTech_FinalExcludes_NoMow$PartID, xlab = "Participant", ylab = "RT", main = "Mixed Model: Residual RTs Predicted by Sound and Trial Type Across Participants"); abline(0,0)

anova(mod.SoundOddball_lmer, mod.SoundOddball_lm)
#                       Df    AIC    BIC  logLik deviance  Chisq Chi Df
# mod.SoundOddball_lm    5 382626 382668 -191308   382616                             
# mod.SoundOddball_lmer  6 377632 377682 -188810   377620 4996.4      1  < 2.2e-16 ***
# Improved fit of lmer model
##### Analyses: Sound x Trial oddball * block order #####
# Anova/regression model ####
mod.SoundOddball_BlockOrd_lm <- lm(data = BigTech_FinalExcludes_NoMow, RT.ms ~ Sound * Code_Good + Block_Ord)
summary(mod.SoundOddball_BlockOrd_lm)

# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                455.1607     1.1884 383.004  < 2e-16 ***
#   Soundcontrol                -3.6080     1.0483  -3.442 0.000579 ***
#   Code_GoodRare               52.2323     2.4544  21.281  < 2e-16 ***
#   Block_Ord                   -2.8960     0.1089 -26.584  < 2e-16 ***
#   Soundcontrol:Code_GoodRare   0.6815     3.4953   0.195 0.845412    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 89.83 on 32269 degrees of freedom
# Multiple R-squared:  0.04709,	Adjusted R-squared:  0.04698 
# F-statistic: 398.7 on 4 and 32269 DF,  p-value: < 2.2e-16
confint(mod.SoundOddball_BlockOrd_lm)
boxplot(resid(mod.SoundOddball_BlockOrd_lm) ~ BigTech_FinalExcludes_NoMow$PartID, xlab = "Participant", ylab = "RT", main = "Anova Model: Residual RTs Predicted by Sound and Trial Type Controlling for Block Order Across Participants"); abline(0,0)
# Exploring residuals
as.data.frame(resid(mod.SoundOddball_BlockOrd_lm)) -> res
colnames(res)[1] <- "e"
plot(density(res$e))
qqnorm(res$e); qqline(res$e)
skewness(res$e) # 0.5384023
# Mixed Model ####
mod.SoundOddball_BlockOrd_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ Sound * Code_Good * Block_Ord + (1|PartID), REML = F)
summary(mod.SoundOddball_BlockOrd_lmer)
# Random effects:
# Groups   Name        Variance Std.Dev.
# PartID   (Intercept) 1244     35.26   
# Residual             6827     82.62   
# Number of obs: 32274, groups:  PartID, 59
# 
# Fixed effects:
#                               Estimate  Std. Error          df t value Pr(>|t|)    
# (Intercept)                  455.17120     4.71948    64.57512  96.445  < 2e-16 ***
#   Soundcontrol                  -3.86315     0.96444 32215.27861  -4.006 0.000062 ***
#   Code_GoodRare                 53.08877     2.25875 32215.73219  23.504  < 2e-16 ***
#   Block_Ord                     -2.89543     0.10022 32215.22951 -28.892  < 2e-16 ***
#   Soundcontrol:Code_GoodRare     0.04116     3.21690 32215.82278   0.013     0.99       

# Peusdo R squared
as.data.frame(VarCorr(mod.SoundOddball_BlockOrd_lmer)) -> vars.mod.SoundOddball_BlockOrd_lmer
pseudoR2 <- 1 - ((vars.mod.SoundOddball_BlockOrd_lmer[1,4] + vars.mod.SoundOddball_BlockOrd_lmer[2,4]) / (vars.basemodel[1,4] + vars.basemodel[2,4]))
pseudoR2 # 0.04707501, Mixed Model with Sound x Trial type interaction controlling fro block order explains an additional 4.9% of variability in RT
# Exploring residuals
as.data.frame(resid(mod.SoundOddball_BlockOrd_lmer)) -> res
colnames(res)[1] <- "e"
plot(density(res$e))
qqnorm(res$e); qqline(res$e)
skewness(res$e) # 0.6775307
plot_model(mod.SoundOddball_BlockOrd_lmer, type = "eff", terms = c("Sound", "Code_Good"), 
           title = "RT predicted from Sounds and Trial Frequency Controlling for Block Order", wrap.legend.title = 5,
           show.values = TRUE, show.p = TRUE) + 
           geom_smooth(method = lm) +
           theme(legend.position = "bottom") +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  geom_hline(yintercept = mean(BigTech_FinalExcludes_NoMow$RT.ms)) 
# Comparing models #####
boxplot(resid(mod.SoundOddball_BlockOrd_lmer) ~ BigTech_FinalExcludes_NoMow$PartID, xlab = "Participant", ylab = "RT", main = "Mixed Model: Residual RTs Predicted by Sound and Trial Type Controlling for Block Order Across Participants"); abline(0,0)

anova(mod.SoundOddball_BlockOrd_lmer, mod.SoundOddball_BlockOrd_lm)
#                                 Df    AIC    BIC  logLik deviance  Chisq Chi Df
# mod.SoundOddball_BlockOrd_lm    6 381929 381979 -190958   381917                             
# mod.SoundOddball_BlockOrd_lmer  7 376810 376868 -188398   376796 5121.3      1  < 2.2e-16 ***
# Lower values across the board suggesting better model fit
######################################
###### Models including SAPS #########
# Mean center SAPS
BigTech_FinalExcludes_NoMow$SAPS.c <- BigTech_FinalExcludes_NoMow$SAPS_Ave - mean(BigTech_FinalExcludes_NoMow$SAPS_Ave)

aggregate(PartID ~ SAPS_Split, data=BigTech_FinalExcludes_NoMow,FUN=function(x){length(unique(x))})
# ICC of RT within High and Low Smartphone Addicted people
ICCbare(data = BigTech_FinalExcludes_NoMow, SAPS_Split, RT.ms) # 0.004612853
# ICC value indicates that a (very small) portion of variance in overall
# overall RT can be attributed to differences between High and Low Smartphone Addicted people
RTby_SAPS <- ddply(BigTech_FinalExcludes_NoMow,
                            .(SAPS_Split),
                            summarise,
                            M.RT.ms = mean(RT.ms),
                            SD.RT.ms = sd(RT.ms),
                            n.RT.ms = length(RT.ms)) 
RTby_SAPS
write.table(RTby_SAPS, file = "RTby_SAPS_72p.txt", sep = ",", quote = FALSE, row.names = F)
RTby_Code_PartID <- ddply(BigTech_FinalExcludes_NoMow,
                   .(Code_Good, PartID),
                   summarise,
                   M.RT.ms = mean(RT.ms),
                   SD.RT.ms = sd(RT.ms),
                   n.RT.ms = length(RT.ms)) 
RTby_Code_PartID

# Boxplot
ggplot(BigTech_FinalExcludes_NoMow, aes(SAPS_Ave, RT.ms, group = Sound)) +
  geom_jitter(size = 1) +
  geom_smooth(method = lm, se = T)
  geom_hline(aes(yintercept = mean(BigTech_FinalExcludes_NoMow$RT.ms))) +
  theme(legend.position = "bottom") 

ggplot(BigTech_FinalExcludes_NoMow) +
  theme_classic()+
  aes(x = SAPS_Ave, y = RT.ms, color = Sound) +
  geom_point(alpha = .3, size = .5) +
  geom_smooth(method = "lm")+
  scale_y_continuous(breaks = seq(300, 600, 50))
  
fun_mean <- function(x){return(data.frame(y=mean(x),label=mean(x,na.rm=T)))}
n_fun <- function(x){
  return(data.frame(y = 0.95*70,
                    label = length(x)))
} 

ggplot(BigTech_FinalExcludes_NoMow, aes(x = SAPS_Ave, y = RT.ms, color = Sound, group = Sound)) +
  geom_jitter(color="black", size=0.15, alpha=0.3, width = .05) +
  geom_smooth(method = lm, se = T) +
  geom_boxplot(aes(x = SAPS_Ave), width= .75, weight= 1, color= "black", fill= NA,
               alpha= 0.85, notch=TRUE, notchwidth = .25, varwidth = TRUE,
               show.legend = F) +
  stat_boxplot(geom ='errorbar', width = 0.6, show.legend = F, color = "black") +
  #stat_summary(fun.y=mean, geom="point", shape=20, size=10, color="green", fill="green", alpha = .9) +
  #stat_summary(fun.data = fun_mean, geom="text", vjust=-1.5, color="green", aes(label=round(..y.., digits=2))) +
  #stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5, color="black") +
  scale_color_brewer(palette = "Set1") +
  #geom_hline(yintercept = mean(BigTech_FinalExcludes_NoMow$RT.ms)) +
  labs(title = "Reaction Time Across SAPS", x = "Smartphone Addiction (SAPS)", y = "Reaction Time (ms)", size = 12) +
  scale_y_continuous(breaks = seq(300, 600, 50)) +
  theme(axis.text = element_text(angle = 0, color="black", size=15, face=3)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  #theme(legend.position = "bottom") +
  theme(axis.text = element_text(angle = 0, color="black",size=15, face=3)) +
  theme(axis.title = element_text(angle = 0, color="black", size=15, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  theme(legend.position="bottom") 
  
  #theme(legend.key = element_rect(fill = "white", colour = "black")) +
  #theme(legend.text = element_text(size = 15, colour = "Black")) +
  #theme(legend.title = element_text(face = "bold")) +



library(plotly)
ggplotly(plot)

mod.OverallRT_SAPS_lm <- lm(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + SAPS_Split)
summary(mod.OverallRT_SAPS_lm)
mod.OverallRT_SAPS_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + SAPS_Split + (1|PartID), REML = F)
summary(mod.OverallRT_SAPS_lmer)
confint(mod.OverallRT_SAPS_lmer)
as.data.frame(VarCorr(mod.OverallRT_SAPS_lmer)) -> vars.mod.OverallRT_SAPS_lmer
pseudoR2 <- 1 - ((vars.mod.OverallRT_SAPS_lmer[1,4] + vars.mod.OverallRT_SAPS_lmer[2,4]) / (vars.basemodel[1,4] + vars.basemodel[2,4]))
pseudoR2 # 0.0007030474
anova(mod.OverallRT_SAPS_lmer, mod.OverallRT_SAPS_lm)
# RT and SAPS #####
mod.OverallRT_SAPS_lm2 <- lm(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + SAPS.c)
summary(mod.OverallRT_SAPS_lm2)
contrasts(BigTech_FinalExcludes_NoMow$SAPS_Split)
mod.OverallRT_SAPS_lmer2 <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + SAPS.c + (1|PartID), REML = F, control = control)
summary(mod.OverallRT_SAPS_lmer2)
# confidence intervals
fixef(mod.OverallRT_SAPS_lmer2) + c(-2,2)*arm::se.fixef(mod.OverallRT_SAPS_lmer2)
confint(mod.OverallRT_SAPS_lmer2)

ICCest(x = PartID, y = RT.ms, data = BigTech_FinalExcludes_NoMow)
# ICC = .14 mlm is wise

# Checking model assumptions
# normality of residuals
# via a density plot
plot(density(resid(mod.OverallRT_SAPS_lmer2)))
# via a Q-Q plot
qqnorm(resid(mod.OverallRT_SAPS_lmer2)); qqline(resid(mod.OverallRT_SAPS_lmer2))
# homoscedasticity 
plot(mod.OverallRT_SAPS_lmer2)
# linearity
# first create a data frame of predicted and residual values
residPlot <- data.frame(fitted(mod.OverallRT_SAPS_lmer2), resid(mod.OverallRT_SAPS_lmer2))
names(residPlot) <- c("predicted", "residual")
# now create the scatterplot with a LOESS smoother added
ggplot(residPlot,
       aes(x = predicted,
           y = residual)) +
  theme_bw() +
  geom_point(shape = 1) +
  geom_smooth(method = loess, se = F)

# Pseudo R squared
# need a base model (no predictors except the intercept)
model.base <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ (1 | PartID))
summary(model.base)
# store variances for model0 and model7 in data frames
model.basevar <- as.data.frame(VarCorr(model.base))
mod.OverallRT_SAPS_lmer2var <- as.data.frame(VarCorr(mod.OverallRT_SAPS_lmer2))
pseudoRsq <- 1 - ((model.basevar[1,4] + model.basevar[2,4]) / (mod.OverallRT_SAPS_lmer2var[1,4] + mod.OverallRT_SAPS_lmer2var[2,4]))
# pseudo r squared = -0.002989668


anova(mod.OverallRT_SAPS_lmer2)

# Cohen's d
(param_tab <- parameters::parameters(mod.OverallRT_SAPS_lmer2))
effectsize::t_to_d(param_tab$t[2], param_tab$df[2], pooled = TRUE)
#effectsize::t_to_d(.94, 68, pooled = TRUE)
#psych::cohen.d(BigTech_FinalExcludes_NoMow$RT.ms, BigTech_FinalExcludes_NoMow$SAPS_Split)
#t2d(0.94,n=69,n2=33,n1=36)
#m2t(435.15,427.21,93.07,89.67,n1=36,n2=33,n=69,pooled=TRUE)

boxplot(resid(mod.OverallRT_SAPS_lm2) ~ BigTech_FinalExcludes_NoMow$PartID, xlab = "Participant", ylab = "RT", main = "ANOVA Model: Residual RTs predicted by the Interaction Between Trial Type and SAPS across Participants"); abline(0,0)
boxplot(resid(mod.OverallRT_SAPS_lmer2) ~ BigTech_FinalExcludes_NoMow$PartID, xlab = "Participant", ylab = "RT", main = "Mixed Model: Residual RTs Predicted by the Interaction Between Trial Type and SAPS Across Participants"); abline(0,0)
library(ez)
ez::ezStats(BigTech_FinalExcludes_NoMow, dv = RT.ms, wid = PartID, between = SAPS.c, type = 3)
# Descritptives #####
ddply(BigTech_NoMow_outliersremoved,
                            .(Code_Good, SAPS_Split),
                            summarise,
                            M.RT.ms = mean(RT.ms),
                            SD.RT.ms = sd(RT.ms),
                            n.Parts = length(PartID)) 

##### Analyses: Trial Type (Overall Oddball) * SAPS #####
# Descritpives #####
RTbyTrialType_SAPS <- ddply(BigTech_FinalExcludes_NoMow,
                            .(Code_Good, SAPS_Split),
                            summarise,
                            M.RT.ms = mean(RT.ms),
                            SD.RT.ms = sd(RT.ms),
                            n.RT.ms = length(RT.ms)) 
RTbyTrialType_SAPS
# OddballEffect SAPs_High
RTbyTrialType_SAPS$RareHighSAPS <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"])
RTbyTrialType_SAPS$FrequentHighSAPS <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"])
RTbyTrialType_SAPS$Oddball.MHighSAPS <- RTbyTrialType_SAPS$RareHighSAPS-RTbyTrialType_SAPS$FrequentHighSAPS 
RTbyTrialType_SAPS$RareHighSAPS_SD <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"])
RTbyTrialType_SAPS$FrequentHighSAPS_SD <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"])
RTbyTrialType_SAPS$Oddball.SDHighSAPS <- RTbyTrialType_SAPS$RareHighSAPS_SD-RTbyTrialType_SAPS$FrequentHighSAPS_SD 
# OddballEffect SAPs_Low
RTbyTrialType_SAPS$RareLowSAPS <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"])
RTbyTrialType_SAPS$FrequentLowSAPS <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"])
RTbyTrialType_SAPS$Oddball.MLowSAPS <- RTbyTrialType_SAPS$RareLowSAPS-RTbyTrialType_SAPS$FrequentLowSAPS
RTbyTrialType_SAPS$RareLowSAPS_SD <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"])
RTbyTrialType_SAPS$FrequentLowSAPS_SD <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"])
RTbyTrialType_SAPS$Oddball.SDLowSAPS <- RTbyTrialType_SAPS$RareLowSAPS_SD-RTbyTrialType_SAPS$FrequentLowSAPS_SD 
RTbyTrialType_SAPS
#   Code_Good SAPS_Split  M.RT.ms  SD.RT.ms n.RT.ms
# 1  Frequent  High SAPS 431.6658  90.30535   14920
# 2  Frequent   Low SAPS 425.5588  85.37844   14450
# 3      Rare  High SAPS 478.2208 118.50677    1474
# 4      Rare   Low SAPS 482.5636 112.97469    1430
write.table(RTbyTrialType_SAPS, file = "RTbyTrialType_SAPS_72p.txt", sep = ",", quote = FALSE, row.names = F)

# Boxplot
ggplot(BigTech_FinalExcludes_NoMow, aes(Code_Good, RT.ms, group = SAPS_Split, color = SAPS_Split)) +
  geom_boxplot(size = 2) +
  geom_hline(aes(yintercept = mean(BigTech_FinalExcludes_NoMow$RT.ms))) +
  theme(legend.position = "bottom") 

ggplot(BigTech_FinalExcludes_NoMow, aes(x = Code_Good, y = RT.ms, color = SAPS_Split)) +
geom_boxplot(width= 1.75,
               weight= 1,
               color="blue",
               fill="blue",
               alpha=0.2,
               notch=TRUE,
               notchwidth = 0.5) +
  geom_smooth(aes(as.numeric(Code_Good), RT.ms, color = SAPS_Split), method = lm, se = T, 
              show.legend = NA, linetype = "dotdash", alpha = .8, size = 3, fullrange = T) +
  scale_color_brewer(palette = "Set1") +
  geom_hline(yintercept = mean(BigTech_FinalExcludes_NoMow$RT.ms)) +
  labs(title = "Reaction Time Across Trial Type for SAPS", x = "Trial Type", y = "RT (ms)", size = 12) +
  scale_y_continuous(limits = c(300, 600), breaks = seq(300, 600, 50)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(angle = 0, color="blue",size=15, face=3)) +
  theme(axis.title = element_text(angle = 0, color="black", size=15, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) 

# Overall Oddball effect for High and Low SAPS
# OddballEffect SAPs_High
M_RT_RareTrials_SAPS_High <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"])
M_RT_FreqTrials_SAPS_High <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"])
M_RT_RareTrials_SAPS_High-M_RT_FreqTrials_SAPS_High # 47.85138
# OddballEffect SAPs_Low
M_RT_RareTrials_SAPS_Low <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"])
M_RT_FreqTrials_SAPS_Low <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"])
M_RT_RareTrials_SAPS_Low-M_RT_FreqTrials_SAPS_Low # 57.78361
474.6492-422.6878
# Anova/regression model #####
mod.OverallOddball_SAPS_lm <- lm(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + Code_Good * SAPS_Split)
options(scipen = 5)
summary(mod.OverallOddball_SAPS_lm)
contrasts(BigTech_FinalExcludes_NoMow$Code_Good) = contr.helmert(2)
contrasts(BigTech_FinalExcludes_NoMow$Code_Good)
library(car)
Anova(mod.OverallOddball_SAPS_lm,type=2)
contrasts(BigTech_FinalExcludes_NoMow$SAPS_Split)
confint(mod.OverallOddball_SAPS_lm)
# Coefficients:
#                                 Estimate Std. Error t value      Pr(>|t|)    
# (Intercept)                      431.6658     0.7432 580.843       < 2e-16 ***
#   Code_GoodRare                     46.5550     2.4785  18.784       < 2e-16 ***
#   SAPS_SplitLow SAPS                -6.1070     1.0595  -5.764 0.00000000829 ***
#   Code_GoodRare:SAPS_SplitLow SAPS  10.4498     3.5321   2.959       0.00309 ** 
# 
# Residual standard error: 90.78 on 32270 degrees of freedom
# Multiple R-squared:  0.0269,	Adjusted R-squared:  0.02681 
# F-statistic: 297.3 on 3 and 32270 DF,  p-value: < 2.2e-16
modelEffectSizes(mod.OverallOddball_SAPS_lm)
confint(mod.OverallOddball_SAPS_lm)
# Visualizing non independence of data within participants 
boxplot(resid(mod.OverallOddball_SAPS_lm) ~ BigTech_FinalExcludes_NoMow$PartID, xlab = "Participant", ylab = "RT", main = "ANOVA Model: Residual RTs predicted by the Interaction Between Trial Type and SAPS across Participants"); abline(0,0)
# Exploring residuals
as.data.frame(resid(mod.OverallOddball_SAPS_lm)) -> res
# renaming the residuals column
colnames(res)[1] <- "e"
# looking at residuals
skewness(res$e) # 0.5169473
# All suggesting normal distribution of RT residuals for Frequent and Rare Stimuli

# Mixed model to account for variance in RT within partiicpants #####
mod.OverallOddball_SAPS_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ Code_Good * SAPS.c + (Code_Good|PartID), REML = F, control = control)
summary(mod.OverallOddball_SAPS_lmer)
# BigTech_FinalExcludes_NoMow <- BigTech_FinalExcludes_NoMow %>% 
#   mutate(oddballeffect = RT.ms[Code_Good == "Rare"] - RT.ms[Code_Good == "Frequent"])
contrasts(BigTech_FinalExcludes_NoMow$Code_Good)
fixef(mod.OverallOddball_SAPS_lmer) + c(-2,2)*arm::se.fixef(mod.OverallOddball_SAPS_lmer)
confint(mod.OverallOddball_SAPS_lmer)
# Cohen's d and conf intervals
(param_tab <- parameters::parameters(mod.OverallOddball_SAPS_lmer))
effectsize::t_to_d(param_tab$t[3:4], param_tab$df[3:4], pooled = TRUE)
# Normality
qqnorm(resid(mod.OverallOddball_SAPS_lmer)); qqline(resid(mod.OverallOddball_SAPS_lmer))
# homoscedasticity 
plot(mod.OverallOddball_SAPS_lmer)
contrasts(BigTech_FinalExcludes_NoMow$Code_Good)
BigTech_FinalExcludes_NoMow$Code_Good <- relevel(BigTech_FinalExcludes_NoMow$Code_Good, ref="Rare")
BigTech_FinalExcludes_NoMow$Code_Good <- relevel(BigTech_FinalExcludes_NoMow$Code_Good, ref="Frequent")
BigTech_FinalExcludes_NoMow$SAPS_Split <- relevel(BigTech_FinalExcludes_NoMow$SAPS_Split, ref="Low SAPS")
contrasts(BigTech_FinalExcludes_NoMow$Code_Good) = contr.treatment(2)
contrasts(BigTech_FinalExcludes_NoMow$Code_Good)
BigTech_FinalExcludes_NoMow$SAPS_Split <- relevel(BigTech_FinalExcludes_NoMow$SAPS_Split, ref="High SAPS")
contrasts(BigTech_FinalExcludes_NoMow$SAPS_Split) = contr.treatment(2)
contrasts(BigTech_FinalExcludes_NoMow$SAPS_Split)
contrasts(BigTech_FinalExcludes_NoMow$Code_Good) <- varContrasts(BigTech_FinalExcludes_NoMow$Code_Good, 
                                                          Type = "POC", 
                                                          POCList = list(c(-.5, .5)), 
                                                          Labels = c("Frequent", "Rare"))
BigTech_FinalExcludes_NoMow$contrastTrialType <- varRegressors(BigTech_FinalExcludes_NoMow, "Code_Good", c("Frequent", "Rare"))
contrasts(BigTech_FinalExcludes_NoMow$SAPS_Split) <- varContrasts(BigTech_FinalExcludes_NoMow$SAPS_Split, 
                                                           Type = "POC", 
                                                           POCList = list(c(-.5, .5)), 
                                                           Labels = c("Low SAPS", "High SAPS"))
BigTech_FinalExcludes_NoMow$contrastSAPS <- varRegressors(BigTech_FinalExcludes_NoMow, "SAPS_Split", c("Low SAPS", "High SAPS"))

# Random effects:
# Groups   Name        Variance Std.Dev.
# PartID   (Intercept) 1237     35.17   
# Residual             7005     83.70   
# Number of obs: 32274, groups:  PartID, 59
# 
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                        431.379      6.457    59.118  66.811  < 2e-16 ***
# Code_GoodRare                       47.455      2.286 32215.588  20.757  < 2e-16 ***
# SAPS_SplitLow SAPS                  -5.755      9.210    59.117  -0.625  0.53443    
# Code_GoodRare:SAPS_SplitLow SAPS     9.714      3.258 32215.609   2.981  0.00287 **   

# Model Effect Sizes
as.data.frame(VarCorr(mod.OverallOddball_SAPS_lmer)) -> vars.mod.OverallOddball_SAPS_lmer
pseudoR2 <- 1 - ((vars.mod.OverallOddball_SAPS_lmer[1,4] + vars.mod.OverallOddball_SAPS_lmer[2,4]) / (vars.basemodel[1,4] + vars.basemodel[2,4]))
pseudoR2 # 0.02679874
confint(mod.OverallOddball_SAPS_lmer)
# 2.7% of additional variance is explained by the mixed model compared to the intercept only model
BigTech_FinalExcludes_NoMow$SAPS_Split <- relevel(BigTech_FinalExcludes_NoMow$SAPS_Split, ref="Low SAPS")
summary(lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + Code_Good * SAPS_Split + (1|PartID), REML = F))
mod.OverallOddball_SAPS_lmer.2 <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + Code_Good * SAPS_Split + (1|PartID), REML = F)
as.data.frame(VarCorr(mod.OverallOddball_SAPS_lmer.2)) -> vars.mod.OverallOddball_SAPS_lmer.2
pseudoR2 <- 1 - ((vars.mod.OverallOddball_SAPS_lmer.2[1,4] + vars.mod.OverallOddball_SAPS_lmer.2[2,4]) / (vars.basemodel[1,4] + vars.basemodel[2,4]))
pseudoR2 # 0.02679874
confint(mod.OverallOddball_SAPS_lmer.2)
#Comparing models#####
boxplot(resid(mod.OverallOddball_SAPS_lmer) ~ BigTech_FinalExcludes_NoMow$PartID, xlab = "Participant", ylab = "RT", main = "Mixed Model: Residual RTs Predicted by the Interaction Between Trial Type and SAPS Across Participants"); abline(0,0)

anova(mod.OverallOddball_SAPS_lmer, mod.OverallOddball_SAPS_lm)
#                               Df    AIC    BIC  logLik deviance  Chisq Chi Df
# mod.OverallOddball_SAPS_lm    5 382604 382646 -191297   382594                             
# mod.OverallOddball_SAPS_lmer  6 377640 377690 -188814   377628 4965.8      1  < 2.2e-16 ***
# Improved fit of lmer model

##### RT differences across sounds * SAPS #####
# Descritpives #####
RTbySounds_SAPS <- ddply(BigTech_FinalExcludes_NoMow,
                         .(Sound, SAPS_Split),
                         summarise,
                         M.RT.ms = mean(RT.ms),
                         SD.RT.ms = sd(RT.ms),
                         n.RT.ms = length(RT.ms)) 
RTbySounds_SAPS
#     Sound SAPS_Split  M.RT.ms SD.RT.ms n.RT.ms
# 1   phone   Low SAPS 431.2715 89.92465    7986
# 2   phone  High SAPS 438.9391 94.90381    8193
# 3 control   Low SAPS 430.1060 89.49453    7894
# 4 control  High SAPS 432.7671 93.26074    8201
write.table(RTbySounds_SAPS, file = "RTbySounds_SAPS_72p.txt", sep = ",", quote = FALSE, row.names = F)
# plot####
ggplot(BigTech_FinalExcludes_NoMow, aes(x = Sound, y = RT.ms, color = SAPS_Split, group = Sound)) +
  geom_jitter(size=0.2, alpha=0.4, width = .4) +
  #stat_summary(fun.y=mean, geom="point", shape=20, size=10, color="green", fill="green", alpha = .9) +
  # geom_stripchart(n.text.params=list(size = 5), location.scale.text.params=list(size =5), test.text.params=list(size = 5), 
  #                 jitter.params = list(size=0, width = 0, alpha = 0), line.params = list(), 
  #                 location.params = list(), point.params = list(color = "blank"), errorbar.params = list(),test.text = TRUE, color = "black") +
  geom_boxplot(width= 1.75,weight=2, aes(color = BigTech_FinalExcludes_NoMow$SAPS_Split),
               alpha=0.2,notch=TRUE,notchwidth = 0.5) +
  geom_hline(yintercept = mean(BigTech_FinalExcludes_NoMow$RT.ms)) +
  labs(title = "Reaction Time Predicted by Sounds for High vs Low Smartphone Addiction", x = "Sound", y = "Reaction Time (ms)", size = 12) +
  scale_y_continuous(limits = c(200, 800), breaks = seq(200, 800, 100)) +
  theme_classic()+
  theme(axis.text = element_text(angle = 0, color="blue", size=15, face=3)) +
  theme(axis.title = element_text(angle = 0, color="black", size=15, face="bold")) +
  theme(plot.title = element_text(hjust = 0.5, size = 20))+
  theme(legend.position="bottom") 
  
plot
# Boxplot ####
ggplot(BigTech_FinalExcludes_NoMow, aes(x = Sound, y = RT.ms, color = SAPS_Split)) +
  geom_boxplot(width= 1.75,
               weight= 1,
               color="blue",
               fill="blue",
               alpha=0.2,
               notch=TRUE,
               notchwidth = 0.5) +
  geom_smooth(aes(as.numeric(Sound), RT.ms, color = SAPS_Split), method = lm, se = T, 
              show.legend = NA, linetype = "dotdash", alpha = .8, size = 3, fullrange = T) +
  scale_color_brewer(palette = "Set1") +
  geom_hline(yintercept = mean(BigTech_FinalExcludes_NoMow$RT.ms)) +
  labs(title = "Reaction Time Across Sounds for SAPS", x = "Trial Type", y = "RT (ms)", size = 12) +
  scale_y_continuous(limits = c(300, 600), breaks = seq(300, 600, 50)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(angle = 0, color="blue",size=15, face=3)) +
  theme(axis.title = element_text(angle = 0, color="black", size=15, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5, size = 20))
# ANOVA/regression model #####
BigTech_FinalExcludes_NoMow$Sound <- relevel(BigTech_FinalExcludes_NoMow$Sound, ref="phone")
mod.RTSounds_SAPS_lm <- lm(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + Sound * SAPS_Split)
summary(mod.RTSounds_SAPS_lm)
# Coefficients:
#                                 Estimate Std. Error t value Pr(>|t|)    
# SoundphoneHighSAPS              438.9391     1.0177 431.294     < 2e-16 ***
# Soundcontrol                     -6.1720     1.4389  -4.289 0.000017959 ***
# Soundmower                        0.3195     1.4350   0.223      0.8238    
# SAPS_SplitLow SAPS               -7.6676     1.4486  -5.293 0.000000121 ***
# Soundcontrol:SAPS_SplitLow SAPS   5.0065     2.0514   2.441      0.0147 *  
# Soundmower:SAPS_SplitLow SAPS     4.5430     2.0437   2.223      0.0262 *  

# Residual standard error: 92.12 on 48605 degrees of freedom
# Multiple R-squared:  0.001517,	Adjusted R-squared:  0.001415 
# F-statistic: 14.77 on 5 and 48605 DF,  p-value: 1.653e-14

# Visualizing non independence of data within participants 
boxplot(resid(mod.RTSounds_SAPS_lm) ~ BigTech_FinalExcludes_NoMow$PartID, xlab = "Participant", ylab = "RT", main = "ANOVA Model: Residual RTs Predicted by Interaction between Sounds and SAPS Across Participants"); abline(0,0)

# Exploring residuals
as.data.frame(resid(mod.RTSounds_SAPS_lm)) -> res
# renaming the residuals column
colnames(res)[1] <- "e"
# looking at residuals
skewness(res$e) # 0.5785889 
# Suggests minor deviation from normal distribution of RT residuals for Sound Stimuli

# Mixed Model #####
BigTech_FinalExcludes_NoMow$Sound <- relevel(BigTech_FinalExcludes_NoMow$Sound, ref="Phone")
BigTech_FinalExcludes_NoMow$Sound <- relevel(BigTech_FinalExcludes_NoMow$Sound, ref="Control")
BigTech_FinalExcludes_NoMow$SAPS_Split <- relevel(BigTech_FinalExcludes_NoMow$SAPS_Split, ref="High SAPS")
mod.RTbySounds_SAPS_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + Sound * SAPS_Ave + (1|PartID), REML = F)
contrasts(BigTech_FinalExcludes_NoMow$Sound) = varContrasts(BigTech_FinalExcludes_NoMow$Sound, 
                                                            Type = "POC", 
                                                            POCList = list(c(-.5, .5)), 
                                                            Labels = c("phone", "control"))
mod.RTbySounds_SAPS_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + Sound * SAPS_Ave + (Sound|PartID), REML = F, control = control)
summary(mod.RTbySounds_SAPS_lmer)
confint(mod.RTbySounds_SAPS_lmer)
# Cohen's d and conf intervals
(param_tab <- parameters::parameters(mod.RTbySounds_SAPS_lmer))
effectsize::t_to_d(param_tab$t[3:4], param_tab$df[3:4], pooled = TRUE)
fixef(mod.RTbySounds_SAPS_lmer) + c(-2,2)*arm::se.fixef(mod.RTbySounds_SAPS_lmer)

contrasts(BigTech_FinalExcludes_NoMow$SAPS_Split)
control <- lmerControl(check.nobs.vs.nRE="ignore", calc.derivs=FALSE)
# Summary
# Random effects:
# Groups   Name        Variance Std.Dev.
# PartID   (Intercept) 1232     35.11   
# Residual             7226     85.00   
# Number of obs: 32274, groups:  PartID, 59
# 
# Fixed effects:
#   Estimate Std. Error        df t value   Pr(>|t|)    
# (Intercept)                       438.843      6.478    60.259  67.743    < 2e-16 ***
#   Soundcontrol                       -6.393      1.328 32215.310  -4.814 0.00000149 ***
#   SAPS_SplitLow SAPS                 -7.311      9.240    60.249  -0.791     0.4319    
# Soundcontrol:SAPS_SplitLow SAPS     4.853      1.893 32215.253   2.564     0.0104 *    
BigTech_FinalExcludes_NoMow$SAPS_Split <- relevel(BigTech_FinalExcludes_NoMow$SAPS_Split, ref="Low SAPS")
summary(mod.RTbySounds_SAPS_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + Sound * SAPS_Split + (1|PartID), REML = F))

# Model Effect Sizes
as.data.frame(VarCorr(mod.RTbySounds_SAPS_lmer)) -> vars.mod.RTbySounds_SAPS_lmer
pseudoR2 <- 1 - ((vars.mod.RTbySounds_SAPS_lmer[1,4] + vars.mod.RTbySounds_SAPS_lmer[2,4]) / (vars.basemodel[1,4] + vars.basemodel[2,4]))
pseudoR2 # 0.0012928, Little variation exists between RT for sounds by SAPS interaction
confint(mod.RTbySounds_SAPS_lmer)
# Exploring residuals
as.data.frame(resid(mod.RTbySounds_SAPS_lmer)) -> res
colnames(res)[1] <- "e"
skewness(res$e) # 0.7569857

# Comparing Models ####
boxplot(resid(mod.RTbySounds_SAPS_lmer) ~ BigTech_FinalExcludes_NoMow$PartID, xlab = "Participant", ylab = "RT", main = "Mixed Model: Residual RTs Predicted by Interaction between Sounds and SAPS Across Participants"); abline(0,0)

anova(mod.RTbySounds_SAPS_lmer, mod.RTSounds_SAPS_lm)
#                           Df    AIC    BIC  logLik deviance  Chisq Chi Df
# mod.RTSounds_SAPS_lm      5 383439 383481 -191715   383429                             
# mod.RTbySounds_SAPS_lmer  6 378637 378687 -189312   378625 4804.8      1  < 2.2e-16 ***
# Improved fit of lmer model
# Interaction Plot ####
ggplot(BigTech_FinalExcludes_NoMow, aes(x = SAPS_Ave, y = RT.ms, linetype = Sound, color = "black")) +
  theme_bw() +
  #geom_point(data = RTMeansAll2, mapping = aes(x = alpha = .4, size = .55) +
  geom_smooth(method = lm, col = "black") +
  labs(x = "Smartphone Addiction Proneness (SAPS)", y = "RT (ms)") +
  theme_classic(base_size = 15) +
  #theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(angle = 0, color = "black", size = 35, face = 3)) +
  theme(axis.title = element_text(angle = 0, color = "black", size = 40, face = "bold")) +
  theme(axis.title.x = element_text(vjust = -.5)) +
  theme(axis.title.y = element_text(vjust = 1.8)) +
  #theme(plot.title = element_text(hjust = 0.5, size = 40)) +
  theme(legend.key = element_rect(fill = "white", colour = "black")) +
  theme(legend.text = element_text(size = 30, colour = "black")) +
  theme(legend.title = element_text(size = 35,face = "bold")) +
  theme(legend.direction = "vertical")+
  theme(legend.position = c(.8, .2)) +
  scale_colour_manual("Sound", values=c("black", "black"), labels = c("Control", "Smartphone"), guide = "none") +
  scale_linetype(aes("Sound"), labels = c("Control", "Smartphone")) +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"))
# ggsave("plot.png", width = 5, height = 5)
##### RT differences across blocks * SAPS #####
# Descritpives #####
RTbyBlockOrder_SAPS <- ddply(BigTech_FinalExcludes_NoMow,
                             .(Block_Ord, SAPS_Split),
                             summarise,
                             M.RT.ms = mean(RT.ms),
                             SD.RT.ms = sd(RT.ms),
                             n.RT.ms = length(RT.ms)) 
RTbyBlockOrder_SAPS
RTbyBlockOrder_SAPS <- ddply(BigTech_FinalExcludes_NoMow,
                             .(Halfway, SAPS_Split, Sound),
                             summarise,
                             M.RT.ms = mean(RT.ms),
                             SD.RT.ms = sd(RT.ms),
                             n.RT.ms = length(RT.ms)) 
RTbyBlockOrder_SAPS
# ANOVA/regression model #####
# Change in RT between blocks for High SAPS
mod.BlockOrd_SAPS_lm <- lm(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + Halfway * SAPS_Split)
summary(mod.BlockOrd_SAPS_lm)
contrasts(BigTech_FinalExcludes_NoMow$Halfway)
BigTech_FinalExcludes_NoMow$Halfway <- relevel(BigTech_FinalExcludes_NoMow$Halfway, ref="Second Half")

# Coefficients:
#                               Estimate Std. Error t value  Pr(>|t|)    
# Block0_SAPS High              457.9937     1.2256 373.690   < 2e-16 ***
# Block_Ord                     -2.4576     0.1263 -19.455   < 2e-16 ***
# SAPS_SplitLow SAPS             1.6859     1.7491   0.964     0.335    
# Block_Ord:SAPS_SplitLow SAPS  -0.7188     0.1803  -3.987 0.0000671 ***
# 
# Residual standard error: 91.24 on 48607 degrees of freedom
# Multiple R-squared:  0.02051,	Adjusted R-squared:  0.02044 
# F-statistic: 339.2 on 3 and 48607 DF,  p-value: < 2.2e-16
boxplot(resid(mod.BlockOrd_SAPS_lm) ~ BigTech_FinalExcludes_NoMow$PartID, xlab = "Participant", ylab = "RT", main = "ANOVA model: Residual RTs Predicted by Interaction between Block Order and SAPS Across Participants"); abline(0,0)
# Exploring residuals
as.data.frame(resid(mod.BlockOrd_SAPS_lm)) -> res
colnames(res)[1] <- "e"
skewness(res$e) # 0.5982561

# Change in RT between blocks for Low SAPS
BigTech_FinalExcludes_NoMow$SAPS_Split <- relevel(BigTech_FinalExcludes_NoMow$SAPS_Split, ref="Low SAPS")
contrasts(BigTech_FinalExcludes_NoMow$SAPS_Split)

mod.BlockOrd_Low_SAPS_lm <- lm(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + Block_Ord * SAPS_Split)
summary(mod.BlockOrd_Low_SAPS_lm)
# Coefficients:
#                               Estimate Std. Error t value  Pr(>|t|)    
# Block0_LowSAPS                459.6796     1.2479 368.358   < 2e-16 ***
# Block_Ord                      -3.1764     0.1286 -24.695   < 2e-16 ***
# SAPS_SplitHigh SAPS            -1.6859     1.7491  -0.964     0.335    
# Block_Ord:SAPS_SplitHigh SAPS   0.7188     0.1803   3.987 0.0000671 ***
# 
# Residual standard error: 91.24 on 48607 degrees of freedom
# Multiple R-squared:  0.02051,	Adjusted R-squared:  0.02044 
# F-statistic: 339.2 on 3 and 48607 DF,  p-value: < 2.2e-16

# Change in RT across all blocks for High SAPS at Block 16 #####
BigTech_FinalExcludes_NoMow$SAPS_Split <- relevel(BigTech_FinalExcludes_NoMow$SAPS_Split, ref="High SAPS")
contrasts(BigTech_FinalExcludes_NoMow$SAPS_Split)
contrasts(BigTech_FinalExcludes_NoMow$Block_Ord.f)
BigTech_FinalExcludes_NoMow$Block_Ord.f.2 <- relevel(BigTech_FinalExcludes_NoMow$Block_Ord.f, ref= "16")

mod.BlockOrd.f.2_Low_SAPS_lm <- lm(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + Block_Ord.f.2 * SAPS_Split)

summary(mod.BlockOrd.f.2_Low_SAPS_lm)

# Output 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         434.631      2.312 188.015  < 2e-16 ***
# Block_Ord.f.21                       26.721      3.308   8.079 6.71e-16 ***
# Block_Ord.f.22                       21.284      3.268   6.513 7.46e-11 ***
# Block_Ord.f.23                       16.523      3.284   5.031 4.89e-07 ***
# Block_Ord.f.24                        6.526      3.302   1.976 0.048113 *  
# Block_Ord.f.25                       16.909      3.275   5.164 2.43e-07 ***
# Block_Ord.f.26                        1.250      3.256   0.384 0.700931    
# Block_Ord.f.27                       15.336      3.277   4.680 2.88e-06 ***
# Block_Ord.f.28                       -8.810      3.262  -2.701 0.006917 ** 
# Block_Ord.f.29                        4.327      3.255   1.329 0.183774    
# Block_Ord.f.210                      -1.713      3.279  -0.522 0.601345    
# Block_Ord.f.211                       5.579      3.271   1.706 0.088080 .  
# Block_Ord.f.212                     -19.520      3.264  -5.980 2.24e-09 ***
# Block_Ord.f.213                     -19.603      3.264  -6.005 1.93e-09 ***
# Block_Ord.f.214                      -7.559      3.269  -2.312 0.020759 *  
# Block_Ord.f.215                     -17.183      3.261  -5.269 1.37e-07 ***
# SAPS_SplitLow SAPS                  -22.895      3.316  -6.904 5.13e-12 ***
# Block_Ord.f.21:SAPS_SplitLow SAPS    26.486      4.735   5.594 2.23e-08 ***
# Block_Ord.f.22:SAPS_SplitLow SAPS    20.510      4.680   4.383 1.17e-05 ***
# Block_Ord.f.23:SAPS_SplitLow SAPS    23.183      4.708   4.924 8.51e-07 ***
# Block_Ord.f.24:SAPS_SplitLow SAPS    17.720      4.704   3.767 0.000165 ***
# Block_Ord.f.25:SAPS_SplitLow SAPS    18.088      4.687   3.859 0.000114 ***
# Block_Ord.f.26:SAPS_SplitLow SAPS    27.911      4.659   5.991 2.10e-09 ***
# Block_Ord.f.27:SAPS_SplitLow SAPS    16.513      4.680   3.528 0.000418 ***
# Block_Ord.f.28:SAPS_SplitLow SAPS    17.830      4.666   3.821 0.000133 ***
# Block_Ord.f.29:SAPS_SplitLow SAPS    15.528      4.654   3.337 0.000848 ***
# Block_Ord.f.210:SAPS_SplitLow SAPS   19.239      4.686   4.105 4.04e-05 ***
# Block_Ord.f.211:SAPS_SplitLow SAPS   10.389      4.674   2.222 0.026255 *  
# Block_Ord.f.212:SAPS_SplitLow SAPS   25.574      4.663   5.484 4.18e-08 ***
# Block_Ord.f.213:SAPS_SplitLow SAPS   21.344      4.663   4.578 4.71e-06 ***
# Block_Ord.f.214:SAPS_SplitLow SAPS   18.443      4.674   3.946 7.96e-05 ***
# Block_Ord.f.215:SAPS_SplitLow SAPS   16.981      4.658   3.646 0.000267 ***
# 
# Residual standard error: 90.98 on 48579 degrees of freedom
# Multiple R-squared:  0.02655,	Adjusted R-squared:  0.02593 
# F-statistic: 42.74 on 31 and 48579 DF,  p-value: < 2.2e-16


# Mixed Model #####
mod.BlockOrd_SAPS_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + Block_Ord * SAPS_Split + (1|PartID), REML = F)
mod.BlockOrd_SAPS_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + Halfway * SAPS_Split + (1|PartID), REML = F)
mod.BlockOrd_SAPS_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + Quarters * SAPS_Split + (1|PartID), REML = F)
summary(mod.BlockOrd_SAPS_lmer)
BigTech_FinalExcludes_NoMow$Halfway <- relevel(BigTech_FinalExcludes_NoMow$Halfway, ref="First Half")

BigTech_FinalExcludes_NoMow$SAPS_Split <- relevel(BigTech_FinalExcludes_NoMow$SAPS_Split, ref="Low SAPS")

contrasts(BigTech_FinalExcludes_NoMow$SAPS_Split)
contrasts(BigTech_FinalExcludes_NoMow$Block_Ord.f) = contr.sum(16)
contrasts(BigTech_FinalExcludes_NoMow$Halfway)
# Random effects:
# Groups   Name        Variance Std.Dev.
# PartID   (Intercept) 1232     35.10   
# Residual             7060     84.02   
# Number of obs: 32274, groups:  PartID, 59
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                    457.3382     6.5575    63.3368  69.743  < 2e-16 ***
#   Block_Ord                       -2.5347     0.1430 32215.2076 -17.719  < 2e-16 ***
#   SAPS_SplitLow SAPS               0.2677     9.3528    63.3225   0.029  0.97725    
# Block_Ord:SAPS_SplitLow SAPS    -0.6034     0.2038 32215.2377  -2.961  0.00307 **
# Peusdo R squared
as.data.frame(VarCorr(mod.BlockOrd_lmer)) -> vars.mod.BlockOrd_lmer
pseudoR2 <- 1 - ((vars.mod.BlockOrd_lmer[1,4] + vars.mod.BlockOrd_lmer[2,4]) / (vars.basemodel[1,4] + vars.basemodel[2,4]))
pseudoR2 # 0.01998192, Block order and SAPS interaction explains an additional 1.9% of variability in RT
confint(mod.BlockOrd_lmer)
# Exploring residuals
as.data.frame(resid(mod.BlockOrd_lmer)) -> res
colnames(res)[1] <- "e"
skewness(res$e) # 0.7437841
# Comparing Models ####
boxplot(resid(mod.BlockOrd_SAPS_lmer) ~ BigTech_FinalExcludes_NoMow$PartID, xlab = "Participant", ylab = "RT", main = "Mixed Model: Residual RTs Predicted by Interaction Between Block Order and SAPS Across Participants"); abline(0,0)

anova(mod.BlockOrd_SAPS_lmer, mod.BlockOrd_SAPS_lm)
#                   Df    AIC    BIC  logLik deviance  Chisq Chi Df
# mod.BlockOrd_SAPS_lm    5 576766 576810 -288378   576756                             
# mod.BlockOrd_SAPS_lmer  6 569082 569135 -284535   569070 7686.2      1  < 2.2e-16 ***
# Suggests Improved fit of lmer model
##### Model SAPS halfway sound####
# Mixed Model #####
mod.BlockOrd_SAPS_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + Block_Ord * SAPS_Split + (1|PartID), REML = F)
mod.BlockOrd_SAPS_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ 1 + Sound * SAPS_Split * Halfway + (1|PartID), REML = F)
summary(mod.BlockOrd_SAPS_lmer)
BigTech_FinalExcludes_NoMow$Halfway <- relevel(BigTech_FinalExcludes_NoMow$Halfway, ref="First Half")
BigTech_FinalExcludes_NoMow$SAPS_Split <- relevel(BigTech_FinalExcludes_NoMow$SAPS_Split, ref="Low SAPS")
contrasts(BigTech_FinalExcludes_NoMow$SAPS_Split)
contrasts(BigTech_FinalExcludes_NoMow$Block_Ord.f) = contr.sum(16)
contrasts(BigTech_FinalExcludes_NoMow$Halfway)
contrasts(BigTech_FinalExcludes_NoMow$Sound)
BigTech_FinalExcludes_NoMow$Sound <- relevel(BigTech_FinalExcludes_NoMow$Sound, ref="control")



##### Analyses: Sound * Trial Type (Oddball) * SAPS interaction  #####
# Descriptives ####
RTbySound_Trial_SAPSSplit <- ddply(BigTech_FinalExcludes_NoMow,
                                   .(Sound, Code_Good, SAPS_Split),
                                   summarise,
                                   M.RT.ms = mean(RT.ms),
                                   SD.RT.ms = sd(RT.ms),
                                   n.RT.ms = length(RT.ms)) 
RTbySound_Trial_SAPSSplit
# OddballEffect SAPs_High phone
RTbySound_Trial_SAPSSplit$RareHighSAPS_phone <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS" & BigTech_FinalExcludes_NoMow$Sound == "phone"])
RTbySound_Trial_SAPSSplit$FrequentHighSAPS_phone <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"& BigTech_FinalExcludes_NoMow$Sound == "phone"])
RTbySound_Trial_SAPSSplit$Oddball.MHighSAPS_phone <- RTbySound_Trial_SAPSSplit$RareHighSAPS_phone-RTbySound_Trial_SAPSSplit$FrequentHighSAPS_phone 
RTbySound_Trial_SAPSSplit$RareHighSAPS_SD_phone <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"& BigTech_FinalExcludes_NoMow$Sound == "phone"])
RTbySound_Trial_SAPSSplit$FrequentHighSAPS_SD_phone <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"& BigTech_FinalExcludes_NoMow$Sound == "phone"])
RTbySound_Trial_SAPSSplit$Oddball.SDHighSAPS_phone <- RTbySound_Trial_SAPSSplit$RareHighSAPS_SD_phone-RTbySound_Trial_SAPSSplit$FrequentHighSAPS_SD_phone 
# OddballEffect SAPs_Low phone
RTbySound_Trial_SAPSSplit$RareLowSAPS_phone <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Sound == "phone"])
RTbySound_Trial_SAPSSplit$FrequentLowSAPS_phone <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Sound == "phone"])
RTbySound_Trial_SAPSSplit$Oddball.MLowSAPS_phone <- RTbySound_Trial_SAPSSplit$RareLowSAPS_phone-RTbySound_Trial_SAPSSplit$FrequentLowSAPS_phone
RTbySound_Trial_SAPSSplit$RareLowSAPS_SD_phone <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Sound == "phone"])
RTbySound_Trial_SAPSSplit$FrequentLowSAPS_SD_phone <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Sound == "phone"])
RTbySound_Trial_SAPSSplit$Oddball.SDLowSAPS_phone <- RTbySound_Trial_SAPSSplit$RareLowSAPS_SD_phone-RTbySound_Trial_SAPSSplit$FrequentLowSAPS_SD_phone 
RTbySound_Trial_SAPSSplit
# OddballEffect SAPs_High control
RTbySound_Trial_SAPSSplit$RareHighSAPS_control <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS" & BigTech_FinalExcludes_NoMow$Sound == "control"])
RTbySound_Trial_SAPSSplit$FrequentHighSAPS_control <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"& BigTech_FinalExcludes_NoMow$Sound == "control"])
RTbySound_Trial_SAPSSplit$Oddball.MHighSAPS_control <- RTbySound_Trial_SAPSSplit$RareHighSAPS_control-RTbySound_Trial_SAPSSplit$FrequentHighSAPS_control 
RTbySound_Trial_SAPSSplit$RareHighSAPS_SD_control <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"& BigTech_FinalExcludes_NoMow$Sound == "control"])
RTbySound_Trial_SAPSSplit$FrequentHighSAPS_SD_control <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"& BigTech_FinalExcludes_NoMow$Sound == "control"])
RTbySound_Trial_SAPSSplit$Oddball.SDHighSAPS_control <- RTbySound_Trial_SAPSSplit$RareHighSAPS_SD_control-RTbySound_Trial_SAPSSplit$FrequentHighSAPS_SD_control 
# OddballEffect SAPs_Low control
RTbySound_Trial_SAPSSplit$RareLowSAPS_control <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Sound == "control"])
RTbySound_Trial_SAPSSplit$FrequentLowSAPS_control <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Sound == "control"])
RTbySound_Trial_SAPSSplit$Oddball.MLowSAPS_control <- RTbySound_Trial_SAPSSplit$RareLowSAPS_control-RTbySound_Trial_SAPSSplit$FrequentLowSAPS_control
RTbySound_Trial_SAPSSplit$RareLowSAPS_SD_control <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Sound == "control"])
RTbySound_Trial_SAPSSplit$FrequentLowSAPS_SD_control <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Sound == "control"])
RTbySound_Trial_SAPSSplit$Oddball.SDLowSAPS_control <- RTbySound_Trial_SAPSSplit$RareLowSAPS_SD_control-RTbySound_Trial_SAPSSplit$FrequentLowSAPS_SD_control 
RTbySound_Trial_SAPSSplit
# Anova/regression model ####
contrasts(BigTech_FinalExcludes_NoMow$Sound)
BigTech_FinalExcludes_NoMow$Sound <- relevel(BigTech_FinalExcludes_NoMow$Sound, ref="phone")
contrasts(BigTech_FinalExcludes_NoMow$Sound)
mod.SoundOddball_SAPS_lm <- lm(data = BigTech_FinalExcludes_NoMow, RT.ms ~ Sound * Code_Good * SAPS_Split)
summary(mod.SoundOddball_SAPS_lm)
# Coefficients:
#                                               Estimate Std. Error t value      Pr(>|t|)    
# SoundPhone_Frequent_HighSAPS                 434.7100     1.0543 412.333       < 2e-16 ***
# Soundmower                                    -0.2136     1.4871  -0.144       0.88577    
# Soundsquare                                   -6.0608     1.4876  -4.074 0.00004622783 ***
# Code_GoodRare                                 45.1746     3.4457  13.110       < 2e-16 ***
# SAPS_SplitLow SAPS                            -8.5912     1.4985  -5.733 0.00000000991 ***
# Soundmower:Code_GoodRare                       4.9956     4.8436   1.031       0.30237    
# Soundsquare:Code_GoodRare                      2.5919     4.9647   0.522       0.60163    
# Soundmower:SAPS_SplitLow SAPS                  4.6078     2.1164   2.177       0.02947 *  
# Soundsquare:SAPS_SplitLow SAPS                 4.9323     2.1208   2.326       0.02004 *  
# Code_GoodRare:SAPS_SplitLow SAPS              13.0272     4.9680   2.622       0.00874 ** 
# Soundmower:Code_GoodRare:SAPS_SplitLow SAPS   -4.1557     6.9410  -0.599       0.54936    
# Soundsquare:Code_GoodRare:SAPS_SplitLow SAPS  -4.9386     7.0728  -0.698       0.48502    
# 
# Residual standard error: 90.85 on 48599 degrees of freedom
# Multiple R-squared:  0.02895,	Adjusted R-squared:  0.02873 
# F-statistic: 131.7 on 11 and 48599 DF,  p-value: < 2.2e-16
boxplot(resid(mod.SoundOddball_SAPS_lm) ~ BigTech_FinalExcludes_NoMow$PartID, xlab = "Participant", ylab = "RT", main = "Anova Model: Residual RTs Predicted by Interaction Between Sound x Trial Type x SAPS Across Participants"); abline(0,0)
# Exploring residuals
as.data.frame(resid(mod.SoundOddball_SAPS_lm)) -> res
colnames(res)[1] <- "e"
skewness(res$e) # 0.4907791
# Mixed Model ####
mod.SoundOddball_SAPS_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ Code_Good * Sound * SAPS.c + (Code_Good+Sound|PartID), REML = F, control = control)

# Cohen's d and conf intervals
parameters::model_parameters(mod.SoundOddball_SAPS_lmer)
effectsize::t_to_d(param_tab$t[6], param_tab$df[6], pooled = TRUE)
fixef(mod.SoundOddball_SAPS_lmer) + c(-2,2)*arm::se.fixef(mod.SoundOddball_SAPS_lmer)
lme4::confint.merMod(mod.SoundOddball_SAPS_lmer)
confint.mlm <- function (model, level = 0.95) {
  beta <- coef(model)
  se <- std_mlm(model)
  alpha <- qt((1 - level) / 2, df = model$df.residual)
  list(lower = beta + alpha * se, upper = beta - alpha * se)
}

## call "confint"
confint.lm(mod)
confint.lm(mod.SoundOddball_SAPS_lmer)
contrasts(BigTech_FinalExcludes_NoMow$Code_Good) 
contrasts(BigTech_FinalExcludes_NoMow$Sound) = contr.treatment(2)
contrasts(BigTech_FinalExcludes_NoMow$Sound)
contrasts(BigTech_FinalExcludes_NoMow$SAPS_Split) 
BigTech_FinalExcludes_NoMow$Sound <- relevel(BigTech_FinalExcludes_NoMow$Sound, ref="Control")
BigTech_FinalExcludes_NoMow$Sound <- relevel(BigTech_FinalExcludes_NoMow$Sound, ref="Phone")
BigTech_FinalExcludes_NoMow$Code_Good <- relevel(BigTech_FinalExcludes_NoMow$Code_Good, ref="Rare")
BigTech_FinalExcludes_NoMow$Code_Good <- relevel(BigTech_FinalExcludes_NoMow$Code_Good, ref="Frequent")

# Peusdo R squared
as.data.frame(VarCorr(mod.SoundOddball_SAPS_lmer)) -> vars.mod.SoundOddball_SAPS_lmer
pseudoR2 <- 1 - ((vars.mod.SoundOddball_SAPS_lmer[1,4] + vars.mod.SoundOddball_SAPS_lmer[2,4]) / (vars.basemodel[1,4] + vars.basemodel[2,4]))
pseudoR2 # 0.02890397, Mixed Model with Sound x Trial type x SAPS explains an additional 2.9% of variability in RT
# Exploring residuals
as.data.frame(resid(mod.SoundOddball_lmer)) -> res
colnames(res)[1] <- "e"
skewness(res$e) # 0.6423345
# Comparing Models ####
boxplot(resid(mod.SoundOddball_lmer) ~ BigTech_FinalExcludes_NoMow$PartID, xlab = "Participant", ylab = "RT", main = "Mixed Model: Residual RTs Predicted by Interaction Between Sound * Trial Type * SAPS Across Participants"); abline(0,0)

anova(mod.SoundOddball_lmer, mod.SoundOddball_lm)
#                       Df    AIC    BIC  logLik deviance  Chisq Chi Df
# mod.SoundOddball_lm    7 576398 576460 -288192   576384                             
# mod.SoundOddball_lmer  8 568604 568674 -284294   568588 7796.3      1  < 2.2e-16 ***
# Improved fit of lmer model
#### Trial Type (Oddball) * SAPS interaction * Halfway
# Descriptives ####
RTby_Trial_SAPS_Halfway <- ddply(BigTech_FinalExcludes_NoMow,
                                   .(Halfway, Code_Good, SAPS_Split),
                                   summarise,
                                   M.RT.ms = mean(RT.ms),
                                   SD.RT.ms = sd(RT.ms),
                                   n.RT.ms = length(RT.ms)) 
RTby_Trial_SAPS_Halfway
# OddballEffect SAPs_High First Half
RTby_Trial_SAPS_Halfway$RareHighSAPS_First_Half <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS" & BigTech_FinalExcludes_NoMow$Halfway == "First Half"])
RTby_Trial_SAPS_Halfway$FrequentHighSAPS_First_Half <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "First Half"])
RTby_Trial_SAPS_Halfway$Oddball.MHighSAPS_First_Half <- RTby_Trial_SAPS_Halfway$RareHighSAPS_First_Half-RTby_Trial_SAPS_Halfway$FrequentHighSAPS_First_Half 
RTby_Trial_SAPS_Halfway$RareHighSAPS_SD_First_Half <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "First Half"])
RTby_Trial_SAPS_Halfway$FrequentHighSAPS_SD_First_Half <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "First Half"])
RTby_Trial_SAPS_Halfway$Oddball.SDHighSAPS_First_Half <- RTby_Trial_SAPS_Halfway$RareHighSAPS_SD_First_Half-RTby_Trial_SAPS_Halfway$FrequentHighSAPS_SD_First_Half 
# OddballEffect SAPs_Low First Half
RTby_Trial_SAPS_Halfway$RareLowSAPS_First_Half <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "First Half"])
RTby_Trial_SAPS_Halfway$FrequentLowSAPS_First_Half <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "First Half"])
RTby_Trial_SAPS_Halfway$Oddball.MLowSAPS_First_Half <- RTby_Trial_SAPS_Halfway$RareLowSAPS_First_Half-RTby_Trial_SAPS_Halfway$FrequentLowSAPS_First_Half
RTby_Trial_SAPS_Halfway$RareLowSAPS_SD_First_Half <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "First Half"])
RTby_Trial_SAPS_Halfway$FrequentLowSAPS_SD_First_Half <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "First Half"])
RTby_Trial_SAPS_Halfway$Oddball.SDLowSAPS_First_Half <- RTby_Trial_SAPS_Halfway$RareLowSAPS_SD_First_Half-RTby_Trial_SAPS_Halfway$FrequentLowSAPS_SD_First_Half 
RTby_Trial_SAPS_Halfway
# OddballEffect SAPs_High Second Half
RTby_Trial_SAPS_Halfway$RareHighSAPS_Second_Half <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS" & BigTech_FinalExcludes_NoMow$Halfway == "Second Half"])
RTby_Trial_SAPS_Halfway$FrequentHighSAPS_Second_Half <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "Second Half"])
RTby_Trial_SAPS_Halfway$Oddball.MHighSAPS_Second_Half <- RTby_Trial_SAPS_Halfway$RareHighSAPS_Second_Half-RTby_Trial_SAPS_Halfway$FrequentHighSAPS_Second_Half 
RTby_Trial_SAPS_Halfway$RareHighSAPS_SD_Second_Half <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "Second Half"])
RTby_Trial_SAPS_Halfway$FrequentHighSAPS_SD_Second_Half <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "Second Half"])
RTby_Trial_SAPS_Halfway$Oddball.SDHighSAPS_Second_Half <- RTby_Trial_SAPS_Halfway$RareHighSAPS_SD_Second_Half-RTby_Trial_SAPS_Halfway$FrequentHighSAPS_SD_Second_Half 
# OddballEffect SAPs_Low Second Half
RTby_Trial_SAPS_Halfway$RareLowSAPS_Second_Half <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "Second Half"])
RTby_Trial_SAPS_Halfway$FrequentLowSAPS_Second_Half <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "Second Half"])
RTby_Trial_SAPS_Halfway$Oddball.MLowSAPS_Second_Half <- RTby_Trial_SAPS_Halfway$RareLowSAPS_Second_Half-RTby_Trial_SAPS_Halfway$FrequentLowSAPS_Second_Half
RTby_Trial_SAPS_Halfway$RareLowSAPS_SD_Second_Half <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "Second Half"])
RTby_Trial_SAPS_Halfway$FrequentLowSAPS_SD_Second_Half <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "Second Half"])
RTby_Trial_SAPS_Halfway$Oddball.SDLowSAPS_Second_Half <- RTby_Trial_SAPS_Halfway$RareLowSAPS_SD_Second_Half-RTby_Trial_SAPS_Halfway$FrequentLowSAPS_SD_Second_Half 
RTby_Trial_SAPS_Halfway
# Mixed model ####
mod.SoundOddball_SAPS_halfway_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ Code_Good * SAPS_Split * Halfway + (1|PartID), REML = F)
summary(mod.SoundOddball_SAPS_halfway_lmer)
contrasts(BigTech_FinalExcludes_NoMow$Code_Good) 
contrasts(BigTech_FinalExcludes_NoMow$Halfway) = contr.treatment(2)
contrasts(BigTech_FinalExcludes_NoMow$Halfway)
contrasts(BigTech_FinalExcludes_NoMow$SAPS_Split) 
BigTech_FinalExcludes_NoMow$Halfway <- relevel(BigTech_FinalExcludes_NoMow$Halfway, ref="First Half")
##### Analyses: Sound x Trial Type (Oddball) x SAPS interaction * Block half [Final Model]#####
# Descriptives ####
RTby_Sound_Trial_SAPS_Halfway <- ddply(BigTech_FinalExcludes_NoMow,
                                 .(Halfway, Code_Good, SAPS_Split, Sound),
                                 summarise,
                                 M.RT.ms = mean(RT.ms),
                                 SD.RT.ms = sd(RT.ms),
                                 n.RT.ms = length(RT.ms)) 
RTby_Sound_Trial_SAPS_Halfway
# OddballEffect SAPs_High First Half phone
RTby_Sound_Trial_SAPS_Halfway$RareHighSAPS_First_Half_phone <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS" & BigTech_FinalExcludes_NoMow$Halfway == "First Half" & BigTech_FinalExcludes_NoMow$Sound == "phone"])
RTby_Sound_Trial_SAPS_Halfway$FrequentHighSAPS_First_Half_phone <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "First Half" & BigTech_FinalExcludes_NoMow$Sound == "phone"])
RTby_Sound_Trial_SAPS_Halfway$Oddball.MHighSAPS_First_Half_phone <- RTby_Sound_Trial_SAPS_Halfway$RareHighSAPS_First_Half_phone-RTby_Sound_Trial_SAPS_Halfway$FrequentHighSAPS_First_Half_phone 
RTby_Sound_Trial_SAPS_Halfway$RareHighSAPS_SD_First_Half_phone <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "First Half" & BigTech_FinalExcludes_NoMow$Sound == "phone"])
RTby_Sound_Trial_SAPS_Halfway$FrequentHighSAPS_SD_First_Half_phone <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "First Half" & BigTech_FinalExcludes_NoMow$Sound == "phone"])
RTby_Sound_Trial_SAPS_Halfway$Oddball.SDHighSAPS_First_Half_phone <- RTby_Sound_Trial_SAPS_Halfway$RareHighSAPS_SD_First_Half_phone-RTby_Sound_Trial_SAPS_Halfway$FrequentHighSAPS_SD_First_Half_phone
# OddballEffect SAPs_Low First Half phone
RTby_Sound_Trial_SAPS_Halfway$RareLowSAPS_First_Half_phone <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "First Half" & BigTech_FinalExcludes_NoMow$Sound == "phone"])
RTby_Sound_Trial_SAPS_Halfway$FrequentLowSAPS_First_Half_phone <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "First Half" & BigTech_FinalExcludes_NoMow$Sound == "phone"])
RTby_Sound_Trial_SAPS_Halfway$Oddball.MLowSAPS_First_Half_phone <- RTby_Sound_Trial_SAPS_Halfway$RareLowSAPS_First_Half_phone-RTby_Sound_Trial_SAPS_Halfway$FrequentLowSAPS_First_Half_phone
RTby_Sound_Trial_SAPS_Halfway$RareLowSAPS_SD_First_Half_phone <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "First Half" & BigTech_FinalExcludes_NoMow$Sound == "phone"])
RTby_Sound_Trial_SAPS_Halfway$FrequentLowSAPS_SD_First_Half_phone <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "First Half" & BigTech_FinalExcludes_NoMow$Sound == "phone"])
RTby_Sound_Trial_SAPS_Halfway$Oddball.SDLowSAPS_First_Half_phone <- RTby_Sound_Trial_SAPS_Halfway$RareLowSAPS_SD_First_Half_phone-RTby_Sound_Trial_SAPS_Halfway$FrequentLowSAPS_SD_First_Half_phone 
# OddballEffect SAPs_High Second Half phone
RTby_Sound_Trial_SAPS_Halfway$RareHighSAPS_Second_Half_phone <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS" & BigTech_FinalExcludes_NoMow$Halfway == "Second Half" & BigTech_FinalExcludes_NoMow$Sound == "phone"])
RTby_Sound_Trial_SAPS_Halfway$FrequentHighSAPS_Second_Half_phone <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "Second Half" & BigTech_FinalExcludes_NoMow$Sound == "phone"])
RTby_Sound_Trial_SAPS_Halfway$Oddball.MHighSAPS_Second_Half_phone <- RTby_Sound_Trial_SAPS_Halfway$RareHighSAPS_Second_Half_phone-RTby_Sound_Trial_SAPS_Halfway$FrequentHighSAPS_Second_Half_phone
RTby_Sound_Trial_SAPS_Halfway$RareHighSAPS_SD_Second_Half_phone <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "Second Half" & BigTech_FinalExcludes_NoMow$Sound == "phone"])
RTby_Sound_Trial_SAPS_Halfway$FrequentHighSAPS_SD_Second_Half_phone <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "Second Half" & BigTech_FinalExcludes_NoMow$Sound == "phone"])
RTby_Sound_Trial_SAPS_Halfway$Oddball.SDHighSAPS_Second_Half_phone <- RTby_Sound_Trial_SAPS_Halfway$RareHighSAPS_SD_Second_Half_phone-RTby_Sound_Trial_SAPS_Halfway$FrequentHighSAPS_SD_Second_Half_phone
# OddballEffect SAPs_Low Second Half phone
RTby_Sound_Trial_SAPS_Halfway$RareLowSAPS_Second_Half_phone <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "Second Half" & BigTech_FinalExcludes_NoMow$Sound == "phone"])
RTby_Sound_Trial_SAPS_Halfway$FrequentLowSAPS_Second_Half_phone <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "Second Half" & BigTech_FinalExcludes_NoMow$Sound == "phone"])
RTby_Sound_Trial_SAPS_Halfway$Oddball.MLowSAPS_Second_Half_phone <- RTby_Sound_Trial_SAPS_Halfway$RareLowSAPS_Second_Half_phone-RTby_Sound_Trial_SAPS_Halfway$FrequentLowSAPS_Second_Half_phone
RTby_Sound_Trial_SAPS_Halfway$RareLowSAPS_SD_Second_Half_phone <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "Second Half" & BigTech_FinalExcludes_NoMow$Sound == "phone"])
RTby_Sound_Trial_SAPS_Halfway$FrequentLowSAPS_SD_Second_Half_phone <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "Second Half" & BigTech_FinalExcludes_NoMow$Sound == "phone"])
RTby_Sound_Trial_SAPS_Halfway$Oddball.SDLowSAPS_Second_Half_phone <- RTby_Sound_Trial_SAPS_Halfway$RareLowSAPS_SD_Second_Half_phone-RTby_Sound_Trial_SAPS_Halfway$FrequentLowSAPS_SD_Second_Half_phone
# OddballEffect SAPs_High First Half control
RTby_Sound_Trial_SAPS_Halfway$RareHighSAPS_First_Half_control <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS" & BigTech_FinalExcludes_NoMow$Halfway == "First Half" & BigTech_FinalExcludes_NoMow$Sound == "control"])
RTby_Sound_Trial_SAPS_Halfway$FrequentHighSAPS_First_Half_control <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "First Half" & BigTech_FinalExcludes_NoMow$Sound == "control"])
RTby_Sound_Trial_SAPS_Halfway$Oddball.MHighSAPS_First_Half_control <- RTby_Sound_Trial_SAPS_Halfway$RareHighSAPS_First_Half_control-RTby_Sound_Trial_SAPS_Halfway$FrequentHighSAPS_First_Half_control 
RTby_Sound_Trial_SAPS_Halfway$RareHighSAPS_SD_First_Half_control <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "First Half" & BigTech_FinalExcludes_NoMow$Sound == "control"])
RTby_Sound_Trial_SAPS_Halfway$FrequentHighSAPS_SD_First_Half_control <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "First Half" & BigTech_FinalExcludes_NoMow$Sound == "control"])
RTby_Sound_Trial_SAPS_Halfway$Oddball.SDHighSAPS_First_Half_control <- RTby_Sound_Trial_SAPS_Halfway$RareHighSAPS_SD_First_Half_control-RTby_Sound_Trial_SAPS_Halfway$FrequentHighSAPS_SD_First_Half_control
# OddballEffect SAPs_Low First Half control
RTby_Sound_Trial_SAPS_Halfway$RareLowSAPS_First_Half_control <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "First Half" & BigTech_FinalExcludes_NoMow$Sound == "control"])
RTby_Sound_Trial_SAPS_Halfway$FrequentLowSAPS_First_Half_control <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "First Half" & BigTech_FinalExcludes_NoMow$Sound == "control"])
RTby_Sound_Trial_SAPS_Halfway$Oddball.MLowSAPS_First_Half_control <- RTby_Sound_Trial_SAPS_Halfway$RareLowSAPS_First_Half_control-RTby_Sound_Trial_SAPS_Halfway$FrequentLowSAPS_First_Half_control
RTby_Sound_Trial_SAPS_Halfway$RareLowSAPS_SD_First_Half_control <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "First Half" & BigTech_FinalExcludes_NoMow$Sound == "control"])
RTby_Sound_Trial_SAPS_Halfway$FrequentLowSAPS_SD_First_Half_control <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "First Half" & BigTech_FinalExcludes_NoMow$Sound == "control"])
RTby_Sound_Trial_SAPS_Halfway$Oddball.SDLowSAPS_First_Half_control <- RTby_Sound_Trial_SAPS_Halfway$RareLowSAPS_SD_First_Half_control-RTby_Sound_Trial_SAPS_Halfway$FrequentLowSAPS_SD_First_Half_control 
# OddballEffect SAPs_High Second Half control
RTby_Sound_Trial_SAPS_Halfway$RareHighSAPS_Second_Half_control <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS" & BigTech_FinalExcludes_NoMow$Halfway == "Second Half" & BigTech_FinalExcludes_NoMow$Sound == "control"])
RTby_Sound_Trial_SAPS_Halfway$FrequentHighSAPS_Second_Half_control <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "Second Half" & BigTech_FinalExcludes_NoMow$Sound == "control"])
RTby_Sound_Trial_SAPS_Halfway$Oddball.MHighSAPS_Second_Half_control <- RTby_Sound_Trial_SAPS_Halfway$RareHighSAPS_Second_Half_control-RTby_Sound_Trial_SAPS_Halfway$FrequentHighSAPS_Second_Half_control
RTby_Sound_Trial_SAPS_Halfway$RareHighSAPS_SD_Second_Half_control <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "Second Half" & BigTech_FinalExcludes_NoMow$Sound == "control"])
RTby_Sound_Trial_SAPS_Halfway$FrequentHighSAPS_SD_Second_Half_control <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "High SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "Second Half" & BigTech_FinalExcludes_NoMow$Sound == "control"])
RTby_Sound_Trial_SAPS_Halfway$Oddball.SDHighSAPS_Second_Half_control <- RTby_Sound_Trial_SAPS_Halfway$RareHighSAPS_SD_Second_Half_control-RTby_Sound_Trial_SAPS_Halfway$FrequentHighSAPS_SD_Second_Half_control
# OddballEffect SAPs_Low Second Half control
RTby_Sound_Trial_SAPS_Halfway$RareLowSAPS_Second_Half_control <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "Second Half" & BigTech_FinalExcludes_NoMow$Sound == "control"])
RTby_Sound_Trial_SAPS_Halfway$FrequentLowSAPS_Second_Half_control <- mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "Second Half" & BigTech_FinalExcludes_NoMow$Sound == "control"])
RTby_Sound_Trial_SAPS_Halfway$Oddball.MLowSAPS_Second_Half_control <- RTby_Sound_Trial_SAPS_Halfway$RareLowSAPS_Second_Half_control-RTby_Sound_Trial_SAPS_Halfway$FrequentLowSAPS_Second_Half_control
RTby_Sound_Trial_SAPS_Halfway$RareLowSAPS_SD_Second_Half_control <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "Second Half" & BigTech_FinalExcludes_NoMow$Sound == "control"])
RTby_Sound_Trial_SAPS_Halfway$FrequentLowSAPS_SD_Second_Half_control <- sd(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent" & BigTech_FinalExcludes_NoMow$SAPS_Split == "Low SAPS"& BigTech_FinalExcludes_NoMow$Halfway == "Second Half" & BigTech_FinalExcludes_NoMow$Sound == "control"])
RTby_Sound_Trial_SAPS_Halfway$Oddball.SDLowSAPS_Second_Half_control <- RTby_Sound_Trial_SAPS_Halfway$RareLowSAPS_SD_Second_Half_control-RTby_Sound_Trial_SAPS_Halfway$FrequentLowSAPS_SD_Second_Half_control
RTby_Sound_Trial_SAPS_Halfway
466.4508-411.3663
465.9170-419.
# Anova/regression model ####
mod.SoundOddball_BlockOrd_SAPS_lm <- lm(data = BigTech_FinalExcludes_NoMow, RT.ms ~ Sound * Code_Good * SAPS_Split + Block_Ord)
summary(mod.SoundOddball_BlockOrd_SAPS_lm)
boxplot(resid(mod.SoundOddball_BlockOrd_SAPS_lm) ~ BigTech_FinalExcludes_NoMow$PartID, xlab = "Participant", ylab = "RT", main = "Anova Model: Residual RTs Predicted by Interactio Between Sound * Trial Type * SAPS Controlling for Block Order Across Participants"); abline(0,0)
# Exploring residuals
as.data.frame(resid(mod.SoundOddball_BlockOrd_SAPS_lm)) -> res
colnames(res)[1] <- "e"
skewness(res$e) # 0.5343211
##### Mixed Model ####
# old stuff ####
# mod.SoundOddball_BlockOrd_SAPS_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ Sound * Code_Good * SAPS_Split + Block_Ord + (1|PartID), REML = F)
# summary(mod.SoundOddball_BlockOrd_SAPS_lmer)
# # Peusdo R squared
# as.data.frame(VarCorr(mod.SoundOddball_BlockOrd_SAPS_lmer)) -> vars.mod.SoundOddball_BlockOrd_SAPS_lmer
# pseudoR2 <- 1 - ((vars.mod.SoundOddball_BlockOrd_SAPS_lmer[1,4] + vars.mod.SoundOddball_BlockOrd_SAPS_lmer[2,4]) / (vars.basemodel[1,4] + vars.basemodel[2,4]))
# pseudoR2 # 0.04818629, Mixed Model with Sound x Trial type x SAPS interaction controlling for block order explains an additional 5.4% of variability in RT
# confint(mod.SoundOddball_BlockOrd_SAPS_lmer)
# # Exploring residuals
# as.data.frame(resid(mod.SoundOddball_BlockOrd_SAPS_lmer)) -> res
# colnames(res)[1] <- "e"
# skewness(res$e) # 0.6396018

# Setting block to 16 
# contrasts(BigTech_FinalExcludes_NoMow$Block_Ord.f)
# BigTech_FinalExcludes_NoMow$Block_Ord.f.2 <- relevel(BigTech_FinalExcludes_NoMow$Block_Ord.f, ref="16")
# mod.SoundOddball_BlockOrd.f.2_SAPS_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ Sound * Code_Good * SAPS_Split + Block_Ord.f.2 + (1|PartID), REML = F)
# summary(mod.SoundOddball_BlockOrd.f.2_SAPS_lmer)
# No differences between Oddball*Sound*SAPS

# new stuff ####
#BigTech_FinalExcludes_NoMow <- mutate(BigTech_FinalExcludes_NoMow, Halfway = as.factor(ifelse(Block_Ord <= 8, "First Half", "Second Half")))
contrasts(BigTech_FinalExcludes_NoMow$Halfway) = contr.sum(2)
contrasts(BigTech_FinalExcludes_NoMow$SAPS_Split) = contr.sum(2)
contrasts(BigTech_FinalExcludes_NoMow$Code_Good) = contr.sum(2)
contrasts(BigTech_FinalExcludes_NoMow$Sound) = contr.sum(2)
contrasts(BigTech_FinalExcludes_NoMow$Code_Good) = contr.treatment(2)
contrasts(BigTech_FinalExcludes_NoMow$Sound) = contr.treatment(2)
contrasts(BigTech_FinalExcludes_NoMow$SAPS_Split) = contr.treatment(2)
contrasts(BigTech_FinalExcludes_NoMow$Halfway) = contr.treatment(2)
contrasts(BigTech_FinalExcludes_NoMow$Block_Ord.f) = contr.helmert(16)
contrasts(BigTech_FinalExcludes_NoMow$Code_Good) 
contrasts(BigTech_FinalExcludes_NoMow$Sound) 
contrasts(BigTech_FinalExcludes_NoMow$SAPS_Split) 
contrasts(BigTech_FinalExcludes_NoMow$Halfway) 
contrasts(BigTech_FinalExcludes_NoMow$Block_Ord.f)
contrasts(BigTech_FinalExcludes_NoMow$SAPS_thirds) 

BigTech_FinalExcludes_NoMow$Halfway <- relevel(BigTech_FinalExcludes_NoMow$Halfway, ref="Second Half")
BigTech_FinalExcludes_NoMow$Halfway <- relevel(BigTech_FinalExcludes_NoMow$Halfway, ref="First Half")
BigTech_FinalExcludes_NoMow$Sound <- relevel(BigTech_FinalExcludes_NoMow$Sound, ref="phone")
BigTech_FinalExcludes_NoMow$Sound <- relevel(BigTech_FinalExcludes_NoMow$Sound, ref="control")
BigTech_FinalExcludes_NoMow$Code_Good <- relevel(BigTech_FinalExcludes_NoMow$Code_Good, ref="Frequent")
BigTech_FinalExcludes_NoMow$Code_Good <- relevel(BigTech_FinalExcludes_NoMow$Code_Good, ref="Rare")
BigTech_FinalExcludes_NoMow$SAPS_Split <- relevel(BigTech_FinalExcludes_NoMow$SAPS_Split, ref="Low SAPS")

control <- lmerControl(check.nobs.vs.nRE="ignore", calc.derivs=FALSE)

mod.SoundOddball_BlockOrd_SAPS_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ Sound * Code_Good * SAPS_Split + (Block_Ord|PartID), REML = F, control = control)
summary(mod.SoundOddball_BlockOrd_SAPS_lmer)
mod.SoundOddball_BlockOrd_SAPS_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ Code_Good * Sound * SAPS_Split * Halfway + (1|PartID), REML = F)
summary(mod.SoundOddball_BlockOrd_SAPS_lmer)


library(lmerTest)
library(lmSupport)
library(psych)
summary(mod.SoundOddball_BlockOrd_SAPS_lmer)
mod.SoundOddball_SAPS_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ Sound * Code_Good * SAPS_Split + (1|PartID), REML = F)
summary(mod.SoundOddball_SAPS_lmer)

# Comparing models #####
boxplot(resid(mod.SoundOddball_BlockOrd_SAPS_lmer) ~ BigTech_FinalExcludes_NoMow$PartID, xlab = "Participant", ylab = "RT", main = "Mixed Model: Residual RTs Predicted by Interaction Between Sound * Trial Type * SAPS Controlling for Block Order Across Participants"); abline(0,0)

anova(mod.SoundOddball_BlockOrd_SAPS_lmer, mod.SoundOddball_BlockOrd_SAPS_lm)
#                                     Df    AIC    BIC  logLik deviance  Chisq Chi Df
# mod.SoundOddball_BlockOrd_SAPS_lm   14 575321 575445 -287647   575293                             
# mod.SoundOddball_BlockOrd_SAPS_lmer 29 567068 567323 -283505   567010 8283.5     15  < 2.2e-16 ***
# Lower values across the board suggesting better model fit

##### Analyses: Sound x Trial Type (Oddball) x SAPS_thirds interaction controlling for block order#####

# New df removing middle thirds SAPS
BigTech_Final_NoMid3SAPS <- BigTech_FinalExcludes_NoMow[!BigTech_FinalExcludes_NoMow$SAPS_thirds == "Mid Third", ]
str(BigTech_Final_NoMid3SAPS$SAPS_thirds)
levels(BigTech_Final_NoMid3SAPS$SAPS_thirds)
# Descriptives #####
RTby_SAPS3 <- ddply(BigTech_Final_NoMid3SAPS,
                    .(SAPS_thirds),
                    summarise,
                    M.RT.ms = mean(RT.ms),
                    SD.RT.ms = sd(RT.ms),
                    n.RT.ms = length(RT.ms)) 
RTby_SAPS3
write.table(RTby_SAPS3, file = "RTby_SAPS3_72p.txt", sep = ",", quote = FALSE, row.names = F)
library(EnvStats)
# fun_mean <- function(x){return(data.frame(y=mean(x),label=mean(x,na.rm=T)))}
# fun_n <- function(x){return(c(y = median(x)*1.05, label = length(x)))}
# 
# plot <- ggplot(BigTech_Final_NoMid3SAPS, aes(x = SAPS_thirds, y = RT.ms, color = SAPS_thirds)) +
#   geom_jitter(color="black", size=0.15, alpha=0.3, width = .35) +
#   geom_boxplot(width= .7,
#                weight= 1,
#                color="black",
#                fill="blue",
#                alpha=0.8,
#                notch=TRUE,
#                notchwidth = .25,
#                outlier.colour="red",
#                outlier.fill="red",
#                outlier.size=3,
#                outlier.alpha = .15) +
#   stat_summary(fun.y=mean, geom="point", shape=20, size=10, color="green", fill="green", alpha = .9) +
#   geom_stripchart(n.text.params=list(size = 5), location.scale.text.params=list(size =5), test.text.params=list(size = 5), 
#                   jitter.params = list(size = 0,width = 0, height = 0), line.params = list(linetype = 0), 
#                   location.params = list(size=0), point.params = list(color = "blank", size = 0,width = 0, height = 0, size = 0), errorbar.params = list(size = 0),test.text = TRUE, color = "black") +
#   theme(legend.position="none") +
#   scale_color_brewer(palette = "Set1") +
#   geom_hline(yintercept = mean(BigTech_FinalExcludes_NoMow$RT.ms)) +
#   labs(title = "Reaction Time Across High Third vs Low Third Smartphone Addiction", x = "Smartphone Addiction (SAPS)", y = "Reaction Time (ms)", size = 12) +
#   scale_y_continuous(breaks = seq(50, 850, 50)) +
#   theme(axis.text = element_text( 
#     angle = 0, 
#     color="blue", 
#     size=15, 
#     face=3)
#   ) +
#   theme(axis.title = element_text(angle = 0, color="black", size=15, face=3)) +
#   theme(plot.title = element_text(hjust = 0.5, size = 20))
# plot

# Anova/regression model ####
BigTech_Final_NoMid3SAPS$Sound <- relevel(BigTech_Final_NoMid3SAPS$Sound, ref="phone")
mod.SoundOddball_BlockOrd_SAPS3_lm <- lm(data = BigTech_Final_NoMid3SAPS, RT.ms ~ Sound * Code_Good * SAPS_thirds + Block_Ord)
summary(mod.SoundOddball_BlockOrd_SAPS3_lm)
boxplot(resid(mod.SoundOddball_BlockOrd_SAPS3_lm) ~ BigTech_Final_NoMid3SAPS$PartID, xlab = "Participant", ylab = "RT", main = "Anova Model: Residual RTs Predicted by Interactio Between Sound * Trial Type * SAPS Controlling for Block Order Across Participants"); abline(0,0)
# Exploring residuals
as.data.frame(resid(mod.SoundOddball_BlockOrd_SAPS3_lm)) -> res
colnames(res)[1] <- "e"
skewness(res$e) # 0.460707
# Mixed Model ####
summary(lmer(data = BigTech_Final_NoMid3SAPS, RT.ms ~ SAPS_thirds + (1|PartID), REML = F))
summary(lmer(data = BigTech_Final_NoMid3SAPS, RT.ms ~ Sound * SAPS_thirds + (1|PartID), REML = F))
summary(lmer(data = BigTech_Final_NoMid3SAPS, RT.ms ~ Code_Good * SAPS_thirds + (1|PartID), REML = F))
BigTech_Final_NoMid3SAPS$SAPS_thirds <- relevel(BigTech_Final_NoMid3SAPS$SAPS_thirds, ref="Low Third")
summary(lmer(data = BigTech_Final_NoMid3SAPS, RT.ms ~ Code_Good * SAPS_thirds + (1|PartID), REML = F))
RTby_SAPS3 <- ddply(BigTech_Final_NoMid3SAPS,
                    .(Code_Good,SAPS_thirds),
                    summarise,
                    M.RT.ms = mean(RT.ms),
                    SD.RT.ms = sd(RT.ms),
                    n.RT.ms = length(RT.ms)) 
RTby_SAPS3 <- ddply(BigTech_Final_NoMid3SAPS,
                    .(Code_Good,SAPS_thirds, Block_Ord),
                    summarise,
                    M.RT.ms = mean(RT.ms),
                    SD.RT.ms = sd(RT.ms),
                    n.RT.ms = length(RT.ms)) 
summary(lmer(data = BigTech_Final_NoMid3SAPS, RT.ms ~ Sound * Code_Good * SAPS_thirds + (1|PartID), REML = F))
mod.SoundOddball_BlockOrd_SAPS3_lmer <- lmer(data = BigTech_Final_NoMid3SAPS, RT.ms ~ Sound * Code_Good * SAPS_thirds * Halfway + (1|PartID), REML = F)
summary(mod.SoundOddball_BlockOrd_SAPS3_lmer)
# Peusdo R squared
as.data.frame(VarCorr(mod.SoundOddball_BlockOrd_SAPS3_lmer)) -> vars.mod.SoundOddball_BlockOrd_SAPS3_lmer
pseudoR2 <- 1 - ((vars.mod.SoundOddball_BlockOrd_SAPS3_lmer[1,4] + vars.mod.SoundOddball_BlockOrd_SAPS3_lmer[2,4]) / (vars.basemodel[1,4] + vars.basemodel[2,4]))
pseudoR2 # 0.01332484, Mixed Model with Sound x Trial type x SAPS_thirds interaction controlling for block order explains an additional 1.3% of variability in RT
confint(mod.SoundOddball_BlockOrd_SAPS3_lmer)
# Exploring residuals
as.data.frame(resid(mod.SoundOddball_BlockOrd_SAPS3_lmer)) -> res
colnames(res)[1] <- "e"
skewness(res$e) # 0.672243

# Setting block to 16
contrasts(BigTech_Final_NoMid3SAPS$Block_Ord.f)
BigTech_Final_NoMid3SAPS$Block_Ord.f.2 <- relevel(BigTech_Final_NoMid3SAPS$Block_Ord.f, ref="8")
mod.SoundOddball_BlockOrd.f.2_SAPS3_lmer <- lmer(data = BigTech_Final_NoMid3SAPS, RT.ms ~ Sound * Code_Good * SAPS_thirds + Block_Ord.f.2 + (1|PartID), REML = F)
summary(mod.SoundOddball_BlockOrd.f.2_SAPS3_lmer)
# No differences between Oddball*Sound*SAPS
# Comparing models #####
boxplot(resid(mod.SoundOddball_BlockOrd_SAPS_lmer) ~ BigTech_FinalExcludes_NoMow$PartID, xlab = "Participant", ylab = "RT", main = "Mixed Model: Residual RTs Predicted by Interaction Between Sound * Trial Type * SAPS Controlling for Block Order Across Participants"); abline(0,0)

anova(mod.SoundOddball_BlockOrd_SAPS_lmer, mod.SoundOddball_BlockOrd_SAPS_lm)
#                                     Df    AIC    BIC  logLik deviance  Chisq Chi Df
# mod.SoundOddball_BlockOrd_SAPS_lm   14 575321 575445 -287647   575293                             
# mod.SoundOddball_BlockOrd_SAPS_lmer 29 567068 567323 -283505   567010 8283.5     15  < 2.2e-16 ***
# Lower values across the board suggesting better model fit

### Exploratory #####
mod.SoundOddball_BlockOrd_SAPS_Gender_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ Sound * SAPS_Split * Block_Ord + (1|PartID), REML = F)
summary(mod.SoundOddball_BlockOrd_SAPS_Gender_lmer)
summary(lm(data = BigTech_FinalExcludes_NoMow, SAPS_Ave ~ MAAS_Ave))
summary(lm(data = BigTech_FinalExcludes_NoMow, RT.ms ~ MAAS_Ave * SAPS_Ave))

hist(BigTech_FinalExcludes_NoMow$Control.Correct.2.)
describeBy(BigTech_Final_Excludes2$Control.Correct.2., BigTech_Final_Excludes2$Sound_Control)
describe(BigTech_FinalExcludes$Sound_Control)
describe(BigTech_FinalExcludes_NoMow)
count(BigTech_FinalExcludes_NoMow)
Wide <- read_csv("BigTech_Wide_SPSS_72p.csv")
##### Analyses: random slopes ####
mod.SoundOddball_BlockOrd_SAPSrand_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ Sound * Code_Good * SAPS_Split + Block_Ord + (1 + SAPS_Split|PartID), REML = F)
summary(mod.SoundOddball_BlockOrd_SAPSrand_lmer)
anova(mod.SoundOddball_BlockOrd_SAPSrand_lmer, mod.SoundOddball_BlockOrd_SAPS_lmer)
mod.SoundrandOddball_BlockOrd_SAPS_lmer <- lmer(data = BigTech_FinalExcludes_NoMow, RT.ms ~ Sound * Code_Good * SAPS_Split + Block_Ord + (1 + Sound|PartID), REML = F)
summary(mod.SoundrandOddball_BlockOrd_SAPS_lmer)
anova(mod.SoundrandOddball_BlockOrd_SAPS_lmer, mod.SoundOddball_BlockOrd_SAPS_lmer)
##### Overall Model Comparisons #####
# Anova models
anova(mod.SoundOddball_BlockOrd_SAPS_lm, mod.SoundOddball_BlockOrd_lm, mod.SoundOddball_lm)
# Model 1: RT.ms ~ Sound * Code_Good * SAPS_Split + Block_Ord
# Model 2: RT.ms ~ Sound * Code_Good + Block_Ord
# Model 3: RT.ms ~ Sound * Code_Good
#   Res.Df       RSS Df Sum of Sq         F         Pr(>F)    
# 1  48598 392624022                                          
# 2  48604 393021526 -6   -397504    8.2003 0.000000006865 ***
# 3  48605 401532974 -1  -8511447 1053.5253      < 2.2e-16 ***

# Mixed Models
anova(mod.SoundOddball_BlockOrd_SAPS_lmer, mod.SoundOddball_BlockOrd_lmer, mod.SoundOddball_lmer, basemodel) 
#                                     Df    AIC    BIC  logLik deviance   Chisq Chi Df Pr(>Chisq)    
# basemodel                            3 570227 570253 -285110   570221                              
# mod.SoundOddball_lmer                8 568604 568674 -284294   568588 1632.97      5  < 2.2e-16 ***
# mod.SoundOddball_BlockOrd_lmer       9 567374 567453 -283678   567356 1231.84      1  < 2.2e-16 ***
# mod.SoundOddball_BlockOrd_SAPS_lmer 29 567068 567323 -283505   567010  346.03     20  < 2.2e-16 ***

###### Plots ######
# Three way interaction
# library(ggeffects)
# mod.plot <- lm(BigTech_FinalExcludes_NoMow$RT.ms ~ BigTech_FinalExcludes_NoMow$Sound*BigTech_FinalExcludes_NoMow$Code_Good*BigTech_FinalExcludes_NoMow$SAPS_Split+Block_Ord, data = BigTech_FinalExcludes_NoMow)
# plot.df.3way <- ggpredict(mod.plot, terms = c("SAPS_Split", "Sound", "Code_Good", "Block_Ord"))
# 
# View(plot.df.3way) 
# 
# colnames(plot.df.3way)[c(1,6,7)] <- c("number", "advice", "industry.f")
# 
# plot.df.3way <- mutate(plot.df.3way, 
#                        advice = as.numeric(advice),
#                        industry.f = factor(industry.f, 
#                                            levels = c(1, 2), 
#                                            labels = c("Lumber", "Steel"))) 
# 
# ggplot()+ theme_classic(base_size = 14) +  
#   geom_ribbon(data = plot.df.3way, alpha = .1, 
#               aes(group = number, ymax = conf.high, ymin = conf.low, 
#                   x = advice, fill = as.factor(number))) + 
#   geom_line(data = plot.df.3way, 
#             aes(y = predicted, x = advice, group = number, color = as.factor(number))) +
#   geom_point(data = tax, size = 2, aes(x = advice, y = taxes)) + 
#   scale_fill_manual("Company size", values = c("red", "black"), 
#                     labels = c("Small (-1 SD)", "Large (+1 SD)")) +
#   scale_color_manual("Company size", values = c("red", "black"), 
#                      labels = c("Small (-1 SD)", "Large (+1 SD)")) +
#   facet_grid(~industry.f) + 
#   theme(legend.position = c(.65, .15))
# ggplot(BigTech_FinalExcludes_NoMow, aes(x=Sound, y=RT.ms, color=Code_Good))+
#   geom_violin(width = .25, alpha = .2, shape = 1, size = .5)+
#   facet_wrap(  ~ SAPS_Split)+
#   labs(y="RT(ms)", x="Sound")+
#   theme(legend.position="bottom")


interaction.plot(BigTech_FinalExcludes_NoMow$Sound, BigTech_FinalExcludes_NoMow$Code_Good, BigTech_FinalExcludes_NoMow$RT.ms)

ggplot(mod.SoundOddball_lm) +
  aes(x = Sound, y = RT.ms, color = Code_Good) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm")
library(ggeffects)
ggplot(BigTech_FinalExcludes_NoMow, aes(x=Sound, y=RT.ms, group=Code_Good)) + 
  scale_x_manual(breaks = 1:3, limits = c("phone", "control"), values = c("phone", "Control")) +
  geom_line() +
  geom_boxplot()+
  geom_errorbar()

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
theme_set(theme_sjplot())

plot_model(mod.SoundOddball_BlockOrd_SAPS_lmer, type = "eff", terms = c("Sound", "Code_Good", "SAPS_Ave"), 
           title = "RT predicted from Sounds * Trial Frequency * SAPS controlling for Block Order", wrap.legend.title = 5,
           show.values = TRUE, show.p = TRUE) + geom_smooth(method = lm) +
  theme(axis.title = element_text(angle = 0, color="black", size=15, face=3)) +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  theme(axis.text = element_text(angle = 0, color="blue", size=15, face=3)) +
  theme(legend.position="bottom") +
  geom_hline(yintercept = mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Frequent"])) +
  geom_hline(yintercept = mean(BigTech_FinalExcludes_NoMow$RT.ms[BigTech_FinalExcludes_NoMow$Code_Good == "Rare"]))

ggplot(BigTech_FinalExcludes_NoMow, aes(x=Code_Good, y=RT.ms, colour=Sound, group = Sound)) + 
  geom_jitter(alpha = .3, size = .2) +
  facet_grid(. ~ BigTech_FinalExcludes_NoMow$SAPS_Split) +
  geom_line()+
  ggtitle("RT predicted from Sounds * Trial Frequency * SAPS controlling for Block Order") +
  labs(colour="") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=10, color="green", fill="green", alpha = .9) 

fun_mean <- function(x){return(data.frame(y=mean(x),label=mean(x,na.rm=T)))}

ggplot(BigTech_FinalExcludes_NoMow, aes(x = Code_Good, y = RT.ms, color = Sound, 
                                        group = Sound)) +
  geom_boxplot(shape = 1, size = 1, alpha = .8) +
  geom_smooth(aes(as.numeric(Code_Good), RT.ms, color = Sound), method = lm, 
              se = F, show.legend = NA, linetype = "twodash", alpha = .8, 
              size = 2, fullrange = T) +
  facet_grid(. ~ BigTech_FinalExcludes_NoMow$SAPS_Split) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=10, color="green", 
               fill="green", alpha = .9) 
  geom_hline(yintercept = mean(BigTech_FinalExcludes_NoMow$RT.ms)) +
  labs(title = "Reaction Time Across Sound and Trial Type", x = "Trial Type", 
       y = "RT (ms)", size = 12) +
  scale_y_continuous(breaks = seq(0, 1000, 100)) +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.position = "bottom") 

# Block quarters variable
BigTech_FinalExcludes_NoMow <- mutate(BigTech_FinalExcludes_NoMow, 
                                      Quarter = as.factor(ifelse(Block_Ord <= 4, "First Qrt", 
                                                                 ifelse(Block_Ord >= 5 & Block_Ord < 9,"Second Qrt",
                                                                        ifelse(Block_Ord >= 9 & Block_Ord < 12,"Third Qrt", " Fourth Qrt")))))
# RT.ms ~ Sound * Code_Good * SAPS_Split * Halfway plot   
  RTMeans <- aggregate(data = BigTech_FinalExcludes_NoMow, RT.ms ~ Sound * Code_Good * SAPS_Split * Halfway, mean)
  RTMeans <- mutate(RTMeans, Group = c(1:16))
  RTMeans <- mutate(RTMeans, SAPS = ifelse(SAPS_Split == "High SAPS", 1, 2))
  RTMeans <- mutate(RTMeans, TrialType = ifelse(Code_Good == "Frequent", 1, 2))
  str(RTMeans)
  ggplot(BigTech_FinalExcludes_NoMow, aes(y=RT.ms, x=Sound)) + theme_bw(base_size = 14) +
    geom_jitter(aes(color=Code_Good),alpha = .5, size=.7, width = .2, fill = "black") +
    set.seed(69) +
    geom_point(data = RTMeans, col = "black", size = 7, shape = 23, fill = RTMeans$TrialType) +
    scale_colour_manual("Trial Type", values=c("red", "black")) +
    geom_line(data = RTMeans, aes(y = RT.ms, group = Code_Good:SAPS_Split, color = RTMeans$TrialType, linetype=SAPS_Split), alpha = .9, col = "black", size = 1) +
    scale_fill_manual(values=c("black", "red")) +
    facet_wrap(~Halfway) +
    geom_hline(yintercept = mean(RTMeans$RT.ms)) +
    labs(title = "Change in RT Oddball between Sounds, Smartphone Addiction, and Experiment Half", x = "Sound", y = "RT (ms)", size = 12) +
    scale_y_continuous(limits = c(400, 525), breaks = seq(400, 525, 25)) +
    scale_x_discrete(labels = c("control" = "Control","phone" = "Phone")) +
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5, size = 15)) +
    theme(legend.position = "bottom") +
    theme(axis.text = element_text(angle = 0, color="blue",size=15, face=3)) +
    theme(axis.title = element_text(angle = 0, color="black", size=15, face = "bold")) +
    theme(plot.title = element_text(hjust = 0.5, size = 20)) +
    theme(legend.key = element_rect(fill = "white", colour = "black")) +
    theme(legend.text = element_text(size = 15, colour = "Black")) +
    theme(legend.title = element_text(face = "bold"))
# RT.ms ~ Sound * Code_Good * SAPS_Split plot   
  RTMeans2 <- aggregate(data = BigTech_FinalExcludes_NoMow, RT.ms ~ Sound * Code_Good * SAPS_Split, mean)
  RTMeans2 <- mutate(RTMeans, Group = c(1:8))
  RTMeans2 <- mutate(RTMeans, SAPS = ifelse(SAPS_Split == "High SAPS", 1, 2))
  RTMeans2 <- mutate(RTMeans, TrialType = ifelse(Code_Good == "Frequent", 1, 2))
  str(RTMeans2)
  ggplot(BigTech_FinalExcludes_NoMow, aes(y=RT.ms, x=Sound)) + theme_bw(base_size = 14) +
    geom_jitter(aes(color=Code_Good),alpha = .5, size=.7, width = .2, fill = "black") +
    set.seed(69) +
    geom_point(data = RTMeans2, col = "black", size = 7, shape = 23, fill = RTMeans2$TrialType) +
    scale_colour_manual("Trial Type", values=c("red", "black")) +
    geom_line(data = RTMeans, aes(y = RT.ms, group = Code_Good:SAPS_Split, color = RTMeans2$TrialType, linetype=SAPS_Split), alpha = .9, col = "black", size = 1) +
    scale_fill_manual(values=c("black", "red")) +
    geom_hline(yintercept = mean(RTMeans2$RT.ms)) +
    labs(title = "Change in RT Oddball between Sounds, and Smartphone Addiction", x = "Sound", y = "RT (ms)", size = 12) +
    scale_y_continuous(limits = c(400, 525), breaks = seq(400, 525, 25)) +
    scale_x_discrete(labels = c("control" = "Control","phone" = "Phone")) +
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5, size = 15)) +
    theme(legend.position = "bottom") +
    theme(axis.text = element_text(angle = 0, color="blue",size=15, face=3)) +
    theme(axis.title = element_text(angle = 0, color="black", size=15, face = "bold")) +
    theme(plot.title = element_text(hjust = 0.5, size = 20)) +
    theme(legend.key = element_rect(fill = "white", colour = "black")) +
    theme(legend.text = element_text(size = 15, colour = "Black")) +
    theme(legend.title = element_text(face = "bold"))
  
# RT.ms ~ Sound  * SAPS_Split plot   
RTMeans5 <- aggregate(data = BigTech_FinalExcludes_NoMow, RT.ms ~ Sound * SAPS_Split, mean)
RTMeans5 <- mutate(RTMeans5, Group = c(1:4))
RTMeans5 <- mutate(RTMeans5, SAPS = ifelse(SAPS_Split == "High SAPS", 1, 2))
ggplot(BigTech_FinalExcludes_NoMow, aes(y=RT.ms, x=Sound)) +
  geom_jitter(aes(color=SAPS_Split),alpha = .5, size=.7, width = .34, fill = "black") +
  set.seed(69) +
  geom_point(data = RTMeans5, col = "black", size = 7, shape = 23, fill = RTMeans5$SAPS) +
  scale_colour_manual("SAPS Split", values=c("red", "black")) +
  geom_line(data = RTMeans5, aes(y = RT.ms, group = SAPS_Split, linetype=SAPS_Split), 
            alpha = .9, col = "black", size = 1) +
  scale_fill_manual(values=c("black", "red")) +
  geom_hline(yintercept = mean(RTMeans5$RT.ms)) +
  labs(title = "Sound RT between SAP", x = "Trial Type", y = "RT (ms)", 
       size = 12) +
  scale_y_continuous(limits = c(400, 500), breaks = seq(400, 500, 25)) +
  scale_x_discrete(labels = c("control" = "Control","phone" = "Phone")) +
  theme_classic(base_size = 14)+
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(angle = 0, color="blue",size=15, face=3)) +
  theme(axis.title = element_text(angle = 0, color="black", size=15, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  theme(legend.key = element_rect(fill = "white", colour = "black")) +
  theme(legend.text = element_text(size = 15, colour = "Black")) +
  theme(legend.title = element_text(face = "bold"))
  
# Creativity ####
install.packages("apaTables")
library(apaTables)
sfnc2020 <- read.csv("SfNC2020.csv")
SfNCTable1 <- apaTables::apa.cor.table(sfnc2020, filename="Table1_APA_SfNC.doc", table.number=1)
