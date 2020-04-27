source("/Users/nchapman/Documents/Documents/MSAS/spring_2020/MAT8406/MAT8406-Regression-Methods/pairs.panels.r")

library(dplyr)
library(arm)
library(nortest)
library(lmtest)
library(MPV)
library(pROC)
library(plotROC)
library(ggplot2)
library(glmnet)
library(MPV)
library(leaps)
library(car)
library(ResourceSelection)
library(xtable)
library(gganimate)
library(lmerTest)

pitching <- read.csv("/Users/nchapman/Documents/Documents/MSAS/spring_2020/MAT8406/Pitcher Injury Project/pitching_data.csv",
                     sep = '|',
                     header =TRUE)

attach(pitching)




## Multicollinearity

correlations <- cor(pitching[,-1:-4])
correlations[upper.tri(correlations)]<-""
correlations<-as.data.frame(correlations)
correlations
write.csv(correlations,"/Users/nchapman/Documents/Documents/MSAS/spring_2020/MAT8406/Pitcher Injury Project/correlations.csv")


# Drop Pitches, Sinkers_cutters_percent, Pitches Accel , SI % (PI)

###

cols_to_drop <- cor(pitching[c("Pitches","IP", "IP_accel_2","Pitches_accel_2","FA...pi.","sinkers_cutters_percent")])
cols_to_drop[upper.tri(cols_to_drop)]<-""
cols_to_drop <-  as.data.frame(cols_to_drop)
write.csv(cols_to_drop,"/Users/nchapman/Documents/Documents/MSAS/spring_2020/MAT8406/Pitcher Injury Project/colstodrop.csv")

### Transformations

hist(sqrt(G))
hist(sqrt(Pitches))
hist(FA...pi.)


## Create Weights

pitching$w <- ifelse(pitching$injury_lagged == 1, 3,1)
mean(pitching$w * pitching$injury_lagged)


## Model Initialization

reg.1 <- glm(formula = injury_lagged ~ (.)^2
             , family = "binomial",data = pitching[,-1:-4])

summary(reg.1)



pitching$years_since_tj_trans <- log(years_since_tj + 1)



## Model Selection Forward Selection


reg.2 <- glm(formula = injury_lagged ~ Pitches + Age + P_per_IP_accel_2 + prior_injury + prior_injury_last_2 + 
               P_per_IP + IP_accel_2 + wSI..pi._accel_2 + wFA..pi._accel_2 + walk_rate + win_rec + 
               vFA..pi. + FA...pi. + tj + SI.Z..pi. + FA.Z..pi. + vFC..pi._accel_2 + wild_pitch_rate_accel_2 +
               + (vFA..pi.*FA...pi.) + (vFA..pi.*tj ) + (IP_accel_2 * FA...pi.) + (P_per_IP * IP_accel_2)  +
               (SI.Z..pi. * tj) + (FA.Z..pi. * FA...pi.) + (SI.Z..pi. * wSI..pi._accel_2) + (vSL..pi._accel_2*IP_accel_2 )
             + (P_per_IP * wFA..pi._accel_2) + (wSI..pi._accel_2 * wFA..pi._accel_2) + (vFC..pi._accel_2 * FA...pi.)
             + (wild_pitch_rate_accel_2 * tj) + (Age * P_per_IP_accel_2)
             , family = "binomial", data = pitching)
summary(reg.2)



## Step Function

null <-glm(formula = injury_lagged ~ 1
           , family = "binomial",data = pitching) 

full<-glm(formula = injury_lagged ~ (Pitches + Age + P_per_IP_accel_2 + prior_injury + prior_injury_last_2 + 
                                       P_per_IP + IP_accel_2 
                                     + wSI..pi._accel_2 + wFA..pi._accel_2 
                                     + walk_rate + win_rec + 
                                       vFA..pi. + FA...pi. + tj + SI.Z..pi. 
                                     + FA.Z..pi. + vFC..pi._accel_2 + wild_pitch_rate_accel_2) ^ 2  
          , family = "binomial",data = pitching)
summary(full)




fwd_lrt <- step(null,direction="forward", test ="none",
            scope=list(lower=formula(null),upper=formula(full)))

fwd_aic <- step(reg.2,direction="backward")

fwd_lrt <- step(reg.2,direction='backward', test='LRT')

lrtest(reg.2, fwd_lrt)



fwd_lrt_test <- step(null, direction="forward", test ="none",
                scope=list(lower=formula(null),upper=formula(full)))



null$aic

#####################################################################################################################
# What is going on. 

test1 <- glm(formula = injury_lagged ~ prior_injury + walk_rate + win_rec + vFA..pi. + 
              Pitches + FA...pi. + tj + Age + P_per_IP_accel_2 + walk_rate:Pitches + 
              prior_injury:FA...pi. + prior_injury:Pitches + Pitches:P_per_IP_accel_2 + vFA..pi.:FA...pi., 
            family = "binomial", data = pitching)


lrtest(test1,fwd_lrt)

rbind(cbind(test1$aic, "test"), cbind(fwd_lrt$aic,"lrt"), cbind(fwd_aic$aic,"aic"), cbind(reg.2$aic,"reg.2"))



#####################################################################################################################
## VIF 


### Fit Model with 




sort(vif(fwd_aic), decreasing = TRUE)

sort(vif(fwd_lrt), decreasing = TRUE)






## Accuracy/Recall comparison


ptest <- as.numeric(reg.2$fitted> # Binary vector set to 1 if the prob.
                      0.3) # exceeds 0.50 and 0 otherwise
table(pitching$injury_lagged,ptest) # Contingency table of test outcomes

ptest <- as.numeric(fwd_aic$fitted> # Binary vector set to 1 if the prob.
                      0.3) # exceeds 0.50 and 0 otherwise
table(pitching$injury_lagged,ptest) # Contingency table of test outcomes


ptest <- as.numeric(fwd_lrt$fitted> # Binary vector set to 1 if the prob.
                      0.3) # exceeds 0.50 and 0 otherwise
table(pitching$injury_lagged,ptest) # Contingency table of test outcomes






## AUC Plot Training
invisible(plot(roc(factor(pitching$injury_lagged),
               fitted(fwd_aic)),
               #print.auc = T,
               col = "red", 
               main = "ROC curve"))

invisible(plot(roc(factor(pitching$injury_lagged),
                   fitted(full)),
              print.auc = T, 
               col = "blue", 
               add = T))




## Holdout

library(pROC)



pitching_holdout <- read.csv("/Users/nchapman/Documents/Documents/MSAS/spring_2020/MAT8406/Pitcher Injury Project/pitching_holdout_data.csv",
                             sep = '|',
                             header =TRUE)

attach(pitching_holdout)

pitching_holdout$predictions <- predict(full, pitching_holdout)

pitching_holdout$predictions_prob <- exp(pitching_holdout$predictions) / (1.0 + exp(pitching_holdout$predictions))

ptest <- as.numeric(pitching_holdout$predictions_prob> # Binary vector set to 1 if the prob.
                      0.30) # exceeds 0.50 and 0 otherwise

table(pitching_holdout$injury_lagged,ptest) # Contingency table of test outcomes


roc_obj <- roc(pitching_holdout$injury_lagged, pitching_holdout$predictions_prob)
auc(roc_obj)






## AUC Plot Training
invisible(plot(roc(factor(pitching_holdout$injury_lagged),
                   pitching_holdout$predictions_prob),
               print.auc = T,
               col = "red", 
               main = "ROC curve"))




## Predictions

pitching_holdout[(pitching_holdout$predictions_prob > .2) &
                   (pitching_holdout$injured == 1), ][c("Name","Season","injured",'predictions_prob')]




## Through A regular logistic regression model, and backward elimination using the likelihood ratio test as the criteria.
## The model turns out not to fit the data all That well. 
## With Logistic Regression, I assessed the model fit using the recall rate, or how well the model fits the 
## Predicted class. Even at modest probability cutoffs, the capture rate was quite low, leading me to doubt the 
## efficacy of the proposed model. 




# Make predictions on the test data
x.test <- model.matrix(injury_lagged ~ Pitches  + prior_injury_last_2 + Age + P_per_IP_accel_2 + 
                         P_per_IP + IP_accel_2 + wSI..pi._accel_2 + wFA..pi._accel_2 + walk_rate + 
                         vFA..pi. + FA...pi. + tj + SI.Z..pi. + FA.Z..pi. + vFC..pi._accel_2 + wild_pitch_rate_accel_2 + 
                         + (vFA..pi.*FA...pi.) + (vFA..pi.*tj ) + (IP_accel_2 * FA...pi.) + (P_per_IP * IP_accel_2) + 
                         (SI.Z..pi. * tj) + (FA.Z..pi. * FA...pi.) + (SI.Z..pi. * wSI..pi._accel_2)
                       + (P_per_IP * wFA..pi._accel_2) + (wSI..pi._accel_2 * wFA..pi._accel_2) + (vFC..pi._accel_2 * FA...pi.)
                       + (wild_pitch_rate_accel_2 * tj) + (Age * P_per_IP_accel_2) + (P_per_IP_accel_2 * FA.Z..pi.)
                       + (P_per_IP_accel_2 * vFC..pi._accel_2) + (walk_rate*vFA..pi.) , pitching)[,-1]
probabilities <- rr.1 %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.01, 1, 0)

# Model accuracy
observed.classes <- pitching$injury_lagged
mean(predicted.classes == observed.classes)
table(observed.classes, predicted.classes)




pitching$predictions <- reg.1$fitted

cb_bdown <- pitching %>% mutate(fbvs = cut(vFA..pi._accel_2, breaks = 5)) %>% 
  group_by(fbvs) %>% 
  summarise( mean.injuries_prop = mean(injury_lagged),
             mean.injured_pred= mean(predictions),
             median.ages = median(Age),
             median.fbvs = median(vFA..pi._accel_2))
cb_bdown






## Inference and Result Communication
## Interpreting the Betas. Confidence Intervals. Interpreting relationships between variables, and with the dependent variable. 


residualPlots(fwd_aic)



fa_x = 0.59*8.727e-01 - 0.58*8.727e-01
delta_or= exp(fa_x)
delta_or


p_i_x = 1*6.347e-01
delta_s_c_x =exp(p_i_x)
delta_s_c_x

b_w_x = 10*1.664e-02 - 9*1.664e-02 
delta_b_w_x =exp(b_w_x)
delta_b_w_x

b_w_r = .1*-8.602e+00 - .05*-8.602e+00
delta_b_w_r =exp(b_w_r)
delta_b_w_r

## Testing for Outliers/ Influence Points

influencePlot(fwd_aic,col="red")


players <- as.character(pitching$Name)

outliers <- data.frame(pitching$Season,
                       pitching$injury_lagged,
                       players,
                       rstudent(fwd_aic),
                       hatvalues(fwd_aic) ,
                       cooks.distance(fwd_aic),
                       dffits(fwd_aic) ,
                       dfbetas(fwd_aic))

write.csv(outliers,"/Users/nchapman/Documents/Documents/MSAS/spring_2020/MAT8406/Pitcher Injury Project/outliers.csv", row.names = FALSE)




## Holdout 2020


pitching_holdout_2020 <- read.csv("/Users/nchapman/Documents/Documents/MSAS/spring_2020/MAT8406/Pitcher Injury Project/pitching_holdout_2020_data.csv",
                             sep = '|',
                             header =TRUE)

attach(pitching_holdout_2020)



pitching_holdout_2020$predictions <- predict(fwd_aic, pitching_holdout_2020)

pitching_holdout_2020$predictions_prob <- exp(pitching_holdout_2020$predictions) / (1.0 + exp(pitching_holdout_2020$predictions))

ptest <- as.numeric(pitching_holdout_2020$predictions_prob> # Binary vector set to 1 if the prob.
                      0.3) # exceeds 0.50 and 0 otherwise

table(pitching_holdout_2020$injury_lagged,ptest) # Contingency table of test outcomes


hist(pitching_holdout_2020$predictions_prob, main="Injuries in 2020", xlab = "Probability",col="blue")

