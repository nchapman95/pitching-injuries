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

rownames(pitching) <- paste(as.character(pitching$Name), as.character(pitching$Season))


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
#write.csv(cols_to_drop,"/Users/nchapman/Documents/Documents/MSAS/spring_2020/MAT8406/Pitcher Injury Project/colstodrop.csv")



## Model Initialization


reg.2 <- glm(formula = injury_lagged ~ Pitches + Age + P_per_IP_accel_2 + prior_injury + prior_injury_last_2 + 
               P_per_IP + IP_accel_2 + wSI..pi._accel_2 + wFA..pi._accel_2 + walk_rate + win_rec + IP_per_G +
               vFA..pi. + FA...pi. + tj + SI.Z..pi. + FA.Z..pi. + vFC..pi._accel_2 + wild_pitch_rate_accel_2
             , family = "binomial", data = pitching)
summary(reg.2)



## Step Function

null <-glm(formula = injury_lagged ~ 1
           , family = "binomial",data = pitching) 

full<-glm(formula = injury_lagged ~ ( Pitches + Age + P_per_IP_accel_2 + prior_injury + prior_injury_last_2 + 
                                        P_per_IP + IP_accel_2 + wSI..pi._accel_2 +
                                        wFA..pi._accel_2 + walk_rate + win_rec + IP_per_G +
                                        vFA..pi. + FA...pi. + tj + SI.Z..pi. +
                                        FA.Z..pi. + vFC..pi._accel_2 + wild_pitch_rate_accel_2) ^ 2  
          , family = "binomial",data = pitching)
summary(full)


test_models(reg.2)


fwd_lrt_test <- step(null, direction="forward", test ="none",
                scope=list(lower=formula(null),upper=formula(full)))

### Failed to reject all 3 tests
reg.3 <- stepAIC(reg.2, direction="backward",steps=1)
### Failed to reject all 3 tests
reg.4 <- stepAIC(reg.3, direction="backward",steps=1)
### Failed to reject all 3 tests
reg.5 <- stepAIC(reg.4, direction="backward",steps=1)
### Failed to reject all 3 tests
reg.6 <- stepAIC(reg.5, direction="backward",steps=1)
### Failed to reject all 3 tests
reg.7 <- stepAIC(reg.6, direction="backward",steps=1)
### Failed to reject all 3 tests
reg.8 <- stepAIC(reg.7, direction="backward",steps=1)
### Failed to reject all 3 tests
reg.9 <- stepAIC(reg.8, direction="backward",steps=1)
### Failed to reject all 3 tests
reg.10 <- stepAIC(reg.9, direction="backward",steps=1)
### Failed to reject all 3 tests
reg.11 <- stepAIC(reg.10, direction="backward",steps=1)
### Failed to reject all 3 tests
reg.12 <- stepAIC(reg.11, direction="backward",steps=1)
### Failed to reject all 3 tests
reg.13 <- stepAIC(reg.12, direction="backward",steps=1)
### Failed to reject all 3 tests
reg.14 <- stepAIC(reg.13, direction="backward",steps=1)



### Rejected all 3 tests.
reg.15 <- stepAIC(reg.14, direction="forward",steps=1, scope=list(lower=formula(reg.14), upper=formula(full)))
### Failed to Reject all 3 tests.
reg.16 <- stepAIC(reg.15, direction="forward",steps=1, scope=list(lower=formula(reg.15), upper=formula(full)))
### Rejected 2 tests. Accepted

reg.18 <- glm(formula = injury_lagged ~ P_per_IP_accel_2 + prior_injury_last_2 
              + walk_rate + win_rec + IP_per_G + vFA..pi. +  P_per_IP + IP_accel_2 + 
                  tj + walk_rate:IP_per_G + P_per_IP:IP_accel_2, 
              family = "binomial", data = pitching)

### Denied
reg.19 <- glm(formula = injury_lagged ~ P_per_IP_accel_2 + prior_injury_last_2 
              + walk_rate + win_rec + IP_per_G + vFA..pi. +  P_per_IP + IP_accel_2 + 
                tj + walk_rate:IP_per_G + P_per_IP:IP_accel_2 , 
              family = "binomial", data = pitching)
### Denied
reg.20 <- glm(formula = injury_lagged ~ P_per_IP_accel_2 + prior_injury_last_2 
              + walk_rate + win_rec + IP_per_G + vFA..pi. +  P_per_IP + IP_accel_2 + 
                tj + walk_rate:IP_per_G + P_per_IP:IP_accel_2 + P_per_IP_accel_2:walk_rate  , 
              family = "binomial", data = pitching)
### Denied
reg.21 <- glm(formula = injury_lagged ~ P_per_IP_accel_2 + prior_injury_last_2 
              + walk_rate + win_rec + IP_per_G + vFA..pi. +  P_per_IP + IP_accel_2 + 
                tj + walk_rate:IP_per_G + P_per_IP:IP_accel_2 + prior_injury_last_2:IP_accel_2, 
              family = "binomial", data = pitching)
### Denied
reg.22 <- glm(formula = injury_lagged ~ P_per_IP_accel_2 + prior_injury_last_2 
              + walk_rate + win_rec + IP_per_G + vFA..pi. +  P_per_IP + IP_accel_2 + 
                tj + walk_rate:IP_per_G + P_per_IP:IP_accel_2 +  FA...pi. + FA.Z..pi. + FA...pi.:FA.Z..pi., 
              family = "binomial", data = pitching)
### Denied
reg.23 <- glm(formula = injury_lagged ~ P_per_IP_accel_2 + prior_injury_last_2 
              + walk_rate + win_rec + IP_per_G + vFA..pi. +  P_per_IP + IP_accel_2 + wFA..pi._accel_2 +
                tj + walk_rate:IP_per_G + P_per_IP:IP_accel_2 + P_per_IP:wFA..pi._accel_2 , 
              family = "binomial", data = pitching)
### Admit
reg.24 <- glm(formula = injury_lagged ~ P_per_IP_accel_2 + prior_injury_last_2 + 
              + walk_rate + win_rec + IP_per_G + vFA..pi. +  P_per_IP + IP_accel_2 
              + wFA..pi._accel_2 + wSI..pi._accel_2 + tj +
                walk_rate:IP_per_G + P_per_IP:IP_accel_2 + wSI..pi._accel_2:wFA..pi._accel_2
               , 
              family = "binomial", data = pitching)

### Admit
reg.25 <- glm(formula = injury_lagged ~ P_per_IP_accel_2 + prior_injury_last_2 + 
                + walk_rate + win_rec + IP_per_G + vFA..pi. +  P_per_IP + IP_accel_2 
              + wFA..pi._accel_2 + wSI..pi._accel_2 + tj + prior_injury_last_2:wSI..pi._accel_2 +
                walk_rate:IP_per_G + P_per_IP:IP_accel_2 + wSI..pi._accel_2:wFA..pi._accel_2
              , 
              family = "binomial", data = pitching)
### 
reg.26 <- glm(formula = injury_lagged ~ P_per_IP_accel_2 + prior_injury_last_2 + 
                + walk_rate + win_rec + IP_per_G + vFA..pi. +  P_per_IP + IP_accel_2 + FA...pi. +
              + wFA..pi._accel_2 + wSI..pi._accel_2 + tj + prior_injury_last_2:wSI..pi._accel_2 +
                walk_rate:IP_per_G + P_per_IP:IP_accel_2 + wSI..pi._accel_2:wFA..pi._accel_2 + 
                FA...pi.:P_per_IP_accel_2 , 
              family = "binomial", data = pitching)


## Tested Models reg.2 to reg.26 using various functions.
test_models(reg.25, reg.26)








#####################################################################################################################
# What is going on. 

test_models_back <- function(x) {
  
  y <- stepAIC(x, direction="backward",steps=1)
  rbind(cbind("AIC",AIC(y) < AIC(x)),
        cbind("WALD",waldtest(x,y)$`Pr(>F)`[2] > .05),
        cbind("LRT",lrtest(x,y)$`Pr(>Chisq)`[2] > .05))
}

test_models_forward <- function(x) {
  
  y <- stepAIC(x, direction="forward",steps=1, scope=list(lower=formula(x), upper=formula(full)))
  rbind(cbind("AIC",AIC(y) < AIC(x)),
  cbind("WALD",waldtest(x,y)$`Pr(>F)`[2] < .05),
  cbind("LRT",lrtest(x,y)$`Pr(>Chisq)`[2] < .05))
}

test_models <- function(x, y) {
  
  rbind(cbind("AIC",AIC(y) < AIC(x)),
        cbind("WALD",waldtest(x,y)$`Pr(>F)`[2]),
        cbind("LRT",lrtest(x,y)$`Pr(>Chisq)`[2]))
}


#####################################################################################################################
## VIF 


### Fit Model with just the continous random variables. See if there linear combination creates high vifs.

# centering with 'scale()'
center_scale <- function(x) {
  scale(x)
}


Data <- pitching[c("P_per_IP_accel_2", "walk_rate", "win_rec", "FA...pi.",
                   "IP_per_G","vFA..pi.", "P_per_IP","IP_accel_2", "wFA..pi._accel_2",
                   "wSI..pi._accel_2")]


# apply it
Trans_Data <- as.data.frame(center_scale(Data))

fwd_vif <- glm(formula = sinkers_cutters_percent ~ P_per_IP_accel_2 + 
                 + walk_rate + win_rec + IP_per_G + vFA..pi. +  P_per_IP + IP_accel_2 + FA...pi. +
                 + wFA..pi._accel_2 + wSI..pi._accel_2 + walk_rate:IP_per_G + 
                 P_per_IP:IP_accel_2 + wSI..pi._accel_2:wFA..pi._accel_2 + 
                 FA...pi.:P_per_IP_accel_2  , data = Trans_Data)


sort(vif(fwd_vif), decreasing = TRUE)
sort(vif(reg.26), decreasing = TRUE)

#write.csv(as.data.frame(vif(fwd_vif)),
#          "/Users/nchapman/Documents/Documents/MSAS/spring_2020/MAT8406/Pitcher Injury Project/vif_transformed.csv")
#write.csv(as.data.frame(vif(reg.24)),
#          "/Users/nchapman/Documents/Documents/MSAS/spring_2020/MAT8406/Pitcher Injury Project/vif.csv")




















## Testing for Outliers/ Influence Points




influencePlot(reg.24,col="red")


players <- as.character(pitching$Name)

outliers <- data.frame(pitching$Season,
                       pitching$Name, 
                       pitching$injury_lagged,
                       ##deviance residuals
                       residuals(reg.24),
                       ## pearson residuals
                       residuals(reg.24, type = "pearson"),
                       ## Leverage
                       hatvalues(reg.24)
                       )

write.csv(outliers,"/Users/nchapman/Documents/Documents/MSAS/spring_2020/MAT8406/Pitcher Injury Project/outliers.csv", row.names = FALSE)

plot(fwd_aic)

## DF Betas

inf1 <- influence.measures(reg.24)

influencePlot(reg.24, col=cols, scale=8, n=2)

dfbetas <- data.frame(inf1$infmat[,2:19])


cols=ifelse (pitching$injury_lagged == 1, "red", "blue")


plot_beta <- function(x) {

op <- par(mar=c(5,5,1,1)+.1)
plot(dfbetas[,x], type = "h", col=cols,
     xlab="Observation index",
     ylab=expression(Delta * beta[P_per_IP:IP_accel_2]),
     cex.lab=1.3)
points(dfbetas[,x], col=cols)
# label some points
big <- abs(dfbetas[,x]) > .25
idx <- 1:nrow(dfbetas)

text(idx[big], dfbetas[big,x], label=rownames(dfbetas)[big],
     cex=0.9, pos=ifelse(dfbetas[big,x]>0, 3, 1),
     xpd=TRUE)

abline(h=c(-.25, 0, .25), col="gray")
par(op)
}

plot_beta(17)





## Inference and Result Communication
## Interpreting the Betas. Confidence Intervals. Interpreting relationships between variables, and with the dependent variable. 


residualPlots(fwd_aic)



fa_x = exp((0.69*0.366233 + .69*-0.981659) - (0.59*0.366233 + .59*-0.981659))
fa_x

p_i_x = exp((0.189058) + (.434982))
p_i_x

p_p_fa_x = exp((4*-0.013548 + 4*0.040647) - (2*-0.013548 + 2*0.040647))
p_p_fa_x

p_p_op_i_x = exp((4*-0.064775 + 4*0.040647 + 4*0.189058) - (2*-0.064775+ 2*0.040647 + 2*0.189058))
p_p_op_i_x

p_p_op_ni_x = exp((4*0.040647 + 4*-0.064775 ) - (2*0.040647 + 2*-0.064775 ))
p_p_op_ni_x

w_r_x = exp((0.7*0.700898) - (0.6*0.700898))
w_r_x








#For Predictive Model. Not for Final Inferential Model


## Accuracy/Recall comparison

ptest <- as.numeric(reg.2$fitted> # Binary vector set to 1 if the prob.
                      0.3) # exceeds 0.50 and 0 otherwise
table(pitching$injury_lagged,ptest) # Contingency table of test outcomes

ptest <- as.numeric(reg.26$fitted> # Binary vector set to 1 if the prob.
                      0.3) # exceeds 0.50 and 0 otherwise
table(pitching$injury_lagged,ptest) # Contingency table of test outcomes





## AUC Plot Training
invisible(plot(roc(factor(pitching$injury_lagged),
                   fitted(reg.26)),
               print.auc = T,
               col = "red", 
               main = "ROC curve"))



## Holdout Sample

library(pROC)


pitching_holdout <- read.csv("/Users/nchapman/Documents/Documents/MSAS/spring_2020/MAT8406/Pitcher Injury Project/pitching_holdout_data.csv",
                             sep = '|',
                             header =TRUE)

attach(pitching_holdout)

pitching_holdout$predictions <- predict(reg.26, pitching_holdout)

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


## Holdout 2020; Predicting the Future


pitching_holdout_2020 <- read.csv("/Users/nchapman/Documents/Documents/MSAS/spring_2020/MAT8406/Pitcher Injury Project/pitching_holdout_2020_data.csv",
                             sep = '|',
                             header =TRUE)

attach(pitching_holdout_2020)



pitching_holdout_2020$predictions <- predict(reg.26, pitching_holdout_2020)

pitching_holdout_2020$predictions_prob <- exp(pitching_holdout_2020$predictions) / (1.0 + exp(pitching_holdout_2020$predictions))

ptest <- as.numeric(pitching_holdout_2020$predictions_prob> # Binary vector set to 1 if the prob.
                      0.3) # exceeds 0.50 and 0 otherwise

table(pitching_holdout_2020$injury_lagged,ptest) # Contingency table of test outcomes


hist(pitching_holdout_2020$predictions_prob, main="Injuries in 2020", xlab = "Probability",col="blue")

