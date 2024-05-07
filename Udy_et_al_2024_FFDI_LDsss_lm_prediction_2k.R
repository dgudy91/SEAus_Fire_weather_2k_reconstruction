remove(list = ls())  #clear environment
options(scipen = 999) #turn off scientific notation
library(tidyverse)
library(patchwork)
#library(zoo)
#library(tidyquant)
library(ggplot2)
library(greybox)
#library(finalfit)
library(dplyr)
library(olr)
library(hydroGOF)


#### Load data ####
dat <- read_csv("C:/Users/dgudy/Dropbox/PhD_UTAS/Data_Analysis/run_025/ice_core_DSS/FFDI_LDsss_regression/JYear_FFDI_area-average_ESB_lines_ACT_and_VicAlps_added_Hunter_Lakes_Entrance_valid_geometry_LDsss.csv")
LDsss_2kdata <- read_csv("C:/Users/dgudy/Dropbox/PhD_UTAS/data/Ice_cores/LawDome/DSS_2k_data_compilation/LDsss_2k_record.csv")

LDsss_2kdata_clean <- na.omit(LDsss_2kdata)

#convert Jan Year to factor

dat$Year = dat$Jan_Year
dat$Jan_Year = as.factor(dat$Jan_Year)
dat$timestep = as.numeric(dat$Jan_Year)
#subsample to LDsss values
ds <- dat[1:66,]
ds_train = dat[10:40,] #1959/1960 to 1989/1990 
ds_val = dat[44:66,] #1994 to 2015/16 - avoid 1991,92 and 93 period around Pinatabo 
ds_post1960 = dat[10:66,]

#### Linear Regression #### 


#define model based on training data: 1960 - 1990
model1 = lm(FFDI_mean~LDsss, data = ds_train)
summary(model1)

#prediction interval gives uncertainty around a single value
#95% prediction interval - relies on assumption that residual errors are normally distributed with a constant variance
# tested this below - model residuals normal distrubtion around 0

pred.train <- predict(model1, interval = "prediction")
training <- cbind(ds_train, pred.train)
#Plot regression, confidence and predict intervals

p1 <- ggplot(training, aes(x = LDsss, y = FFDI_mean, label = Jan_Year)) +
  stat_smooth(method = lm, color = "darkgrey")+
  geom_point(color = "black")+
  geom_text(nudge_y = .2) +
  labs(x = '', y ='FFDI_mean')

  
#Add prediction intervals
p1 + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")



#validate model - 1991 - 2016
val.LDsss = data.frame(LDsss = (ds_val$LDsss))
pred.val = predict(model1, val.LDsss, interval = "prediction")

validation = cbind(ds_val,pred.val)

#validate - full record
full.LDsss = data.frame(LDsss = (ds$LDsss))
pred.full <- predict(model1, full.LDsss, interval = "prediction")

full_obs = cbind(ds,pred.full)

#Add calibration and validation identifier

training$ID = "train"
validation$ID = "validation"

train_val = rbind(training, validation)

p2 <- ggplot(train_val, aes(x = LDsss, y = FFDI_mean, label = Jan_Year, color = ID)) +
  stat_smooth(method = lm, color = "darkgrey")+
  geom_point(color = "black")+
  geom_text(nudge_y = .2) +
  labs(x = '', y ='FFDI_mean')

#Add prediction intervals
p2 + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")

#validate - full record

p3 <- ggplot(full_obs, aes(x = LDsss, y = FFDI_mean, label = Jan_Year)) +
  stat_smooth(method = lm, color = "darkgrey")+
  geom_point(color = "black")+
  geom_text(nudge_y = .2) +
  labs(x = '', y ='FFDI_mean')

#Add prediction intervals
p3 + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")

#prediction over 2000 year dataset

full2k.LDsss = data.frame(LDsss = (LDsss_2kdata_clean$Cl_DJFM))
pred.2k = predict(model1, full2k.LDsss, interval = "prediction")

full2k_prediction = cbind(LDsss_2kdata_clean,pred.2k)

p3 = ggplot(full2k_prediction, aes(x = Year_CE, y = fit))+
  geom_line()+
  theme_classic()
p3

p3 + geom_line(aes(y = lwr), color = "blue")+
  geom_line(aes(y = upr), color = "red")

p4 = ggplot(full2k_prediction, aes(x = Year_CE))+
  geom_line(aes(y = fit))+
  geom_line(aes(y = lwr))+
  geom_line(aes(y = upr))+
  theme_classic()
p4

write.csv(train_val, "C:/Users/dgudy/Dropbox/UTAS_postdoc/Manuscript_prep/Fire_paper/Reviews_Dec23/Data_analysis/train_val_FFDI_mean_predict_fit_lwr_upr_95_prediction_interval.csv")
write.csv(full2k_prediction, "C:/Users/dgudy/Dropbox/UTAS_postdoc/Manuscript_prep/Fire_paper/Reviews_Dec23/Data_analysis/full2k_FFDI_predict_fit_lwr_upr_95_prediction_interval.csv")

#test sensitivity to model training period - train model 1951 to 1990

ds_train2 = dat[1:40,] #1950/51 to 1989/1990 

model2 = lm(FFDI_mean~LDsss, data = ds_train2)  #define model based on training data: 1950 - 1990
model3 = lm(FFDI_mean~LDsss, data = ds)  #define model based on full observational record: 1950-2016
summary(model1) #r2 0.38, p<0.001)
summary(model2) #r2 0.2, p<0.002
summary(model3) #r2 0.16, p<0.0005)



#prediction interval gives uncertainty around a single value
#95% prediction interval - relies on assumption that residual errors are normally distributed with a constant variance
# tested this below - model residuals normal distrubtion around 0

pred.train2 <- predict(model2, interval = "prediction")
training2 <- cbind(ds_train2, pred.train2)
#Plot regression, confidence and predict intervals

p5 <- ggplot(training2, aes(x = LDsss, y = FFDI_mean, label = Jan_Year)) +
  stat_smooth(method = lm, color = "darkgrey")+
  geom_point(color = "black")+
  geom_text(nudge_y = .2) +
  labs(x = '', y ='FFDI_mean')


#Add prediction intervals
p5 + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")


#prediction over 2000 year dataset - using 1950-1990 training period model

#full2k.LDsss = data.frame(LDsss = (LDsss_2kdata_clean$Cl_DJFM))
pred.2k_train2 = predict(model2, full2k.LDsss, interval = "prediction")

full2k_prediction_train2 = cbind(LDsss_2kdata_clean,pred.2k_train2)

p6 = ggplot(full2k_prediction_train2, aes(x = Year_CE, y = fit))+
  geom_line()+
  theme_classic()
p6

#Difference between prediction from 2 training periods
Diff = full2k_prediction$fit - full2k_prediction_train2$fit
plot(Diff)
mean(Diff)

mean(full2k_prediction$fit)
mean(full2k_prediction_train2$fit)
sd(full2k_prediction$fit)
sd(full2k_prediction_train2$fit)

Model_sensitivity = data.frame(cbind(full2k_prediction$Year_CE, full2k_prediction$fit, full2k_prediction_train2$fit))
colnames(Model_sensitivity) = c("YearCE", "Fitmodel1", "Fitmodel2")

ggplot()+
  geom_line(data = Model_sensitivity, aes(x = YearCE, y = Fitmodel1, color = "Model1 (1960-1990)"))+
  geom_line(data = Model_sensitivity, aes(x = YearCE, y = Fitmodel2, color = "Model2 (1951-1990)", alpha = 0.5))+
  theme_classic()+
  xlab("Year CE")+
  ylab("FFDI prediction")+
  scale_color_manual(name = "Model train period", values = c("Model1 (1960-1990)" = "#D81B60" ,"Model2 (1951-1990)" = "#1E88E5"))
  


#Fit model 1 - more variability than fit model 2 but similar mean values. The 6 high 

#### Model performance ####

#training period: 1960 - 1990

#residiuals normal distrubtion and around 0. 
ggplot(data=ds_train, aes(model1$residuals)) +
  geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")

cor(ds_train$LDsss, ds_train$FFDI_mean)
#-0.6300556


#climatological mean - add to dataframe
mean_train = mean(ds_train$FFDI_mean)
mean_validate = mean(ds_val$FFDI_mean)

training_stats = training
training_stats$pred_mean = mean_train

validation_stats = validation
validation_stats$pred_mean = mean_validate

#Nash Sutcliffe efficiency (NSE) - normalised statistic that determines the relative
# magnitude of the residual variance compared to the measured data variance

#NSE indicates how well the plot of observed verses the simulated data fits the 1:1 line
# results range from -inf to 1. Closer to 1 = more accurate model
# NSE = 1 perfect match between modelled and observed
# NSE = 0 indicates the model predictions are as accurate as the mean of the observed data
# NSE < 0 indicates that the observed mean is a better predictor than the model

train_obs = training_stats$FFDI_mean
train_pred_recon = training_stats$fit
train_pred_cmean = training_stats$pred_mean


val_obs = validation_stats$FFDI_mean
val_pred_recon = validation_stats$fit
val_pred_cmean = validation_stats$pred_mean

#training period  
NSE_train = NSE(train_pred_recon, train_obs, na.rm = TRUE)
#0.3969701

#adjusted r2
traini = lm(train_obs ~ train_pred_recon)
summary(traini)

rmse(train_pred_recon, train_obs)
#[1] 1.816159

rPearson(train_pred_recon, train_obs)
#[1] 0.6300556

rSpearman(train_pred_recon, train_obs)
#[1] 0.5483871

#validation period
NSE_val = NSE(val_pred_recon, val_obs, na.rm = TRUE)
#0.0479014

rmse(val_pred_recon, val_obs)
#[1] 2.321964

#adjusted r2
vali = lm(val_obs ~ val_pred_recon)
summary(vali)

rPearson(val_pred_recon, val_obs)
#[1] 0.4210504

rSpearman(val_pred_recon, val_obs)
#[1] 0.4189723

#1960 - 2016
obs = train_val$FFDI_mean
pred = train_val$fit

NSE(pred, obs, na.rm = TRUE)
#[1] 0.2169247

rmse(pred, obs)
#2.046932

#adjusted r2
fullr2 = lm(obs~pred)
summary(fullr2)

rPearson(pred, obs)

rSpearman(pred, obs)

#1950 - 2016
obs = full_obs$FFDI_mean
pred = full_obs$fit

NSE(pred, obs, na.rm = TRUE)
#[1] 0.1248616

rmse(pred, obs)
#2.046932

#adjusted r2
fullr2 = lm(obs~pred)
summary(fullr2)

rPearson(pred, obs)

rSpearman(pred, obs)


