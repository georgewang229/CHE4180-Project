#CHE4180 S1 2020

#FIJI TWS image analysis project


#Load Packages------------
library(raster)
library(tidyverse)
library(ROCR)
library(rgdal)

#Load the collected data-------
intden <- read_csv("https://raw.githubusercontent.com/georgewang229/CHE4180-Project/master/CHE4180_FINAL_IntDen_Summary.csv")


#Integrated Density Error Analysis--------------
intden0 <- intden %>%
  select(Frame, RF0, NB0, Man0) %>%
  mutate(`Random Forest` = (RF0 - Man0)/Man0,
         `Naive Bayesian` = (NB0 - Man0)/Man0) %>%
  pivot_longer(c(`Random Forest`, `Naive Bayesian`), names_to = "ErrorType", values_to = "Error") %>%
  mutate(Time = "0 min")

intden5 <- intden %>%
  select(Frame, RF5, NB5, Man5, RF5_0min) %>%
  mutate(`Random Forest` = (RF5 - Man5)/Man5,
         `Naive Bayesian` = (NB5 - Man5)/Man5,
         `0min Random Forest` = (RF5_0min - Man5)/Man5) %>%
  pivot_longer(c(`Random Forest`, `Naive Bayesian`, `0min Random Forest`), names_to = "ErrorType", values_to = "Error") %>%
  mutate(Time = "05 min")

intden10 <- intden %>%
  select(Frame, RF10, NB10, Man10, RF10_0min) %>%
  mutate(`Random Forest` = (RF10 - Man10)/Man10,
         `Naive Bayesian` = (NB10 - Man10)/Man10,
         `0min Random Forest` = (RF10_0min - Man10)/Man10) %>%
  pivot_longer(c(`Random Forest`, `Naive Bayesian`, `0min Random Forest`), names_to = "ErrorType", values_to = "Error") %>%
  mutate(Time = "10 min")

intden15 <- intden %>%
  select(Frame, RF15, NB15, Man15, RF15_0min) %>%
  mutate(`Random Forest` = (RF15 - Man15)/Man15,
         `Naive Bayesian` = (NB15 - Man15)/Man15,
         `0min Random Forest` = (RF15_0min - Man15)/Man15) %>%
  pivot_longer(c(`Random Forest`, `Naive Bayesian`, `0min Random Forest`), names_to = "ErrorType", values_to = "Error") %>%
  mutate(Time = "15 min")

#Error table
error0 <- intden0 %>%
  select(Frame, ErrorType, Error) %>%
  mutate(Time = "0 min")

error5 <- intden5 %>%
  select(Frame, ErrorType, Error) %>%
  mutate(Time = "05 min")

error10 <- intden10 %>%
  select(Frame, ErrorType, Error) %>%
  mutate(Time = "10 min")

error15 <- intden15 %>%
  select(Frame, ErrorType, Error) %>%
  mutate(Time = "15 min")

error_all <- bind_rows(error0, error5, error10, error15)%>%
  rename(Model = ErrorType)

#Error plot
mycols <- c("red", "yellow3", "green")

error_all %>%
  ggplot(aes(x = Frame, y = abs(Error) * 100, colour = Model)) +
  geom_line(size = 1) +
  facet_grid(vars(Time)) +
  labs(y = "Absolute Percentage Error %",
       title = "Absolute Percentage Error on Integrated Density Measurements") +
  scale_colour_manual(name = "Deep Learning Model",
                      values = mycols)

#ROC & performance measures------------------


#0min Performance Evaluation---------------
table_0min <- tibble(Frame = numeric(),
                     Type = character(),
                     Accuracy = numeric(),
                     Sensitivity = numeric(),
                     Specificity = numeric(),
                     Precision = numeric(),
                     `F-score` = numeric()) #Pre-allocate the tibble

for(i in 1:100) {
  #Download and Read in manual image file (.tif)
  download.file(paste0("https://github.com/georgewang229/CHE4180-Project/blob/master/ROC%20Prep%200min/0min_Manual_slice", i, ".tif?raw=true"),
                manual_0min <- tempfile())
  manual <- readGDAL(manual_0min)
  manual$band1[1] <- 0

  
  #Download and Read in NB image file (.tif)
  download.file(paste0("https://github.com/georgewang229/CHE4180-Project/blob/master/ROC%20Prep%200min/0min_NB_slice", i, ".tif?raw=true"),
                NB_0min <- tempfile())
  NB <- readGDAL(NB_0min)
  NB$band1[1] <- 0

  
  
  #Download and Read in RF image file (.tif)
  download.file(paste0("https://github.com/georgewang229/CHE4180-Project/blob/master/ROC%20Prep%200min/0min_RF_slice", i, ".tif?raw=true"),
                RF_0min <- tempfile())
  RF <- readGDAL(RF_0min)
  RF$band1[1] <- 0

  
  #Create prediction objects 
  RF_pred <- prediction(RF$band1, manual$band1)
  NB_pred <- prediction(NB$band1, manual$band1)
  
  
  #Performance evaluation
  RF_acc <- performance(RF_pred, "acc")@y.values[[1]][2]
  RF_sens <- performance(RF_pred, "sens")@y.values[[1]][2]
  RF_spec <- performance(RF_pred, "spec")@y.values[[1]][2]
  RF_prec <- performance(RF_pred, "prec")@y.values[[1]][2]
  RF_F <- performance(RF_pred, "f")@y.values[[1]][2]

  NB_acc <- performance(NB_pred, "acc")@y.values[[1]][2]
  NB_sens <- performance(NB_pred, "sens")@y.values[[1]][2]
  NB_spec <- performance(NB_pred, "spec")@y.values[[1]][2]
  NB_prec <- performance(NB_pred, "prec")@y.values[[1]][2]
  NB_F <- performance(NB_pred, "f")@y.values[[1]][2]
  
  #Add performance measures to the tibble
  table_0min <- table_0min %>% add_row(Frame = i, Type = "Random Forest", Accuracy = RF_acc, Sensitivity = RF_sens, 
                         Specificity = RF_spec, Precision = RF_prec, `F-score` = RF_F)
  
  table_0min <- table_0min %>% add_row(Frame = i, Type = "Naive Bayesian", Accuracy = NB_acc, Sensitivity = NB_sens, 
                                       Specificity = NB_spec, Precision = NB_prec, `F-score` = NB_F)
  
  }





#5min Performance Evaluation----------------------
table_5min <- tibble(Frame = numeric(),
                     Type = character(),
                     Accuracy = numeric(),
                     Sensitivity = numeric(),
                     Specificity = numeric(),
                     Precision = numeric(),
                     `F-score` = numeric()) #Pre-allocate the tibble

for(i in 1:100) {
  #Download and Read in manual image file (.tif)
  download.file(paste0("https://github.com/georgewang229/CHE4180-Project/blob/master/ROC%20Prep%205min/5min_Manual_slice", i, ".tif?raw=true"),
                manual_5min <- tempfile())
  manual <- readGDAL(manual_5min)
  manual$band1[1] <- 0
  
  
  #Download and Read in NB image file (.tif)
  download.file(paste0("https://github.com/georgewang229/CHE4180-Project/blob/master/ROC%20Prep%205min/5min_NB_slice", i, ".tif?raw=true"),
                NB_5min <- tempfile())
  NB <- readGDAL(NB_5min)
  NB$band1[1] <- 0
  
  
  
  #Download and Read in RF image file (.tif)
  download.file(paste0("https://github.com/georgewang229/CHE4180-Project/blob/master/ROC%20Prep%205min/5min_RF_slice", i, ".tif?raw=true"),
                RF_5min <- tempfile())
  RF <- readGDAL(RF_5min)
  RF$band1[1] <- 0
  
  
  #Download and Read in RFuse0min image file (.tif)
  download.file(paste0("https://github.com/georgewang229/CHE4180-Project/blob/master/ROC%20Prep%205min/5min_Use0_slice", i, ".tif?raw=true"),
                Use0_5min <- tempfile())
  Use0 <- readGDAL(Use0_5min)
  Use0$band1[1] <- 0
  
  
  #Create prediction objects 
  RF_pred <- prediction(RF$band1, manual$band1)
  NB_pred <- prediction(NB$band1, manual$band1)
  Use0_pred <- prediction(Use0$band1, manual$band1)
  
  
  #Performance evaluation
  RF_acc <- performance(RF_pred, "acc")@y.values[[1]][2]
  RF_sens <- performance(RF_pred, "sens")@y.values[[1]][2]
  RF_spec <- performance(RF_pred, "spec")@y.values[[1]][2]
  RF_prec <- performance(RF_pred, "prec")@y.values[[1]][2]
  RF_F <- performance(RF_pred, "f")@y.values[[1]][2]
  
  NB_acc <- performance(NB_pred, "acc")@y.values[[1]][2]
  NB_sens <- performance(NB_pred, "sens")@y.values[[1]][2]
  NB_spec <- performance(NB_pred, "spec")@y.values[[1]][2]
  NB_prec <- performance(NB_pred, "prec")@y.values[[1]][2]
  NB_F <- performance(NB_pred, "f")@y.values[[1]][2]
  
  Use0_acc <- performance(Use0_pred, "acc")@y.values[[1]][2]
  Use0_sens <- performance(Use0_pred, "sens")@y.values[[1]][2]
  Use0_spec <- performance(Use0_pred, "spec")@y.values[[1]][2]
  Use0_prec <- performance(Use0_pred, "prec")@y.values[[1]][2]
  Use0_F <- performance(Use0_pred, "f")@y.values[[1]][2]
  
  #Add performance measures to the tibble
  table_5min <- table_5min %>% add_row(Frame = i, Type = "Random Forest", Accuracy = RF_acc, Sensitivity = RF_sens, 
                                       Specificity = RF_spec, Precision = RF_prec, `F-score` = RF_F)
  
  table_5min <- table_5min %>% add_row(Frame = i, Type = "Naive Bayesian", Accuracy = NB_acc, Sensitivity = NB_sens, 
                                       Specificity = NB_spec, Precision = NB_prec, `F-score` = NB_F)
  
  table_5min <- table_5min %>% add_row(Frame = i, Type = "0min RF Model", Accuracy = Use0_acc, Sensitivity = Use0_sens, 
                                       Specificity = Use0_spec, Precision = Use0_prec, `F-score` = Use0_F)
  
}





#10min Performance Evaluation----------------------
table_10min <- tibble(Frame = numeric(),
                     Type = character(),
                     Accuracy = numeric(),
                     Sensitivity = numeric(),
                     Specificity = numeric(),
                     Precision = numeric(),
                     `F-score` = numeric()) #Pre-allocate the tibble

for(i in 1:100) {
  #Download and Read in manual image file (.tif)
  download.file(paste0("https://github.com/georgewang229/CHE4180-Project/blob/master/ROC%20Prep%2010min/10min_Manual_slice", i, ".tif?raw=true"),
                manual_10min <- tempfile())
  manual <- readGDAL(manual_10min)
  manual$band1[1] <- 0
  
  
  #Download and Read in NB image file (.tif)
  download.file(paste0("https://github.com/georgewang229/CHE4180-Project/blob/master/ROC%20Prep%2010min/10min_NB_slice", i, ".tif?raw=true"),
                NB_10min <- tempfile())
  NB <- readGDAL(NB_10min)
  NB$band1[1] <- 0
  
  
  
  #Download and Read in RF image file (.tif)
  download.file(paste0("https://github.com/georgewang229/CHE4180-Project/blob/master/ROC%20Prep%2010min/10min_RF_slice", i, ".tif?raw=true"),
                RF_10min <- tempfile())
  RF <- readGDAL(RF_10min)
  RF$band1[1] <- 0
  
  
  #Download and Read in RFuse0min image file (.tif)
  download.file(paste0("https://github.com/georgewang229/CHE4180-Project/blob/master/ROC%20Prep%2010min/10min_Use0_slice", i, ".tif?raw=true"),
                Use0_10min <- tempfile())
  Use0 <- readGDAL(Use0_10min)
  Use0$band1[1] <- 0
  
  
  #Create prediction objects 
  RF_pred <- prediction(RF$band1, manual$band1)
  NB_pred <- prediction(NB$band1, manual$band1)
  Use0_pred <- prediction(Use0$band1, manual$band1)
  
  
  #Performance evaluation
  RF_acc <- performance(RF_pred, "acc")@y.values[[1]][2]
  RF_sens <- performance(RF_pred, "sens")@y.values[[1]][2]
  RF_spec <- performance(RF_pred, "spec")@y.values[[1]][2]
  RF_prec <- performance(RF_pred, "prec")@y.values[[1]][2]
  RF_F <- performance(RF_pred, "f")@y.values[[1]][2]
  
  NB_acc <- performance(NB_pred, "acc")@y.values[[1]][2]
  NB_sens <- performance(NB_pred, "sens")@y.values[[1]][2]
  NB_spec <- performance(NB_pred, "spec")@y.values[[1]][2]
  NB_prec <- performance(NB_pred, "prec")@y.values[[1]][2]
  NB_F <- performance(NB_pred, "f")@y.values[[1]][2]
  
  Use0_acc <- performance(Use0_pred, "acc")@y.values[[1]][2]
  Use0_sens <- performance(Use0_pred, "sens")@y.values[[1]][2]
  Use0_spec <- performance(Use0_pred, "spec")@y.values[[1]][2]
  Use0_prec <- performance(Use0_pred, "prec")@y.values[[1]][2]
  Use0_F <- performance(Use0_pred, "f")@y.values[[1]][2]
  
  #Add performance measures to the tibble
  table_10min <- table_10min %>% add_row(Frame = i, Type = "Random Forest", Accuracy = RF_acc, Sensitivity = RF_sens, 
                                       Specificity = RF_spec, Precision = RF_prec, `F-score` = RF_F)
  
  table_10min <- table_10min %>% add_row(Frame = i, Type = "Naive Bayesian", Accuracy = NB_acc, Sensitivity = NB_sens, 
                                       Specificity = NB_spec, Precision = NB_prec, `F-score` = NB_F)
  
  table_10min <- table_10min %>% add_row(Frame = i, Type = "0min RF Model", Accuracy = Use0_acc, Sensitivity = Use0_sens, 
                                       Specificity = Use0_spec, Precision = Use0_prec, `F-score` = Use0_F)
  
}





#15min Performance Evaluation----------------------
table_15min <- tibble(Frame = numeric(),
                     Type = character(),
                     Accuracy = numeric(),
                     Sensitivity = numeric(),
                     Specificity = numeric(),
                     Precision = numeric(),
                     `F-score` = numeric()) #Pre-allocate the tibble

for(i in 1:100) {
  #Download and Read in manual image file (.tif)
  download.file(paste0("https://github.com/georgewang229/CHE4180-Project/blob/master/ROC%20Prep%2015min/15min_Manual_slice", i, ".tif?raw=true"),
                manual_15min <- tempfile())
  manual <- readGDAL(manual_15min)
  manual$band1[1] <- 0
  
  
  #Download and Read in NB image file (.tif)
  download.file(paste0("https://github.com/georgewang229/CHE4180-Project/blob/master/ROC%20Prep%2015min/15min_NB_slice", i, ".tif?raw=true"),
                NB_15min <- tempfile())
  NB <- readGDAL(NB_15min)
  NB$band1[1] <- 0
  
  
  
  #Download and Read in RF image file (.tif)
  download.file(paste0("https://github.com/georgewang229/CHE4180-Project/blob/master/ROC%20Prep%2015min/15min_RF_slice", i, ".tif?raw=true"),
                RF_15min <- tempfile())
  RF <- readGDAL(RF_15min)
  RF$band1[1] <- 0
  
  
  #Download and Read in RFuse0min image file (.tif)
  download.file(paste0("https://github.com/georgewang229/CHE4180-Project/blob/master/ROC%20Prep%2015min/15min_Use0_slice", i, ".tif?raw=true"),
                Use0_15min <- tempfile())
  Use0 <- readGDAL(Use0_15min)
  Use0$band1[1] <- 0
  
  
  #Create prediction objects 
  RF_pred <- prediction(RF$band1, manual$band1)
  NB_pred <- prediction(NB$band1, manual$band1)
  Use0_pred <- prediction(Use0$band1, manual$band1)
  
  
  #Performance evaluation
  RF_acc <- performance(RF_pred, "acc")@y.values[[1]][2]
  RF_sens <- performance(RF_pred, "sens")@y.values[[1]][2]
  RF_spec <- performance(RF_pred, "spec")@y.values[[1]][2]
  RF_prec <- performance(RF_pred, "prec")@y.values[[1]][2]
  RF_F <- performance(RF_pred, "f")@y.values[[1]][2]
  
  NB_acc <- performance(NB_pred, "acc")@y.values[[1]][2]
  NB_sens <- performance(NB_pred, "sens")@y.values[[1]][2]
  NB_spec <- performance(NB_pred, "spec")@y.values[[1]][2]
  NB_prec <- performance(NB_pred, "prec")@y.values[[1]][2]
  NB_F <- performance(NB_pred, "f")@y.values[[1]][2]
  
  Use0_acc <- performance(Use0_pred, "acc")@y.values[[1]][2]
  Use0_sens <- performance(Use0_pred, "sens")@y.values[[1]][2]
  Use0_spec <- performance(Use0_pred, "spec")@y.values[[1]][2]
  Use0_prec <- performance(Use0_pred, "prec")@y.values[[1]][2]
  Use0_F <- performance(Use0_pred, "f")@y.values[[1]][2]
  
  #Add performance measures to the tibble
  table_15min <- table_15min %>% add_row(Frame = i, Type = "Random Forest", Accuracy = RF_acc, Sensitivity = RF_sens, 
                                       Specificity = RF_spec, Precision = RF_prec, `F-score` = RF_F)
  
  table_15min <- table_15min %>% add_row(Frame = i, Type = "Naive Bayesian", Accuracy = NB_acc, Sensitivity = NB_sens, 
                                       Specificity = NB_spec, Precision = NB_prec, `F-score` = NB_F)
  
  table_15min <- table_15min %>% add_row(Frame = i, Type = "0min RF Model", Accuracy = Use0_acc, Sensitivity = Use0_sens, 
                                       Specificity = Use0_spec, Precision = Use0_prec, `F-score` = Use0_F)
  
}








#Whole Table-----------

table_0min <- table_0min %>%
  mutate(Time = "0 min")

table_5min <- table_5min %>%
  mutate(Time = "5 min")

table_10min <- table_10min %>%
  mutate(Time = "10 min")

table_15min <- table_15min %>%
  mutate(Time = "15 min")

table_all <- bind_rows(table_0min, table_5min, table_10min, table_15min) %>% 
  select(Time, everything())

#ROC Curve---------------------------
rf_plotx <- c(0, 1 - sort(filter(table_all, Type == "Random Forest")$Specificity, decreasing = TRUE), 1)
rf_ploty <- c(0, sort(filter(table_all, Type == "Random Forest")$Sensitivity), 1)

nb_plotx <- c(0, 1 - sort(filter(table_all, Type == "Naive Bayesian")$Specificity, decreasing = TRUE), 1)
nb_ploty <- c(0, sort(filter(table_all, Type == "Naive Bayesian")$Sensitivity), 1)

rf0_plotx <- c(0, 1 - sort(filter(table_all, Type == "0min RF Model")$Specificity, decreasing = TRUE), 1)
rf0_ploty <- c(0, sort(filter(table_all, Type == "0min RF Model")$Sensitivity), 1)


#Plot for RF vs NB--------------
colours1 <- c("Random Forest" = "green", "Naive Bayesian" = "blue")

ggplot(NULL) +
  geom_segment(aes(x = 0, y = 0, xend = 1,yend = 1), alpha = 0.5) + 
  geom_path(aes(x = rf_plotx, y = rf_ploty, colour = "Random Forest"), size = 1) +
  geom_path(aes(x = nb_plotx, y = nb_ploty, colour = "Naive Bayesian"), size = 1) +
  scale_x_continuous(name = "1 - Specificity",limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001,0.001)) + 
  scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001, 0.001)) +
  theme(axis.ticks = element_line(color = "grey80")) +
  coord_equal() +
  theme_bw() +
  labs(color = "Legend", title = "ROC Curve for Random Forest & Naive Bayesian - All") + 
  scale_colour_manual(values = colours1)

#Without legends
ggplot(NULL) +
  geom_segment(aes(x = 0, y = 0, xend = 1,yend = 1), alpha = 0.5) + 
  geom_path(aes(x = rf_plotx, y = rf_ploty), colour = "green", size = 1) +
  geom_path(aes(x = nb_plotx, y = nb_ploty), colour = "blue", size = 1) +
  scale_x_continuous(name = "1 - Specificity",limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001,0.001)) + 
  scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001, 0.001)) +
  theme(axis.ticks = element_line(color = "grey80")) +
  coord_equal() +
  theme_bw() +
  ggtitle("ROC Curve for Random Forest & Naive Bayesian - All data")



#Plot for RF vs RF_0min--------------
colours2 <- c("Random Forest" = "green", "Random Forest - Universal Model Application (using 0min model)" = "red")

ggplot(NULL) +
  geom_segment(aes(x = 0, y = 0, xend = 1,yend = 1), alpha = 0.5) + 
  geom_path(aes(x = rf_plotx, y = rf_ploty, colour = "Random Forest"), size = 1) +
  geom_path(aes(x = rf0_plotx, y = rf0_ploty, colour = "Random Forest - Universal Model Application (using 0min model)"), size = 1) +
  scale_x_continuous(name = "1 - Specificity",limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001,0.001)) + 
  scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001, 0.001)) +
  theme(axis.ticks = element_line(color = "grey80")) +
  coord_equal() +
  theme_bw() +
  labs(color = "Legend") + 
  scale_colour_manual(values = colours2)


#Without Legends
ggplot(NULL) +
  geom_segment(aes(x = 0, y = 0, xend = 1,yend = 1), alpha = 0.5) + 
  geom_path(aes(x = rf_plotx, y = rf_ploty), colour = "green", size = 1) +
  geom_path(aes(x = rf0_plotx, y = rf0_ploty), colour = "red", size = 1) +
  scale_x_continuous(name = "1 - Specificity",limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001,0.001)) + 
  scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001, 0.001)) +
  theme(axis.ticks = element_line(color = "grey80")) +
  coord_equal() +
  theme_bw() +
  ggtitle("ROC Curve for Random Forest & Generalised(0min) Random Forest - All data")

#All together
ggplot(NULL) +
  geom_segment(aes(x = 0, y = 0, xend = 1,yend = 1), alpha = 0.5) + 
  geom_path(aes(x = rf_plotx, y = rf_ploty), colour = "green", size = 1) +
  geom_path(aes(x = rf0_plotx, y = rf0_ploty), colour = "red", size = 1) +
  geom_path(aes(x = nb_plotx, y = nb_ploty), colour = "blue", size = 1) +
  scale_x_continuous(name = "1 - Specificity",limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001,0.001)) + 
  scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001, 0.001)) +
  theme(axis.ticks = element_line(color = "grey80")) +
  coord_equal() +
  theme_bw() +
  ggtitle("ROC Curves - All data")




#AUC---------------------
#RF
rf_area <- tibble(x = rf_plotx, y = rf_ploty)

rf_area %>%
  mutate(area_rec = (lead(x) - x) * pmin(y, lead(y)),
         area_tri = 0.5 * (lead(x) - x) * abs(y - lead(y))) %>%
  summarise(area = sum(area_rec + area_tri,
                       na.rm = TRUE)) #0.988

#NB
nb_area <- tibble(x = nb_plotx, y = nb_ploty)

nb_area %>%
  mutate(area_rec = (lead(x) - x) * pmin(y, lead(y)),
         area_tri = 0.5 * (lead(x) - x) * abs(y - lead(y))) %>%
  summarise(area = sum(area_rec + area_tri,
                       na.rm = TRUE)) #0.958

#RF 0min
rf0_area <- tibble(x = rf0_plotx, y = rf0_ploty)

rf0_area %>%
  mutate(area_rec = (lead(x) - x) * pmin(y, lead(y)),
         area_tri = 0.5 * (lead(x) - x) * abs(y - lead(y))) %>%
  summarise(area = sum(area_rec + area_tri,
                       na.rm = TRUE)) #0.942


#plot at each time----------------------
#0min-------------------
rf_plotx_0min <- c(0, 1 - sort(filter(table_0min, Type == "Random Forest")$Specificity, decreasing = TRUE), 1)
rf_ploty_0min <- c(0, sort(filter(table_0min, Type == "Random Forest")$Sensitivity), 1)

nb_plotx_0min <- c(0, 1 - sort(filter(table_0min, Type == "Naive Bayesian")$Specificity, decreasing = TRUE), 1)
nb_ploty_0min <- c(0, sort(filter(table_0min, Type == "Naive Bayesian")$Sensitivity), 1)


#RF vs NB
ggplot(NULL) +
  geom_segment(aes(x = 0, y = 0, xend = 1,yend = 1), alpha = 0.5) + 
  geom_path(aes(x = rf_plotx_0min, y = rf_ploty_0min), colour = "green", size = 1) +
  geom_path(aes(x = nb_plotx_0min, y = nb_ploty_0min), colour = "blue", size = 1) +
  scale_x_continuous(name = "1 - Specificity",limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001,0.001)) + 
  scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001, 0.001)) +
  theme(axis.ticks = element_line(color = "grey80")) +
  coord_equal() +
  theme_bw() +
  ggtitle("ROC Curves - 0 min")


#AUC
#RF
rf_area_0min <- tibble(x = rf_plotx_0min, y = rf_ploty_0min)

rf_area_0min %>%
  mutate(area_rec = (lead(x) - x) * pmin(y, lead(y)),
         area_tri = 0.5 * (lead(x) - x) * abs(y - lead(y))) %>%
  summarise(area = sum(area_rec + area_tri,
                       na.rm = TRUE)) #0.961

#NB
nb_area_0min <- tibble(x = nb_plotx_0min, y = nb_ploty_0min)

nb_area_0min %>%
  mutate(area_rec = (lead(x) - x) * pmin(y, lead(y)),
         area_tri = 0.5 * (lead(x) - x) * abs(y - lead(y))) %>%
  summarise(area = sum(area_rec + area_tri,
                       na.rm = TRUE)) #0.961



#5min-------------------
rf_plotx_5min <- c(0, 1 - sort(filter(table_5min, Type == "Random Forest")$Specificity, decreasing = TRUE), 1)
rf_ploty_5min <- c(0, sort(filter(table_5min, Type == "Random Forest")$Sensitivity), 1)

nb_plotx_5min <- c(0, 1 - sort(filter(table_5min, Type == "Naive Bayesian")$Specificity, decreasing = TRUE), 1)
nb_ploty_5min <- c(0, sort(filter(table_5min, Type == "Naive Bayesian")$Sensitivity), 1)

rf0_plotx_5min <- c(0, 1 - sort(filter(table_5min, Type == "0min RF Model")$Specificity, decreasing = TRUE), 1)
rf0_ploty_5min <- c(0, sort(filter(table_5min, Type == "0min RF Model")$Sensitivity), 1)

#RF vs NB
ggplot(NULL) +
  geom_segment(aes(x = 0, y = 0, xend = 1,yend = 1), alpha = 0.5) + 
  geom_path(aes(x = rf_plotx_5min, y = rf_ploty_5min), colour = "green", size = 1) +
  geom_path(aes(x = nb_plotx_5min, y = nb_ploty_5min), colour = "blue", size = 1) +
  scale_x_continuous(name = "1 - Specificity",limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001,0.001)) + 
  scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001, 0.001)) +
  theme(axis.ticks = element_line(color = "grey80")) +
  coord_equal() +
  theme_bw() +
  ggtitle("ROC Curve for Random Forest & Naive Bayesian at 5 min")

#RF vs RF0
ggplot(NULL) +
  geom_segment(aes(x = 0, y = 0, xend = 1,yend = 1), alpha = 0.5) + 
  geom_path(aes(x = rf_plotx_5min, y = rf_ploty_5min), colour = "green", size = 1) +
  geom_path(aes(x = rf0_plotx_5min, y = rf0_ploty_5min), colour = "red", size = 1) +
  scale_x_continuous(name = "1 - Specificity",limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001,0.001)) + 
  scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001, 0.001)) +
  theme(axis.ticks = element_line(color = "grey80")) +
  coord_equal() +
  theme_bw() +
  ggtitle("ROC Curve for Random Forest & Generalised(0min) Random Forest at 5 min")

#All together

ggplot(NULL) +
  geom_segment(aes(x = 0, y = 0, xend = 1,yend = 1), alpha = 0.5) + 
  geom_path(aes(x = rf_plotx_5min, y = rf_ploty_5min), colour = "green", size = 1) +
  geom_path(aes(x = nb_plotx_5min, y = nb_ploty_5min), colour = "blue", size = 1) +
  geom_path(aes(x = rf0_plotx_5min, y = rf0_ploty_5min), colour = "red", size = 1) +
  scale_x_continuous(name = "1 - Specificity",limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001,0.001)) + 
  scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001, 0.001)) +
  theme(axis.ticks = element_line(color = "grey80")) +
  coord_equal() +
  theme_bw() +
  ggtitle("ROC Curves - 5 min")

#AUC
#RF
rf_area_5min <- tibble(x = rf_plotx_5min, y = rf_ploty_5min)

rf_area_5min %>%
  mutate(area_rec = (lead(x) - x) * pmin(y, lead(y)),
         area_tri = 0.5 * (lead(x) - x) * abs(y - lead(y))) %>%
  summarise(area = sum(area_rec + area_tri,
                       na.rm = TRUE)) #0.987

#NB
nb_area_5min <- tibble(x = nb_plotx_5min, y = nb_ploty_5min)

nb_area_5min %>%
  mutate(area_rec = (lead(x) - x) * pmin(y, lead(y)),
         area_tri = 0.5 * (lead(x) - x) * abs(y - lead(y))) %>%
  summarise(area = sum(area_rec + area_tri,
                       na.rm = TRUE)) #0.946

#RF 0min
rf0_area_5min <- tibble(x = rf0_plotx_5min, y = rf0_ploty_5min)

rf0_area_5min %>%
  mutate(area_rec = (lead(x) - x) * pmin(y, lead(y)),
         area_tri = 0.5 * (lead(x) - x) * abs(y - lead(y))) %>%
  summarise(area = sum(area_rec + area_tri,
                       na.rm = TRUE)) #0.943


#10min-------------------
rf_plotx_10min <- c(0, 1 - sort(filter(table_10min, Type == "Random Forest")$Specificity, decreasing = TRUE), 1)
rf_ploty_10min <- c(0, sort(filter(table_10min, Type == "Random Forest")$Sensitivity), 1)

nb_plotx_10min <- c(0, 1 - sort(filter(table_10min, Type == "Naive Bayesian")$Specificity, decreasing = TRUE), 1)
nb_ploty_10min <- c(0, sort(filter(table_10min, Type == "Naive Bayesian")$Sensitivity), 1)

rf0_plotx_10min <- c(0, 1 - sort(filter(table_10min, Type == "0min RF Model")$Specificity, decreasing = TRUE), 1)
rf0_ploty_10min <- c(0, sort(filter(table_10min, Type == "0min RF Model")$Sensitivity), 1)

#RF vs NB
ggplot(NULL) +
  geom_segment(aes(x = 0, y = 0, xend = 1,yend = 1), alpha = 0.5) + 
  geom_path(aes(x = rf_plotx_10min, y = rf_ploty_10min), colour = "green", size = 1) +
  geom_path(aes(x = nb_plotx_10min, y = nb_ploty_10min), colour = "blue", size = 1) +
  scale_x_continuous(name = "1 - Specificity",limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001,0.001)) + 
  scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001, 0.001)) +
  theme(axis.ticks = element_line(color = "grey80")) +
  coord_equal() +
  theme_bw() +
  ggtitle("ROC Curve for Random Forest & Naive Bayesian at 10 min")
#RF vs RF0
ggplot(NULL) +
  geom_segment(aes(x = 0, y = 0, xend = 1,yend = 1), alpha = 0.5) + 
  geom_path(aes(x = rf_plotx_10min, y = rf_ploty_10min), colour = "green", size = 1) +
  geom_path(aes(x = rf0_plotx_10min, y = rf0_ploty_10min), colour = "red", size = 1) +
  scale_x_continuous(name = "1 - Specificity",limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001,0.001)) + 
  scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001, 0.001)) +
  theme(axis.ticks = element_line(color = "grey80")) +
  coord_equal() +
  theme_bw() +
  ggtitle("ROC Curve for Random Forest & Generalised(0min) Random Forest at 10 min") 

#All together
ggplot(NULL) +
  geom_segment(aes(x = 0, y = 0, xend = 1,yend = 1), alpha = 0.5) + 
  geom_path(aes(x = rf_plotx_10min, y = rf_ploty_10min), colour = "green", size = 1) +
  geom_path(aes(x = rf0_plotx_10min, y = rf0_ploty_10min), colour = "red", size = 1) +
  geom_path(aes(x = nb_plotx_10min, y = nb_ploty_10min), colour = "blue", size = 1) +
  scale_x_continuous(name = "1 - Specificity",limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001,0.001)) + 
  scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001, 0.001)) +
  theme(axis.ticks = element_line(color = "grey80")) +
  coord_equal() +
  theme_bw() +
  ggtitle("ROC Curves - 10 min") 

#AUC
#RF
rf_area_10min <- tibble(x = rf_plotx_10min, y = rf_ploty_10min)

rf_area_10min %>%
  mutate(area_rec = (lead(x) - x) * pmin(y, lead(y)),
         area_tri = 0.5 * (lead(x) - x) * abs(y - lead(y))) %>%
  summarise(area = sum(area_rec + area_tri,
                       na.rm = TRUE)) #0.967

#NB
nb_area_10min <- tibble(x = nb_plotx_10min, y = nb_ploty_10min)

nb_area_10min %>%
  mutate(area_rec = (lead(x) - x) * pmin(y, lead(y)),
         area_tri = 0.5 * (lead(x) - x) * abs(y - lead(y))) %>%
  summarise(area = sum(area_rec + area_tri,
                       na.rm = TRUE)) #0.936

#RF 0min
rf0_area_10min <- tibble(x = rf0_plotx_10min, y = rf0_ploty_10min)

rf0_area_10min %>%
  mutate(area_rec = (lead(x) - x) * pmin(y, lead(y)),
         area_tri = 0.5 * (lead(x) - x) * abs(y - lead(y))) %>%
  summarise(area = sum(area_rec + area_tri,
                       na.rm = TRUE)) #0.922


#15min-------------------
rf_plotx_15min <- c(0, 1 - sort(filter(table_15min, Type == "Random Forest")$Specificity, decreasing = TRUE), 1)
rf_ploty_15min <- c(0, sort(filter(table_15min, Type == "Random Forest")$Sensitivity), 1)

nb_plotx_15min <- c(0, 1 - sort(filter(table_15min, Type == "Naive Bayesian")$Specificity, decreasing = TRUE), 1)
nb_ploty_15min <- c(0, sort(filter(table_15min, Type == "Naive Bayesian")$Sensitivity), 1)

rf0_plotx_15min <- c(0, 1 - sort(filter(table_15min, Type == "0min RF Model")$Specificity, decreasing = TRUE), 1)
rf0_ploty_15min <- c(0, sort(filter(table_15min, Type == "0min RF Model")$Sensitivity), 1)

#RF vs NB
ggplot(NULL) +
  geom_segment(aes(x = 0, y = 0, xend = 1,yend = 1), alpha = 0.5) + 
  geom_path(aes(x = rf_plotx_15min, y = rf_ploty_15min), colour = "green", size = 1) +
  geom_path(aes(x = nb_plotx_15min, y = nb_ploty_15min), colour = "blue", size = 1) +
  scale_x_continuous(name = "1 - Specificity",limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001,0.001)) + 
  scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001, 0.001)) +
  theme(axis.ticks = element_line(color = "grey80")) +
  coord_equal() +
  theme_bw() +
  ggtitle("ROC Curve for Random Forest & Naive Bayesian at 15 min")
#RF vs RF0
ggplot(NULL) +
  geom_segment(aes(x = 0, y = 0, xend = 1,yend = 1), alpha = 0.5) + 
  geom_path(aes(x = rf_plotx_15min, y = rf_ploty_15min), colour = "green", size = 1) +
  geom_path(aes(x = rf0_plotx_15min, y = rf0_ploty_15min), colour = "red", size = 1) +
  scale_x_continuous(name = "1 - Specificity",limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001,0.001)) + 
  scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001, 0.001)) +
  theme(axis.ticks = element_line(color = "grey80")) +
  coord_equal() +
  theme_bw() +
  ggtitle("ROC Curve for Random Forest & Generalised(0min) Random Forest at 15 min")  

#All together
ggplot(NULL) +
  geom_segment(aes(x = 0, y = 0, xend = 1,yend = 1), alpha = 0.5) + 
  geom_path(aes(x = rf_plotx_15min, y = rf_ploty_15min), colour = "green", size = 1) +
  geom_path(aes(x = rf0_plotx_15min, y = rf0_ploty_15min), colour = "red", size = 1) +
  geom_path(aes(x = nb_plotx_15min, y = nb_ploty_15min), colour = "blue", size = 1) +
  scale_x_continuous(name = "1 - Specificity",limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001,0.001)) + 
  scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.001, 0.001)) +
  theme(axis.ticks = element_line(color = "grey80")) +
  coord_equal() +
  theme_bw() +
  ggtitle("ROC Curves - 15 min")  

#AUC
#RF
rf_area_15min <- tibble(x = rf_plotx_15min, y = rf_ploty_15min)

rf_area_15min %>%
  mutate(area_rec = (lead(x) - x) * pmin(y, lead(y)),
         area_tri = 0.5 * (lead(x) - x) * abs(y - lead(y))) %>%
  summarise(area = sum(area_rec + area_tri,
                       na.rm = TRUE)) #0.990

#NB
nb_area_15min <- tibble(x = nb_plotx_15min, y = nb_ploty_15min)

nb_area_15min %>%
  mutate(area_rec = (lead(x) - x) * pmin(y, lead(y)),
         area_tri = 0.5 * (lead(x) - x) * abs(y - lead(y))) %>%
  summarise(area = sum(area_rec + area_tri,
                       na.rm = TRUE)) #0.929

#RF 0min
rf0_area_15min <- tibble(x = rf0_plotx_15min, y = rf0_ploty_15min)

rf0_area_15min %>%
  mutate(area_rec = (lead(x) - x) * pmin(y, lead(y)),
         area_tri = 0.5 * (lead(x) - x) * abs(y - lead(y))) %>%
  summarise(area = sum(area_rec + area_tri,
                       na.rm = TRUE)) #0.933


#Summarise ROC performance evaluation measures-------------------------------
# Summarising 0mins
table_0min %>% 
  group_by(Type) %>% 
  summarise(mean1 = mean(Accuracy),
            sd1 = sd(Accuracy),
            mean2 = mean(Sensitivity),
            sd2 = sd(Sensitivity),
            mean3 = mean(Specificity),
            sd3 = sd(Specificity),
            mean4 = mean(Precision),
            sd4 = sd(Precision),
            mean5 = mean(`F-score`),
            sd5 = sd(`F-score`))

# Summarising 5mins
table_5min %>% 
  group_by(Type) %>% 
  summarise(mean1 = mean(Accuracy),
            sd1 = sd(Accuracy),
            mean2 = mean(Sensitivity),
            sd2 = sd(Sensitivity),
            mean3 = mean(Specificity),
            sd3 = sd(Specificity),
            mean4 = mean(Precision),
            sd4 = sd(Precision),
            mean5 = mean(`F-score`),
            sd5 = sd(`F-score`))

# Summarising 10mins
table_10min %>% 
  group_by(Type) %>% 
  summarise(mean1 = mean(Accuracy),
            sd1 = sd(Accuracy),
            mean2 = mean(Sensitivity),
            sd2 = sd(Sensitivity),
            mean3 = mean(Specificity),
            sd3 = sd(Specificity),
            mean4 = mean(Precision),
            sd4 = sd(Precision),
            mean5 = mean(`F-score`),
            sd5 = sd(`F-score`))

# Summarising 15mins
table_15min %>% 
  group_by(Type) %>% 
  summarise(mean1 = mean(Accuracy),
            sd1 = sd(Accuracy),
            mean2 = mean(Sensitivity),
            sd2 = sd(Sensitivity),
            mean3 = mean(Specificity),
            sd3 = sd(Specificity),
            mean4 = mean(Precision),
            sd4 = sd(Precision),
            mean5 = mean(`F-score`),
            sd5 = sd(`F-score`))

# Summarising ALL
table_all %>% 
  group_by(Type) %>% 
  summarise(mean1 = mean(Accuracy),
            sd1 = sd(Accuracy),
            mean2 = mean(Sensitivity),
            sd2 = sd(Sensitivity),
            mean3 = mean(Specificity),
            sd3 = sd(Specificity),
            mean4 = mean(Precision),
            sd4 = sd(Precision),
            mean5 = mean(`F-score`),
            sd5 = sd(`F-score`))