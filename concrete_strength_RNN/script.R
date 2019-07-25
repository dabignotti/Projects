###################################################
## ARTIFICIAL NEURAL NETWORK - CONCRETE STRENGTH ##
###################################################


# 1: load libraries

library(ggvis) #Data visulization
library(psych) #Scatterplot matrix
library(knitr) #html table
library(neuralnet) #artifical neural network 

concrete <- read.csv(file = "C:/Users/dabig/Documents/R/data/cement/concrete.csv")

knitr::kable(head(concrete), caption = "Partial Table Preview")

str(concrete)


# 2: data visualization

concrete %>% ggvis(x = ~compressive.str, fill:= "#27bc9c") %>% 
  layer_histograms() %>% 
  layer_paths(y = ~compressive.str, 35.82, stroke := "red") #hist of compressive.str

pairs.panels(concrete[c("kg.m3", "bf.slag", "fly.ash", "compressive.str")]) #correlation matrix


# 3: preprocessing and preparation

normalize <- function(x){
  return ((x - min(x))/(max(x) - min(x) ))
}

concrete_norm <- as.data.frame(lapply(concrete, normalize))

kable(round(head(concrete_norm), digits = 3), caption = "Normalized Data Preview")

concrete_train <- concrete_norm[1:773, ] #training set

concrete_test <- concrete_norm[774:1030, ] #test set


# 4: model training and evaluation

#Build a neural network with one hidden layer 
concrete_model <- neuralnet(compressive.str ~ kg.m3 + bf.slag + fly.ash + water + superplast + 
                              coarse.agg + fine.agg + age.days , data = concrete_train, hidden = 1)

plot(concrete_model)

#building the predictor, exclude the target variable column
model_results <- compute(concrete_model, concrete_test[1:8])

#store the net.results column 
predicted_strength <- model_results$net.result

cor(predicted_strength, concrete_test$compressive.str)


# 5: model improvement

#building the new model
concrete_model2 <- neuralnet(compressive.str ~ kg.m3 + bf.slag + fly.ash + water + superplast + 
                               coarse.agg + fine.agg + age.days, data = concrete_train, hidden = 5 )


# 6: implement and evaluate new model

#building the new predictor
model_results2 <- compute(concrete_model2, concrete_test[1:8])

#storing the results
predicted_strength2 <- model_results2$net.result

cor(predicted_strength2, concrete_test$strength)


