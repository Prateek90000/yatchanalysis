rm(list = ls())
install.packages('tidyverse')
install.packages('neuralnet')
install.packages('GGally')

library(tidyverse)
library(neuralnet)
library(GGally)

url <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/00243/yacht_hydrodynamics.data'

Yacht_Data <- read_table(file = url,
                         col_names = c('LongPos_COB', 'Prismatic_Coeff',
                                       'Len_Disp_Ratio', 'Beam_Draut_Ratio', 
                                       'Length_Beam_Ratio','Froude_Num', 
                                       'Residuary_Resist')) %>%
  na.omit()


str(Yacht_Data)
summary(Yacht_Data)
ggpairs(Yacht_Data)

# Scale the Data
scale01 <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

Yacht_Data <- Yacht_Data %>%
  mutate_all(scale01)

# Split into test and train sets
set.seed(12345)
Yacht_Data_Train <- sample_frac(tbl = Yacht_Data, replace = FALSE, size = 0.80)
Yacht_Data_Test <- anti_join(Yacht_Data, Yacht_Data_Train)

# construction of model
NNModel = neuralnet(Residuary_Resist~., data = Yacht_Data_Train)
plot(NNModel, rep = "best")

NNModel2 = neuralnet(Residuary_Resist~., data = Yacht_Data_Train,act.fct = "logistic")
plot(NNModel2, rep = "best")
NNModel3 = neuralnet(Residuary_Resist~., data = Yacht_Data_Train,act.fct = "tanh")
plot(NNModel3, rep = "best")

# change the hidden layer
NNModel4 = neuralnet(Residuary_Resist~., data = Yacht_Data_Train,act.fct = "tanh", hidden=c(4,1))
plot(NNModel4, rep = "best")

NNModel5 = neuralnet(Residuary_Resist~., data = Yacht_Data_Train,act.fct = "tanh", hidden=c(5,1))
plot(NNModel5, rep = "best")

NNModel6 = neuralnet(Residuary_Resist~., data = Yacht_Data_Train,act.fct = "tanh", hidden=c(4,4,1))
plot(NNModel6, rep = "best")

NNModel7 = neuralnet(Residuary_Resist~., data = Yacht_Data_Train,act.fct = "tanh", hidden=c(4,4,2))
plot(NNModel7, rep = "best")

NNModel7$result.matrix
