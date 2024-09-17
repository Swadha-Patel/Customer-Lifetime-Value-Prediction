# CLV Project 
# Model Evaluations

library(readxl)
df <- read_excel("Desktop/CLV.xlsx")
summary(df)

colnames(df)

library(randomForestExplainer)

#converting categorical feauters to factors.
df$State <- as.factor(df$State)
df$Response <- as.factor(df$Response)
df$Coverage <- as.factor(df$Coverage)
df$Education <- as.factor(df$Education)
df$EmploymentStatus <- as.factor(df$EmploymentStatus)
df$Gender <- as.factor(df$Gender)
df$Location.Code  <- as.factor(df$Location.Code)
df$Marital.Status  <- as.factor(df$Marital.Status)
df$Policy.Type  <- as.factor(df$Policy.Type)
df$Renew.Offer.Type  <- as.factor(df$Renew.Offer.Type)
df$Policy  <- as.factor(df$Policy)
df$Sales.Channel  <- as.factor(df$Sales.Channel)
df$Vehicle.Class  <- as.factor(df$Vehicle.Class)
df$Vehicle.Size  <- as.factor(df$Vehicle.Size)

set.seed(223)
index <- sample(nrow(df), 0.70 * nrow(df))
train <- df[index, ]
test <- df[-index, ]

# Linear Model

fit1<- lm(Customer.Lifetime.Value ~ 	State+Response+Coverage+
            Education+EmploymentStatus+Gender+
            Income+Location.Code+Marital.Status+
            Months.Since.Last.Claim+Months.Since.Policy.Inception+
            Number.of.Open.Complaints+Number.of.Policies+Policy+
            Renew.Offer.Type+Sales.Channel+Total.Claim.Amount+Vehicle.Class+Vehicle.Size , data= train)
model_summary <- summary(fit1)
model_summary
plot(fit1)

# In sample MSE, AIC, BIC
MSE <- (model_summary$sigma)^2
MSE

AIC(fit1)
BIC(fit1)

# Out of Sample Prediction
pi <- predict(fit1, test)
pi 

MSE<- mean((pi - test$Customer.Lifetime.Value)^2)
MSE

# Backward Elimination 
Selected_features <- step(fit1, direction = "backward" )

library(rpart)
library(rpart.plot)

#Regression Tree
# Model Building: Fiting regression tree 
insurance_rpart <- rpart(formula = Customer.Lifetime.Value ~ Coverage + EmploymentStatus + Marital.Status + 
                           Months.Since.Last.Claim + Number.of.Open.Complaints + Number.of.Policies + 
                           Renew.Offer.Type + Total.Claim.Amount + Vehicle.Class, data = train)        

# Printing and plotting the tree
insurance_rpart
prp(insurance_rpart,digits = 4, extra = 1) 

# In sample prediction 
insurance_train_pred_tree = predict(insurance_rpart)
MSE.train <- mean((insurance_train_pred_tree - train$Customer.Lifetime.Value)^2)
MSE.train

# Out of sample prediction 
insurance_test_pred_tree = predict(insurance_rpart,test)

# Out of sample MSE
MSE.tree <- mean((insurance_test_pred_tree - test$Customer.Lifetime.Value)^2)
MSE.tree

# Pruning 
# Generate large tree 
insurance_largetree <- rpart(formula = Customer.Lifetime.Value ~ Coverage + EmploymentStatus + Marital.Status + 
                               Months.Since.Last.Claim + Number.of.Open.Complaints + Number.of.Policies + 
                               Renew.Offer.Type + Total.Claim.Amount + Vehicle.Class, data = train, cp = 0.001) 
prp(insurance_largetree)

# Plotting the cp values 
plotcp(insurance_largetree)


# Random Forest 
library(randomForest)
rf.model <- randomForest(Customer.Lifetime.Value ~ Coverage + EmploymentStatus + Marital.Status + 
                           Months.Since.Last.Claim + Number.of.Open.Complaints + Number.of.Policies + 
                           Renew.Offer.Type + Total.Claim.Amount + Vehicle.Class, data = train, proximity = TRUE)
rf.model

# Find number of trees that produce lowest test MSE
which.min(rf.model$mse)

# Find RMSE of best model
sqrt(rf.model$mse[which.min(rf.model$mse)]) 

# Plot the test MSE by number of trees
plot(rf.model)

# Variable importance plot
varImpPlot(rf.model)

# Tune the model
model_tuned <- tuneRF(
  x=df[,-1], #define predictor variables
  y=df$Customer.Lifetime.Value, #define response variable
  ntreeTry=500,
  mtryStart=4, 
  stepFactor=1.5,
  improve=0.01,
  trace=FALSE #don't show real-time progress
)
plot(model_tuned)



