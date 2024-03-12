  #! Install and load required packages
#! install.packages("caret")
#! install.packages("readxl")
library(caret)
library(readxl)

  #! Load Data into DataFrames
train_data <- data.frame(train)
test_data <- data.frame(test)
    #! Data Structure Check
#! str(train_data)
#! str(test_data)


#! Factorize the categorical variables
factorize_variables <- c(
  "BldgType","RoofMatl", "ExterQual",
  "ExterCond", "Foundation", "BsmtQual", "KitchenQual"
)

for (var in factorize_variables) {
  train_data[[paste0(var, "_factorized")]] <- as.factor(train_data[[var]])
}

# Test VIF of the variables from train_data
calculate_vif <- function(data, response, variables) {
  model_formula <- as.formula(paste(response, "~", paste(variables, collapse = "+")))
  model_matrix <- model.matrix(model_formula, data = data)
  vif_values <- car::vif(lm(model_formula, data = data))
  return(vif_values)
}

selected_variables <- c(
  "LotArea", "BldgType_factorized", "OverallQual", "OverallCond", "YearBuilt",
  "RoofMatl_factorized", "MasVnrArea", "ExterQual_factorized", "BsmtQual_factorized",
  "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "LowQualFinSF", "GrLivArea",
  "KitchenQual_factorized", "PoolArea"
)

response_variable <- "SalePrice"
vif_results <- calculate_vif(data = train_data, response = response_variable, variables = selected_variables)

  #! Factorize the categorical variables
train_data$OverallQual_factorized <- as.factor(train_data$OverallQual)
train_data$OverallCond_factorized <- as.factor(train_data$OverallCond)
train_data$YearBuilt_factorized <- as.factor(train_data$YearBuilt)
train_data$RoofMatl_factorized <- as.factor(train_data$RoofMatl)
train_data$MasVnrArea_factorized <- as.factor(train_data$MasVnrArea)
train_data$ExterQual_factorized <- as.factor(train_data$ExterQual)
train_data$BsmtQual_factorized <- as.factor(train_data$BsmtQual)
train_data$BsmtFinSF1_factorized <- as.factor(train_data$BsmtFinSF1)
train_data$BsmtFinSF2_factorized <- as.factor(train_data$BsmtFinSF2)
train_data$BsmtUnfSF_factorized <- as.factor(train_data$BsmtUnfSF)
train_data$LowQualFinSF_factorized <- as.factor(train_data$LowQualFinSF)
train_data$GrLivArea_factorized <- as.factor(train_data$GrLivArea)
train_data$KitchenQual_factorized <- as.factor(train_data$KitchenQual)
train_data$PoolArea_factorized <- as.factor(train_data$PoolArea)


  #! Create a linear regression model (with factorized variables)
model <- lm(SalePrice ~ LotArea
            + BldgType_factorized
            + OverallQual
            + OverallCond
            + YearBuilt
            + RoofMatl_factorized
            + MasVnrArea
            + ExterQual_factorized
            + BsmtQual_factorized
            + BsmtFinSF1
            + BsmtFinSF2
            + BsmtUnfSF
            + LowQualFinSF
            + GrLivArea
            + KitchenQual_factorized
            + PoolArea, data=train_data)


  #! Print VIF and model
cat("VIF Values:\n")
print(vif_results)
summary(model)