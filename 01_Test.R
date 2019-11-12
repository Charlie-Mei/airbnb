## Importing packages

library(tidyverse) # metapackage with lots of helpful functions
library(leaps)
library(glmnet)
library(gridExtra)
library(caret)
library(broom)

# Import the data and create and train and test set
mydata <- read.csv("Data/analysisData.csv", stringsAsFactors = F)


set.seed(1431)
split <- createDataPartition(mydata$price, p = 0.8, list = FALSE, groups = 100)
train <- as_tibble(mydata[split, ])
test <- as_tibble(mydata[-split, ])

summary(train$price)

# Density plot
p1 <- ggplot(train, aes(price)) + 
  geom_density(col = "blue", fill = "blue", alpha = 0.4) + 
  geom_vline(xintercept = mean(mydata$price), col = "red") +
  labs(title = "Distribution of prices") +
  theme_minimal()

# Boxplot to identify outliers
p2 <- ggplot(train, aes(1, price)) + 
  geom_boxplot(col = "red", fill = "blue", alpha = 0.4) + 
  theme_minimal()

grid.arrange(p1, p2, ncol = 2)

# Create LB (lower bound) and UB (upper bound) to remove outliers, also remove price = 0
LB <- quantile(train$price, probs = 0.25 ) - 1.5*IQR(train$price)
UB <- quantile(train$price, probs = 0.75) + 1.5*IQR(train$price)

train <- train %>% filter(price >= LB & price <= UB, price != 0)

# Locate our nuerical 
locs <- lapply(train, is.numeric) %>% unlist()
ndata <- train[, locs] %>% select(-id, -price, -weekly_price, -monthly_price)

missing_counts <- lapply(ndata, function(x){
  sum(is.na(x))
}) %>% unlist()
missing_counts

# mdata is a placeholder atm with just numeric variables with no NAs
mdata <- ndata %>% select(-square_feet, -security_deposit, -cleaning_fee, -reviews_per_month, -contains("listings_count"), -beds, 
                          -contains("maximum_minimum_nights"), -contains("minimum_minimum_nights"), -number_of_reviews_ltm, -contains("0"))
price <- train$price
mdata <- cbind(mdata, price)

str(mdata)

# Forward stepwise model
start_model <- lm(price ~ 1, mdata)
end_model <- lm(price ~ ., mdata)

model <- step(start_model, 
              scope = list(upper = end_model, lower = start_model), 
              direction = "forward",
              trace = F)
summary(model)


# Lasso model
X <- model.matrix(price ~ ., mdata)
y <- mdata$price

set.seed(1431)
cv.lasso <- cv.glmnet(X, y, alpha = 1)
#plot(cv.lasso)
#coef(cv.lasso)

model2 <- lm(price ~ accommodates + bathrooms + guests_included + minimum_nights + 
               availability_365 + number_of_reviews + review_scores_rating + 
               review_scores_cleanliness + review_scores_location + review_scores_value, mdata)
summary(model2)


pred <- predict(model2)

rmse <- mean((train$price - pred)^2) %>% sqrt()

test_pred <- predict(model2, newdata = test)
t_rmse <- mean((test$price - test_pred)^2) %>% sqrt()

paste("Training RMSE:", rmse)
paste("Test RMSE:", t_rmse)

scoring <- read.csv("Data/scoringData.csv", stringsAsFactors = F)

pred <- predict(model, newdata = scoring)

submission <- data.frame(id = scoring$id, price = pred)

write_csv(submission, "submission.csv")