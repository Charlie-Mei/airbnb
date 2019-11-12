library(tidyverse) # metapackage with lots of helpful functions
library(leaps)
library(glmnet)
library(gridExtra)
library(caret)
library(broom)

# Import the data and create and train and test set
mydata <- read.csv("Data/analysisData.csv", stringsAsFactors = F)
scoring <- read.csv("Data/scoringData.csv", stringsAsFactors = F)

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


# Locate numerical data
#locs <- lapply(train, is.numeric) %>% unlist()
#ndata <- train[, locs] %>% select(-id, -price, -weekly_price, -monthly_price)

# Tabularize counts of missingness
tab.missing <- function(df){
  locs <- lapply(df, is.numeric) %>% unlist()
  ndata <- df[, locs] %>% select(-id, -price, -weekly_price, -monthly_price)
  lapply(ndata, function(x){sum(is.na(x))}) %>% unlist()
}


# Filter out missing beds and reviews per month
train <- train %>% 
  filter(!is.na(beds), !is.na(reviews_per_month), !is.na(host_listings_count), !is.na(host_total_listings_count)) %>%
  mutate(security_deposit2 = ifelse(security_deposit > 0, 1, 0),
         cleaning_fee2 = ifelse(cleaning_fee > 0, 1, 0)) %>%
  select(-security_deposit, -cleaning_fee, -square_feet)

train$security_deposit2[is.na(train$security_deposit2)] <- 0
train$cleaning_fee2[is.na(train$cleaning_fee2)] <- 0

# Make corresponding additional feature additions to scoringdata and test data
scoring <- scoring %>%
  mutate(security_deposit2 = ifelse(security_deposit > 0, 1, 0),
         cleaning_fee2 = ifelse(cleaning_fee > 0, 1, 0))

scoring$security_deposit2[is.na(scoring$security_deposit2)] <- 0
scoring$cleaning_fee2[is.na(scoring$cleaning_fee2)] <- 0

test <- test %>%
  mutate(security_deposit2 = ifelse(security_deposit > 0, 1, 0),
         cleaning_fee2 = ifelse(cleaning_fee > 0, 1, 0))

test$security_deposit2[is.na(test$security_deposit2)] <- 0
test$cleaning_fee2[is.na(test$cleaning_fee2)] <- 0

train <- train %>% 
  select(-host_total_listings_count, -minimum_minimum_nights, -minimum_nights, -maximum_nights,
         -maximum_minimum_nights, -minimum_maximum_nights, -maximum_maximum_nights, -availability_30,
         -availability_60, -availability_90, -number_of_reviews_ltm)

# mdata is a placeholder atm with just numeric variables with no NAs
mdata <- train

# Placeholder to just get numeric variables
locs <- lapply(mdata, is.numeric) %>% unlist()
mdata <- mdata[, locs] %>% select(-contains("_price"))

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
plot(cv.lasso)
coef(cv.lasso, s = cv.lasso$lambda.min)

pred <- predict(model)

rmse <- mean((train$price - pred)^2) %>% sqrt()

test_pred <- predict(model, newdata = test)
t_rmse <- mean((test$price - test_pred)^2) %>% sqrt()

paste("Training RMSE:", rmse)
paste("Test RMSE:", t_rmse)

pred <- predict(model, newdata = scoring)
submission <- data.frame(id = scoring$id, price = pred)
write_csv(submission, "submission.csv")
