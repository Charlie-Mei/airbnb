library(tidyverse)
library(leaps)
library(glmnet)
library(gridExtra)
library(caret)
library(broom)
library(lubridate)
library(randomForest)
library(zipcode)


load("Cleaned_data/train.Rdata")
load("Cleaned_data/test.Rdata")
load("Cleaned_data/scoring.Rdata")
# data("zipcode")

# Removing missing values
# Tabularize counts of missingness
# tab.missing <- function(df){
#   lapply(df, function(x){sum(is.na(x))}) %>% unlist()
# }
# 
# tab.missing(clean_train)


##### Turn variables to factors
f_vars <- c("host_superhost", "host_verified", 
            "neighbourhood_group_cleansed", "is_location_exact",
            "has_transit", "has_hrules", "property_type", "room_type",
            "bed_type", "has_sdeposit", "instant_bookable", "cancellation_policy")

clean_train[f_vars] <- lapply(clean_train[f_vars], factor)
test[f_vars] <- lapply(test[f_vars], factor)
scoring[f_vars] <- lapply(scoring[f_vars], factor)


########## THE MODEL ##########


# Model being used for modeling
model_formula <- price ~
  host_superhost +
  host_verified +
  as.numeric(host_since2) +
  calculated_host_listings_count_entire_homes +
  calculated_host_listings_count_private_rooms +
  calculated_host_listings_count_shared_rooms +
  neighbourhood_group_cleansed +
  is_location_exact +
  number_of_reviews_ltm +
  bed_type +
  instant_bookable +
  cancellation_policy +
  description_length +
  bathrooms +
  bedrooms +
  beds +
  latitude +
  longitude +
  minimum_nights_avg_ntm +
  availability_365 +
  review_scores_value +
  as.numeric(last_rev_days) +
  reviews_per_month +
  room_type +
  guests_included +
  extra_people +
  accommodates +
  availability_30 +
  review_scores_rating +
  review_scores_cleanliness +
  review_scores_location +
  has_transit +
  has_hrules +
  has_sdeposit +
  availability_60 +
  review_scores_checkin +
  review_scores_communication +
  review_scores_accuracy +
  has_IT +
  has_WF +
  has_AC +
  has_KN +
  has_WS +
  has_HT +
  has_TV +
  has_BF +
  has_GY +
  has_SB +
  availability_90 +
  is_apartment +
  has_cleaning +
  as.numeric(listing_time) +
  cleaning_fee +
  host_year


########## RANDOM FOREST CROSS VALIDATION ##########

# CROSS VALIDATION TO FIND OPTIMAL MTRY
set.seed(258)
# CV random forest - remove coefficients that don't make sense
trControl <- trainControl(method= "cv", number = 3)
tuneGrid <- expand.grid(mtry = 8)

cvForest <- train(model_formula,
                  clean_train,
                 method = "rf",
                 ntree = 100,
                 trControl = trControl,
                 tuneGrid = tuneGrid)

cvForest
pred <- predict(cvForest, newdata = test)
rmse <- mean((pred - test$price)^2) %>% sqrt()
rmse

########## GRADIENT BOOSTING ##########

# set.seed(258)
# 
# boost <- gbm(model_formula, 
#              clean_train,
#              distribution="gaussian",
#              n.trees = 1000,
#              interaction.depth = 8,
#              shrinkage = 0.001)
# 
# predBoost <- predict(boost, newdata = test, n.trees = 1000)
# rmseBoost <- sqrt(mean((predBoost - test$price)^2))
# rmseBoost


########## SCORE THE MODEL ##########

scoring$host_identity_verified[which(scoring$host_identity_verified == "")] <- "f"
scoring$host_identity_verified <- factor(scoring$host_identity_verified, levels = c("t", "f"))

# Impute missing values in scoring data with means
score.missing <- function(scoring){
  lapply(scoring, function(x){sum(is.na(x))}) %>% unlist()
}

scoring$host_listings_count[which(is.na(scoring$host_listings_count))] <- 
  mean(scoring$host_listings_count, na.rm = T)
scoring$beds[which(is.na(scoring$beds))] <- 
  mean(scoring$beds, na.rm = T)


pred_s <- predict(cvForest, newdata = scoring)
submission <- data.frame(id = scoring$id, price = pred_s)
# submission$price[which(is.na(submission$price))] <- mean(submission$price, na.rm = T)

write_csv(submission, "submission.csv")
