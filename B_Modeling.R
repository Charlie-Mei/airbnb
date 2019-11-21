library(tidyverse)
library(gridExtra)
library(caret)
library(randomForest)

rm(list = ls())

load("Cleaned_data/train.Rdata")
load("Cleaned_data/test.Rdata")
load("Cleaned_data/scoring.Rdata")

##### Turn variables to factors
f_vars <- c("host_superhost", "host_verified", 
            "neighbourhood_group_cleansed", "is_location_exact",
            "has_transit", "has_hrules", "property_type", "room_type",
            "bed_type", "has_sdeposit", "instant_bookable", "cancellation_policy")

clean_train[f_vars] <- lapply(clean_train[f_vars], factor)
test[f_vars] <- lapply(test[f_vars], factor)
scoring[f_vars] <- lapply(scoring[f_vars], factor)

clean_train$square_feet2 <- ifelse(is.na(clean_train$square_feet),
                                   mean(clean_train$square_feet, na.rm = T),
                                   clean_train$square_feet)

test$square_feet2 <- ifelse(is.na(test$square_feet),
                                   mean(test$square_feet, na.rm = T),
                                   test$square_feet)

scoring$square_feet2 <- ifelse(is.na(scoring$square_feet),
                                   mean(scoring$square_feet, na.rm = T),
                                   scoring$square_feet)



########## THE MODEL ##########

# Model being used for modeling
model_formula <- price ~
  host_superhost + host_verified + host_since2 +
  calculated_host_listings_count_entire_homes +
  calculated_host_listings_count_private_rooms +
  calculated_host_listings_count_shared_rooms +
  neighbourhood_group_cleansed + is_location_exact +
  number_of_reviews_ltm + bed_type + instant_bookable +
  cancellation_policy + description_length +
  name_length + space_length + bathrooms +
  bedrooms + beds + latitude + longitude +
  minimum_nights_avg_ntm + availability_365 +
  review_scores_value + last_rev_days +
  reviews_per_month + room_type + guests_included +
  extra_people + accommodates + availability_30 +
  review_scores_rating +
  review_scores_cleanliness +
  review_scores_location +
  has_transit + has_hrules + 
  has_sdeposit +
  availability_60 + review_scores_checkin +
  review_scores_communication + review_scores_accuracy +
  has_IT + has_WF + has_AC + has_KN + has_WS + has_HT +
  has_TV + has_BF + has_GY + has_SB + availability_90 +
  has_DM + has_EV + has_PL + has_ES +
  is_apartment + has_cleaning + listing_time +
  cleaning_fee + host_year +
  is_hotel + is_condo + is_house + is_loft +
  is_townhouse +
  require_guest_phone_verification + host_about_length +
  host_response100 + recent_update



# clean_train %>%
#   select(price, host_superhost , host_verified , host_since2 ,
#              calculated_host_listings_count_entire_homes ,
#              calculated_host_listings_count_private_rooms ,
#              calculated_host_listings_count_shared_rooms ,
#              neighbourhood_group_cleansed , is_location_exact ,
#              number_of_reviews_ltm , bed_type , instant_bookable ,
#              cancellation_policy , description_length ,
#              name_length , space_length , bathrooms ,
#              bedrooms , beds , latitude , longitude ,
#              minimum_nights_avg_ntm , availability_365 ,
#              review_scores_value , last_rev_days ,
#              reviews_per_month , room_type , guests_included ,
#              extra_people , accommodates , availability_30 ,
#              review_scores_rating ,
#              review_scores_cleanliness ,
#              review_scores_location ,
#              has_transit , has_hrules , 
#              has_sdeposit ,
#              availability_60 , review_scores_checkin ,
#              review_scores_communication , review_scores_accuracy ,
#              has_IT , has_WF , has_AC , has_KN , has_WS , has_HT ,
#              has_TV , has_BF , has_GY , has_SB , availability_90 ,
#              has_DM , has_EV , has_PL , has_ES ,
#              is_apartment , has_cleaning , listing_time ,
#              cleaning_fee , host_year ,
#              is_hotel , is_condo , is_house , is_loft ,
#              is_townhouse ,
#              require_guest_phone_verification , host_about_length ,
#              host_response100 , recent_update) %>% summary()


########## RANDOM FOREST CROSS VALIDATION ##########

# CROSS VALIDATION TO FIND OPTIMAL MTRY
set.seed(258)
# CV random forest - remove coefficients that don't make sense
trControl <- trainControl(method= "cv", number = 3)
tuneGrid <- expand.grid(mtry = 30)

cvForest <- train(model_formula,
                  clean_train,
                  method = "rf",
                  ntree = 500,
                  trControl = trControl,
                  tuneGrid = tuneGrid)

cvForest
pred <- predict(cvForest, newdata = test)
rmse <- mean((pred - test$price)^2) %>% sqrt()
rmse

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
