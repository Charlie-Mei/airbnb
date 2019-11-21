library(tidyverse)
library(leaps)
library(glmnet)
library(gridExtra)
library(caret)
library(broom)
library(lubridate)
library(zipcode)

########## PREAMBLE ##########

# Import the data and create and train and test set
mydata <- read.csv("Data/analysisData.csv", stringsAsFactors = F)
scoring <- read.csv("Data/scoringData.csv", stringsAsFactors = F)
data("zipcode")

set.seed(1251)
split <- createDataPartition(mydata$price, p = 0.7, list = F, groups = 50)
train <- mydata[split, ]
test <- mydata[-split, ]

########## ABOUT THE HOST ##########

clean_train <- train

# Feature engineering on details of the host
clean_train <- train %>%
  mutate(host_since2 = as.Date(host_since) - as.Date("2019-01-01")) %>%
  mutate(host_superhost = ifelse(host_is_superhost == "t", 1, 0),
         host_verified = ifelse(host_identity_verified == "t", 1, 0),
         host_about_length = nchar(host_about),
         host_response100 = ifelse(host_response_rate == "100%", 1, 0) %>% as.factor())

test <- test %>%
  mutate(host_since2 = as.Date(host_since) - as.Date("2019-01-01")) %>%
  mutate(host_superhost = ifelse(host_is_superhost == "t", 1, 0),
         host_verified = ifelse(host_identity_verified == "t", 1, 0),
         host_about_length = nchar(host_about),
         host_response100 = ifelse(host_response_rate == "100%", 1, 0) %>% as.factor())


scoring <- scoring %>%
  mutate(host_since2 = as.Date(host_since) - as.Date("2019-01-01")) %>%
  mutate(host_superhost = ifelse(host_is_superhost == "t", 1, 0),
         host_verified = ifelse(host_identity_verified == "t", 1, 0),
         host_about_length = nchar(host_about),
         host_response100 = ifelse(host_response_rate == "100%", 1, 0) %>% as.factor())

recent <- c("yesterday", "today", "2 days ago", "3 days ago", "4 days ago",
            "5 days ago", "6 days ago", "a week ago", "1 week ago", "2 weeks ago")

clean_train$recent_update <- ifelse(clean_train$calendar_updated %in% recent,
                                    "Recent", "Not recent") %>% as.factor()
test$recent_update <- ifelse(test$calendar_updated %in% recent,
                                    "Recent", "Not recent") %>% as.factor()
scoring$recent_update <- ifelse(scoring$calendar_updated %in% recent,
                                    "Recent", "Not recent") %>% as.factor()


########## LOCATION ##########

### Latitude and longitude of zipcodes to use as additional features

clean_train <- left_join(clean_train, zipcode %>% select(-city, -state), by = c("zipcode" = "zip"))
test <- left_join(test, zipcode %>% select(-city, -state), by = c("zipcode" = "zip"))
scoring <- left_join(scoring, zipcode %>% select(-city, -state), by = c("zipcode" = "zip"))

# What to do with missing lats and lons
clean_train$latitude[is.na(clean_train$latitude)] <- mean(clean_train$latitude, na.rm = T)
clean_train$longitude[is.na(clean_train$longitude)] <- mean(clean_train$longitude, na.rm = T)

test$latitude[is.na(test$latitude)] <- mean(test$latitude, na.rm = T)
test$longitude[is.na(test$longitude)] <- mean(test$longitude, na.rm = T)

scoring$latitude[is.na(scoring$latitude)] <- mean(scoring$latitude, na.rm = T)
scoring$longitude[is.na(scoring$longitude)] <- mean(scoring$longitude, na.rm = T)


########## PROPERTY ##########

# Feature engineering about the property
clean_train <- clean_train %>%
  mutate(name_length = nchar(name),
         space_length = nchar(space),
         description_length = nchar(description),
         has_notes = nchar(notes),
         has_transit = ifelse(nchar(transit) != 0, 1, 0),
         has_hrules = ifelse(nchar(house_rules) != 0, 1, 0),
         has_sdeposit = ifelse(security_deposit > 0, 1, 0),
         has_cleaning_fee = ifelse(cleaning_fee > 0, 1, 0),
         is_apartment = ifelse(property_type == "Apartment", 1, 0) %>% as.factor(),
         is_hotel = ifelse(property_type == "Hotel", 1, 0) %>% as.factor(),
         is_house = ifelse(property_type == "House", 1, 0) %>% as.factor(),
         is_loft = ifelse(property_type == "Loft", 1, 0) %>% as.factor(),
         is_townhouse = ifelse(property_type == "Townhouse", 1, 0) %>% as.factor(),
         is_condo = ifelse(property_type == "Condominium", 1, 0) %>% as.factor(),
         has_cleaning = ifelse(cleaning_fee > 0, 1, 0) %>% as.factor()) %>%
  filter(!is.na(beds))
  


test <- test %>%
  mutate(name_length = nchar(name),
         space_length = nchar(space),
         description_length = nchar(description),
         has_notes = nchar(notes),
         has_transit = ifelse(nchar(transit) != 0, 1, 0),
         has_hrules = ifelse(nchar(house_rules) != 0, 1, 0),
         has_sdeposit = ifelse(security_deposit > 0, 1, 0),
         has_cleaning_fee = ifelse(cleaning_fee > 0, 1, 0),
         is_apartment = ifelse(property_type == "Apartment", 1, 0) %>% as.factor(),
         is_hotel = ifelse(property_type == "Hotel", 1, 0) %>% as.factor(),
         is_house = ifelse(property_type == "House", 1, 0) %>% as.factor(),
         is_loft = ifelse(property_type == "Loft", 1, 0) %>% as.factor(),
         is_townhouse = ifelse(property_type == "Townhouse", 1, 0) %>% as.factor(),
         is_condo = ifelse(property_type == "Condominium", 1, 0) %>% as.factor(),
         has_cleaning = ifelse(cleaning_fee > 0, 1, 0) %>% as.factor()) %>%
  filter(!is.na(beds))

scoring <- scoring %>%
  mutate(name_length = nchar(name),
         space_length = nchar(space),
         description_length = nchar(description),
         has_notes = nchar(notes),
         has_transit = ifelse(nchar(transit) != 0, 1, 0),
         has_hrules = ifelse(nchar(house_rules) != 0, 1, 0),
         has_sdeposit = ifelse(security_deposit > 0, 1, 0),
         has_cleaning_fee = ifelse(cleaning_fee > 0, 1, 0),
         is_apartment = ifelse(property_type == "Apartment", 1, 0) %>% as.factor(),
         is_hotel = ifelse(property_type == "Hotel", 1, 0) %>% as.factor(),
         is_house = ifelse(property_type == "House", 1, 0) %>% as.factor(),
         is_loft = ifelse(property_type == "Loft", 1, 0) %>% as.factor(),
         is_townhouse = ifelse(property_type == "Townhouse", 1, 0) %>% as.factor(),
         is_condo = ifelse(property_type == "Condominium", 1, 0) %>% as.factor(),
         has_cleaning = ifelse(cleaning_fee > 0, 1, 0) %>% as.factor())


### Feature engineering from amenities
# TV
locs <- grepl(pattern = "TV", clean_train$amenities)
clean_train$has_TV <- 0
clean_train$has_TV[locs] <- 1
clean_train$has_TV <- as.factor(clean_train$has_TV)

# Wifi
locs <- grepl(pattern = "Wifi", clean_train$amenities)
clean_train$has_WF <- 0
clean_train$has_WF[locs] <- 1
clean_train$has_WF <- as.factor(clean_train$has_WF)

# Internet
locs <- grepl(pattern = "Air conditioning", clean_train$amenities)
clean_train$has_AC <- 0
clean_train$has_AC[locs] <- 1
clean_train$has_AC <- as.factor(clean_train$has_AC)

# Kitchen
locs <- grepl(pattern = "Kitchen", clean_train$amenities)
clean_train$has_KN <- 0
clean_train$has_KN[locs] <- 1
clean_train$has_KN <- as.factor(clean_train$has_KN)

# Internet
locs <- grepl(pattern = "Internet", clean_train$amenities)
clean_train$has_IT <- 0
clean_train$has_IT[locs] <- 1
clean_train$has_IT <- as.factor(clean_train$has_IT)

# Washer
locs <- grepl(pattern = "Washer", clean_train$amenities)
clean_train$has_WS <- 0
clean_train$has_WS[locs] <- 1
clean_train$has_WS <- as.factor(clean_train$has_WS)

# Heating
locs <- grepl(pattern = "Heating", clean_train$amenities)
clean_train$has_HT <- 0
clean_train$has_HT[locs] <- 1
clean_train$has_HT <- as.factor(clean_train$has_HT)



# TV
locs <- grepl(pattern = "TV", test$amenities)
test$has_TV <- 0
test$has_TV[locs] <- 1
test$has_TV <- as.factor(test$has_TV)

# Wifi
locs <- grepl(pattern = "Wifi", test$amenities)
test$has_WF <- 0
test$has_WF[locs] <- 1
test$has_WF <- as.factor(test$has_WF)

# Internet
locs <- grepl(pattern = "Air conditioning", test$amenities)
test$has_AC <- 0
test$has_AC[locs] <- 1
test$has_AC <- as.factor(test$has_AC)

# Kitchen
locs <- grepl(pattern = "Kitchen", test$amenities)
test$has_KN <- 0
test$has_KN[locs] <- 1
test$has_KN <- as.factor(test$has_KN)

# Internet
locs <- grepl(pattern = "Internet", test$amenities)
test$has_IT <- 0
test$has_IT[locs] <- 1
test$has_IT <- as.factor(test$has_IT)

# Washer
locs <- grepl(pattern = "Washer", test$amenities)
test$has_WS <- 0
test$has_WS[locs] <- 1
test$has_WS <- as.factor(test$has_WS)

# Heating
locs <- grepl(pattern = "Heating", test$amenities)
test$has_HT <- 0
test$has_HT[locs] <- 1
test$has_HT <- as.factor(test$has_HT)


# TV
locs <- grepl(pattern = "TV", scoring$amenities)
scoring$has_TV <- 0
scoring$has_TV[locs] <- 1
scoring$has_TV <- as.factor(scoring$has_TV)

# Wifi
locs <- grepl(pattern = "Wifi", scoring$amenities)
scoring$has_WF <- 0
scoring$has_WF[locs] <- 1
scoring$has_WF <- as.factor(scoring$has_WF)

# Internet
locs <- grepl(pattern = "Air conditioning", scoring$amenities)
scoring$has_AC <- 0
scoring$has_AC[locs] <- 1
scoring$has_AC <- as.factor(scoring$has_AC)

# Kitchen
locs <- grepl(pattern = "Kitchen", scoring$amenities)
scoring$has_KN <- 0
scoring$has_KN[locs] <- 1
scoring$has_KN <- as.factor(scoring$has_KN)

# Internet
locs <- grepl(pattern = "Internet", scoring$amenities)
scoring$has_IT <- 0
scoring$has_IT[locs] <- 1
scoring$has_IT <- as.factor(scoring$has_IT)

# Washer
locs <- grepl(pattern = "Washer", scoring$amenities)
scoring$has_WS <- 0
scoring$has_WS[locs] <- 1
scoring$has_WS <- as.factor(scoring$has_WS)

# Heating
locs <- grepl(pattern = "Heating", scoring$amenities)
scoring$has_HT <- 0
scoring$has_HT[locs] <- 1
scoring$has_HT <- as.factor(scoring$has_HT)


# Breakfast

locs <- grepl(pattern = "Breakfast", clean_train$amenities)
clean_train$has_BF <- 0
clean_train$has_BF[locs] <- 1
clean_train$has_BF <- as.factor(clean_train$has_BF)

locs <- grepl(pattern = "Breakfast", test$amenities)
test$has_BF <- 0
test$has_BF[locs] <- 1
test$has_BF <- as.factor(test$has_BF)

locs <- grepl(pattern = "Breakfast", scoring$amenities)
scoring$has_BF <- 0
scoring$has_BF[locs] <- 1
scoring$has_BF <- as.factor(scoring$has_BF)


# Gym
locs <- grepl(pattern = "Gym", clean_train$amenities)
clean_train$has_GY <- 0
clean_train$has_GY[locs] <- 1
clean_train$has_GY <- as.factor(clean_train$has_GY)

locs <- grepl(pattern = "Gym", test$amenities)
test$has_GY <- 0
test$has_GY[locs] <- 1
test$has_GY <- as.factor(test$has_GY)

locs <- grepl(pattern = "Gym", scoring$amenities)
scoring$has_GY <- 0
scoring$has_GY[locs] <- 1
scoring$has_GY <- as.factor(scoring$has_GY)


locs <- grepl(pattern = "Doorman", clean_train$amenities)
clean_train$has_DM <- 0
clean_train$has_DM[locs] <- 1
clean_train$has_DM <- as.factor(clean_train$has_DM)
locs <- grepl(pattern = "Doorman", test$amenities)
test$has_DM <- 0
test$has_DM[locs] <- 1
test$has_DM <- as.factor(test$has_DM)
locs <- grepl(pattern = "Doorman", scoring$amenities)
scoring$has_DM <- 0
scoring$has_DM[locs] <- 1
scoring$has_DM <- as.factor(scoring$has_DM)

locs <- grepl(pattern = "Elevator", clean_train$amenities)
clean_train$has_EV <- 0
clean_train$has_EV[locs] <- 1
clean_train$has_EV <- as.factor(clean_train$has_EV)
locs <- grepl(pattern = "Elevator", test$amenities)
test$has_EV <- 0
test$has_EV[locs] <- 1
test$has_EV <- as.factor(test$has_EV)
locs <- grepl(pattern = "Elevator", scoring$amenities)
scoring$has_EV <- 0
scoring$has_EV[locs] <- 1
scoring$has_EV <- as.factor(scoring$has_EV)

locs <- grepl(pattern = "Pool", clean_train$amenities)
clean_train$has_PL <- 0
clean_train$has_PL[locs] <- 1
clean_train$has_PL <- as.factor(clean_train$has_PL)
locs <- grepl(pattern = "Pool", test$amenities)
test$has_PL <- 0
test$has_PL[locs] <- 1
test$has_PL <- as.factor(test$has_PL)
locs <- grepl(pattern = "Pool", scoring$amenities)
scoring$has_PL <- 0
scoring$has_PL[locs] <- 1
scoring$has_PL <- as.factor(scoring$has_PL)

locs <- grepl(pattern = "Essentials", clean_train$amenities)
clean_train$has_ES <- 0
clean_train$has_ES[locs] <- 1
clean_train$has_ES <- as.factor(clean_train$has_ES)
locs <- grepl(pattern = "Essentials", test$amenities)
test$has_ES <- 0
test$has_ES[locs] <- 1
test$has_ES <- as.factor(test$has_ES)
locs <- grepl(pattern = "Essentials", scoring$amenities)
scoring$has_ES <- 0
scoring$has_ES[locs] <- 1
scoring$has_ES <- as.factor(scoring$has_ES)

# Has subway
locs <- grepl(pattern = "subway", clean_train$description)
clean_train$has_SB <- 0
clean_train$has_SB[locs] <- 1
clean_train$has_SB <- as.factor(clean_train$has_SB)

locs <- grepl(pattern = "subway", test$description)
test$has_SB <- 0
test$has_SB[locs] <- 1
test$has_SB <- as.factor(test$has_SB)

locs <- grepl(pattern = "subway", scoring$description)
scoring$has_SB <- 0
scoring$has_SB[locs] <- 1
scoring$has_SB <- as.factor(scoring$has_SB)

########## REVIEWS ##########

clean_train <- clean_train %>%
  mutate(first_review = as.Date(first_review),
         last_review = as.Date(last_review)) %>%
  mutate(first_rev_days = as.Date("2019-01-01") - first_review,
         last_rev_days = as.Date("2019-01-01") - last_review)

test <- test %>%
  mutate(first_review = as.Date(first_review),
         last_review = as.Date(last_review)) %>%
  mutate(first_rev_days = as.Date("2019-01-01") - first_review,
         last_rev_days = as.Date("2019-01-01") - last_review)

scoring <- scoring %>%
  mutate(first_review = as.Date(first_review),
         last_review = as.Date(last_review)) %>%
  mutate(first_rev_days = as.Date("2019-01-01") - first_review,
         last_rev_days = as.Date("2019-01-01") - last_review)


### Listing time = Last review - first review
clean_train$first_review <- as.Date(clean_train$first_review)
clean_train$last_review <- as.Date(clean_train$last_review)
clean_train$listing_time <- clean_train$last_review - clean_train$first_review

test$first_review <- as.Date(test$first_review)
test$last_review <- as.Date(test$last_review)
test$listing_time <- test$last_review - test$first_review

scoring$first_review <- as.Date(scoring$first_review)
scoring$last_review <- as.Date(scoring$last_review)
scoring$listing_time <- scoring$last_review - scoring$first_review


### More cleaning - repeated on clean, test and scoring

clean_train$host_since2[is.na(clean_train$host_since2)] <- mean(clean_train$host_since2, na.rm = T)
clean_train$first_rev_days[is.na(clean_train$first_rev_days)] <- mean(clean_train$first_rev_days, na.rm = T)
clean_train$last_rev_days[is.na(clean_train$last_rev_days)] <- mean(clean_train$last_rev_days, na.rm = T)
clean_train$reviews_per_month[is.na(clean_train$reviews_per_month)] <- mean(clean_train$reviews_per_month, na.rm = T)
clean_train$has_sdeposit[is.na(clean_train$has_sdeposit)] <- 0
clean_train$is_apartment[is.na(clean_train$is_aparment)] <- 0
clean_train$has_cleaning[is.na(clean_train$has_cleaning)] <- 0
clean_train$listing_time[is.na(clean_train$listing_time)] <- mean(clean_train$listing_time, na.rm = T)
clean_train$cleaning_fee[is.na(clean_train$cleaning_fee)] <- mean(clean_train$cleaning_fee, na.rm = T)
clean_train$host_since <- as.Date(clean_train$host_since)
clean_train$host_year <- year(clean_train$host_since)
clean_train$host_year[is.na(clean_train$host_year)] <- mean(clean_train$host_year, na.rm = T)
clean_train$host_about_length[is.na(clean_train$host_about_length)] <- mean(clean_train$host_about_length, na.rm = T)

test$host_since2[is.na(test$host_since2)] <- mean(test$host_since2, na.rm = T)
test$first_rev_days[is.na(test$first_rev_days)] <- mean(test$first_rev_days, na.rm = T)
test$last_rev_days[is.na(test$last_rev_days)] <- mean(test$last_rev_days, na.rm = T)
test$reviews_per_month[is.na(test$reviews_per_month)] <- mean(test$reviews_per_month, na.rm = T)
test$has_sdeposit[is.na(test$has_sdeposit)] <- 0
test$is_apartment[is.na(test$is_aparment)] <- 0
test$has_cleaning[is.na(test$has_cleaning)] <- 0
test$listing_time[is.na(test$listing_time)] <- mean(test$listing_time, na.rm = T)
test$cleaning_fee[is.na(test$cleaning_fee)] <- mean(test$cleaning_fee, na.rm = T)
test$host_since <- as.Date(test$host_since)
test$host_year <- year(test$host_since)
test$host_year[is.na(test$host_year)] <- mean(test$host_year, na.rm = T)
test$host_about_length[is.na(test$host_about_length)] <- mean(test$host_about_length, na.rm = T)


scoring$host_since2[is.na(scoring$host_since2)] <- mean(scoring$host_since2, na.rm = T)
scoring$first_rev_days[is.na(scoring$first_rev_days)] <- mean(scoring$first_rev_days, na.rm = T)
scoring$last_rev_days[is.na(scoring$last_rev_days)] <- mean(scoring$last_rev_days, na.rm = T)
scoring$reviews_per_month[is.na(scoring$reviews_per_month)] <- mean(scoring$reviews_per_month, na.rm = T)
scoring$has_sdeposit[is.na(scoring$has_sdeposit)] <- 0
scoring$is_apartment[is.na(scoring$is_aparment)] <- 0
scoring$has_cleaning[is.na(scoring$has_cleaning)] <- 0
scoring$listing_time[is.na(scoring$listing_time)] <- mean(scoring$listing_time, na.rm = T)
scoring$cleaning_fee[is.na(scoring$cleaning_fee)] <- mean(scoring$cleaning_fee, na.rm = T)
scoring$host_since <- as.Date(scoring$host_since)
scoring$host_year <- year(scoring$host_since)
scoring$host_year[is.na(scoring$host_year)] <- mean(scoring$host_year, na.rm = T)
scoring$host_about_length[is.na(scoring$host_about_length)] <- mean(scoring$host_about_length, na.rm = T)


########## SAVE CLEANING OUTPUTS ##########

save(clean_train, file = "Cleaned_data/train.Rdata")
save(test, file = "Cleaned_data/test.Rdata")
save(scoring, file = "Cleaned_data/scoring.Rdata")