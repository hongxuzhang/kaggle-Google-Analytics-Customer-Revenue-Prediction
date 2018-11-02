# this is a script for cleaning and exploring data for customer revenue prediction in Kaggle 
# Developed by Hongxu,Zhang
# 20181103

# read libraries
library(dplyr) # data manipulation
library(tidyr)
library(readxl)
library(data.table)
library(splitstackshape)
library(jsonlite)
library(lubridate)
library(magrittr)
library(readr)
library(ggplot2)
library(reshape2)
library(readr)
library(dplyr)
library(stringr)
library(data.table)

# # read data 
setwd("/Users/AA366716/2018_Kaggle/customer_revenue_predict")
df_test <- fread("test.csv")
df_train <- fread("train.csv")

# ########### get clean dataset ########
# merge test and train (for same preprocess)
df_test$tag <- "test"
df_train$tag <- "train"
# 
df <- rbind(df_test, df_train)
# 
# #-- deal with json 2nd way
flatten_json <- . %>%  # simple way of writing function
  gsub('\"\"','\"',.) %>%
  str_c(., collapse = ",") %>%  # add rows together
  str_c("[", ., "]") %>%   # to make list
  fromJSON(flatten = T)
# 
device <- bind_cols(flatten_json(df$device))
trafficSource <- bind_cols(flatten_json(df$trafficSource))
geoNetwork <- bind_cols(flatten_json(df$geoNetwork))
totals <- bind_cols(flatten_json(df$totals))
# 
# the rest of the dataframe
df_rest <- df %>% select(-device, -geoNetwork,-totals, -trafficSource)
df_clean <- cbind(df_rest, device, geoNetwork,totals, trafficSource)

# # clean_df backup
write.csv(df_clean, "df_clean.csv",row.names = FALSE)


################ EDA  #############
# read data 
setwd("/Users/AA366716/2018_Kaggle/customer_revenue_predict")
df_clean <- fread("df_clean.csv", stringsAsFactors = TRUE)

#----- deal with data type------
#converting the data variable to the date format
df_clean$date <- ymd(df_clean$date)

#converting variables types
numVars <- c("visits", "hits", "bounces", "pageviews", "newVisits")
df_clean[, numVars] <- lapply(df_clean[, ..numVars], as.integer)
df_clean$transactionRevenue[df_clean$tag=="train"] <- as.numeric(df_clean$transactionRevenue[df_clean$tag=="train"])
df_clean$visitStartTime <- as.POSIXct(df_clean$visitStartTime, tz="UTC", origin='1970-01-01')

# ################# add Feature ##########
# df_clean$weekday <- wday(df_clean$date, label=TRUE)
# df_clean$month <- month(df_clean$date, label=TRUE)
# df_clean$sourceMedium <- paste(df_clean$source, df_clean$medium, sep="/")
# df_clean$sessionHourOfDay <- hour(df_clean$visitStartTime)

# # divide train and test 
train <- df_clean %>% filter(tag=="train") %>% select(-tag)
test <- df_clean %>% filter(tag=="test")

colSums(is.na(train0))
#---------------------- check train data -------------
# change [not avaiable] into NAs
is_na_val <- c("not available in demo dataset", "(not provided)",
               "(not set)", "(not set)", "<NA>", "unknown.unknown",  "(none)")
train0 <- mutate_all(train, funs(replace(., .%in%is_na_val, NA)))

colSums(is.na(train0))
dim(train0) #55 cols
glimpse(train0)
str(train0)

#-- deal with NAs 
# delete columns that only have NAs
colSums(is.na(train0)) #check
train1 = train0[, colSums(is.na(train0)) != nrow(train0)]
dim(train1) # 38 cols
glimpse(train1)

train2 <- train1 %>% mutate(isMobile = ifelse(isMobile, 1L, 0L),
                          isTrueDirect = ifelse(isTrueDirect, 1L, 0L),
                          adwordsClickInfo.isVideoAd = ifelse(!adwordsClickInfo.isVideoAd, 0L, 1L))

colSums(is.na(train2))

train3 <- train2 %>% 
  dplyr::mutate(transactionRevenue=ifelse(is.na(transactionRevenue), 0, transactionRevenue),
                visits=ifelse(is.na(visits), 0, visits),
                hits=ifelse(is.na(hits), 0, hits),
                pageviews=ifelse(is.na(pageviews), 0, pageviews),
                newVisits=ifelse(is.na(newVisits), 0, newVisits),
                bounces=ifelse(is.na(bounces), 0, bounces))

glimpse(train3)
str(train3)

# add hasRevenue
train4 <- train3 %>% 
  dplyr::mutate(hasRevenue=ifelse(transactionRevenue>0, 1, 0))

# --- check browser ----
brs <- train4 %>% 
  dplyr::group_by(browser) %>% 
  summarise(n=n()) %>% 
  filter(n>100)

train5 <- train4 %>% 
  dplyr::mutate(browser=as.character(browser)) %>% 
  dplyr::mutate(browser= ifelse(browser %in% brs$browser, browser, "Other"))


ggplot(train5, aes(x=factor(browser), group = hasRevenue))+
  geom_bar(aes(y = ..prop.., fill = factor(hasRevenue)), stat="count", position=position_dodge()) + 
  # geom_bar(stat="count", width=0.7, fill=hasRevenue)+
  # facet_grid(~hasRevenue)+
  scale_fill_brewer(palette="Paired")+ # change the color 
  theme_bw(base_family = "HiraKakuPro-W3")+
  labs(title="hasRevenue X Browser")+
  theme(axis.text.x=element_text(angle=-90, hjust=1))


# ----- check isMobile ----
ggplot(train4, aes(x=factor(isMobile), group = hasRevenue))+
  geom_bar(aes(y = ..prop.., fill = factor(hasRevenue)), stat="count", position=position_dodge()) + 
  # geom_bar(stat="count", width=0.7, fill=hasRevenue)+
  # facet_grid(~hasRevenue)+
  scale_fill_brewer(palette="Paired")+ # change the color 
  theme_bw(base_family = "HiraKakuPro-W3")+
  labs(title="hasRevenue X isMobile")


# check 10~19
names(train[,10:20])

# BARPLOT PERCENTAGE with filling factors
myplot <- ggplot(yakuzaishi4, aes(x=SHAIN_KBN, group = RM_FLAG)) + 
  geom_bar(aes(y = ..prop.., fill = factor(RM_FLAG)), stat="count", position=position_dodge()) + 
  # scale_y_continuous(labels=scales::percent) + # to percentage 34%
  scale_fill_brewer(palette="Paired")+ # change the color 
  theme_bw(base_family = "HiraKakuPro-W3")+
  labs(title="指導記録のある出勤日数")+
  labs(x="ロールモデル区分")+
  labs(y="日数")
# facet_grid(~vs)
myplot

