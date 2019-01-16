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
library(rlist)
library(caret)
library(lightgbm)

################ read data  #############
# read data 
setwd("/Users/AA366716/2018_Kaggle/customer_revenue_predict")
train_raw <- readRDS("train_v2_clean.rds")
test_raw <- readRDS("test_v2_clean.rds")
# submission2 <- fread("sample_submission_v2.csv")
table(unique(test_raw$fullVisitorId) %in% unique(train_raw$fullVisitorId))

c2 <-test_raw %>% arrange(., fullVisitorId, visitNumber)

# ############ Clean data ##########
# #++++++++++++++++++ delete uncessary colums ++++++++++
# #--------cccccheck -----
# # colSums(is.na(train1))
# # c <- train0 %>% group_by(adwordsClickInfo.isVideoAd) %>% summarise(N=n()) %>% dplyr::arrange(desc(N))
# #--------cccccheck -----
# 
# # change NA meaning words into NAs 
# is_na_val <- c("not available in demo dataset", "(not provided)",
#                "(not set)", "<NA>",  "(none)")
# train_raw0 <- mutate_all(train_raw, funs(replace(., .%in%is_na_val, NA)))
# test_raw0 <- mutate_all(test_raw, funs(replace(., .%in%is_na_val, NA)))
# 
# #--------cccccheck -----
# # x <- train_raw0 %>% filter(grepl("not available in demo dataset", train_raw0))
# # c <- train_raw0 %>% group_by(keyword) %>% summarise(N=n()) %>% dplyr::arrange(desc(N))
# 
# # Eliminate Columns&Rows with all NAs 
# train0 <- train_raw0 %>% select_if(colSums(is.na(.)) != nrow(train_raw))
# test0 <- test_raw0 %>% select_if(colSums(is.na(.)) != nrow(test_raw))
# 
# # Columns with Constant Values 
# cols_same_values = train0 %>% select_if(function(col) length(unique(col)) <= 1) %>% names(.)
# # columns in train not test
# cols_only_in_train <- setdiff(names(train0), names(test0))
# 
# train1 <- train0 %>% select(-(cols_same_values), -(cols_only_in_train)) # 35
# test1 <- test0 %>% select(-(cols_same_values)) 

# dim(train1)
# dim(test1)
# saveRDS(train1, "train1.rds")
# saveRDS(test1, "test1.rds")
setwd("/Users/AA366716/2018_projects/TAKEDA_ph5/data")
tt <- read.csv("VEV_CALL_DISCUSSION_DRUGMAP.csv")




setwd("/Users/AA366716/2018_Kaggle/customer_revenue_predict")
train1_raw <- readRDS("train1.rds")
test1_raw <- readRDS("test1.rds")

# ################cccccccccccccccccccccccccccccc#####################
# #++++++++++++++++ check fullvisitorid
# sub_ids <- submission2 %>% select(fullVisitorId)
# test_ids <- test_raw %>% select(fullVisitorId) %>% distinct()
# train_ids <- train_raw %>% select(fullVisitorId) %>% distinct()
# 
# # all test ids and sub ids are same 
# table(test_ids$fullVisitorId %in% sub_ids$fullVisitorId)
# 
# # train ids: only 2759 ids are in sub
# table(train_ids$fullVisitorId %in% sub_ids$fullVisitorId)
# 
# # train ids and test ids 
# table(test_ids$fullVisitorId %in% train_ids$fullVisitorId)
# ################cccccccccccccccccccccccccccccc#####################


################ make train  #########
# train: 2017/5/1 ~ 2017/10/15
# predict_target : 2017/12/1 ~ 2018/1/31
train1_old <- train1_raw %>% dplyr::filter(date>=20161201, date<=20170430)
train1 <- train1_raw %>% dplyr::filter(date>=20170501, date<=20171015)
target <- train1_raw %>% dplyr::filter(date>=20171201, date<= 20180131)
test1_old <- train1_raw %>% dplyr::filter(date>=20171201, date<=20180430) 

# common
add_data_before <- function(test1_old){
  
test1_old_n<- test1_old %>% 
  dplyr::mutate(month = month(ymd(date), label = TRUE), 
                transactionRevenue = ifelse(is.na(transactionRevenue), 0, as.numeric(transactionRevenue))) %>% 
  dplyr::filter(fullVisitorId %in% test1_raw$fullVisitorId) %>% 
  dplyr::group_by(fullVisitorId) %>% 
  dplyr::summarise(revenue_log_before = log1p(sum(transactionRevenue)),
                   visit_times_before = n())
  
  return(test1_old_n)
}

test1_old_n <- add_data_before(test1_old)
train1_old_n <- add_data_before(train1_old)


# hist(train1_old_n$revenue_log_before, breaks=30)
# hist(target0$revenue_log, breaks=30)

# #how many has no revenue are from just one visit
# onevisit <- train1 %>% group_by(fullVisitorId) %>% summarise(N=n()) %>%
#   filter(N==1)
# check <- target0 %>%
#   mutate(isonevisit = ifelse(fullVisitorId %in% onevisit$fullVisitorId & visitNumber_max==1, 1, 0))
# 
# myplot <- ggplot(check, aes(x=hasRevenue, group = isonevisit)) +
#   geom_bar(aes(y = ..prop.., fill = factor(isonevisit)), stat="count", position=position_dodge()) +
#   # scale_y_continuous(labels=scales::percent) + # to percentage 34%
#   scale_fill_brewer(palette="Paired")+ # change the color
#   theme_bw(base_family = "HiraKakuPro-W3")
# 
# myplot


target0 <- target %>%
  dplyr::mutate(transactionRevenue=ifelse(is.na(transactionRevenue), 0, transactionRevenue)) %>% 
  dplyr::mutate(transactionRevenue = as.numeric(transactionRevenue)) %>%
  dplyr::group_by(fullVisitorId) %>% 
  dplyr::summarise(revenue_all = sum(transactionRevenue),
                   visitNumber_max= max(visitNumber)) %>% 
  dplyr::mutate(hasRevenue = ifelse(revenue_all>0, 1, 0),
                revenue_log = log1p(revenue_all)) 

target_revenue <- target0 %>% select(-revenue_all, -hasRevenue, -visitNumber_max)  

# # check how many actuall returned 0.7% not even 1%
# table(unique(tr$fullVisitorId) %in% unique(tt$fullVisitorId))


# df<- convert_time(train1)
# df2 <-df %>% dplyr::mutate(sessionHourOfDay = hour(visitStartTime)) %>% 
#   dplyr::mutate(sessionTime = ifelse(sessionHourOfDay %in% c(9,10,11,12,13,14,15,16,17), 1, 0))

################ clean train ##################
#++++++++++++++++ Deal with NAs ++++++++++++++++
# transactionRevenue NAs
feature_engineer <-function(train1) {

  train1 <- test1
  na_to_value <- function(df){
    df <- df %>% 
      dplyr::mutate(hits=ifelse(is.na(hits), 0, as.integer(hits)),
                    pageviews=ifelse(is.na(pageviews), 0, as.integer(pageviews)),
                    bounces=ifelse(is.na(bounces), 0, as.integer(bounces)),
                    newVisits=ifelse(is.na(newVisits), 0, as.integer(newVisits)),
                    sessionQualityDim = ifelse(is.na(sessionQualityDim), 0, as.integer(sessionQualityDim)),
                    timeOnSite = ifelse(is.na(timeOnSite), 0, as.integer(timeOnSite)),
                    transactions=ifelse(is.na(transactions),0, as.integer(transactions)),
                    transactionRevenue=ifelse(is.na(transactionRevenue),0, as.numeric(transactionRevenue)),
                    # totalTransactionRevenue=ifelse(is.na(totalTransactionRevenue),0, as.numeric(totalTransactionRevenue)),
                    isTrueDirect=ifelse(is.na(isTrueDirect), "FALSE", isTrueDirect),
                    adwordsClickInfo.isVideoAd=ifelse(is.na(adwordsClickInfo.isVideoAd), "TRUE", adwordsClickInfo.isVideoAd))#NA means isnotTrueDirect
    return(df)
  }
  
  train2 <- na_to_value(train1)
  
  
  #++++++++++++++++++ fix date type  +++++++++++++++
  # fix time in df
  convert_time <- function(df){
    df$visitStartTime <- as.POSIXct(df$visitStartTime, tz="UTC", origin='1970-01-01')
    df$date <- ymd(df$date %>% as.factor)
    return(df)
  }
  train3 <- convert_time(train2)
  # test3 <- convert_time(test2)
 
  
  #++++++++++++++++++  add new feature about time +++++++++++++++
  add_feature <- function(df){
  
    df<- df %>%
      dplyr::mutate(sessionHourOfDay = hour(visitStartTime)) %>% 
      dplyr::mutate(sessionTime = ifelse(sessionHourOfDay %in% c(9,10,11,12,13,14,15,16,17), 1, 0)) %>% 
      # dplyr::mutate(weekday = wday(date, label = TRUE)) %>%
      # dplyr::mutate(week = week(date)) %>% 
      dplyr::mutate(month = month(date, label = TRUE)) %>% 
      dplyr::mutate(hascampaign = ifelse(is.na(campaign), 0, 1)) %>% 
      dplyr::mutate(isMobile = ifelse(isMobile==FALSE, 0, 1)) %>% 
      dplyr::mutate(isTrueDirect = ifelse(isTrueDirect==FALSE, 0, 1)) %>% 
      # dplyr::mutate(sessionQualityDim_high=ifelse(sessionQualityDim>80, 1, 0)) %>%
      # dplyr::mutate(df$sourceMedium = paste(source, medium, sep="/")) %>% 
      dplyr::mutate(hasRevenue = ifelse(transactionRevenue>0, 1, 0)) %>% 
      dplyr::mutate(timeOnSite_zero = ifelse(timeOnSite ==0, 0, 1)) %>%
      dplyr::mutate(hasAd = ifelse(is.na(adContent), 0, 1)) %>% 
      dplyr::mutate(adContent = ifelse(is.na(adContent), "", adContent)) %>% 
      dplyr::mutate(adwordsClickInfo.page = ifelse(is.na(adwordsClickInfo.page), "", adwordsClickInfo.page)) %>% 
      dplyr::mutate(adwordsClickInfo.slot = ifelse(is.na(adwordsClickInfo.slot), "", adwordsClickInfo.slot)) %>% 
      dplyr::select(-adwordsClickInfo.gclId) %>% 
      dplyr::mutate(adwordsClickInfo.adNetworkType = ifelse(is.na(adwordsClickInfo.adNetworkType), "", adwordsClickInfo.adNetworkType)) %>% 
      dplyr::mutate(ad = paste(adContent, adwordsClickInfo.page, adwordsClickInfo.slot, adwordsClickInfo.adNetworkType, sep=" ")) %>% 
      dplyr::mutate(adwordsClickInfo.isVideoAd = ifelse(is.na(adwordsClickInfo.isVideoAd), 1, 0))  
      
    return(df)
  }
  
  train4 <- add_feature(train3)
  # test4 <- add_feature(test3)
  
  # ------------- plot ------------
  # qplot(sessionQualityDim, log1p(transactionRevenue), data=train4, colour=hasRevenue)
  # qplot(timeOnSite, log1p(transactionRevenue), data=train4, colour=hasRevenue)
  # cor.test(train4$timeOnSite, log1p(train4$transactionRevenue))
  
  
  # add time features
  month_data <- train4 %>% 
    dplyr::group_by(fullVisitorId, month) %>% 
    dplyr::summarise(revenue = sum(transactionRevenue)) %>% 
    tidyr::spread(., key=month, value=revenue, fill=0) %>% 
    dplyr::mutate(M567=log1p(` 5` + ` 6` + ` 7`),
                  M678 =log1p(` 6` +` 7`+` 8`),
                  M789 =log1p( ` 7` +` 8`+` 9`),
                  M890 =log1p(` 8` +` 9`+ `10`)) %>% 
    dplyr::select(fullVisitorId,M567,M678,M789,M890)
    
  
  # week_data <- train4 %>% 
  #   dplyr::group_by(fullVisitorId, week) %>% 
  #   dplyr::summarise(revenue = log1p(sum(transactionRevenue))) %>% 
  #   # dplyr::mutate(revenue = ifelse(revenue==0, 1, revenue)) %>% 
  #   tidyr::spread(., key=week, value=revenue, fill=0)
  # 
  train4 <- train4 %>% 
    dplyr::group_by(fullVisitorId) %>% 
    dplyr::arrange(., visitNumber, .by_group=TRUE) 
  
  # add_aggregate_features <- function(df){
  train5_1 <- train4 %>% 
    dplyr::group_by(fullVisitorId) %>% 
    dplyr::summarise(
                     # channelGrouping_all = paste(channelGrouping, collapse = "_"),
                     visitNumber_max= max(visitNumber, na.rm=TRUE),
                     visitNumber_min= min(visitNumber, na.rm=TRUE),
                     visitNumber_mean = mean(visitNumber, na.rm=TRUE),
                     visitNumber_change = paste(visitNumber, collapse="_"),
                     isMobile_rate = sum(isMobile)/n(),
                     # browser_all = paste(browser, collapse = "_"),
                     # operatingSystem_all = paste(operatingSystem, collapse = "_"),
                     # deviceCategory_all = paste(deviceCategory, collapse = "_"),
                     # continent_all = paste(continent, collapse = "_"),
                     # subContinent_all = paste(subContinent, collapse = "_"),
                     # country_all = paste(country, collapse = "_"),
                     # region_all = paste(region, collapse = "_"),
                     # metro_all = paste(metro, collapse = "_"),
                     # city_all = paste(city, collapse = "_"),
                     # networkDomain_all = paste(networkDomain, collapse = "_"),
                     hits_total = sum(hits, na.rm=TRUE),
                     hits_max = max(hits, na.rm=TRUE),
                     hits_min = min(hits, na.rm=TRUE),
                     hits_mean = mean(hits, na.rm=TRUE),
                     hits_sd = sd(hits, na.rm=TRUE),
                     hits_change = paste(hits, collapse="_"),
                     pageviews_total = sum(pageviews, na.rm=TRUE),
                     pageviews_max = max(pageviews, na.rm=TRUE),
                     pageviews_min = min(pageviews, na.rm=TRUE),
                     pageviews_mean = mean(pageviews, na.rm=TRUE),
                     pageviews_sd = sd(pageviews, na.rm=TRUE),
                     pageviews_change = paste(pageviews, collapse="_"),
                     bounces_times_total = sum(bounces),
                     # bounces_times_mean = mean(bounces), # since its boolean mean does not mean much 
                     bounces_rate = sum(bounces)/n(), # bounce rate if high probably not gonna generate high revenue 
                     newVisits_times_total = sum(newVisits),
                     # newVisits_times_mean = mean(newVisits), # since its boolean mean does not mean much 
                     newVisits_rate = sum(newVisits)/n(), # bounce rate if high probably not gonna generate high revenue 
                     sessionQualityDim_total = sum(sessionQualityDim, na.rm=TRUE),
                     sessionQualityDim_max = max(sessionQualityDim, na.rm=TRUE),
                     sessionQualityDim_min = min(sessionQualityDim, na.rm=TRUE),
                     sessionQualityDim_mean = mean(sessionQualityDim, na.rm=TRUE),
                     sessionQualityDim_sd = sd(sessionQualityDim, na.rm=TRUE),
                     sessionQualityDim_change = paste(sessionQualityDim, collapse="_"),
                     # sessionQualityDim_high_rate = sum(sessionQualityDim_high)/n(), 
                     timeOnSite_total = sum(timeOnSite, na.rm=TRUE),
                     timeOnSite_max = max(timeOnSite, na.rm=TRUE),
                     timeOnSite_min = min(timeOnSite, na.rm=TRUE),
                     timeOnSite_mean = mean(timeOnSite, na.rm=TRUE),
                     timeOnSite_sd = sd(timeOnSite, na.rm=TRUE),
                     timeOnSite_zero_rate = sum(timeOnSite_zero)/n(),
                     timeOnSite_change = paste(timeOnSite, collapse="_"),
                     transactions_total = sum(transactions, na.rm=TRUE),
                     transactions_max = max(transactions, na.rm=TRUE),
                     transactions_min = min(transactions, na.rm=TRUE),
                     transactions_mean = mean(transactions, na.rm=TRUE),
                     transactions_sd = sd(transactions, na.rm=TRUE),
                     transactionRevenue_total_log = log1p(sum(transactionRevenue, na.rm=TRUE)),
                     transactionRevenue_total = sum(transactionRevenue, na.rm=TRUE),
                     transactionRevenue_max = max(transactionRevenue, na.rm=TRUE),
                     transactionRevenue_min = min(transactionRevenue, na.rm=TRUE),
                     transactionRevenue_mean = mean(transactionRevenue, na.rm=TRUE),
                     transactionRevenue_sd = sd(transactionRevenue, na.rm=TRUE),
                     # campaign_all = paste(campaign, collapse = "_"),
                     # source_all = paste(source, collapse = "_"),
                     # medium_all = paste(medium, collapse = "_"),
                     # keyword_all = paste(keyword, collapse = "_"),
                     # referralPath_all = paste(referralPath, collapse = "_"),
                     # ad_all= paste(ad, collapse = "_"),
                     isTrueDirect_rate = sum(isTrueDirect)/n(),
                     adwordsClickInfo.isVideoAd_rate = sum(adwordsClickInfo.isVideoAd)/n(),
                     hasAd_rate = sum(hasAd)/n(),
                     hascampaign_rate = sum(hascampaign)/n(),
                     hasRevenue_rate = sum(hasRevenue)/n(),
                     officeTime_rate = sum(sessionTime)/n(),
                     visit_times = n()
                     )
  
  train4[is.na(train4)]<-""
  train5_2 <- train4 %>% 
    dplyr::select(fullVisitorId,channelGrouping, browser, operatingSystem, deviceCategory,continent, 
                  subContinent, country, region, metro, city, networkDomain, campaign, 
                  source, medium, keyword, referralPath,ad) %>%
    dplyr::group_by(fullVisitorId) %>% 
    dplyr::summarise(
      channelGrouping = first(channelGrouping),
      browser = first(browser),
      operatingSystem = first(operatingSystem),
      deviceCategory = first(deviceCategory),
      continent = first(continent),
      subContinent = first(subContinent),
      country = first(country),
      region = first(region),
      metro = first(metro),
      city = first(city),
      networkDomain = first(networkDomain),
      campaign_all = paste(campaign, collapse = ""),
      source = first(source),
      medium = first(medium),
      keyword_all = paste(keyword, collapse = ""),
      referralPath_all = paste(referralPath, collapse = ""),
      ad_all= paste(ad, collapse = ""))
  
  tem <- data.frame(train5_1, train5_2, month_data) %>% 
    dplyr::select(-fullVisitorId.1, -fullVisitorId.2)
  
  return(tem)
}

# ------ train make feature 
tem <- feature_engineer(train1)
train <- tem %>%
  dplyr::left_join(., target_revenue, by="fullVisitorId") %>%
  dplyr::mutate(revenue_log = ifelse(is.na(revenue_log), 0, revenue_log)) %>%
  dplyr::select(revenue_log, fullVisitorId:M890) %>% 
  dplyr::left_join(., train1_old_n, by="fullVisitorId") %>% 
  dplyr::mutate(revenue_log_before= ifelse(is.na(revenue_log_before),0, revenue_log_before)) %>% 
  dplyr::mutate(visit_times_before= ifelse(is.na( visit_times_before),0,  visit_times_before)) 

saveRDS(train, "traindata_ready_v3.rds")

# ------ test make feature 
test1 <- readRDS("test1.rds")
test_tem <- feature_engineer(test1)
test <- test_tem %>%
  dplyr::left_join(., test1_old_n, by="fullVisitorId") %>% 
  dplyr::mutate(revenue_log_before= ifelse(is.na(revenue_log_before),0, revenue_log_before)) %>% 
  dplyr::mutate(visit_times_before= ifelse(is.na( visit_times_before),0,  visit_times_before)) 
saveRDS(test, "testdata_ready_v3.rds")

# #00000---------submit correct answers to check
# sub <- read_csv("sample_submission_v2.csv")
# correct <- test1 %>% 
#   dplyr::select(fullVisitorId, transactionRevenue) %>% 
#   dplyr::mutate(transactionRevenue=ifelse(is.na(transactionRevenue), 0, as.numeric(transactionRevenue)))
# 
# dat <- correct %>% 
#   group_by(fullVisitorId) %>% 
#   summarise(PredictedLogRevenue= log1p(sum(transactionRevenue)))
# 
# write.csv(dat, "submission_correctAnswer.csv", row.names = FALSE)


########### add these features later #############
########## other features can think of #########
#--visit features right before buying
#--visit features right after last buying
# visit_times_since_last_buy
# time_since_last_buy
# revenue_of_last_buy
# revenue_monthly
# revenue_every_visit
# mean_invertals_between_each_buying

setwd("/Users/AA366716/2018_Kaggle/customer_revenue_predict")
train <- readRDS("traindata_ready_v2.rds")
test <- readRDS("testdata_ready_v2.rds")


# #############tttttttt test
# tem <- data.frame(month_data, week_data) %>%
#   dplyr::select(-fullVisitorId.1)
# 
# train <- tem %>% 
#   dplyr::left_join(., target_revenue, by="fullVisitorId") %>% 
#   dplyr::mutate(revenue_log = ifelse(is.na(revenue_log), 0, revenue_log)) %>% 
#   dplyr::mutate(variable = revenue_log) %>% 
#   dplyr::select(revenue_log,variable, fullVisitorId)



##################### define functions ###############
get_rmse <- function(lgb.model, x_test, y_test){
  pred <- predict(lgb.model, as.matrix(x_test)) %>% as.data.frame()
  colnames(pred) <- "pred"
  
  y_test <- y_test %>% as.data.frame()
  colnames(y_test)<-"true"
  
  # rmse= sqrt(mean((pred$pred - y_test$true)^2))
  rs <- cbind(pred, y_test) %>% 
    # dplyr::mutate(pred = ifelse(pred <= 0.01, 0, pred)) %>%   # since most will be 0
    dplyr::mutate(diff = (pred - true)^2) 
  rmse= mean(rs$diff) %>% sqrt(.)
  
  return(rmse)
}




######################### grid search ######################### 
# 組み合わせ決定
# grid_search <- expand.grid(num_leaves= c(3,5,7,10,15,20),
#                            learning_rate = c(0.01, 0.03, 0.1, 0.3, 0.4, 0.8),
#                            max_depth=c(3,4,6,8,15)
# )
# 
# perf <- numeric(nrow(grid_search))
# 
# #グリッドサーチ実行
# i=0
# for (i in 1:nrow(grid_search)) {
#   perf[i] <- my_cv(grid_search[i, "num_leaves"], grid_search[i, "learning_rate"],grid_search[i, "max_depth"])
#   gc(verbose = FALSE)
# }
# 
# 
# bst_num_leaves <- grid_search[which.min(perf),"num_leaves"]
# bst_learning_rate <- grid_search[which.min(perf),"learning_rate"]
# bst_max_depth <- grid_search[which.min(perf),"max_depth"]

setwd("/Users/AA366716/2018_Kaggle/customer_revenue_predict")
train <- readRDS("traindata_ready_v3.rds")
test <- readRDS("testdata_ready_v3.rds")


###### パラメータ
bst_num_leaves<- 5
bst_learning_rate <-0.1
bst_max_depth <-4

######################### lightGBM Cross Validation ##############################3
#--- folds作成
fullVisitorIds <- train %>% select(fullVisitorId)
train <- train %>% select(-fullVisitorId)

set.seed(1204)
folds<-createFolds(y=train$revenue_log, k=5,
                   list=TRUE, returnTrain=FALSE)

categorical_feature <- train %>% select_if(is.character) %>% names(.)
train_data <- mutate_if(train, is.character, as.factor) %>% 
  mutate_if(., is.factor, as.numeric)

# --------- lightGBM 回す ----------------
# my_cv <- function(bst_num_leaves, bst_learning_rate, bst_max_depth){

  test_rs <-c()
  train_rs <- c()
  
  set.seed(22)
  for(i in 1:5){
    
    fold_test <- train_data[folds[[i]],]   #取folds[[i]]作为测试集
    fold_train <- train_data[-folds[[i]],]   # 剩下的数据作为训练集
    
    x_train <- fold_train[,-1] 
    y_train <- fold_train[, 1]  
    x_test <- fold_test[, -1]
    y_test <- fold_test[, 1]  
    

    lgb.model <- lightgbm(data = as.matrix(x_train),
                           label = y_train,
                           num_leaves = bst_num_leaves,
                           learning_rate = bst_learning_rate,
                           max_depth = bst_max_depth,
                           early_stopping_rounds = 20,
                           objective = "regression",
                           metric ="rmse",
                           categorical_feature= categorical_feature)
  
    test_rs <- append(test_rs, get_rmse(lgb.model, x_test, y_test))
    train_rs <- append(train_rs, get_rmse(lgb.model, x_train, y_train))
    
    gc(verbose = FALSE)  
    
  }
  
  
  test_rs
  mean(test_rs)
  sd(test_rs)
  
  train_rs
  mean(train_rs)
  sd(train_rs)
  
#   return(mean(test_rs))
# }

### importance of variables 
tree_imp <- lgb.importance(lgb.model, percentage = TRUE)

tree_imp$Feature
View(tree_imp)
lgb.plot.importance(tree_imp,  measure = "Gain", 50)

write.csv(tree_imp, "v3_important_factors.csv")



########################## real run  #####################################
setwd("/Users/AA366716/2018_Kaggle/customer_revenue_predict")
train0 <- readRDS("traindata_ready_v3.rds")
test0 <- readRDS("testdata_ready_v3.rds")

train<- train0 %>% select(revenue_log, fullVisitorId, tree_imp$Feature)
test<- test0 %>% select(fullVisitorId, fullVisitorId, tree_imp$Feature)

###### パラメータ
bst_num_leaves<- 5
bst_learning_rate <-0.1
bst_max_depth <-4

fullVisitorIds <- test %>% select(fullVisitorId)
test <- test %>% select(-fullVisitorId)
train <- train %>% select(-fullVisitorId)


# eliminate unimportant factors to train and run model
train0 <- train %>% select(revenue_log, tree_imp$Feature)
test0 <- test %>% select(tree_imp$Feature)


categorical_feature <- train0 %>% select_if(is.character) %>% names(.)
train_data <- mutate_if(train0, is.character, as.factor) %>% mutate_if(., is.factor, as.numeric)
test_data <- mutate_if(test0, is.character, as.factor) %>% mutate_if(., is.factor, as.numeric)

x_train <- train_data[,-1] 
y_train <- train_data[, 1]  
x_test <- test_data

model <- lightgbm(data = as.matrix(x_train),
                      label = y_train,
                      num_leaves = bst_num_leaves,
                      learning_rate = bst_learning_rate,
                      max_depth = bst_max_depth,
                      early_stopping_rounds = 20,
                      objective = "regression",
                      metric ="rmse",
                      categorical_feature= categorical_feature)

pred <- predict(model, as.matrix(x_test)) %>% as.data.frame()
colnames(pred)<-"PredictedLogRevenue"

result <- data.frame(fullVisitorIds, pred) 

result0 <- result %>% 
  inner_join(., test0, by="fullVisitorId") %>% 
  mutate(PredictedLogRevenue= ifelse((visitNumber_max <=1 & visit_times<=1) | pageviews_max<=1| hits_max <=1|PredictedLogRevenue<0 , 0, PredictedLogRevenue))

range(result0$PredictedLogRevenue)
hist(result0$PredictedLogRevenue)
write.csv(result, "predict_only_importantFeature_v3.csv", row.names = FALSE)
nrow(result0)


# # +++++++++++++ modeling!!! ++++++++++++
# set.seed(123)
# lgb.train = lgb.Dataset(data=as.matrix(dtrain),label=trainLabel, categorical_feature =categorical_feature)
# lgb.valid = lgb.Dataset(data=as.matrix(dval),label=valLabel, categorical_feature =categorical_feature)
# 
# params <- list(objective="regression",
#                metric="rmse",
#                learning_rate=0.01)
# 
# lgb.model <- lgb.train(params = params,
#                        data = lgb.train,
#                        valids = list(val = lgb.valid),
#                        learning_rate=0.01,
#                        nrounds=1000,
#                        verbose=1,
#                        early_stopping_rounds=50,
#                        eval_freq=100
# )
# 
# lgb.model$best_iter
# lgb.model$best_score
# 
# 
# tree_imp <- lgb.importance(lgb.model, percentage = TRUE)
# lgb.plot.importance(tree_imp, top_n = 50, measure = "Gain")
# 
# 
# pred <- predict(lgb.model, as.matrix(dtest)) %>% 
#   as_tibble() %>% 
#   set_names("y") %>% 
#   mutate(y = expm1(y)) %>% 
#   mutate(y = ifelse(y < 0, 0, y))
# 
# pred <- cbind(pred, idPageBounce)
# pred <- pred %>%
#   mutate(y = ifelse((pageviews<7|bounces==1), 0, y))
# pred <- pred %>% select(-pageviews, -bounces) %>%
#   group_by(fullVisitorId) %>% 
#   summarise(y = log1p(sum(y)))
# 
# read_csv("../input/sample_submission.csv") %>%  
#   left_join(pred, by = "fullVisitorId") %>% 
#   mutate(PredictedLogRevenue = round(y, 5)) %>% 
#   select(-y) %>% 
#   write_csv(paste0("Lightgbm",round(lgb.model$best_score,5),".csv"))
# 
# 
# 



# train5_2 <- train4 %>% 
#   dplyr::select(fullVisitorId,channelGrouping, browser, operatingSystem, deviceCategory,continent, 
#                 subContinent, country, region, metro, city, networkDomain, campaign, 
#                 source, medium, keyword, referralPath,ad) %>% 
#   replace(is.na(.), "") %>% 
#   dplyr::group_by(fullVisitorId) %>% 
#   dplyr::summarise(
#     channelGrouping_all = paste(channelGrouping, collapse = ""),
#     browser_all = paste(browser, collapse = ""),
#     operatingSystem_all = paste(operatingSystem, collapse = ""),
#     deviceCategory_all = paste(deviceCategory, collapse = ""),
#     continent_all = paste(continent, collapse = ""),
#     subContinent_all = paste(subContinent, collapse = ""),
#     country_all = paste(country, collapse = ""),
#     region_all = paste(region, collapse = ""),
#     metro_all = paste(metro, collapse = ""),
#     city_all = paste(city, collapse = ""),
#     networkDomain_all = paste(networkDomain, collapse = ""),
#     campaign_all = paste(campaign, collapse = ""),
#     source_all = paste(source, collapse = ""),
#     medium_all = paste(medium, collapse = ""),
#     keyword_all = paste(keyword, collapse = ""),
#     referralPath_all = paste(referralPath, collapse = ""),
#     ad_all= paste(ad, collapse = ""))

# train5_2 <- gsub("\\s*", "", train5_2) 
# train5_2 <- gsub("_*", "", train5_2) 











       