# this is a script for cleaning json in data for customer revenue prediction in Kaggle 
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
# df_test <- fread("test_v2.csv")
df_train <- fread("train_v2.csv", drop=c("hits","customDimensions"))

# ########### get clean dataset ########
##----- function define ----
# json flatten 
flatten_json <- . %>%  # simple way of writing function
  gsub('\"\"','\"',.) %>%
  str_c(., collapse = ",") %>%  # add rows together
  str_c("[", ., "]") %>%   # to make list
  fromJSON(flatten = T)


# clean json on dataframe function 
clean_json <- function(df){
  
  device <- bind_cols(flatten_json(df$device))
  trafficSource <- bind_cols(flatten_json(df$trafficSource))
  geoNetwork <- bind_cols(flatten_json(df$geoNetwork))
  totals <- bind_cols(flatten_json(df$totals))
  
  # the rest of the dataframe
  df_rest <- df %>% select(-device, -geoNetwork,-totals, -trafficSource)
  df_clean <- cbind(df_rest, device, geoNetwork,totals, trafficSource)
  
  return(df_clean)
}

# clean
train <- clean_json(df_train)
test <- clean_json(df_test)


######## clean trainv2 
# df <- df_train
# 
# device <- bind_cols(flatten_json(df$device))
# trafficSource <- bind_cols(flatten_json(df$trafficSource))
# geoNetwork <- bind_cols(flatten_json(df$geoNetwork))
# totals <- bind_cols(flatten_json(df$totals))
# 
# df_rest <- df %>% select(-device, -geoNetwork,-totals, -trafficSource)
# df_clean <- cbind(df_rest, device, geoNetwork,totals, trafficSource)


# # clean_df backup
saveRDS(train, file = "train_v2_clean.rds")
saveRDS(test, file = "test_v2_clean_with_CustomDimension&hits.rds")
df_test_n <- test %>% select(-hits, -customDimensions)
saveRDS(df_test_n, file = "test_v2_clean.rds")



################## read trainv2 ############## 
# df_train <- fread("train_v2.csv")
# ctypes <- cols(fullVisitorId = col_character(),
#                channelGrouping = col_character(),
#                date = col_datetime(),
#                device = col_character(),
#                geoNetwork = col_character(),
#                socialEngagementType = col_character(),# should not skip this one?
#                totals = col_character(),
#                trafficSource = col_character(),
#                visitId = col_integer(),
#                visitNumber = col_integer(),
#                visitStartTime = col_integer(),
#                hits = col_skip(),
#                customDimensions = col_skip())
# df_train <- fread('unzip -q train_v2.csv.zip')
# df_train <- read_csv("train_v2.csv", colClasses  = ctypes)

#----------------------------------
# check the data before and after 
# df_test <- readRDS(file = "test_v2_clean.rds")
# test_raw <- fread("test_clean.csv")
# dim(test_raw)
# table(names(df_test) %in% names(test_raw))
# setdiff(names(df_test), names(test_raw))
# setdiff(names(test_raw), names(df_test))
# 

################################
#------- some tryouts for reading train_v2 -----
# library("ff")
# df_train<- read.csv.ffdf(file="train_v2.csv", header=TRUE, first.rows=10000, next.rows=50000)
#--------
# library(sparklyr)
# library(tidyverse)
# spark_dir = "/my_2_to_disk/spark/"
# config = spark_config()
# config$`sparklyr.shell.driver-java-options` <-  paste0("-Djava.io.tmpdir=", spark_dir)
# config$`sparklyr.shell.driver-memory` <- "4G"
# config$`sparklyr.shell.executor-memory` <- "4G"
# config$`spark.yarn.executor.memoryOverhead` <- "512"
# 
# 
# sc = spark_connect(master = "local", config = config)
# air = spark_read_csv(sc, name = "air", path = "train_v2.csv")

# #------
# library("sqldf")
# write.csv(iris, "iris.csv", quote = FALSE, row.names = FALSE)
# iris2 <- read.csv.sql("iris.csv",sql = "select * from file LIMIT 5")
# 
# df_test <- read.csv.sql("test_v2.csv", sql = "select * from file LIMIT 5")
