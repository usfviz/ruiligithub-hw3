rm(list = ls())
setwd("~/Google-Drive/0.USF/5.2Visualization/hw/hw3")

if(!require(ggplot2))install.packages("ggplot2",repos="http://cran.rstudio.com/")
if(!require(tidyr))install.packages("tidyr",repos="http://cran.rstudio.com/")
if(!require(plyr))install.packages("plyr",repos="http://cran.rstudio.com/")
if(!require(shiny))install.packages("shiny",repos="http://cran.rstudio.com/")
if(!require(reshape2))install.packages("reshape2",repos="http://cran.rstudio.com/")
if(!require(lubridate))install.packages("lubridate",repos="http://cran.rstudio.com/")
if(!require(GGally))install.packages("GGally",repos="http://cran.rstudio.com/")

library(GGally)
library(ggvis)
library(ggplot2)
library(tidyr)
library(plyr)
library(shiny)
library(lubridate)
library(reshape2)


process_df <- function(df){
  
  # drop na
  df <- na.omit(df)  
  # make time to oridnal factors
  df$Post.Month <- factor(df$Post.Month, 
                          levels = 1:12, 
                          labels = month.name, 
                          ordered = T)
  df$Post.Weekday <- factor(df$Post.Weekday, 
                            levels = 1:7,
                            labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday", "Sunday"), 
                            ordered = T)
  df$Post.Hour <- factor(df$Post.Hour, 
                         levels = 1:23, 
                         labels = 1:23, 
                         ordered = T) 
  
  # make other categorical variable to nominal factors
  df$Type <- factor(df$Type)
  df$Category <- factor(df$Category)
  df$Paid <- factor(df$Paid)
  # cant do all together
  # df[c('Type', 'Category', 'Paid')] <- apply(df[c('Type', 'Category', 'Paid')],2, factor)
  return(df)
}

fb <- read.table('dataset_Facebook.csv',sep=';',header = TRUE, stringsAsFactors = FALSE)
fb <- process_df(fb)

factor_names <- names(sapply(fb, is.factor))[sapply(fb, is.factor)]
numeric_names <- names(sapply(fb, is.numeric))[sapply(fb, is.numeric)]
nominal_names <- c("Type", "Category", "Paid")
ordinal_variables <- c("Post.Month", "Post.Weekday", "Post.Hour")