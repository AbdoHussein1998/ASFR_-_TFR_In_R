setwd("D:/Colleage/Enviornmet/env2/1")
# activate liberaries
library(readxl)
library(ggplot2)
library(tidyr)

# read the dataframes
women_df<- read_excel("women.xlsx",sheet = "Data",col_names = F)
births_df<-read_excel("births.xls",sheet = "Data",col_names = F)

#cleaning the dataframe

  women_df=adjusting_colnames(women_df)
  births_df=adjusting_colnames(births_df)
  
  names(births_df)<-extract_ages(births_df)
  names(women_df)<-extract_ages(women_df)
  
  women_df<-change_dtypes(women_df)
  births_df<-change_dtypes(births_df)

  