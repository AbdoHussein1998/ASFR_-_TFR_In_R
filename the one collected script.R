setwd("D:/Colleage/Enviornmet/env2/1")
# activate liberaries
library(readxl)
library(ggplot2)
library(tidyr)
############ functions ############


adjusting_colnames <- function(df) { 
  # Merge the first 5 rows column-wise to form new column names
  merged_rows <- apply(X = df[1:5, ], MARGIN = 2, FUN = paste, collapse = ", ")
  
  # Adjust specific column names
  merged_rows[1] <- "country_id"
  merged_rows[2] <- "country_name"
  merged_rows[3] <- "year"
  
  # Rename the columns of the data frame
  names(df) <- merged_rows
  
  # Remove the first 5 rows used for column names
  df <- df[6:nrow(df), ]
  
  # Return the modified data frame
  return(df)
}


change_dtypes<- function(df){
  for (name in colnames(df)){
    if (name!="country_name"){
      df[[name]]=as.numeric(df[[name]])
    }
  }
  return(df)
}


extract_ages <- function(df) { # it takes the dataframe as an argument
  new_names <- c() # construct an empty vector
  for (i in names(df)) {
    m <- strsplit(i, ",") # to split each name 
    if (length(m[[1]]) == 1)  # to extract the age catagroy
    {
      
      new_names <- append(x = new_names, values = m[[1]], after = length(new_names))
      
    }else if (length(m[[1]]) > 1) 
    {
      trimmed <- trimws(m[[1]][3])
      new_names <- append(x = new_names, values = trimmed, after = length(new_names))
    }
  }
  return(new_names) # return vector after getting the required age group
}




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

  
  
  
  
  #### building the main datframes #### 
  
  
  # building the ASFR dataframe
  ASFR <- data.frame()
  # both data frame has the same diamensions and the same data types
  for (col_name in names(births_df)) {
    if (col_name != "country_id" && col_name != "country_name" && col_name != "year") {
      ve <- ((births_df[col_name] / women_df[col_name])*1000)
      if (length(ASFR) == 0) {
        ASFR <- data.frame(ve)
      } else {
        ASFR <- cbind(ASFR, ve)
      }
    } else {
      if (length(ASFR) == 0) {
        ASFR <- data.frame(births_df[col_name])
      } else {
        ASFR <- cbind(ASFR, births_df[col_name])
      }
    }
  }
  numeric_col=names(ASFR)[c(-1,-2,-3)]
  cata_col=names(ASFR)[c(1,2,3)]
  
  
  
  # building the TFR dataframe
  TFR_df <- c()
  for (country_id in ASFR$country_id)
  {
    country_name <- ASFR[ASFR$country_id == country_id, ][[cata_col[2]]]
    value <- sum(ASFR[ASFR$country_id == country_id, numeric_col])
    v <- c((value*5)/1000)
    names(v) <- country_name
    if (length(TFR_df) == 0) {
      TFR_df <- v
    } else {
      TFR_df <- append(TFR_df, v, after = length(TFR_df))
    }
  }
  TFR_df <- data.frame(TFR_df)
  names(TFR_df) <- "TFR"
  
  
  
  #### ploting ####
  
  graph_asfr=t(ASFR[,seq(4,10)])
  graph_asfr<-data.frame(graph_asfr)
  colnames(graph_asfr) <- ASFR$country_name
  graph_asfr[,"age"]<-factor(row.names(graph_asfr))
  graph_asfr[,"age"]<- as.numeric(graph_asfr$age)
  
  
  plot(x = graph_asfr$age,
       y = graph_asfr$Croatia,
       xlab = "age",
       ylab = "ASFR",
       xaxt= "n",
       col="blue",
       type="b",
       main="ASFR of 2024",
       lwd=3,
       ylim = c(0, 150))
  
  axis(side = 1,at = graph_asfr$age,labels = row.names(graph_asfr))
  lines(x = graph_asfr$age,y= graph_asfr$Cyprus,col="red",type="b",lwd=3)
  lines(x = graph_asfr$age,y= graph_asfr$Botswana,col="#858503",type="b",lwd=3)
  lines(x = graph_asfr$age,y= graph_asfr$Colombia,col="#630313",type="b",lwd=3)
  lines(x = graph_asfr$age,y= graph_asfr$Andorra,col="#6b09a8",type="b",lwd=3)
  
  legend("topright",
         legend=c("Croatia","Cyprus","Botswana","Colombia","Andorra"),
         col=c("blue","red","#858503","#630313","#6b09a8"),
         lty = 1,lwd=5)
  
  
  
  
  selected_country_c=c(12,14,5,9,2)
  country_value=TFR_df[selected_country_c,]
  country_name=labels(TFR_df)[[1]][selected_country_c]
  bar_graph=data.frame(row.names = country_name, country_value)
  
  barplot(bar_graph$country_value,xlab = "Country",
          ylab = "TFR",
          names.arg = row.names(bar_graph),
          col = c("blue","red","#858503","#630313","#6b09a8"),
          main = "TFR by country")
  
  barplot(bar_graph$country_value,ylab = "Country",
          xlab = "TFR",
          names.arg = row.names(bar_graph),
          col = c("blue","red","#858503","#630313","#6b09a8"),
          main = "TFR by country",horiz = T)
  
  
  
  
  