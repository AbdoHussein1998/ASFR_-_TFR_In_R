#### functions ####


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
