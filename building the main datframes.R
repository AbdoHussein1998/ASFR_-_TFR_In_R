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
