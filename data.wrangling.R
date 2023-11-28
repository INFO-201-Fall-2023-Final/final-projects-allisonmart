library(dplyr)
library(stringr)
library(ggplot2)

college_df <- read.csv("college.enrollment.csv")

economics_df <- read.csv("state.economics.csv")

df <- merge(economics_df, college_df, by = c("State", "Year"))

college_df$State <- trimws(college_df$State)
economics_df$State <- trimws(economics_df$State)

df <- merge(economics_df, college_df, by = c("State", "Year"))

# Categorical Variable: whether or not the state's average household income was above or below the average of 
# all of the states household income for that particular year 
clean_and_convert <- function(x) {
  cleaned_values <- gsub("[^0-9]", "", gsub("\\(X\\)", "", x))
  numeric_values <- as.numeric(cleaned_values)
  return(numeric_values)
}
df$Mean.household.income..dollars. <- clean_and_convert(df$Mean.household.income..dollars.)

df_2010 <- filter(df, Year == 2010, Label == "Estimate")
mean_income_2010 <- mean(df_2010$Mean.household.income..dollars.)

df_2014 <- filter(df, Year == 2014, Label == "Estimate")
mean_income_2014 <- mean(df_2014$Mean.household.income..dollars.)

df_2018 <- filter(df, Year == 2018, Label == "Estimate")
mean_income_2018 <- mean(df_2018$Mean.household.income..dollars.)

df$Above.Or.Below.US.Mean.Income <- ""
for (i in 1:nrow(df)) {
  if (is.na(df$Mean.household.income..dollars.[i])) {
    df$Above.Or.Below.US.Mean.Income[i] <- ""
  } else if (df$Year[i] == 2010) {
    if (df$Mean.household.income..dollars.[i] >= mean_income_2010) {
      df$Above.Or.Below.US.Mean.Income[i] <- "Above"
    } else {
      df$Above.Or.Below.US.Mean.Income[i] <- "Below"
    }
  } else if (df$Year[i] == 2014) {
    if (df$Mean.household.income..dollars.[i] >= mean_income_2014) {
      df$Above.Or.Below.US.Mean.Income[i] <- "Above"
    } else {
      df$Above.Or.Below.US.Mean.Income[i] <- "Below"
    }
  } else if (df$Year[i] == 2018) {
    if (df$Mean.household.income..dollars.[i] >= mean_income_2018) {
      df$Above.Or.Below.US.Mean.Income[i] <- "Above"
    } else {
      df$Above.Or.Below.US.Mean.Income[i] <- "Below"
    }
  }
}


# Continuous/numerical variable:
df$Enrollment <- clean_and_convert(df$Enrollment)
df_fix <- df[(df["Label"] == "Estimate"), ]
grouped_df <- group_by(df_fix, Year)
grouped_df$total_enrollment <- 0
grouped_df <- summarize(grouped_df, total_enrollment = sum(Enrollment))
df <- merge(df, grouped_df, by = "Year", all.x = TRUE)


# Summary of the college / post-secondary enrollment and unemployment rates in only the southern states in 2014
clean_and_convert_percent <- function(x) {
  numeric_values <- as.numeric(ifelse(grepl("%", x), gsub("%", "", x), NA))
  return(numeric_values)
}
df$Unemployment.Rate <- clean_and_convert_percent(df$Unemployment.Rate)

filtered_df <- df %>%
  filter(Year == 2014, State %in% c("Alabama", "Arkansas", "Florida", "Georgia", "Kentucky", 
                                    "Louisiana", "Mississippi", "Missouri", "North Carolina", "South Carolina", 
                                    "Tennessee", "Texas", "Virginia", "West Virginia"))
summarized_df <- filtered_df %>%
  mutate(Enrollment = as.numeric(Enrollment)) %>%
  group_by(State) %>%
  summarize(
    Total_College_Enrollment = sum(Enrollment, na.rm = TRUE),
    Mean_Unemployment_Rate = mean(Unemployment.Rate, na.rm = TRUE)
  )

write.csv(df, "df.csv")

