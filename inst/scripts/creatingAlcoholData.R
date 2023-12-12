# Script to bring the YRBS data
# Catalina Canizares
# December 11 2023


# Focus of Plots: Alcohol Use Among Adolescents
# This section involves selecting variables specifically related to alcohol consumption
# from the three datasets. Following the selection, these variables will be combined
# into a unified dataset for comprehensive analysis and visualization.


library(dissertationData)
library(tidyverse)


data("raw2017")
data("raw2019")
data("raw2021")


## 2017

alcohol2017 <-
  raw2017 |>
  tidyREDCap::drop_labels() |>
  mutate(
    Age = case_when(
      Q1 == 1 ~ 12L,
      Q1 == 2 ~ 13L,
      Q1 == 3 ~ 14L,
      Q1 == 4 ~ 15L,
      Q1 == 5 ~ 16L,
      Q1 == 6 ~ 17L,
      Q1 == 7 ~ 18L,
      TRUE ~ NA_integer_
    )
  ) |>
  mutate(
    Sex = case_when(
      Q2 == 2 ~ "Male",
      Q2 == 1 ~ "Female",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    Grade = case_when(
      Q3 == 1 ~ "9",
      Q3 == 2 ~ "10",
      Q3 == 3 ~ "11",
      Q3 == 4 ~ "12",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    Race = case_when(
      raceeth == 1 ~ "Am Indian/Alaska Native",
      raceeth == 2 ~ "Asian",
      raceeth == 3 ~ "Black or African American",
      raceeth == 4 ~ "Native Hawaiian/Other PI",
      raceeth == 5 ~ "White",
      raceeth == 6 ~ "Hispanic/Latino",
      raceeth == 7 ~ "Multiple-Hispanic",
      raceeth == 8 ~ "Multiple-Non-Hispanic",
      TRUE ~ NA_character_
    )
  ) |>
  # Item number changed, in 2019 and 2021 SexOrientation is Q66
  mutate(
    SexOrientation = case_when(
      Q67 == 1 ~ "Heterosexual",
      Q67 == 2 ~ "Gay or Lesbian",
      Q67 == 3 ~ "Bisexual",
      Q67 == 4 ~ "Not sure",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(Year = "2017") |>
  select(Year, Age, Sex, Grade, Race, SexOrientation,
         TimesPhysicalFight = Q17,
         AgeFirstAlcohol = Q41,
         HowManyDaysAlcoholInMonth = Q42,
         SourceAlcohol = Q43,
         DaysOfBingeDrinking = Q44,
         LargestNumberOfDrinks = Q45) |>
  # Assumed the largest number in the range
  mutate(TimesPhysicalFight = case_when(
    TimesPhysicalFight == 1 ~ 0,
    TimesPhysicalFight == 2 ~ 1,
    TimesPhysicalFight == 3 ~ 3,
    TimesPhysicalFight == 4 ~ 5,
    TimesPhysicalFight == 5 ~ 7,
    TimesPhysicalFight == 6 ~ 9,
    TimesPhysicalFight == 7 ~ 11,
    TimesPhysicalFight == 8 ~ 12,
    TRUE ~ NA
  )) |>
  # Assumed the youngest age from the range
  mutate(AgeFirstAlcohol  = case_when(
    AgeFirstAlcohol  == 1 ~ 0,
    AgeFirstAlcohol  == 2 ~ 8,
    AgeFirstAlcohol  == 3 ~ 9,
    AgeFirstAlcohol  == 4 ~ 11,
    AgeFirstAlcohol  == 5 ~ 13,
    AgeFirstAlcohol  == 6 ~ 15,
    AgeFirstAlcohol  == 7 ~ 17,
    TRUE ~ NA
  )) |>
  # Assumed the largest number in the range
  mutate(HowManyDaysAlcoholInMonth   = case_when(
    HowManyDaysAlcoholInMonth   == 1 ~ 0,
    HowManyDaysAlcoholInMonth   == 2 ~ 2,
    HowManyDaysAlcoholInMonth   == 3 ~ 5,
    HowManyDaysAlcoholInMonth   == 4 ~ 9,
    HowManyDaysAlcoholInMonth  == 5 ~ 19,
    HowManyDaysAlcoholInMonth  == 6 ~ 29,
    HowManyDaysAlcoholInMonth  == 7 ~ 30,
    TRUE ~ NA
  )) |>
  # Assumed the largest number in the range
  mutate(DaysOfBingeDrinking  = case_when(
    DaysOfBingeDrinking == 1 ~ 0,
    DaysOfBingeDrinking == 2 ~ 1,
    DaysOfBingeDrinking == 3 ~ 2,
    DaysOfBingeDrinking == 4 ~ 5,
    DaysOfBingeDrinking == 5 ~ 9,
    DaysOfBingeDrinking == 6 ~ 19,
    DaysOfBingeDrinking == 7 ~ 20,
    TRUE ~ NA
  )) |>
  mutate(LargestNumberOfDrinks  = case_when(
    LargestNumberOfDrinks == 1 ~ 0,
    LargestNumberOfDrinks == 2 ~ 2,
    LargestNumberOfDrinks == 3 ~ 3,
    LargestNumberOfDrinks == 4 ~ 4,
    LargestNumberOfDrinks == 5 ~ 5,
    LargestNumberOfDrinks == 6 ~ 7,
    LargestNumberOfDrinks == 7 ~ 9,
    LargestNumberOfDrinks == 8 ~ 10,
    TRUE ~ NA
  )) |>
  filter(AgeFirstAlcohol != 0) |>
  filter(HowManyDaysAlcoholInMonth != 0) |>
  filter(DaysOfBingeDrinking != 0) |>
  filter(LargestNumberOfDrinks != 0)


## 2019

alcohol2019 <-
  raw2019 |>
  tidyREDCap::drop_labels() |>
  mutate(
    Age = case_when(
      Q1 == 1 ~ 12L,
      Q1 == 2 ~ 13L,
      Q1 == 3 ~ 14L,
      Q1 == 4 ~ 15L,
      Q1 == 5 ~ 16L,
      Q1 == 6 ~ 17L,
      Q1 == 7 ~ 18L,
      TRUE ~ NA_integer_
    )
  ) |>
  mutate(
    Sex = case_when(
      Q2 == 2 ~ "Male",
      Q2 == 1 ~ "Female",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    Grade = case_when(
      Q3 == 1 ~ "9",
      Q3 == 2 ~ "10",
      Q3 == 3 ~ "11",
      Q3 == 4 ~ "12",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    Race = case_when(
      raceeth == 1 ~ "Am Indian/Alaska Native",
      raceeth == 2 ~ "Asian",
      raceeth == 3 ~ "Black or African American",
      raceeth == 4 ~ "Native Hawaiian/Other PI",
      raceeth == 5 ~ "White",
      raceeth == 6 ~ "Hispanic/Latino",
      raceeth == 7 ~ "Multiple-Hispanic",
      raceeth == 8 ~ "Multiple-Non-Hispanic",
      TRUE ~ NA_character_
    )
  ) |>
  # Item number changed, in 2019 and 2021 SexOrientation is Q66
  mutate(
    SexOrientation = case_when(
      Q66 == 1 ~ "Heterosexual",
      Q66 == 2 ~ "Gay or Lesbian",
      Q66 == 3 ~ "Bisexual",
      Q66 == 4 ~ "Not sure",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(Year = "2019") |>
  select(Year, Age, Sex, Grade, Race, SexOrientation,
         TimesPhysicalFight = Q17,
         AgeFirstAlcohol = Q40,
         HowManyDaysAlcoholInMonth = Q41,
         SourceAlcohol = Q44,
         DaysOfBingeDrinking = Q42,
         LargestNumberOfDrinks = Q43) |>
  # Assumed the largest number in the range
  mutate(TimesPhysicalFight = case_when(
    TimesPhysicalFight == 1 ~ 0,
    TimesPhysicalFight == 2 ~ 1,
    TimesPhysicalFight == 3 ~ 3,
    TimesPhysicalFight == 4 ~ 5,
    TimesPhysicalFight == 5 ~ 7,
    TimesPhysicalFight == 6 ~ 9,
    TimesPhysicalFight == 7 ~ 11,
    TimesPhysicalFight == 8 ~ 12,
    TRUE ~ NA
  )) |>
  # Assumed the youngest age from the range
  mutate(AgeFirstAlcohol  = case_when(
    AgeFirstAlcohol  == 1 ~ 0,
    AgeFirstAlcohol  == 2 ~ 8,
    AgeFirstAlcohol  == 3 ~ 9,
    AgeFirstAlcohol  == 4 ~ 11,
    AgeFirstAlcohol  == 5 ~ 13,
    AgeFirstAlcohol  == 6 ~ 15,
    AgeFirstAlcohol  == 7 ~ 17,
    TRUE ~ NA
  )) |>
  # Assumed the largest number in the range
  mutate(HowManyDaysAlcoholInMonth   = case_when(
    HowManyDaysAlcoholInMonth   == 1 ~ 0,
    HowManyDaysAlcoholInMonth   == 2 ~ 2,
    HowManyDaysAlcoholInMonth   == 3 ~ 5,
    HowManyDaysAlcoholInMonth   == 4 ~ 9,
    HowManyDaysAlcoholInMonth  == 5 ~ 19,
    HowManyDaysAlcoholInMonth  == 6 ~ 29,
    HowManyDaysAlcoholInMonth  == 7 ~ 30,
    TRUE ~ NA
  )) |>
  # Assumed the largest number in the range
  mutate(DaysOfBingeDrinking  = case_when(
    DaysOfBingeDrinking == 1 ~ 0,
    DaysOfBingeDrinking == 2 ~ 1,
    DaysOfBingeDrinking == 3 ~ 2,
    DaysOfBingeDrinking == 4 ~ 5,
    DaysOfBingeDrinking == 5 ~ 9,
    DaysOfBingeDrinking == 6 ~ 19,
    DaysOfBingeDrinking == 7 ~ 20,
    TRUE ~ NA
  )) |>
  mutate(LargestNumberOfDrinks  = case_when(
    LargestNumberOfDrinks == 1 ~ 0,
    LargestNumberOfDrinks == 2 ~ 2,
    LargestNumberOfDrinks == 3 ~ 3,
    LargestNumberOfDrinks == 4 ~ 4,
    LargestNumberOfDrinks == 5 ~ 5,
    LargestNumberOfDrinks == 6 ~ 7,
    LargestNumberOfDrinks == 7 ~ 9,
    LargestNumberOfDrinks == 8 ~ 10,
    TRUE ~ NA
  )) |>
  filter(AgeFirstAlcohol != 0) |>
  filter(HowManyDaysAlcoholInMonth != 0) |>
  filter(DaysOfBingeDrinking != 0) |>
  filter(LargestNumberOfDrinks != 0)


## 2021

alcohol2021 <-
  raw2021 |>
  tidyREDCap::drop_labels() |>
  mutate(
    Age = case_when(
      Q1 == 1 ~ 12L,
      Q1 == 2 ~ 13L,
      Q1 == 3 ~ 14L,
      Q1 == 4 ~ 15L,
      Q1 == 5 ~ 16L,
      Q1 == 6 ~ 17L,
      Q1 == 7 ~ 18L,
      TRUE ~ NA_integer_
    )
  ) |>
  mutate(
    Sex = case_when(
      Q2 == 2 ~ "Male",
      Q2 == 1 ~ "Female",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    Grade = case_when(
      Q3 == 1 ~ "9",
      Q3 == 2 ~ "10",
      Q3 == 3 ~ "11",
      Q3 == 4 ~ "12",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    Race = case_when(
      raceeth == 1 ~ "Am Indian/Alaska Native",
      raceeth == 2 ~ "Asian",
      raceeth == 3 ~ "Black or African American",
      raceeth == 4 ~ "Native Hawaiian/Other PI",
      raceeth == 5 ~ "White",
      raceeth == 6 ~ "Hispanic/Latino",
      raceeth == 7 ~ "Multiple-Hispanic",
      raceeth == 8 ~ "Multiple-Non-Hispanic",
      TRUE ~ NA_character_
    )
  ) |>
  # Item number changed, in 2019 and 2021 SexOrientation is Q66
  mutate(
    SexOrientation = case_when(
      Q66 == 1 ~ "Heterosexual",
      Q66 == 2 ~ "Gay or Lesbian",
      Q66 == 3 ~ "Bisexual",
      Q66 == 4 ~ "Not sure",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(Year = "2021") |>
  select(Year, Age, Sex, Grade, Race, SexOrientation,
         TimesPhysicalFight = Q16,
         AgeFirstAlcohol = Q40,
         HowManyDaysAlcoholInMonth = Q41,
         SourceAlcohol = Q44,
         DaysOfBingeDrinking = Q42,
         LargestNumberOfDrinks = Q43) |>
  # Assumed the largest number in the range
  mutate(TimesPhysicalFight = case_when(
    TimesPhysicalFight == 1 ~ 0,
    TimesPhysicalFight == 2 ~ 1,
    TimesPhysicalFight == 3 ~ 3,
    TimesPhysicalFight == 4 ~ 5,
    TimesPhysicalFight == 5 ~ 7,
    TimesPhysicalFight == 6 ~ 9,
    TimesPhysicalFight == 7 ~ 11,
    TimesPhysicalFight == 8 ~ 12,
    TRUE ~ NA
  )) |>
  # Assumed the youngest age from the range
  mutate(AgeFirstAlcohol  = case_when(
    AgeFirstAlcohol  == 1 ~ 0,
    AgeFirstAlcohol  == 2 ~ 8,
    AgeFirstAlcohol  == 3 ~ 9,
    AgeFirstAlcohol  == 4 ~ 11,
    AgeFirstAlcohol  == 5 ~ 13,
    AgeFirstAlcohol  == 6 ~ 15,
    AgeFirstAlcohol  == 7 ~ 17,
    TRUE ~ NA
  )) |>
  # Assumed the largest number in the range
  mutate(HowManyDaysAlcoholInMonth   = case_when(
    HowManyDaysAlcoholInMonth   == 1 ~ 0,
    HowManyDaysAlcoholInMonth   == 2 ~ 2,
    HowManyDaysAlcoholInMonth   == 3 ~ 5,
    HowManyDaysAlcoholInMonth   == 4 ~ 9,
    HowManyDaysAlcoholInMonth  == 5 ~ 19,
    HowManyDaysAlcoholInMonth  == 6 ~ 29,
    HowManyDaysAlcoholInMonth  == 7 ~ 30,
    TRUE ~ NA
  )) |>
  # Assumed the largest number in the range
  mutate(DaysOfBingeDrinking  = case_when(
    DaysOfBingeDrinking == 1 ~ 0,
    DaysOfBingeDrinking == 2 ~ 1,
    DaysOfBingeDrinking == 3 ~ 2,
    DaysOfBingeDrinking == 4 ~ 5,
    DaysOfBingeDrinking == 5 ~ 9,
    DaysOfBingeDrinking == 6 ~ 19,
    DaysOfBingeDrinking == 7 ~ 20,
    TRUE ~ NA
  )) |>
  mutate(LargestNumberOfDrinks  = case_when(
    LargestNumberOfDrinks == 1 ~ 0,
    LargestNumberOfDrinks == 2 ~ 2,
    LargestNumberOfDrinks == 3 ~ 3,
    LargestNumberOfDrinks == 4 ~ 4,
    LargestNumberOfDrinks == 5 ~ 5,
    LargestNumberOfDrinks == 6 ~ 7,
    LargestNumberOfDrinks == 7 ~ 9,
    LargestNumberOfDrinks == 8 ~ 10,
    TRUE ~ NA
  )) |>
  filter(AgeFirstAlcohol != 0) |>
  filter(HowManyDaysAlcoholInMonth != 0) |>
  filter(DaysOfBingeDrinking != 0) |>
  filter(LargestNumberOfDrinks != 0)


## Complete data set

youthAlcoholUse <-
  bind_rows(alcohol2017, alcohol2019, alcohol2021) |>
  mutate(across(c(Sex, Grade, Race, SexOrientation, SourceAlcohol),  as.factor))


usethis::use_data(youthAlcoholUse, overwrite = TRUE)
