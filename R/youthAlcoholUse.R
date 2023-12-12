#' @title youthAlcoholUse
#'
#' @docType data
#'
#' @usage data(youthAlcoholUse)
#'
#' @format A tibble with 12 rows and 12 variables:
#'
#' \itemize{
#' \item{Year:} {Survey year}
#' \item{Age:} {Pariticpant age, numeric}
#' \item{Sex:} {Participant sex assigned at birth, factor}
#' \item{Grade:} {Participant Grade, factor}
#' \item{Race:} {Participant Race/Ethnicity, factor}
#' \item{SexOrientation:} {Participant sex orientation, factor}
#' \item{TimesPhysicalFight;} {Times an adolescent reported physical fights, numeric}
#' \item{AgeFirstAlcohol:} {Age first alcoholic drink, numeric}
#' \item{HowManyDaysAlcoholInMonth:} {Druing the past 30 days, how many days did the participant have at least one drink, numeric}
#' \item{SourceAlcohol:} {How did the participant get the alcohol, numeric}
#' \item{DaysOfBingeDrinking:} {Druing the past 30 days, how many days did the participant have at least 4 drinks, numeric}
#' \item{LargestNumberOfDrinks:} {During the past 30 days, what is the larget number of drinks the participant had in a row}
#' }
#'
#' @description
#' This dataset consolidates data from the 2017, 2019, and 2021 Youth Risk
#' Behavior Surveys (YRBS) focusing specifically on instances of alcohol
#' use and physical altercations among adolescents. It is important to note
#' that this compilation includes only those cases where adolescents reported
# 'having consumed alcohol; instances where individuals reported abstaining
#' from alcohol are excluded. The primary objective of this dataset is to
#' facilitate the creation of visual representations; hence, survey weights
#' have been omitted for simplification. Users should be aware that due to
#' the removal of these weights and the selective nature of the data, this
#' dataset is not suitable for conducting inferential statistical analyses
#'
"youthAlcoholUse"
