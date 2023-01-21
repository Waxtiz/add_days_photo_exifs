library(dplyr)
library(fs)
library(lubridate)
library(exifr)

date_for_exif <- function(date) {
  year <- year(date)
  month <- month(date)
  day <- day(date)

  hour <- hour(date)
  minute <- minute(date)
  second <- second(date)

  day <- ifelse(day < 10, paste0("0", day), day)
  month <- ifelse(month < 10, paste0("0", month), month)
  hour <- ifelse(hour < 10, paste0("0", hour), hour)
  minute <- ifelse(minute < 10, paste0("0", minute), minute)
  second <- ifelse(second < 10, paste0("0", second), second)

  paste0(year, ":", month, ":", day, " ", hour, ":", minute, ":", second)
}

p1 <- "C:/Path/to/folder"


pictures_path <- fs::dir_ls(p1,
                       recurse = TRUE,
                       regexp = ".jpg$|.JPG$")

original_exif <- read_exif(path = pictures_path,
                           tags = c("ModifyDate", "FileName", "Directory"))

original_time <- original_exif %>%
  arrange(ModifyDate) %>%
  pull(ModifyDate) %>%
  parse_date_time("Ymd HMS",
                  tz = "Europe/Paris")

first_old_date <- original_time[1]

real_date <- lubridate::dmy_hms("08/01/2023-16:14:00", tz = "Europe/Paris")
days_diff <- real_date - first_old_date

for (i in seq_len(nrow(original_exif))) {

  old_date <- parse_date_time(original_exif[i, 2], "Ymd HMS", tz = "Europe/Paris")

  correct_time <- old_date + days_diff
  char_correct_date <- date_for_exif(correct_time)

  exiftool_call(paste0('-ModifyDate="', char_correct_date, '"'), original_exif[i, 1], quiet = TRUE)
}