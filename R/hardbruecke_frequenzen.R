## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----libraries and folder settings, echo=FALSE------------------------------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)
library(flexdashboard)
library(leaflet)

# settings -------------
data_folder <- "D:/mygithub/vbz_interview/data"


## ----data reading from files, eval=TRUE, echo=FALSE-------------------------------------------------------------------------------------
# read data -------------
years <- 2020:2024

# read all years, data downloaded from 
# <https://data.stadt-zuerich.ch/dataset/vbz_frequenzen_hardbruecke>
freq_df <- map_dfr(years, 
                   ~ read_csv(file.path(data_folder, 
                                        paste0("frequenzen_hardbruecke_", 
                                               .x, ".csv")), 
                              show_col_types = FALSE, progress = FALSE)) %>%   
  # add some rows: add a sum of in and out
  mutate(in_out = In + Out,
         # remove "total" in Name
         Name = str_replace(Name, "(?i) total", ""),
         # add column before and after pandemic
         after = if_else(Timestamp >= ymd("2022-04-01"), TRUE, FALSE),
         # add weekday and year
         weekday = wday(Timestamp, 
                        week_start = getOption("lubridate.week.start", 1)),
         year = year(Timestamp)) %>% 
  # split Name in direction (Ost, West) and location (Süd, Nord, SBB, VBZ)
  separate_wider_delim(Name, delim = "-", names = c("direction", "location"))

# save data as rds for faster reading
saveRDS(freq_df, file.path(data_folder, "freq_df.rds"))


## ----data reading from rds, eval=FALSE, echo=FALSE--------------------------------------------------------------------------------------
## # load data faster from rds file
## freq_df <- readRDS(file.path(data_folder, "freq_df.rds"))


## ----data and colour settings, echo=FALSE-----------------------------------------------------------------------------------------------
# just analyse workdays here
freq_df <- freq_df %>% filter(weekday %in% 1:5)

# get date from last entry
last_entry_date <- freq_df %>% arrange(Timestamp) %>% last() %>% 
  pull(Timestamp) %>% as_date()

# german labels for workdays
workday_labels <- c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag")

## define colours for 2024 and 2023
# get number of years to plot value boxes in same colour as bars
nyears <- freq_df %>% pull(year) %>% unique() %>% length()
# create as many colours as having unique years using 
#   khroma medium conrast color scale
mediumcontrast <- khroma::color("medium contrast")
mediumcontrast_colours <- mediumcontrast(nyears)
# take the last and second last colour for 2023 and 2024, respectively
colour_2023 <- mediumcontrast_colours %>% nth(-2)
colour_2024 <- mediumcontrast_colours %>% nth(-1)


## ----function definition, echo=FALSE----------------------------------------------------------------------------------------------------
# create two frequency plots, one with absolute and one with relative numbers
freq_plots <- function(freq_df) {
  
  # get average frequencies per day for each weekday and year
  daily_freq_yearly <- freq_df %>% 
    # calculate mean frequency for each 5 min entry, separately for each weekday
      # year, location and direction
    group_by(weekday, year, location, direction) %>% 
    # multiply by 288 = 24 * 60 / 5 to get estimated daily values
    summarise(freq = mean(in_out) * 288) %>% 
    # get estimated daily sums for each weekday and year
    group_by(weekday, year) %>% summarise(freq = sum(freq)) %>% 
    # get relative values in percentage for each year separately 
    group_by(year) %>% mutate(perc = freq/sum(freq)*100) %>% ungroup() %>% 
    # convert year and weekday to factors for discrete plotting options
    mutate(year = factor(year), weekday = factor(weekday)) 
  
  # analyse each year separately
  gg_yearly <- ggplot(daily_freq_yearly, 
                      aes(x = weekday, y = freq, fill = year)) + 
    # each year side-by-side bar graph
    geom_col(position = position_dodge()) +
    # use German workday labels on x-axis
    scale_x_discrete(breaks = 1:5, labels = workday_labels) +
    # use meaningful colour scale
    khroma::scale_fill_mediumcontrast() +
    ylab("Fahrgastzahlen pro Wochentag") +
    theme_bw() +
    # no x-axis title and no legend title
    theme(axis.title.x = element_blank(),
          legend.title = element_blank())
  
  # analyse each year separately in relative numbers
  gg_yearly_perc <- ggplot(daily_freq_yearly, 
                           aes(x = weekday, y = perc, fill = year)) +
    # each year side-by-side bar graph
    geom_col(position = position_dodge()) +
    # use German workday labels on x-axis
    scale_x_discrete(breaks = 1:5, labels = workday_labels) +
    # use meaningful colour scale
    khroma::scale_fill_mediumcontrast() +
    ylab("Prozent") +
    theme_bw() +  
    # no x-axis title and no legend title
    theme(axis.title.x = element_blank(), 
          legend.title = element_blank())
  
  # return the absolute and relative plot in a list
  return(list(abs = gg_yearly, rel = gg_yearly_perc))
}

# percentage increase between two years
calc_increase <- function(freq_df, start_year, end_year) {
  
  mean_years <- freq_df %>% 
    # filter only entries from the start and the end year
    filter(year %in% c(start_year, end_year)) %>%
    # calculate the mean 5-min frequency per year
    group_by(year) %>% summarise(mean = mean(in_out)) %>%
    # take a vector with 2 entries
    pull(mean)
  
  # calculate the increase in percentage and round it to one digit after comma
  incr_years <- (diff(mean_years)/sum(mean_years) * 100) %>% 
    round(digits = 1)
  
  return(incr_years)
}


## ----leaflet, echo=FALSE----------------------------------------------------------------------------------------------------------------
# add a laeflet with the coordinates of the hardbruecke as first page and a 
  # popup window showing the image from 
  # <https://data.stadt-zuerich.ch/dataset/vbz_frequenzen_hardbruecke>
leaflet() %>%
  addTiles() %>% 
  addMarkers(lng = 8.51718, lat = 47.38517, group = "pnt") %>% 
  leafpop::addPopupImages(file.path(data_folder, "Hardbruecke.PNG"), 
                                    group = "pnt", width = 1000, heigth = 600)


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------
## absolute and relative plots for vbz for direction "Ost" since one can read:
  # "Zählwerte ab 1.1.2023 auf dem Perron "West" sind nicht korrekt. Bitte 
  # verwenden Sie die absoluten Werte derzeit nicht, bis das Problem behoben 
  # ist."

# filter data set to VBZ and Ost only
freq_vbz_east <- freq_df %>% filter(location == "VBZ", direction == "Ost")
# create absolute and relative bar graphs
vbz_east <- freq_plots(freq_vbz_east)
# render absolute plot with specific title
vbz_east$abs + ggtitle("Frequenzen VBZ Ost")


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------
valueBox("Di, Mi, Do", caption = "Frequenzstärkste Wochentage", 
         icon = "fa-solid fa-calendar-days")


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------
valueBox(paste0("+", calc_increase(freq_vbz_east, 2020, 2023), " %"), 
         caption = "Anstieg 2023 gegenüber 2020", 
         color = colour_2023, 
         icon = "fa-solid fa-arrow-trend-up")


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------
valueBox(paste0("+", calc_increase(freq_vbz_east, 2020, 2024), " %"), 
         caption = paste0("Anstieg 2024 gegenüber 2020 (Daten nur bis ", 
                          last_entry_date,")"), 
         color = colour_2024, 
         icon = "fa-solid fa-triangle-exclamation")


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------
vbz_east$rel + ggtitle("Relative Frequenzen VBZ Ost")


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------
valueBox("Mo + Fr", 
         caption = "Relativer Rückgang", 
         icon = "fa-solid fa-calendar-days")


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------
freq_all_loc_east <- freq_df %>% filter(direction == "Ost")
all_loc_east <- freq_plots(freq_all_loc_east)
all_loc_east$abs + ggtitle("Frequenzen alle Messorte Ost")


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------
valueBox("Di, Mi, Do", 
         caption = "Frequenzstärkste Wochentage", 
         icon = "fa-solid fa-calendar-days")


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------
valueBox(paste0("+", calc_increase(freq_all_loc_east, 2020, 2023), " %"), 
         caption = "Anstieg 2023 gegenüber 2020", 
         color = colour_2023, 
         icon = "fa-solid fa-arrow-trend-up")


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------
valueBox(paste0("+", calc_increase(freq_all_loc_east, 2020, 2024), " %"), 
         caption = paste0("Anstieg 2024 gegenüber 2020 (Daten nur bis ", 
                          last_entry_date,")"), 
         color = colour_2024, icon = "fa-solid fa-triangle-exclamation")


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------
all_loc_east$rel + ggtitle("Relative Frequenzen alle Messorte Ost")


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------
valueBox("Mo + Fr", caption = "Relativer Rückgang", 
         icon = "fa-solid fa-calendar-days")


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------
freq_all_loc <- freq_df
all_loc <- freq_plots(freq_all_loc)
all_loc$abs + ggtitle("Frequenzen alle Daten!")


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------
valueBox("Di, Mi, Do", caption = "Frequenzstärkste Wochentage", 
         icon = "fa-solid fa-calendar-days")


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------
valueBox(paste0("+", calc_increase(freq_all_loc, 2020, 2023), " %"), 
         caption = "Anstieg 2023 gegenüber 2020", color = colour_2023, 
         icon = "fa-solid fa-arrow-trend-up")


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------
valueBox(paste0("+", calc_increase(freq_all_loc, 2020, 2024), " %"), 
         caption = paste0("Anstieg 2024 gegenüber 2020 (Daten nur bis ", 
                          last_entry_date,")"),
         color = colour_2024, 
         icon = "fa-solid fa-triangle-exclamation")


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------
all_loc$rel + ggtitle("Relative Frequenzen alle Daten!")


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------
valueBox("Mo + Fr", caption = "Relativer Rückgang", 
         icon = "fa-solid fa-calendar-days")

