library(tidyverse)
library(openxlsx)
library(devtools)
tide_pre = openxlsx::read.xlsx(xlsxFile = "C:/Users/carol/Desktop/R Directory/WRDS/Predicted Tide/Merge of all merge.xlsx",
                               sheet = "Merge-merge")
tide_pre = tide_pre %>%
  pivot_longer(-c("Station", "Year", "Month", "Day"), values_to = "Value", names_to = "Hour") %>%
  mutate(Year = as.numeric(Year),
         Month = as.numeric(Month),
         Day = as.numeric(Day),
         Hour = as.numeric(Hour),
         Hour = paste0(sprintf("%04d", Year), "-",
                       sprintf("%02d", Month), "-",
                       sprintf("%02d", Day), " ",
                       sprintf("%02d", Hour)),
         Min = "Min_00") %>%
  select(-c("Day", "Month", "Year")) %>%
  mutate(Type = "Predicted Tidal Height (m)") %>%
  relocate(Station, Hour, Min, Type, Value) %>%
  mutate(DateTime = ISOdatetime(year = substr(Hour, 1, 4),
                                month = substr(Hour, 6, 7),
                                day = substr(Hour, 9, 10),
                                hour = substr(Hour, 12, 13),
                                min = 0,
                                sec = 0,
                                tz = "HongKong"))

tide_pre = tide_pre %>%
  arrange(Hour) %>%
  arrange(Station)

use_data(tide_pre, overwrite = T)

tide_pre_station = tide_pre %>%
  select(Station) %>%
  distinct() %>%
  mutate(Fullname = NA) %>%
  mutate(Fullname = ifelse(Station == "CLK", "Chek Lap Kok (E)",
                    ifelse(Station == "CCH", "Cheung Chau",
                    ifelse(Station == "CMW", "Chi Ma Wan",
                    ifelse(Station == "KLW", "Ko Lau Wan",
                    ifelse(Station == "KCT", "Kwai Chung",
                    ifelse(Station == "LOP", "Lok On Pai",
                    ifelse(Station == "MWC", "Ma Wan",
                    ifelse(Station == "QUB", "Quarry Bay",
                    ifelse(Station == "SPW", "Shek Pik",
                    ifelse(Station == "TMW", "Tai Miu Wan",
                    ifelse(Station == "TAO", "Tai O",
                    ifelse(Station == "TPK", "Tai Po Kau",
                    ifelse(Station == "TBT", "Tsim Bei Tsui",
                    ifelse(Station == "WAG", "Waglan Island", NA)))))))))))))))

use_data(tide_pre_station, overwrite = T)

#Station Hour Min Type Value
#          Station          Hour    Min             Type Value
#1      Quarry Bay 2022-08-28 23 Min_55 Tidal Height (m)  1.62
#2      Quarry Bay 2022-08-29 00 Min_00 Tidal Height (m)  1.62
#3      Quarry Bay 2022-08-29 00 Min_05 Tidal Height (m)  1.61
#4      Quarry Bay 2022-08-29 00 Min_10 Tidal Height (m)  1.59
#5      Quarry Bay 2022-08-29 00 Min_15 Tidal Height (m)  1.58
