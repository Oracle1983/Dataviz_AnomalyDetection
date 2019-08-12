library(tidyverse)
library(data.table)
library(plotly)
library(openair)

data <- readRDS("./Kelvin/SMU_WIP/Viz project/data/dfJul07_2.rds")

### Data processing (General)
colnames(data)[1] = "date"
data$Value <- data$Value %>% replace_na(0)
# Removed 2000+ rows with duplicated timestamps
data <- data %>% distinct()

# Create a dictionary to populate details after aggregating timestamps
dict <- data %>% 
  distinct(ID, .keep_all=T) %>%
  select(-c("date","Value"))

# Perform aggregation of timestamps in 5 mins interval        
five_min = timeAverage(data, avg.time="5 min", statistic="mean", type="ID")
five_min = merge(five_min, dict, by="ID", all.x=T)
# Perform aggregation of timestamps in hourly interval        
hourly = timeAverage(data, avg.time="hour", statistic="mean", type="ID")
hourly = merge(hourly, dict, by="ID", all.x=T)
# Perform aggregation of timestamps in daily interval        
daily = timeAverage(five_min, avg.time="day", statistic="mean", type="ID")
daily = merge(daily, dict, by="ID", all.x=T)

# Exclude data on 01/08/2007 as it is errornous 
daily <- daily %>% filter(date < "2007-08-01")
five_min <- five_min %>% filter(date < "2007-08-01")
hourly <- hourly %>% filter(date < "2007-08-1")

# Output files in csv
write.csv(five_min, "./data/five_min.csv")
write.csv(hourly, "./data/hourly.csv")
write.csv(daily, "./data/daily.csv")

### Seperated runs
### Data processing (Parallel Coord)
five_min = read_csv("./data/five_min.csv")
hourly = read_csv("./data/hourly.csv")
daily = read_csv("./data/daily.csv")

data_pc_5min = five_min %>% spread(MsureGr_lvl, Value)
data_pc_hourly = hourly %>% spread(MsureGr_lvl, Value)
data_pc_daily = daily %>% spread(MsureGr_lvl, Value)

data_pc_5min = rename(data_pc_5min,
                  H11 = "11H",
                  H2 = "2H",
                  H5 = "5H",
                  H8 = "8H",
                  End_Face = "End Face"
)

data_pc_hourly = rename(data_pc_hourly,
                H11 = "11H",
                H2 = "2H",
                H5 = "5H",
                H8 = "8H",
                End_Face = "End Face"
)

data_pc_daily = rename(data_pc_daily,
                  H11 = "11H",
                  H2 = "2H",
                  H5 = "5H",
                  H8 = "8H",
                  End_Face = "End Face"
)

# Prepare data for Parallel coordinates plot (FunSys_item)
per_day = group_by(data_pc_daily, date, FunSys_item) %>% summarize(
  H11 = mean(H11, na.rm = T),
  H2 = mean(H2, na.rm = T),
  H5 = mean(H5, na.rm = T),
  H8 = mean(H8, na.rm = T),
  AP1 = mean(AP1, na.rm = T),
  DISK11 = mean(DISK11, na.rm = T),
  DISK12 = mean(DISK12, na.rm = T),
  End_Face = mean(End_Face, na.rm = T),
  HP1 = mean(HP1, na.rm = T),
  HPCLR_1 = mean(HPCLR_1, na.rm = T),
  HPCLR_2 = mean(HPCLR_2, na.rm = T),
  HPCLR_3 = mean(HPCLR_3, na.rm = T),
  L = mean(L, na.rm = T),
  L1 = mean(L1, na.rm = T),
  L2 = mean(L2, na.rm = T),
  L2oo3 = mean(L2oo3, na.rm = T),
  MBH40GD010 = mean(MBH40GD010, na.rm = T),
  R = mean(R, na.rm = T),
  T1 = mean(T1, na.rm = T),
  T3 = mean(T3, na.rm = T)
)

per_hour = group_by(data_pc_hourly, date, FunSys_item) %>% summarize(
  H11 = mean(H11, na.rm = T),
  H2 = mean(H2, na.rm = T),
  H5 = mean(H5, na.rm = T),
  H8 = mean(H8, na.rm = T),
  AP1 = mean(AP1, na.rm = T),
  DISK11 = mean(DISK11, na.rm = T),
  DISK12 = mean(DISK12, na.rm = T),
  End_Face = mean(End_Face, na.rm = T),
  HP1 = mean(HP1, na.rm = T),
  HPCLR_1 = mean(HPCLR_1, na.rm = T),
  HPCLR_2 = mean(HPCLR_2, na.rm = T),
  HPCLR_3 = mean(HPCLR_3, na.rm = T),
  L = mean(L, na.rm = T),
  L1 = mean(L1, na.rm = T),
  L2 = mean(L2, na.rm = T),
  L2oo3 = mean(L2oo3, na.rm = T),
  MBH40GD010 = mean(MBH40GD010, na.rm = T),
  R = mean(R, na.rm = T),
  T1 = mean(T1, na.rm = T),
  T3 = mean(T3, na.rm = T)
)

per_5min = group_by(data_pc_5min, date, FunSys_item) %>% summarize(
  H11 = mean(H11, na.rm = T),
  H2 = mean(H2, na.rm = T),
  H5 = mean(H5, na.rm = T),
  H8 = mean(H8, na.rm = T),
  AP1 = mean(AP1, na.rm = T),
  DISK11 = mean(DISK11, na.rm = T),
  DISK12 = mean(DISK12, na.rm = T),
  End_Face = mean(End_Face, na.rm = T),
  HP1 = mean(HP1, na.rm = T),
  HPCLR_1 = mean(HPCLR_1, na.rm = T),
  HPCLR_2 = mean(HPCLR_2, na.rm = T),
  HPCLR_3 = mean(HPCLR_3, na.rm = T),
  L = mean(L, na.rm = T),
  L1 = mean(L1, na.rm = T),
  L2 = mean(L2, na.rm = T),
  L2oo3 = mean(L2oo3, na.rm = T),
  MBH40GD010 = mean(MBH40GD010, na.rm = T),
  R = mean(R, na.rm = T),
  T1 = mean(T1, na.rm = T),
  T3 = mean(T3, na.rm = T)
)

# Create a column in running order for each func_sys_item
per_5min <- per_5min %>% group_by(FunSys_item) %>% mutate(id=row_number())
per_hour <- per_hour %>% group_by(FunSys_item) %>% mutate(id=row_number())
per_day <- per_day %>% group_by(FunSys_item) %>% mutate(id=row_number())

# # Prepare data for Parallel coordinates plot (FunSys_item)
# per_day = group_by(data_pc_daily, date, FunSys_item) %>% summarize(
#   X1_ave = mean(X1_ave, na.rm = T),
#   X1_DC = mean(X1_DC, na.rm = T),
#   X1_max = mean(X1_max, na.rm = T),
#   X1_min = mean(X1_min, na.rm = T),
#   X2_ave = mean(X2_ave, na.rm = T),
#   X2_DC = mean(X2_DC, na.rm = T),
#   X2_R1_ave = mean(X2_R1_ave, na.rm = T),
#   X2_R1_max = mean(X2_R1_max, na.rm = T),
#   X2_R2_ave = mean(X2_R2_ave, na.rm = T),
#   X2_R2_max = mean(X2_R2_max, na.rm = T),
#   X2_R3_ave = mean(X2_R3_ave, na.rm = T),
#   X2_R3_max = mean(X2_R3_max, na.rm = T),
#   BOT = mean(BOT, na.rm = T),
#   MBA01 = mean(MBA01, na.rm = T),
#   MBA0GD020 = mean(MBA01GD020, na.rm = T),
#   MBA0GD040 = mean(MBA01GD040, na.rm = T),
#   MBA0GD070 = mean(MBA01GD070, na.rm = T),
#   MBA0GD080 = mean(MBA01GD080, na.rm = T),
#   TIT1 = mean(TIT1, na.rm = T),
#   TIT2 = mean(TIT2, na.rm = T),
#   TOP = mean(TOP, na.rm = T),
#   X.NA. = mean(X.NA., na.rm = T)
# )
# 
# per_hour = group_by(data_pc_hourly, date, FunSys_item) %>% summarize(
#   X1_ave = mean(X1_ave, na.rm = T),
#   X1_DC = mean(X1_DC, na.rm = T),
#   X1_max = mean(X1_max, na.rm = T),
#   X1_min = mean(X1_min, na.rm = T),
#   X2_ave = mean(X2_ave, na.rm = T),
#   X2_DC = mean(X2_DC, na.rm = T),
#   X2_R1_ave = mean(X2_R1_ave, na.rm = T),
#   X2_R1_max = mean(X2_R1_max, na.rm = T),
#   X2_R2_ave = mean(X2_R2_ave, na.rm = T),
#   X2_R2_max = mean(X2_R2_max, na.rm = T),
#   X2_R3_ave = mean(X2_R3_ave, na.rm = T),
#   X2_R3_max = mean(X2_R3_max, na.rm = T),
#   BOT = mean(BOT, na.rm = T),
#   MBA01 = mean(MBA01, na.rm = T),
#   MBA0GD020 = mean(MBA01GD020, na.rm = T),
#   MBA0GD040 = mean(MBA01GD040, na.rm = T),
#   MBA0GD070 = mean(MBA01GD070, na.rm = T),
#   MBA0GD080 = mean(MBA01GD080, na.rm = T),
#   TIT1 = mean(TIT1, na.rm = T),
#   TIT2 = mean(TIT2, na.rm = T),
#   TOP = mean(TOP, na.rm = T),
#   X.NA. = mean(X.NA., na.rm = T)
# )
# 
# per_5min = group_by(data_pc_5min, date, FunSys_item) %>% summarize(
#   X1_ave = mean(X1_ave, na.rm = T),
#   X1_DC = mean(X1_DC, na.rm = T),
#   X1_max = mean(X1_max, na.rm = T),
#   X1_min = mean(X1_min, na.rm = T),
#   X2_ave = mean(X2_ave, na.rm = T),
#   X2_DC = mean(X2_DC, na.rm = T),
#   X2_R1_ave = mean(X2_R1_ave, na.rm = T),
#   X2_R1_max = mean(X2_R1_max, na.rm = T),
#   X2_R2_ave = mean(X2_R2_ave, na.rm = T),
#   X2_R2_max = mean(X2_R2_max, na.rm = T),
#   X2_R3_ave = mean(X2_R3_ave, na.rm = T),
#   X2_R3_max = mean(X2_R3_max, na.rm = T),
#   BOT = mean(BOT, na.rm = T),
#   MBA01 = mean(MBA01, na.rm = T),
#   MBA0GD020 = mean(MBA01GD020, na.rm = T),
#   MBA0GD040 = mean(MBA01GD040, na.rm = T),
#   MBA0GD070 = mean(MBA01GD070, na.rm = T),
#   MBA0GD080 = mean(MBA01GD080, na.rm = T),
#   TIT1 = mean(TIT1, na.rm = T),
#   TIT2 = mean(TIT2, na.rm = T),
#   TOP = mean(TOP, na.rm = T),
#   X.NA. = mean(X.NA., na.rm = T)
# )

# Output files in csv
write.csv(data_pc_5min, "./data/data_pc_5min.csv")
write.csv(data_pc_hourly, "./data/data_pc_hourly.csv")
write.csv(data_pc_daily, "./data/data_pc_daily.csv")
write.csv(per_5min, "./data/per_5min.csv")
write.csv(per_hour, "./data/per_hour.csv")
write.csv(per_day, "./data/per_day.csv")
