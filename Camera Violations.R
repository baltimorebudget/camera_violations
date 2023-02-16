#.libPaths("C:/Users/sara.brumfield2/Documents/r_library")
# .libPaths("C:/Users/sara.brumfield2/.conda/envs/bbmr/Lib/R/library")
.libPaths("C:/Users/sara.brumfield2/OneDrive - City Of Baltimore/Documents/r_library")
library(tidyverse)
library(lubridate)
library(rio)
library(janitor)
devtools::load_all("G:/Analyst Folders/Sara Brumfield/_packages/bbmR")

# .libPaths("G:/Data/r_library")
# library(bbmR)

#load("Data/Camera Violations 2022-05-03.Rdata")

#This is how you normally input data

data <- import("L:/BBMR_CSV_FILE_20230202.CSV")

violations <- data %>%
  mutate_at(vars(ends_with("DATE")), ymd) %>% 
  mutate(Fees = case_when(`VIOL CODE` %in% c(30, 31) ~ 75,
                          `VIOL CODE` %in% c(32, 33, 34, 35) ~ 40,
                          `VIOL CODE` == 36 ~ 0,
                          `VIOL CODE` == 37 ~ 125,
                          `VIOL CODE` == 38 ~ 250,
                          `VIOL CODE` == 99 ~ 32,
                          `VIOL CODE` %in% c(2, 3) ~ 102),
         Type = case_when(`VIOL CODE` == 30 ~ "Red Light Violation",
                          `VIOL CODE` == 31 ~ "Right on Red",
                          `VIOL CODE` == 32 ~ "Speed Violation",
                          `VIOL CODE` == 33 ~ "No Description",
                          `VIOL CODE` == 34 ~ "No Description",
                          `VIOL CODE` == 35 ~ "No Description",
                          `VIOL CODE` == 36 ~ "Truck Overheight Warning Notice",
                          `VIOL CODE` == 37 ~ "Truck Overheight Second Violation",
                          `VIOL CODE` == 38 ~ "Truck Overheight Third or Subsequent Violation",
                          `VIOL CODE` == 39 ~ "Interstate 83",
                          `VIOL CODE` == 2 ~ "No Stopping or No Parking Pimlico Event",
                          `VIOL CODE` == 3 ~ "Obstruct/Impeding Flow of Traffic",
                          `VIOL CODE` == 99 ~ "All Other Stopping or Parking Violations",
                          `VIOL CODE` == NA ~ "Unknown"),
         `VIOL MONTH` = ymd(paste0(year(`VIOL DATE`), "-", month(`VIOL DATE`), "-01")),
         `PAID MONTH` = ymd(paste0(year(`PAID DATE`), "-", month(`PAID DATE`), "-01")),
         Status = case_when(STATUS == "P" ~ "Paid in full", 
                            STATUS == "O" ~ "Open",
                            (STATUS == "A" & is.na(`PAID DATE`)) ~ "Balance abated (unpaid)",
                            (STATUS == "A" & !is.na(`PAID DATE`)) ~ "Balance abated (paid)",
                            STATUS == "H" ~ "Hold processing",
                            STATUS == "U" ~ "Uncollectable",
                            TRUE ~ STATUS),
         `VIOL YEAR` = format(`VIOL DATE`, format = "%Y"),
         `PAID YEAR` = format(`PAID DATE`, format = "%Y"))

#this can take a while
violations <- violations %>% mutate(`Violation Year` = format(`VIOL DATE`, format = "%Y"),
                                    `Violation Month` = format(`VIOL DATE`, format = "%m"),
                                    `Violation Year-Month` = format(`VIOL DATE`, format = "%Y-%m"),
                                    `Paid Year` = format(`PAID DATE`, format = "%Y"),
                                    `Paid Month` = format(`PAID DATE`, format = "%m"),
                                    `Paid Year-Month` = format(`PAID DATE`, format = "%Y-%m"))

issued <- violations %>% 
  ##filter out data from previous FYs starting Feb 2023
  filter(`VIOL DATE` >= "2022-07-01") %>%
  select(`Violation Year-Month`, `CITATION`, `Type`, LOCATION) %>%
  arrange(`Violation Year-Month`) %>%
  pivot_wider(names_from = `Violation Year-Month`, values_from = CITATION, values_fn = list(CITATION = length))

paid <- violations %>%
  filter(Status %in% c("Paid in full", "Balance abated (paid)")) %>%
  select(`Paid Year-Month`, `CITATION`, `Type`, Status, LOCATION) %>%
  arrange(`Paid Year-Month`) %>%
  pivot_wider(names_from = `Paid Year-Month`, values_from = CITATION, values_fn = list(CITATION = length)) %>%
  rename(`No Paid Date` = `NA`)

status_count <- violations %>%
  filter(`VIOL DATE` >= "2022-07-01") %>%
  select(`Violation Year-Month`, `CITATION`, `Type`, Status) %>%
  arrange(`Violation Year-Month`) %>%
  pivot_wider(names_from = `Violation Year-Month`, values_from = CITATION, values_fn = list(CITATION = length)) %>%
  arrange(Type, Status) %>%
  remove_empty(which = "rows", quiet = FALSE)

status_fees <- violations %>%
  filter(`PAID DATE` >= "2022-07-01") %>%
  select(`Paid Year-Month`, Fees, `Type`, Status) %>%
  arrange(`Paid Year-Month`) %>%
  pivot_wider(names_from = `Paid Year-Month`, values_from = Fees, values_fn = sum) %>%
  arrange(Type, Status) %>%
  remove_empty(which = "rows", quiet = FALSE)


##will need full dataset not just current FY for this section ====
unpaid <- violations %>%
  select(CITATION, TAG, Type, `VIOL DATE`, Status, `Violation Year`) %>%
  filter(Status != "Paid in full" & Status != "Balance abated (paid)") %>%
  arrange(TAG)

#find tickets more than 3 years old, count as lost revenue
#locates tags where ALL tickets are from 2019 or earlier
lost <- violations %>%
  filter((Status != "Paid in full" & Status != "Balance abated (paid)")) %>%
  select(CITATION, TAG, Type, `VIOL DATE`, Status, `Violation Year`) %>%
  mutate(Year = as.numeric(`Violation Year`)) %>%
  group_by(TAG) %>%
  summarise(count = n(),
            max = max(Year)) %>%
  filter(max <= 2019) %>%
  ungroup()

lost_tags <- lost$TAG

lost_rev <- lost %>% left_join(unpaid, by = c("TAG")) %>%
  select(-count, -max) %>%
  relocate(TAG, .after = CITATION) %>%
  arrange(TAG)

##refine unpaid data to omit lost revenue citations
unpaid <- unpaid %>%
  filter(!(TAG %in% lost_tags))

##repeat offenders by TAG
tags <- violations %>%
  filter(Type %in% c("Speed Violation", "Interstate 83")) %>%
  select(TAG, `Violation Year`, CITATION) %>%
  arrange(`Violation Year`) %>%
  pivot_wider(id_cols = TAG, names_from = `Violation Year`, values_from = CITATION, values_fn = list(CITATION = length)) %>%
  arrange(TAG) %>%
  remove_empty(which = "rows", quiet = FALSE) %>%
  adorn_totals(where = "col", fill = 0, na.rm = TRUE, name = "Total Citations") %>%
  filter(`Total Citations` > 1)


# file_path = "G:/BBMR - Revenue Team/2. Revenue Accounts/A001 - General Fund/191-193 - Speed - Red-Light Violations"

export_excel(issued, "Issued (Viol Date)", paste0("outputs/Camera Violations ", Sys.Date() ,".xlsx"), "new")
export_excel(paid, "Paid (Paid Date)", paste0("outputs/Camera Violations ", Sys.Date(), ".xlsx"), "existing")
export_excel(status_count, "Status # (Viol Date)", paste0("outputs/Camera Violations ", Sys.Date(), ".xlsx"), "existing")
export_excel(status_fees, "Status $ (Paid Date)", paste0("outputs/Camera Violations ", Sys.Date(), ".xlsx"), "existing")
export_excel(unpaid, "Unpaid", paste0("outputs/Camera Violations ", Sys.Date(), ".xlsx"), "existing")
export_excel(lost_rev, "Lost Revenue", paste0("outputs/Camera Violations ", Sys.Date(), ".xlsx"), "existing")
export_excel(tags, "Repeat Tags", paste0("outputs/Camera Violations ", Sys.Date(), ".xlsx"), "existing")

##filtered for FY end ====
# violations_FY22 <- violations %>% filter(`Year-Month` < "2022-07")
# 
# issued_fy22 <- violations_FY22 %>% 
#   select(`Year-Month`, `CITATION`, `Type`, LOCATION) %>%
#   arrange(`Year-Month`) %>%
#   pivot_wider(names_from = `Year-Month`, values_from = CITATION, values_fn = list(CITATION = length))
# 
# paid_fy22 <- violations_FY22 %>%
#   filter(Status %in% c("Paid in full", "Balance abated (paid)")) %>%
#   select(`Year-Month`, `CITATION`, `Type`, Status, LOCATION) %>%
#   arrange(`Year-Month`) %>%
#   pivot_wider(names_from = `Year-Month`, values_from = CITATION, values_fn = list(CITATION = length))

##export ========
export_excel(issued_fy22, "Issued 06302022", "outputs/Camera Violations 2022-06-30.xlsx", "new")
export_excel(paid_fy22, "Paid 06302022", "outputs/Camera Violations 2022-06-30.xlsx", "existing")

##====
tag <- violations %>%
  count(STATUS, TAG, `VIOL CODE`, `VIOL MONTH`)

fy <- tag %>%
  mu

summary <- violations %>%
  count(LOCATION, `VIOL CODE`)

summary <- violations %>%
  group_by(LOCATION, `VIOL CODE`) %>%
  summarize(Fees = sum(Fees, na.rm = TRUE))


tag <- violations %>%
  count(TAG, LOCATION, Fees) %>%
  arrange(-n)

tag.summary <- violations %>%
  group_by(TAG) %>%
  summarize(Fees = sum(Fees, na.rm = TRUE))

##public data request
##check $$ against DOT ====
violation_fees <- violations %>% #pivot_longer(cols = "Year", names_to = "Status", values_to = "Fees")
  group_by(Year, Type, Status) %>% 
  summarise("$$" = sum(Fees), .groups = "drop_last") %>%
  pivot_wider(names_from = Year, values_from = "$$")

export_excel(violation_fees, "Fees by Status", "outputs/Camera Violations by Year and Status.xlsx", "new")

#2016 and 47/75 fees only======
violations <- violations %>% mutate(`Year` = format(`VIOL DATE`, format = "%Y"),
                                    `Month` = format(`VIOL DATE`, format = "%m"),
                                    `Year-Month` = format(`VIOL DATE`, format = "%Y-%m"))

filtered_paid <- filter(violations, `VIOL CODE` %in% c(30, 31, 32, 33, 34, 35) & `Year` > 2015) %>%
  mutate(`Pay Status` = case_when(!is.na(`PAID DATE`) == TRUE ~ "Paid",
                                  TRUE ~ "Issued"),
         `Paid Amount` = case_when(is.na(`PAID DATE`) == TRUE ~ 0,
                                   TRUE ~ `Fees`)) %>%
  group_by(`Year`, `Month`, `VIOL CODE`, `Type`, `Pay Status`) %>% 
  summarise(`Total Fees` = sum(`Paid Amount`, na.rm = TRUE),
            `Total Tickets` = n(), , .groups = 'drop')

filtered_paid <- filtered_paid %>%
  filter(`Pay Status` == "Paid")



filtered_issued <- filter(violations, `VIOL CODE` %in% c(30, 31, 32, 33, 34, 35) & `Year` > 2015) %>%
  mutate(`Pay Status` = case_when(!is.na(`PAID DATE`) == TRUE ~ "Paid",
                                  TRUE ~ "Issued"),
         `Paid Amount` = case_when(is.na(`PAID DATE`) == TRUE ~ 0,
                                   TRUE ~ `Fees`)) %>%
  group_by(`Year`, `Month`, `VIOL CODE`, `Type`) %>% 
  summarise(`Total Fees` = sum(`Paid Amount`, na.rm = TRUE),
            `Total Tickets` = n())

export_excel(filtered_paid, "Paid", "Camera Violations 40-75 Only 2022-05-03.xlsx", "new")
export_excel(filtered_issued, "Issued", "Camera Violations 40-75 Only 2022-05-03.xlsx", "existing")
