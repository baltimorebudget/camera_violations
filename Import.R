library(tidyverse)
library(lubridate)
library(rio)
library(magrittr)

# parking ####

data <- list.files(path = "//balt-fileld-srv/legacy/BBMR CAMERA", pattern =  "TXT", full.names = TRUE) %>%
  map(read_delim, "¦")

saveRDS(test, "camera_viol_2020-01-23.Rds")

# camera ####

files <- list.files(path = "//balt-fileld-srv/legacy/BBMR CAMERA", pattern =  "FILE_20[0-9]{2}.*.CSV", full.names = TRUE)

camera <- files %>%
  map(import) %>%
  set_names(gsub("[^0-9]", "", files)) %>%
  bind_rows(.id = "file") %>%
  mutate(file = ifelse(file == "2019", "20190101", file)) %>%
  arrange(CITATION, desc(file)) %>%
  distinct(CITATION, TAG, `VIOL CODE`, LOCATION, `VIOL DATE`, .keep_all = TRUE) %>% 
  select(-file)

saveRDS(camera, "camera_viol_2020_07_31.Rds")
