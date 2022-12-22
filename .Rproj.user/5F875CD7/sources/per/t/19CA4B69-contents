# analysis for bill response for Pedro


comm.veh <- data %>%
  filter(grepl("Comm vehicle", Type)) %>%
  group_by(Type) %>%
  count(Status) %>%
  spread(Status, n) %>%
  left_join(data %>%
              filter(grepl("Comm vehicle", Type)) %>%
              group_by(Type, Status) %>%
              summarize(Fees = sum(Fees, na.rm = TRUE)) %>%
              spread(Status, Fees) %>%
              mutate_if(is.numeric, replace_na, 0) %>%
              mutate_if(is.numeric, scales::dollar_format()) %>%
              set_colnames(c("Type", "Abated $", "Open $", "Paid $", "Uncollectible $"))) %>% 
  select(Type, sort(current_vars())) %T>%
  export_excel("Status", "Comm Veh Height Violations.xlsx", "new")

comm.tag <- data %>%
  filter(grepl("Comm vehicle", Type),
         Status %in% c("Open", "Paid")) %>%
  group_by(Tag) %>%
  count() %>%
  arrange(-n) %>%
  ungroup() %>%
  top_n(15) %>%
  mutate(Fees = ((n - 2 ) * 250) + 150) %T>%
  export_excel("Tags w Most Viol", "Comm Veh Height Violations.xlsx", "existing")

comm.mos <- data %>%
  filter(grepl("Comm vehicle", Type)) %>%
  group_by(`Viol Month`, Type) %>%
  count(Type) %>%
  spread(Type, n) %T>%
  export_excel("Viols by Months", "Comm Veh Height Violations.xlsx", "existing")

comm.cameras <- data %>%
  filter(grepl("Comm vehicle", Type)) %>%
  distinct(Location, `Month Deployed`) %>%
  arrange(`Month Deployed`) %T>%
  export_excel("Camera locations", "Comm Veh Height Violations.xlsx", "existing")
