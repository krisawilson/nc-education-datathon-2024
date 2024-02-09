# reading in data ----
library(tidyverse)
library(readxl)
#setwd("C:/nc-education-datathon-2024")

setwd("Data/src_datasets-SY21-22")
credentials <- read_excel("rcd_cte_credentials.xlsx")

location <- read_excel("rcd_location.xlsx")

enrollment <- read_excel("rcd_cte_enrollment.xlsx")

concentrators <- read_excel("rcd_cte_concentrators.xlsx")

setwd("C:/nc-education-datathon-2024/Data/NC Schools")
agencyCodes <- read_excel("2022-23 Agency Number to School Region.xlsx")
funding <- read_csv("State_Allotment_One_PRC_Dollar.csv") |> select(4:11)

setwd("C:/nc-education-datathon-2024")

# funding cleaning ----
split_column <- function(x) {
  tibble(column1 = x / 2, column2 = x / 2)
}

funds <- funding |>
  rowwise() |>
  mutate(split_data = list(split_column(c_across(-`Lea Name`)))) |>
  unnest_wider(split_data) |> 
  unnest_wider(c(column1, column2), names_sep = "_") |> 
  select("Lea Name", contains("column"))

nice_funds <- funds |> 
  rename("district_name" = "Lea Name",
         "F23" = "column1_1", "S24" = "column2_1",
         "F22" = "column1_2", "S23" = "column2_2",
         "F21" = "column1_3", "S22" = "column2_3",
         "F20" = "column1_4", "S21" = "column2_4",
         "F19" = "column1_5", "S20" = "column2_5",
         "F18" = "column1_6", "S19" = "column2_6",
         "F17" = "column1_7", "S18" = "column2_7")

nice_funds$district_name <- paste(nice_funds$district_name, "Schools", 
                                  sep = " ")

yearly_funds <- nice_funds |> rowwise() |>
  mutate(Funding_2023 = F23 + S23, Funding_2022 = F22 + S22,
         Funding_2021 = F21 + S21, Funding_2020 = F20 + S20,
         Funding_2019 = F19 + S19, Funding_2018 = F18 + S18
       ) |> 
  select(district_name, contains("Funding"))

rm(nice_funds, funds, funding)

# credentials cleaning ----
clean_creds <- credentials |> 
  mutate(cred_per_stu = cred_earned_pct/100) |> 
  rename("num_creds_earned" = "num_credentials",
         "num_stu_enroll" = "num_enrollments") |> 
  select(year, agency_code, num_creds_earned)
rm(credentials)
# enrollement cleaning----
clean_enroll <- enrollment |> 
  mutate(pct_stu_enroll = pct/100) |> 
  rename("num_cte_enroll" = "cte_enroll",
         "num_stu_enroll" = "stu_enroll") |> 
  select(year, agency_code, num_cte_enroll, 
         num_stu_enroll, pct_stu_enroll)
rm(enrollment)

# location cleaning ----
clean_location <- location |> 
  filter(year >= 2018) |> 
  mutate(
    designation = case_when(
      designation_Type == "C" ~ "Charter",
      designation_Type == "P" ~ "Public",
      TRUE ~ designation_Type),
    title_i = if_else(is.na(title_i), "No", "Yes")
    ) |> 
  select(year, agency_code, designation, name, county, 
         street_addr, city, zip, title_i)
rm(location)

# concentrators cleaning ----
clean_concentrators <- concentrators |> 
  select(-career_cluster) |> 
  group_by(year, agency_code) |> 
  summarize(total_num_concentrators = sum(num_concentrators)) |> 
  ungroup()
rm(concentrators)

# agencies cleaning ----
clean_agencies <- agencyCodes |> 
  rename("agency_code" = "School\r\nCode",
         "school_name" = "School Name",
         "district_name" = "District Name") |> 
  select(1:3)
rm(agencyCodes)

# joining time ----
years_stuff <- clean_location |> 
  full_join(clean_concentrators, by = c("year", "agency_code")) |> 
  full_join(clean_enroll, by = c("year", "agency_code")) |> 
  full_join(clean_creds, by = c("year", "agency_code"))

school_stuff <- left_join(clean_agencies, yearly_funds, by = "district_name")
full_data <- left_join(years_stuff, 
                       select(school_stuff, -school_name), 
                       by = "agency_code")
full_data <- full_data |> 
  filter(year > 2017) |> 
  rename("school_name" = "name") |> 
  select(year, school_name, street_addr, county, city, zip, district_name, 
         designation, title_i, total_num_concentrators, num_creds_earned,
         num_cte_enroll, num_stu_enroll, pct_stu_enroll, Funding_2018,
         Funding_2019, Funding_2020, Funding_2021, Funding_2022,
         Funding_2023)

data_2018 <- full_data |> filter(year == 2018)
data_2019 <- full_data |> filter(year == 2019)
data_2020 <- full_data |> filter(year == 2020)
data_2021 <- full_data |> filter(year == 2021)
data_2022 <- full_data |> filter(year == 2022)
rm(school_stuff, yearly_funds, years_stuff, clean_agencies, clean_creds,
   clean_enroll, clean_location, clean_concentrators)
write_csv(full_data, "final-data.csv")
write_csv(data_2018, "data-by-year/data_2018.csv")
write_csv(data_2019, "data-by-year/data_2019.csv")
write_csv(data_2020, "data-by-year/data_2020.csv")
write_csv(data_2021, "data-by-year/data_2021.csv")
write_csv(data_2022, "data-by-year/data_2022.csv")