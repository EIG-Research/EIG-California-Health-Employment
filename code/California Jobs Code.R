
# Title: Healthcare Jobs Map
# Author: Thomas Cronin
# File Purpose: Examine health sector employment changes in California
# Last Updated: 5/8/2026

# Data dictionary: https://www.bls.gov/cew/about-data/documentation-guide.htm

## Load Data ----

#load packages
packages <- c("tidyverse", "openxlsx", "tigris", "sf", "janitor", "tidycensus", "fredr", "lubridate")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}
invisible(lapply(packages, library, character.only = TRUE))

#qcew default api code
qcewGetIndustryData <- function (year, qtr, industry) {
  url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/industry/INDUSTRY.csv"
  url <- sub("YEAR", year, url, ignore.case=FALSE)
  url <- sub("QTR", tolower(qtr), url, ignore.case=FALSE)
  url <- sub("INDUSTRY", industry, url, ignore.case=FALSE)
  read.csv(url, header = TRUE, sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
}

qcewGetAreaData <- function(year, qtr, area) {
  url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/area/AREA.csv"
  url <- sub("YEAR", year, url, ignore.case=FALSE)
  url <- sub("QTR", tolower(qtr), url, ignore.case=FALSE)
  url <- sub("AREA", toupper(area), url, ignore.case=FALSE)
  read.csv(url, header = TRUE, sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
}

#qcew modified industry api code (all quarters in a range)
qcew_safe <- safely(qcewGetIndustryData)
qcewGetIndustryData_range <- function(years, quarters, industry) {
  results <- expand_grid(year = years, qtr = quarters) %>%
    mutate(res = map2(year, qtr, ~ qcew_safe(.x, .y, industry)))
  
  data <- results %>%
    mutate(data = map(res, "result")) %>%
    pull(data) %>%
    bind_rows()
  
  errors <- results %>%
    mutate(error = map(res, "error")) %>%
    filter(!map_lgl(error, is.null))
  
  list(data = data, errors = errors)
}

## Import and Clean Data ----

#import QCEW data
qcew_industry_2022 <- qcewGetAreaData(2022, 3, "06000")
qcew_industry_2025 <- qcewGetAreaData(2025, 3, "06000")
qcew_elderly_care <- qcewGetIndustryData_range(years = 2022:2025, quarters = c("1", "2", "3", "4"), industry = "62412")$data

#import cpi
fredr_set_key("KEY") #replace KEY with your FRED access key
cpi <- fredr(series_id = "CPIAUCSL") %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(cpi = mean(value, na.rm = TRUE), .groups = "drop")

base_cpi <- cpi %>%
  filter(year == 2025) %>%
  pull(cpi)

deflator_2025 <- cpi %>%
  mutate(deflator = cpi / base_cpi) %>%
  filter(year == 2022) %>%
  pull(deflator)

#import codebooks - created manually from here: https://www.bls.gov/cew/about-data/documentation-guide.htm
codebook_naics <- read.xlsx("data/QCEW Codes.xlsx", sheet = "NAICS")
codebook_health <- read.xlsx("data/QCEW Codes.xlsx", sheet = "Healthcare Industries") 

#create xwalk
state_xwalk <- states(cb = TRUE, year = 2023) %>%
  st_drop_geometry() %>%
  select(state_fips = STATEFP, state = NAME) %>%
  mutate(state_fips = str_c(state_fips, "000"))

## Analyze State NAICS Data ----

#all sectors
sectors <- c("10", "11", "21", "22", "23", "31-33", "42", "44-45", "48-49", "51", "52", "53", "54", "55", "56", "61", "62", "71", "72", "81")
naics <- bind_rows(qcew_industry_2022, qcew_industry_2025) %>%
  filter(industry_code %in% sectors, own_code == 5) %>%
  left_join(codebook_naics, by = c("industry_code" = "code")) %>%
  mutate(emp_q = round((month1_emplvl + month2_emplvl + month3_emplvl)/3, digits = 0),
         suppressed = case_when(disclosure_code == "N" ~ "1",
                                TRUE ~ "0"),
         suppressed = as.numeric(suppressed)) %>%
  select(year, description, emp_q, avg_wkly_wage) %>%
  pivot_wider(names_from = year, values_from = c(emp_q, avg_wkly_wage)) %>%
  mutate(pct_change_emp = (emp_q_2025 - emp_q_2022)/emp_q_2022,
         pct_change_wage = (avg_wkly_wage_2025 - avg_wkly_wage_2022)/avg_wkly_wage_2022,
         real_wage_2022 = avg_wkly_wage_2022 / deflator_2025,
         pct_change_wage_real = (avg_wkly_wage_2025 - real_wage_2022)/real_wage_2022) %>%
  select(description, starts_with("emp_q"), avg_wkly_wage_2022, real_wage_2022, everything())

#all health subsectors
health <- bind_rows(qcew_industry_2022, qcew_industry_2025) %>%
  filter(str_starts(industry_code, "62"), own_code == 5) %>%
  mutate(emp_q = round((month1_emplvl + month2_emplvl + month3_emplvl)/3, digits = 0),
         avg_wkly_wage = case_when(avg_wkly_wage == 0 ~ NA,
                                   TRUE ~ avg_wkly_wage)) %>%
  select(year, industry_code, emp_q, avg_wkly_wage) %>%
  pivot_wider(names_from = year, values_from = c(emp_q, avg_wkly_wage)) %>%
  left_join(codebook_health, by = c("industry_code" = "code")) %>%
  mutate(code_length = nchar(industry_code),
         pct_change_emp = (emp_q_2025 - emp_q_2022)/emp_q_2022,
         pct_change_wage = (avg_wkly_wage_2025 - avg_wkly_wage_2022)/avg_wkly_wage_2022) %>%
  arrange(code_length, industry_code) %>%
  select(industry_code, description, starts_with(c("emp_q", "avg_wkly", "pct")))

#elderly services quarterly - california
qcew_elderly_care_cal <- qcew_elderly_care %>%
  filter(area_fips == "06000", own_code == 5, !(year == 2022 & qtr < 3)) %>%
  mutate(emp_q = round(rowMeans(across(c(month1_emplvl, month2_emplvl, month3_emplvl)), na.rm = TRUE), 0)) %>%
  select(year, qtr, emp_q, avg_wkly_wage, oty_avg_wkly_wage_pct_chg)

#elderly services quarterly - all states
qcew_elderly_care_state <- qcew_elderly_care %>%
  filter(str_ends(area_fips, "000"), own_code == 5, !(year == 2022 & qtr < 3), !(area_fips %in% c("72000", "78000"))) %>%
  left_join(state_xwalk, by = c("area_fips" = "state_fips")) %>%
  mutate(emp_q = round(rowMeans(across(c(month1_emplvl, month2_emplvl, month3_emplvl)), na.rm = TRUE), 0),
         avg_wkly_wage = avg_wkly_wage / 100,
         state = case_when(area_fips == "US000" ~ "National",
                           TRUE ~ state)) %>%
  select(state, area_fips, year, qtr, emp_q, avg_wkly_wage, oty_avg_wkly_wage_pct_chg) %>%
  arrange(state, year, qtr)

## Analyze Elderly Care by State ----

qcew_elderly_care_state_change <- qcew_elderly_care_state %>%
  filter(year %in% c(2022, 2025), qtr == 3, !(area_fips %in% c("72000", "78000"))) %>%
  select(area_fips, year, emp_q) %>%
  pivot_wider(names_from = year, values_from = emp_q) %>%
  rename(emp_q_2022 = `2022`, emp_q_2025 = `2025`) %>%
  left_join(state_xwalk, by = c("area_fips" = "state_fips")) %>%
  mutate(pct_change_emp = round((emp_q_2025 - emp_q_2022)/emp_q_2022, 3),
         state = case_when(area_fips == "US000" ~ "National",
                           TRUE ~ state)) %>%
  select(state, everything())

## Calculate Real Wage Changes in California ----

wage_changes <- bind_rows(qcew_industry_2022, qcew_industry_2025) %>%
  filter(own_code == 5) %>%
  mutate(naics_len = str_length(industry_code)) %>%
  group_by(area_fips, year, qtr) %>%
  group_modify(~{ #identify rows that are not parents of any more-detailed published industry
    df <- .x
    df %>%
      rowwise() %>%
      mutate(has_child = any(str_starts(df$industry_code, industry_code) &
                               str_length(df$industry_code) > naics_len)) %>%
      ungroup() %>%
      filter(!has_child, !str_detect(industry_code, "-")) #maximum of 4 digit naics
  }) %>%
  ungroup() %>%
  mutate(emp_q = round((month1_emplvl + month2_emplvl + month3_emplvl)/3, digits = 0),
         avg_wkly_wage = case_when(avg_wkly_wage == 0 ~ NA,
                                   TRUE ~ avg_wkly_wage)) %>%
  select(year, industry_code, emp_q, avg_wkly_wage) %>%
  pivot_wider(names_from = year, values_from = c(emp_q, avg_wkly_wage)) %>%
  mutate(avg_wkly_wage_2022 = round(avg_wkly_wage_2022 / deflator_2025, 0),
         wage_change = (emp_q_2025 - emp_q_2022) * avg_wkly_wage_2022) %>%
  summarize(wage_change = sum(wage_change, na.rm = TRUE)) %>%
mutate(wage_change = wage_change * 52)

## Export to Excel ----

#clean names
naics_export <- naics %>% 
  rename(Sector = description, `Total Employment, 2022q3` = emp_q_2022, `Total Employment, 2025q3` = emp_q_2025, 
         `Average Nominal Weekly Wage, 2022q3` = avg_wkly_wage_2022, `Average Real Weekly Wage, 2022q3 (2025$)` = real_wage_2022,
         `Average Weekly Wage, 2025q3` = avg_wkly_wage_2025, `% Change Employment` = pct_change_emp, `% Change Wages` = pct_change_wage,
         `% Change Real Wages` = pct_change_wage_real)
health_export <- health %>% 
  rename(`Industry Code` = industry_code, Description = description, `Total Employment, 2022q3` = emp_q_2022, 
         `Total Employment, 2025q3` = emp_q_2025, `Average Weekly Wage, 2022q3` = avg_wkly_wage_2022, 
         `Average Weekly Wage, 2025q3` = avg_wkly_wage_2025, `% Change Employment` = pct_change_emp, `% Change Wages` = pct_change_wage)
elderly_services_export <- qcew_elderly_care_cal %>% 
  rename(Year = year, Quarter = qtr, `Total Employment` = emp_q,
         `Average Weekly Wage` = avg_wkly_wage, `% Change Average Weekly Wage (OTY)` = oty_avg_wkly_wage_pct_chg)
elderly_services_state_quarters_export <- qcew_elderly_care_state %>% 
  rename(State = state, Year = year, Quarter = qtr, `Total Employment` = emp_q,
         `Average Weekly Wage` = avg_wkly_wage, `% Change Average Weekly Wage (OTY)` = oty_avg_wkly_wage_pct_chg) %>%
  select(-area_fips)
elderly_services_state_pct_change_export <- qcew_elderly_care_state_change %>%
  rename(State = state, `Total Employment, 2022q3` = emp_q_2022, `Total Employment, 2025q3` = emp_q_2025, `% Change Employment` = pct_change_emp) %>%
  select(-area_fips)
  
#export
output <- createWorkbook()
addWorksheet(output, "NAICS")
addWorksheet(output, "All Health")
addWorksheet(output, "Elderly Services")
addWorksheet(output, "Elderly Services % Change")
writeData(output, sheet = "NAICS", x = naics_export)
writeData(output, sheet = "All Health", x = health_export)
writeData(output, sheet = "Elderly Services", x = elderly_services_state_quarters_export)
writeData(output, sheet = "Elderly Services % Change", x = elderly_services_state_pct_change_export)
saveWorkbook(output, "Output/California Employment Growth by Industry, 2022-2025 unformatted.xlsx", overwrite = TRUE)
