library(tidyverse)
library(lubridate)
library(tidygeocoder)
library(tidycensus)
library(haven)
library(tigris)
library(did)

# Temple data from https://churchofjesuschristtemples.org/
temples <- read.csv("./temples.csv")

temples$Announced <- mdy(temples$Announced)
temples$Ground_Broken <- mdy(temples$Ground_Broken)
temples$Dedicated <- mdy(temples$Dedicated)
temples$Zipcode <- stringr::str_pad(temples$Zipcode, width = 5, side = "left", pad = "0")

# Census Data
# 1. Define the 2000 SF3 Variable List
#    (These codes are specific to the 2000 Long Form)
vars_2000 <- c(
    # Wealth & Density
    med_income     = "P053001",  # Median Household Income in 1999
    total_pop      = "P001001",  # Total Population
    
    # Housing Stability (Owner vs Renter)
    occupied_units = "H007001",  # Denominator: Total Occupied Units
    owner_occupied = "H007002",  # Numerator: Owner Occupied
    
    # Family Structure (Kids)
    total_hh       = "P010001",  # Denominator: Total Households
    hh_with_kids   = "P010002",  # Numerator: Households with 1+ person <18
    
    # Education (Bachelor's Degree or Higher)
    # Note: 2000 Census splits this by sex, so we must grab all columns to sum them.
    pop_25_plus    = "P037001",  # Denominator: Population 25+
    
    # Males (Bach, Master, Prof, PhD)
    m_bach = "P037015", m_mast = "P037016", m_prof = "P037017", m_phd = "P037018",
    
    # Females (Bach, Master, Prof, PhD)
    f_bach = "P037032", f_mast = "P037033", f_prof = "P037034", f_phd = "P037035",

    med_year_built = "H035001"  # Median Year Structure Built
    )

# 2. Fetch the Data
control_data_raw <- get_decennial(
    geography = "zcta",
    variables = vars_2000,
    year = 2000,
    sumfile = "sf3",       # CRITICAL: This accesses the 'Long Form' economic data
    geometry = FALSE       # We just need the data to join to your existing shapes
)

# 3. Clean and Calculate Ratios
control_data_final <- control_data_raw %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    mutate(
        # A. Log Wealth (Standard for price models to normalize skew)
        base_log_inc = log(med_income+1),
        
        # B. Population Density (Proxy)
        # Note: True density requires area, but raw pop is a fine proxy if using fixed effects.
        base_log_pop = log(total_pop+1),
        
        # C. Stability (% Homeowners)
        base_pct_own = owner_occupied / occupied_units,
        
        # D. Family Structure (% Households with Kids)
        base_pct_kids = hh_with_kids / total_hh,
        
        # E. Education (% Bachelor's+)
        # Summing all the degree columns for the numerator
        college_count = (m_bach + m_mast + m_prof + m_phd) + 
                        (f_bach + f_mast + f_prof + f_phd),
        base_pct_educ = college_count / pop_25_plus,

        # F. Median Year Built (to control for housing age)
        base_year_built = med_year_built
    ) %>%
    
    # Keep only ID and the 6 final variables
    select(
        GEOID, 
        base_log_inc, 
        base_log_pop, 
        base_pct_own, 
        base_pct_kids, 
        base_pct_educ,
        base_year_built
    )

# Zillow ZHVI Single Family Residence Index
# Downloaded from https://www.zillow.com/research/data/
zillow_data <- read.csv("./zillow_zipcodes.csv")

zillow_data <- zillow_data |> 
    pivot_longer(
        cols = starts_with("X"),           # Select all date columns
        names_to = "date",                 # New column for dates
        values_to = "value"                # New column for values (home prices)
    ) |> 
    mutate(
        # Clean up the date column: remove "X" and convert to Date
        date = gsub("X", "", date),
        date = as.Date(date, format = "%Y.%m.%d"),
        Zipcode = stringr::str_pad(RegionName, width = 5, side = "left", pad = "0")
    ) |> 
    left_join(
        control_data_final, by = c("Zipcode" = "GEOID")
    )

# Note: Ephraim UT Temple Zipcode not in zillow data
sum(zillow_data$Zipcode == "84627")

clean_county_names <- function(name) {
    # 1. Lowercase
    name <- tolower(name)
    
    # 2. Remove Punctuation (Fixes "Prince George's" -> "Prince Georges")
    #    We remove periods (.) and apostrophes (')
    name <- gsub("[.']", "", name)
    
    # 3. Expand "st " to "saint " (The #1 mismatch killer)
    #    ^st\\s matches "st " at the start; \\sst\\s matches " st " in the middle
    name <- gsub("^st\\s", "saint ", name)
    name <- gsub("\\sst\\s", " saint ", name)
    
    # 4. Handle "Ste." (Rare, e.g., Ste. Genevieve, MO)
    name <- gsub("^ste\\s", "sainte ", name)
    
    # 5. Remove Suffixes (Order matters! Longest first)
    name <- gsub(" city and borough", "", name) # Juneau, AK
    name <- gsub(" census area", "", name)      # Alaska
    name <- gsub(" municipality", "", name)     # Anchorage, AK
    name <- gsub(" parish", "", name)           # Louisiana
    name <- gsub(" borough", "", name)          # Alaska / NYC
    name <- gsub(" county", "", name)           # Everyone else
    
    # 6. Clean Whitespace (Fixes double spaces inside the name)
    name <- gsub("\\s+", " ", name)
    name <- trimws(name)
    
    return(name)
}

# LDS per capita yr2010 (county-level)
# Downloaded here: https://thearda.com/data-archive?fid=RCMSCY10
data("fips_codes")
(lds_data <- read_sav("./us_religion_census_2010.sav") |> 
    mutate(county_prop_lds = (LDSADH / POP2010)*100) |> 
    select(STCODE, CNTYCODE, county_prop_lds) |> 
    mutate(STCODE = as.character(STCODE), CNTYCODE = as.character(CNTYCODE)) |> 
    mutate(
        STCODE = stringr::str_pad(STCODE, width = 2, side = "left", pad = "0"),
        CNTYCODE = stringr::str_pad(CNTYCODE, width = 3, side = "left", pad = "0")
    ) |> 
    inner_join(fips_codes, by = c("STCODE" = "state_code", "CNTYCODE" = "county_code")) |> 
    select(state, state_name, county, county_prop_lds) |> 
    mutate(county = clean_county_names(county), county_prop_lds = ifelse(is.na(county_prop_lds), 0, county_prop_lds)))

data <- zillow_data |> 
    left_join(temples, by = "Zipcode") |> 
    mutate(state = State, county = clean_county_names(CountyName))

data <- data |> 
    left_join(lds_data, by = c("state", "county"))

# Because I am working with a computer 16GB RAM, clean up intermediate data

rm(zillow_data)
rm(control_data_final)
rm(control_data_raw)
rm(lds_data)

sorted_memory <- sort(sapply(ls(), function(x) object.size(get(x))), decreasing = TRUE)
print(round(sorted_memory / 1024^2, 2)) # Convert bytes to MB and print

# 1. What is the average increase of home prices in Temple and nontemple zips?
# Temple is higher!

data |> 
    filter(year(date)>=2010) |>
    filter(year(Announced)>=2011 | is.na(Announced)) |>
    group_by(Zipcode) |> 
    summarize(
        county = unique(CountyName),
        state = unique(State),
        tmple_dummy = ifelse(is.na(unique(Temple)), 0, 1),
        first_value = first(value),
        last_value = last(value),
        pct_change = (last(value)-first(value))/first(value),
        county_prop_lds = unique(county_prop_lds),
        Announced = unique(Announced),
        first_year = min(year(date)),
        last_year = max(year(date)),
        n_obs = n(),
        temple = unique(Temple)
    ) |> 
    group_by(tmple_dummy) |>
    summarize(
        avg_pct_change = mean(pct_change, na.rm=TRUE),
        n = n()
    )

# 2. How does this vary by LDS population share in the county?
# Higher LDS share counties see bigger increases in temple zips.

data |> 
    filter(year(date)>=2010) |>
    filter(year(Announced)>=2011 | is.na(Announced)) |>
    group_by(Zipcode) |> 
    summarize(
        county = unique(CountyName),
        state = unique(State),
        tmple_dummy = ifelse(is.na(unique(Temple)), 0, 1),
        first_value = first(value),
        last_value = last(value),
        pct_change = (last(value)-first(value))/first(value),
        county_prop_lds = unique(county_prop_lds),
        Announced = unique(Announced),
        first_year = min(year(date)),
        last_year = max(year(date)),
        n_obs = n(),
        temple = unique(Temple)
    ) |> 
    filter(county_prop_lds>=30) |> 
    group_by(tmple_dummy) |>
    summarize(
        avg_pct_change = mean(pct_change, na.rm=TRUE),
        n = n()
    )

data |> 
    filter(year(date)>=2010) |>
    filter(year(Announced)>=2011 | is.na(Announced)) |>
    group_by(Zipcode) |> 
    summarize(
        county = unique(CountyName),
        state = unique(State),
        tmple_dummy = ifelse(is.na(unique(Temple)), 0, 1),
        first_value = first(value),
        last_value = last(value),
        pct_change = (last(value)-first(value))/first(value),
        county_prop_lds = unique(county_prop_lds),
        Announced = unique(Announced),
        first_year = min(year(date)),
        last_year = max(year(date)),
        n_obs = n(),
        temple = unique(Temple)
    ) |> 
    filter(county_prop_lds>=5) |> 
    group_by(tmple_dummy) |>
    summarize(
        avg_pct_change = mean(pct_change, na.rm=TRUE),
        n = n()
    )

data |> 
    filter(year(date)>=2010) |>
    filter(year(Announced)>=2011 | is.na(Announced)) |>
    group_by(Zipcode) |> 
    summarize(
        county = unique(CountyName),
        state = unique(State),
        tmple_dummy = ifelse(is.na(unique(Temple)), 0, 1),
        first_value = first(value),
        last_value = last(value),
        pct_change = (last(value)-first(value))/first(value),
        county_prop_lds = unique(county_prop_lds),
        Announced = unique(Announced),
        first_year = min(year(date)),
        last_year = max(year(date)),
        n_obs = n(),
        temple = unique(Temple)
    ) |> 
    filter(county_prop_lds<5) |> 
    group_by(tmple_dummy) |>
    summarize(
        avg_pct_change = mean(pct_change, na.rm=TRUE),
        n = n()
    )

# 3. How are temple zips different from nontemple zips in 2000 census data?

data |> 
    select(
        Zipcode,
        base_log_inc,
        base_log_pop,
        base_pct_own,
        base_pct_kids,
        base_pct_educ,
        base_year_built,
        county_prop_lds,
        Temple
    ) |> 
    distinct() |>
    mutate(tmple_dummy = ifelse(is.na(Temple), 0, 1)) |> 
    group_by(tmple_dummy) |>
    summarize(
        avg_log_inc = median(base_log_inc, na.rm=TRUE),
        avg_log_pop = median(base_log_pop, na.rm=TRUE),
        avg_pct_own = mean(base_pct_own, na.rm=TRUE),
        avg_pct_kids = mean(base_pct_kids, na.rm=TRUE),
        avg_pct_educ = mean(base_pct_educ, na.rm=TRUE),
        avg_year_built = mean(base_year_built, na.rm=TRUE),
        avg_county_prop_lds = mean(county_prop_lds, na.rm=TRUE))


# # A tibble: 2 × 8
#   tmple_dummy avg_log_inc avg_log_pop avg_pct_own avg_pct_kids avg_pct_educ
#         <dbl>       <dbl>       <dbl>       <dbl>        <dbl>        <dbl>
# 1           0        10.5        8.40       0.750        0.241        0.194
# 2           1        10.8       10.1        0.748        0.203        0.328
# # ℹ 2 more variables: avg_year_built <dbl>, avg_county_prop_lds <dbl>

# # A tibble: 2 × 2
#   avg_year_built avg_county_prop_lds
#            <dbl>               <dbl>
# 1          1967.                1.74
# 2          1978.               22.6 


