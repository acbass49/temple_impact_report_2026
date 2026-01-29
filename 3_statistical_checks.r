## Common Support ##

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

rm(zillow_data)
rm(control_data_final)
rm(control_data_raw)
rm(lds_data)

sorted_memory <- sort(sapply(ls(), function(x) object.size(get(x))), decreasing = TRUE)
print(round(sorted_memory / 1024^2, 2)) # Convert bytes to MB and print

gc()

plot_data <- data |> 
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
    mutate(tmple_dummy = ifelse(is.na(Temple), 0, 1))

plot_data <- plot_data  |> 
    select(-Temple) |> 
    drop_na()

plot_data <- plot_data %>%
    filter(county_prop_lds > 3) # Only look at counties with >3% LDS

# 1. Estimate Propensity Scores on this filtered universe
ps_model <- glm(
    tmple_dummy ~ base_log_inc + base_log_pop + base_pct_own + 
                    base_pct_kids + base_pct_educ + county_prop_lds, 
    data = plot_data,
    family = binomial()
)

# 2. Add scores back
plot_data$propensity_score <- predict(ps_model, type = "response")

# 3. Labels
plot_data <- plot_data %>%
    mutate(treatment_label = ifelse(tmple_dummy == 1, "Treated (Temple)", "Control (No Temple)"))

# 4. Generate the Plot
p_support <- ggplot(plot_data, aes(x = propensity_score, fill = treatment_label)) +
    
    geom_density(alpha = 0.5, color = "white") +
    
    scale_fill_manual(values = c("Control (No Temple)" = "#555555", "Treated (Temple)" = "#337AB7")) +
    
    labs(
        title = "Common Support Check: Propensity Score Overlap",
        subtitle = "Comparing Treated vs. Control Zips (Restricted to >3% LDS Counties)",
        x = "Propensity Score (Predicted Probability of Temple)",
        y = "Density",
        fill = "Group",
        caption = "Source: Mormon Metrics | Sub-sample: >3% LDS Density"
    ) +
    
    theme_minimal(base_size = 14) +
    theme(
        legend.position = "top",
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank()
    )

ggsave("./images/propensity_score_overlap.png", p_support, width = 2000, height = 2000, units = "px", dpi = 300)

rm(plot_data)

# Leave one out analysis
quaterly_model_data <- data  |> 
    filter(year(date)>=2010) |>
    filter(year(Announced)>=2011 | is.na(Announced)) |>
    mutate(
        # A. Create a "Month Index" (Continuous Counter)
        #    Jan 2010 becomes 1, Feb 2010 becomes 2...
        #    This ensures the gap between Dec and Jan is always 1 unit.
        quarter_index = (year(date) - 2010) * 4 + quarter(date), # Ensure 'date_column' exists
        
        # B. Fix the Treatment Group (gname)
        #    We need to calculate the "Month Index" of the announcement.
        #    If announce_date is NA (control), set it to 0.
        treat_quarter_index = case_when(
            is.na(Announced) ~ 0,
            TRUE ~ (year(Announced) - 2010) * 4 + quarter(Announced)
        ),
        
        # C. ID and Cluster Variables
        id_numeric = as.numeric(as.factor(Zipcode)),
        cluster_id = as.numeric(as.factor(county)),
        log_price = log(value + 1)  # Log price (add 1 to avoid log(0))
    ) |> 
    select(
        id_numeric, cluster_id, quarter_index, treat_quarter_index,
        log_price, Zipcode, state,
        base_log_inc, base_log_pop, base_pct_own, base_pct_kids, county_prop_lds,
        base_pct_educ, base_year_built
    ) |> 
    # E. THE AGGREGATION STEP (Collapse 3 months -> 1 quarter)
    group_by(id_numeric, cluster_id, quarter_index, treat_quarter_index, Zipcode) |>
    summarize(
        # Average the 3 monthly prices into 1 quarterly price
        log_price = mean(log_price, na.rm = TRUE),
        
        # Keep the covariates (Grab the first value, as they don't change month-to-month)
        base_log_inc = first(base_log_inc), 
        base_log_pop = first(base_log_pop), 
        base_pct_own = first(base_pct_own), 
        base_pct_kids = first(base_pct_kids), 
        county_prop_lds = first(county_prop_lds),
        base_pct_educ = first(base_pct_educ),
        base_year_built = first(base_year_built),
        state = first(state),
        .groups = "drop"
    )

rm(data)
gc()

# 1. Identify your distinct treated clusters (Temples/Markets)
# Assuming 'treat_quarter_index > 0' implies treated
treated_zips <- temples |> 
    filter(Announced >= as.Date("2011-01-01")) |>
    pull(Zipcode)

# 2. Initialize a storage container
loo_results <- data.frame()

# 3. Loop through each temple, drop it, and re-run
# Note: This might take time due to bootstrapping. Reduce 'biters' if needed for speed testing.
for (excluded_id in treated_zips) {
    
    message(paste("Running model without cluster:", excluded_id))
    message(paste("Progress:", which(treated_zips == excluded_id), "of", length(treated_zips)))
    
    # A. Filter out the single temple/cluster
    loo_data <- quaterly_model_data %>% 
        filter(Zipcode != excluded_id)
    
    # B. Run the base att_gt model on subset
    out_loo <- did::att_gt(
        yname = "log_price",
        tname = "quarter_index",
        idname = "id_numeric",
        gname = "treat_quarter_index",
        xformla = ~base_log_inc + base_log_pop + base_pct_own + base_pct_kids + county_prop_lds + base_pct_educ + base_year_built + state,
        data = loo_data,
        control_group = "notyettreated",
        clustervars = "cluster_id",
        allow_unbalanced_panel = TRUE,
        est_method = "reg",
        bstrap = TRUE,      # Keep TRUE for valid CIs in the plot
        biters = 1000,      # Standard is 1000; reduce to 100 if debugging speed is an issue
        pl = TRUE,
        cores = 7,
        print_details = FALSE
    )
    
    # C. Aggregate to a simple overall ATT (best for forest plots)
    es_loo <- did::aggte(out_loo, type = "simple", na.rm = TRUE)
    
    # D. Store results
    loo_results <- rbind(loo_results, data.frame(
        Excluded_Cluster = as.character(excluded_id),
        ATT = es_loo$overall.att,
        SE = es_loo$overall.se,
        Lower_CI = es_loo$overall.att - (1.96 * es_loo$overall.se),
        Upper_CI = es_loo$overall.att + (1.96 * es_loo$overall.se)
    ))

    gc()
}

# 4. (Optional) Add the "Full Model" baseline for comparison
# Re-running your original model to get the 'simple' stats
loo_results <- rbind(loo_results, data.frame(
    Excluded_Cluster = "Full Model (Baseline)",
    ATT = -0.0261,
    SE = 0.0402,
    Lower_CI = -0.105,
    Upper_CI = 0.0528
))

# Create the Forest Plot
forest_plot <- ggplot(loo_results, aes(x = ATT, y = reorder(Excluded_Cluster, ATT))) +
    
    # 1. Add a vertical line at 0 (Null Effect)
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    
    # 2. Add a vertical line for your Baseline Estimate (to see deviations)
    geom_vline(aes(xintercept = ATT[Excluded_Cluster == "Full Model (Baseline)"]), 
                color = "blue", alpha = 0.3) +
    
    # 3. Point and Error Bars
    geom_pointrange(aes(xmin = Lower_CI, xmax = Upper_CI, 
                        color = (Excluded_Cluster == "Full Model (Baseline)"))) +
    
    # 4. Styling
    scale_color_manual(values = c("black", "red")) + # Highlights baseline in red
    labs(
        title = "Leave-One-Out Robustness Check",
        subtitle = "Does removing any single temple change the overall result?",
        x = "Average Treatment Effect on the Treated (ATT)",
        y = "Excluded Zipcode (Temple)",
        caption = "Mormon Metrics | Error bars represent 95% Confidence Intervals"
    ) +
    theme_minimal() +
    theme(
        legend.position = "none",
        plot.background = element_rect(fill = "white", color = NA), # Force white background in plot
        panel.background = element_rect(fill = "white", color = NA) # Force white background in panel
    )

ggsave("./images/loo_forest_plot.png", forest_plot, width = 12, height = 20, units = "in", dpi = 300, bg = "white")
