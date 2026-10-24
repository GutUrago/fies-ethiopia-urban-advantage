# ==============================================================================
# FIES ETHIOPIA ANALYSIS
# The Fragility of the Urban Advantage: Vulnerability to Food Insecurity under
# Systemic Shocks
# ==============================================================================
#
# FOLDER STRUCTURE
# Open "fies-ethiopia-urban-advantage.Rproj" (or set the working directory to the project
# root) so that here() resolves the paths below.
#
#   fies-ethiopia-urban-advantage/
#   |-- fies-ethiopia-urban-advantage.Rproj
#   |-- 1_scripts/
#   |   `-- fies_ethiopia_analysis.R    <- this script
#   |-- 2_data_raw/                     <- FAO FIES microdata, one folder per year
#   |   |-- ETH_2014_FIES_v01_EN_M_v01_A_OCS/
#   |   |   `-- microdata/ETH_2014_FIES_v01_EN_M_v01_A_OCS.RData
#   |   |-- ...                         (2015-2020 and 2022, same layout)
#   |   `-- ETH_2023_FIES_v01_EN_M_v01_A_ESS/
#   |       `-- microdata/ETH_2023_FIES_v01_EN_M_v01_A_ESS.RData
#   |-- 3_data_clean/
#   |   |-- data.rds                    <- cleaned pooled data (created)
#   |   |-- population.rds              <- World Bank population (downloaded)
#   |   `-- macro.rds                   <- World Bank GDP and CPI (downloaded)
#   `-- 4_outputs/                      <- tables and figures (created)
#
# REPRODUCING THE RESULTS
# - Raw data: FIES microdata for Ethiopia (2014-2020, 2022, 2023) from the FAO
#   Microdata Catalogue (https://microdata.fao.org). Only the .RData file in
#   each "microdata" subfolder is read; the year is taken from the file name.
# - Files in 3_data_clean are cached: if present, they are loaded instead of
#   rebuilt. Use the deposited population.rds and macro.rds to reproduce the
#   published figures exactly, as World Bank data are revised over time.
#   Delete data.rds to rebuild the clean data from the raw files.
# - Package versions used are recorded in sessionInfo.txt.
#
# OUTPUTS (4_outputs) AND WHERE THEY APPEAR IN THE PAPER
#   1_rasch_models.xlsx               Table 1, Supplementary Table 1
#   2_food_insecurity_by_groups.xlsx  Supplementary Table 2 (sheet "insecured")
#   3_food_insecurity_prevalence.png  Figure 1
#   4_macro_economic_trends.png       Figure 2
#   5_regression_models.xlsx          Table 2 ("insecured"), Supplementary
#                                     Table 3 ("vif"), Supplementary Table 4
#                                     ("or_insecured"), 2020 robustness checks
#   6_predicted_probability.png       Predicted probabilities (not in paper)
#
# CONTENTS
# 1. Setup, packages, and global settings
# 2. Data import, preparation, and cleaning
# 3. Rasch models and item statistics
# 4. Descriptive statistics by food security status
# 5. Food insecurity prevalence trends
# 6. Macroeconomic trends (GDP growth and inflation)
# 7. Regression models
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Setup, packages, and global settings
# ------------------------------------------------------------------------------

# Install (if missing) and load all packages
if (!require(pacman)) {
    install.packages("pacman")
}

pacman::p_load(
    here, # Project-relative file paths
    tidyverse, # Data manipulation and plotting
    sjlabelled, # Labelled survey data
    wbstats, # World Bank data (population, GDP, CPI)
    RM.weights, # Weighted Rasch models (FAO)
    collapse, # Fast weighted means
    Kendall, # Mann-Kendall trend test
    openxlsx2, # Excel export
    marginaleffects, # Predicted probabilities
    survey, # Survey-weighted models and tests
    scales, # Axis labels
    broom, # Tidy model output
    srvyr, # dplyr syntax for survey designs
    patchwork, # Combining plots
    ragg, # PNG device with custom fonts
    lme4, # Required by mice
    mice, # Imputation of missing income
    car, # Variance inflation factors
    conflicted # Function conflict resolution
)

# Resolve function name conflicts between packages
conflicts_prefer(dplyr::filter, dplyr::summarize, broom::tidy, dplyr::lag)


# Project directories
data_raw_dir <- here("2_data_raw")
data_clean_dir <- here("3_data_clean")
out_dir <- here("4_outputs")

# Cached data files
pop_rds <- here("3_data_clean/population.rds")
macro_rds <- here("3_data_clean/macro.rds")
data_rds <- here("3_data_clean/data.rds")

# Create output directory if it does not exist
if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
}

# FIES global reference thresholds (logits) applied to Raw_score_par
msfi <- -0.31 # Moderate or severe food insecurity
sfi <- 1.88 # Severe food insecurity

# Output file paths
rasch_xlsx <- file.path(out_dir, "1_rasch_models.xlsx")
fi_grp_xlsx <- file.path(out_dir, "2_food_insecurity_by_groups.xlsx")
prev_png <- file.path(out_dir, "3_food_insecurity_prevalence.png")
macro_png <- file.path(out_dir, "4_macro_economic_trends.png")
models_xlsx <- file.path(out_dir, "5_regression_models.xlsx")
models_png <- file.path(out_dir, "6_predicted_probability.png")

# ------------------------------------------------------------------------------
# 2. Data import, preparation, and cleaning
# ------------------------------------------------------------------------------

# Load the cached clean data if available; otherwise build it from raw files
if (file.exists(data_rds)) {
    data <- readRDS(data_rds)
} else {
    # Locate the .RData file of each survey year
    folders <- list.dirs(data_raw_dir)
    folders <- folders[grepl("microdata", folders)]
    datasets <- list.files(folders, "\\.RData$", full.names = TRUE)
    years <- str_extract(basename(datasets), "\\d{4}")

    # Read and stack all years into one data frame
    data_raw <- datasets |>
        set_names(years) |>
        imap(function(f, y) {
            # Load into a separate environment (each file holds
            # an object named "data")
            e <- new.env()
            load(f, envir = e)
            # Add survey year
            as_character(mutate(e[["data"]], year = y))
        }) |>
        bind_rows() |>
        # Restore variable types
        mutate(
            across(everything(), parse_guess),
            across(starts_with("N_"), parse_number)
        )

    # Cleaning and derived variables
    data <- data_raw |>
        # Household size = adults + children; missing when the
        # number of adults is missing or zero
        mutate(
            N_adults = if_else(N_adults == 0, NA_real_, N_adults),
            hh_size = if_else(
                is.na(N_adults),
                NA_real_,
                N_adults + coalesce(N_child, 0)
            )
        ) |>
        # Age groups
        mutate(
            Age_grp = case_when(
                Age >= 15 & Age <= 24 ~ "15-24",
                Age >= 25 & Age <= 34 ~ "25-34",
                Age >= 35 & Age <= 44 ~ "35-44",
                Age >= 45 & Age <= 54 ~ "45-54",
                Age >= 55 & Age <= 70 ~ "55-70",
                Age >= 71 ~ "71+"
            ),
            Age_grp = factor(
                Age_grp,
                levels = c(
                    "15-24",
                    "25-34",
                    "35-44",
                    "45-54",
                    "55-70",
                    "71+"
                )
            ),
            .after = Age
        ) |>
        # Harmonise education categories across years
        mutate(
            Education = case_when(
                Education %in%
                    c(
                        "09",
                        "10",
                        "10gread",
                        "11",
                        "12",
                        "12class",
                        "Gread10",
                        "Gread9"
                    ) ~ "Secondary",
                Education %in%
                    c(
                        "10+3",
                        "Diploma",
                        "Master's",
                        "Masters",
                        "College"
                    ) ~ "College",
                Education == "Secondary" ~ "Secondary",
                Education %in%
                    c(
                        "Dont_know",
                        "Refused"
                    ) ~ NA_character_,
                TRUE ~ "Elementary_or_less"
            ),
            Education = factor(
                Education,
                levels = c(
                    "Elementary_or_less",
                    "Secondary",
                    "College"
                )
            ),
            Secondary = if_else(
                Education %in% c("Secondary", "College"),
                1,
                0
            )
        ) |>
        # Harmonise area (urban/rural) across years
        mutate(
            Area = case_when(
                grepl("Urban", Area) ~ "Urban",
                grepl("Rural", Area) ~ "Rural",
                TRUE ~ Area
            ),
            Area = if_else(
                Area == "Dont_know",
                NA_character_,
                Area
            ),
            Area = factor(Area),
            Urban = if_else(Area == "Urban", 1, 0, NA),
            Rural = if_else(Area == "Rural", 1, 0, NA)
        ) |>
        # Gender indicators
        mutate(
            Female = if_else(Gender == "Female", 1, 0),
            Male = if_else(Gender == "Male", 1, 0),
            Gender = factor(Gender)
        ) |>
        # Food insecurity indicators
        # mod_sev, severe: FIES thresholds on the respondent parameter
        # insecured: at least one item affirmed (regression outcome)
        mutate(
            mod_sev = if_else(Raw_score_par >= msfi, 1, 0),
            severe = if_else(Raw_score_par >= sfi, 1, 0),
            secured = if_else(Raw_score == 0, 1, 0),
            insecured = if_else(Raw_score > 0, 1, 0)
        ) |>
        # Income quintiles (ordered)
        mutate(
            Income = factor(
                Income,
                levels = c(
                    "Poorest_20%",
                    "Second_20%",
                    "Middle_20%",
                    "Fourth_20%",
                    "Richest_20%"
                )
            )
        ) |>
        # Child weights for total-population prevalence (FAO guidance)
        mutate(wt_child = (wt / N_adults) * N_child) |>
        # Lowercase variable names
        rename_all(tolower)
    saveRDS(data, data_rds)
}

# National population by age (World Bank WDI); downloaded once, then cached
if (!file.exists(pop_rds)) {
    ids <- c(pop_total = "SP.POP.TOTL", pop_below_14 = "SP.POP.0014.TO")
    pop <- wbstats::wb_data(
        indicator = ids,
        country = "Ethiopia",
        start_date = 2014,
        end_date = 2023
    ) |>
        select(year = date, starts_with("pop_")) |>
        # Adult population (15+)
        mutate(
            pop_above_14 = pop_total - pop_below_14,
            .before = pop_total
        ) |>
        remove_all_labels()

    saveRDS(pop, pop_rds)
} else {
    pop <- readRDS(pop_rds)
}


# GDP growth and CPI inflation (World Bank WDI); downloaded once, then cached
if (!file.exists(macro_rds)) {
    macro_ids <- c(
        gdp_growth = "NY.GDP.MKTP.KD.ZG",
        inflation = "FP.CPI.TOTL.ZG"
    )

    macro <- wbstats::wb_data(
        indicator = macro_ids,
        country = "Ethiopia",
        start_date = 2014,
        end_date = 2023
    ) |>
        select(year = date, matches("inflation|gdp"))

    saveRDS(macro, macro_rds)
} else {
    macro <- readRDS(macro_rds)
}


# ------------------------------------------------------------------------------
# 3. Rasch models and item statistics
# ------------------------------------------------------------------------------

# FIES items
items <- c(
    "worried",
    "healthy",
    "fewfood",
    "skipped",
    "ateless",
    "runout",
    "hungry",
    "whlday"
)
unique_years <- unique(data$year)

# Weighted Rasch model for each survey year
rasch_models <- data |>
    group_by(year) |>
    group_split() |>
    set_names(unique_years) |>
    # Drop all-missing columns (FEWFOOD is not available in 2023)
    map(~ select(.x, where(~ any(!is.na(.))))) |>
    map(~ RM.w(select(.x, any_of(items)), .w = .x$wt))


# Rasch reliability by year
rasch_reliab <- imap_dfr(rasch_models, function(mod, yr) {
    tibble(
        year = yr,
        item = "rasch reliability",
        severity = mod[["reliab"]]
    )
})


# Item severity, standard error, infit, and outfit by year
rasch_stats <- imap_dfr(rasch_models, function(mod, yr) {
    tibble(
        year = yr,
        item = names(mod[["b"]]),
        severity = mod[["b"]],
        se = mod[["se.b"]],
        infit = mod[["infit"]],
        outfit = mod[["outfit"]]
    )
}) |>
    bind_rows(rasch_reliab) |>
    pivot_wider(
        id_cols = item,
        names_from = year,
        values_from = c(severity, se, infit, outfit),
        names_glue = "{.value}_{year}"
    ) |>
    mutate(item = str_to_upper(item))

# Reorder columns so each year forms one block
rasch_stats <- map(
    as.character(unique_years),
    ~ select(rasch_stats, item, ends_with(.x))
) |>
    reduce(full_join, by = "item") |>
    mutate(across(where(is.numeric), ~ sprintf("%.2f", .x)))


# Reliability table (wide)
reliability <- rasch_reliab |>
    pivot_wider(names_from = year, values_from = severity)


# Excel helper: add a table as a new sheet, auto-fit columns, blank for NA
options("openxlsx2.na" = '')
wb_write_table <- function(wb, x, nm) {
    wb_add_worksheet(wb, nm) |>
        wb_add_data_table(x = x) |>
        wb_set_col_widths(cols = 1:ncol(x), widths = 'auto')
}


# Export Rasch results
wb_workbook() |>
    wb_write_table(rasch_stats, "rasch stats") |>
    wb_write_table(reliability, "reliability") |>
    wb_save(rasch_xlsx)


# ------------------------------------------------------------------------------
# 4. Descriptive statistics by food security status
# ------------------------------------------------------------------------------

# summary_table(): by year and food security status, returns weighted means
# (numeric variables) or counts and row percentages (categorical variables),
# with a design-based test statistic for each year

summary_table <- function(data, x_var, by_var) {
    # 1. Keep complete cases and label the grouping variable
    df_clean <- data |>
        filter(
            !is.na(.data[[x_var]]),
            !is.na(.data[[by_var]]),
            !is.na(wt)
        ) |>
        mutate(
            by_factor = factor(
                if_else(
                    .data[[by_var]] == 1,
                    "Food insecure",
                    "Food secure"
                ),
                levels = c("Food secure", "Food insecure")
            ),
            year = as.character(year)
        )

    is_num <- is.numeric(df_clean[[x_var]])

    # 2. Tests by year: t-test (numeric) or Rao-Scott chi-square (categorical)
    stats_results <- df_clean |>
        nest(.by = year) |>
        mutate(
            stat_label = map_chr(data, \(df_sub) {
                design <- df_sub |>
                    as_survey_design(weights = wt)

                if (is_num) {
                    formula_str <- as.formula(paste(
                        x_var,
                        "~ by_factor"
                    ))
                    res <- svyttest(
                        formula_str,
                        design = design
                    )
                    val <- res$statistic
                    pval <- res$p.value
                } else {
                    formula_str <- as.formula(paste(
                        "~",
                        x_var,
                        "+ by_factor"
                    ))
                    res <- svychisq(
                        formula_str,
                        design = design
                    )
                    val <- res$statistic
                    pval <- res$p.value
                }

                stars <- case_when(
                    pval <= 0.01 ~ "***",
                    pval <= 0.05 ~ "**",
                    pval <= 0.1 ~ "*",
                    TRUE ~ ""
                )
                sprintf("%.02f%s", val, stars)
            })
        ) |>
        select(year, stat_label)

    # 3. Cell values
    if (is_num) {
        summary_values <- df_clean |>
            as_survey_design(weights = wt) |>
            group_by(year, by_factor) |>
            summarize(
                val_calc = survey_mean(
                    .data[[x_var]],
                    na.rm = TRUE
                ),
                .groups = "drop"
            ) |>
            mutate(
                val = sprintf("%.02f", val_calc),
                level = x_var
            )
    } else {
        summary_values <- df_clean |>
            group_by(year, level = .data[[x_var]], by_factor) |>
            summarize(n = n(), .groups = "drop_last") |>
            mutate(
                pct = (n / sum(n)) * 100,
                val = sprintf("%d (%.0f%%)", n, pct)
            ) |>
            ungroup()
    }

    # 4. Reshape to one column per year and status
    table_final <- summary_values |>
        select(year, by_factor, level, val) |>
        pivot_wider(
            names_from = c(year, by_factor),
            values_from = val
        ) |>
        cross_join(
            stats_results |>
                pivot_wider(
                    names_from = year,
                    values_from = stat_label,
                    names_glue = "{year}_stats"
                )
        ) |>
        mutate(Variable = x_var, .before = 1)

    return(table_final)
}


# Sample size by year and food security status
count_n <- function(data, by) {
    data |>
        mutate(
            by = .data[[by]],
            by = if_else(by == 1, "Food insecure", "Food secure")
        ) |>
        filter(!is.na(.data[[by]])) |>
        count(year, by) |>
        mutate(total = fsum(n), .by = year) |>
        mutate(N = sprintf("%s (%.0f%%)", n, (n / total) * 100)) |>
        select(year, by, N) |>
        pivot_wider(
            names_from = c(year, by),
            values_from = N
        ) |>
        mutate(Variable = "N")
}

# Variables and column order
grp_vars <- c("area", "age_grp", "gender", "hh_size", "education", "income")
col_order <- paste0(
    rep(2014:2023, each = 3),
    c("_Food insecure", "_Food secure", "_stats")
)

# By any food insecurity (Supplementary Table 2)
insecured_table <- map_dfr(grp_vars, ~ summary_table(data, .x, "insecured")) |>
    bind_rows(count_n(data, "insecured")) |>
    select(Variable, level, any_of(col_order))

# By moderate or severe food insecurity (not reported in the paper)
moderate_table <- map_dfr(grp_vars, ~ summary_table(data, .x, "mod_sev")) |>
    bind_rows(count_n(data, "mod_sev")) |>
    select(Variable, level, any_of(col_order))


# Export descriptive tables
wb_workbook() |>
    wb_write_table(insecured_table, "insecured") |>
    wb_write_table(moderate_table, "moderate") |>
    wb_save(fi_grp_xlsx)


# ------------------------------------------------------------------------------
# 5. Food insecurity prevalence trends
# ------------------------------------------------------------------------------

# kendall_test(): Mann-Kendall trend test; returns tau with significance stars
kendall_test <- function(x) {
    res <- Kendall::MannKendall(x)
    tau <- sprintf("%.02f", res$tau)
    p <- res$sl
    if (p <= 0.01) {
        paste0(tau, "***")
    } else if (p <= 0.05) {
        paste0(tau, "**")
    } else if (p <= 0.1) {
        paste0(tau, "*")
    } else {
        tau
    }
}

# Weighted prevalence from the FAO probabilities (prob_sev, prob_mod_sev):
# survey weight for adults, child weight for children
prev_tbl <- data |>
    group_by(year) |>
    summarize(
        severe_adult = fmean(prob_sev, w = wt),
        severe_urban = fmean(prob_sev[urban == 1], w = wt[urban == 1]),
        severe_rural = fmean(prob_sev[rural == 1], w = wt[rural == 1]),
        severe_child = fmean(prob_sev, w = wt_child),
        moderate_adult = fmean(prob_mod_sev, w = wt),
        moderate_urban = fmean(
            prob_mod_sev[urban == 1],
            w = wt[urban == 1]
        ),
        moderate_rural = fmean(
            prob_mod_sev[rural == 1],
            w = wt[rural == 1]
        ),
        moderate_child = fmean(prob_mod_sev, w = wt_child),
        .groups = "drop"
    ) |>
    left_join(pop, by = "year") |>
    # Total population prevalence: adult and child prevalence weighted by
    # population size (FAO, Derived variables and computation of indicators)
    mutate(
        severe_total = ((severe_adult * pop_above_14) +
            (severe_child * pop_below_14)) /
            pop_total,
        moderate_total = ((moderate_adult * pop_above_14) +
            (moderate_child * pop_below_14)) /
            pop_total
    ) |>
    select(
        Year = year,
        Severe_Total = severe_total,
        Severe_Adult = severe_adult,
        Severe_Urban = severe_urban,
        Severe_Rural = severe_rural,
        Moderate_Total = moderate_total,
        Moderate_Adult = moderate_adult,
        Moderate_Urban = moderate_urban,
        Moderate_Rural = moderate_rural
    ) |>
    arrange(Year)

# Long format for plotting
prev_plt_df <- prev_tbl |>
    pivot_longer(!Year) |>
    separate_wider_delim(
        name,
        delim = "_",
        names = c("Level", "Population")
    ) |>
    mutate(
        Level = if_else(
            Level == "Moderate",
            "Moderate or Severe FI",
            "Severe FI"
        ),
        Population = factor(
            Population,
            levels = c("Total", "Adult", "Urban", "Rural")
        )
    )

# Trend test for each series, placed as a label at the last year
prev_tau <- map_df(prev_tbl, kendall_test) |>
    mutate(Year = max(prev_tbl$Year)) |>
    pivot_longer(!Year, values_to = "Label") |>
    separate_wider_delim(
        name,
        delim = "_",
        names = c("Level", "Population")
    ) |>
    mutate(
        Level = if_else(
            Level == "Moderate",
            "Moderate or Severe FI",
            "Severe FI"
        ),
        Label = sprintf("τ = %s", Label)
    ) |>
    left_join(prev_plt_df, by = c("Year", "Level", "Population"))


# Prevalence trends (Figure 1)
prevalence_plt <- ggplot(
    data = prev_plt_df,
    mapping = aes(
        x = Year,
        y = value,
        color = Population,
        shape = Population
    )
) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 1) +
    geom_text(
        data = prev_tau,
        aes(label = Label),
        size = 2.3,
        hjust = -0.2,
        show.legend = FALSE
    ) +
    scale_color_manual(
        values = c(
            "Total" = "#333333",
            "Adult" = "#1f77b4",
            "Rural" = "#2ca02c",
            "Urban" = "#e6550d"
        )
    ) +
    scale_x_continuous(
        breaks = seq(2014, 2023, by = 2),
        expand = expansion(mult = c(0.05, 0.2))
    ) +
    scale_y_continuous(
        breaks = seq(0, 1, by = 0.05),
        labels = label_percent(suffix = ""),
        expand = expansion(mult = c(0.15, 0.2))
    ) +
    labs(
        y = "Prevalence (%)",
        color = "Population",
        shape = "Population"
    ) +
    facet_wrap(~Level, scales = "free") +
    theme_bw() +
    theme(
        legend.position = "bottom",
        panel.grid.major = element_line(
            colour = "grey",
            linetype = 2,
            linewidth = 0.2
        ),
        strip.text = element_text(face = "bold"),
        text = element_text(family = "Segoe UI")
    )

ggsave(
    filename = prev_png,
    plot = prevalence_plt,
    width = 9,
    height = 5,
    dpi = 600,
    device = ragg::agg_png
)


# ------------------------------------------------------------------------------
# 6. Macroeconomic trends (GDP growth and inflation)
# ------------------------------------------------------------------------------

# GDP growth and inflation (Figure 2)
macro_plt <- macro |>
    pivot_longer(!year) |>
    mutate(
        name = case_when(
            name == "gdp_growth" ~ "GDP Growth",
            name == "inflation" ~ "Inflation (CPI, annual %)",
            TRUE ~ ""
        )
    ) |>
    ggplot(aes(x = year, y = value, color = name, shape = name)) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 1) +
    scale_color_manual(
        values = c(
            "GDP Growth" = "#1f77b4",
            "Inflation (CPI, annual %)" = "#e6550d"
        )
    ) +
    scale_y_continuous(
        breaks = seq(0, 100, by = 5),
        expand = expansion(mult = c(0.15, 0.2))
    ) +
    scale_x_continuous(
        breaks = seq(2014, 2023, by = 2)
    ) +
    labs(
        x = "Year",
        y = "Rate (%)",
        color = "",
        shape = ""
    ) +
    theme_bw() +
    theme(
        legend.position = "bottom",
        panel.grid.major = element_line(
            colour = "grey",
            linetype = 2,
            linewidth = 0.2
        ),
        strip.text = element_text(face = "bold"),
        text = element_text(family = "Segoe UI")
    )

ggsave(
    filename = macro_png,
    plot = macro_plt,
    width = 9,
    height = 5,
    dpi = 600,
    device = ragg::agg_png
)


# ------------------------------------------------------------------------------
# 7. Regression models
# ------------------------------------------------------------------------------

# Impute missing income quintiles (ordered logit, single imputation); used in
# Model 3 only
meth <- c("income" = "polr")
forms <- list(
    income = income ~ worried +
        healthy +
        fewfood +
        skipped +
        ateless +
        runout +
        hungry +
        whlday +
        year +
        n_adults +
        n_child +
        age_grp +
        education +
        area +
        gender
)
imp <- mice(data, method = meth, formulas = forms, m = 1, seed = 123)
data$income_imp <- complete(imp)$income


# Survey design: weights only (no strata or PSU in the public data)
design <- svydesign(
    ids = ~1,
    weights = ~wt,
    data = mutate(data, year = factor(year))
)

# Base specification (Model 2)
formula <- insecured ~ area +
    age_grp +
    gender +
    hh_size +
    education +
    year +
    area * year

# Fit the five specifications (logit, quasibinomial):
#   Model 1: area, year, and area-by-year interactions
#   Model 2: Model 1 + age group, gender, household size, education
#   Model 3: Model 2 + income quintile (imputed where missing)
#   Model 4: Model 2 + income quintile (observed only)
#   Model 5: Model 2 excluding 2020 (phone survey)
fit_models <- function(y) {
    f <- update(formula, as.formula(paste(y, "~ .")))
    list(
        model_1 = svyglm(
            update(f, . ~ area * year + area + year),
            design = design,
            family = quasibinomial()
        ),
        model_2 = svyglm(
            update(f, . ~ .),
            design = design,
            family = quasibinomial()
        ),
        model_3 = svyglm(
            update(f, . ~ . + income),
            design = update(design, income = income_imp),
            family = quasibinomial()
        ),
        model_4 = svyglm(
            update(f, . ~ . + income),
            design = design,
            family = quasibinomial()
        ),
        model_5 = svyglm(
            update(f, . ~ .),
            subset = year != 2020,
            design = design,
            family = quasibinomial()
        )
    )
}

models <- fit_models("insecured")

# make_table(): coefficients as log-odds (SE) with stars (*** p<=0.01,
# ** p<=0.05, * p<=0.1), followed by N, Wald F, Nagelkerke pseudo-R2, AIC,
# and the joint test of the area-by-year terms (Table 2)
make_table <- function(models, joint = ~ area:year) {
    models_coef <- imap_dfr(
        models,
        ~ {
            tidy(.x) |>
                mutate(model = .y)
        }
    ) |>
        mutate(
            OR = sprintf("%.02f", exp(estimate)),
            star = case_when(
                p.value <= 0.01 ~ "***",
                p.value <= 0.05 ~ "**",
                p.value <= 0.1 ~ "*",
                TRUE ~ ""
            ),
            estimate = sprintf(
                "%.02f%s (%.02f)",
                round(estimate, 2) + 0,
                star,
                std.error
            )
        ) |>
        select(term, estimate, model) |>
        pivot_wider(
            id_cols = term,
            values_from = estimate,
            names_from = model
        )

    # Number of observations
    models_nobs <- imap_dfr(models, ~ as.character(nobs(.x))) |>
        mutate(term = "Observations")

    # Wald F test of all model terms
    models_wald <- imap_dfr(
        models,
        ~ {
            res <- regTermTest(
                .x,
                update(.x[["formula"]], NULL ~ .)
            )
            tibble(
                name = .y,
                value = as.numeric(res$Ftest),
                p = as.numeric(res$p)
            )
        }
    ) |>
        mutate(
            star = case_when(
                p <= 0.01 ~ "***",
                p <= 0.05 ~ "**",
                p <= 0.1 ~ "*",
                TRUE ~ ""
            ),
            value = sprintf("%.02f%s", value, star),
            term = "Wald F"
        ) |>
        select(term, name, value) |>
        pivot_wider(id_cols = term)

    # Pseudo-R2, AIC, and joint test of the interaction terms
    models_fit <- imap_dfr(
        models,
        ~ {
            r <- regTermTest(.x, joint)
            tibble(
                name = .y,
                `Nagelkerke pseudo-R2` = sprintf(
                    "%.3f",
                    psrsq(.x, method = "Nagelkerke")
                ),
                AIC = sprintf("%.0f", AIC(.x)[["AIC"]]),
                `Joint test of urban x year terms (F)` = sprintf(
                    "%.2f%s",
                    as.numeric(r$Ftest),
                    case_when(
                        as.numeric(r$p) <= 0.01 ~ "***",
                        as.numeric(r$p) <= 0.05 ~ "**",
                        as.numeric(r$p) <= 0.1 ~ "*",
                        TRUE ~ ""
                    )
                )
            )
        }
    ) |>
        pivot_longer(-name, names_to = "term") |>
        pivot_wider(id_cols = term)

    models_gof <- bind_rows(models_nobs, models_wald, models_fit)

    bind_rows(models_coef, models_gof) |>
        select(term, all_of(names(models)))
}

# make_or(): odds ratios with 95% CI (Supplementary Table 4)
make_or <- function(models) {
    imap_dfr(
        models,
        ~ tidy(.x, conf.int = TRUE, exponentiate = TRUE) |>
            mutate(model = .y)
    ) |>
        mutate(
            or = sprintf(
                "%.2f [%.2f, %.2f]",
                estimate,
                conf.low,
                conf.high
            )
        ) |>
        select(term, model, or) |>
        pivot_wider(names_from = model, values_from = or)
}

# 2020 robustness check: Model 2 among respondents with secondary education
# or higher, the group over-represented in the 2020 phone survey
models_secondary <- list(
    insecured_secondary = svyglm(
        formula,
        design = subset(design, secondary == 1),
        family = quasibinomial()
    )
)

# Weighted sample composition by year (2020 phone survey check)
comp_2020 <- svyby(
    ~ urban + secondary + hh_size + age,
    ~year,
    design,
    svymean,
    na.rm = TRUE
)

# Multicollinearity: GVIF of the main-effects model with income
# (Supplementary Table 3)
vif_tbl <- car::vif(
    svyglm(
        insecured ~ area +
            year +
            age_grp +
            gender +
            hh_size +
            education +
            income,
        design = design,
        family = quasibinomial()
    )
) |>
    as.data.frame() |>
    rownames_to_column("term")

# Export regression results
tables <- list(
    insecured = make_table(models),
    or_insecured = make_or(models),
    secondary_check = make_table(models_secondary),
    composition = as.data.frame(comp_2020),
    vif = vif_tbl
)

wb <- wb_workbook()
for (nm in names(tables)) {
    wb <- wb_write_table(wb, tables[[nm]], nm)
}
wb_save(wb, models_xlsx)


# Predicted probability of food insecurity by area and year, one panel per
# model
predict_plts <- imap(
    models,
    ~ {
        pred_data <- plot_predictions(
            .x,
            by = c("year", "area"),
            wts = weights(.x[["survey.design"]]),
            draw = FALSE
        )

        pred_data$year <- as.numeric(as.character(pred_data$year))

        ggplot(
            pred_data,
            aes(
                x = year,
                y = estimate,
                ymin = conf.low,
                ymax = conf.high,
                color = area
            )
        ) +
            geom_pointrange(
                size = 0.5,
                position = position_dodge(width = 0.3)
            ) +
            scale_color_manual(
                values = c(
                    "Rural" = "#2ca02c",
                    "Urban" = "#e6550d"
                )
            ) +
            scale_x_continuous(
                breaks = seq(from = 2014, to = 2023, by = 2)
            ) +
            labs(
                title = paste("Model: ", parse_number(.y)),
                y = "Predicted Probability",
                x = "Year",
                color = "Area"
            ) +
            theme_bw() +
            theme(legend.position = "none")
    }
)

# Combine panels
models_plot <- wrap_plots(predict_plts, ncol = 3) +
    plot_layout(
        guides = "collect",
        axis_titles = "collect"
    ) &
    theme(
        legend.position = "bottom",
        panel.grid.major = element_line(
            colour = "grey",
            linetype = 2,
            linewidth = 0.2
        ),
        strip.text = element_text(face = "bold"),
        text = element_text(family = "Segoe UI")
    )


ggsave(
    filename = models_png,
    plot = models_plot,
    width = 9,
    height = 5,
    dpi = 600,
    device = ragg::agg_png
)

# Record R and package versions
writeLines(capture.output(sessionInfo()), file.path(out_dir, "sessionInfo.txt"))
