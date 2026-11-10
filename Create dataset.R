#For windows
setwd("Output directory")
data_dir <- path.expand("Data directory")

# load packages
packages <- c("haven", "dplyr", "expss", "psych", "tidyr", "readxl", "labelled")
#install.packages(packages, dependencies = TRUE)
lapply(packages, require, character.only = TRUE)

#==============================================================================#
# Import and merge datasets ####
#==============================================================================#
ha_o <- read_dta(file.path(data_dir,"H_CHARLS_D_Data.dta"))

## wave 5 ####
wave5 <- read_dta(file.path(data_dir, "Sample_Infor.dta"))
# Helper function to safely full_join and coalesce overlapping columns
safe_full_join <- function(x, y, by) {
  out <- full_join(x, y, by = by, suffix = c("", ".new"))
  
  dup_vars <- grep("\\.new$", names(out), value = TRUE)
  
  for (v in dup_vars) {
    base <- sub("\\.new$", "", v)
    
    # Align classes
    if (!is.null(out[[base]])) {
      if (class(out[[base]])[1] != class(out[[v]])[1]) {
        out[[base]] <- as.character(out[[base]])
        out[[v]]    <- as.character(out[[v]])
      }
    }
    out[[base]] <- coalesce(out[[base]], out[[v]])
    out[[v]] <- NULL
  }
  out
}

# List of files to merge
files <- c(
  "Demographic_Background.dta",
  "Work_Retirement.dta",
  "Weights.dta",
  "Individual_Income.dta",
  "Health_Status_and_Functioning.dta",
  "Exit_Module.dta",
  "COVID_Module.dta",
  "Family_Information.dta",
  "Household_Income.dta"
)

# Initial dataset
wave5 <- read_dta(file.path(data_dir, "Sample_Infor.dta"))

# Loop to merge all files
for (f in files) {
  dat <- read_dta(file.path(data_dir, f))
  # Decide join keys depending on file
  if (f %in% c("Family_Information.dta", "Household_Income.dta")) {
    join_keys <- c("householdID")
  } else {
    join_keys <- c("ID")
  }
  wave5 <- safe_full_join(wave5, dat, by = join_keys)
}
names(wave5)[grepl("\\.(x|y)$", names(wave5))]

## psu ####
psu <- read_dta(file.path(data_dir,"2011_#psu.dta"))
psu <- psu %>% 
  full_join(read_dta(file.path(data_dir, "2013_#psu.dta")) %>% 
              select(-versionID), 
            by = c("communityID", "province", "city", "urban_nbs", "areatype"))
anyDuplicated(psu$communityID)

## sub datasets: 2011-2018 ####
a2011_D_Health_status_and_functioning <- read_dta(file.path(data_dir,"2011_D_Health_status_and_functioning.dta"))
a2013_D_Health_status_and_functioning <- read_dta(file.path(data_dir,"2013_D_Health_status_and_functioning.dta"))
a2015_D_Health_status_and_functioning <- read_dta(file.path(data_dir,"2015_D_health_status_and_functioning.dta"))
a2018_D_Health_status_and_functioning <- read_dta(file.path(data_dir,"2018_D_Health_Status_and_Functioning.dta"))

a2011_E_Health_care_and_insurance <- read_dta(file.path(data_dir,"2011_E_Health_care_and_insurance.dta"))
a2013_E_Health_care_and_insurance <- read_dta(file.path(data_dir,"2013_E_Health_care_and_insurance.dta"))
a2015_E_Health_care_and_insurance <- read_dta(file.path(data_dir,"2015_E_health_care_and_insurance.dta"))
a2018_E_Health_care_and_insurance <- read_dta(file.path(data_dir,"2018_E_Health_Care_and_Insurance.dta"))

## merge ####
ha <- ha_o %>%
  full_join(wave5, by = c("ID", "householdID", "communityID")) %>%
  mutate(
    merge1 = case_when(
      !is.na(ID) & ID %in% ha_o$ID & ID %in% wave5$ID ~ "matched",
      !is.na(ID) & ID %in% ha_o$ID & !(ID %in% wave5$ID) ~ "only_ha_o",
      !is.na(ID) & !(ID %in% ha_o$ID) & ID %in% wave5$ID ~ "only_wave5"
    )
  ) %>% 
  full_join(psu, by = "communityID")
names(ha)[grepl("\\.(x|y)$", names(ha))]
table(ha$merge1)
nrow(ha) # n = 25,882

# delete used datasets
rm(ha_o, wave5, psu, dat, packages)
gc()

#==============================================================================#
# Create variables 
#==============================================================================#

# Birth year ####
sum(is.na(ha$rabyear) & !is.na(ha$ba003_1)) # n = 143
sum(is.na(ha$rabyear) & !is.na(ha$zrbirthyear)) # n = 238
ha <- ha %>% 
  mutate(
    birth_year = coalesce(rabyear, ba003_1, zrbirthyear)
  )
describe(ha[, c("rabyear", "birth_year")], fast = TRUE)

# Sex ####
sum(is.na(ha$ragender) & !is.na(ha$xrgender)) # n = 288
sum(is.na(ha$ragender) & !is.na(ha$ba001)) # n = 288
ha <- ha %>%
  mutate(
    ragender_num = as.double(ragender),  # strip labels safely
    sex = coalesce(ragender_num, xrgender),
    sex = factor(sex, levels = c(2, 1), labels = c("Female", "Male"))
  ) %>%
  select(-ragender_num)
table(ha$sex, useNA = "always") # missing = 16  

# Education ####
ha <- ha %>%
  mutate(
    zredu_num = case_when(
      !is.na(zredu) ~ zredu,
      !is.na(ba010) ~ ba010,
      is.na(zredu) & is.na(ba010) & ba010_1 == 2 ~ 1
    )
  )
table(ha$zredu, useNA = "always")
table(ha$zredu_num, useNA = "always")
ha <- ha %>%
  mutate(
    educ = coalesce(raeduc_c, zredu_num),
    educ = factor(
      educ,
      levels = 1:11,
      labels = c(
        "Illiterate",
        "Didn't finish primary school but can read",
        "Sishu",
        "Elementary school",
        "Middle school",
        "High school",
        "Vocational school",
        "Two/three-year college",
        "College graduate",
        "Postgraduate degree",
        "PhD"
      )
    )
  )
table(ha$educ, useNA = "always") # missing = 41

ha <- ha %>%
  mutate(
    educl = raeducl,
    edu_numeric = as.numeric(educ),  # get numeric codes from factor
    educl = case_when(
      is.na(educl) & edu_numeric %in% 1:5  ~ 1,
      is.na(educl) & edu_numeric %in% 6:7  ~ 2,
      is.na(educl) & edu_numeric %in% 8:10 ~ 3,
      TRUE ~ educl
    ),
    educl = factor(
      educl,
      levels = 1:3,
      labels = c(
        "Less than lower secondary education",
        "Upper secondary & vocational training",
        "Tertiary education"
      )
    )
  ) %>%
  select(-edu_numeric)
table(ha$educl, useNA = "always") # missing = 14

# Marital status ####
sum(ha$r1mnev == 1 & (ha$r1mstat != 8 | ha$r1mstath != 8), na.rm = TRUE)
subset(ha, r2mnev == 1 & (r2mstat != 8 | r2mstath != 8),
       select = c(r2mstat, r2mstath, r2mnev))
sum(ha$r3mnev == 1 & (ha$r3mstat != 8 | ha$r3mstath != 8), na.rm = TRUE)
subset(ha, r4mnev == 1 & (r4mstat != 8 | r4mstath != 8),
       select = c(r4mstat, r4mstath, r4mnev))
## create raw 6 categories of marital status
ha <- ha %>%
  mutate(
    maritalc2011 = case_when(
      !is.na(r1mstat) ~ r1mstat,
      !is.na(r1mstath) ~ r1mstath,
      r1mnev == 1 ~ 8,
      TRUE ~ NA_real_
    ),
    maritalc2013 = case_when(
      !is.na(r2mstat) ~ r2mstat,
      !is.na(r2mstath) ~ r2mstath,
      r2mnev == 1 ~ 8,
      TRUE ~ NA_real_
    ),
    maritalc2015 = case_when(
      !is.na(r3mstat) ~ r3mstat,
      !is.na(r3mstath) ~ r3mstath,
      r3mnev == 1 ~ 8,
      TRUE ~ NA_real_
    ),
    maritalc2018 = case_when(
      !is.na(r4mstat) ~ r4mstat,
      !is.na(r4mstath) ~ r4mstath,
      r4mnev == 1 ~ 8,
      TRUE ~ NA_real_
    ),
    maritalc2020 = case_when(
      ba011 == 1 ~ 1,  # Married, spouse present
      ba011 == 2 ~ 3,  # Married, spouse absent
      ba011 == 3 ~ 4,  # Separated
      ba011 == 4 ~ 5,  # Divorced
      ba011 == 5 ~ 7,  # Widowed
      ba011 == 6 ~ 8,  # Never married
      TRUE ~ NA_real_
    )
  )

labels <- c(
  "Married, spouse present" = 1,
  "Married, spouse absent" = 3,
  "Separated" = 4,
  "Divorced" = 5,
  "Widowed" = 7,
  "Never married" = 8
)
vars <- paste0("maritalc20", c("11", "13", "15", "18", "20"))
ha <- ha %>%
  mutate(across(all_of(vars), ~ factor(., levels = labels, labels = names(labels))))


# Household size ####
ha <- ha %>%
  mutate(
    hhsize2011 = as.numeric(h1hhres),
    hhsize2013 = as.numeric(h2hhres),
    hhsize2015 = as.numeric(h3hhres),
    hhsize2018 = as.numeric(h4hhres),
    spouse2020 = case_when(
      maritalc2020 == "Married, spouse present" ~ 2,
      !is.na(maritalc2020) ~ 1,
      TRUE ~ NA_real_
    ),
    hhsize2020 = case_when(
      is.na(xhhmembernum) & is.na(spouse2020) ~ NA_real_,
      TRUE ~ rowSums(cbind(xhhmembernum, spouse2020), na.rm = TRUE)
    )
  )
table(ha$hhsize2020) # 0 means the respondent died

# Income, wealth & consumption ####
## wealth. Better No use due to large missing and negative values ####
#Note. Wave 1&2 = household wealth. Wave 3&4 = wealth at the couple level
ha <- ha %>%
  mutate(
    fam_wealth2011 = as.numeric(hh1atotb),
    fam_wealth2013 = as.numeric(hh2atotb),
    fam_wealth2015 = as.numeric(h3atotb),
    fam_wealth2018 = as.numeric(h4atotb),
    # personal wealth for 2011 and 2013
    pcwealth2011 = fam_wealth2011 / hhsize2011,
    pcwealth2013 = fam_wealth2013 / hhsize2013,
    # personal wealth for 2015 and 2018, adjusted by marital status
    pcwealth2015 = case_when(
      maritalc2015 %in% c("Married, spouse present", "Married, spouse absent") ~ fam_wealth2015 / 2,
      maritalc2015 %in% c("Separated", "Divorced", "Widowed", "Never married") ~ fam_wealth2015,
      TRUE ~ NA_real_
    ),
    pcwealth2018 = case_when(
      maritalc2018 %in% c("Married, spouse present", "Married, spouse absent") ~ fam_wealth2018 / 2,
      maritalc2018 %in% c("Separated", "Divorced", "Widowed", "Never married") ~ fam_wealth2018,
      TRUE ~ NA_real_
    )
  )
describe(ha[, c("hhsize2015", "fam_wealth2015", "pcwealth2015",
                "hhsize2018", "fam_wealth2018", "pcwealth2018")], fast = TRUE)

## household income ####
ha <- ha %>% 
  mutate(
    hhincome2011 = as.numeric(hh1itot),
    hhincome2013 = as.numeric(hh2itot),
    hhincome2015 = as.numeric(hh3itot),
    hhincome2018 = as.numeric(hh4itot)
  )

# 2020
## individual income
replace_with_mean <- function(x, minx, maxx) {
  case_when(
    !is.na(x) & x != -1 ~ x, 
    TRUE ~ rowMeans(
      cbind(
        ifelse(minx == -1, NA, minx),
        ifelse(maxx == -1, NA, maxx)
      ), 
      na.rm = TRUE
    )
  )
}

ha$ga002 <- replace_with_mean(ha$ga002, ha$ga002_min, ha$ga002_max)

ha$ga005_1 <- replace_with_mean(ha$ga005_1, ha$ga005_1_min, ha$ga005_1_max)
ha$ga005_1 <- case_when(
  !is.na(ha$ga005_1)                    ~ ha$ga005_1,
  ha$ga005_s1 == 0 | ha$ga005_s10 == 0  ~ 0,
  TRUE                                  ~ NA_real_
)

vars <- paste0("ga005_", 2:9)
for (v in vars) {
  s_col <- paste0(v, "_s")  # corresponding _s column
  ha[[v]] <- ifelse(
    !is.na(ha[[v]]) & ha[[v]] != -1,          # keep if not missing and not -1
    ha[[v]],
    ifelse(ha[[s_col]] == 0 | ha$ga005_s10 == 0, 0, NA_real_)  # check _s column and constant s10
  )
}
bef_vars <- paste0("ga005_", 1:9)
ha <- ha %>%
  mutate(
    inbef = if_else(
      rowSums(!is.na(select(., all_of(bef_vars)))) == 0,  # all missing?
      NA_real_,                                           # then NA
      rowSums(select(., all_of(bef_vars)), na.rm = TRUE) # else sum ignoring missing
    )
  )
describe(ha[, c("inbef", "ga005_1", "ga005_2", "ga005_3", "ga005_4", "ga005_5",
                "ga005_6", "ga005_7", "ga005_8")], fast = TRUE)

## individual earned income
ha <- ha %>%
  mutate(across(c(fc033:fc039, -fc035),
                ~ replace_with_mean(.x,
                                    get(paste0(cur_column(), "_min")),
                                    get(paste0(cur_column(), "_max")))))
describe(ha[, c("fc033", "fc034", "fc035", "fc036", "fc037", "fc038", "fc039")], fast = TRUE)
ha$inearned <- case_when(
  !is.na(ha$fc037) ~ ha$fc037 * 40 * 52, # No. of hours/week * No. of weeks/year
  !is.na(ha$fc036) ~ ha$fc036 * 5 * 52, # No. of days/week * No. of weeks.year
  !is.na(ha$fc035) ~ ha$fc035 * 52,
  !is.na(ha$fc034) ~ ha$fc034 * 12,
  !is.na(ha$fc033) ~ ha$fc033,
  TRUE             ~ NA_real_
)
ha <- ha %>%
  mutate(
    inearned = case_when(
      is.na(inearned) & is.na(fc039) ~ NA_real_,
      TRUE ~ rowSums(cbind(inearned, fc039), na.rm = TRUE)
    )
  )


ha <- ha %>%
  mutate(across(c(fc042_1:fc042_4),
                ~ replace_with_mean(.x,
                                    get(paste0(cur_column(), "_min")),
                                    get(paste0(cur_column(), "_max")))))
ha <- ha %>%
  mutate(
    fc042_1 = coalesce(fc042_1,
                       if_else(fc042_s5 == 0 | fc042_s1 == 0, 0, NA_real_)),
    fc042_2 = coalesce(fc042_2,
                       if_else(fc042_s5 == 0 | fc042_s2 == 0, 0, NA_real_)),
    fc042_3 = coalesce(fc042_3,
                       if_else(fc042_s5 == 0 | fc042_s3 == 0, 0, NA_real_)),
    fc042_3 = coalesce(fc042_4,
                       if_else(fc042_s5 == 0 | fc042_s4 == 0, 0, NA_real_))
  )
ha <- ha %>%
  mutate(
    inotherbef = case_when(
      rowSums(!is.na(select(., fc042_1:fc042_4))) == 0 ~ NA_real_,
      TRUE ~ rowSums(select(., fc042_1:fc042_4), na.rm = TRUE)
    )
  )
describe(ha[, c("fc042_1", "fc042_2", "fc042_3", "fc042_4", "inotherbef")], fast = TRUE)

ha$idunmploy <- case_when(
  !is.na(ha$fg010_1) & !is.na(ha$fg010_2) ~ ha$fg010_1 * ha$fg010_2,
  ha$fg009 == 2                           ~ 0,
  TRUE                                    ~ NA_real_
)

## household wage
hhnet_list <- vector("list", 11)
for (i in 1:11) {
  # Variable names
  gb003_var <- paste0("gb003_", i, "_")
  gb004_var <- paste0("gb004_", i, "_")
  gb005_1_var <- paste0("gb005_1_", i, "_")
  gb005_2_var <- paste0("gb005_2_", i, "_")
  gb005_3_var <- paste0("gb005_3_", i, "_")
  # Initialize ave as NA vector
  ave <- rep(NA, nrow(ha))
  # Only compute average if min/max exist
  if (i %in% c(1:7, 11)) {
    min_var <- paste0("gb003_min_", i, "_")
    max_var <- paste0("gb003_max_", i, "_")
    if (all(c(min_var, max_var) %in% names(ha))) {
      ave <- rowMeans(cbind(
        ifelse(ha[[min_var]] == -1, NA, ha[[min_var]]),
        ifelse(ha[[max_var]] == -1, NA, ha[[max_var]])
      ), na.rm = TRUE)
    }
  }
  # Wage
  hhwage <- ifelse(
    !is.na(ha[[gb003_var]]) & ha[[gb003_var]] != -1,
    ha[[gb003_var]],
    ave
  )
  # Excluded wage
  hhex <- ifelse(
    ha[[gb004_var]] == 2 & ha[[gb005_1_var]] == 2 & !is.na(ha[[gb005_2_var]]) & ha[[gb005_2_var]] != -1,
    ha[[gb005_2_var]],
    ifelse(
      ha[[gb004_var]] == 2 & ha[[gb005_1_var]] == 1 & !is.na(ha[[gb005_1_var]]) & ha[[gb005_1_var]] != -1,
      12 * ha[[gb005_1_var]],
      ifelse(
        ha[[gb004_var]] == 2 & ha[[gb005_1_var]] == 3 & !is.na(ha[[gb005_3_var]]) & ha[[gb005_3_var]] != -1,
        ha[[gb005_3_var]] * hhwage,
        NA
      )
    )
  )
  # Net wage
  hhnet <- ifelse(!is.na(hhwage) & !is.na(hhex), hhwage - hhex, hhwage)
  
  # Ensure hhnet is the correct length
  if (length(hhnet) != nrow(ha)) {
    hhnet <- rep(NA, nrow(ha))
  }
  
  # Store
  hhnet_list[[i]] <- hhnet
  ha[[paste0("hhwagenet", i)]] <- hhnet
}
## Combine and sum
hhnet_matrix <- ha[paste0("hhwagenet", 1:11)]
ha$hhwage_total <- rowSums(hhnet_matrix, na.rm = TRUE)
ha$hhwage_total[apply(is.na(hhnet_matrix), 1, all)] <- NA

## household transfer: hhtransfer
# Step 1: Define all variables to sum
transfer_vars <- c(
  # gb006
  "gb006_1_1_", "gb006_1_2_", "gb006_1_3_", "gb006_1_4_", "gb006_1_5_", "gb006_1_6_", "gb006_1_7_",
  "gb006_2_1_", "gb006_2_2_", "gb006_2_3_", "gb006_2_4_",
  "gb006_3_1_", "gb006_3_2_", "gb006_3_3_", "gb006_3_4_",
  "gb006_4_1_", "gb006_4_2_", "gb006_4_3_", "gb006_4_4_", "gb006_4_5_", "gb006_4_6_",
  "gb006_5_1_", "gb006_5_2_", "gb006_5_3_", "gb006_5_4_",
  "gb006_6_1_", "gb006_6_2_", "gb006_6_3_", "gb006_6_4_",
  "gb006_7_1_", "gb006_7_2_", "gb006_7_3_", "gb006_7_4_", "gb006_7_6_",
  "gb006_8_1_", "gb006_8_2_", "gb006_8_3_", "gb006_8_4_", "gb006_8_5_", "gb006_8_6_", "gb006_8_8_",
  "gb006_9_1_", "gb006_9_2_", "gb006_9_3_", "gb006_9_4_",
  # gb008
  "gb008_7_1_", "gb008_7_2_", "gb008_7_3_", "gb008_7_4_", "gb008_7_5_",
  "gb008_8_1_", "gb008_8_2_", "gb008_8_3_", "gb008_8_4_", "gb008_8_5_", "gb008_8_6_", "gb008_8_7_",
  "gb008_9_1_", "gb008_9_2_", "gb008_9_3_", "gb008_9_4_", "gb008_9_5_", "gb008_9_6_"
)
# Step 2: Replace -1 with NA
ha[transfer_vars] <- lapply(ha[transfer_vars], function(x) ifelse(x == -1, NA, x))
# Step 3: Calculate hhtransfer
ha$hhtransfer <- rowSums(ha[transfer_vars], na.rm = TRUE)
# Step 4: Set hhtransfer to NA if all components are NA
ha$hhtransfer[!rowSums(!is.na(ha[transfer_vars]))] <- NA
s10_flags <- c(
  "gb008_1__s10", "gb008_2__s10", "gb008_3__s10", "gb008_4__s10", "gb008_5__s10", "gb008_6__s10",
  "gb008_7__s10", "gb008_8__s10", "gb008_9__s10", "gb008_10__s10", "gb008_11__s10", "gb008_12__s10",
  "gb006_1__s10", "gb006_2__s10", "gb006_3__s10", "gb006_4__s10", "gb006_5__s10", "gb006_6__s10",
  "gb006_7__s10", "gb006_8__s10", "gb006_9__s10", "gb006_10__s10", "gb006_11__s10", "gb006_12__s10",
  "gb006_13__s10", "gb006_14__s10"
)
# Step 6: If hhtransfer is NA and any _s10 flag == 10, set hhtransfer to 0
ha$hhtransfer <- ifelse(
  is.na(ha$hhtransfer) & rowSums(ha[s10_flags] == 10, na.rm = TRUE) > 0,
  0,
  ha$hhtransfer
)

## household farm, livestock, bussiness: hhflb
ha$gc004_1 <- replace_with_mean(ha$gc004_1, ha$gc004_1_min, ha$gc004_1_max)
ha$gc004_2 <- replace_with_mean(ha$gc004_2, ha$gc004_2_min, ha$gc004_2_max)
ha$gc006_1 <- replace_with_mean(ha$gc006_1, ha$gc006_1_min, ha$gc006_1_max)
ha$gc006_2 <- replace_with_mean(ha$gc006_2, ha$gc006_2_min, ha$gc006_2_max)
ha$gd004_1 <- replace_with_mean(ha$gd004_1, ha$gd004_1_min, ha$gd004_1_max)
ha$gd004_2 <- replace_with_mean(ha$gd004_2, ha$gd004_2_min, ha$gd004_2_max)

make_hhvar <- function(x1, x2, x) {
  case_when(
    !is.na(x1) & x1 != -1 ~ x1,
    !is.na(x2) & x2 != -1 ~ -x2,
    !is.na(x)  & x  == 3  ~ 0,
    TRUE                  ~ NA_real_
  )
}

ha <- ha %>%
  mutate(
    hhfarm = make_hhvar(gc004_1, gc004_2, gc004),
    hhlive = make_hhvar(gc006_1, gc006_2, gc006),
    hhbus  = make_hhvar(gd004_1, gd004_2, gd004)
  )
ha <- ha %>%
  mutate(
    hhflb = if_else(
      rowSums(!is.na(across(c(hhfarm, hhlive, hhbus)))) == 0,
      NA_real_,
      rowSums(across(c(hhfarm, hhlive, hhbus)), na.rm = TRUE)
    )
  )
describe(ha[, c("hhfarm", "hhlive", "hhbus", "hhflb")], fast = TRUE)

## houseld public benefits: hhpublic
ha$hhwubao <- replace_with_mean(ha$ge004_1_, ha$ge004_min_1_, ha$ge004_max_1_)
ha$hhdibao <- replace_with_mean(ha$ge004_2_, ha$ge004_min_2_, ha$ge004_max_2_)
ha$hhtekun <- replace_with_mean(ha$ge004_3_, ha$ge004_min_3_, ha$ge004_max_3_)
ha$hhjiandang <- replace_with_mean(ha$ge004_4_, ha$ge004_min_4_, ha$ge004_max_4_)
ha$hhotherben <- replace_with_mean(ha$ge004_5_, ha$ge004_min_5_, ha$ge004_max_5_)
ha$hhcovid <- replace_with_mean(ha$ge008_1, ha$ge008_1_min, ha$ge008_1_max)
ha$hhland <- replace_with_mean(ha$ge012, ha$ge012_min, ha$ge012_max)
ha$hhhouse <- replace_with_mean(ha$ge013, ha$ge013_min, ha$ge013_max)

replace_ge006 <- function(x, s, s9) {
  case_when(
    !is.na(x) & x != -1 ~ x,
    s == 0 | s9 == 0    ~ 0,
    TRUE                ~ NA_real_
  )
}
ha <- ha %>%
  mutate(across(
    .cols = ge006_1:ge006_8,
    .fns = ~ replace_ge006(.x, get(sub("ge006_", "ge006_s", cur_column())), ge006_s9)
  ))
ha <- ha %>%
  mutate(
    hhothertransfer = case_when(
      rowSums(!is.na(across(ge006_1:ge006_8))) == 0 ~ NA_real_,
      TRUE ~ rowSums(across(ge006_1:ge006_8), na.rm = TRUE)
    ),
    hhinsure = case_when(
      !is.na(ge007) & ge007 != -1 ~ ge007,
      TRUE ~ NA_real_
    ),
    hhphot = case_when(
      !is.na(ge011) & ge011 != -1 ~ ge011,
      TRUE ~ NA_real_
    ),
    hhrent = case_when(
      !is.na(ge014_1) & ge014_1 != -1 ~ ge014_1,
      is.na(ge014_1) & ge014 == 2     ~ 0,
      TRUE ~ NA_real_
    )
  )

public_vars <- c(
  "hhwubao", "hhdibao", "hhtekun", "hhjiandang", "hhotherben",
  "hhcovid", "hhland", "hhhouse", "hhothertransfer", "hhinsure",
  "hhphot", "hhrent"
)
ha <- ha %>%
  mutate(
    hhpublic = case_when(
      rowSums(!is.na(across(all_of(public_vars)))) == 0 ~ NA_real_,
      TRUE ~ rowSums(across(all_of(public_vars)), na.rm = TRUE)
    )
  )
describe(ha$hhpublic, fast = TRUE)

## household income
hhincome_var <- c("ga002", "inbef", "fc039", "inearned", "inotherbef", "idunmploy",
                  "hhwage_total", "hhtransfer", "hhflb", "hhpublic")
ha <- ha %>%
  mutate(
    hhincome2020 = case_when(
      rowSums(!is.na(across(all_of(hhincome_var)))) == 0 ~ NA_real_,
      TRUE ~ rowSums(across(all_of(hhincome_var)), na.rm = TRUE)
    )
  )
describe(ha[, c("hhincome2011", "hhincome2013", "hhincome2015", "hhincome2018", "hhincome2020")], fast = TRUE)


## annual consumption ####
ha <- ha %>% 
  mutate(
    hhconsumption2011 = as.numeric(hh1ctot),
    hhconsumption2013 = as.numeric(hh2ctot),
    hhconsumption2015 = as.numeric(hh3ctot),
    hhconsumption2018 = as.numeric(hh4ctot),
    pcconsumption2011 = as.numeric(hh1cperc),
    pcconsumption2013 = as.numeric(hh2cperc),
    pcconsumption2015 = as.numeric(hh3cperc),
    pcconsumption2018 = as.numeric(hh4cperc)
  )
# 2020
cons_vars <- c("gf001", "gf008", paste0("gf011_", 1:7), paste0("gf013_", 1:16))
ha <- ha %>%
  mutate(across(all_of(cons_vars), ~ na_if(., -1))) %>%
  mutate(gf008 = gf008 * 52) %>%
  mutate(
    hhconsumption2020 = rowSums(across(all_of(cons_vars)), na.rm = TRUE),
    hhconsumption2020 = if_else(rowSums(!is.na(across(all_of(cons_vars)))) == 0, NA_real_, hhconsumption2020),
    pcconsumption2020 = hhconsumption2020 / hhsize2020
  )

rm(list = ls()[!ls() %in% c("ha", ls()[startsWith(ls(), "a20")])])
gc()

# Self-rated health ####
ha <- ha %>%
  mutate(
    selfhealth2011 = r1shlta,
    selfhealth2013 = r2shlta,
    selfhealth2015 = r3shlta,
    selfhealth2018 = r4shlta,
    selfhealth2020 = if_else(da001 == 997, NA_real_, da001)
  ) %>%
  # then apply consistent factor labels to all new variables
  mutate(across(
    c(selfhealth2011, selfhealth2013, selfhealth2015, selfhealth2018, selfhealth2020),
    ~ factor(.x,
             levels = 5:1,
             labels = c("Very poor", "Poor", "Fair", "Good", "Very good"))
  ))

# ADLs - 6 items ####
adl_vars <- list(
  "2011" = c("r1dressa", "r1batha", "r1eata", "r1beda", "r1toilta", "r1urina"),
  "2013" = c("r2dressa", "r2batha", "r2eata", "r2beda", "r2toilta", "r2urina"),
  "2015" = c("r3dressa", "r3batha", "r3eata", "r3beda", "r3toilta", "r3urina"),
  "2018" = c("r4dressa", "r4batha", "r4eata", "r4beda", "r4toilta", "r4urina")
)
for (year in names(adl_vars)) {
  vars <- adl_vars[[year]]
  # Replace tagged NA ("a: Healthy and young") with 0
  ha <- ha %>%
    mutate(across(all_of(vars),
                  ~ if_else(is_tagged_na(.) & na_tag(.) == "a", 0, .)))
  # Create ADL variable using apply
  ha[[paste0("ADL", year)]] <- ifelse(
    apply(ha[vars], 1, function(x) all(is.na(x))),
    NA_real_,
    rowSums(ha[vars], na.rm = TRUE)
  )
}
# 2020
adl_var <- c("db001", "db003", "db005", "db007", "db009", "db011")
iadl_var <- c("db022", "db020", "db016", "db014", "db012", "db018")
dl_var <- c(adl_var, iadl_var)

for (v in dl_var) {
  ha[[paste0(v, "_d")]] <- ifelse(ha[[v]] == 1, 0,
                                  ifelse(ha[[v]] %in% 2:4, 1, NA))
  ha[[paste0(v, "_d")]] <- ifelse(
    is.na(ha[[paste0(v, "_d")]]) & ha$birth_year > 1970 & 
      ha$selfhealth2020 %in% c("Good", "Very good"), 
    0,
    ha[[paste0(v, "_d")]]
  )
}

adl_d_vars <- paste0(adl_var, "_d")
ha$ADL2020 <- ifelse(
  rowSums(!is.na(ha[adl_d_vars])) == 0,
  NA,
  rowSums(ha[adl_d_vars] == 1, na.rm = TRUE)
)
iadl_d_vars <- paste0(iadl_var, "_d")
ha$IADL2020 <- ifelse(
  rowSums(!is.na(ha[iadl_d_vars])) == 0,
  NA,
  rowSums(ha[iadl_d_vars] == 1, na.rm = TRUE)
)

# IADLs - 6 items ####
iadl_vars <- list(
  "2011" = c("r1moneya", "r1medsa", "r1shopa", "r1mealsa", "r1housewka"),
  "2013" = c("r2moneya", "r2medsa", "r2shopa", "r2mealsa", "r2housewka", "r2phonea"),
  "2015" = c("r3moneya", "r3medsa", "r3shopa", "r3mealsa", "r3housewka", "r3phonea"),
  "2018" = c("r4moneya", "r4medsa", "r4shopa", "r4mealsa", "r4housewka", "r4phonea")
)
for (year in names(iadl_vars)) {
  vars <- iadl_vars[[year]]
  # Create IADL variable using apply
  ha[[paste0("IADL", year)]] <- ifelse(
    apply(ha[vars], 1, function(x) all(is.na(x))),
    NA_real_,
    rowSums(ha[vars], na.rm = TRUE)
  )
}

# Cognition ####
# MMSE
## 2020
im_vars <- paste0("dc012_s", 1:10)
ha[im_vars] <- lapply(ha[im_vars], function(x) case_when(
  !is.na(x) & x > 0 ~ 1,
  !is.na(x)          ~ 0,
  TRUE               ~ NA_real_
))
ha <- ha %>%
  mutate(
    imrecall = case_when(
      rowSums(!is.na(across(all_of(im_vars)))) == 0 ~ NA_real_,
      is.na(rowSums(across(all_of(im_vars)), na.rm = TRUE)) & dc012_s11 == 11 ~ 0,
      TRUE ~ rowSums(across(all_of(im_vars)), na.rm = TRUE)
    )
  )

de_vars <- paste0("dc028_s", 1:10)
ha[de_vars] <- lapply(ha[de_vars], function(x) case_when(
  !is.na(x) & x > 0 ~ 1,
  !is.na(x)          ~ 0,
  TRUE               ~ NA_real_
))
ha <- ha %>%
  mutate(
    derecall = case_when(
      rowSums(!is.na(across(all_of(de_vars)))) == 0 ~ NA_real_,
      is.na(rowSums(across(all_of(de_vars)), na.rm = TRUE)) & dc028_s11 == 11 ~ 0,
      TRUE ~ rowSums(across(all_of(de_vars)), na.rm = TRUE)
    )
  )

ha <- ha %>%
  mutate(
    dc007_1 = case_when(
      dc007_1_1 == 93                   ~ 1,
      dc007_1_1 != 93 | dc007_1 == 997 ~ 0,
      TRUE                              ~ NA_real_
    ),
    dc007_2 = case_when(
      dc007_2_1 == 86                   ~ 1,
      dc007_2_1 != 86 | dc007_2 == 997 ~ 0,
      TRUE                              ~ NA_real_
    ),
    dc007_3 = case_when(
      dc007_3_1 == 79                   ~ 1,
      dc007_3_1 != 79 | dc007_3 == 997 ~ 0,
      TRUE                              ~ NA_real_
    ),
    dc007_4 = case_when(
      dc007_4_1 == 72                   ~ 1,
      dc007_4_1 != 72 | dc007_4 == 997 ~ 0,
      TRUE                              ~ NA_real_
    ),
    dc007_5 = case_when(
      dc007_5_1 == 65                   ~ 1,
      dc007_5_1 != 65 | dc007_5 == 997 ~ 0,
      TRUE                              ~ NA_real_
    ),
    serial = ifelse(
      rowSums(!is.na(across(c(dc007_1, dc007_2, dc007_3, dc007_4, dc007_5)))) == 0,
      NA_real_,
      rowSums(across(c(dc007_1, dc007_2, dc007_3, dc007_4, dc007_5)), na.rm = TRUE)
    )
  )

ha <- ha %>%
  mutate(
    across(c(dc001, dc003, dc004, dc005),
           ~ case_when(
             . == 1           ~ 1,
             . %in% c(2, 997) ~ 0,
             TRUE             ~ NA_real_
           )),
    naming = rowSums(across(c(dc001, dc003, dc004, dc005)), na.rm = TRUE),
    naming = ifelse(rowSums(!is.na(across(c(dc001, dc003, dc004, dc005)))) == 0, NA, naming)
  )

ha$draw <- case_when(
  ha$dc009 == 1           ~ 1,
  ha$dc009 %in% c(2, 997) ~ 0,
  TRUE                    ~ NA_real_
)

## 2011-2020 sum
vars_list <- list(
  "2011" = c("r1imrc", "r1dlrc", "r1ser7", "r1mo", "r1dy", "r1yr", "r1draw"),
  "2013" = c("r2imrc", "r2dlrc", "r2ser7", "r2mo", "r2dy", "r2yr", "r2draw"),
  "2015" = c("r3imrc", "r3dlrc", "r3ser7", "r3mo", "r3dy", "r3yr", "r3draw"),
  "2018" = c("r4imrc", "r4dlrc", "r4ser7", "r4mo", "r4dy", "r4yr", "r4draw"),
  "2020" = c("imrecall", "derecall", "serial", "naming", "draw")
)
for (year in names(vars_list)) {
  vars <- vars_list[[year]]
  ha[[paste0("cognition", year)]] <- ifelse(
    apply(ha[vars], 1, function(x) all(is.na(x))),
    NA_real_,
    rowSums(ha[vars], na.rm = TRUE)
  )
}

# CESD ####
cesd_var <- c("dc016","dc017","dc018","dc019","dc020","dc021","dc022","dc023","dc024","dc025")
reverse_var <- c("dc020","dc023")

ha <- ha %>%
  mutate(
    # c2011-2018
    depress2011 = as.numeric(r1cesd10),
    depress2013 = as.numeric(r2cesd10),
    depress2015 = as.numeric(r3cesd10),
    depress2018 = as.numeric(r4cesd10),
    # 2020: reverse-score dc020 and dc023 for 2020, set 997/999 to NA
    across(all_of(reverse_var), ~ ifelse(.x %in% c(997, 999), NA, ifelse(.x %in% 1:4, 4 - .x, .x))),
    # 2020: recode other CESD items to 0-3 for 2020, set 997/999 to NA
    across(setdiff(cesd_var, reverse_var), ~ ifelse(.x %in% c(997, 999), NA, ifelse(.x %in% 1:4, .x - 1, .x))),
    # 2020: compute total depression score for 2020 in one line
    depress2020 = ifelse(rowSums(!is.na(across(all_of(cesd_var)))) == 0, NA,
                         rowSums(across(all_of(cesd_var)), na.rm = TRUE))
  )

# life satisfaction ####
life_map <- c(life2011 = "r1satlife",
              life2013 = "r2satlife",
              life2015 = "r3satlife",
              life2018 = "r4satlife")
ha <- ha %>%
  mutate(
    across(all_of(life_map), 
           ~ factor(.x, levels = 1:5,
                    labels = c("Not at all satisfied",
                               "Not very satisfied",
                               "Somewhat satisfied",
                               "Very satisfied",
                               "Completely satisfied")),
           .names = "{names(life_map)}"),
    
    life2020 = factor(dc026, levels = 5:1,
                      labels = c("Not at all satisfied",
                                 "Not very satisfied",
                                 "Somewhat satisfied",
                                 "Very satisfied",
                                 "Completely satisfied"))
  )

# Unmet ADL needs ####
# 2013-2018
## Helper function for unmet ADL coding
create_unmet <- function(data, vars, prefix) {
  data %>%
    mutate(
      across(all_of(vars),
             ~ case_when(
               .x == 0 ~ 1,
               .x == 1 ~ 0,
               haven::as_factor(.x) %in% c(".a:Healthy and young", ".x:No difficulty") ~ 0,
               TRUE ~ NA_real_
             ),
             .names = paste0(prefix, "_{.col}")),
      !!paste0("unmetADL", sub(".*(\\d{4})$", "\\1", prefix)) := rowSums(across(starts_with(prefix)), na.rm = TRUE),
      !!paste0("unmetADL", sub(".*(\\d{4})$", "\\1", prefix)) := ifelse(
        rowSums(!is.na(across(starts_with(prefix)))) == 0,
        NA,
        !!sym(paste0("unmetADL", sub(".*(\\d{4})$", "\\1", prefix)))
      )
    )
}
## List of waves and their variables
waves <- list(
  "2013" = c("r2dresshlp", "r2bathehlp", "r2eathlp", "r2bedhlp", "r2toilethlp"),
  "2015" = c("r3dresshlp", "r3bathehlp", "r3eathlp", "r3bedhlp", "r3toilethlp"),
  "2018" = c("r4dresshlp", "r4bathehlp", "r4eathlp", "r4bedhlp", "r4toilethlp")
)
## Loop through waves
for (yr in names(waves)) {
  ha <- create_unmet(ha, waves[[yr]], paste0("unmet", yr))
}

# 2020
ha <- ha %>% 
  mutate(
    db002 = ifelse(is.na(db002) & (db001 == 1 | (birth_year > 1970 & selfhealth2020 %in% c("Good", "Very good"))), 0, db002),
    db004 = ifelse(is.na(db004) & (db003 == 1 | (birth_year > 1970 & selfhealth2020 %in% c("Good", "Very good"))), 0, db004),
    db006 = ifelse(is.na(db006) & (db005 == 1 | (birth_year > 1970 & selfhealth2020 %in% c("Good", "Very good"))), 0, db006),
    db008 = ifelse(is.na(db008) & (db007 == 1 | (birth_year > 1970 & selfhealth2020 %in% c("Good", "Very good"))), 0, db008),
    db010 = ifelse(is.na(db010) & (db009 == 1 | (birth_year > 1970 & selfhealth2020 %in% c("Good", "Very good"))), 0, db010)
  ) ## replace missing with 0 if age < 50 & selfhealth = good

unmet2020 <- c("db002", "db004", "db006", "db008", "db010")
ha <- ha %>%
  mutate(
    across(all_of(unmet2020),
           ~ case_when(
             .x == 2 ~ 1,
             .x %in% c(0, 1) ~ 0,
             TRUE ~ NA_real_
           ),
           .names = "unmet2020_{.col}"),
    unmetADL2020 = rowSums(across(starts_with("unmet2020_")), na.rm = TRUE),
    unmetADL2020 = ifelse(rowSums(!is.na(across(starts_with("unmet2020_")))) == 0, NA, unmetADL2020)
  )

# Unmet IADL needs ####
# 2013-2018: 2011 is unavailable
## Helper for unmet IADL
create_unmetIADL <- function(data, vars, prefix, labels) {
  data %>%
    mutate(
      across(all_of(vars),
             ~ case_when(
               .x == 0 ~ 1,
               .x == 1 ~ 0,
               haven::as_factor(.x) %in% labels ~ 0,
               TRUE ~ NA_real_
             ),
             .names = paste0(prefix, "_{.col}")),
      !!prefix := rowSums(across(starts_with(paste0(prefix, "_"))), na.rm = TRUE),
      !!prefix := ifelse(
        rowSums(!is.na(across(starts_with(paste0(prefix, "_"))))) == 0,
        NA,
        !!sym(prefix)
      )
    )
}
## Define waves with their variables and label sets
waves_IADL <- list(
  "2013" = list(
    vars = c("r2hswkhlp", "r2mealhlp", "r2shophlp", "r2medhlp", "r2phonehlp", "r2moneyhlp"),
    labels = c(".a:Healthy and young", ".x:No difficulty", ".n:No phone")
  ),
  "2015" = list(
    vars = c("r3hswkhlp", "r3mealhlp", "r3shophlp", "r3medhlp", "r3phonehlp", "r3moneyhlp"),
    labels = c(".n:Healthy and young", ".x:No difficulty", ".n:No phone")
  ),
  "2018" = list(
    vars = c("r4hswkhlp", "r4mealhlp", "r4shophlp", "r4medhlp", "r4phonehlp", "r4moneyhlp"),
    labels = c(".a:Healthy and young", ".x:No difficulty", ".n:No phone")
  )
)
## Loop through waves
for (yr in names(waves_IADL)) {
  ha <- create_unmetIADL(
    ha,
    waves_IADL[[yr]]$vars,
    paste0("unmetIADL", yr),
    waves_IADL[[yr]]$labels
  )
}

# 2020
ha <- ha %>% 
  mutate(
    db013 = ifelse(is.na(db013) & (db012 == 1 | (birth_year > 1970 & selfhealth2020 %in% c("Good", "Very good"))), 0, db013),
    db015 = ifelse(is.na(db015) & (db014 == 1 | (birth_year > 1970 & selfhealth2020 %in% c("Good", "Very good"))), 0, db015),
    db017 = ifelse(is.na(db017) & (db016 == 1 | (birth_year > 1970 & selfhealth2020 %in% c("Good", "Very good"))), 0, db017),
    db019 = ifelse(is.na(db019) & (db018 == 1 | (birth_year > 1970 & selfhealth2020 %in% c("Good", "Very good"))), 0, db019),
    db021 = ifelse(is.na(db021) & (db020 == 1 | (birth_year > 1970 & selfhealth2020 %in% c("Good", "Very good"))), 0, db021),
    db023 = ifelse(is.na(db023) & (db022 == 1 | (birth_year > 1970 & selfhealth2020 %in% c("Good", "Very good"))), 0, db023)
  ) ## replace missing with 0 if age < 50 & selfhealth = good

unmetI2020 <- c("db013", "db015", "db017", "db019", "db021", "db023")
ha <- ha %>%
  mutate(
    across(all_of(unmetI2020),
           ~ case_when(
             .x == 2 ~ 1,
             .x %in% c(0, 1) ~ 0,
             TRUE ~ NA_real_
           ),
           .names = "unmetI2020_{.col}"),
    unmetIADL2020 = rowSums(across(starts_with("unmetI2020_")), na.rm = TRUE),
    unmetIADL2020 = ifelse(rowSums(!is.na(across(starts_with("unmetI2020_")))) == 0, NA, unmetIADL2020)
  )

# help from spouse ####
waves <- list(
  "2011" = "r1rscare",
  "2013" = "r2rscare",
  "2015" = "r3rscare",
  "2018" = "r4rscare",
  "2020" = "db024_s1"
)

for (yr in names(waves)) {
  var <- waves[[yr]]
  newvar <- paste0("helpspousem", yr)
  
  ha <- ha %>%
    mutate(
      !!sym(newvar) := factor(case_when(
        !!sym(var) == 0 ~ "No",
        !!sym(var) == 1 ~ "Yes",
        haven::as_factor(!!sym(var)) == ".a:Healthy and young" ~ "healthy and young",
        haven::as_factor(!!sym(var)) == ".x:No difficulty" ~ "no difficulty",
        haven::as_factor(!!sym(var)) == ".h:No help received" ~ "no help received",
        TRUE ~ NA_character_
      ), levels = c("No", "Yes", "healthy and young", "no difficulty", "no help received"))
    )
}
# revise 2020
ha <- ha %>%
  mutate(
    helpspousem2020 = case_when(
      is.na(helpspousem2020) & birth_year > 1970 & selfhealth2020 %in% c("Good", "Very good") ~ "healthy and young",
      is.na(helpspousem2020) & IADL2020 == 0 & ADL2020 == 0 ~ "no difficulty",
      is.na(helpspousem2020) & (xehelperselect == 0 | (unmetADL2020 == 5 & unmetIADL2020 == 6)) ~ "no help received",
      TRUE ~ helpspousem2020
    ),
    helpspousem2020 = factor(helpspousem2020, 
                             levels = c("No", "Yes", "healthy and young", "no difficulty", "no help received"))
  )

# help from child/grandchild ####
ha <- ha %>%
  mutate(db024_s3 = if_else(db024_s3 == 3, 1, db024_s3))

waves_child <- list(
  "2011" = "r1rccare",
  "2013" = "r2rccare",
  "2015" = "r3rccare",
  "2018" = "r4rccare",
  "2020" = "db024_s3"
)

for (yr in names(waves_child)) {
  var <- waves_child[[yr]]
  newvar <- paste0("helpchildm", yr)
  
  ha <- ha %>%
    mutate(
      !!sym(newvar) := factor(case_when(
        !!sym(var) == 0 ~ "No",
        !!sym(var) == 1 ~ "Yes",
        haven::as_factor(!!sym(var)) == ".a:Healthy and young" ~ "healthy and young",
        haven::as_factor(!!sym(var)) == ".x:No difficulty" ~ "no difficulty",
        haven::as_factor(!!sym(var)) == ".h:No help received" ~ "no help received",
        TRUE ~ NA_character_
      ), levels = c("No", "Yes", "healthy and young", "no difficulty", "no help received"))
    )
}

# revise 2020
ha <- ha %>%
  mutate(
    helpchildm2020 = case_when(
      is.na(helpchildm2020) & birth_year > 1970 & selfhealth2020 %in% c("Good", "Very good") ~ "healthy and young",
      is.na(helpchildm2020) & IADL2020 == 0 & ADL2020 == 0 ~ "no difficulty",
      is.na(helpchildm2020) & (xehelperselect == 0 | (unmetADL2020 == 5 & unmetIADL2020 == 6)) ~ "no help received",
      TRUE ~ helpchildm2020
    ),
    helpchildm2020 = factor(helpchildm2020, 
                            levels = c("No", "Yes", "healthy and young", "no difficulty", "no help received"))
  )


# Interview status ####
ha <- ha %>% 
  mutate(
    inw1 = ifelse(!is.na(r1iwstat) & r1iwstat == 1, "Yes", "No"),
    inw2 = ifelse(!is.na(r2iwstat) & r2iwstat == 1, "Yes", "No"),
    inw3 = ifelse(!is.na(r3iwstat) & r3iwstat == 1, "Yes", "No"),
    inw4 = ifelse(!is.na(r4iwstat) & r4iwstat == 1, "Yes", "No"),
    inw5 = ifelse(!is.na(iyear) & iyear == 2020, "Yes", "No")
  )

#==============================================================================#
# Variables not available from the Harmonized dataset
#==============================================================================#

# care hours ####
# 2011
a2011_D_Health_status_and_functioning <- a2011_D_Health_status_and_functioning %>%
  mutate(
    # Convert to numeric
    across(starts_with("db023_"), ~ as.numeric(.)),
    across(starts_with("db024_"), ~ as.numeric(.)),
    across(starts_with("db025_"), ~ as.numeric(.)),
    
    # db023_*: sum and count between 1 and 31
    caredayc2011 = rowSums(across(starts_with("db023_")), na.rm = TRUE),
    caredayd2011 = rowSums(across(starts_with("db023_"), ~ .x > 0 & .x <= 31), na.rm = TRUE),
    
    # db024_*: sum and count between 1 and 24
    carehourc2011 = rowSums(across(starts_with("db024_")), na.rm = TRUE),
    carehourd2011 = rowSums(across(starts_with("db024_"), ~ .x > 0 & .x <= 24), na.rm = TRUE),
    
    # db025_*: sum
    livein2011 = rowSums(across(starts_with("db025_")), na.rm = TRUE)
  )

idx2011 <- c(1:2, 4:12, 31:37, 39)
for (i in idx2011) {
  d1 <- paste0("db023_", i, "_")
  d2 <- paste0("db024_", i, "_")
  d3 <- paste0("db025_", i, "_")
  
  a2011_D_Health_status_and_functioning[[paste0("tmp_", i)]] <-
    ifelse(!is.na(a2011_D_Health_status_and_functioning[[d1]]) & 
             !is.na(a2011_D_Health_status_and_functioning[[d2]]),
           a2011_D_Health_status_and_functioning[[d1]] * a2011_D_Health_status_and_functioning[[d2]],
           ifelse(is.na(a2011_D_Health_status_and_functioning[[d1]]) &
                    !is.na(a2011_D_Health_status_and_functioning[[d2]]) &
                    a2011_D_Health_status_and_functioning[[d3]] == 1,
                  31 * a2011_D_Health_status_and_functioning[[d2]],
                  0))
}
a2011_D_Health_status_and_functioning <- a2011_D_Health_status_and_functioning %>%
  mutate(caretotal2011 = rowSums(select(., starts_with("tmp_")), na.rm = TRUE)) %>%
  select(-starts_with("tmp_"))

# 2013
a2013_D_Health_status_and_functioning <- a2013_D_Health_status_and_functioning %>%
  mutate(
    # Convert to numeric
    across(starts_with("db023_"), ~ as.numeric(.)),
    across(starts_with("db024_"), ~ as.numeric(.)),
    across(starts_with("db025_"), ~ as.numeric(.)),
    
    # db023_*: sum and count between 1 and 31
    caredayc2013 = rowSums(across(starts_with("db023_")), na.rm = TRUE),
    caredayd2013 = rowSums(across(starts_with("db023_"), ~ .x > 0 & .x <= 31), na.rm = TRUE),
    
    # db024_*: sum and count between 1 and 24
    carehourc2013 = rowSums(across(starts_with("db024_")), na.rm = TRUE),
    carehourd2013 = rowSums(across(starts_with("db024_"), ~ .x > 0 & .x <= 24), na.rm = TRUE),
    
    # db025_*: sum
    livein2013 = rowSums(across(starts_with("db025_")), na.rm = TRUE)
  )

idx2013 <- c(1:9, 11, 26:33, 36, 51)
for (i in idx2013) {
  d1 <- paste0("db023_", i, "_")
  d2 <- paste0("db024_", i, "_")
  d3 <- paste0("db025_", i, "_")
  
  a2013_D_Health_status_and_functioning[[paste0("tmp_", i)]] <-
    ifelse(!is.na(a2013_D_Health_status_and_functioning[[d1]]) & 
             !is.na(a2013_D_Health_status_and_functioning[[d2]]),
           a2013_D_Health_status_and_functioning[[d1]] * a2013_D_Health_status_and_functioning[[d2]],
           ifelse(is.na(a2013_D_Health_status_and_functioning[[d1]]) &
                    !is.na(a2013_D_Health_status_and_functioning[[d2]]) &
                    a2013_D_Health_status_and_functioning[[d3]] == 1,
                  31 * a2013_D_Health_status_and_functioning[[d2]],
                  0))
}
a2013_D_Health_status_and_functioning <- a2013_D_Health_status_and_functioning %>%
  mutate(caretotal2013 = rowSums(select(., starts_with("tmp_")), na.rm = TRUE)) %>%
  select(-starts_with("tmp_"))

# 2015
a2015_D_Health_status_and_functioning <- a2015_D_Health_status_and_functioning %>%
  mutate(
    # Convert to numeric
    across(matches("^db023_(?:[1-9]|1[0-9]|2[0-5])_$"), ~ as.numeric(.)),
    across(starts_with("db024_"), ~ as.numeric(.)),
    across(starts_with("db025_"), ~ as.numeric(.)),
    
    # db023_*: sum and count between 1 and 31
    caredayc2015 = rowSums(across(matches("^db023_(?:[1-9]|1[0-9]|2[0-5])_$")), na.rm = TRUE),
    caredayd2015 = rowSums(across(matches("^db023_(?:[1-9]|1[0-9]|2[0-5])_$"), ~ .x > 0 & .x <= 31), na.rm = TRUE),
    
    # db024_*: sum and count between 1 and 24
    carehourc2015 = rowSums(across(starts_with("db024_")), na.rm = TRUE),
    carehourd2015 = rowSums(across(starts_with("db024_"), ~ .x > 0 & .x <= 24), na.rm = TRUE),
    
    # db025_*: sum
    livein2015 = rowSums(across(starts_with("db025_")), na.rm = TRUE)
  )

idx2015 <- c(1:22, 24:25)
for (i in idx2015) {
  d1 <- paste0("db023_", i, "_")
  d2 <- paste0("db024_", i, "_")
  d3 <- paste0("db025_", i, "_")
  
  a2015_D_Health_status_and_functioning[[paste0("tmp_", i)]] <-
    ifelse(!is.na(a2015_D_Health_status_and_functioning[[d1]]) & 
             !is.na(a2015_D_Health_status_and_functioning[[d2]]),
           a2015_D_Health_status_and_functioning[[d1]] * a2015_D_Health_status_and_functioning[[d2]],
           ifelse(is.na(a2015_D_Health_status_and_functioning[[d1]]) &
                    !is.na(a2015_D_Health_status_and_functioning[[d2]]) &
                    a2015_D_Health_status_and_functioning[[d3]] == 1,
                  31 * a2015_D_Health_status_and_functioning[[d2]],
                  0))
}
a2015_D_Health_status_and_functioning <- a2015_D_Health_status_and_functioning %>%
  mutate(caretotal2015 = rowSums(select(., starts_with("tmp_")), na.rm = TRUE)) %>%
  select(-starts_with("tmp_"))

# 2018
a2018_D_Health_status_and_functioning <- a2018_D_Health_status_and_functioning %>%
  mutate(
    # Convert to numeric
    across(db023_1_:db023_34_, ~ as.numeric(.)),
    across(starts_with("db024_"), ~ as.numeric(.)),
    across(starts_with("db025_"), ~ as.numeric(.)),
    
    # db023_*: sum and count between 1 and 31
    caredayc2018 = rowSums(across(db023_1_:db023_34_), na.rm = TRUE),
    caredayd2018 = rowSums(across(db023_1_:db023_34_, ~ .x > 0 & .x <= 31), na.rm = TRUE),
    
    # db024_*: sum and count between 1 and 24
    carehourc2018 = rowSums(across(starts_with("db024_")), na.rm = TRUE),
    carehourd2018 = rowSums(across(starts_with("db024_"), ~ .x > 0 & .x <= 24), na.rm = TRUE),
    
    # db025_*: sum
    livein2018 = rowSums(across(starts_with("db025_")), na.rm = TRUE)
  )

idx2018 <- c(1:34)
for (i in idx2018) {
  d1 <- paste0("db023_", i, "_")
  d2 <- paste0("db024_", i, "_")
  d3 <- paste0("db025_", i, "_")
  
  a2018_D_Health_status_and_functioning[[paste0("tmp_", i)]] <-
    ifelse(!is.na(a2018_D_Health_status_and_functioning[[d1]]) & 
             !is.na(a2018_D_Health_status_and_functioning[[d2]]),
           a2018_D_Health_status_and_functioning[[d1]] * a2018_D_Health_status_and_functioning[[d2]],
           ifelse(is.na(a2018_D_Health_status_and_functioning[[d1]]) &
                    !is.na(a2018_D_Health_status_and_functioning[[d2]]) &
                    a2018_D_Health_status_and_functioning[[d3]] == 1,
                  31 * a2018_D_Health_status_and_functioning[[d2]],
                  0))
}
a2018_D_Health_status_and_functioning <- a2018_D_Health_status_and_functioning %>%
  mutate(caretotal2018 = rowSums(select(., starts_with("tmp_")), na.rm = TRUE)) %>%
  select(-starts_with("tmp_"))

# 2020
ha <- ha %>%
  mutate(
    # db033_*: recode -1 to NA and convert to numeric
    across(db033_1_:db033_25_, ~ as.numeric(na_if(.x, -1))),
    # Sum of all db033_* variables
    caredayc2020 = rowSums(across(db033_1_:db033_25_), na.rm = TRUE),
    # Count how many db033_* are between 1 and 31
    caredayd2020 = rowSums(across(db033_1_:db033_25_, ~ .x > 0 & .x <= 31), na.rm = TRUE),
    
    # db034_*: recode -1 to NA and convert to numeric
    across(starts_with("db034_"), ~ as.numeric(na_if(.x, -1))),
    # Sum of all db034_* variables
    carehourc2020 = rowSums(across(starts_with("db034_")), na.rm = TRUE),
    # Count how many db034_* are between 1 and 24
    carehourd2020 = rowSums(across(starts_with("db034_"), ~ .x > 0 & .x <= 24), na.rm = TRUE),
    
    # Recode 2 to 0 for all db035_* variables
    across(starts_with("db035_"), ~ ifelse(.x == 2, 0, .x)),
    # Sum of all db035_* variables
    livein2020 = rowSums(across(starts_with("db035_")), na.rm = TRUE)
  )

idx2020 <- c(1:25)
for (i in idx2020) {
  d1 <- paste0("db033_", i, "_")
  d2 <- paste0("db034_", i, "_")
  d3 <- paste0("db035_", i, "_")
  
  ha[[paste0("tmp_", i)]] <-
    ifelse(!is.na(ha[[d1]]) & 
             !is.na(ha[[d2]]),
           ha[[d1]] * ha[[d2]],
           ifelse(is.na(ha[[d1]]) &
                    !is.na(ha[[d2]]) &
                    ha[[d3]] == 1,
                  31 * ha[[d2]],
                  0))
}
ha <- ha %>%
  mutate(caretotal2020 = rowSums(select(., starts_with("tmp_")), na.rm = TRUE)) %>%
  select(-starts_with("tmp_"))


# Health insurance ####
# across 5 waves, use mode to replace missing insurance type
  # 1 = UEBMI (Urban Employee Basic Medical Insurance)
  # 2 = URRBMI (Urban and Rural Resident Basic Medical Insurance)
  # 3 = URBMI (Urban Resident Basic Medical Insurance)
  # 4 = NRCMS (New Rural Cooperative Medical Scheme)
  # 5 = Gongfei (Government medical insurance)
  # 6 = Aid (Medical aid)
  # 7 = Dabing (Urban non-employed persons’s health insurance)
  # 8 = LTCI (Long-term care insurance)
  # 9 = other
  # 10 = No insurance

# 2011
a2011_E_Health_care_and_insurance <- a2011_E_Health_care_and_insurance %>%
  mutate(across(starts_with("ea001s"), as.numeric)) %>%
  mutate(
    hinsurance2011 = case_when(
      ea001s1  == 1  ~ 1, # 1 = UEBMI (Urban Employee Basic Medical Insurance)
      ea001s5  == 5  ~ 5, # 5 = Gongfei (Government medical insurance)
      ea001s2  == 2  ~ 3, # 3 = URBMI (Urban Resident Basic Medical Insurance)
      ea001s3  == 3  ~ 4, # 4 = NRCMS (New Rural Cooperative Medical Scheme)
      ea001s4  == 4  ~ 2, # 2 = URRBMI (Urban and Rural Resident Basic Medical Insurance)
      ea001s6  == 6  ~ 6, # 6 = Aid (Medical aid)
      ea001s9  == 9  ~ 9, # 9 = other
      ea001s10 == 10 ~ 10, # 10 = No insurance
      TRUE ~ NA_real_
    )
  )

# 2013
a2013_E_Health_care_and_insurance <- a2013_E_Health_care_and_insurance %>%
  mutate(across(starts_with("ea001s"), as.numeric)) %>% 
  mutate(
    hinsurance2013 = case_when(
      ea001s1  == 1  ~ 1, # 1 = UEBMI (Urban Employee Basic Medical Insurance)
      ea001s5  == 5  ~ 5, # 5 = Gongfei (Government medical insurance)
      ea001s2  == 2  ~ 3, # 3 = URBMI (Urban Resident Basic Medical Insurance)
      ea001s3  == 3  ~ 4, # 4 = NRCMS (New Rural Cooperative Medical Scheme)
      ea001s4  == 4  ~ 2, # 2 = URRBMI (Urban and Rural Resident Basic Medical Insurance)
      ea001s6  == 6  ~ 6, # 6 = Aid (Medical aid)
      ea001s9  == 9  ~ 7, # 7 = Dabing (Urban non-employed persons’s health insurance)
      ea001s10 == 10 ~ 9, # 9 = other
      ea001s11 == 11 ~ 10, # 10 = No insurance
      TRUE ~ NA_real_
    )
  )

# 2015
## Vector of column suffixes
suffixes <- 1:10
## Column names
cols_2 <- paste0("ea001_w3_2_", suffixes, "_")
cols_3 <- paste0("ea001_w3_3_", suffixes, "_")
## Ensure numeric
a2015_E_Health_care_and_insurance[cols_2] <- lapply(a2015_E_Health_care_and_insurance[cols_2], as.numeric)
a2015_E_Health_care_and_insurance[cols_3] <- lapply(a2015_E_Health_care_and_insurance[cols_3], as.numeric)
## Apply the Yes/No/NA logic to all pairs in one go
a2015_E_Health_care_and_insurance[cols_2] <- lapply(seq_along(cols_2), function(i) {
  a <- a2015_E_Health_care_and_insurance[[cols_2[i]]]
  b <- a2015_E_Health_care_and_insurance[[cols_3[i]]]
  
  ifelse(
    rowSums(cbind(a, b) == 1, na.rm = TRUE) >= 1, 1, # Yes
    ifelse(rowSums(cbind(a, b) == 2, na.rm = TRUE) >= 1, 0, NA_real_) # No and NA
  )
})

a2015_E_Health_care_and_insurance <- a2015_E_Health_care_and_insurance %>%
  mutate(
    hinsurance2015 = case_when(
      ea001_w3_2_1_  == 1 ~ 1,  # 1 = UEBMI
      ea001_w3_2_5_  == 1 ~ 5,  # 5 = Gongfei
      ea001_w3_2_2_  == 1 ~ 3,  # 3 = URBMI
      ea001_w3_2_3_  == 1 ~ 4,  # 4 = NRCMS
      ea001_w3_2_4_  == 1 ~ 2,  # 2 = URRBMI
      ea001_w3_2_6_  == 1 ~ 6,  # 6 = Aid
      ea001_w3_2_9_  == 1 ~ 7,  # 7 = Dabing
      ea001_w3_2_10_ == 1 ~ 9,  # 9 = Other
      ea009 %in% 1:7 ~ 10,      # 10 = No insurance
      TRUE ~ NA_real_
    )
  )

# 2018
a2018_E_Health_care_and_insurance <- a2018_E_Health_care_and_insurance %>%
  mutate(across(starts_with("ea001_w4_s"), as.numeric)) %>% 
  mutate(
    hinsurance2018 = case_when(
      ea001_w4_s1  == 1  ~ 1, # 1 = UEBMI (Urban Employee Basic Medical Insurance)
      ea001_w4_s5  == 5  ~ 5, # 5 = Gongfei (Government medical insurance)
      ea001_w4_s10 == 10 ~ 8, # 8 = LTCI
      ea001_w4_s3  == 3  ~ 3, # 3 = URBMI (Urban Resident Basic Medical Insurance)
      ea001_w4_s4  == 4  ~ 4, # 4 = NRCMS (New Rural Cooperative Medical Scheme)
      ea001_w4_s2  == 2  ~ 2, # 2 = URRBMI (Urban and Rural Resident Basic Medical Insurance)
      ea001_w4_s6  == 6  ~ 6, # 6 = Aid (Medical aid)
      ea001_w4_s9  == 9  ~ 7, # 7 = Dabing (Urban non-employed persons’s health insurance)
      ea001_w4_s11 == 11 ~ 9, # 9 = other
      ea001_w4_s12 == 12 ~ 10, # 10 = No insurance
      TRUE ~ NA_real_
    )
  )

# 2020
ha <- ha %>% 
  mutate(
    ba017 = as.numeric(ba017),
    hinsurance2020 = case_when(
      ba017 == 1 ~ 1, # 1 = UEBMI (Urban Employee Basic Medical Insurance)
      ba017 == 5 ~ 5, # 5 = Gongfei (Government medical insurance)
      ba017 == 3 ~ 3, # 3 = URBMI (Urban Resident Basic Medical Insurance)
      ba017 == 4 ~ 4, # 4 = NRCMS (New Rural Cooperative Medical Scheme)
      ba017 == 2 ~ 2, # 2 = URRBMI (Urban and Rural Resident Basic Medical Insurance)
      ba017 == 6 ~ 9, # 9 = other
      TRUE ~ NA_real_
    )
  )

# merge with each wave's dataset ####
# keep using variables
ha_subset <- ha %>% 
  select(ID, householdID, communityID, hhid, hhidc, ID_w1, householdID_w1, r1iwm,
         birth_year, sex, educ, educl, inw1, inw2, inw3, inw4, inw5, province, city,
         ends_with(c("2011", "2013", "2015", "2018", "2020")))

# merge with variables unavailable in the Harmonized dataset
ha_subset <- ha_subset %>%
  # health and functioning
  left_join(a2011_D_Health_status_and_functioning %>% select(ID, ends_with("2011")), join_by(ID_w1 == ID)) %>%
  left_join(a2013_D_Health_status_and_functioning %>% select(ID, ends_with("2013")), join_by(ID)) %>%
  left_join(a2015_D_Health_status_and_functioning %>% select(ID, ends_with("2015")), join_by(ID)) %>%
  left_join(a2018_D_Health_status_and_functioning %>% select(ID, ends_with("2018")), join_by(ID)) %>% 
  # health insurance
  left_join(a2011_E_Health_care_and_insurance %>% select(ID, ends_with("2011")), join_by(ID_w1 == ID)) %>%
  left_join(a2013_E_Health_care_and_insurance %>% select(ID, ends_with("2013")), join_by(ID)) %>%
  left_join(a2015_E_Health_care_and_insurance %>% select(ID, ends_with("2015")), join_by(ID)) %>%
  left_join(a2018_E_Health_care_and_insurance %>% select(ID, ends_with("2018")), join_by(ID))

# check duplicates
anyDuplicated(ha_subset$ID) # it should = 0
names(ha_subset)[grepl("\\.(x|y)$", names(ha_subset))] # character(0)

# order variable names
fixed_order <- c("ID", "householdID", "communityID", "birth_year", "sex", "educ", "educl")
all_vars <- names(ha_subset)
year_vars <- all_vars[grepl("2011$|2013$|2015$|2018$|2020$", all_vars)]
sorted_year_vars <- year_vars[order(substr(year_vars, 1, nchar(year_vars) - 4))]
end_vars <- c("province", "city", "ID_w1", "householdID_w1",
              "inw1", "inw2", "inw3", "inw4", "inw5")
final_order <- c(fixed_order, sorted_year_vars, end_vars)
ha_subset <- ha_subset[, final_order]

## Reshape dataset ####
year_suffixes <- c("2011","2013","2015","2018","2020")
long <- ha_subset %>%
  pivot_longer(
    cols = ends_with(year_suffixes),
    names_to = c(".value", "year"),   # .value keeps the base variable name
    names_pattern = "(.*)(20\\d{2})"  # splits variable into name + year
  ) %>%
  mutate(year = as.integer(year))

long <- long %>% 
  mutate(age = year - birth_year)

# remove used datasets
rm(list = ls()[!ls() %in% c("long", "ha", ls()[startsWith(ls(), "a20")])])
gc()

# City code and covariates ####
sum(is.na(long$city))
sum(is.na(long$province))
long <- rename(long,
               cityname = city,
               provincename = province)

# Create a named vector: names = cityname, values = city code
city_map <- c(
  "上海市" = 310000,
  "上饶市" = 361100,
  "临汾市" = 141000,
  "临沂市" = 371300,
  "临沧市" = 530900,
  "临沧"   = 530900,
  "丽水市" = 331100,
  "九江市" = 360400,
  "亳州市" = 341600,
  "佛山市" = 440600,
  "佳木斯市" = 230800,
  "保定市" = 130600,
  "保山市" = 530500,
  "信阳市" = 411500,
  "六安市" = 341500,
  "兰州市" = 620100,
  "兴安盟" = 152200,
  "内江市" = 511000,
  "凉山彝族自治州" = 513400,
  "北京"   = 110000,
  "南充市" = 511300,
  "南宁市" = 450100,
  "南昌市" = 360100,
  "台州市" = 331000,
  "吉安市" = 360800,
  "吉林市" = 220200,
  "周口市" = 411600,
  "呼伦贝尔市" = 150700,
  "呼和浩特市" = 150100,
  "哈尔滨市" = 230100,
  "哈尔滨"   = 230100,
  "嘉兴市" = 330400,
  "四平市" = 220300,
  "大连市" = 210200,
  "天津"   = 120000,
  "威海市" = 371000,
  "娄底市" = 431300,
  "宁德市" = 350900,
  "宁波市" = 330200,
  "安庆市" = 340800,
  "安阳市" = 410500,
  "定西市" = 621100,
  "宜宾市" = 511500,
  "宜春市" = 360900,
  "宝鸡市" = 610300,
  "宿州市" = 341300,
  "宿迁市" = 321300,
  "岳阳市" = 430600,
  "巢湖市" = 341400,
  "常德市" = 430700,
  "平凉市" = 620800,
  "平顶山市" = 410400,
  "广安市" = 511600,
  "广州市" = 440100,
  "张掖市" = 620700,
  "徐州市" = 320300,
  "德州市" = 371400,
  "忻州市" = 140900,
  "恩施土家族苗族自治州" = 422800,
  "成都市" = 510100,
  "扬州市" = 321000,
  "承德市" = 130800,
  "昆明市" = 530100,
  "昭通市" = 530600,
  "景德镇市" = 360200,
  "朝阳市" = 211300,
  "本溪市" = 210500,
  "杭州市" = 330100,
  "枣庄市" = 370400,
  "桂林市" = 450300,
  "楚雄彝族自治州" = 532300,
  "榆林市" = 610800,
  "汉中市" = 610700,
  "江门市" = 440700,
  "沧州市" = 130900,
  "河池市" = 451200,
  "泰州市" = 321200,
  "洛阳市" = 410300,
  "济南市" = 370100,
  "海东地区" = 630200,
  "淮南市" = 340400,
  "深圳市" = 440300,
  "清远市" = 441800,
  "渭南市" = 610500,
  "湖州市" = 330500,
  "滨州市" = 371600,
  "漳州市" = 350600,
  "潍坊市" = 370700,
  "潮州市" = 445100,
  "濮阳市" = 410900,
  "焦作市" = 410800,
  "玉林市" = 450900,
  "甘孜藏族自治州" = 513300,
  "益阳市" = 430900,
  "盐城市" = 320900,
  "眉山市" = 511400,
  "石家庄市" = 130100,
  "福州市" = 350100,
  "绵阳市" = 510700,
  "聊城市" = 371500,
  "苏州市" = 320500,
  "茂名市" = 440900,
  "荆门市" = 420800,
  "莆田市" = 350300,
  "襄樊市" = 420600,
  "资阳市" = 512000,
  "赣州市" = 360700,
  "赤峰市" = 150400,
  "运城市" = 140800,
  "连云港市" = 320700,
  "邵阳市" = 430500,
  "郑州市" = 410100,
  "重庆市" = 500000,
  "锡林郭勒盟" = 152500,
  "锦州市" = 210700,
  "长沙市" = 430100,
  "阜阳市" = 341200,
  "阳泉市" = 140300,
  "阿克苏地区" = 652900,
  "青岛市" = 370200,
  "鞍山市" = 210300,
  "鸡西市" = 230300,
  "黄冈市" = 421100,
  "黔东南苗族侗族自治州" = 522600,
  "黔南布依族苗族自治州" = 522700,
  "齐齐哈尔市" = 230200,
  "丽江市" = 530700
)
# Assign codes using lookup
long$city <- city_map[long$cityname]
# how many are missing?
sum(is.na(long$city))


## check ID ####
an <- long
sum(is.na(an$ID))
table(an$year, useNA = "always")

any(table(an[c("ID","year")]) > 1) # duplicates r ID year
sum(is.na(an$ID) | trimws(an$ID) == "") # No. of missing ID

# interview status
an <- an %>%
  mutate(
    inw = case_when(
      year == 2011 ~ as.character(inw1),
      year == 2013 ~ as.character(inw2),
      year == 2015 ~ as.character(inw3),
      year == 2018 ~ as.character(inw4),
      year == 2020 ~ as.character(inw5),
      TRUE ~ NA_character_
    )
  )

# write dataset ####
# check unique ID and observations
nrow(an) # n = 129410
length(unique(an$ID)) # Total respondents = 25,882

# write data for LTCI_cash
write_dta(an, "an.dta")

# remove used datasets
rm(list = setdiff(ls(), "an"))
gc()
