library(tidyverse)
library(vroom)
library(data.table)
library(tictoc)


# 2.0 DATA IMPORT ----

# 2.1 Loan Acquisitions Data ----

col_types_acq <- list(
  loan_id                            = col_factor(),
  original_channel                   = col_factor(NULL),
  seller_name                        = col_factor(NULL),
  original_interest_rate             = col_double(),
  original_upb                       = col_integer(),
  original_loan_term                 = col_integer(),
  original_date                      = col_date("%m/%Y"),
  first_pay_date                     = col_date("%m/%Y"),
  original_ltv                       = col_double(),
  original_cltv                      = col_double(),
  number_of_borrowers                = col_double(),
  original_dti                       = col_double(),
  original_borrower_credit_score     = col_double(),
  first_time_home_buyer              = col_factor(NULL),
  loan_purpose                       = col_factor(NULL),
  property_type                      = col_factor(NULL),
  number_of_units                    = col_integer(),
  occupancy_status                   = col_factor(NULL),
  property_state                     = col_factor(NULL),
  zip                                = col_integer(),
  primary_mortgage_insurance_percent = col_double(),
  product_type                       = col_factor(NULL),
  original_coborrower_credit_score   = col_double(),
  mortgage_insurance_type            = col_double(),
  relocation_mortgage_indicator      = col_factor(NULL))


acquisition_data <- vroom(
  file       = "00_data/03_loan_data/Acquisition_2019Q1.txt", 
  delim      = "|", 
  col_names  = names(col_types_acq),
  col_types  = col_types_acq,
  na         = c("", "NA", "NULL"))


# 2.2 Performance Data ----

col_types_perf = list(
  loan_id                                = col_factor(),
  monthly_reporting_period               = col_date("%m/%d/%Y"),
  servicer_name                          = col_factor(NULL),
  current_interest_rate                  = col_double(),
  current_upb                            = col_double(),
  loan_age                               = col_double(),
  remaining_months_to_legal_maturity     = col_double(),
  adj_remaining_months_to_maturity       = col_double(),
  maturity_date                          = col_date("%m/%Y"),
  msa                                    = col_double(),
  current_loan_delinquency_status        = col_double(),
  modification_flag                      = col_factor(NULL),
  zero_balance_code                      = col_factor(NULL),
  zero_balance_effective_date            = col_date("%m/%Y"),
  last_paid_installment_date             = col_date("%m/%d/%Y"),
  foreclosed_after                       = col_date("%m/%d/%Y"),
  disposition_date                       = col_date("%m/%d/%Y"),
  foreclosure_costs                      = col_double(),
  prop_preservation_and_repair_costs     = col_double(),
  asset_recovery_costs                   = col_double(),
  misc_holding_expenses                  = col_double(),
  holding_taxes                          = col_double(),
  net_sale_proceeds                      = col_double(),
  credit_enhancement_proceeds            = col_double(),
  repurchase_make_whole_proceeds         = col_double(),
  other_foreclosure_proceeds             = col_double(),
  non_interest_bearing_upb               = col_double(),
  principal_forgiveness_upb              = col_double(),
  repurchase_make_whole_proceeds_flag    = col_factor(NULL),
  foreclosure_principal_write_off_amount = col_double(),
  servicing_activity_indicator           = col_factor(NULL))

performance_data <- vroom(
  file       = "00_data/03_loan_data/Performance_2019Q1.txt", 
  delim      = "|", 
  col_names  = names(col_types_perf),
  col_types  = col_types_perf,
  na         = c("", "NA", "NULL"))


# 3.1 Acquisition Data ----

setDT(acquisition_data)

# 3.2 Performance Data ----

setDT(performance_data)

# 4.0 DATA WRANGLING ----

# 4.1 Joining / Merging Data ----

combined_data <- merge(acquisition_data, 
                       performance_data, 
                       by = "loan_id", 
                       all.x = TRUE, 
                       all.y = FALSE)

# Preparing the Data Table

setkey(combined_data, "loan_id")
key(combined_data)
setorderv(combined_data, c("loan_id", "monthly_reporting_period"))

# 4.3 Select Columns ----

combined_data %>% dim()

keep_cols <- c("loan_id",
               "monthly_reporting_period",
               "seller_name",
               "current_interest_rate",
               "current_upb",
               "loan_age",
               "remaining_months_to_legal_maturity",
               "adj_remaining_months_to_maturity",
               "current_loan_delinquency_status",
               "modification_flag",
               "zero_balance_code",
               "foreclosure_costs",
               "prop_preservation_and_repair_costs",
               "asset_recovery_costs",
               "misc_holding_expenses",
               "holding_taxes",
               "net_sale_proceeds",
               "credit_enhancement_proceeds",
               "repurchase_make_whole_proceeds",
               "other_foreclosure_proceeds",
               "non_interest_bearing_upb",
               "principal_forgiveness_upb",
               "repurchase_make_whole_proceeds_flag",
               "foreclosure_principal_write_off_amount",
               "servicing_activity_indicator",
               "original_channel",
               "original_interest_rate",
               "original_upb",
               "original_loan_term",
               "original_ltv",
               "original_cltv",
               "number_of_borrowers",
               "original_dti",
               "original_borrower_credit_score",
               "first_time_home_buyer",
               "loan_purpose",
               "property_type",
               "number_of_units",
               "property_state",
               "occupancy_status",
               "primary_mortgage_insurance_percent",
               "product_type",
               "original_coborrower_credit_score",
               "mortgage_insurance_type",
               "relocation_mortgage_indicator")

combined_data <- combined_data[, ..keep_cols]

combined_data %>% dim()

# 4.4 Grouped Mutations ----
# - Add response variable (Predict wether loan will become delinquent in next 3 months)
combined_data[, gt_1mo_behind_in_3mo := lead(current_loan_delinquency_status, n = 3) >= 1,
              by = loan_id]





loans_in_month <- combined_data[!is.na(monthly_reporting_period), .N, by = monthly_reporting_period]

loans_max_delinq <- combined_data[current_loan_delinquency_status == 12, by = loan_id]

loans_max_upb <- combined_data[current_loan_delinquency_status >= 1, max(current_loan_delinquency_status), by = loan_id][
                  order(V1, decreasing = TRUE)]

loans_last_ups <- combined_data[current_loan_delinquency_status >= 1, .SD[.N], by = loan_id][
  !is.na(current_upb)][
    order(-current_upb), .(loan_id, monthly_reporting_period, current_loan_delinquency_status, seller_name, current_upb)  
  ]

upb_by_company <- combined_data[!is.na(current_upb), .SD[.N], by = loan_id][
  ,.(sum_current_upb = sum(current_upb, na.rm = TRUE)), by = seller_name][
    order(-sum_current_upb)]
