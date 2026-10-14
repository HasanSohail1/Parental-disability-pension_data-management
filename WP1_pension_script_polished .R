# WP1: Parental disability pension preparation and checks
# Hasan Sohail | Prepared 13 October 2026; printing fix 14 October 2026
#
# Original workflow, object names and exploratory sections retained.
# Updated to the definitions agreed during review on 13 October 2026:
# - Any/full/partial DP event: first date > baseline and <= censoring date.
# - Censoring: earliest death, first LAJI 5/5A retirement, or 2017-12-31.
# - Multiple diagnoses: more than one distinct known DRYHMA1 code on the
#   first date of the corresponding DP type, even within the Other group.
# - Duplicate identical codes count once; 21/missing/unrecognised codes do
#   not represent an additional known cause. No known code = Unknown.
# - Unknown and multiple causes count in all-cause events; their mental,
#   musculoskeletal and Other indicators are zero at the same first DP date.
# Required objects already loaded: parent_cohort_1987_master_final,
# pension_parents_1987.cohort (or pension_parents_1987), and
# matched_cohort_1987_allexposures_final.
# Files are saved in the current working directory.
# The initial LAJI 0/2 section and fixed-count power calculation are historical
# exploration; final preparation starts in section 2.
# Reconstructed screenshot setup blocks remain explicitly marked.
# Marital status is measured in 1987; birth is the time origin.
# Source extraction, matching and register coverage are not certified here.
# Validation reported successful in the user's SD Desktop RStudio session
# on 14 October 2026 after the printing fix below. The user's comparison
# reported matching parent IDs and values in all shared columns versus the
# previous final datasets; rebuilt outputs contain additional columns.
# Confidential register data are not included in this script.

if (!exists("pension_parents_1987.cohort") && exists("pension_parents_1987")) {
  pension_parents_1987.cohort <- pension_parents_1987
}

# -----------------------------------------------------------------------------
# 1. Exploratory linkage and pension timing (initial LAJI 0/2 definition)
# -----------------------------------------------------------------------------

parent_ids <- unique(na.omit(as.character(
  parent_cohort_1987_master_final$parent_id
)))
pension_ids <- unique(na.omit(as.character(
  pension_parents_1987.cohort$FID
)))

sum(parent_ids %in% pension_ids)
length(parent_ids)
# Original observed count: 5,068 parents linked to pension records.

# Inner join is used only for this exploratory pension-record subset.
parent_data <- parent_cohort_1987_master_final
pension_data <- pension_parents_1987.cohort
parent_data$parent_id <- as.character(parent_data$parent_id)
pension_data$FID <- as.character(pension_data$FID)

parent_pension_merged <- merge(
  parent_data, pension_data,
  by.x = "parent_id", by.y = "FID", all = FALSE
)

nrow(parent_pension_merged)
length(unique(parent_pension_merged$parent_id))

one_row_per_parent <- parent_pension_merged[
  !duplicated(parent_pension_merged$parent_id),
]
table(
  one_row_per_parent$special_need,
  one_row_per_parent$pccc_exposure_final,
  useNA = "ifany"
)
table(one_row_per_parent$vammaistuki_label, useNA = "ifany")

# Initial exploratory definition; final six-code definition is in section 2.
dp_records <- parent_pension_merged[
  as.character(parent_pension_merged$LAJI) %in% c("0", "2"),
]
length(unique(dp_records$parent_id))
table(unique(dp_records[c("parent_id", "LAJI")])$LAJI)

dp_one_per_parent <- dp_records[!duplicated(dp_records$parent_id), ]
table(dp_one_per_parent$special_need, useNA = "ifany")

class(dp_records$APVM)
head(dp_records$APVM, 10)
sum(is.na(dp_records$APVM))
dp_records$APVM_date <- as.Date(dp_records$APVM)
dp_records <- dp_records[
  order(dp_records$parent_id, dp_records$APVM_date),
]
first_dp <- dp_records[
  !duplicated(dp_records$parent_id),
  c("parent_id", "special_need", "LAJI", "APVM_date")
]
nrow(first_dp) # Original observed count: 2,015.
table(first_dp$special_need, first_dp$LAJI)
range(first_dp$APVM_date, na.rm = TRUE)
table(first_dp$special_need, first_dp$APVM_date < as.Date("1987-01-01"))

# Compare first pension date with each parent's assigned child diagnosis date.
parent_base <- parent_cohort_1987_master_final
parent_base$parent_id <- as.character(parent_base$parent_id)
sum(duplicated(parent_base$parent_id))
sum(is.na(parent_base$index_date) | trimws(as.character(parent_base$index_date)) == "")

first_dp_dates <- first_dp[c("parent_id", "APVM_date", "LAJI")]
names(first_dp_dates)[2:3] <- c("first_dp_date", "first_dp_type")
parent_dp_check <- merge(
  parent_base, first_dp_dates,
  by = "parent_id", all.x = TRUE, sort = FALSE
)
parent_dp_check$index_date <- as.Date(parent_dp_check$index_date)
parent_dp_check$dp_timing <- "No recorded 0/2 pension"
parent_dp_check$dp_timing[
  !is.na(parent_dp_check$first_dp_date) &
    parent_dp_check$first_dp_date < parent_dp_check$index_date
] <- "Before index"
parent_dp_check$dp_timing[
  !is.na(parent_dp_check$first_dp_date) &
    parent_dp_check$first_dp_date == parent_dp_check$index_date
] <- "On index date"
parent_dp_check$dp_timing[
  !is.na(parent_dp_check$first_dp_date) &
    parent_dp_check$first_dp_date > parent_dp_check$index_date
] <- "After index"
parent_dp_check$dp_timing[is.na(parent_dp_check$index_date)] <-
  "Missing index date"
table(parent_dp_check$special_need, parent_dp_check$dp_timing, useNA = "ifany")

names(parent_dp_check)[
  grepl("death|emigr|date", names(parent_dp_check), ignore.case = TRUE)
]
class(parent_dp_check$death_date)
head(parent_dp_check$death_date, 10)
class(parent_dp_check$emigrated)
table(parent_dp_check$emigrated, useNA = "ifany")
range(parent_dp_check$index_date, na.rm = TRUE)
range(parent_dp_check$first_dp_date, na.rm = TRUE)

parent_dp_check$death_date_parsed <- as.Date(parent_dp_check$death_date)
after_index <- parent_dp_check$dp_timing == "After index"
death_check <- ifelse(
  is.na(parent_dp_check$death_date_parsed),
  "No death recorded",
  ifelse(
    parent_dp_check$first_dp_date <= parent_dp_check$death_date_parsed,
    "Pension on/before death", "Pension after death"
  )
)
table(parent_dp_check$special_need[after_index], death_check[after_index])
table(
  parent_dp_check$special_need[after_index],
  parent_dp_check$emigrated[after_index], useNA = "ifany"
)

parent_dp_check$index_date <- as.Date(parent_dp_check$index_date)
died_before_index <- !is.na(parent_dp_check$death_date_parsed) &
  !is.na(parent_dp_check$index_date) &
  parent_dp_check$death_date_parsed < parent_dp_check$index_date
table(
  group = parent_dp_check$special_need,
  died_before_index = died_before_index, useNA = "ifany"
)
prior_pension <- parent_dp_check$dp_timing == "Before index"
emigrated <- parent_dp_check$emigrated == "Yes"
eligible <- !died_before_index & !prior_pension & !emigrated &
  !is.na(parent_dp_check$index_date)
table(parent_dp_check$special_need[eligible])
table(
  group = parent_dp_check$special_need[eligible],
  pension_event = parent_dp_check$dp_timing[eligible] == "After index"
)

# Exploratory power calculation: original supplied counts retained.
n_special <- 2389
n_control <- 9609
events_total <- 1917
target_HR <- 1.20
alpha <- 0.05
exposed_fraction <- n_special / (n_special + n_control)
information <- events_total * exposed_fraction * (1 - exposed_fraction)
z_alpha <- qnorm(1 - alpha / 2)
power <- pnorm(abs(log(target_HR)) * sqrt(information) - z_alpha)
events_needed_80 <- (z_alpha + qnorm(0.80))^2 /
  (exposed_fraction * (1 - exposed_fraction) * log(target_HR)^2)
minimum_HR_80 <- exp((z_alpha + qnorm(0.80)) / sqrt(information))
cat("Target HR:", target_HR, "\n")
cat("Approximate power:", round(power * 100, 1), "%\n")
cat("Events needed for 80% power:", ceiling(events_needed_80), "\n")
cat("Observed events:", events_total, "\n")
cat("Smallest HR detectable with ~80% power:", round(minimum_HR_80, 2), "\n")

# -----------------------------------------------------------------------------
# 2. Diagnosis-based preparation using the full parent cohort and six DP codes
# -----------------------------------------------------------------------------

c(
  cohort_rows = nrow(parent_cohort_1987_master_final),
  unique_parents = length(unique(na.omit(parent_cohort_1987_master_final$parent_id)))
)
names(parent_cohort_1987_master_final)
names(pension_parents_1987.cohort)
class(pension_parents_1987.cohort$APVM)
head(pension_parents_1987.cohort$APVM)
c(
  rows = nrow(parent_cohort_1987_master_final),
  unique_parents = length(unique(na.omit(parent_cohort_1987_master_final$parent_id))),
  missing_parent_ids = sum(is.na(parent_cohort_1987_master_final$parent_id))
)

dp_restart <- subset(
  pension_parents_1987.cohort,
  trimws(as.character(LAJI)) %in% c("0", "0K", "2", "2K", "0Y", "2Y")
)
dp_restart$APVM_date <- as.Date(dp_restart$APVM, format = "%Y-%m-%d")
c(
  disability_rows = nrow(dp_restart),
  missing_dates = sum(is.na(dp_restart$APVM_date)),
  missing_parent_ids = sum(is.na(dp_restart$FID))
)
first_dp_restart <- aggregate(APVM_date ~ FID, data = dp_restart, FUN = min)
names(first_dp_restart) <- c("parent_id", "first_dp_date")

# Retain all parents, including parents without any pension record.
parent_analysis_restart <- parent_cohort_1987_master_final
parent_analysis_restart$first_dp_date <- first_dp_restart$first_dp_date[
  match(parent_analysis_restart$parent_id, first_dp_restart$parent_id)
]
c(
  total_parents = nrow(parent_analysis_restart),
  with_dp_date = sum(!is.na(parent_analysis_restart$first_dp_date)),
  without_dp_date = sum(is.na(parent_analysis_restart$first_dp_date))
)
sapply(parent_analysis_restart[c("index_date", "death_date", "first_dp_date")], class)
head(parent_analysis_restart[c("index_date", "death_date", "first_dp_date")])

d <- parent_analysis_restart
d$index_date <- as.Date(d$index_date, "%Y-%m-%d")
d$death_date <- as.Date(d$death_date, "%Y-%m-%d")
d$dp_before_index <- with(d, !is.na(first_dp_date) & first_dp_date <= index_date)
d$death_before_index <- with(d, !is.na(death_date) & death_date <= index_date)
c(
  missing_index = sum(is.na(d$index_date)),
  dp_before_or_on_index = sum(d$dp_before_index, na.rm = TRUE),
  death_before_or_on_index = sum(d$death_before_index, na.rm = TRUE),
  either_condition = sum(d$dp_before_index | d$death_before_index, na.rm = TRUE)
)
eligible <- subset(d, !dp_before_index & !death_before_index)
nrow(eligible)
table(eligible$emigrated, useNA = "ifany")
followup_data <- subset(eligible, emigrated == "No")
nrow(followup_data) # Original observed count: 11,995.

study_end <- as.Date("2017-12-31")
followup_data$censor_date <- pmin(followup_data$death_date, study_end, na.rm = TRUE)
followup_data$event <- with(
  followup_data,
  as.integer(!is.na(first_dp_date) & first_dp_date > index_date &
    first_dp_date <= censor_date)
)
followup_data$end_date <- followup_data$censor_date
event_rows <- followup_data$event == 1
followup_data$end_date[event_rows] <- followup_data$first_dp_date[event_rows]
followup_data$followup_days <- as.numeric(followup_data$end_date - followup_data$index_date)
table(followup_data$event, useNA = "ifany")
summary(followup_data$followup_days)
sum(followup_data$followup_days <= 0, na.rm = TRUE)

# Define pension types and first dates from original records.
pension_outcomes <- pension_parents_1987.cohort
pension_outcomes$LAJI_clean <- toupper(trimws(as.character(pension_outcomes$LAJI)))
pension_outcomes$APVM_date <- as.Date(pension_outcomes$APVM, format = "%Y-%m-%d")
pension_outcomes$dp_any <- as.integer(
  pension_outcomes$LAJI_clean %in% c("0", "0K", "0Y", "2", "2K", "2Y")
)
pension_outcomes$dp_full <- as.integer(pension_outcomes$LAJI_clean %in% c("0", "0K", "0Y"))
pension_outcomes$dp_partial <- as.integer(pension_outcomes$LAJI_clean %in% c("2", "2K", "2Y"))
colSums(pension_outcomes[, c("dp_any", "dp_full", "dp_partial")])
first_any <- aggregate(APVM_date ~ FID, data = subset(pension_outcomes, dp_any == 1), FUN = min)
first_full <- aggregate(APVM_date ~ FID, data = subset(pension_outcomes, dp_full == 1), FUN = min)
first_partial <- aggregate(APVM_date ~ FID, data = subset(pension_outcomes, dp_partial == 1), FUN = min)

parent_outcomes <- parent_cohort_1987_master_final
parent_outcomes$first_dp_any_date <- first_any$APVM_date[match(parent_outcomes$parent_id, first_any$FID)]
parent_outcomes$first_dp_full_date <- first_full$APVM_date[match(parent_outcomes$parent_id, first_full$FID)]
parent_outcomes$first_dp_partial_date <- first_partial$APVM_date[match(parent_outcomes$parent_id, first_partial$FID)]
colSums(!is.na(parent_outcomes[c("first_dp_any_date", "first_dp_full_date", "first_dp_partial_date")]))

pension_outcomes$diagnosis_group <- suppressWarnings(
  as.integer(trimws(as.character(pension_outcomes$DRYHMA1)))
)
other_codes <- c(1:4, 6:12, 14, 17, 19, 20)
known_diagnosis_codes <- c(5L, 13L, other_codes)
pension_outcomes$diagnosis_group[
  !pension_outcomes$diagnosis_group %in% known_diagnosis_codes
] <- NA_integer_
pension_outcomes$dp_cause <- NA_character_
pension_outcomes$dp_cause[pension_outcomes$dp_any == 1 & pension_outcomes$diagnosis_group %in% 5] <- "Mental health"
pension_outcomes$dp_cause[pension_outcomes$dp_any == 1 & pension_outcomes$diagnosis_group %in% 13] <- "Musculoskeletal"
pension_outcomes$dp_cause[pension_outcomes$dp_any == 1 & pension_outcomes$diagnosis_group %in% other_codes] <- "Other"
table(pension_outcomes$dp_cause[pension_outcomes$dp_any == 1], useNA = "ifany")
table(pension_outcomes$dp_cause[pension_outcomes$dp_full == 1], useNA = "ifany")
table(pension_outcomes$dp_cause[pension_outcomes$dp_partial == 1], useNA = "ifany")

# Exploratory earliest date for each cause; removed from final analysis copy.
pension_types <- c("any", "full", "partial")
cause_labels <- c(mental = "Mental health", musculoskeletal = "Musculoskeletal", other = "Other")
for (type in pension_types) {
  for (cause in names(cause_labels)) {
    selected <- pension_outcomes[
      pension_outcomes[[paste0("dp_", type)]] == 1 &
        pension_outcomes$dp_cause %in% cause_labels[[cause]],
      c("FID", "APVM_date")
    ]
    variable_name <- paste0("first_dp_", type, "_", cause, "_date")
    parent_outcomes[[variable_name]] <- as.Date(NA)
    if (nrow(selected) > 0) {
      first_dates <- aggregate(APVM_date ~ FID, data = selected, FUN = min)
      parent_outcomes[[variable_name]] <- first_dates$APVM_date[
        match(parent_outcomes$parent_id, first_dates$FID)
      ]
    }
  }
}
diagnosis_date_variables <- unlist(lapply(pension_types, function(type) {
  paste0("first_dp_", type, "_", names(cause_labels), "_date")
}))
colSums(!is.na(parent_outcomes[diagnosis_date_variables]))
outcome_date_variables <- c(
  "first_dp_any_date", "first_dp_full_date", "first_dp_partial_date",
  diagnosis_date_variables
)
parent_match <- match(followup_data$parent_id, parent_outcomes$parent_id)
followup_data[outcome_date_variables] <- parent_outcomes[parent_match, outcome_date_variables]
nrow(followup_data)
sum(is.na(parent_match))
colSums(!is.na(followup_data[outcome_date_variables]))

for (type in c("any", "full", "partial")) {
  outcome_date <- followup_data[[paste0("first_dp_", type, "_date")]]
  event <- !is.na(outcome_date) & outcome_date > followup_data$index_date &
    outcome_date <= followup_data$censor_date
  end_date <- followup_data$censor_date
  end_date[event] <- outcome_date[event]
  followup_data[[paste0("dp_", type, "_event")]] <- as.integer(event)
  followup_data[[paste0("dp_", type, "_end_date")]] <- end_date
  followup_data[[paste0("dp_", type, "_followup_days")]] <-
    as.numeric(end_date - followup_data$index_date)
}
colSums(followup_data[c("dp_any_event", "dp_full_event", "dp_partial_event")])
sapply(followup_data[c("dp_any_followup_days", "dp_full_followup_days", "dp_partial_followup_days")], function(x) {
  c(missing = sum(is.na(x)), non_positive = sum(x <= 0, na.rm = TRUE))
})

# RECONSTRUCTED SETUP: definitions absent from the screenshots.
# Keep every record on the parent's first any-DP date, as in the full/partial
# code below; count distinct known DRYHMA1 codes on that date.
any_records <- subset(pension_outcomes, dp_any == 1 & FID %in% parent_outcomes$parent_id)
any_dates <- parent_outcomes$first_dp_any_date[match(any_records$FID, parent_outcomes$parent_id)]
first_dp_records <- any_records[
  !is.na(any_dates) & !is.na(any_records$APVM_date) & any_records$APVM_date == any_dates,
]
cause_counts <- tapply(first_dp_records$diagnosis_group, as.character(first_dp_records$FID), function(x) {
  length(unique(x[!is.na(x)]))
})
# End reconstructed setup.

conflicting_ids <- names(cause_counts)[cause_counts > 1]
length(conflicting_ids)
conflicting_history <- pension_outcomes[
  pension_outcomes$dp_any == 1 & as.character(pension_outcomes$FID) %in% conflicting_ids,
]
first_cause <- tapply(first_dp_records$diagnosis_group, as.character(first_dp_records$FID), function(x) {
  known <- unique(x[!is.na(x)])
  if (length(known) == 0) return("Unknown")
  if (length(known) > 1) return("Multiple diagnoses")
  if (known[[1]] == 5L) return("Mental health")
  if (known[[1]] == 13L) return("Musculoskeletal")
  "Other"
})
parent_outcomes$first_dp_any_cause <- unname(first_cause[
  match(as.character(parent_outcomes$parent_id), names(first_cause))
])
followup_data$first_dp_any_cause <- parent_outcomes$first_dp_any_cause[
  match(followup_data$parent_id, parent_outcomes$parent_id)
]
table(followup_data$first_dp_any_cause[followup_data$dp_any_event == 1], useNA = "ifany")

for (cause in names(cause_labels)) {
  followup_data[[paste0("dp_any_", cause, "_event")]] <- as.integer(
    followup_data$dp_any_event == 1 & followup_data$first_dp_any_cause %in% cause_labels[[cause]]
  )
  followup_data[[paste0("dp_any_", cause, "_end_date")]] <- followup_data$dp_any_end_date
  followup_data[[paste0("dp_any_", cause, "_followup_days")]] <- followup_data$dp_any_followup_days
}
colSums(followup_data[c("dp_any_mental_event", "dp_any_musculoskeletal_event", "dp_any_other_event")])

full_records <- subset(pension_outcomes, dp_full == 1 & FID %in% parent_outcomes$parent_id)
full_dates <- parent_outcomes$first_dp_full_date[match(full_records$FID, parent_outcomes$parent_id)]
first_full_records <- full_records[
  !is.na(full_dates) & !is.na(full_records$APVM_date) & full_records$APVM_date == full_dates,
]
full_cause <- tapply(first_full_records$diagnosis_group, as.character(first_full_records$FID), function(x) {
  causes <- unique(x[!is.na(x)])
  if (length(causes) == 0) return("Unknown")
  if (length(causes) > 1) return("Multiple diagnoses")
  if (causes[[1]] == 5L) return("Mental health")
  if (causes[[1]] == 13L) return("Musculoskeletal")
  "Other"
})
followup_data$first_dp_full_cause <- unname(full_cause[as.character(followup_data$parent_id)])
table(followup_data$first_dp_full_cause[followup_data$dp_full_event == 1], useNA = "ifany")

partial_records <- subset(pension_outcomes, dp_partial == 1 & FID %in% parent_outcomes$parent_id)
partial_dates <- parent_outcomes$first_dp_partial_date[match(partial_records$FID, parent_outcomes$parent_id)]
first_partial_records <- partial_records[
  !is.na(partial_dates) & !is.na(partial_records$APVM_date) & partial_records$APVM_date == partial_dates,
]
partial_cause <- tapply(first_partial_records$diagnosis_group, as.character(first_partial_records$FID), function(x) {
  causes <- unique(x[!is.na(x)])
  if (length(causes) == 0) return("Unknown")
  if (length(causes) > 1) return("Multiple diagnoses")
  if (causes[[1]] == 5L) return("Mental health")
  if (causes[[1]] == 13L) return("Musculoskeletal")
  "Other"
})
followup_data$first_dp_partial_cause <- unname(partial_cause[as.character(followup_data$parent_id)])
table(followup_data$first_dp_partial_cause[followup_data$dp_partial_event == 1], useNA = "ifany")

followup_data$dp_full_mental_event <- as.integer(followup_data$dp_full_event == 1 & followup_data$first_dp_full_cause %in% "Mental health")
followup_data$dp_full_musculoskeletal_event <- as.integer(followup_data$dp_full_event == 1 & followup_data$first_dp_full_cause %in% "Musculoskeletal")
followup_data$dp_full_other_event <- as.integer(followup_data$dp_full_event == 1 & followup_data$first_dp_full_cause %in% "Other")
followup_data$dp_partial_mental_event <- as.integer(followup_data$dp_partial_event == 1 & followup_data$first_dp_partial_cause %in% "Mental health")
followup_data$dp_partial_musculoskeletal_event <- as.integer(followup_data$dp_partial_event == 1 & followup_data$first_dp_partial_cause %in% "Musculoskeletal")
followup_data$dp_partial_other_event <- as.integer(followup_data$dp_partial_event == 1 & followup_data$first_dp_partial_cause %in% "Other")
colSums(followup_data[c(
  "dp_full_mental_event", "dp_full_musculoskeletal_event", "dp_full_other_event",
  "dp_partial_mental_event", "dp_partial_musculoskeletal_event", "dp_partial_other_event"
)])

# Create all five cause indicators for every pension type.
# These indicators also cover the two confirmed within-Other code conflicts.
cause_labels <- c(
  mental = "Mental health", musculoskeletal = "Musculoskeletal", other = "Other",
  multiple = "Multiple diagnoses", unknown = "Unknown"
)
for (type in c("any", "full", "partial")) {
  event <- followup_data[[paste0("dp_", type, "_event")]]
  cause <- followup_data[[paste0("first_dp_", type, "_cause")]]
  for (group in names(cause_labels)) {
    followup_data[[paste0("dp_", type, "_", group, "_event")]] <-
      as.integer(event == 1L & cause %in% cause_labels[[group]])
  }
  stopifnot(all(rowSums(followup_data[
    paste0("dp_", type, "_", names(cause_labels), "_event")
  ]) == event))
}

event_variables <- c(
  "dp_any_event", "dp_full_event", "dp_partial_event",
  "dp_any_mental_event", "dp_any_musculoskeletal_event", "dp_any_other_event",
  "dp_full_mental_event", "dp_full_musculoskeletal_event", "dp_full_other_event",
  "dp_partial_mental_event", "dp_partial_musculoskeletal_event", "dp_partial_other_event"
)
event_variables <- unique(c(event_variables, unlist(lapply(
  c("any", "full", "partial"),
  function(type) paste0("dp_", type, "_", c("multiple", "unknown"), "_event")
))))
sapply(followup_data[event_variables], function(x) {
  c(missing = sum(is.na(x)), invalid = sum(!is.na(x) & !(x %in% c(0, 1))))
})
nrow(followup_data)
anyDuplicated(followup_data$parent_id)
parent_pension_outcomes_1987 <- followup_data
saveRDS(parent_pension_outcomes_1987, "parent_pension_outcomes_1987.rds")
file.exists("parent_pension_outcomes_1987.rds")
getwd()

remove_vars <- c(
  "first_dp_date", "event", "end_date", "followup_days",
  "first_dp_any_mental_date", "first_dp_any_musculoskeletal_date", "first_dp_any_other_date",
  "first_dp_full_mental_date", "first_dp_full_musculoskeletal_date", "first_dp_full_other_date",
  "first_dp_partial_mental_date", "first_dp_partial_musculoskeletal_date", "first_dp_partial_other_date",
  "dp_any_mental_end_date", "dp_any_musculoskeletal_end_date", "dp_any_other_end_date",
  "dp_any_mental_followup_days", "dp_any_musculoskeletal_followup_days", "dp_any_other_followup_days"
)
parent_pension_analysis_1987 <- parent_pension_outcomes_1987[
  , !names(parent_pension_outcomes_1987) %in% remove_vars
]
dim(parent_pension_analysis_1987)

# -----------------------------------------------------------------------------
# 3. Dictionary for the original diagnosis-based analysis copy
# -----------------------------------------------------------------------------

# RECONSTRUCTED SETUP: dictionary initialisation was not visible. These columns
# are those used by the dictionary editing code in the screenshots.
d <- parent_pension_analysis_1987
dictionary <- data.frame(
  variable = names(d),
  description = "",
  data_type = vapply(d, function(x) paste(class(x), collapse = "; "), character(1)),
  coding_or_units = "",
  notes = "",
  stringsAsFactors = FALSE
)
# End reconstructed setup.

descriptions <- c(
  parent_id = "Unique parent identifier",
  parent_type = "Parent category",
  special_need = "Child special healthcare needs exposure group",
  pccc_exposure_final = "Child chronic condition classification",
  vammaistuki_label = "Child disability allowance category",
  multimorbidity_label = "Child multimorbidity category",
  first_exposure_date = "First qualifying child exposure date",
  index_date = "Start of parent follow-up",
  first_exposure_year = "Year of first qualifying child exposure",
  index_year = "Year parent follow-up starts",
  first_cancer_date = "First qualifying child cancer date",
  first_congenital_date = "First qualifying child congenital condition date",
  first_other_chronic_date = "First qualifying other child chronic condition date",
  any_child_cancer = "Indicator of qualifying cancer in any linked child",
  any_child_congenital = "Indicator of a qualifying congenital condition in any linked child",
  any_child_other_chronic = "Indicator of another qualifying chronic condition in any linked child",
  n_children = "Number of linked children",
  date_of_birth = "Parent date of birth",
  mother_tongue = "Parent mother tongue",
  postcode = "Parent postal code",
  death_date = "Parent death date",
  emigrated = "Parent emigration status",
  parent_died = "Parent death indicator",
  age_at_index = "Parent age at index",
  married_at_index = "Parent marital status measured in 1987",
  region2 = "Parent region classification",
  twin_birth_1987 = "Twin birth indicator in the 1987 cohort",
  dp_before_index = "Disability pension at or before index",
  death_before_index = "Parent death at or before index",
  censor_date = "Earlier of parent death date and 2017-12-31"
)
i <- match(dictionary$variable, names(descriptions))
dictionary$description <- unname(descriptions[i])
dictionary$description[is.na(dictionary$description)] <- ""
cause_labels <- c(
  mental = "Mental health", musculoskeletal = "Musculoskeletal", other = "Other",
  multiple = "Multiple diagnoses", unknown = "Unknown"
)

for (type in c("any", "full", "partial")) {
  definitions <- setNames(
    c(
      paste("First recorded", type, "disability pension date"),
      paste("First", type, "disability pension during follow-up, regardless of cause"),
      paste("End date of", type, "disability pension follow-up"),
      paste("Days from index to end of", type, "disability pension follow-up"),
      paste("Cause category at the first", type, "disability pension")
    ),
    c(
      paste0("first_dp_", type, "_date"),
      paste0("dp_", type, "_event"),
      paste0("dp_", type, "_end_date"),
      paste0("dp_", type, "_followup_days"),
      paste0("first_dp_", type, "_cause")
    )
  )
  for (cause in names(cause_labels)) {
    definitions[paste0("dp_", type, "_", cause, "_event")] <- paste(
      "First", type, "disability pension occurs during follow-up",
      "and its cause category is", cause_labels[[cause]]
    )
  }
  for (v in intersect(names(definitions), dictionary$variable)) {
    dictionary$description[dictionary$variable == v] <- definitions[[v]]
  }
  v <- paste0("dp_", type, "_event")
  dictionary$notes[dictionary$variable == v] <-
    "Event=1 if first pension date is after index and on or before censor date; otherwise 0."
  v <- paste0("dp_", type, "_end_date")
  dictionary$notes[dictionary$variable == v] <-
    "First pension date of this type if an event occurs; otherwise censor date."
  for (cause in names(cause_labels)) {
    v <- paste0("dp_", type, "_", cause, "_event")
    dictionary$notes[dictionary$variable == v] <- paste(
      "Event=0 for any non-target cause or no pension.",
      "Follow-up ends at the first pension of this type regardless of cause,",
      "or at censor date if no pension occurs during follow-up."
    )
  }
}
dictionary$coding_or_units[grepl("_event$", dictionary$variable)] <-
  "0 = no corresponding event; 1 = corresponding event"
dictionary$coding_or_units[grepl("_followup_days$", dictionary$variable)] <- "Days"
dictionary$coding_or_units[
  grepl("_date$", dictionary$variable) | dictionary$variable == "date_of_birth"
] <- "YYYY-MM-DD"
dictionary$coding_or_units[grepl("_cause$", dictionary$variable)] <- paste(
  "Mental health; Musculoskeletal; Other;",
  "Multiple diagnoses; Unknown; NA = no corresponding pension"
)
dictionary$coding_or_units[dictionary$variable == "age_at_index"] <- "Years"
for (v in names(d)) {
  x <- d[[v]]
  if (is.character(x) || is.factor(x) || is.logical(x)) {
    values <- sort(unique(as.character(x[!is.na(x)])))
    i <- match(v, dictionary$variable)
    if (length(values) > 0 && length(values) <= 15 && dictionary$coding_or_units[i] == "") {
      dictionary$coding_or_units[i] <- paste(values, collapse = "; ")
    }
  }
}
dictionary$notes[dictionary$variable == "index_date"] <-
  "Child exposure index date for exposed parents; corresponding assigned date for comparison parents."
dictionary$notes[dictionary$variable == "censor_date"] <-
  "Study end is used if death date is missing. Old-age retirement censoring is not included in this original analysis copy."
dictionary$notes[dictionary$variable %in% c("dp_before_index", "death_before_index")] <-
  "Parents meeting this exclusion criterion were removed from the analysis."
dictionary$notes[dictionary$variable == "emigrated"] <-
  "Parents recorded as emigrated were excluded because emigration dates were unavailable."
pension_codes <- c(
  any = "LAJI: 0, 0K, 0Y, 2, 2K, 2Y",
  full = "LAJI: 0, 0K, 0Y",
  partial = "LAJI: 2, 2K, 2Y"
)
for (type in names(pension_codes)) {
  v <- paste0("first_dp_", type, "_date")
  dictionary$notes[dictionary$variable == v] <- paste(
    "Earliest APVM among included records.", pension_codes[[type]],
    "ETK earnings-related pensions only."
  )
  v <- paste0("first_dp_", type, "_cause")
  dictionary$notes[dictionary$variable == v] <- paste(
    "DRYHMA1: 05 = Mental health; 13 = Musculoskeletal;",
    "01-04, 06-12, 14, 17, 19, 20 = Other; 21 = missing.",
    "Multiple diagnoses means more than one distinct known DRYHMA1 code",
    "on the same first pension date, including different codes within Other."
  )
}
dictionary$description[dictionary$description == ""] <- "Definition needs checking"
stopifnot(nrow(dictionary) == ncol(d), identical(dictionary$variable, names(d)))
# Dictionary export filename reconstructed: it was below the screenshot edge.
write.csv(dictionary, "parent_pension_analysis_1987_data_dictionary.csv", row.names = FALSE)
write.csv(parent_pension_analysis_1987, "parent_pension_analysis_1987.csv", row.names = FALSE, na = "")
getwd()
save.image(file = "parent_pension_7_10_workspace_1987.RData")
getwd()

# -----------------------------------------------------------------------------
# 4. Add retirement censoring to the diagnosis-based dataset
# -----------------------------------------------------------------------------

p <- pension_parents_1987.cohort
retirement <- data.frame(
  parent_id = as.character(p$FID),
  retirement_date = as.Date(p$APVM),
  stringsAsFactors = FALSE
)
keep <- toupper(trimws(as.character(p$LAJI))) %in% c("5", "5A") & !is.na(retirement$retirement_date)
retirement <- retirement[keep, ]
first_retirement <- aggregate(retirement_date ~ parent_id, data = retirement, FUN = min)
d <- parent_pension_analysis_1987
d$parent_id <- as.character(d$parent_id)
d$first_old_age_date <- first_retirement$retirement_date[
  match(d$parent_id, first_retirement$parent_id)
]
retired_at_index <- !is.na(d$first_old_age_date) & d$first_old_age_date <= d$index_date
sum(retired_at_index) # Original observed count: 6.
d <- d[!retired_at_index, ]
d$censor_date <- pmin(as.Date(d$death_date), d$first_old_age_date, as.Date("2017-12-31"), na.rm = TRUE)

for (type in c("any", "full", "partial")) {
  date_var <- paste0("first_dp_", type, "_date")
  event_var <- paste0("dp_", type, "_event")
  end_var <- paste0("dp_", type, "_end_date")
  time_var <- paste0("dp_", type, "_followup_days")
  dp_date <- as.Date(d[[date_var]])
  event <- !is.na(dp_date) & dp_date > d$index_date & dp_date <= d$censor_date
  d[[event_var]] <- as.integer(event)
  end_date <- d$censor_date
  end_date[event] <- dp_date[event]
  d[[end_var]] <- end_date
  d[[time_var]] <- as.numeric(end_date - d$index_date)
  for (cause in c("mental", "musculoskeletal", "other", "multiple", "unknown")) {
    cause_var <- paste0("dp_", type, "_", cause, "_event")
    d[[cause_var]][!event] <- 0L
  }
}
print(c(
  rows = nrow(d), unique_parents = length(unique(d$parent_id)),
  any_dp_events = sum(d$dp_any_event), full_dp_events = sum(d$dp_full_event),
  partial_dp_events = sum(d$dp_partial_event)
))
time_vars <- paste0("dp_", c("any", "full", "partial"), "_followup_days")
stopifnot(
  !anyDuplicated(d$parent_id),
  all(vapply(d[time_vars], function(x) all(!is.na(x) & x > 0), logical(1)))
)
parent_pension_analysis_1987_retirement <- d
saveRDS(parent_pension_analysis_1987_retirement, "parent_pension_analysis_1987_retirement.rds")
write.csv(parent_pension_analysis_1987_retirement, "parent_pension_analysis_1987_retirement.csv", row.names = FALSE, na = "")
# Exact second copy of the retirement extraction/update block was removed.

# -----------------------------------------------------------------------------
# 5. Birth-based preparation: link child birth dates to parents
# -----------------------------------------------------------------------------

library(dplyr)
library(tidyr)

child_birth_link <- matched_cohort_1987_allexposures_final %>%
  transmute(
    child_id = as.character(FID), child_birth_date = as.Date(DATE_OF_BIRTH),
    mother = as.character(FID_M), father = as.character(FID_F)
  ) %>%
  pivot_longer(
    cols = c(mother, father), names_to = "child_parent_role", values_to = "parent_id"
  ) %>%
  filter(!is.na(parent_id), parent_id != "") %>%
  distinct()
child_birth_link %>% summarise(
  rows = n(), unique_parents = n_distinct(parent_id),
  missing_child_birth = sum(is.na(child_birth_date))
)
multiple_children <- child_birth_link %>%
  count(parent_id, name = "n_cohort_children") %>% filter(n_cohort_children > 1)
nrow(multiple_children)
unlinked_parents <- parent_cohort_1987_master_final %>%
  mutate(parent_id = as.character(parent_id)) %>% anti_join(child_birth_link, by = "parent_id")
nrow(unlinked_parents)
birth_date_check <- child_birth_link %>%
  semi_join(multiple_children, by = "parent_id") %>% group_by(parent_id) %>%
  summarise(n_children = n_distinct(child_id), n_birth_dates = n_distinct(child_birth_date), .groups = "drop")
birth_date_check %>% count(n_birth_dates)

parent_birth_dates <- child_birth_link %>% distinct(parent_id, child_birth_date)
stopifnot(!anyDuplicated(parent_birth_dates$parent_id))
parent_cohort_1987_birth <- parent_cohort_1987_master_final %>%
  mutate(parent_id = as.character(parent_id)) %>%
  left_join(parent_birth_dates, by = "parent_id") %>%
  mutate(followup_start_birth = child_birth_date)
stopifnot(
  nrow(parent_cohort_1987_birth) == nrow(parent_cohort_1987_master_final),
  !anyNA(parent_cohort_1987_birth$followup_start_birth)
)
parent_cohort_1987_birth %>% summarise(
  rows = n(), unique_parents = n_distinct(parent_id),
  earliest_birth = min(followup_start_birth), latest_birth = max(followup_start_birth)
)
str(pension_parents_1987.cohort$APVM)
head(pension_parents_1987.cohort$APVM, 10)

pension_birth_prepared <- pension_parents_1987.cohort %>% mutate(
  parent_id = as.character(FID),
  LAJI = toupper(trimws(as.character(LAJI))),
  APVM_date = as.Date(APVM, format = "%Y-%m-%d"),
  diagnosis_group = suppressWarnings(as.integer(trimws(as.character(DRYHMA1)))),
  diagnosis_group = if_else(
    diagnosis_group %in% known_diagnosis_codes, diagnosis_group, NA_integer_
  )
)
pension_birth_prepared %>% summarise(
  pension_rows = n(), missing_APVM = sum(is.na(APVM) | trimws(APVM) == ""),
  failed_conversion = sum(!is.na(APVM) & trimws(APVM) != "" & is.na(APVM_date))
)
laji_counts <- pension_birth_prepared %>%
  dplyr::count(LAJI, sort = TRUE)
print(as.data.frame(laji_counts), row.names = FALSE)

dp_codes <- c("0", "0K", "2", "2K", "0Y", "2Y")
first_dp_birth <- pension_birth_prepared %>% filter(LAJI %in% dp_codes, !is.na(APVM_date)) %>%
  group_by(parent_id) %>% summarise(first_dp_date = min(APVM_date), .groups = "drop")
parent_pension_birth_check <- parent_cohort_1987_birth %>%
  left_join(first_dp_birth, by = "parent_id") %>% mutate(
    dp_on_or_before_birth = !is.na(first_dp_date) & first_dp_date <= followup_start_birth,
    dp_after_birth = !is.na(first_dp_date) & first_dp_date > followup_start_birth
  )
parent_pension_birth_check %>% summarise(
  total_parents = n(), parents_with_recorded_dp = sum(!is.na(first_dp_date)),
  dp_on_or_before_birth = sum(dp_on_or_before_birth), dp_after_birth = sum(dp_after_birth),
  no_recorded_dp = sum(is.na(first_dp_date))
)
str(parent_pension_birth_check$date_of_birth)
str(parent_pension_birth_check$death_date)
parent_pension_birth_check <- parent_pension_birth_check %>% mutate(
  parent_birth_date = as.Date(date_of_birth, format = "%Y-%m-%d"),
  parent_death_date = as.Date(death_date, format = "%Y-%m-%d"),
  death_on_or_before_birth = !is.na(parent_death_date) & parent_death_date <= followup_start_birth,
  age_at_child_birth = as.numeric(followup_start_birth - parent_birth_date) / 365.25,
  eligible_birth_initial = !dp_on_or_before_birth & !death_on_or_before_birth
)
parent_pension_birth_check %>% summarise(
  total_parents = n(), pre_existing_dp = sum(dp_on_or_before_birth),
  death_on_or_before_birth = sum(death_on_or_before_birth),
  excluded_either_reason = sum(!eligible_birth_initial),
  initially_eligible = sum(eligible_birth_initial),
  missing_parent_birth_date = sum(is.na(parent_birth_date))
)
parent_pension_birth_eligible <- parent_pension_birth_check %>% filter(eligible_birth_initial)
stopifnot(
  !anyDuplicated(parent_pension_birth_eligible$parent_id),
  !any(parent_pension_birth_eligible$dp_on_or_before_birth),
  !any(parent_pension_birth_eligible$death_on_or_before_birth)
)
table(parent_pension_birth_eligible$emigrated, useNA = "ifany")
names(parent_pension_analysis_1987_retirement)

first_retirement_birth <- pension_birth_prepared %>% filter(LAJI %in% c("5", "5A"), !is.na(APVM_date)) %>%
  group_by(parent_id) %>% summarise(first_old_age_date = min(APVM_date), .groups = "drop")
parent_pension_birth_retirement_check <- parent_pension_birth_eligible %>%
  filter(emigrated == "No") %>% left_join(first_retirement_birth, by = "parent_id") %>%
  mutate(retirement_on_or_before_birth = !is.na(first_old_age_date) & first_old_age_date <= followup_start_birth)
parent_pension_birth_retirement_check %>% summarise(
  non_emigrated_parents = n(), retired_on_or_before_birth = sum(retirement_on_or_before_birth)
)

parent_pension_analysis_1987_birth <- parent_pension_birth_retirement_check %>%
  filter(!retirement_on_or_before_birth) %>% mutate(
    censor_date_birth = pmin(as.Date("2017-12-31"), parent_death_date, first_old_age_date, na.rm = TRUE),
    # Agreed common rule: include DP on the censoring date.
    dp_any_event = as.integer(
      !is.na(first_dp_date) & first_dp_date > followup_start_birth &
        first_dp_date <= censor_date_birth
    ),
    dp_any_end_date = if_else(dp_any_event == 1L, first_dp_date, censor_date_birth),
    dp_any_followup_days = as.numeric(dp_any_end_date - followup_start_birth)
  )
stopifnot(
  !anyDuplicated(parent_pension_analysis_1987_birth$parent_id),
  !anyNA(parent_pension_analysis_1987_birth$dp_any_followup_days),
  all(parent_pension_analysis_1987_birth$dp_any_followup_days > 0)
)
parent_pension_analysis_1987_birth %>% summarise(
  parents = n(), dp_events = sum(dp_any_event), censored = sum(dp_any_event == 0L),
  total_person_years = sum(dp_any_followup_days) / 365.25
)
parent_pension_analysis_1987_birth %>% summarise(
  dp_same_day_as_death = sum(!is.na(first_dp_date) & !is.na(parent_death_date) & first_dp_date == parent_death_date),
  dp_same_day_as_retirement = sum(!is.na(first_dp_date) & !is.na(first_old_age_date) & first_dp_date == first_old_age_date)
)
parent_pension_analysis_1987_birth %>% group_by(special_need) %>% summarise(
  parents = n(), dp_events = sum(dp_any_event), censored = sum(dp_any_event == 0L),
  person_years = sum(dp_any_followup_days) / 365.25,
  rate_per_1000_py = dp_events / person_years * 1000, .groups = "drop"
)

# RECONSTRUCTED SETUP: first_dp_diagnosis_rows was not defined in the screenshots.
# Match all DP records on the first any-DP date; use normalised known codes.
first_dp_diagnosis_rows <- pension_birth_prepared %>% filter(LAJI %in% dp_codes) %>%
  inner_join(first_dp_birth, by = "parent_id") %>% filter(APVM_date == first_dp_date) %>%
  mutate(primary_diagnosis_group = diagnosis_group)
# End reconstructed setup.

first_dp_cause_birth <- first_dp_diagnosis_rows %>% group_by(parent_id) %>% summarise(
  n_primary_groups = n_distinct(primary_diagnosis_group, na.rm = TRUE),
  primary_group = first(primary_diagnosis_group[!is.na(primary_diagnosis_group)], default = NA_integer_),
  .groups = "drop"
) %>% mutate(first_dp_any_cause = case_when(
  n_primary_groups > 1 ~ "Multiple diagnoses",
  n_primary_groups == 0 ~ "Unknown",
  primary_group == 5L ~ "Mental health",
  primary_group == 13L ~ "Musculoskeletal",
  TRUE ~ "Other"
))
parent_pension_analysis_1987_birth <- parent_pension_analysis_1987_birth %>%
  left_join(first_dp_cause_birth, by = "parent_id")
parent_pension_analysis_1987_birth %>% filter(dp_any_event == 1L) %>% count(first_dp_any_cause)
parent_pension_analysis_1987_birth <- parent_pension_analysis_1987_birth %>% mutate(
  dp_any_mental_event = as.integer(dp_any_event == 1L & first_dp_any_cause %in% "Mental health"),
  dp_any_musculoskeletal_event = as.integer(dp_any_event == 1L & first_dp_any_cause %in% "Musculoskeletal"),
  dp_any_other_event = as.integer(dp_any_event == 1L & first_dp_any_cause %in% "Other"),
  dp_any_multiple_event = as.integer(dp_any_event == 1L & first_dp_any_cause %in% "Multiple diagnoses"),
  dp_any_unknown_event = as.integer(dp_any_event == 1L & first_dp_any_cause %in% "Unknown"),
  first_dp_any_date = first_dp_date
)
# Every event belongs to exactly one of the five cause categories.
stopifnot(with(parent_pension_analysis_1987_birth, all(
  dp_any_mental_event + dp_any_musculoskeletal_event + dp_any_other_event +
    dp_any_multiple_event + dp_any_unknown_event == dp_any_event
)))

full_dp_codes <- c("0", "0K", "0Y")
partial_dp_codes <- c("2", "2K", "2Y")
first_full_dp_birth <- pension_birth_prepared %>% filter(LAJI %in% full_dp_codes, !is.na(APVM_date)) %>%
  group_by(parent_id) %>% summarise(first_dp_full_date = min(APVM_date), .groups = "drop")
first_partial_dp_birth <- pension_birth_prepared %>% filter(LAJI %in% partial_dp_codes, !is.na(APVM_date)) %>%
  group_by(parent_id) %>% summarise(first_dp_partial_date = min(APVM_date), .groups = "drop")
parent_pension_analysis_1987_birth <- parent_pension_analysis_1987_birth %>%
  left_join(first_full_dp_birth, by = "parent_id") %>% left_join(first_partial_dp_birth, by = "parent_id")

# One copy of the original full/partial update loop is retained.
for (dp_type in c("full", "partial")) {
  date_column <- paste0("first_dp_", dp_type, "_date")
  event_date <- parent_pension_analysis_1987_birth[[date_column]]
  event_flag <- with(parent_pension_analysis_1987_birth,
    !is.na(event_date) & event_date > followup_start_birth &
      event_date <= censor_date_birth
  )
  end_date <- dplyr::if_else(event_flag, event_date, parent_pension_analysis_1987_birth$censor_date_birth)
  parent_pension_analysis_1987_birth[[paste0("dp_", dp_type, "_event")]] <- as.integer(event_flag)
  parent_pension_analysis_1987_birth[[paste0("dp_", dp_type, "_end_date")]] <- end_date
  parent_pension_analysis_1987_birth[[paste0("dp_", dp_type, "_followup_days")]] <-
    as.numeric(end_date - parent_pension_analysis_1987_birth$followup_start_birth)
}
stopifnot(
  all(parent_pension_analysis_1987_birth$dp_full_followup_days > 0),
  all(parent_pension_analysis_1987_birth$dp_partial_followup_days > 0)
)
parent_pension_analysis_1987_birth %>% group_by(special_need) %>% summarise(
  parents = n(), any_dp = sum(dp_any_event), full_dp = sum(dp_full_event),
  partial_dp = sum(dp_partial_event), .groups = "drop"
)

# Extract the cause from all records on the first date of each DP type.
get_first_dp_cause <- function(codes, first_dates, date_column) {
  first_dates_local <- first_dates %>% transmute(parent_id, first_type_date = .data[[date_column]])
  pension_birth_prepared %>% filter(LAJI %in% codes) %>%
    inner_join(first_dates_local, by = "parent_id") %>% filter(APVM_date == first_type_date) %>%
    group_by(parent_id) %>% summarise(
      n_groups = n_distinct(diagnosis_group, na.rm = TRUE),
      primary_group = first(diagnosis_group[!is.na(diagnosis_group)], default = NA_integer_),
      .groups = "drop"
    ) %>% mutate(cause = case_when(
      n_groups > 1 ~ "Multiple diagnoses",
      n_groups == 0 ~ "Unknown",
      primary_group == 5L ~ "Mental health",
      primary_group == 13L ~ "Musculoskeletal",
      TRUE ~ "Other"
    )) %>% select(parent_id, cause)
}
full_dp_causes <- get_first_dp_cause(full_dp_codes, first_full_dp_birth, "first_dp_full_date") %>%
  rename(first_dp_full_cause = cause)
partial_dp_causes <- get_first_dp_cause(partial_dp_codes, first_partial_dp_birth, "first_dp_partial_date") %>%
  rename(first_dp_partial_cause = cause)
parent_pension_analysis_1987_birth <- parent_pension_analysis_1987_birth %>%
  select(-any_of(c("first_dp_full_cause", "first_dp_partial_cause"))) %>%
  left_join(full_dp_causes, by = "parent_id") %>% left_join(partial_dp_causes, by = "parent_id")
parent_pension_analysis_1987_birth %>% filter(dp_full_event == 1L) %>% count(first_dp_full_cause)
parent_pension_analysis_1987_birth %>% filter(dp_partial_event == 1L) %>% count(first_dp_partial_cause)

cause_labels <- c(
  mental = "Mental health", musculoskeletal = "Musculoskeletal", other = "Other",
  multiple = "Multiple diagnoses", unknown = "Unknown"
)
for (dp_type in c("full", "partial")) {
  event_column <- paste0("dp_", dp_type, "_event")
  cause_column <- paste0("first_dp_", dp_type, "_cause")
  event_flag <- parent_pension_analysis_1987_birth[[event_column]]
  cause <- parent_pension_analysis_1987_birth[[cause_column]]
  for (cause_name in names(cause_labels)) {
    output_column <- paste0("dp_", dp_type, "_", cause_name, "_event")
    parent_pension_analysis_1987_birth[[output_column]] <- as.integer(
      event_flag == 1L & cause %in% cause_labels[[cause_name]]
    )
  }
  cause_columns <- paste0("dp_", dp_type, "_", names(cause_labels), "_event")
  stopifnot(!anyNA(event_flag), all(rowSums(parent_pension_analysis_1987_birth[cause_columns]) == event_flag))
}

# -----------------------------------------------------------------------------
# 6. Original final validation and exports of the birth-based cohort
# -----------------------------------------------------------------------------

dat <- parent_pension_analysis_1987_birth
admin_end <- as.Date("2017-12-31")
types <- c("any", "full", "partial")
causes <- c("mental", "musculoskeletal", "other", "multiple", "unknown")
required <- c(
  "parent_id", "special_need", "followup_start_birth", "parent_death_date",
  "first_old_age_date", "emigrated", "first_dp_date", "first_dp_full_date",
  "first_dp_partial_date", "censor_date_birth",
  paste0("dp_", types, "_event"), paste0("dp_", types, "_end_date"),
  paste0("dp_", types, "_followup_days"), paste0("first_dp_", types, "_cause"),
  unlist(lapply(types, function(x) paste0("dp_", x, "_", causes, "_event")))
)
missing_columns <- setdiff(required, names(dat))
print(missing_columns)
stopifnot(length(missing_columns) == 0L)
stopifnot(
  !anyNA(dat$parent_id), !anyDuplicated(dat$parent_id),
  all(dat$emigrated == "No", na.rm = FALSE), !anyNA(dat$followup_start_birth),
  all(format(dat$followup_start_birth, "%Y") == "1987"),
  all(is.na(dat$first_dp_date) | dat$first_dp_date > dat$followup_start_birth),
  all(is.na(dat$parent_death_date) | dat$parent_death_date > dat$followup_start_birth),
  all(is.na(dat$first_old_age_date) | dat$first_old_age_date > dat$followup_start_birth)
)
expected_censor <- pmin(admin_end, dat$parent_death_date, dat$first_old_age_date, na.rm = TRUE)
stopifnot(!anyNA(dat$censor_date_birth), all(dat$censor_date_birth == expected_censor))
event_dates <- c(any = "first_dp_date", full = "first_dp_full_date", partial = "first_dp_partial_date")
validation_summary <- list()

for (type in types) {
  event <- dat[[paste0("dp_", type, "_event")]]
  end_date <- dat[[paste0("dp_", type, "_end_date")]]
  days <- dat[[paste0("dp_", type, "_followup_days")]]
  event_date <- dat[[event_dates[[type]]]]
  expected_event <- as.integer(
    !is.na(event_date) & event_date > dat$followup_start_birth &
      event_date <= dat$censor_date_birth
  )
  expected_end <- dat$censor_date_birth
  expected_end[expected_event == 1L] <- event_date[expected_event == 1L]
  cause_columns <- paste0("dp_", type, "_", causes, "_event")
  cause_flags <- as.matrix(dat[cause_columns])
  stopifnot(
    !anyNA(event), all(event %in% c(0L, 1L)), all(event == expected_event),
    !anyNA(end_date), all(end_date == expected_end),
    !anyNA(days), all(is.finite(days)), all(days > 0),
    all(days == as.numeric(end_date - dat$followup_start_birth)),
    !anyNA(cause_flags), all(cause_flags %in% c(0L, 1L)), all(rowSums(cause_flags) == event)
  )
  labels <- c(
    mental = "Mental health", musculoskeletal = "Musculoskeletal", other = "Other",
    multiple = "Multiple diagnoses", unknown = "Unknown"
  )
  cause <- dat[[paste0("first_dp_", type, "_cause")]]
  for (cause_name in names(labels)) {
    column <- paste0("dp_", type, "_", cause_name, "_event")
    if (column %in% names(dat)) {
      stopifnot(all(dat[[column]] == as.integer(event == 1L & cause %in% labels[[cause_name]])))
    }
  }
  validation_summary[[type]] <- data.frame(
    outcome = type, parents = nrow(dat), events = sum(event), censored = sum(event == 0L),
    person_years = sum(days) / 365.25, minimum_days = min(days), maximum_days = max(days)
  )
}
bind_rows(validation_summary)
cat("\nPASS: All outcome and follow-up checks passed.\n")
saveRDS(parent_pension_analysis_1987_birth, "parent_pension_analysis_1987_birth.rds")
write.csv(parent_pension_analysis_1987_birth, "parent_pension_analysis_1987_birth.csv", row.names = FALSE, na = "")
save.image("WP1_birth_DP_progress.RData")

parent_pension_cox_1987_birth <- parent_pension_analysis_1987_birth %>% select(
  parent_id, special_need, pccc_exposure_final, vammaistuki_label, multimorbidity_label,
  age_at_child_birth, parent_type, married_at_index, region2, twin_birth_1987,
  matches("^dp_(any|full|partial)_.*event$"),
  matches("^dp_(any|full|partial)_followup_days$")
)
names(parent_pension_cox_1987_birth)
# Formatting correction: omit row names from the compact CSV as in other exports.
write.csv(parent_pension_cox_1987_birth, "parent_pension_cox_1987_birth.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# 7. Original separate quality checks after importing the two analysis CSVs
# -----------------------------------------------------------------------------

# Read the outputs just generated above, so an older CSV cannot undo alignment.
diagnosis <- read.csv("parent_pension_analysis_1987_retirement.csv", stringsAsFactors = FALSE)
birth <- read.csv("parent_pension_analysis_1987_birth.csv", stringsAsFactors = FALSE)

diagnosis$index_date <- as.Date(diagnosis$index_date)
diagnosis$censor_date <- as.Date(diagnosis$censor_date)
diagnosis$death_date <- as.Date(diagnosis$death_date)
diagnosis$first_old_age_date <- as.Date(diagnosis$first_old_age_date)
diagnosis$first_dp_any_date <- as.Date(diagnosis$first_dp_any_date)
diagnosis$first_dp_full_date <- as.Date(diagnosis$first_dp_full_date)
diagnosis$first_dp_partial_date <- as.Date(diagnosis$first_dp_partial_date)
diagnosis$dp_any_end_date <- as.Date(diagnosis$dp_any_end_date)
diagnosis$dp_full_end_date <- as.Date(diagnosis$dp_full_end_date)
diagnosis$dp_partial_end_date <- as.Date(diagnosis$dp_partial_end_date)

birth$child_birth_date <- as.Date(birth$child_birth_date)
birth$followup_start_birth <- as.Date(birth$followup_start_birth)
birth$censor_date_birth <- as.Date(birth$censor_date_birth)
birth$death_date <- as.Date(birth$death_date)
birth$first_old_age_date <- as.Date(birth$first_old_age_date)
birth$first_dp_full_date <- as.Date(birth$first_dp_full_date)
birth$first_dp_partial_date <- as.Date(birth$first_dp_partial_date)
birth$dp_any_end_date <- as.Date(birth$dp_any_end_date)
birth$dp_full_end_date <- as.Date(birth$dp_full_end_date)
birth$dp_partial_end_date <- as.Date(birth$dp_partial_end_date)
if ("first_dp_any_date" %in% names(birth)) {
  birth$first_dp_any_date <- as.Date(birth$first_dp_any_date)
} else {
  birth$first_dp_any_date <- birth$first_dp_full_date
  use_partial <- !is.na(birth$first_dp_partial_date) &
    (is.na(birth$first_dp_full_date) | birth$first_dp_partial_date < birth$first_dp_full_date)
  birth$first_dp_any_date[use_partial] <- birth$first_dp_partial_date[use_partial]
}

# ID and baseline checks: each count should be zero.
sum(is.na(diagnosis$parent_id))
sum(duplicated(diagnosis$parent_id))
sum(is.na(birth$parent_id))
sum(duplicated(birth$parent_id))
sum(is.na(diagnosis$index_date))
sum(is.na(diagnosis$censor_date))
sum(is.na(birth$followup_start_birth))
sum(is.na(birth$censor_date_birth))
sum(is.na(birth$child_birth_date))
sum(birth$followup_start_birth != birth$child_birth_date, na.rm = TRUE)
sum(diagnosis$first_dp_any_date <= diagnosis$index_date, na.rm = TRUE)
sum(birth$first_dp_any_date <= birth$followup_start_birth, na.rm = TRUE)

diagnosis_expected_censor <- pmin(diagnosis$death_date, diagnosis$first_old_age_date, as.Date("2017-12-31"), na.rm = TRUE)
birth_expected_censor <- pmin(birth$death_date, birth$first_old_age_date, as.Date("2017-12-31"), na.rm = TRUE)
sum(diagnosis$censor_date != diagnosis_expected_censor, na.rm = TRUE)
sum(birth$censor_date_birth != birth_expected_censor, na.rm = TRUE)
sum(diagnosis$censor_date <= diagnosis$index_date, na.rm = TRUE)
sum(birth$censor_date_birth <= birth$followup_start_birth, na.rm = TRUE)

# Tables should contain only 0 and 1, with no NA.
table(diagnosis$dp_any_event, useNA = "ifany")
table(diagnosis$dp_full_event, useNA = "ifany")
table(diagnosis$dp_partial_event, useNA = "ifany")
table(birth$dp_any_event, useNA = "ifany")
table(birth$dp_full_event, useNA = "ifany")
table(birth$dp_partial_event, useNA = "ifany")

# Follow-up checks: each count should be zero.
sum(!is.finite(diagnosis$dp_any_followup_days) | diagnosis$dp_any_followup_days <= 0)
sum(!is.finite(diagnosis$dp_full_followup_days) | diagnosis$dp_full_followup_days <= 0)
sum(!is.finite(diagnosis$dp_partial_followup_days) | diagnosis$dp_partial_followup_days <= 0)
sum(!is.finite(birth$dp_any_followup_days) | birth$dp_any_followup_days <= 0)
sum(!is.finite(birth$dp_full_followup_days) | birth$dp_full_followup_days <= 0)
sum(!is.finite(birth$dp_partial_followup_days) | birth$dp_partial_followup_days <= 0)
colSums(is.na(diagnosis[c("dp_any_end_date", "dp_full_end_date", "dp_partial_end_date")]))
colSums(is.na(birth[c("dp_any_end_date", "dp_full_end_date", "dp_partial_end_date")]))
sum(diagnosis$dp_any_followup_days != as.numeric(diagnosis$dp_any_end_date - diagnosis$index_date), na.rm = TRUE)
sum(diagnosis$dp_full_followup_days != as.numeric(diagnosis$dp_full_end_date - diagnosis$index_date), na.rm = TRUE)
sum(diagnosis$dp_partial_followup_days != as.numeric(diagnosis$dp_partial_end_date - diagnosis$index_date), na.rm = TRUE)
sum(birth$dp_any_followup_days != as.numeric(birth$dp_any_end_date - birth$followup_start_birth), na.rm = TRUE)
sum(birth$dp_full_followup_days != as.numeric(birth$dp_full_end_date - birth$followup_start_birth), na.rm = TRUE)
sum(birth$dp_partial_followup_days != as.numeric(birth$dp_partial_end_date - birth$followup_start_birth), na.rm = TRUE)

# Common inclusive pension-date rule, matching both preparation sections.
sum(diagnosis$dp_any_event != as.integer(
  !is.na(diagnosis$first_dp_any_date) & diagnosis$first_dp_any_date > diagnosis$index_date &
    diagnosis$first_dp_any_date <= diagnosis$censor_date
), na.rm = TRUE)
sum(diagnosis$dp_full_event != as.integer(
  !is.na(diagnosis$first_dp_full_date) & diagnosis$first_dp_full_date > diagnosis$index_date &
    diagnosis$first_dp_full_date <= diagnosis$censor_date
), na.rm = TRUE)
sum(diagnosis$dp_partial_event != as.integer(
  !is.na(diagnosis$first_dp_partial_date) & diagnosis$first_dp_partial_date > diagnosis$index_date &
    diagnosis$first_dp_partial_date <= diagnosis$censor_date
), na.rm = TRUE)
sum(birth$dp_any_event != as.integer(
  !is.na(birth$first_dp_any_date) & birth$first_dp_any_date > birth$followup_start_birth &
    birth$first_dp_any_date <= birth$censor_date_birth
), na.rm = TRUE)
sum(birth$dp_full_event != as.integer(
  !is.na(birth$first_dp_full_date) & birth$first_dp_full_date > birth$followup_start_birth &
    birth$first_dp_full_date <= birth$censor_date_birth
), na.rm = TRUE)
# Reconstructed continuation: partial check was not visible between screenshots.
sum(birth$dp_partial_event != as.integer(
  !is.na(birth$first_dp_partial_date) & birth$first_dp_partial_date > birth$followup_start_birth &
    birth$first_dp_partial_date <= birth$censor_date_birth
), na.rm = TRUE)

# End-date checks: numeric comparison avoids Date/numeric class mismatch.
# The first diagnosis comparison has the correction discussed in the conversation.
sum(as.numeric(diagnosis$dp_any_end_date) != ifelse(
  diagnosis$dp_any_event == 1, as.numeric(diagnosis$first_dp_any_date), as.numeric(diagnosis$censor_date)
), na.rm = TRUE)
sum(as.numeric(diagnosis$dp_full_end_date) != ifelse(
  diagnosis$dp_full_event == 1, as.numeric(diagnosis$first_dp_full_date), as.numeric(diagnosis$censor_date)
), na.rm = TRUE)
sum(as.numeric(diagnosis$dp_partial_end_date) != ifelse(
  diagnosis$dp_partial_event == 1, as.numeric(diagnosis$first_dp_partial_date), as.numeric(diagnosis$censor_date)
), na.rm = TRUE)
sum(as.numeric(birth$dp_any_end_date) != ifelse(
  birth$dp_any_event == 1, as.numeric(birth$first_dp_any_date), as.numeric(birth$censor_date_birth)
), na.rm = TRUE)
sum(as.numeric(birth$dp_full_end_date) != ifelse(
  birth$dp_full_event == 1, as.numeric(birth$first_dp_full_date), as.numeric(birth$censor_date_birth)
), na.rm = TRUE)
sum(as.numeric(birth$dp_partial_end_date) != ifelse(
  birth$dp_partial_event == 1, as.numeric(birth$first_dp_partial_date), as.numeric(birth$censor_date_birth)
), na.rm = TRUE)

# Covariate missingness is reported separately from outcome validation.
colSums(is.na(diagnosis[c(
  "special_need", "age_at_index", "index_year", "parent_type",
  "married_at_index", "region2", "twin_birth_1987"
)]))
colSums(is.na(birth[c(
  "special_need", "age_at_child_birth", "parent_type",
  "married_at_index", "region2", "twin_birth_1987"
)]))

# Final checks below must pass before the aligned outputs are saved.
datasets_to_check <- list(Diagnosis = diagnosis, Birth = birth)
cause_labels <- c(
  mental = "Mental health", musculoskeletal = "Musculoskeletal", other = "Other",
  multiple = "Multiple diagnoses", unknown = "Unknown"
)
for (dataset_name in names(datasets_to_check)) {
  check_data <- datasets_to_check[[dataset_name]]
  start <- if (dataset_name == "Diagnosis") check_data$index_date else check_data$followup_start_birth
  censor <- if (dataset_name == "Diagnosis") check_data$censor_date else check_data$censor_date_birth
  expected_censor <- pmin(check_data$death_date, check_data$first_old_age_date, as.Date("2017-12-31"), na.rm = TRUE)
  stopifnot(
    nrow(check_data) > 0L, !anyNA(check_data$parent_id),
    !anyDuplicated(check_data$parent_id), !anyNA(start), !anyNA(censor),
    all(censor == expected_censor), all(censor > start),
    all(is.na(check_data$first_dp_any_date) | check_data$first_dp_any_date > start)
  )
  for (type in c("any", "full", "partial")) {
    event <- check_data[[paste0("dp_", type, "_event")]]
    date <- check_data[[paste0("first_dp_", type, "_date")]]
    end <- check_data[[paste0("dp_", type, "_end_date")]]
    days <- check_data[[paste0("dp_", type, "_followup_days")]]
    cause <- check_data[[paste0("first_dp_", type, "_cause")]]
    expected_event <- as.integer(!is.na(date) & date > start & date <= censor)
    expected_end <- censor
    expected_end[expected_event == 1L] <- date[expected_event == 1L]
    cause_columns <- paste0("dp_", type, "_", names(cause_labels), "_event")
    cause_flags <- as.matrix(check_data[cause_columns])
    stopifnot(
      !anyNA(event), all(event %in% c(0L, 1L)), all(event == expected_event),
      !anyNA(end), all(end == expected_end), all(is.finite(days)), all(days > 0),
      all(days == as.numeric(end - start)),
      !anyNA(cause_flags), all(cause_flags %in% c(0L, 1L)),
      all(rowSums(cause_flags) == event)
    )
    for (group in names(cause_labels)) {
      stopifnot(all(check_data[[paste0("dp_", type, "_", group, "_event")]] ==
        as.integer(event == 1L & cause %in% cause_labels[[group]])))
    }
  }
}
cat("\nPASS: Both final datasets use the aligned outcome rules.\n")

saveRDS(diagnosis, "parent_pension_analysis_1987_diagnoses_corrected.rds")
saveRDS(birth, "parent_pension_analysis_1987_birth_final.rds")
write.csv(diagnosis, "parent_pension_analysis_1987_diagnoses_corrected.csv", row.names = FALSE)
write.csv(birth, "parent_pension_analysis_1987_birth_final.csv", row.names = FALSE)

# Update the original dictionary for the FINAL retirement-censored diagnosis file.
# The section 3 dictionary describes the earlier, non-retirement-censored copy.
dictionary_final <- dictionary[match(names(diagnosis), dictionary$variable), ]
rownames(dictionary_final) <- NULL
dictionary_final$variable <- names(diagnosis)
dictionary_final$data_type <- vapply(diagnosis, function(x) paste(class(x), collapse = "; "), character(1))
dictionary_final$description[is.na(dictionary_final$description)] <- "Retained source variable; see source cohort dictionary."
dictionary_final$coding_or_units[is.na(dictionary_final$coding_or_units)] <- ""
dictionary_final$notes[is.na(dictionary_final$notes)] <- ""
dictionary_final$dataset_file <- "parent_pension_analysis_1987_diagnoses_corrected.rds / .csv"
dictionary_final$description[dictionary_final$variable == "censor_date"] <-
  "Earliest of parent death, first old-age/early old-age pension date and 2017-12-31."
dictionary_final$notes[dictionary_final$variable == "censor_date"] <-
  "Missing death/retirement dates are ignored; administrative end remains available. DP on the censor date counts as an event."
dictionary_final$description[dictionary_final$variable == "first_old_age_date"] <-
  "Earliest APVM for LAJI 5 or 5A; parents already retired at baseline are excluded."
dictionary_final$coding_or_units[dictionary_final$variable == "first_old_age_date"] <- "YYYY-MM-DD"
dictionary_final$description[dictionary_final$variable == "married_at_index"] <-
  "Parent marital status measured in 1987."
for (type in c("any", "full", "partial")) {
  cause_var <- paste0("first_dp_", type, "_cause")
  dictionary_final$notes[dictionary_final$variable == cause_var] <- paste(
    "Multiple diagnoses = more than one distinct known DRYHMA1 code on the",
    "first pension date of this type, including codes within the same broad group.",
    "Identical repeated codes count once. No known code = Unknown."
  )
  for (group in c("multiple", "unknown")) {
    v <- paste0("dp_", type, "_", group, "_event")
    dictionary_final$description[dictionary_final$variable == v] <- paste(
      "First", type, "DP occurs during follow-up and is classified as", cause_labels[[group]]
    )
    dictionary_final$coding_or_units[dictionary_final$variable == v] <- "0 = no target event; 1 = target event"
    dictionary_final$notes[dictionary_final$variable == v] <- paste(
      "Use", paste0("dp_", type, "_followup_days"),
      "for follow-up. Other causes stop at the first DP date with target event=0."
    )
  }
}
stopifnot(identical(dictionary_final$variable, names(diagnosis)))
write.csv(dictionary_final, "parent_pension_analysis_1987_diagnoses_corrected_data_dictionary.csv", row.names = FALSE)
