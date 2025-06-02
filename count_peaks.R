args <- commandArgs(trailingOnly = TRUE)

input_file <- args[1]
start_year <- as.integer(args[2])
start_month <- as.integer(args[3])
end_year <- as.integer(args[4])
end_month <- as.integer(args[5])
output_file <- args[6]

library(tidyverse)
library(stringi)

df <- read_csv(input_file)

df_out <- df %>%
  filter((year > start_year & year < end_year) |
         (year == start_year) |
         (year == end_year)) %>%
  mutate(
    usage_trimmed = case_when(
      year == start_year & year == end_year ~ substr(usage, start_month, end_month),
      year == start_year ~ substr(usage, start_month, 12),
      year == end_year ~ substr(usage, 1, end_month),
      TRUE ~ usage
    ),
    peaks = case_when(
      usage_trimmed == str_dup("F", str_length(usage_trimmed)) ~ 0,
      usage_trimmed == str_dup("T", str_length(usage_trimmed)) ~ 100,
      TRUE ~ stri_count_regex(usage_trimmed, "F*T+")
    )
  )

write_csv(df_out, output_file)


# Each row in df represents a year of data.
# The usage column is likely a 12-character string, where each character represents usage for a month (e.g., "FTFTFTFTFTFT").
# The goal is to trim this string to only include months within a specified range:
#   start_year, start_month
# end_year, end_month

# This logic trims the usage string based on the year:
#   
#   If the row's year is both the start and end year:
# 
# Trim from start_month to end_month.
# Example: If start_month = 3 and end_month = 6, then "FTFTFTFTFTFT" → "FTFT" (March to June).
# If it's the start year only:
#   
#   Trim from start_month to December (month 12).
# Example: start_month = 4 → "FTFTFTFTFTFT" → "FTFTFTFT" (April to December).
# If it's the end year only:
# 
# Trim from January to end_month.
# Example: end_month = 5 → "FTFTFTFTFTFT" → "FTFTF" (January to May).
# If it's a year in between:
#   
#   Keep the full usage string.
# 
# This trimming ensures that only the relevant months are analyzed when counting "peaks" (patterns like "F*T+") later in the script. It avoids counting usage outside the specified date range.