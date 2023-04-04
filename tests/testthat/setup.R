testdata_Nd <- c(
  # Valid uncensored
  "0.1", "10",
  # Valid censored
  "ND<0.5", "<2.7",
  # Invalid uncensored
  "3,14", "4..80",
  # Invalid censored
  "ND<5,0", "<1 1",
  # Missingness
  NA_character_, "NA"
)
