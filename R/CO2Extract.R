#' @author Zachary Kramer, \email{kramer.zachary.nau@gmail.com}


#############
# Quick info
##################################################################
# Description:
#   Extract the given years for the given RCPs and store the data
#   in a global data frame
#
# Required global variables:
#   > CO2.Years
#   > CO2.RCPs
#   > tr_CO2
#   > TODO: Add more during integration phase
#
# Output:
#   > One global variable: CO2.Extracted
##################################################################


####################
# Create dummy data
####################
if (interactive()) {
  CO2.Years <- c(seq(2000, 2099))
  CO2.RCPs  <- c(45)
  dir.sw.in.tr <- "C:/GIT/Soilwat_R_Wrapper_v191/1_Data_SWInput/treatments/LookupCO2"
  trfile.CO2 <- "CO2_RCP_Concentrations.csv"
  tr_CO2 <- read.csv(file.path(dir.sw.in.tr, trfile.CO2),  header = TRUE, stringsAsFactors = FALSE)
  be.quiet <- FALSE
}

#########################
# Check for invalid data
#########################
input_is_valid <- function() {
  # Does not use else statements so that all errors are printed at once
  answer <- TRUE
  # Header
  if (!be.quiet) cat("Extracting CO2 and its biomass multipliers...\n")
  # Check for valid years
  if (length(CO2.Years) == 0) {
    if (!be.quiet) cat("  > No years were chosen\n\n")
    answer <- FALSE
  } 
  if (min(CO2.Years) < 1915) {
    if (!be.quiet) cat("  > Please do not use years before 1915\n")
    answer <- FALSE
  } 
  if (max(CO2.Years) > 2099) {
    if (!be.quiet) cat("  > Please do not use years after 2099\n")
    answer <- FALSE
  } 
  # Check for valid RCPs
  if (length(CO2.RCPs) == 0) {
    if (!be.quiet) cat("  > No RCPs were given\n")
    answer <- FALSE
  }
  return(answer)
}

if (input_is_valid()) {
  #######################################
  # Create dataframe to hold yearly data
  #######################################
  col_names               <- c("RCP", "Year", "CO2", "Multiplier") 
  CO2.Extracted           <- data.frame(data.frame(matrix(ncol = length(col_names), nrow = 1)))
  colnames(CO2.Extracted) <- col_names
  row                     <- 1
  used_a_RCP              <- FALSE
  
  ###############################
  # Grab multiplier coefficients
  ###############################
  # TODO: Use .in file
  coeff1 <- 0.0008
  coeff2 <- 0.6914
  
  ######################
  # Grab requested data
  ######################
  # Navigate to the given RCPs
  RCP_row_nums <- which(grepl("RCP", tr_CO2[, 1]))
  for (i in 1:length(RCP_row_nums)) {
    # Check if user wanted to use this RCP
    if (tr_CO2[RCP_row_nums[i], 2] %in% CO2.RCPs) {
      # Grab every given year for this RCP
      used_a_RCP <- TRUE
      if (i == length(RCP_row_nums)) end <- nrow(tr_CO2)  # Stop at the last row of the CSV
      else end <- RCP_row_nums[i + 1]                     # Stop at the beginning of the next RCP
      for (j in RCP_row_nums[i]:end) {
        year <- tr_CO2[j, 1]
        if (year %in% CO2.Years) {
          # Append this year's data
          CO2 <- tr_CO2[j, 4]
          RCP <- tr_CO2[RCP_row_nums[i], 2]
          CO2.Extracted[row, "RCP"]        <- RCP
          CO2.Extracted[row, "Year"]       <- year
          CO2.Extracted[row, "CO2"]        <- CO2
          CO2.Extracted[row, "Multiplier"] <- coeff1 * as.numeric(CO2) + coeff2  # Calculate the biomass multiplier
          row <- row + 1
        }
      }
    }
  }
  #######################
  # Print success/errors
  #######################
  if (!be.quiet) {
    if (!is.na(CO2.Extracted[1, 1])) cat("  > Done!\n")  # Assume that if any data exists, the extraction was successful
    else {
      cat("  > No data was extracted\n")
      if (!used_a_RCP) cat("    > None of the given RCPs were found")
    }
  } 
}
