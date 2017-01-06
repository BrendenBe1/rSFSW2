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
#   > create_treatments
#   > TODO: Add more during integration phase
#
# Output:
#   > One global variable: CO2.Extracted
#   > Example:
#       RCP Year       CO2   Multiplier
#  1    85 2000    368.865   0.986492
#  2    85 2001   370.4675   0.987774
#  3    85 2002   372.5225   0.989418
#  4    85 2003     374.76   0.991208
#
# Asymptotic Runtime:
#   > Num of RCPs         = i
#   > Num of chosen years = j
#   > Num of years in CSV = k
#   > ϴ(i*(j+k))
##################################################################


####################
# Create dummy data
####################
if (FALSE) {
  CO2.Years    <- c(seq(2000, 2099))
  CO2.RCPs     <- c(85)
  dir.sw.in.tr <- "C:/GIT/Soilwat_R_Wrapper_v191/1_Data_SWInput/treatments/LookupCO2"
  trfile.CO2   <- "CO2_RCP_Concentrations.csv"
  tr_CO2       <- read.csv(file.path(dir.sw.in.tr, trfile.CO2),  header = TRUE, stringsAsFactors = FALSE)
  be.quiet     <- FALSE
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
  col_names               <- c("RCP", "Year", "Bio Mult", "Sto Mult") 
  CO2.Extracted           <- data.frame(data.frame(matrix(ncol = length(col_names), nrow = 1)))
  colnames(CO2.Extracted) <- col_names
  row                     <- 1
  used_a_RCP              <- FALSE
  
  ###############################
  # Grab multiplier coefficients
  ###############################
  CO2BioCoeff1 <- swDataFromFiles@prod@CO2Coefficients[1]
  CO2BioCoeff2 <- swDataFromFiles@prod@CO2Coefficients[2]
  CO2StoCoeff1 <- swDataFromFiles@prod@CO2Coefficients[3]
  CO2StoCoeff2 <- swDataFromFiles@prod@CO2Coefficients[4]
  
  ######################
  # Grab requested data
  ######################
  RCP_row_nums <- which(grepl("\\<RCP\\>", tr_CO2[, 1]))  # Grab all instances that 'RCP' is mentioned in the first column
  for (i in 1:length(RCP_row_nums)) {                     # Check each row that 'RCP' was mentioned
    if (tr_CO2[RCP_row_nums[i], 2] %in% CO2.RCPs) {       # If the value in the second column is in the user's given RCPs, continue
      used_a_RCP <- TRUE                                  # Record that we found a RCP so that more accurate errors can be shown
      if (i == length(RCP_row_nums)) end <- nrow(tr_CO2)  # Stop at the last row of the CSV
      else end <- RCP_row_nums[i + 1]                     # Stop at the beginning of the next RCP
      
      ################################
      # Is a year missing in the CSV?
      ################################
      for (j in 1:length(CO2.Years)) {
        # Yes, set the multipliers to 1 for this RCP
        if (!(CO2.Years[j] %in% tr_CO2[RCP_row_nums[i]:end, 1]))
        {
          CO2.Extracted[row, "RCP"]      <- tr_CO2[RCP_row_nums[i], 2]
          CO2.Extracted[row, "Year"]     <- CO2.Years[j]
          CO2.Extracted[row, "Bio Mult"] <- 1
          CO2.Extracted[row, "Sto Mult"] <- 1
          row <- row + 1
        }
      }
      
      # No, estimate the multiplier
      for (k in RCP_row_nums[i]:end) {                    # Go from this RCP to either the next RCP or the end of file
        year <- tr_CO2[k, 1]                              # Extract the current year
        if (year %in% CO2.Years) {                        # Check if user wanted to use this year
          CO2 <- tr_CO2[k, 4]                             # Extract CO2
          RCP <- tr_CO2[RCP_row_nums[i], 2]               # Extract RCP
          # Append this year's data
          CO2.Extracted[row, "RCP"]  <- RCP               
          CO2.Extracted[row, "Year"] <- year
          # Are we using the multiplier for this year?
          if ((any(create_treatments == "UseCO2Coefficients_Retro")  && year <= endyr) || 
              (any(create_treatments == "UseCO2Coefficients_Future") && year >  endyr)) {
            CO2.Extracted[row, "Bio Mult"] <- CO2BioCoeff1 * as.numeric(CO2) + CO2BioCoeff2  # Calculate the biomass multiplier
            CO2.Extracted[row, "Sto Mult"] <- CO2StoCoeff1 * as.numeric(CO2) + CO2StoCoeff2  # Calculate the stomatal conductance multiplier
          } else {
            # Do not change vegetation data
            CO2.Extracted[row, "Bio Mult"] <- 1
            CO2.Extracted[row, "Sto Mult"] <- 1
          }
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
