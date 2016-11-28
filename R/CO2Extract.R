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
#   > dir.sw.dat
#   > trfile.CO2
#
# Output:
#   > One global variable: CO2.Extracted
##################################################################


####################
# Create dummy data
####################
if (interactive()) {
  CO2.Years <- c(2004, 2007, 2490)
  CO2.RCPs  <- c(85, 45)
  dir.sw.dat = "C:/Users/Zach/Documents/USGS"
  trfile.CO2 = "RCP85_MIDYEAR_CONCENTRATIONS.csv"
}

#########################
# Check for invalid data
#########################
if (length(CO2.Years) == 0) {
  print("  > No years were chosen")
} else if (length(CO2.RCPs) == 0) {
  print("  > No RCPs were chosen") 
  } else {
      #######################################
      # Create dataframe to hold yearly data
      #######################################
      col_names <- c("RCP", "Year", "CO2EQ",	"KYOTO-CO2EQ",	"CO2",	"CH4",	"N2O",	"FGASSUMHFC134AEQ",	"MHALOSUMCFC12EQ",	"CF4",	"C2F6",	"C6F14",	"HFC23",	"HFC32",	
                     "HFC43_10",	"HFC125",	"HFC134a",	"HFC143a",	"HFC227ea",	"HFC245fa",	"SF6",	"CFC_11",	"CFC_12",	"CFC_113",	"CFC_114",	"CFC_115",	
                     "CARB_TET",	"MCF",	"HCFC_22",	"HCFC_141B",	"HCFC_142B",	"HALON1211",	"HALON1202",	"HALON1301",	"HALON2402",	"CH3BR",	"CH3CL")
      CO2.Extracted <- data.frame(data.frame(matrix(ncol = 37, nrow = 1)))
      colnames(CO2.Extracted) <- col_names
      row <- 1
      
      ##################
      # Read in CO2 CSV
      ##################
      co2_data <- read.csv(file.path(dir.sw.dat, trfile.CO2),  header = TRUE, stringsAsFactors = FALSE)
      # TODO: Change co2_data to trfile.CO2 when integrating with 4 of 5
      
      ######################
      # Grab requested data
      ######################
      # Navigate to the chosen RCPs
      RCP_row_nums <- which(grepl("RCP", co2_data[, 1]))
      for (i in 1:length(RCP_row_nums)) {
        # Check if user wanted to use this RCP
        if (co2_data[RCP_row_nums[i], 2] %in% CO2.RCPs) {
          # Grab every chosen year for this RCP
          if (i == length(RCP_row_nums)) end <- nrow(co2_data)
          else end <- RCP_row_nums[i + 1]
          for (j in RCP_row_nums[i]:end) {
            if (co2_data[j, 1] %in% CO2.Years) {
              # Append this year's data
              print(co2_data[RCP_row_nums[i], 2])
              CO2.Extracted[row, "RCP"] <- co2_data[RCP_row_nums[i], 2]
              CO2.Extracted[row, 2:ncol(CO2.Extracted)] <- co2_data[j, ]
              row <- row + 1
            }
          }
        }
      }
    }