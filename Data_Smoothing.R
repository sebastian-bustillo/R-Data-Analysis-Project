library(pracma)
library(DescTools)
library(ggplot2)
library(stringr)
library(grid)
library(devtools)
library(easyGgplot2)
library(signal)
# Command to clear the global environment = rm(list = ls())

XRAYPINB <- list.files(path = "C:\\Users\\Sebastian B\\Documents\\School\\Spring 2020\\PIC\\Original Data\\Diagnostics\\XRAYPINB" )
setwd("C:\\Users\\Sebastian B\\Documents\\Spring 2020\\PIC\\Original Data\\Diagnostics\\XRAYPINB")

par(mfrow = c(2, 2))
df <- read.csv(XRAYPINB[1], header = F, sep = ",")
plot(df[,1], df[,2], xlab = "Seconds", ylab = "Voltage", type = "l" )
plot(df[,1], sgolay(df[,2], 51), xlab = "Seconds", ylab = "Voltage", type = "l" )


folder_names <-
  list.files(path = "C:\\Users\\Sebastian B\\Documents\\School\\Spring 2020\\PIC\\Original Data\\Diagnostics")

# Get the diagnostic ID.




for (diagnostic_ID in folder_names) {
  # Get a list with all the file names
  files = list.files(
    path = paste0(
      "C:\\Users\\Sebastian B\\Documents\\School\\Spring 2020\\PIC\\Original Data\\Diagnostics\\",
      diagnostic_ID
    )
  )
  setwd(
    paste0(
      "C:\\Users\\Sebastian B\\Documents\\School\\Spring 2020\\PIC\\Original Data\\Diagnostics\\",
      diagnostic_ID
    )
  )
  
  ## Function to get the features of the data
  ## my_data is the data frame with the voltage & time
  ## my_list is the data frame to store
  get_signal_features <- function(my_data, my_list, filename) {
    signal <- my_data[, 2]
    times <- my_data[, 1]
    
    signal <- savgol(signal, 51, forder = 4)
    
    #Shot number,
    my_list[filename, "Shot"] <- str_extract(filename, "@....")
    
    #Height of the peak.
    my_list[filename, "Height"] <- max(signal) - min(signal)
    
    # Width of the whole signal. **Note: we want to calculate the width of the spike only.**
    my_list[filename, "Width"] <- max(times)
    
    # Find the root mean sqaure value for the signal.
    my_list[filename, "RMS"] <- sqrt(mean(signal ^ 2))
    
    # Find the highest value for the voltage
    my_list[filename, "MAX"] <- max(signal)
    
    # Find the smallest values for the voltage
    my_list[filename, "MIN"] <- min(signal)
    
    # Calculate the mean for the voltage
    my_list[filename, "MEAN"] <- mean(signal)
    
    # Calculate the standard deviation for the signal
    my_list[filename, "SD"] <- sd(signal)
    
    # Find the area under the cuve.
    my_list[filename, "AOS"] <-
      AUC(times, signal, subdivisions = 9000)
    
    # Absolute area under curve. *Note: area here is being calculated with a different method.*
    my_list[filename, "AAOS"] <-
      AUC(
        times,
        signal,
        method = "spline",
        absolutearea = T,
        subdivisions = 9000
      )
    return(my_list)
  }
  
  # Create a data frame with the different signal traits (ex. height, max, min, etc.)
  # Using the following format: "Height Width RMS MIN MAX MEAN SD AOS"
  properties <- data.frame()
  
  for (i in 1:length(files)) {
    if (file.size(files[i]) != 0) {
      my_Data_copy <- read.csv(files[i], header = FALSE, sep = ",")
      
    } else{
      my_Data_copy <- data.frame(matrix(0, 2, 2))
    }
    properties <-
      get_signal_features(my_Data_copy, properties, files[i])
  }
  
  # Create a list for the different plots.
  plist <-
    sapply(names(properties)[-grep("Shot", names(properties))], function(col) {
      ggplot(properties, aes_string(x = "Shot", y = col)) + geom_point(aes(color = col), size = 5) + ggtitle(diagnostic_ID)
    }, simplify = F)
  
  setwd(
    paste0(
      "C:\\Users\\Sebastian B\\Documents\\School\\Spring 2020\\PIC\\Original Data\\Plots",
      paste0("\\", diagnostic_ID)
    )
  )
  
  ## Save the plots for the time series as PDFs.
  for (i in 1:length(plist)) {
    print(plist[[i]])
    ggsave(
      paste0(names(plist[i]), '.pdf'),
      width = 35,
      height = 20,
      units = 'cm',
      dpi = 300
    )
    
  }
  
}
