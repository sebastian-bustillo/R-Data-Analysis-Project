############################################
# Data analysis project (PIC Math) for the Nevada National Security Site.
# Mentors: Dr. Sykes & Dr. Howard
# Ouachita Baptist University
# 3-20-2020
################################################

# Import libraries needed for the project
library(dplyr)
library(tidyr)
library(data.table)
library(pracma)
library(DescTools)
library(ggplot2)
library(stringr)
library(grid)
library(devtools)
library(easyGgplot2)
library(signal)
library(corrgram)
library(corrplot)
library(varhandle)
library(plyr)
library(gridExtra)

#######
# Get the radiation doses
#######
setwd("C:\\Users\\Sebastian B\\Documents\\Spring 2020\\PIC\\Original Data\\Rad Dose")
rad_dose <-
  read.csv('ShotsLog.csv',
           header = T,
           na.strings = c("", " ", "NA"))
####
## Some Data cleaning
####
rad_dose <-  select(rad_dose, 1:2)
rad_dose <- slice(rad_dose, 2:80)
names(rad_dose)[names(rad_dose) == "ï..Shot.Number"] <- "Shot"
names(rad_dose)[names(rad_dose) == "Dose.C1.Rads"] <- "Dose"
###
# Get the radiaton doses for Cygnus 1 only.
###
rad_dose <-
  rad_dose[as.numeric(as.character(rad_dose[, 1])) %% 2 == 1,]

folder_names <-
  list.files(path = "C:\\Users\\Sebastian B\\Documents\\Spring 2020\\PIC\\Original Data\\Diagnostics")
peakArea <- TRUE
for (diagnostic_ID in folder_names) {
  # Get the file names
  files <- list.files(
    path = paste0(
      "C:\\Users\\Sebastian B\\Documents\\Spring 2020\\PIC\\Original Data\\Diagnostics\\",
      diagnostic_ID
    )
  )
  ###
  # Function to determine some statistical values of the trace like height, width, mean, rms, etc.
  # **Notice** The signal is being smoothed out using the Savitzky-Golay smoothin algorithm.
  # The dominant peak is being cropped from the signal to calculate all of the values.
  # my_data == data frame containing the ".txt" files.
  # my_list == data frame with the different statistical values of the signal
  # filename == file name
  # peakArea == flag to crop just the predominant peak in the signal
  ###
  get_signal_features <-
    function(my_data, my_list, filename, peakArea) {
      
      
      
      signal <- my_data[, 2]
      times <- my_data[, 1]
      
      
      
      
      signal <- savgol(signal, 51, forder = 4)
      
      if (peakArea) {
        maxIndex <- which(signal == max(signal))
        
        rms <- 0
        for (i in 1:maxIndex[1]) {
          if (signal[i] > 1) {
            rms <- sqrt(mean(signal[1:i] ^ 2))
            break
          }
        }
        
        for (i in 1:length(signal)) {
          if (signal[i] <= rms) {
            signal[i] <- 0
          }
          
        }
        
        signalStart <- 0
        signalEnd <- 0
        
        for (i in maxIndex[1]:length(signal)) {
          if (signal[i] == 0) {
            signalEnd <- i
            break
          }
        }
        for (i in maxIndex[1]:1) {
          if (signal[i] == 0) {
            signalStart <- i
            break
          }
        }
        
        signal <- signal[signalStart:signalEnd]
        times <- times[signalStart:signalEnd]
      }
      signal <- savgol(signal, 51, forder = 4)
      
      row_num <- nrow(my_list) + 1
      
      str <- str_extract(filename, "@....")
      #Shot number,
      my_list[row_num, "Shot"] <- substr(str, 2, 5)
      
      #Height of the peak.
      my_list[row_num, "Height"] <- max(signal) - min(signal)
      
      # Find the root mean sqaure value for the signal.
      my_list[row_num, "RMS"] <- sqrt(mean(signal ^ 2))
      
      # Find the smallest values for the voltage
      my_list[row_num, "MIN"] <- min(signal)
      
      # Calculate the mean for the voltage
      my_list[row_num, "MEAN"] <- mean(signal)
      
      # Calculate the standard deviation for the signal
      my_list[row_num, "SD"] <- sd(signal)
      
      # Find the area under the cuve.
      my_list[row_num, "AOS"] <-
        AUC(times, signal, method = "trapezoid", subdivisions = 500)
      
      # Absolute area under curve. *Note: area here is being calculated with a different method.*
      my_list[row_num, "AAOS"] <-
        AUC(
          times,
          signal,
          method = "trapezoid",
          absolutearea = T,
          subdivisions = 500
        )
      
      
      return(my_list)
    }
  
  setwd(
    paste0(
      "C:\\Users\\Sebastian B\\Documents\\Spring 2020\\PIC\\Original Data\\Diagnostics\\",
      diagnostic_ID
    )
  )
  # Create a data frame with the different signal traits (ex. height, max, min, etc.)
  # Using the following format: "Height Width RMS MIN MAX MEAN SD AOS"
  statistical_properties <- data.frame()
  
  for (i in 1:length(files)) {
    if (file.size(files[i]) != 0) {
      my_Data_copy <- read.csv(files[i], header = FALSE, sep = ",")
      
    } else{
      my_Data_copy <- data.frame(matrix(0, 2, 2))
    }
    statistical_properties <-
      get_signal_features(my_Data_copy, statistical_properties, files[i], peakArea)
  }
  
  
  
  statistical_properties <-
    join(statistical_properties,
         rad_dose,
         by = "Shot",
         type = "inner")
  
  shot.cat <- function(my_Signal) {
    my_Signal[, "Qualitative_Dose"] <- NA
    
    for (i in 1:length(my_Signal[, 1])) {
      dose <- as.numeric(as.character(my_Signal$Dose[i]))
      
      if (dose <= 3.5) {
        my_Signal$Qualitative_Dose[i] <- "bad"
      } else if ((dose > 3.5) & (dose < 4)) {
        my_Signal$Qualitative_Dose[i] <- "average"
      } else if ((dose >= 4) & (dose < 4.5)) {
        my_Signal$Qualitative_Dose[i] <- "good"
      } else if (dose >= 4.5) {
        my_Signal$Qualitative_Dose[i] <- "great"
      }
      
    }
    
    return(my_Signal)
  }
  
  statistical_properties <- shot.cat(statistical_properties)
  
  setwd(
    "C:\\Users\\Sebastian B\\Documents\\Spring 2020\\PIC\\Original Data\\Analysis Files"
  )
  
  
  plist <-
    sapply(names(statistical_properties)[-grep("Shot", names(statistical_properties))], function(col) {
      ggplot(statistical_properties, aes_string(x = "Shot", y = col)) + geom_point(aes(color = Qualitative_Dose),
                                                                                   size = 5,
                                                                                   shape = 17) + labs(title = diagnostic_ID, subtitle = col) + theme_linedraw()
    }, simplify = F)
  
  # pl <-
  #   ggplot(statistical_properties, aes(x = Shot, y = AOS)) + geom_point(aes(color = Qualitative_Dose),
  #                                                                       size = 5,
  #                                                                       shape = 17) + ggtitle(diagnostic_ID) + theme_bw()
  
  plist2 <-
    sapply(names(statistical_properties)[-grep("Dose", names(statistical_properties))], function(col) {
      ggplot(statistical_properties, aes_string(x = "Dose", y = col)) + geom_point(aes(color = Qualitative_Dose),
                                                                                   size = 5,
                                                                                   shape = 17) + labs(title = diagnostic_ID, subtitle = col) + theme_linedraw()
    }, simplify = F)
  
  # pl2 <-
  #   ggplot(statistical_properties, aes(x = Dose, y = AOS)) + geom_point(size = 5) + ggtitle(diagnostic_ID) + theme_bw()
  ##############
  ## Test to unfactor Dose
  ##############
  statistical_properties$Dose <-
    unfactor(statistical_properties$Dose)
  
  # # Num only
  num.cols <- sapply(statistical_properties, is.numeric)
  # #
  # # # filter
  cor.data <- cor(statistical_properties[, num.cols])
  
  setwd(
    "C:\\Users\\Sebastian B\\Documents\\Spring 2020\\PIC\\Original Data\\Analysis Files\\Correlation Plots"
  )
  pdf(paste0(diagnostic_ID, '_Corr_Plot.pdf'))
  corrplot(
    cor.data,
    method = 'number',
    type = 'lower',
    tl.col = '#d4a261',
    title = paste0("Correlation for ", diagnostic_ID),
    bg = '#81c3de'
  )
  dev.off()
  
  setwd(
    "C:\\Users\\Sebastian B\\Documents\\Spring 2020\\PIC\\Original Data\\Analysis Files\\Time Series Plots"
  )
  pdf(paste0(diagnostic_ID, '_Time_Series_Plot.pdf'), onefile = TRUE)
  for (i in seq(length(plist))) {
    print(plist[[i]])
  }
  dev.off()
  
  setwd(
    "C:\\Users\\Sebastian B\\Documents\\Spring 2020\\PIC\\Original Data\\Analysis Files\\Statistical Values Plots"
  )
  pdf(paste0(diagnostic_ID, '_Statistical_Values_Plots.pdf'),
      onefile = TRUE)
  for (i in seq(length(plist2))) {
    print(plist2[[i]])
  }
  dev.off()
  
}
