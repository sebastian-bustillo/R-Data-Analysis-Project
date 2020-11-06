  library(pracma)
  library(DescTools)
  library(ggplot2)
  library(reshape2)
  library(dplyr)
  
  # Program to create PDFs for signal plots
  # Sebastian Bustillo
  # Ouachita Baptist University 
  # 2-29-2020
  
  # Get the names of the different diagnostic types. (Ex. DRD1, IMRX, IPFL4SW, etc.)
  folder_names <- list.files(path = "C:\\Users\\Sebastian B\\Documents\\Spring 2020\\PIC\\Original Data\\Diagnostics")
  
  # Iterate through all of the diagnostic IDs
  for (diagnostic_ID in folder_names) {
    
    # Set directory to the diagnostic ID
    setwd(paste0("C:\\Users\\Sebastian B\\Documents\\Spring 2020\\PIC\\Original Data\\Diagnostics", paste0("\\", diagnostic_ID)))
    # Get all of the txt files from the current diagnostic.
    file_names <- list.files(getwd())
    
    # Create a folder for the plots
    dir.create(file.path("C:\\Users\\Sebastian B\\Documents\\Spring 2020\\PIC\\Original Data\\Plots", diagnostic_ID)) 
    
    for (filename in file_names) {
    
    # Set directory to the ID plot
    setwd(paste0("C:\\Users\\Sebastian B\\Documents\\Spring 2020\\PIC\\Original Data\\Diagnostics", paste0("\\", diagnostic_ID)))
    
    # Read in the data
    data <- read.csv(filename, sep = ",")
    setwd(paste0("C:\\Users\\Sebastian B\\Documents\\Spring 2020\\PIC\\Original Data\\Plots", paste0("\\", diagnostic_ID)))
    
    # Create a PDF file for the plot
    pdf(paste0(filename, '.pdf'))
    
    # Notice, the signal is being filtered using the Savitzky-Golay filter. Filter uses a 4th degree. 
    plot(data[,1], savgol(data[,2], 51), main = filename, xlab = "Time (seconds)", ylab = "Voltage", type = 'l')
    # Close PDF file.
    dev.off()
    
  }
  
  }
  
  
