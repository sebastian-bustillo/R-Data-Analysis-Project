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
  ######################################################################################################################
  setwd("C:\\Users\\Sebastian B\\Documents\\Spring 2020\\PIC\\Original Data\\Diagnostics\\PinM")

  shot1652 <- "s@1652 PinM_569.txt"
  shot1657 <- "s@1657 PinM_649.txt"
  shot1661 <- "s@1661 PinM_705.txt"
  shot1793 <- "s@1793 SCRP1@VDIVERT_2558.txt"
  shot1799 <- "s@1799 SCRP1@VDIVERT_2642.txt"
  
  s1652 <- read.csv(shot1652, header = FALSE, sep = ",")
  s1657 <- read.csv(shot1657, header = FALSE, sep = ",")
  s1661 <- read.csv(shot1661, header = FALSE, sep = ",")
  s1793 <- read.csv(shot1793, header = FALSE, sep = ",")
  s1799 <- read.csv(shot1799, header = FALSE, sep = ",")
  
  plot(s1652$V1*(10^6), savgol(s1652$V2, 51), type = "l", col = "blue", xlab = expression(paste("Time ", "(", mu, "s)")), ylab = "Voltage", main = "VDIVERT", xlim = c(1.4, 2.2))
  #lines(s1657$V1*(10^6), savgol(s1657$V2,51), type = "l", col = "red")
  lines(s1661$V1*(10^6), savgol(s1661$V2,51), type = "l", col = "red")
  #lines(s1793$V1*(10^6), savgol(s1793$V2,51), type = "l", col = "green")
  lines(s1657$V1*(10^6), savgol(s1657$V2,51), type = "l", col = "black")
  legend("topright" ,legend = c("1652",  "1661",  "1657"), col = c("blue", "red", "black"), cex = 0.8, lty = 1:1 )

  
  
  # p = ggplot() + geom_line(data = s1652, aes(x = V1, y = V2), color = "blue") +
  #   geom_line(data = s1657, aes(x = V1, y = V2), color = "green") + geom_line(data = s1661, aes(x = V1, y = V2), color = "red") +
  #   geom_line(data = s1661, aes(x = V1, y = V2), color = "magenta")+ geom_line(data = s1793, aes(x = V1, y = V2), color = "purple") + 
  #   geom_line(data = s1799, aes(x = V1, y = V2), color = "#f5429e") + theme_bw() + xlab('Time') + ylab('Voltage') +
  #   title(main = "Vpipe 3") 
  # 
  # pdf("Rplot.pdf")
  #  print(p)
  #  dev.off()
  
  
  
  
  
  
  
  
  
  
  ########################################################################################################################
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
  
  
