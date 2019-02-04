## FIRST BOOTSTRAP ALGORITHM with Uniform Distribution
## first, write generalised bootstrap algorithm 
## N_n is number of sample points you want from data
## this is bootstrap with replacement mind you 
my_Bootstrap <- function(data, N_n){
  N <- nrow(data) ## to make sure you encompass all the data
  boot_index <- round(runif(n = N_n, min = 1, max = N))
  x <- data[boot_index,]
  return(x)
}

###### Boot strap trajectory sampling 
## Will need to find way to choose sample window size and width
### Should width differ or stay the same? 
## perhaps combine both flunctuating window size and consistent window size

my_trajectory_Boot <- function (data,wind_size){
  N_data <- nrow(data)
  ### make sure rand uni less than N_data - wind_size at all times
  boot_index <- round(runif(n= 1, min = 1, max = (N_data - wind_size)))
  x<- data[boot_index:(boot_index + wind_size),]
  
  return(x)
}

##stochastic window smapling 
## note: min windsize is a control var
## min_wind_size makes sure the minimum bound never exceeds the lenght of the main data set
## Greatest lower bound if you will 
my_stoch_traj_boot <- function(data, min_wind_size){
  N_data <- nrow(data)
  
  boot_start_index <- round(runif(n = 1, min = 1, max = (N_data - min_wind_size) ))
  boot_end_index <- round(runif(n = 1, min = boot_start_index, max = N_data ))
  
  x <- data[boot_start_index:boot_end_index, ]
  return(x)
}