# Clear environment and console
remove(list = ls())
shell("cls")
  
# Load Libraries set Working Directory
library(gtools)
library(readr)
library(data.table)
library(MXM)
library(diagram)
library(stringr)
library(rapportools)
#remove.packages("")

dir <- "...\\Data\\"
setwd(dir)
  
# Load and combine data
  # https://www.federalreserve.gov/datadownload/Choose.aspx?rel=CHGDEL
  

files <- list.files(path=dir, pattern="*.csv", full.names=TRUE)
file_names <-list.files(path=dir, pattern="*.csv", full.names=FALSE)
file_names <- str_replace_all(file_names,".csv","")
#[1] "Average loan volume_Other banks.csv"         "Average loan volume_All banks.csv"          
#[3] "Average loan volume_Top 100 banks.csv"       "Charge-off rates_All banks.csv"             
#[5] "Charge-off rates_Other banks.csv"            "Charge-off rates_Top 100 banks.csv"         
#[7] "Charge-offs_All banks.csv"                   "Charge-offs_Other banks.csv"                
#[9] "Charge-offs_Top 100 banks.csv"               "Delinquencies_All banks.csv"                
#[11] "Delinquencies_Other banks.csv"               "Delinquency rates_All banks.csv"            
#[13] "Delinquency rates_Other banks.csv"           "Delinquency rates_Top 100 banks.csv"        
#[15] "Delinquency_Top 100 banks.csv"               "End-of-period loan volume_Top 100 banks.csv"
#[17] "End-of-period loan volume_All banks.csv"     "End-of-period loan volume_Other banks.csv"  

for (f in 1:length(files)) {
  d <- read_csv(files[f]) # load fiirst file of files list (see blow for combining all files)
  d <- d[c(-1:-5),]# remove first five rows - Unit, Multiplier, Currency, Unique ID, Time Period
  d <- as.data.frame(d)
  
  # for (i in 2:length(files)) { # cbind all data sets - combining files greatly slows down the feature importance process
  #   temp <- read_csv(files[i])
  #   temp <- temp[c(-1:-5),] # remove first five rows - Unit, Multiplier, Currency, Unique ID, Time Period
  #   temp <- temp[,-1] # revove first column 'Series Description' from appended data sets 
  #   temp <- as.data.frame(temp)
  #   d <- cbind(d,temp)
  # }
  
  for (i in 2:ncol(d)) { # convert chr to numeric
    d[,i] <- as.numeric(d[,i])
  }
  
  col_names <- colnames(d) # create list of original columns names
  colnames(d)<- c(c(1:ncol(d))) # rename column names with numbers
  
  ind <- unique(which(is.na(d), arr.ind=TRUE)[,1]) 
  if (is.empty(ind) == FALSE) { # remove rows with any column that is missing data
    d <- d[-ind,]# remove rows with missing data
  }

  d_n <- as.data.frame(apply(d[, 2:ncol(d)], 2, function(x) (x - min(x))/(max(x)-min(x)))) # normalize data
  d_l <- d[,1] # inititalize lagged data matrix
  
  for (i in 2:(ncol(d_n)+1)) { # add one step lag to data
    temp <- c(NA, head(d_n[,(i-1)], -1))
    d_l <- cbind(d_l,temp)
    colnames(d_l)[ncol(d_l)] <- paste(colnames(d_n)[i-1],"_1")
  }
  d_n <- d_n[-1,]
  d_l <- d_l[-1,-1]
  d_l <- as.data.frame(d_l)
  for (i in 1:ncol(d_l)) { # convert chr to numeric
    d_l[,i] <- as.numeric(d_l[,i])
  }
  
  # Feature Importance - one vs all for each feature
    # https://cran.r-project.org/web/packages/MXM/vignettes/MMPC_tutorial.html
  
  vars <- matrix(, nrow = ncol(d_n), ncol = 4)
  for (i in 1:ncol(d_l)) {
    mod <- MXM::MMPC(
      d_n[,i],           # The target variable vector
      d_l,          # All lagged features
      max_k = 3,        # The maximum size of the conditioning set to use 
      threshold = 0.05, # level of alpha for statistical  significance
      test = 'testIndFisher',   
      ini = NULL,        # if TRUE, the calculated univariate associations
      # are stored for runtime efficiency in subsequent 
      # MMPC runs with diferent hyper-parameters.
      hash =  TRUE,      # if TRUE, the calculated statistics are stored.
      hashObject = NULL, # the mmpcobject from a previous run
      ncores = 1,        # number of cores for parallel execution. 
      # Recommended for thousands of variables.
      backward = TRUE)   # If TRUE, the backward phase
    vars[i,1] <- i # populate target column name
    vars[i,2] <- mod@selectedVarsOrder[1] # populate best feature name
  }
  
  # Create network visualization
    # https://www.rdocumentation.org/packages/diagram/versions/1.6.4/topics/plotmat
  
  M <- matrix(nrow = ncol(d_n), ncol = ncol(d_n), byrow = TRUE, data = 0)
  for (i in 1:ncol(d_n)) {
    to <- vars[i,1]
    from <- vars[i,2]
    M[to,from] <- -1
  }
  
  pp <- plotmat(M, name = c(colnames(d_n)),
                lwd = 1, box.lwd = 2, cex.txt = 0.8, 
                box.size = 0.05, box.type = "circle", box.prop = 0.3,
                main = file_names[f])
  
  # create data tables for network visualization
  leg <- vars
  leg[,1] <- leg[,1] + 1
  leg[,3] <-leg[,2] + 1
  leg[,2] <- col_names[2:(ncol(d_n)+1)]
  colnames(leg) <- c("Target Feature Number","Target Feature Description","Best Lagged [1-qtr] Feature Number","Lagged Feature Description")
  for (i in 1:nrow(leg)) {
    ind <- which(leg[i,3]==leg[,3])
    leg[ind,4] <- leg[i,2]
  }
  
  # Export tables
  write.csv(leg,file = paste0(file_names[f],"_results.csv"))
  
  # Create table of features where best lagged deature is itself
  ind <- which(leg[,1] == leg[,3])
  leg_self <- leg[ind,]
  
  # Create table of features where best lagged deature is not itself
  ind <- which(leg[,1] != leg[,3])
  leg_nself <- leg[ind,]
}
