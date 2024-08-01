# Load necessary library
if (!require(qcc)) install.packages("qcc", dependencies = TRUE)
library(qcc)

set.seed(12)
# Define parameters
n_samples <- 30    # Number of samples
sample_size <- 7   # Size of each sample
mean_weight <- 12.9  # Average weight of cookies in grams
sd_weight <- 0.1     # Standard deviation (you can adjust this as needed)

# Generate normally distributed data
data_matrix <- matrix(rnorm(n_samples * sample_size, mean = mean_weight, sd = sd_weight), 
                      nrow = n_samples, ncol = sample_size)

# Create an X-bar chart using the qcc package
xbar_chart <- qcc(data_matrix, type = "xbar", sizes = rep(sample_size, n_samples), title = "X-bar Chart for Cookie Weights")

# Extract UCL and LCL from the original X-bar chart
original_ucl <- xbar_chart$limits[, "UCL"]
original_lcl <- xbar_chart$limits[, "LCL"]

# Print the UCL and LCL from the original X-bar chart
print(paste("Original UCL:", original_ucl))
print(paste("Original LCL:", original_lcl))
#[1] "Original UCL: 13.007"
#[1] "Original LCL: 12.787"


xbar_chart$violations


###############################################################
#now let's say that there is a small shift in the mean,half sd
set.seed(1)
new_mean_weight <- 12.95
new_n_samples <- 10
# Generate normally distributed data
new_data_matrix<- matrix(rnorm(new_n_samples  * sample_size, mean = new_mean_weight, sd = sd_weight), 
                   nrow = new_n_samples , ncol = sample_size)



# Create a new X-bar chart using the combined data
combined_xbar_chart <- qcc(data_matrix,newdata = new_data_matrix,type="xbar",title = "",
                           ylim = c(12.787,13.05)) 
title("Control Chart with small shift in the mean (0.5 sd)", cex.main=1.5)

combined_xbar_chart$violations


#######################################################
#now a large shift, 2sd
set.seed(2)
large_mean_weight <- 13.1

# Generate new normally distributed data with the large new mean
large_new_data <- matrix(rnorm(new_n_samples * sample_size, mean = large_mean_weight, sd = sd_weight), 
                         nrow = new_n_samples, ncol = sample_size)


large_new_xbar_chart <- qcc(data_matrix, newdata = large_new_data, type="xbar", title = "",
                            ylim = c(12.787,13.18))
title("Combined X-bar Chart for New Cookie Weights After Large Mean Shift", cex.main=0.9)

#######################################################
#now the s charts for sd

# S chart for sd change

s_chart<- qcc(data_matrix,type="S",title = "")
title("S bar Control Chart", cex.main=2)

s_chart$center
s_chart$limits

#for small shift in sd

new_sd_weight <- 0.15
set.seed(2)
new_sd_matrix<- matrix(rnorm(new_n_samples  * sample_size, mean = mean_weight, sd = new_sd_weight), 
                         nrow = new_n_samples , ncol = sample_size)
new_s_chart<- qcc(data_matrix,newdata =new_sd_matrix,type="S",title = "")
title("Combined S bar Control Chart/Small shift", cex.main=1.5)

#for large shift in sd

large_sd_weight<-0.3
set.seed(2)
large_sd_matrix<- matrix(rnorm(new_n_samples  * sample_size, mean = mean_weight, sd = large_sd_weight), 
                       nrow = new_n_samples , ncol = sample_size)
large_s_chart<- qcc(data_matrix,newdata =large_sd_matrix,type="S",title = "")
title("Combined S bar Control Chart/Large shift", cex.main=1.5)

#EWMA chart

ewma(data_matrix,lambda = 0.2)
ewma(data_matrix,lambda = 0.2,newdata = new_data_matrix)
ewma(data_matrix,lambda = 0.2,newdata = large_new_data)

#CUSUM chart

cusum(data_matrix)
cusum(data_matrix,newdata = new_data_matrix)

cusum(data_matrix,newdata = large_new_data)
