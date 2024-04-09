### Sample Group Randomization
### To randomize samples into groups for assays such as RNA isolation
### Author: SAL
### Date: 04-01-24

library(dplyr)

# CHANGE ONLY THESE VALUES
# Assign file output path
setwd("Working/Dir/Here")

experiment_name <- "Experiment_Name"

# Input # of samples to be randomized
num_samples <- 120

# Input # of groups to randomly assign samples to
num_groups <- 9

# Input desired # of samples per group
group_size <- 14

###########################################################

# Vector of sample IDs from 1 - num_samples
sample_list <- seq(from = 1, to = num_samples, by = 1)

# Make list of groups for easy access
group <- list()

# Vector to add already-used sample IDs to 
# To prevent duplicate sample assignment
already_used <- numeric()

# Loop to assign samples to groups
for (i in 1:num_groups) {
    if (length(sample_list) > group_size) {
        samples <- sample(sample_list, group_size, replace = FALSE)
        already_used <- c(already_used, samples)
        sample_list <- sample_list[!(sample_list %in% already_used)]

        group[[i]] <- samples
    } else {
        samples <- sample_list
        group[[i]] <- samples
    }
}

# Reformat into dataframe for easy pasting into excel file
# Each list is now a column
group_df <- as.data.frame(do.call(cbind, group))

# Check each column for duplicate values
# Remove if necessary
for (i in 1:ncol(group_df)) {
    dup_values <- which(duplicated(group_df[, i]))
    if (length(dup_values) > 0) {
        group_df[dup_values, i] <- NA
    }
}

# Check that # of values in group_df matches num_samples
ifelse(nrow(group_df) * ncol(group_df) - length(which(is.na(group_df))) == num_samples,
       write.csv(group_df, paste0(experiment_name, "_Group_Randomization.csv")),
       "Error: number of values in randomized dataframe does not match num_samples input. Verify inputs and re-run script.")
