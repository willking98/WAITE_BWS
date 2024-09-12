# OMEP

# Load the necessary libraries
library(DoE.base)
library(AlgDesign)

# Define the number of attributes and levels
num_attributes <- 7
num_levels <- 5
num_tasks <- 100 # Total number of unique tasks (adjust as needed)
num_participants <- 1000 # Total number of participants
tasks_per_participant <- 8 # Number of tasks per participant

# Generate an orthogonal array with 7 attributes and 5 levels
omep_design <- oa.design(nruns = 100, nfactors = num_attributes, nlevels = num_levels, factor.names = paste0("Attr", 1:num_attributes))

# Randomize the combined design
randomized_design <- omep_design[sample(1:nrow(omep_design), replace = F), ]

# Generate profile IDs for each unique task
profileIDs <- paste0(final_design$Attr1, final_design$Attr2, final_design$Attr3,
                     final_design$Attr4, final_design$Attr5, final_design$Attr6,
                     final_design$Attr7)

# Add profile IDs to the design
final_design <- cbind(profileID = profileIDs, final_design)

# Prepare dataframe to assign tasks to participants
respID <- rep(1:num_participants, each = tasks_per_participant)
qID <- rep(1:tasks_per_participant, num_participants)
altID <- rep(1, num_participants * tasks_per_participant)
obsID <- 1:(num_participants * tasks_per_participant)

# Randomly assign tasks to participants
assigned_tasks <- final_design[sample(1:nrow(final_design), num_participants * tasks_per_participant, replace = TRUE), ]

# Convert numeric levels to descriptive text
level_labels <- c('never', 'almost never', 'sometimes', 'often', 'always')

# Apply labels to each attribute
assigned_tasks$Attr1 <- factor(assigned_tasks$Attr1, levels = 1:num_levels, labels = level_labels)
assigned_tasks$Attr2 <- factor(assigned_tasks$Attr2, levels = 1:num_levels, labels = level_labels)
assigned_tasks$Attr3 <- factor(assigned_tasks$Attr3, levels = 1:num_levels, labels = level_labels)
assigned_tasks$Attr4 <- factor(assigned_tasks$Attr4, levels = 1:num_levels, labels = level_labels)
assigned_tasks$Attr5 <- factor(assigned_tasks$Attr5, levels = 1:num_levels, labels = level_labels)
assigned_tasks$Attr6 <- factor(assigned_tasks$Attr6, levels = 1:num_levels, labels = level_labels)
assigned_tasks$Attr7 <- factor(assigned_tasks$Attr7, levels = 1:num_levels, labels = level_labels)

# Combine all the necessary information into one dataframe
result_df <- data.frame(
  profileID = assigned_tasks$profileID,
  respID = respID,
  qID = qID,
  altID = altID,
  obsID = obsID,
  tired = as.character(assigned_tasks$Attr1),
  walking = as.character(assigned_tasks$Attr2),
  sports = as.character(assigned_tasks$Attr3),
  concentration = as.character(assigned_tasks$Attr4),
  embarrassed = as.character(assigned_tasks$Attr5),
  unhappiness = as.character(assigned_tasks$Attr6),
  treated = as.character(assigned_tasks$Attr7)
)


# Optionally, export the dataframe to a CSV file
write.csv(result_df, "BWS_design_for_1000_participants.csv", row.names = FALSE)



