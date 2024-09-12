# Make conjoint surveys using the cbcTools package

# Install packages
# install.packages("remotes")
# install.packages("tidyverse")
# remotes::install_github("jhelvy/cbcTools")

# Load libraries
library(cbcTools)
library(tidyverse)

# Define profiles with attributes and levels
#profiles <- cbc_profiles(
#    type      = c('Fuji', 'Gala', 'Honeycrisp', 'Pink Lady', 'Red Delicious'),
#    price     = seq(1, 4, 0.5), # $ per pound 
#    freshness = c('Excellent', 'Average', 'Poor')
#)

profiles <- cbc_profiles(
  tired      = c('never', 'almost never', 'sometimes', 'often', 'always'),
  walking      = c('never', 'almost never', 'sometimes', 'often', 'always'),
  sports      = c('never', 'almost never', 'sometimes', 'often', 'always'),
  concentration      = c('never', 'almost never', 'sometimes', 'often', 'always'),
  embarrassed      = c('never', 'almost never', 'sometimes', 'often', 'always'),
  unhappiness      = c('never', 'almost never', 'sometimes', 'often', 'always'),
  treated      = c('never', 'almost never', 'sometimes', 'often', 'always')
)

# Make a basic survey using the full factorial of all profiles
design <- cbc_design(
    profiles = profiles,
    n_resp   = 96, # Number of respondents
    n_alts   = 2,    # Number of alternatives per question
    n_q      = 8,     # Number of questions per respondent
    n_blocks = 8,
    method = "Modfed"
)

head(design) # preview
design[1:20,]

# Save design
write_csv(design, 'choice_questions.csv')
