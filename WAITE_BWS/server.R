#################################
# Deifne the Conjoint Questions #
################################# 

# Read in the full survey design file
design <- readr::read_csv("BWS_design_for_1000_participants.csv")

# database setup
db <- sd_database(
  host   = "aws-0-eu-west-2.pooler.supabase.com",
  dbname = "postgres",
  port   = "6543",
  user   = "postgres.msujfghwfbdvnnejymgi",
  table  = "AdultBWS",
  ignore = TRUE
)

#####################
# If running for first time block out below 

# Get all previously used respIDs
data <- sd_get_data(db)
respIDs <- unique(data$respID)

# Sample a respID from full design file
#design <- readr::read_csv("choice_questions.csv")
respID <- 1
# Keep sampling until you get one that hasn't yet been used
while (respID %in% respIDs) {
  respID <- sample(design$respID, 1)
}
#####################
# and enable below
# respID <- 1
#####################

# Store the respondentID
sd_store_value(respID)


# # Sample a random respondentID
respondentID <- respID

# Store the respondentID
sd_store_value(respondentID, "respID")

# Filter for the rows for the chosen respondentID
df <- design %>%
  filter(respID == respondentID) 

# Function to create the labels for a choice question
# based on the values in df

make_cbc_options <- function(df) {
  alt1 <- df |> filter(altID == 1)
  
  options <- c("tired", "walking", "sports", "concentration", "embarrassed", "unhappiness", "treated")

  names(options) <- c(
    glue("I **{alt1$tired}** get tired"),
    glue("I **{alt1$walking}** struggle to keep up when I am walking around with others"),
    glue("I **{alt1$sports}** avoid doing sports"),
    glue("I **{alt1$concentration}** struggle to concentrate on my studies/work"),
    glue("I **{alt1$embarrassed}** feel embarrassed shopping for clothes"),
    glue("I **{alt1$unhappiness}** feel unhappy because I am unable to do the same things as others"),
    glue("People **{alt1$treated}** treat me differently when I go out")
  )
  
  return(options)
}

#########################################################################
# Functions for each attribute to be missing in the worst question
#########################################################################
make_cbc_options_tired <- function(df) {
  alt1 <- df |> filter(altID == 1)
  
  options <- c("walking", "sports", "concentration", "embarrassed", "unhappiness", "treated")

  names(options) <- c(
    glue("I **{alt1$walking}** struggle to keep up when I am walking around with others"),
    glue("I **{alt1$sports}** avoid doing sports"),
    glue("I **{alt1$concentration}** struggle to concentrate on my studies/work"),
    glue("I **{alt1$embarrassed}** feel embarrassed shopping for clothes"),
    glue("I **{alt1$unhappiness}** feel unhappy because I am unable to do the same things as others"),
    glue("People **{alt1$treated}** treat me differently when I go out")
  )
  
  return(options)
}

make_cbc_options_walking <- function(df) {
  alt1 <- df |> filter(altID == 1)
  
  options <- c("tired", "sports", "concentration", "embarrassed", "unhappiness", "treated")

  names(options) <- c(
    glue("I **{alt1$tired}** get tired"),
    glue("I **{alt1$sports}** avoid doing sports"),
    glue("I **{alt1$concentration}** struggle to concentrate on my studies/work"),
    glue("I **{alt1$embarrassed}** feel embarrassed shopping for clothes"),
    glue("I **{alt1$unhappiness}** feel unhappy because I am unable to do the same things as others"),
    glue("People **{alt1$treated}** treat me differently when I go out")
  )
  
  return(options)
}

make_cbc_options_sports <- function(df) {
  alt1 <- df |> filter(altID == 1)
  
  options <- c("tired", "walking", "concentration", "embarrassed", "unhappiness", "treated")

  names(options) <- c(
    glue("I **{alt1$tired}** get tired"),
    glue("I **{alt1$walking}** struggle to keep up when I am walking around with others"),
    glue("I **{alt1$concentration}** struggle to concentrate on my studies/work"),
    glue("I **{alt1$embarrassed}** feel embarrassed shopping for clothes"),
    glue("I **{alt1$unhappiness}** feel unhappy because I am unable to do the same things as others"),
    glue("People **{alt1$treated}** treat me differently when I go out")
  )
  
  return(options)
}

make_cbc_options_concentration <- function(df) {
  alt1 <- df |> filter(altID == 1)
  
  options <- c("tired", "walking", "sports", "embarrassed", "unhappiness", "treated")
  
  names(options) <- c(
    glue("I **{alt1$tired}** get tired"),
    glue("I **{alt1$walking}** struggle to keep up when I am walking around with others"),
    glue("I **{alt1$sports}** avoid doing sports"),
    glue("I **{alt1$embarrassed}** feel embarrassed shopping for clothes"),
    glue("I **{alt1$unhappiness}** feel unhappy because I am unable to do the same things as others"),
    glue("People **{alt1$treated}** treat me differently when I go out")
  )
  
  return(options)
}
make_cbc_options_embarrassed <- function(df) {
  alt1 <- df |> filter(altID == 1)
  
  options <- c("tired", "walking", "sports", "concentration", "unhappiness", "treated")
  
  names(options) <- c(
    glue("I **{alt1$tired}** get tired"),
    glue("I **{alt1$walking}** struggle to keep up when I am walking around with others"),
    glue("I **{alt1$sports}** avoid doing sports"),
    glue("I **{alt1$concentration}** struggle to concentrate on my studies/work"),
    glue("I **{alt1$unhappiness}** feel unhappy because I am unable to do the same things as others"),
    glue("People **{alt1$treated}** treat me differently when I go out")
  )
  
  return(options)
}
make_cbc_options_unhappiness <- function(df) {
  alt1 <- df |> filter(altID == 1)
  
  options <- c("tired", "walking", "sports", "concentration", "embarrassed", "treated")
  
  names(options) <- c(
    glue("I **{alt1$tired}** get tired"),
    glue("I **{alt1$walking}** struggle to keep up when I am walking around with others"),
    glue("I **{alt1$sports}** avoid doing sports"),
    glue("I **{alt1$concentration}** struggle to concentrate on my studies/work"),
    glue("I **{alt1$embarrassed}** feel embarrassed shopping for clothes"),
    glue("People **{alt1$treated}** treat me differently when I go out")
  )
  
  return(options)
}
make_cbc_options_treated <- function(df) {
  alt1 <- df |> filter(altID == 1)
  
  options <- c("tired", "walking", "sports", "concentration", "embarrassed", "unhappiness")
  
  names(options) <- c(
    glue("I **{alt1$tired}** get tired"),
    glue("I **{alt1$walking}** struggle to keep up when I am walking around with others"),
    glue("I **{alt1$sports}** avoid doing sports"),
    glue("I **{alt1$concentration}** struggle to concentrate on my studies/work"),
    glue("I **{alt1$embarrassed}** feel embarrassed shopping for clothes"),
    glue("I **{alt1$unhappiness}** feel unhappy because I am unable to do the same things as others")
  )
  
  return(options)
}
# Create the options for each choice question

cbc1_options <- make_cbc_options(df |> filter(qID == 1))
cbc1_options_w_tired <- make_cbc_options_tired(df |> filter(qID == 1))
cbc1_options_w_walking <- make_cbc_options_walking(df |> filter(qID == 1))
cbc1_options_w_sports <- make_cbc_options_sports(df |> filter(qID == 1))
cbc1_options_w_concentration <- make_cbc_options_concentration(df |> filter(qID == 1))
cbc1_options_w_embarrassed <- make_cbc_options_embarrassed(df |> filter(qID == 1))
cbc1_options_w_unhappiness <- make_cbc_options_unhappiness(df |> filter(qID == 1))
cbc1_options_w_treated <- make_cbc_options_treated(df |> filter(qID == 1))

###############################################################################

cbc2_options <- make_cbc_options(df |> filter(qID == 2))
cbc2_options_w_tired <- make_cbc_options_tired(df |> filter(qID == 2))
cbc2_options_w_walking <- make_cbc_options_walking(df |> filter(qID == 2))
cbc2_options_w_sports <- make_cbc_options_sports(df |> filter(qID == 2))
cbc2_options_w_concentration <- make_cbc_options_concentration(df |> filter(qID == 2))
cbc2_options_w_embarrassed <- make_cbc_options_embarrassed(df |> filter(qID == 2))
cbc2_options_w_unhappiness <- make_cbc_options_unhappiness(df |> filter(qID == 2))
cbc2_options_w_treated <- make_cbc_options_treated(df |> filter(qID == 2))

###############################################################################

cbc3_options <- make_cbc_options(df |> filter(qID == 3))
cbc3_options_w_tired <- make_cbc_options_tired(df |> filter(qID == 3))
cbc3_options_w_walking <- make_cbc_options_walking(df |> filter(qID == 3))
cbc3_options_w_sports <- make_cbc_options_sports(df |> filter(qID == 3))
cbc3_options_w_concentration <- make_cbc_options_concentration(df |> filter(qID == 3))
cbc3_options_w_embarrassed <- make_cbc_options_embarrassed(df |> filter(qID == 3))
cbc3_options_w_unhappiness <- make_cbc_options_unhappiness(df |> filter(qID == 3))
cbc3_options_w_treated <- make_cbc_options_treated(df |> filter(qID == 3))

###############################################################################

cbc4_options <- make_cbc_options(df |> filter(qID == 4))
cbc4_options_w_tired <- make_cbc_options_tired(df |> filter(qID == 4))
cbc4_options_w_walking <- make_cbc_options_walking(df |> filter(qID == 4))
cbc4_options_w_sports <- make_cbc_options_sports(df |> filter(qID == 4))
cbc4_options_w_concentration <- make_cbc_options_concentration(df |> filter(qID == 4))
cbc4_options_w_embarrassed <- make_cbc_options_embarrassed(df |> filter(qID == 4))
cbc4_options_w_unhappiness <- make_cbc_options_unhappiness(df |> filter(qID == 4))
cbc4_options_w_treated <- make_cbc_options_treated(df |> filter(qID == 4))

###############################################################################

cbc5_options <- make_cbc_options(df |> filter(qID == 5))
cbc5_options_w_tired <- make_cbc_options_tired(df |> filter(qID == 5))
cbc5_options_w_walking <- make_cbc_options_walking(df |> filter(qID == 5))
cbc5_options_w_sports <- make_cbc_options_sports(df |> filter(qID == 5))
cbc5_options_w_concentration <- make_cbc_options_concentration(df |> filter(qID == 5))
cbc5_options_w_embarrassed <- make_cbc_options_embarrassed(df |> filter(qID == 5))
cbc5_options_w_unhappiness <- make_cbc_options_unhappiness(df |> filter(qID == 5))
cbc5_options_w_treated <- make_cbc_options_treated(df |> filter(qID == 5))

###############################################################################

cbc6_options <- make_cbc_options(df |> filter(qID == 6))
cbc6_options_w_tired <- make_cbc_options_tired(df |> filter(qID == 6))
cbc6_options_w_walking <- make_cbc_options_walking(df |> filter(qID == 6))
cbc6_options_w_sports <- make_cbc_options_sports(df |> filter(qID == 6))
cbc6_options_w_concentration <- make_cbc_options_concentration(df |> filter(qID == 6))
cbc6_options_w_embarrassed <- make_cbc_options_embarrassed(df |> filter(qID == 6))
cbc6_options_w_unhappiness <- make_cbc_options_unhappiness(df |> filter(qID == 6))
cbc6_options_w_treated <- make_cbc_options_treated(df |> filter(qID == 6))

###############################################################################

cbc7_options <- make_cbc_options(df |> filter(qID == 7))
cbc7_options_w_tired <- make_cbc_options_tired(df |> filter(qID == 7))
cbc7_options_w_walking <- make_cbc_options_walking(df |> filter(qID == 7))
cbc7_options_w_sports <- make_cbc_options_sports(df |> filter(qID == 7))
cbc7_options_w_concentration <- make_cbc_options_concentration(df |> filter(qID == 7))
cbc7_options_w_embarrassed <- make_cbc_options_embarrassed(df |> filter(qID == 7))
cbc7_options_w_unhappiness <- make_cbc_options_unhappiness(df |> filter(qID == 7))
cbc7_options_w_treated <- make_cbc_options_treated(df |> filter(qID == 7))

###############################################################################

cbc8_options <- make_cbc_options(df |> filter(qID == 8))
cbc8_options_w_tired <- make_cbc_options_tired(df |> filter(qID == 8))
cbc8_options_w_walking <- make_cbc_options_walking(df |> filter(qID == 8))
cbc8_options_w_sports <- make_cbc_options_sports(df |> filter(qID == 8))
cbc8_options_w_concentration <- make_cbc_options_concentration(df |> filter(qID == 8))
cbc8_options_w_embarrassed <- make_cbc_options_embarrassed(df |> filter(qID == 8))
cbc8_options_w_unhappiness <- make_cbc_options_unhappiness(df |> filter(qID == 8))
cbc8_options_w_treated <- make_cbc_options_treated(df |> filter(qID == 8))

###############################################################################

# Create each choice question

sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q1',
  label  = "(1 of 8) Select the **best** option",
  option = cbc1_options
)

sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q1_w_tired',
  label  = "(1 of 8) Select the **worst** option",
  option = cbc1_options_w_tired
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q1_w_walking',
  label  = "(1 of 8) Select the **worst** option",
  option = cbc1_options_w_walking
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q1_w_sports',
  label  = "(1 of 8) Select the **worst** option",
  option = cbc1_options_w_sports
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q1_w_concentration',
  label  = "(1 of 8) Select the **worst** option",
  option = cbc1_options_w_concentration
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q1_w_embarrassed',
  label  = "(1 of 8) Select the **worst** option",
  option = cbc1_options_w_embarrassed
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q1_w_unhappiness',
  label  = "(1 of 8) Select the **worst** option",
  option = cbc1_options_w_unhappiness
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q1_w_treated',
  label  = "(1 of 8) Select the **worst** option",
  option = cbc1_options_w_treated
)

###############################################################################
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q2',
  label  = "(2 of 8) Select the **best** option",
  option = cbc2_options
)

sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q2_w_tired',
  label  = "(2 of 8) Select the **worst** option",
  option = cbc2_options_w_tired
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q2_w_walking',
  label  = "(2 of 8) Select the **worst** option",
  option = cbc2_options_w_walking
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q2_w_sports',
  label  = "(2 of 8) Select the **worst** option",
  option = cbc2_options_w_sports
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q2_w_concentration',
  label  = "(2 of 8) Select the **worst** option",
  option = cbc2_options_w_concentration
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q2_w_embarrassed',
  label  = "(2 of 8) Select the **worst** option",
  option = cbc2_options_w_embarrassed
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q2_w_unhappiness',
  label  = "(2 of 8) Select the **worst** option",
  option = cbc2_options_w_unhappiness
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q2_w_treated',
  label  = "(2 of 8) Select the **worst** option",
  option = cbc2_options_w_treated
)

###############################################################################
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q3',
  label  = "(3 of 8) Select the **best** option",
  option = cbc3_options
)

sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q3_w_tired',
  label  = "(3 of 8) Select the **worst** option",
  option = cbc3_options_w_tired
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q3_w_walking',
  label  = "(3 of 8) Select the **worst** option",
  option = cbc3_options_w_walking
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q3_w_sports',
  label  = "(3 of 8) Select the **worst** option",
  option = cbc3_options_w_sports
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q3_w_concentration',
  label  = "(3 of 8) Select the **worst** option",
  option = cbc3_options_w_concentration
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q3_w_embarrassed',
  label  = "(3 of 8) Select the **worst** option",
  option = cbc3_options_w_embarrassed
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q3_w_unhappiness',
  label  = "(3 of 8) Select the **worst** option",
  option = cbc3_options_w_unhappiness
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q3_w_treated',
  label  = "(3 of 8) Select the **worst** option",
  option = cbc3_options_w_treated
)

###############################################################################
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q4',
  label  = "(4 of 8) Select the **best** option",
  option = cbc4_options
)

sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q4_w_tired',
  label  = "(4 of 8) Select the **worst** option",
  option = cbc4_options_w_tired
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q4_w_walking',
  label  = "(4 of 8) Select the **worst** option",
  option = cbc4_options_w_walking
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q4_w_sports',
  label  = "(4 of 8) Select the **worst** option",
  option = cbc4_options_w_sports
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q4_w_concentration',
  label  = "(4 of 8) Select the **worst** option",
  option = cbc4_options_w_concentration
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q4_w_embarrassed',
  label  = "(4 of 8) Select the **worst** option",
  option = cbc4_options_w_embarrassed
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q4_w_unhappiness',
  label  = "(4 of 8) Select the **worst** option",
  option = cbc4_options_w_unhappiness
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q4_w_treated',
  label  = "(4 of 8) Select the **worst** option",
  option = cbc4_options_w_treated
)

###############################################################################
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q5',
  label  = "(5 of 8) Select the **best** option",
  option = cbc5_options
)

sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q5_w_tired',
  label  = "(5 of 8) Select the **worst** option",
  option = cbc5_options_w_tired
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q5_w_walking',
  label  = "(5 of 8) Select the **worst** option",
  option = cbc5_options_w_walking
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q5_w_sports',
  label  = "(5 of 8) Select the **worst** option",
  option = cbc5_options_w_sports
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q5_w_concentration',
  label  = "(5 of 8) Select the **worst** option",
  option = cbc5_options_w_concentration
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q5_w_embarrassed',
  label  = "(5 of 8) Select the **worst** option",
  option = cbc5_options_w_embarrassed
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q5_w_unhappiness',
  label  = "(5 of 8) Select the **worst** option",
  option = cbc5_options_w_unhappiness
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q5_w_treated',
  label  = "(5 of 8) Select the **worst** option",
  option = cbc5_options_w_treated
)

###############################################################################
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q6',
  label  = "(6 of 8) Select the **best** option",
  option = cbc6_options
)

sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q6_w_tired',
  label  = "(6 of 8) Select the **worst** option",
  option = cbc6_options_w_tired
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q6_w_walking',
  label  = "(6 of 8) Select the **worst** option",
  option = cbc6_options_w_walking
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q6_w_sports',
  label  = "(6 of 8) Select the **worst** option",
  option = cbc6_options_w_sports
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q6_w_concentration',
  label  = "(6 of 8) Select the **worst** option",
  option = cbc6_options_w_concentration
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q6_w_embarrassed',
  label  = "(6 of 8) Select the **worst** option",
  option = cbc6_options_w_embarrassed
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q6_w_unhappiness',
  label  = "(6 of 8) Select the **worst** option",
  option = cbc6_options_w_unhappiness
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q6_w_treated',
  label  = "(6 of 8) Select the **worst** option",
  option = cbc6_options_w_treated
)

###############################################################################
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q7',
  label  = "(7 of 8) Select the **best** option",
  option = cbc7_options
)

sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q7_w_tired',
  label  = "(7 of 8) Select the **worst** option",
  option = cbc7_options_w_tired
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q7_w_walking',
  label  = "(7 of 8) Select the **worst** option",
  option = cbc7_options_w_walking
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q7_w_sports',
  label  = "(7 of 8) Select the **worst** option",
  option = cbc7_options_w_sports
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q7_w_concentration',
  label  = "(7 of 8) Select the **worst** option",
  option = cbc7_options_w_concentration
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q7_w_embarrassed',
  label  = "(7 of 8) Select the **worst** option",
  option = cbc7_options_w_embarrassed
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q7_w_unhappiness',
  label  = "(7 of 8) Select the **worst** option",
  option = cbc7_options_w_unhappiness
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q7_w_treated',
  label  = "(7 of 8) Select the **worst** option",
  option = cbc7_options_w_treated
)

###############################################################################
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q8',
  label  = "(8 of 8) Select the **best** option",
  option = cbc8_options
)

sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q8_w_tired',
  label  = "(8 of 8) Select the **worst** option",
  option = cbc8_options_w_tired
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q8_w_walking',
  label  = "(8 of 8) Select the **worst** option",
  option = cbc8_options_w_walking
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q8_w_sports',
  label  = "(8 of 8) Select the **worst** option",
  option = cbc8_options_w_sports
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q8_w_concentration',
  label  = "(8 of 8) Select the **worst** option",
  option = cbc8_options_w_concentration
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q8_w_embarrassed',
  label  = "(8 of 8) Select the **worst** option",
  option = cbc8_options_w_embarrassed
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q8_w_unhappiness',
  label  = "(8 of 8) Select the **worst** option",
  option = cbc8_options_w_unhappiness
)
sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q8_w_treated',
  label  = "(8 of 8) Select the **worst** option",
  option = cbc8_options_w_treated
)

###############################################################################
# Practice question

show_tired <- function(input) {
  return(input$cbc_practice_best == "tired")
}
show_walking <- function(input) {
  return(input$cbc_practice_best == "walking")
}
show_sports <- function(input) {
  return(input$cbc_practice_best == "sports")
}
show_concentration <- function(input) {
  return(input$cbc_practice_best == "concentration")
}
show_embarrassed <- function(input) {
  return(input$cbc_practice_best == "embarrassed")
}
show_unhappiness <- function(input) {
  return(input$cbc_practice_best == "unhappiness")
}
show_treated <- function(input) {
  return(input$cbc_practice_best == "treated")
}

###############################################################################
# Question 1

show_tired_q1 <- function(input) {
  return(input$cbc_q1 == "tired")
}
show_walking_q1 <- function(input) {
  return(input$cbc_q1 == "walking")
}
show_sports_q1 <- function(input) {
  return(input$cbc_q1 == "sports")
}
show_concentration_q1 <- function(input) {
  return(input$cbc_q1 == "concentration")
}
show_embarrassed_q1 <- function(input) {
  return(input$cbc_q1 == "embarrassed")
}
show_unhappiness_q1 <- function(input) {
  return(input$cbc_q1 == "unhappiness")
}
show_treated_q1 <- function(input) {
  return(input$cbc_q1 == "treated")
}

###############################################################################
# Question 2

show_tired_q2 <- function(input) {
  return(input$cbc_q2 == "tired")
}
show_walking_q2 <- function(input) {
  return(input$cbc_q2 == "walking")
}
show_sports_q2 <- function(input) {
  return(input$cbc_q2 == "sports")
}
show_concentration_q2 <- function(input) {
  return(input$cbc_q2 == "concentration")
}
show_embarrassed_q2 <- function(input) {
  return(input$cbc_q2 == "embarrassed")
}
show_unhappiness_q2 <- function(input) {
  return(input$cbc_q2 == "unhappiness")
}
show_treated_q2 <- function(input) {
  return(input$cbc_q2 == "treated")
}

###############################################################################
# Question 3

show_tired_q3 <- function(input) {
  return(input$cbc_q3 == "tired")
}
show_walking_q3 <- function(input) {
  return(input$cbc_q3 == "walking")
}
show_sports_q3 <- function(input) {
  return(input$cbc_q3 == "sports")
}
show_concentration_q3 <- function(input) {
  return(input$cbc_q3 == "concentration")
}
show_embarrassed_q3 <- function(input) {
  return(input$cbc_q3 == "embarrassed")
}
show_unhappiness_q3 <- function(input) {
  return(input$cbc_q3 == "unhappiness")
}
show_treated_q3 <- function(input) {
  return(input$cbc_q3 == "treated")
}

###############################################################################
# Question 4

show_tired_q4 <- function(input) {
  return(input$cbc_q4 == "tired")
}
show_walking_q4 <- function(input) {
  return(input$cbc_q4 == "walking")
}
show_sports_q4 <- function(input) {
  return(input$cbc_q4 == "sports")
}
show_concentration_q4 <- function(input) {
  return(input$cbc_q4 == "concentration")
}
show_embarrassed_q4 <- function(input) {
  return(input$cbc_q4 == "embarrassed")
}
show_unhappiness_q4 <- function(input) {
  return(input$cbc_q4 == "unhappiness")
}
show_treated_q4 <- function(input) {
  return(input$cbc_q4 == "treated")
}

###############################################################################
# Question 5

show_tired_q5 <- function(input) {
  return(input$cbc_q5 == "tired")
}
show_walking_q5 <- function(input) {
  return(input$cbc_q5 == "walking")
}
show_sports_q5 <- function(input) {
  return(input$cbc_q5 == "sports")
}
show_concentration_q5 <- function(input) {
  return(input$cbc_q5 == "concentration")
}
show_embarrassed_q5 <- function(input) {
  return(input$cbc_q5 == "embarrassed")
}
show_unhappiness_q5 <- function(input) {
  return(input$cbc_q5 == "unhappiness")
}
show_treated_q5 <- function(input) {
  return(input$cbc_q5 == "treated")
}

###############################################################################
# Question 6

show_tired_q6 <- function(input) {
  return(input$cbc_q6 == "tired")
}
show_walking_q6 <- function(input) {
  return(input$cbc_q6 == "walking")
}
show_sports_q6 <- function(input) {
  return(input$cbc_q6 == "sports")
}
show_concentration_q6 <- function(input) {
  return(input$cbc_q6 == "concentration")
}
show_embarrassed_q6 <- function(input) {
  return(input$cbc_q6 == "embarrassed")
}
show_unhappiness_q6 <- function(input) {
  return(input$cbc_q6 == "unhappiness")
}
show_treated_q6 <- function(input) {
  return(input$cbc_q6 == "treated")
}

###############################################################################
# Question 7

show_tired_q7 <- function(input) {
  return(input$cbc_q7 == "tired")
}
show_walking_q7 <- function(input) {
  return(input$cbc_q7 == "walking")
}
show_sports_q7 <- function(input) {
  return(input$cbc_q7 == "sports")
}
show_concentration_q7 <- function(input) {
  return(input$cbc_q7 == "concentration")
}
show_embarrassed_q7 <- function(input) {
  return(input$cbc_q7 == "embarrassed")
}
show_unhappiness_q7 <- function(input) {
  return(input$cbc_q7 == "unhappiness")
}
show_treated_q7 <- function(input) {
  return(input$cbc_q7 == "treated")
}

###############################################################################
# Question 8

show_tired_q8 <- function(input) {
  return(input$cbc_q8 == "tired")
}
show_walking_q8 <- function(input) {
  return(input$cbc_q8 == "walking")
}
show_sports_q8 <- function(input) {
  return(input$cbc_q8 == "sports")
}
show_concentration_q8 <- function(input) {
  return(input$cbc_q8 == "concentration")
}
show_embarrassed_q8 <- function(input) {
  return(input$cbc_q8 == "embarrassed")
}
show_unhappiness_q8 <- function(input) {
  return(input$cbc_q8 == "unhappiness")
}
show_treated_q8 <- function(input) {
  return(input$cbc_q8 == "treated")
}



################
# config setup #
################

config <- sd_config(
  admin_page = TRUE,
  skip_if = tibble::tribble(
    ~question_id,         ~question_value, ~target,
    "screenout",          "blue",          "end_screenout",
    "consent",        "no",            "end_consent",
    "consent_understand", "no",            "end_consent"
  ),
  # show_if = tibble::tribble(
  #   ~question_id,         ~question_value, ~target,
  #   "cbc_practice_best", "tired", "cbc_practice_w_tired",
  #   "cbc_practice_best", "walking", "cbc_practice_w_walking",
  #   "cbc_practice_best", "sports", "cbc_practice_w_sports",
  #   "cbc_practice_best", "concentration", "cbc_practice_w_concentration",
  #   "cbc_practice_best", "embarrassed", "cbc_practice_w_embarrassed",
  #   "cbc_practice_best", "unhappiness", "cbc_practice_w_unhappiness",
  #   "cbc_practice_best", "treated", "cbc_practice_w_treated"
  # ),
  
  show_if_custom = list(
    list(condition = show_tired, target = "cbc_practice_w_tired"),
    list(condition = show_walking, target = "cbc_practice_w_walking"),
    list(condition = show_sports, target = "cbc_practice_w_sports"),
    list(condition = show_concentration, target = "cbc_practice_w_concentration"),
    list(condition = show_embarrassed, target = "cbc_practice_w_embarrassed"),
    list(condition = show_unhappiness, target = "cbc_practice_w_unhappiness"),
    list(condition = show_treated, target = "cbc_practice_w_treated"),
    
    list(condition = show_tired_q1, target = "cbc_q1_w_tired"),
    list(condition = show_walking_q1, target = "cbc_q1_w_walking"),
    list(condition = show_sports_q1, target = "cbc_q1_w_sports"),
    list(condition = show_concentration_q1, target = "cbc_q1_w_concentration"),
    list(condition = show_embarrassed_q1, target = "cbc_q1_w_embarrassed"),
    list(condition = show_unhappiness_q1, target = "cbc_q1_w_unhappiness"),
    list(condition = show_treated_q1, target = "cbc_q1_w_treated"),
    
    list(condition = show_tired_q2, target = "cbc_q2_w_tired"),
    list(condition = show_walking_q2, target = "cbc_q2_w_walking"),
    list(condition = show_sports_q2, target = "cbc_q2_w_sports"),
    list(condition = show_concentration_q2, target = "cbc_q2_w_concentration"),
    list(condition = show_embarrassed_q2, target = "cbc_q2_w_embarrassed"),
    list(condition = show_unhappiness_q2, target = "cbc_q2_w_unhappiness"),
    list(condition = show_treated_q2, target = "cbc_q2_w_treated"),
    
    list(condition = show_tired_q3, target = "cbc_q3_w_tired"),
    list(condition = show_walking_q3, target = "cbc_q3_w_walking"),
    list(condition = show_sports_q3, target = "cbc_q3_w_sports"),
    list(condition = show_concentration_q3, target = "cbc_q3_w_concentration"),
    list(condition = show_embarrassed_q3, target = "cbc_q3_w_embarrassed"),
    list(condition = show_unhappiness_q3, target = "cbc_q3_w_unhappiness"),
    list(condition = show_treated_q3, target = "cbc_q3_w_treated"),
    
    list(condition = show_tired_q4, target = "cbc_q4_w_tired"),
    list(condition = show_walking_q4, target = "cbc_q4_w_walking"),
    list(condition = show_sports_q4, target = "cbc_q4_w_sports"),
    list(condition = show_concentration_q4, target = "cbc_q4_w_concentration"),
    list(condition = show_embarrassed_q4, target = "cbc_q4_w_embarrassed"),
    list(condition = show_unhappiness_q4, target = "cbc_q4_w_unhappiness"),
    list(condition = show_treated_q4, target = "cbc_q4_w_treated"),
    
    list(condition = show_tired_q5, target = "cbc_q5_w_tired"),
    list(condition = show_walking_q5, target = "cbc_q5_w_walking"),
    list(condition = show_sports_q5, target = "cbc_q5_w_sports"),
    list(condition = show_concentration_q5, target = "cbc_q5_w_concentration"),
    list(condition = show_embarrassed_q5, target = "cbc_q5_w_embarrassed"),
    list(condition = show_unhappiness_q5, target = "cbc_q5_w_unhappiness"),
    list(condition = show_treated_q5, target = "cbc_q5_w_treated"),
    
    list(condition = show_tired_q6, target = "cbc_q6_w_tired"),
    list(condition = show_walking_q6, target = "cbc_q6_w_walking"),
    list(condition = show_sports_q6, target = "cbc_q6_w_sports"),
    list(condition = show_concentration_q6, target = "cbc_q6_w_concentration"),
    list(condition = show_embarrassed_q6, target = "cbc_q6_w_embarrassed"),
    list(condition = show_unhappiness_q6, target = "cbc_q6_w_unhappiness"),
    list(condition = show_treated_q6, target = "cbc_q6_w_treated"),
    
    list(condition = show_tired_q7, target = "cbc_q7_w_tired"),
    list(condition = show_walking_q7, target = "cbc_q7_w_walking"),
    list(condition = show_sports_q7, target = "cbc_q7_w_sports"),
    list(condition = show_concentration_q7, target = "cbc_q7_w_concentration"),
    list(condition = show_embarrassed_q7, target = "cbc_q7_w_embarrassed"),
    list(condition = show_unhappiness_q7, target = "cbc_q7_w_unhappiness"),
    list(condition = show_treated_q7, target = "cbc_q7_w_treated"),
    
    list(condition = show_tired_q8, target = "cbc_q8_w_tired"),
    list(condition = show_walking_q8, target = "cbc_q8_w_walking"),
    list(condition = show_sports_q8, target = "cbc_q8_w_sports"),
    list(condition = show_concentration_q8, target = "cbc_q8_w_concentration"),
    list(condition = show_embarrassed_q8, target = "cbc_q8_w_embarrassed"),
    list(condition = show_unhappiness_q8, target = "cbc_q8_w_unhappiness"),
    list(condition = show_treated_q8, target = "cbc_q8_w_treated")
    
  ),
  all_questions_required = TRUE
)
