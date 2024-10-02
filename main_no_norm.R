# libraries:

library(boot) # for cross validation
library(corrplot)
library(car)
library(pROC)

# import dataset

df <- read.csv('DatasetR_eng_fin.csv', header=TRUE, sep=';', 
               fileEncoding="UTF-8")

anyNA(df)

# create a new column that identifies matches from the "winner - 1" and "control - 0" groups
df$Group <- ifelse(grepl("Ca", df$ID), "1",
                   ifelse(grepl("Co", df$ID), "0", NA))
df$Group <- as.integer(df$Group)

# let's see what data types we have in the columns:
str(df)

# we notice that percentages variables are stored as characters. we would like them to be
# represented as numerical [0,1] 
df$BallPoss <- as.numeric(sub(",", ".", sub("%", "", df$BallPoss))) # / 100
df$ShotsPrec <- as.numeric(sub(",", ".", sub("%", "", df$ShotsPrec))) # / 100
df$PassPrec <- as.numeric(sub(",", ".", sub("%", "", df$PassPrec))) # / 100
df$TacklesWRatio <- as.numeric(sub(",", ".", sub("%", "", df$TacklesWRatio))) # / 100
df$AirDuelW <- as.numeric(sub(",", ".", sub("%", "", df$AirDuelW))) # / 100
df$DribWRatio <- as.numeric(sub(",", ".", sub("%", "", df$DribWRatio))) # / 100

# the Knowledge column is a chr as well, let's handle it
df$Knowledge <- as.numeric(sub(",", ".", df$Knowledge))

# columns ID and match are not useful for our model, let's remove them
df$ID <- NULL
df$Match <- NULL
# we notice that the "result" column is redundant wrt goals made and taken, we remove it
df$Result <- NULL

# Let's do some label encoding. We want in FieldAdvantage column, to show home matches as 1 and
# Away matches as 0
df$FieldAdvantage <- ifelse(df$FieldAdvantage == "H", 1, 
                            ifelse(df$FieldAdvantage == "A", 0, NA))
# we want losses as -1, wins as 1 and draws as 0 in OutcomeWL column. This column can be redundant
# wrt to Points won. We will think about it later
df$OutcomeWL <- ifelse(df$OutcomeWL == "W", 1, 
                       ifelse(df$OutcomeWL == "D", 0, 
                              ifelse(df$OutcomeWL == "L", -1, NA)))

# Double check every column is now numerical 
str(df)

summary(df)

# check if the target variable is balanced
prop.table(table(df$OutcomeWL))

# we decide to consider just "won vs non-won" outcomes, since we are interested in what
# makes a team win, rather that in what makes a team win, lose or draw
df$OutcomeWL[df$OutcomeWL == -1] <- 0
prop.table(table(df$OutcomeWL))

# qqplots for non target features

par(mar=c(1,1,1,1))
par(mfrow=c(5, 4))
columns <- c("BallPoss", "Shots", "ShotsOnT", "ShotsPrec", 
             "PassAtt", "PassSucc", "PassPrec", "RosterQuality", "Knowledge", 
             "YellowC", "RedC", "FoulsC", "FoulsT", "TacklesW", "TacklesAtt", "TacklesWRatio", 
             "AirDuelW", "DribW", "DribAtt", "DribWRatio")

# Loop through each column name and create the QQ plot
for (col in columns) {
  # Create QQ plot with title, axis labels, and correct y-axis scale
  qqnorm(df[[col]], main='')
  qqline(df[[col]], col = "red")
  title <- paste(col)
  text(x = par("usr")[1] + 0.05 * diff(par("usr")[1:2]), 
       y = par("usr")[4] - 0.1 * diff(par("usr")[3:4]), 
       labels = title, pos = 4, cex = 1.2, font = 2)
}

# we are satisfied with most of the features, but Roster quality and knowledge require
# some manipulation in order to be used properly

# create a new df with normalized values
df_norm <- df

# function to visualize intervals in roster quality
clean_bin <- function(bin) {
  # Extract numbers using regular expression
  numbers <- gsub("[^0-9e+.-]", "", unlist(strsplit(bin, ",")))
  # Convert to numeric and format them
  formatted_numbers <- format(as.numeric(numbers), big.mark = ",", scientific = FALSE)
  # Combine back into a readable range
  cleaned_bin <- paste0("(", formatted_numbers[1], ", ", formatted_numbers[2], ")")
  return(cleaned_bin)
}

# binning of roster quality feature
df_norm$RosterQuality <- cut(df$RosterQuality, breaks = 10)  # Adjust the number of bins as needed
levels.roster <- levels(df_norm$RosterQuality)

# Apply the cleaning function to all bins
levels.roster <- sapply(levels.roster, clean_bin)
levels.roster

# make it numeric and divide by 10 to have 0-1 range
df_norm$RosterQuality <- as.numeric(df_norm$RosterQuality)


par(mar=c(1,1,1,1))

par(mfrow=c(1, 2))
hist(df_norm$RosterQuality, 
     main = "Roster Quality", 
     xlab = "Values", 
     ylab = "Frequency", 
     col = "blue", 
     border = "black",
     probability = TRUE)

lines(density(df_norm$RosterQuality), 
      col = "red", 
      lwd = 2)  # Adjust line width

qqnorm(df_norm$RosterQuality, main='')

qqline(df_norm$RosterQuality, col = "red")



# binning of Knowledge feature
df_norm$Knowledge <- cut(df$Knowledge, breaks = 10)  # Adjust the number of bins as needed
levels.Knowledge <- levels(df_norm$Knowledge)

# visualize levels of knowledge
levels.Knowledge

# make it numeric and divide by 10 to have 0-1 range
df_norm$Knowledge <- as.numeric(df_norm$Knowledge)

par(mfrow=c(1, 2))
hist(df_norm$Knowledge, 
     main = "Knowledge", 
     xlab = "Values", 
     ylab = "Frequency", 
     col = "blue", 
     border = "black",
     probability = TRUE)

lines(density(df_norm$Knowledge), 
      col = "red", 
      lwd = 2)  # Adjust line width

qqnorm(df_norm$Knowledge, main='')
qqline(df_norm$Knowledge, col = "red")


# by testing the model on both non normalized data and normalized data, the result is that 
# the model that uses un normalized data performs best. we decided to not normalize the data 

# put all data in same format [0,1]
# min_max_normalize <- function(x) {
#   return((x - min(x)) / (max(x) - min(x)))
# }

# columns_to_normalize <- c("Shots", "ShotsOnT","PassAtt", "PassSucc", "YellowC", "RedC",
#                          "FoulsC", "FoulsT", "DribW", "DribAtt", "TacklesW", "TacklesAtt")

# df_norm[columns_to_normalize] <- lapply(df_norm[columns_to_normalize], min_max_normalize)

# ------------------------------------------------------ # 

# BOXPLOTS

par(mar=c(1, 2.5, 1, 5) + 0.1)
par(mfrow=c(5, 4))

# Generate box plots for each variable
for (col in columns) {
  boxplot(df_norm[[col]] ~ df$Group, 
          df_norm,
          names = c("Group 0", "Group 1"),
          main = paste(col),
          col = c("lightblue", "#FF0000")
  )
}

# explore correlation

selected_corr <- cor(df_norm)
par(mar=c(1,1,1,1))

# Collinearity matrix
corrplot(selected_corr,
         method = "square",
         order = "hclust",
         diag = FALSE,
         tl.cex = 0.8,
         number.cex = 0.65,
         tl.col = "black")

t_test_results <- list()

columns_to_test <- c("BallPoss", "Shots", "ShotsOnT", "ShotsPrec", 
                     "PassAtt", "PassSucc", "PassPrec","YellowC", "RedC", 
                     "FoulsC", "FoulsT", "TacklesW", "TacklesAtt", "TacklesWRatio", 
                     "AirDuelW", "DribW", "DribAtt", "DribWRatio")

# Loop through each column and perform t-test
for (col in columns_to_test) {
  t_test_result <- t.test(df_norm[[col]] ~ df_norm$Group)
  t_test_results[[col]] <- t_test_result
}

# Print the t-test results
t_test_results

# If you want to save specific elements (e.g., p-values) in a vector
p_values <- sapply(t_test_results, function(x) x$p.value)
p_values


# to explore the difference in winning teams and average teams let's split the df:
df_1_norm <- df_norm[df_norm$Group == 1, ]
df_1_norm$Group <- NULL
df_0_norm <- df_norm[df_norm$Group == 0, ]
df_0_norm$Group <- NULL


calc_stats <- function(df) {
  stats <- data.frame(Variable = names(df), Mean = sapply(df, mean))
  return(stats)
}

stats_0 <- calc_stats(df_0_norm)
stats_1 <- calc_stats(df_1_norm)

summary_df <- data.frame(Variable = stats_1$Variable, Mean_1 = stats_1$Mean,
                         Mean_0 = stats_0$Mean)

summary_df$Mean_Difference <- summary_df$Mean_1 - summary_df$Mean_0

summary_df <- summary_df[, c("Variable", "Mean_1", "Mean_0", "Mean_Difference")]


par(mar=c(1, 2.5, 1, 1) + 0.1)
par(mfrow=c(5, 4))

# Let's see, inside each group, what are the difference between successful matches
# and unsuccessful
# Generate box plots for each variable
for (col in columns) {
  boxplot(df_1_norm[[col]] ~ df_1_norm$OutcomeWL, 
          df_1_norm,
          names = c("Loss/Draw", "Win"),
          main = paste(col),
          col = c("lightblue", "#FF0000")
  )
}

for (col in columns) {
  boxplot(df_0_norm[[col]] ~ df_0_norm$OutcomeWL, 
          df_0_norm,
          names = c("Loss/Draw", "Win"),
          main = paste(col),
          col = c("lightblue", "#FF0000")
  )
}

t_test_results_1 <- list()

# Loop through each column and perform t-test
for (col in columns_to_test) {
  t_test_result_1 <- t.test(df_1_norm[[col]] ~ df_1_norm$OutcomeWL)
  t_test_results_1[[col]] <- t_test_result_1
}

# Print the t-test results
t_test_results_1

# If you want to save specific elements (e.g., p-values) in a vector
p_values_1 <- sapply(t_test_results_1, function(x) x$p.value)
p_values_1

t_test_results_0 <- list()

# Loop through each column and perform t-test
for (col in columns_to_test) {
  t_test_result_0 <- t.test(df_0_norm[[col]] ~ df_0_norm$OutcomeWL)
  t_test_results_0[[col]] <- t_test_result_0
}

# Print the t-test results
t_test_results_0

# If you want to save specific elements (e.g., p-values) in a vector
p_values_0 <- sapply(t_test_results_0, function(x) x$p.value)
p_values_0

# interesting. from these plots it looks like bat teams play as bad teams both in
# winning and losing matches and good teams play like good teams as well, both in
# winning and losing. The difference seems to be on shots on target/shots precision

summary_df <- data.frame(
  Variable = c("OutcomeWL", "FieldAdvantage", "BallPoss", "Shots", "ShotsOnT",
               "PassAtt", "PassSucc", "TacklesW", "TacklesAtt", "AirDuelW", "DribW"),
  Mean = sapply(df[, c("OutcomeWL", "FieldAdvantage", "BallPoss", "Shots", "ShotsOnT",
                       "PassAtt", "PassSucc", "TacklesW", "TacklesAtt", "AirDuelW", "DribW")], mean),
  Median = sapply(df[, c("OutcomeWL", "FieldAdvantage", "BallPoss", "Shots", "ShotsOnT",
                         "PassAtt", "PassSucc", "TacklesW", "TacklesAtt", "AirDuelW", "DribW")], median),
  SD = sapply(df[, c("OutcomeWL", "FieldAdvantage", "BallPoss", "Shots", "ShotsOnT",
                     "PassAtt", "PassSucc", "TacklesW", "TacklesAtt", "AirDuelW", "DribW")], sd),
  Min = sapply(df[, c("OutcomeWL", "FieldAdvantage", "BallPoss", "Shots", "ShotsOnT",
                      "PassAtt", "PassSucc", "TacklesW", "TacklesAtt", "AirDuelW", "DribW")], min),
  Max = sapply(df[, c("OutcomeWL", "FieldAdvantage", "BallPoss", "Shots", "ShotsOnT",
                      "PassAtt", "PassSucc", "TacklesW", "TacklesAtt", "AirDuelW", "DribW")], max)
)
summary_df

# ----------------------------------------------------- #

# FEATURE SELECTION 

# from the correlation matrix we already know there are some highly correlated 
# features that we can probably exclude form the model. We start with the full model
# and apply the stepwise backward elimination strategy. 

# remove target variables. df_norm.F is our full dataset without target variables
# that we are not interested in at the moment
df_norm.F <- df_norm
df_norm.F$PointsWon <- NULL
df_norm.F$GoalsMade <- NULL
df_norm.F$GoalsTaken <- NULL
df_norm.F$Group <- NULL 


# univariate (da vedere se utilizzare questi risultati)

univariate_results <- data.frame(variable = character(),
                                 p_value = numeric(),
                                 AIC = numeric(),
                                 BIC = numeric())
columns <- c("FieldAdvantage","BallPoss", "Shots", "ShotsOnT", "ShotsPrec", 
             "PassAtt", "PassSucc", "PassPrec", "RosterQuality", "Knowledge", 
             "YellowC", "RedC", "FoulsC", "FoulsT", "TacklesW", "TacklesAtt", "TacklesWRatio", 
             "AirDuelW", "DribW", "DribAtt", "DribWRatio")


for (col in columns) {
  glm_u <- glm(OutcomeWL ~ df_norm.F[[col]], data = df_norm.F, family = binomial)
  model_summary <- summary(glm_u)
  pvalue <- model_summary$coefficients[2, "Pr(>|z|)"][1]
  pvalue <- format(pvalue, scientific = FALSE)
  
  new_row <- data.frame(variable = col,
                        p_value = pvalue,
                        AIC = AIC(glm_u),
                        BIC = BIC(glm_u),
                        stringsAsFactors = FALSE)
  univariate_results <- rbind(univariate_results, new_row)
  
}

univariate_results

# ------------------------------------------------------------------------
# FEATURE SELECTION 
# we use feature selection techniques to asses which are the most important factors
# that define a winning performance

results <- data.frame(df = character(),
                      method = character(),
                      accuracy = numeric(),
                      AIC = numeric(),
                      BIC = numeric())

# full model 
glm_F <- glm(OutcomeWL ~ FieldAdvantage + BallPoss + Shots + ShotsOnT + ShotsPrec + PassAtt + PassSucc 
             + PassPrec + RosterQuality + Knowledge + YellowC + RedC + FoulsC + FoulsT
             + TacklesW + TacklesAtt + TacklesWRatio + AirDuelW + DribW + DribAtt + DribWRatio,
             data = df_norm.F,
             family = binomial)
summary(glm_F)
aic_value_full <- AIC(glm_F)
bic_value_full <- BIC(glm_F)


# backward
glm_bkw <- step(glm_F, direction = "backward")

summary(glm_bkw)
aic_value_bwd <- AIC(glm_bkw)
bic_value_bwd <- BIC(glm_bkw)


# forward
glm_null <- glm(OutcomeWL ~ 1, data = df_norm.F, family = binomial)

glm_fwd <- step(glm_null, scope = list(lower = glm_null, upper = glm_F), direction = "forward")
summary(glm_fwd)
aic_value_fwd <- AIC(glm_fwd)
bic_value_fwd <- BIC(glm_fwd)

# leave one out cv, comparison of errors 

cv_err_full <- cv.glm(df_norm.F, glm_F)
cv_err_full$delta

cv_err_bwd <- cv.glm(df_norm.F, glm_bkw)
cv_err_bwd$delta

cv_err_fwd <- cv.glm(df_norm.F, glm_fwd)
cv_err_fwd$delta

cv_err <- c(cv_err_full$delta[1], cv_err_bwd$delta[1], cv_err_fwd$delta[1])

new_row <- data.frame(df = "Full", method = "Full model", accuracy = cv_err_full$delta[1], 
                      AIC = AIC(glm_F), BIC = BIC(glm_F), stringsAsFactors = FALSE)
results <- rbind(results, new_row)
new_row <- data.frame(df = "Full", method = "Backward", accuracy = cv_err_bwd$delta[1], 
                      AIC = AIC(glm_bkw), BIC = BIC(glm_bkw), stringsAsFactors = FALSE)
results <- rbind(results, new_row)
new_row <- data.frame(df = "Full", method = "Forward", accuracy = cv_err_fwd$delta[1], 
                      AIC = AIC(glm_fwd), BIC = BIC(glm_fwd), stringsAsFactors = FALSE)
results <- rbind(results, new_row)
results

# our best model is the fwd selected one. with an error of 0.1548526. We have our result
# for our most important question! We can look at the 8 features extracted by the model
# and consider these as the best predictors for a win 

# we can try to repeat the process with the two separated df to see if the winning conditions
# change between the groups 

# prepare DF
df_1_norm.F <- df_1_norm
df_1_norm.F$PointsWon <- NULL
df_1_norm.F$GoalsMade <- NULL
df_1_norm.F$GoalsTaken <- NULL
df_1_norm.F$Group <- NULL 

df_0_norm.F <- df_0_norm
df_0_norm.F$PointsWon <- NULL
df_0_norm.F$GoalsMade <- NULL
df_0_norm.F$GoalsTaken <- NULL
df_0_norm.F$Group <- NULL 

# start with winning group: 

glm_F_W <- glm(OutcomeWL ~ FieldAdvantage + BallPoss + Shots + ShotsOnT + ShotsPrec + PassAtt + PassSucc 
               + PassPrec + RosterQuality + Knowledge + YellowC + RedC + FoulsC + FoulsT
               + TacklesW + TacklesAtt + TacklesWRatio + AirDuelW + DribW + DribAtt + DribWRatio,
               data = df_1_norm.F,
               family = binomial)
summary(glm_F_W)
aic_value_full_W <- AIC(glm_F_W)
bic_value_full_W <- BIC(glm_F_W)

# backward
glm_bkw_W <- step(glm_F_W, direction = "backward")

summary(glm_bkw_W)
aic_value_bwd_W <- AIC(glm_bkw_W)
bic_value_bwd_W <- BIC(glm_bkw_W)

# forward
glm_null_W <- glm(OutcomeWL ~ 1, data = df_1_norm.F, family = binomial)

glm_fwd_W <- step(glm_null_W, scope = list(lower = glm_null_W, upper = glm_F_W), direction = "forward")
summary(glm_fwd_W)
aic_value_fwd <- AIC(glm_fwd_W)
bic_value_fwd <- BIC(glm_fwd_W)

# leave one out cv, comparison of errors 

cv_err_full_W <- cv.glm(df_1_norm.F, glm_F_W)
cv_err_full_W$delta

cv_err_bwd_W <- cv.glm(df_1_norm.F, glm_bkw_W)
cv_err_bwd_W$delta

cv_err_fwd_W <- cv.glm(df_1_norm.F, glm_fwd_W)
cv_err_fwd_W$delta

cv_err_W <- c(cv_err_full_W$delta[1], cv_err_bwd_W$delta[1], cv_err_fwd_W$delta[1])

new_row <- data.frame(df = "Group 1", method = "Full model", accuracy = cv_err_full_W$delta[1], 
                      AIC = AIC(glm_F_W), BIC = BIC(glm_F_W), stringsAsFactors = FALSE)
results <- rbind(results, new_row)
new_row <- data.frame(df = "Group 1", method = "Backward", accuracy = cv_err_bwd_W$delta[1], 
                      AIC = AIC(glm_bkw_W), BIC = BIC(glm_bkw_W), stringsAsFactors = FALSE)
results <- rbind(results, new_row)
new_row <- data.frame(df = "Group 1", method = "Forward", accuracy = cv_err_fwd_W$delta[1], 
                      AIC = AIC(glm_fwd_W), BIC = BIC(glm_fwd_W), stringsAsFactors = FALSE)
results <- rbind(results, new_row)

# and finally do the same with control group

glm_F_C <- glm(OutcomeWL ~ FieldAdvantage + BallPoss + Shots + ShotsOnT + ShotsPrec + PassAtt + PassSucc 
               + PassPrec + RosterQuality + Knowledge + YellowC + RedC + FoulsC + FoulsT
               + TacklesW + TacklesAtt + TacklesWRatio + AirDuelW + DribW + DribAtt + DribWRatio,
               data = df_0_norm.F,
               family = binomial)
summary(glm_F_C)
aic_value_full_C <- AIC(glm_F_C)
bic_value_full_C <- BIC(glm_F_C)

# backward
glm_bkw_C <- step(glm_F_C, direction = "backward")

summary(glm_bkw_C)
aic_value_bwd_C <- AIC(glm_bkw_C)
bic_value_bwd_C <- BIC(glm_bkw_C)

# forward
glm_null_C <- glm(OutcomeWL ~ 1, data = df_0_norm.F, family = binomial)

glm_fwd_C <- step(glm_null_C, scope = list(lower = glm_null_C, upper = glm_F_C), direction = "forward")
summary(glm_fwd_C)
aic_value_fwd <- AIC(glm_fwd_C)
bic_value_fwd <- BIC(glm_fwd_C)

# leave one out cv, comparison of errors 

cv_err_full_C <- cv.glm(df_0_norm.F, glm_F_C)
cv_err_full_C$delta

cv_err_bwd_C <- cv.glm(df_0_norm.F, glm_bkw_C)
cv_err_bwd_C$delta

cv_err_fwd_C <- cv.glm(df_0_norm.F, glm_fwd_C)
cv_err_fwd_C$delta

cv_err_C <- c(cv_err_full_C$delta[1], cv_err_bwd_C$delta[1], cv_err_fwd_C$delta[1])

new_row <- data.frame(df = "Group 0", method = "Full model", accuracy = cv_err_full_C$delta[1], 
                      AIC = AIC(glm_F_C), BIC = BIC(glm_F_C), stringsAsFactors = FALSE)
results <- rbind(results, new_row)
new_row <- data.frame(df = "Group 0", method = "Backward", accuracy = cv_err_bwd_C$delta[1], 
                      AIC = AIC(glm_bkw_C), BIC = BIC(glm_bkw_C), stringsAsFactors = FALSE)
results <- rbind(results, new_row)
new_row <- data.frame(df = "Group 0", method = "Forward", accuracy = cv_err_fwd_C$delta[1], 
                      AIC = AIC(glm_fwd_C), BIC = BIC(glm_fwd_C), stringsAsFactors = FALSE)
results <- rbind(results, new_row)

results

# We decide to use the model with lowest BIC in each case, in this way we 
# penalize models with more variables. In all three cases we will use the forward
# model

# Let's check some diagnostics to see how good is our model: 

vif(glm_fwd)
vif(glm_fwd_W)
vif(glm_fwd_C)

pred <- predict(glm_fwd, type = "response")
pred_1 <- predict(glm_fwd_W, type = "response")
pred_2 <- predict(glm_fwd_C, type = "response")

par(mfrow=c(1, 3))

roc_obj_F <- roc(df_norm.F$OutcomeWL, pred)
roc_obj_1 <- roc(df_1_norm.F$OutcomeWL, pred_1)
roc_obj_0 <- roc(df_0_norm.F$OutcomeWL, pred_2)

par(mar = c(4, 4, 2, 2) + 0.1) # Default margins for a clear plot
plot(roc_obj_F, print.auc=TRUE)
plot(roc_obj_1, print.auc=TRUE)
plot(roc_obj_0, print.auc=TRUE)

# full df
coefficients_F <- summary(glm_fwd)$coefficients
odds_ratios_F <- exp(coefficients_F[, "Estimate"])
odds_ratios_F <- format(odds_ratios_F, scientific = FALSE, digits = 4)
odds_ratios_F

# df 1
coefficients_1 <- summary(glm_fwd_W)$coefficients
odds_ratios_1 <- exp(coefficients_1[, "Estimate"])
odds_ratios_1 <- format(odds_ratios_1, scientific = FALSE, digits = 4)
odds_ratios_1

# df 0 
coefficients_0 <- summary(glm_fwd_C)$coefficients
odds_ratios_0 <- exp(coefficients_0[, "Estimate"])
odds_ratios_0 <- format(odds_ratios_0, scientific = FALSE, digits = 4)
odds_ratios_0

# ----------------------------------------------------------------- 

# We can now start exploring if we can predict the number of goals scored
df_norm_goals <- df_norm
df_norm_goals$OutcomeWL <- NULL
df_norm_goals$PointsWon <- NULL

df_norm_goals_1 <- df_norm_goals[df_norm_goals$Group == 1, ]
df_norm_goals_0 <- df_norm_goals[df_norm_goals$Group == 0, ]

df_norm_goals$Group <- NULL
df_norm_goals_1$Group <- NULL
df_norm_goals_0$Group <- NULL

full.mod <- lm(GoalsMade~.-GoalsTaken, data=df_norm_goals)
summary(full.mod)

#------------------------------------------------------------------------------------------------------------
#--------------------------------- FEATURE SELECTION with LINEAR REGRESSION ---------------------------------
# In order to rightly perform linear regression we'll now use Goal Scored as target variable (lr operates on continuous variable)



target_var_index <- which(colnames(df_norm_goals) == "GoalsMade")


correlations <- cor(df_norm_goals[, -target_var_index])  # Exclude the target variable column

abs_correlations <- abs(correlations)  # Absolute values for easier interpretation

# Identify features with strong correlations
threshold <- 0.5
important_features <- names(abs_correlations[, 1] > threshold)

# Linear Regression Model Fitting and K-Fold Cross-Validation

# Select features based on correlation
selected_features <- important_features

k <- 10

set.seed(123)

folds <- sample(1:k, nrow(df_norm_goals), replace=TRUE)

cv_errors <- matrix(NA, k, length(selected_features))
colnames(cv_errors) <- selected_features

# Apply the K-fold cross-validation loop
for (j in 1:k) {
  training_data <- df_norm_goals[folds != j, ]
  
  test_data <- df_norm_goals[folds == j, ]
  
  for (i in 1:length(selected_features)) {
    
    current_feature_indices <- which(names(df_norm_goals) %in% selected_features[1:i])

    model <- lm(formula = paste("GoalsMade ~", paste(names(df_norm_goals)[current_feature_indices], collapse = "+"), sep = ""), data = training_data)
    
    predictions <- predict(model, newdata = test_data[, current_feature_indices, drop=FALSE])
    
    mse <- mean((test_data$GoalsMade - predictions)^2)
    
    cv_errors[j, i] <- mse
  }
}

mean_cv_errors <- apply(cv_errors, 2, mean)

best_feature_set <- colnames(cv_errors)[which.min(mean_cv_errors)]

final_model <- lm(formula = paste("GoalsMade ~", paste(best_feature_set, collapse = "+"), sep = ""), data = df_norm_goals)

cat("Average RMSE across folds (best feature set):", round(mean(cv_errors[, which.min(mean_cv_errors)]), digits = 4))
cat("\nBest feature set:", paste(best_feature_set, collapse = ", "))

##### Results are pretty useless (RedCard as most important feature on GoalsMade target)

######## Let's now try with multiple linear regression
###### first step: feature selection
##### univariate analysis


# Univariate analysis for predicting goals scored
# Corrected data frame name (assuming your data is in df_norm_goals)
univariate_results_goalsmade <- data.frame(variable = character(),
                                           p_value = numeric())

columns <- c("FieldAdvantage", "BallPoss", "Shots", "ShotsOnT", "ShotsPrec",
             "PassAtt", "PassSucc", "PassPrec", "RosterQuality", "Knowledge",
             "YellowC", "RedC", "FoulsC", "FoulsT", "TacklesW", "TacklesAtt",
             "TacklesWRatio", "AirDuelW", "DribW", "DribAtt", "DribWRatio")


initial_row <- data.frame(variable = character(),
                          p_value = numeric(),
                          stringsAsFactors = FALSE)

univariate_results_goalsmade <- initial_row


for (col in columns) {
  model <- lm(GoalsMade ~ df_norm_goals[[col]], data = df_norm_goals)
  
  # Extract p-value from summary
  summary_model <- summary(model)
  pvalue <- summary_model$coefficients[2, "Pr(>|t|)"][1]
  pvalue <- format(pvalue, scientific = FALSE)
  
  new_row <- data.frame(variable = col,
                        p_value = pvalue,
                        stringsAsFactors = FALSE)
  
  univariate_results_goalsmade <- rbind(univariate_results_goalsmade, new_row)
}

print(univariate_results_goalsmade)

##################################################################
################ backward and forward elimination ################
##################################################################
results <- data.frame(df = character(),
                      method = character(),
                      R2 = numeric(), 
                      adj_R2 = numeric(),  # Adjusted R-squared for multiple linear regression
                      AIC = numeric(),
                      BIC = numeric(),
                      kfold_mse = numeric())

# Full model
full_features <- c("FieldAdvantage", "BallPoss", "Shots", "ShotsOnT", "ShotsPrec",
                   "PassAtt", "PassSucc", "PassPrec", "RosterQuality", "Knowledge",
                   "YellowC", "RedC", "FoulsC", "FoulsT", "TacklesW", "TacklesAtt",
                   "TacklesWRatio", "AirDuelW", "DribW", "DribAtt", "DribWRatio")

lm_full <- lm(GoalsMade ~ FieldAdvantage + BallPoss + Shots + ShotsOnT + ShotsPrec + PassAtt + PassSucc 
              + PassPrec + RosterQuality + Knowledge + YellowC + RedC + FoulsC + FoulsT
              + TacklesW + TacklesAtt + TacklesWRatio + AirDuelW + DribW + DribAtt + DribWRatio, data = df_norm_goals)

summary(lm_full)
aic_value_full <- AIC(lm_full)
bic_value_full <- BIC(lm_full)

# Backward selection
lm_bkw <- step(lm_full, direction = "backward")

summary(lm_bkw)
aic_value_bwd <- AIC(lm_bkw)
bic_value_bwd <- BIC(lm_bkw)

# Forward selection
lm_null <- lm(GoalsMade ~ 1, data = df_norm_goals)

lm_fwd <- step(lm_null, scope = list(lower = lm_null, upper = lm_full), direction = "forward")
summary(lm_fwd)
aic_value_fwd <- AIC(lm_fwd)
bic_value_fwd <- BIC(lm_fwd)



# k-fold cv, comparison of errors 
k <- 10

# MSE
mse <- function(y_true, y_pred) {
  mean((y_true - y_pred)^2)
}


# Loop through models: Full, Backward, Forward
models <- c("Full Model", "Backward", "Forward")
for (model_name in models) {
  
  # Define model formula based on selection step
  if (model_name == "Full Model") {
    model_formula <- GoalsMade ~ .  # All features
  } else if (model_name == "Backward") {
    model_formula <- lm_bkw$call[[2]]  # Formula from backward selection
  } else {
    model_formula <- lm_fwd$call[[2]]  # Formula from forward selection
  }
  
  # Initialize variables for k-fold CV
  all_errors <- c()
  
  # Loop for k-fold cross-validation
  for (j in 1:k) {
    # Split data into folds
    test_size <- nrow(df_norm_goals) / k
    test_indices <- sample(1:nrow(df_norm_goals), size = test_size, replace = FALSE)
    training_data <- df_norm_goals[-test_indices, ]
    test_data <- df_norm_goals[test_indices, ]
    
    # Train model on training data
    model_lm <- lm(model_formula, data = training_data)
    
    # Predict on test data
    predictions <- predict(model_lm, newdata = test_data)
    
    # Calculate error
    error <- mse(test_data$GoalsMade, predictions)
    all_errors <- c(all_errors, error)
  }
  
  # Average error across all folds
  average_error <- mean(all_errors)
  
  # Evaluate and store results on entire data
  if (model_name == "Full Model") {
    lm_fit <- lm(GoalsMade ~ ., data = df_norm_goals)
  } else if (model_name == "Backward") {
    lm_fit <- lm(model_formula, data = df_norm_goals)  # Use backward selection formula
  } else {
    lm_fit <- lm(model_formula, data = df_norm_goals)  # Use forward selection formula
  }
  summary(lm_fit)
  aic_value <- AIC(lm_fit)
  bic_value <- BIC(lm_fit)
  
  results <- rbind(results,
                   data.frame(df = model_name, method = "lm", R2 = summary(lm_fit)$r.squared,
                              adj_R2 = summary(lm_fit)$adj.r.squared, AIC = aic_value, BIC = bic_value,
                              kfold_mse = average_error))
}
print(results)

# backward is the best. plot results
df_norm_goals_pred <- df_norm_goals
df_norm_goals_pred$predicted_goals <- predict(lm_bkw, df_norm_goals_pred)

plot(df$FieldAdvantage, df$goals, main = "Regression Line with Actual Goals",
     xlab = "FieldAdvantage", ylab = "Goals Scored", pch = 19, col = "blue")

# Add the regression line (fitted values)
points(df$FieldAdvantage, df$fitted_values, pch = 19, col = "red")
lines(sort(df$FieldAdvantage), fitted(model)[order(df$FieldAdvantage)], col = "red", lwd = 2)


################ based on the results obtained we can see that:
################ Highest Fit -> Highest R2 -> Full Model (but potentially overfitting) 
################ Balance (Fit & Complexity) -> Balance bet. AIC/BIC (both low) -> Backward Selection
################ Best on Unseen Data (Prediction) -> Lowest k-fold MSE -> Forward Selection

# Considering the trade-offs, a good starting point could be the Backward Selection model. 
# It achieves a reasonable fit with a simpler structure and performs well on k-fold CV.
# However, we might also want to consider the Forward Selection model for prediction performance as priority (I chose Backward at the end)

df_1_norm_goals <- df_1_norm
df_1_norm_goals$PointsWon <- NULL
df_1_norm_goals$OutcomeWL <- NULL
df_1_norm_goals$GoalsTaken <- NULL
df_1_norm_goals$Group <- NULL 

df_0_norm_goals <- df_0_norm
df_0_norm_goals$PointsWon <- NULL
df_0_norm_goals$OutcomeWL <- NULL
df_0_norm_goals$GoalsTaken <- NULL
df_0_norm_goals$Group <- NULL 

results <- data.frame(df = character(),
                      method = character(),
                      R2 = numeric(), 
                      adj_R2 = numeric(),
                      AIC = numeric(),
                      BIC = numeric(),
                      kfold_mse = numeric())
# winning group: 

lm_full_W <- lm(GoalsMade ~ FieldAdvantage + BallPoss + Shots + ShotsOnT + ShotsPrec + PassAtt + PassSucc 
              + PassPrec + RosterQuality + Knowledge + YellowC + RedC + FoulsC + FoulsT
              + TacklesW + TacklesAtt + TacklesWRatio + AirDuelW + DribW + DribAtt + DribWRatio, data = df_1_norm_goals)

summary(lm_full_W)
aic_value_full_W <- AIC(lm_full_W)
bic_value_full_W <- BIC(lm_full_W)

# Backward selection
lm_bkw_W <- step(lm_full_W, direction = "backward")

summary(lm_bkw_W)
aic_value_bwd <- AIC(lm_bkw_W)
bic_value_bwd <- BIC(lm_bkw_W)

# Forward selection
lm_null_W <- lm(GoalsMade ~ 1, data = df_1_norm_goals)

lm_fwd_W <- step(lm_null_W, scope = list(lower = lm_null_W, upper = lm_full_W), direction = "forward")
summary(lm_fwd_W)
aic_value_fwd <- AIC(lm_fwd_W)
bic_value_fwd <- BIC(lm_fwd_W)


# k-fold cross validation 

models <- c("Full Model", "Backward", "Forward")
for (model_name in models) {
  
  # Define model formula based on selection step
  if (model_name == "Full Model") {
    model_formula <- GoalsMade ~ .  # All features
  } else if (model_name == "Backward") {
    model_formula <- lm_bkw_W$call[[2]]  # Formula from backward selection
  } else {
    model_formula <- lm_fwd_W$call[[2]]  # Formula from forward selection
  }
  
  # Initialize variables for k-fold CV
  all_errors <- c()
  
  # Loop for k-fold cross-validation
  for (j in 1:k) {
    # Split data into folds
    test_size <- nrow(df_1_norm_goals) / k
    test_indices <- sample(1:nrow(df_1_norm_goals), size = test_size, replace = FALSE)
    training_data <- df_1_norm_goals[-test_indices, ]
    test_data <- df_1_norm_goals[test_indices, ]
    
    # Train model on training data
    model_lm <- lm(model_formula, data = training_data)
    
    # Predict on test data
    predictions <- predict(model_lm, newdata = test_data)
    
    # Calculate error
    error <- mse(test_data$GoalsMade, predictions)
    all_errors <- c(all_errors, error)
  }
  
  # Average error across all folds
  average_error <- mean(all_errors)
  
  if (model_name == "Full Model") {
    lm_fit <- lm(GoalsMade ~ ., data = df_1_norm_goals)
  } else if (model_name == "Backward") {
    lm_fit <- lm(model_formula, data = df_1_norm_goals)  # Use backward selection formula
  } else {
    lm_fit <- lm(model_formula, data = df_1_norm_goals)  # Use forward selection formula
  }
  summary(lm_fit)
  aic_value <- AIC(lm_fit)
  bic_value <- BIC(lm_fit)
  
  results <- rbind(results,
                   data.frame(df = model_name, method = "lm", R2 = summary(lm_fit)$r.squared,
                              adj_R2 = summary(lm_fit)$adj.r.squared, AIC = aic_value, BIC = bic_value,
                              kfold_mse = average_error))
}

print(results)

# control group

lm_full_C <- lm(GoalsMade ~ FieldAdvantage + BallPoss + Shots + ShotsOnT + ShotsPrec + PassAtt + PassSucc 
                + PassPrec + RosterQuality + Knowledge + YellowC + RedC + FoulsC + FoulsT
                + TacklesW + TacklesAtt + TacklesWRatio + AirDuelW + DribW + DribAtt + DribWRatio, data = df_0_norm_goals)

summary(lm_full_C)
aic_value_full_C <- AIC(lm_full_C)
bic_value_full_C <- BIC(lm_full_C)

# Backward selection
lm_bkw_C <- step(lm_full_C, direction = "backward")

summary(lm_bkw_C)
aic_value_bwd <- AIC(lm_bkw_C)
bic_value_bwd <- BIC(lm_bkw_C)

# Forward selection
lm_null_C <- lm(GoalsMade ~ 1, data = df_0_norm_goals)

lm_fwd_C <- step(lm_null_C, scope = list(lower = lm_null_C, upper = lm_full_C), direction = "forward")
summary(lm_fwd_C)
aic_value_fwd <- AIC(lm_fwd_C)
bic_value_fwd <- BIC(lm_fwd_C)


# k-fold cross validation 

models <- c("Full Model", "Backward", "Forward")
for (model_name in models) {
  
  # Define model formula based on selection step
  if (model_name == "Full Model") {
    model_formula <- GoalsMade ~ .  # All features
  } else if (model_name == "Backward") {
    model_formula <- lm_bkw_C$call[[2]]  # Formula from backward selection
  } else {
    model_formula <- lm_fwd_C$call[[2]]  # Formula from forward selection
  }
  
  # Initialize variables for k-fold CV
  all_errors <- c()
  
  # Loop for k-fold cross-validation
  for (j in 1:k) {
    # Split data into folds
    test_size <- nrow(df_0_norm_goals) / k
    test_indices <- sample(1:nrow(df_0_norm_goals), size = test_size, replace = FALSE)
    training_data <- df_0_norm_goals[-test_indices, ]
    test_data <- df_0_norm_goals[test_indices, ]
    
    # Train model on training data
    model_lm <- lm(model_formula, data = training_data)
    
    # Predict on test data
    predictions <- predict(model_lm, newdata = test_data)
    
    # Calculate error
    error <- mse(test_data$GoalsMade, predictions)
    all_errors <- c(all_errors, error)
  }
  
  # Average error across all folds
  average_error <- mean(all_errors)
  
  # Evaluate and store results on entire data
  if (model_name == "Full Model") {
    lm_fit <- lm(GoalsMade ~ ., data = df_1_norm_goals)
  } else if (model_name == "Backward") {
    lm_fit <- lm(model_formula, data = df_1_norm_goals)  # Use backward selection formula
  } else {
    lm_fit <- lm(model_formula, data = df_1_norm_goals)  # Use forward selection formula
  }
  summary(lm_fit)
  aic_value <- AIC(lm_fit)
  bic_value <- BIC(lm_fit)
  
  results <- rbind(results,
                   data.frame(df = model_name, method = "lm", R2 = summary(lm_fit)$r.squared,
                              adj_R2 = summary(lm_fit)$adj.r.squared, AIC = aic_value, BIC = bic_value,
                              kfold_mse = average_error))
}

results


# Calculate and format the odds ratios for each model

# Overall group (using the backward selection model)
coefficients_F <- summary(lm_bkw)$coefficients
odds_ratios_F <- exp(coefficients_F[, "Estimate"])
odds_ratios_F <- format(odds_ratios_F, scientific = FALSE, digits = 4)
print("Odds Ratios for backward (Overall Group):")
print(odds_ratios_F)

# Group 1 (using the forward selection model)
coefficients_1 <- summary(lm_fwd_W)$coefficients
odds_ratios_1 <- exp(coefficients_1[, "Estimate"])
odds_ratios_1 <- format(odds_ratios_1, scientific = FALSE, digits = 4)
print("Odds Ratios for forward Selection Model (Group 1):")
print(odds_ratios_1)

# Group 0 (using the forward selection model)
coefficients_0 <- summary(lm_fwd_C)$coefficients
odds_ratios_0 <- exp(coefficients_0[, "Estimate"])
odds_ratios_0 <- format(odds_ratios_0, scientific = FALSE, digits = 4)
print("Odds Ratios for Forward Selection Model (Group 0):")
print(odds_ratios_0)

#### Names below are wrong but actual data are ok

importance_backward_O <- as.numeric(odds_ratios_F[2:length(odds_ratios_F)])
ordered_indices <- order(importance_backward_O)
importance_backward_O <- importance_backward_O[ordered_indices]
names(importance_backward_O) <- names(odds_ratios_F)[ordered_indices + 1]
importance_backward_O

importance_backward_W <- as.numeric(odds_ratios_1[2:length(odds_ratios_1)])
ordered_indices <- order(importance_backward_W)
importance_backward_W <- importance_backward_W[ordered_indices]
names(importance_backward_W) <- names(odds_ratios_1)[ordered_indices + 1]
importance_backward_W

# Feature importance for Control Group (Forward Selection)
importance_backward_C <- as.numeric(odds_ratios_0[2:length(odds_ratios_0)])
ordered_indices <- order(importance_backward_C)
importance_backward_C <- importance_backward_C[ordered_indices]
names(importance_backward_C) <- names(odds_ratios_0)[ordered_indices + 1]
importance_backward_C


# Plotting feature importance
# Overall Group (Backward Selection)
par(mfrow=c(1, 1))
barplot(importance_backward_O, 
        main = "Overall ImportanceOfVariables", 
        xlab = "", 
        ylab = "Odds Ratios", 
        las = 2, # Makes the variable names perpendicular to the axis
        col = "skyblue")
# Winning Group (Forward Selection)
par(mfrow=c(1, 2))
barplot(importance_backward_W, 
        main = "Winning ImportanceOfVariables", 
        xlab = "", 
        ylab = "Odds Ratios", 
        las = 2, # Makes the variable names perpendicular to the axis
        col = "skyblue")

# Control Group (Forward Selection)
barplot(importance_backward_C, 
        main = "Control ImportanceOfVariables", 
        xlab = "", 
        ylab = "Odds Ratios", 
        las = 2, # Makes the variable names perpendicular to the axis
        col = "skyblue")
par(mfrow=c(1, 1))

#######################################################################
#######################################################################
################ Lasso Regression with LOOCV ########
#######################################################################

library(glmnet)

set.seed(123)
n <- nrow(df_norm_goals)
test_idx <- sample(1:n, 0.3*n)
train_idx <- setdiff(1:n, test_idx)

# Training subset
training_data <- df_norm_goals[train_idx, ]
training_response <- training_data$GoalsMade
training_predictors <- as.matrix(training_data[, -2])
# Test subset
testing_data <- df_norm_goals[test_idx, ]
testing_response <- testing_data$GoalsMade
testing_predictors <- as.matrix(testing_data[, -2])

lasso_model <- cv.glmnet(training_predictors, training_response, alpha=1, nfolds = 10)

plot(lasso_model)
lasso_model

lasso_best <- lasso_model$lambda.min

test_predictions <- predict(lasso_model, newx = testing_predictors)
test_mse <- mean((test_predictions - testing_response)^2)


lasso_final <- glmnet(df_norm_goals[, -2], df_norm_goals$GoalsMade, alpha=1)

coef(lasso_final, s=lasso_best)

lasso_pred <- predict(lasso_final, s=lasso_best, newx=df_norm_goals[, -2], type="coefficients")
lasso_pred


importance <- which(abs(coef(lasso_final, s=lasso_best)) > 0)
feature_names <- names(df_norm_goals)[importance]  

barplot(importance, names.arg = feature_names, 
        las = 2,  # Rotate feature names for readability
        main = "Feature Importance (Lasso)",
        xlab = "Features",
        ylab = "Importance",
        col = "skyblue")

plot(testing_response, test_predictions, 
     xlab = "Actual Goals Made", 
     ylab = "Predicted Goals Made", 
     main = "Predicted vs. Actual Goals Made")
abline(a = 0, b = 1, lty = 1, col="red")

#################################################################################
######################### lasso with two groups split ###########################
#################################################################################

######################### Group Winners #########################################
set.seed(123)
n <- nrow(df_1_norm_goals)
test_idx <- sample(1:n, 0.3*n)
train_idx <- setdiff(1:n, test_idx)

# Training subset
training_data <- df_1_norm_goals[train_idx, ]
training_response <- training_data$GoalsMade
training_predictors <- as.matrix(training_data[, -2])
# Test subset
testing_data <- df_1_norm_goals[test_idx, ]
testing_response <- testing_data$GoalsMade
testing_predictors <- as.matrix(testing_data[, -2])

lasso_model <- cv.glmnet(training_predictors, training_response, alpha=1, nfolds = 10)

plot(lasso_model)

lasso_best <- lasso_model$lambda.min

test_predictions <- predict(lasso_model, newx = testing_predictors)
test_mse <- mean((test_predictions - testing_response)^2)


lasso_final <- glmnet(df_norm_goals[, -2], df_norm_goals$GoalsMade, alpha=1)

coef(lasso_final, s=lasso_best)

lasso_pred <- predict(lasso_final, s=lasso_best, newx=df_norm_goals[, -2], type="coefficients")
lasso_pred


importance <- which(abs(coef(lasso_final, s=lasso_best)) > 0)
feature_names <- names(df_norm_goals)[importance]  

barplot(importance, names.arg = feature_names, 
        las = 2,  # Rotate feature names for readability
        main = "Feature Importance (Lasso)",
        xlab = "Features",
        ylab = "Importance",
        col = "skyblue")

plot(testing_response, test_predictions, 
     xlab = "Actual Goals Made", 
     ylab = "Predicted Goals Made", 
     main = "Predicted vs. Actual Goals Made")
abline(a = 0, b = 1, lty = 1, col="red")

######################### Control Group ####################################

set.seed(123)
n <- nrow(df_0_norm_goals)
test_idx <- sample(1:n, 0.3*n)
train_idx <- setdiff(1:n, test_idx)

# Training subset
training_data <- df_0_norm_goals[train_idx, ]
training_response <- training_data$GoalsMade
training_predictors <- as.matrix(training_data[, -2])
# Test subset
testing_data <- df_0_norm_goals[test_idx, ]
testing_response <- testing_data$GoalsMade
testing_predictors <- as.matrix(testing_data[, -2])

lasso_model <- cv.glmnet(training_predictors, training_response, alpha=1, nfolds = 10)

plot(lasso_model)

lasso_best <- lasso_model$lambda.min

test_predictions <- predict(lasso_model, newx = testing_predictors)
test_mse <- mean((test_predictions - testing_response)^2)


lasso_final <- glmnet(df_norm_goals[, -2], df_norm_goals$GoalsMade, alpha=1)

coef(lasso_final, s=lasso_best)

lasso_pred <- predict(lasso_final, s=lasso_best, newx=df_norm_goals[, -2], type="coefficients")
lasso_pred


importance <- which(abs(coef(lasso_final, s=lasso_best)) > 0)
feature_names <- names(df_norm_goals)[importance]  

barplot(importance, names.arg = feature_names, 
        las = 2,  # Rotate feature names for readability
        main = "Feature Importance (Lasso)",
        xlab = "Features",
        ylab = "Importance",
        col = "skyblue")

plot(testing_response, test_predictions, 
     xlab = "Actual Goals Made", 
     ylab = "Predicted Goals Made", 
     main = "Predicted vs. Actual Goals Made")
abline(a = 0, b = 1, lty = 1, col="red")
