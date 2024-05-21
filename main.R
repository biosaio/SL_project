# import dataset

df <- read.csv('C:/Users/saius/Documents/SL_project/DatasetR_eng_fin.csv', header=TRUE, sep=';', 
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
df$BallPoss <- as.numeric(sub(",", ".", sub("%", "", df$BallPoss))) / 100
df$ShotsPrec <- as.numeric(sub(",", ".", sub("%", "", df$ShotsPrec))) / 100
df$PassPrec <- as.numeric(sub(",", ".", sub("%", "", df$PassPrec))) / 100
df$TacklesWRatio <- as.numeric(sub(",", ".", sub("%", "", df$TacklesWRatio))) / 100
df$AirDuelW <- as.numeric(sub(",", ".", sub("%", "", df$AirDuelW))) / 100
df$DribWRatio <- as.numeric(sub(",", ".", sub("%", "", df$DribWRatio))) / 100

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

df$OutcomeWL[df$OutcomeWL == -1] <- 0


# Let's see which features are correlated with each other:
S <- round(cov(df), 4)
P <- round(cor(df), 4)

# We use a heatmap to explore the correlations
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = P, col = col, symm = TRUE)

df_winners <- df[df$Group == 1, ]
df_winners$Group <- NULL
df_control <- df[df$Group == 0, ]
df_control$Group <- NULL

P_win <- round(cor(df_winners), 4)
P_cont <- round(cor(df_control), 4)

heatmap(x = P_win, col = col, symm = TRUE, main = 'P_win')
heatmap(x = P_cont, col = col, symm = TRUE, main = 'P_cont')

