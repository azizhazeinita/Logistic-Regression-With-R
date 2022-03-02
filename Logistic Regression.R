# Use diabetes dataset from canvas. Readmitted is dependent variable.
df <- read.csv("/Users/azizhazeinita/Documents/S2 Uchicago/MScA/Winter 2022 - 2nd Quarter/Data Mining Principles/Assignment 4 - Data Mining Principles/Diabetese Dataset Files/diabetes_data.csv")

colSums(is.na(df))
df <- na.omit(df)


# Merge the < 30 days and > 30 days values to “Yes”. For logistic regression dependent variable should be binary.
df['readmitted'][df['readmitted'] == '<30'] <- 'Yes'
df['readmitted'][df['readmitted'] == '>30'] <- 'Yes'

#3. Dummy/oneHot encode categorical variables. (One hot encode on whole dataset and then use for splitting)
#install.packages('caret')
df['readmitted'] <- NULL

library(caret)

dummy <- dummyVars(" ~ .", data=df) #define one-hot encoding function
#dummy[['vars']]
final_df <- data.frame(predict(dummy, newdata=df)) #perform one-hot encoding on data frame

unique(df_readmitted)
df_readmitted$readmitted = factor(df_readmitted$readmitted, levels = c('Yes', 'NO'),labels = c(1, 0))

final_df['readmitted'] <- df_readmitted

# Data Encoding
unique(df[c('race')])
df$race = factor(df$race, levels = c('Caucasian', 'AfricanAmerican', 'Asian', 'Hispanic', 'Other'),labels = c(1, 2, 3, 4, 5))
unique(df[c('gender')])
df$gender = factor(df$gender, levels = c('Female', 'Male'),labels = c(1, 2))
unique(df[c('age')])
df$age = factor(df$age, levels = c('[0-10)','[10-20)', '[20-30)', '[30-40)', '[40-50)', '[50-60)', '[60-70)', '[70-80)', '[80-90)', '[90-100)'),labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
unique(df[c('max_glu_serum')])
df$max_glu_serum = factor(df$max_glu_serum, levels = c('None', '>300', 'Norm', '>200'),labels = c(1, 2, 3, 4))
unique(df[c('A1Cresult')])
df$A1Cresult = factor(df$A1Cresult, levels = c('None', '>7', 'Norm', '>8'),labels = c(1, 2, 3, 4))
unique(df[c('metformin')])
df$metformin = factor(df$metformin, levels = c('No', 'Steady', 'Up', 'Down'),labels = c(1, 2, 3, 4))
unique(df[c('repaglinide')])
df$repaglinide = factor(df$repaglinide, levels = c('No', 'Steady', 'Up', 'Down'),labels = c(1, 2, 3, 4))
unique(df[c('nateglinide')])
df$nateglinide = factor(df$nateglinide, levels = c('No', 'Steady', 'Up', 'Down'),labels = c(1, 2, 3, 4))
unique(df[c('chlorpropamide')])
df$chlorpropamide = factor(df$chlorpropamide, levels = c('No', 'Steady', 'Up', 'Down'),labels = c(1, 2, 3, 4))
unique(df[c('glimepiride')])
df$glimepiride = factor(df$glimepiride, levels = c('No', 'Steady', 'Up', 'Down'),labels = c(1, 2, 3, 4))
unique(df[c('glipizide')])
df$glipizide = factor(df$glipizide, levels = c('No', 'Steady', 'Up', 'Down'),labels = c(1, 2, 3, 4))
unique(df[c('glyburide')])
df$glyburide = factor(df$glyburide, levels = c('No', 'Steady', 'Up', 'Down'),labels = c(1, 2, 3, 4))
unique(df[c('pioglitazone')])
df$pioglitazone = factor(df$pioglitazone, levels = c('No', 'Steady', 'Up', 'Down'),labels = c(1, 2, 3, 4))
unique(df[c('rosiglitazone')])
df$rosiglitazone = factor(df$rosiglitazone, levels = c('No', 'Steady', 'Up', 'Down'),labels = c(1, 2, 3, 4))
unique(df[c('acarbose')])
df$acarbose = factor(df$acarbose, levels = c('No', 'Steady', 'Up', 'Down'),labels = c(1, 2, 3, 4))
unique(df[c('miglitol')])
df$miglitol = factor(df$miglitol, levels = c('No', 'Steady', 'Up', 'Down'),labels = c(1, 2, 3, 4))
unique(df[c('tolazamide')])
df$tolazamide = factor(df$tolazamide, levels = c('No', 'Steady', 'Up', 'Down'),labels = c(1, 2, 3, 4))
unique(df[c('acetohexamide')]) 
df$acetohexamide = factor(df$acetohexamide, levels = c('No', 'Steady'),labels = c(1, 2))
unique(df[c('tolbutamide')])
df$tolbutamide = factor(df$tolbutamide, levels = c('No', 'Steady', 'Up', 'Down'),labels = c(1, 2, 3, 4))
unique(df[c('troglitazone')])
df$troglitazone = factor(df$troglitazone, levels = c('No', 'Steady', 'Up', 'Down'),labels = c(1, 2, 3, 4))
unique(df[c('insulin')])
df$insulin = factor(df$insulin, levels = c('No', 'Steady', 'Up', 'Down'),labels = c(1, 2, 3, 4))
unique(df[c('glyburide.metformin')])
df$glyburide.metformin = factor(df$glyburide.metformin, levels = c('No', 'Steady', 'Up', 'Down'),labels = c(1, 2, 3, 4))
unique(df[c('glipizide.metformin')])
df$glipizide.metformin = factor(df$glipizide.metformin, levels = c('No', 'Steady', 'Up', 'Down'),labels = c(1, 2, 3, 4))
unique(df[c('glimepiride.pioglitazone')])
df$glimepiride.pioglitazone = factor(df$glimepiride.pioglitazone, levels = c('No', 'Steady', 'Up', 'Down'),labels = c(1, 2, 3, 4))
unique(df[c('metformin.pioglitazone')])
df$metformin.pioglitazone = factor(df$metformin.pioglitazone, levels = c('No', 'Steady', 'Up', 'Down'),labels = c(1, 2, 3, 4))
unique(df[c('change')])
df$change = factor(df$change, levels = c('Ch', 'No'),labels = c(1, 2))
unique(df[c('diabetesMed')])
df$diabetesMed = factor(df$diabetesMed, levels = c('Yes', 'No'),labels = c(1, 2))
unique(df[c('readmitted')])
df$readmitted = factor(df$readmitted, levels = c('Yes', 'NO'),labels = c(1, 2))

str(df)
df$diag_1 <- as.double(df$diag_1)
df$diag_2 <- as.double(df$diag_2)
df$diag_3 <- as.double(df$diag_3)

# Separate dataset into train and test samples (70:30). Using set.seed (R)/ random_state (Python) while splitting. 

smp_size <- floor(0.7 * nrow(df)) #70% data train
set.seed(123) # set the seed to make your partition reproducible
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]

lapply(train, unique)
#acetohexamide only has one level, so it can't be included to the model
train$acetohexamide <- NULL
test$acetohexamide <- NULL

#5. Build logistic regression model on Train Data set - Use below steps for reference.
#Variables that are taking too long time to be run (more than 30 mins and didn't work): diag_2, diag_3, diag_1
dimnames(train)
#step(glm(readmitted~., data=train, family=binomial(link=logit)), direction="forward")
#step(glm(readmitted~., data=train, family=binomial(link=logit)), direction="backward")
  
model <- glm(readmitted~., data=train, family=binomial(link=logit))
summary(step(model,direction="forward"))
summary(step(model,direction="backward"))

library(MASS)
stepAIC(model, direction=c("both"))

#help(step)



# df1_t <- train[ , c('encounter_id', 'age', 'time_in_hospital', 'number_outpatient', 'chlorpropamide','tolbutamide', 'miglitol', 'glyburide.metformin', 'change','patient_nbr', 'readmitted')]
# model1 <- glm(readmitted~., data=df1_t, family=binomial(link=logit))
# df2_t <- train[ , c('admission_type_id', 'num_lab_procedures', 'number_emergency', 'metformin', 'glimepiride', 'pioglitazone', 'troglitazone', 'glipizide.metformin', 'diabetesMed', 'readmitted')]
# model2 <- glm(readmitted~., data=df2_t, family=binomial(link=logit))
# df3_t <- train[ , c('race', 'discharge_disposition_id', 'num_procedures', 'number_inpatient', 'number_diagnoses', 'repaglinide', 'glipizide', 'rosiglitazone', 'tolazamide', 'glimepiride.pioglitazone', 'readmitted')]
# model3 <- glm(readmitted~., data=df3_t, family=binomial(link=logit))
# df4_t <- train[ , c('gender', 'admission_source_id', 'num_medications', 'max_glu_serum', 'nateglinide', 'glyburide', 'acarbose', 'insulin', 'metformin.pioglitazone', 'readmitted')]
# model4 <- glm(readmitted~., data=df4_t, family=binomial(link=logit))
# str(df)
# 
# df5_t <- train[ , c('diag_2', 'readmitted')]
# model5 <- glm(readmitted~train$diag_2, data=df2_t, family=binomial(link=logit))



