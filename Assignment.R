# install and load the libraries
install.packages("caret")
install.packages('e1071')
install.packages("FSelector")
library(FSelector)
library(caret)
library(e1071)

# ensure the results are repeatable
set.seed(1)

# load the data
df <- read.csv2("Desktop/AegonTrainingA3/data.csv",header = TRUE, sep = ";",strip.white=TRUE)
df_features <- df[,-grep('X|user_id|future_activity|past_activity',colnames(df))]

# Make factor column into dummy features
install.packages("reshape")
library(reshape)
x <- cbind(df,value=1)

CreateDummies <- function(df,columnindex){
  distValues <- length(unique(df[,columnindex]))
  y <- cast(df, X ~ device_type, fill=FALSE)
  for(i in seq(distValues)){
    while(i<distValues){
      colnames(y)[i+1] <- paste0(colnames(df)[columnindex],'_',colnames(y)[i+1])
    }
    
  }
  
}

colnames(y)[2] <- paste0(colnames(df_features)[9],'_',colnames(y)[2])

## Filer method: Correlations ##
df_features_numeric <- df_features[,which(sapply(df_features,is.numeric))]
Correlations <- cor(df_features_numeric)
view(Correlations)
ToDelete <- findCorrelation(Correlations)

## Wrapper method: RecursiveFeatureElimination ##
# Define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=5)
# Run the RFE algorithm
results <- rfe(df_s[,-grep('User-ID|FUTURE_ACTIVITY|PAST_ACTIVITY', colnames(df_s))], df_s[,grep('FUTURE_ACTIVITY', colnames(df_s))], sizes=2^(1:3), rfeControl=control)
# Summarize the results
print(results)
# List the chosen features
predictors(results)
# Plot the results
plot(results, type=c("g", "o"))








df_s <- df[sample(nrow(df), 1000), c(45,82,98,121,386,391,555,789,834,911)] 
df_s <- cbind(df[sample(nrow(df), 1000), c(45,82,98,121,386,391,555,789,834,911)] ,df[sample(nrow(df), 1000),'FUTURE_ACTIVITY'])
colnames(df_s)[ncol(df_s)] <- 'FUTURE_ACTIVITY'
df_s[df_s$FUTURE_ACTIVITY >0,'FUTURE_ACTIVITY']  <- '1'
df_s$FUTURE_ACTIVITY  <- as.factor(df_s$FUTURE_ACTIVITY)

# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=5)
# run the RFE algorithm
results <- rfe(df_s[,-grep('User-ID|FUTURE_ACTIVITY|PAST_ACTIVITY', colnames(df_s))], df_s[,grep('FUTURE_ACTIVITY', colnames(df_s))], sizes=2, rfeControl=control)

# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))


filterCtrl <- sbfControl(functions = rfSBF, method = "repeatedcv", repeats = 5)
set.seed(10)
rfWithFilter <- sbf(df_s[,-grep('User-ID|FUTURE_ACTIVITY|PAST_ACTIVITY', colnames(df_s))], df_s[,grep('FUTURE_ACTIVITY', colnames(df_s))], sbfControl = filterCtrl)
print(rfWithFilter)

varImp <- filterVarImp(df_s[,-grep('User-ID|FUTURE_ACTIVITY|PAST_ACTIVITY', colnames(df_s))], df_s[,grep('FUTURE_ACTIVITY', colnames(df_s))], nonpara = FALSE)
print(varImp[order(varImp$X0),])

Correlation_based <- cfs(df_s$FUTURE_ACTIVITY~df_s[,-grep('User-ID|FUTURE_ACTIVITY|PAST_ACTIVITY', colnames(df_s))],df_s)
