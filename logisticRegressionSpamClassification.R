#Nathan Suh
#Spam Classification
#Building logistic regression model after data exploration


load("~/Projects/spamFilter/cleanTrainingVariableData.rda")
load("~/Projects/spamFilter/testData.rda")

yellingLogReg = glm(isSpam ~ isYelling, data = trainVariables, family = binomial)
capitalsLogReg = glm(isSpam ~ percentCapitals, data = trainVariables, family = binomial)
plot(isSpam ~ numRecipients, data = trainVariables)


densityplot(isSpam ~ percentCapitals, groups = isSpam, data = trainVariables, auto.key = TRUE)

plot(trainVariables$subjectSpamWords, trainVariables$isSpam)


#percentage of capitals in the body
densityplot(isSpam ~ percentCapitals, groups = isSpam, data = trainVariables, auto.key = TRUE)
percentCapitalsLogReg = glm(isSpam ~ percentCapitals, data = trainVariables, family = binomial)
model = summary(percentCapitalsLogReg)
betas = percentCapitalsLogReg$coefficients    ### To extract the coefficients of the model
res = percentCapitalsLogReg$residuals         #### To extract the residuals
fit = percentCapitalsLogReg$fitted            #### To extract fitted values : estimated probabilities
dev = percentCapitalsLogReg$deviance          #### To extract the deviance
se = model$coefficients[,2]   #### To extract std error
pvalues = model$coefficients[,4] #### To extract p-values
#with(percentCapitalsLogReg, pchisq(7266.4-5726., 1, lower.tail = FALSE))
#logLik(percentCapitalsLogReg)



#number of exclaimations in subject
densityplot(isSpam ~ subjectExclamationCount, groups = isSpam, data = trainVariables, auto.key = TRUE)
subjectExclamationLogReg = glm(isSpam ~ subjectExclamationCount, data = trainVariables, family = binomial)

#if the header has an In Reply To field
#densityplot(isSpam ~ isInReplyTo, groups = isSpam, data = trainVariables, auto.key = TRUE)
#inReplyLogReg = glm(isSpam ~ isInReplyTo, data = trainVariables, family = binomial)

#percent forwards (how much of the message contains old messages)
percentFowardsLogReg = glm(isSpam ~ percentForwards, data = trainVariables, family = binomial)
densityplot(isSpam ~ percentForwards, groups = isSpam, data = trainVariables, auto.key = TRUE)

#if the subject contains spam words
spamWordsLogReg = glm(isSpam ~ subjectSpamWords, data = trainVariables, family = binomial)


#number of recipients
densityplot(isSpam ~ numRecipients, groups = isSpam, data = trainVariables, auto.key = TRUE)
numRecipientsLogReg = glm(isSpam ~ numRecipients, data = trainVariables, family = binomial)




#building the model:
completeLogisticRegressionModel = glm(isSpam ~ percentCapitals + subjectExclamationCount + percentForwards + subjectSpamWords + numRecipients, data = trainVariables, family = binomial)



#Testing the model!!
testData = testVariables[40,]
predict(completeLogisticRegressionModel, testData, type = "response")

errorCount = 0

for (i in 1:length(testVariables[,1])) {
  testData = testVariables[i,]
  prediction = predict(completeLogisticRegressionModel, testData, type = "response")
  if (is.na(prediction) == TRUE) {
    next
  }
  if ((prediction < 0.5) & (testVariables$isSpam[i] == TRUE)) { #if we classify SPAM as a HAM (false negative)
    errorCount = errorCount + 1
  } else if ((prediction > 0.5) & (testVariables$isSpam[i] == FALSE)) { #if we classify a HAM as SPAM, this is not good (false positive)
    errorCount = errorCount + 1
  }
}
logisticRegressionAccuracy = (2000-errorCount)/2000

#0.8675 accuracy rate.



