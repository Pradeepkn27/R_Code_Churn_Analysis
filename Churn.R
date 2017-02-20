install.packages("ROCR")
require(caret)
library(ROCR)

#Read data into R
Churn<-read.table(file="E:\\Pradeep_pc_backup\\Pradeep_pc_backup\\Data_Science_Material\\Specialization_project\\attachment_churn_data.txt",sep=",")
colnames(Churn) <- c("state","account_length","area","phone_number","international_plan","voice_mail_plan","number_of_voice_mail_messages","total_day_minutes_used","day_calls_made","total_day_charge","total_evening_minutes","total_evening_calls","total_evening_charge","total_night_minutes","total_night_calls","total_night_charge","total_international_minutes_used","total_international_calls_made","total_international_charge","number_customer_service_calls_made","Churnstatus")

summary(Churn)
table(is.na(Churn))
cor(Churn[,unlist(lapply(Churn, is.numeric))])


sample.ind <- sample(2, 
                     nrow(Churn),
                     replace = T,
                     prob = c(0.6,0.4))

Churn.data.dev <- Churn[sample.ind==1,]
Churn.data.val <- Churn[sample.ind==2,]


Churnstatusmodel <- glm(Churnstatus~(state + account_length + area+international_plan+voice_mail_plan+number_of_voice_mail_messages+total_day_minutes_used+day_calls_made+total_day_charge+total_evening_minutes+total_evening_calls+total_evening_charge+total_night_minutes+total_night_calls+total_night_charge+total_international_minutes_used+total_international_calls_made+total_international_charge+number_customer_service_calls_made),data=Churn.data.dev,family=binomial(link=logit))

summary(Churnstatusmodel) # display results

exp(coef(Churnstatusmodel))

churnresultstatus = round(predict(Churnstatusmodel, newdata=Churn.data.val, type='response'))

Churn.data.val$Churnstatus1 <- ifelse((Churn.data.val$Churnstatus)==" False.",0,1)
Churn.data.val$Churnstatus1

accuracy <- table(churnresultstatus, Churn.data.val[,"Churnstatus1"])
sum(diag(accuracy))/sum(accuracy)

confusionMatrix(churnresultstatus,Churn.data.val$Churnstatus1)

p <- predict(Churnstatusmodel, newdata=Churn.data.val, type="response")
pr <- prediction(p, Churn.data.val$Churnstatus1)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

