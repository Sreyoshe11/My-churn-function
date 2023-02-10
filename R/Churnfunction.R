Churnfunction <- function(cust_data, Spec_CustomerId=1){

  # browser()
  # check if the customer id provided exists in the dataset  and throw an error otherwise
  if (!Spec_CustomerId %in% cust_data$CustomerId) {
    stop(paste("Customer with ID", Spec_CustomerId, "not found in the dataset"))}
  # } else {
  #   message(paste("Customer with ID", Spec_CustomerId, "found in the dataset"))
  # }
  
  # set cols as factors  
  cust_data$Exited <- as.factor(cust_data$Exited)
  cust_data$Gender <- as.factor(cust_data$Gender)
  
  # Fit the logistic regression model
  model <- glm(Exited ~ CreditScore + Gender + Age + Tenure + Balance + 
                 NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary,
               data = cust_data, family = "binomial")
  
  # Predict the churn probability for each customer
  cust_data$ChurnProbability <- predict(model, newdata = cust_data, type = "response")
  
  # churn prob for the specific customer
  churn_cust <- cust_data[cust_data$CustomerId == Spec_CustomerId,ChurnProbability]
  
  # return final churn prob for customer
  return(churn_cust)
}
