# usethis::use_testthat()

context("Probability comparision")

library(data.table)

# Read data
data_customer <- fread("data_customer.csv")
data_personal <- fread("data_personal.csv")

# Prepare data for analysis
merge_df <- merge(data_customer, data_personal, by="CustomerId", all.x=TRUE)
merge_df$Exited <- as.factor(merge_df$Exited)
merge_df$Gender <- as.factor(merge_df$Gender)

high_prob <- Churnfunction(merge_df,15653251)
low_prob <- Churnfunction(merge_df,15662641)

testthat("first 2 numbers in a list are added",{
  expect_gt(high_prob > low_prob, TRUE)

})



