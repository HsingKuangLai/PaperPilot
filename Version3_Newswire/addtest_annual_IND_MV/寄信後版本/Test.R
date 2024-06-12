

data<-read.csv("ERP_RPA_TEST.csv")
# Perform the T-test
t_test_result <- t.test(data$Peer, data$Study)

sink("mean_diff.txt")
# Print the results
print(t_test_result)
sink()