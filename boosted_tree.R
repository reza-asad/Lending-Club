# Reza Asad
# September 25th, 2015
# LendUp Data Challenge Question 2 part 1

library("adabag")
library("zoo")

rm(list = setdiff(ls(), lsf.str()))
loan_dat <- read.csv("/home/reza/LendUp/LoanStats3b.csv",
  skip = 1, header = TRUE, stringsAsFactors = FALSE)

numeric_cols <- c("loan_amnt", "installment", "dti", 
  "delinq_2yrs", "inq_last_6mths", "open_acc",
  "pub_rec", "revol_bal", "last_pymnt_amnt", 
  "mths_since_last_delinq","collections_12_mths_ex_med",
  "policy_code", "annual_inc", "mths_since_last_record",
  "total_acc", "int_rate", "revol_util", 
  "mths_since_last_major_derog", "total_rec_int")

date_cols <- c("issue_d", "earliest_cr_line", "last_pymnt_d",
  "last_credit_pull_d")

string_cols <- c("grade", "sub_grade", "verification_status",
  "pymnt_plan", "purpose", "emp_length", "home_ownership",  
  "zip_code", "addr_state", "initial_list_status")

# Here I will do a number of data cleaning before buildign
# the model.
# 1) This converts string of percent values to integers.
loan_dat["int_rate"] = as.numeric(sub("%", "", 
  loan_dat$int_rate))
loan_dat["revol_util"] = as.numeric(sub("%", "", 
  loan_dat$revol_util))

# 2) This builds an assumption that if the field 
#    "mths_since_last_delin" is empty it means that
#     borrower did not have a delinquency. Similarly
#     for the field mths_since_last_major_derog
#     and mths_since_last_record. I set the values
#     to a large number like 1000
I <- is.na(loan_dat$mths_since_last_delinq)
loan_dat$mths_since_last_delinq[I] = "1000"

I <- is.na(loan_dat$mths_since_last_record)
loan_dat$mths_since_last_record[I] = "1000"

I <- is.na(loan_dat$mths_since_last_major_derog)
loan_dat$mths_since_last_major_derog[I] = "1000"

# 3) This removes some of the columns
loan_dat = loan_dat[,-which(names(loan_dat) %in% 
  c("url", "id", "member_id", "desc", "title",
    "out_prncp", "next_pymnt_d", "funded_amnt",
    "out_prncp_inv", "emp_title", "funded_amnt_inv",
    "total_pymnt_inv", "total_rec_prncp"))]

# 4) This converts strings in numeric_cols
#    to numeric values
for (i in 1:length(numeric_cols)) {
  loan_dat[numeric_cols[i]] = as.numeric(
    loan_dat[[numeric_cols[i]]])
}

# 5) This converts the fields with value month-year
#    to numeric values
for (i in 1:length(date_cols)) {
  loan_dat[date_cols[i]] = as.numeric(
    as.yearmon(loan_dat[[date_cols[i]]],
    format = "%b-%Y"))
}

# 6) This converts the "term" values to number of years 
I <- gsub("[[:space:]]", "", loan_dat$term)
I <- (I == "36months")
loan_dat$term[I] = 3
I <- gsub("[[:space:]]", "", loan_dat$term)
I <- (I == "60months")
loan_dat$term[I] = 5

# this omits the NA values
loan_dat <- na.omit(loan_dat)

# To create the training data I find data with label
# "Current" or "In Grace Period". This part of data
# is called test_dat. I am going to predict its label
# later.
I <- gsub("[[:space:]]", "", loan_dat$loan_status)
I <- (I == "Current" | I == "InGracePeriod")
test_dat <- loan_dat[I, ]
test_dat <- test_dat[,-which(names(test_dat) == 
  "loan_status")]


# This is the full training data with labels
# fully paid and late. I convert the labesl
# to 1 and zero respectively.
y_dat <- loan_dat[!I, "loan_status"]
loan_dat <- loan_dat[,-which(names(loan_dat) == 
  "loan_status")]
x_dat <- loan_dat[!I, ]

I <- gsub("[[:space:]]", "", y_dat)
I <- (I == "FullyPaid")
y_dat[I] <- 1
y_dat[!I] <- 0


# I Use logistic regression to predict the label for
# test_dat. I have created an artificial field 
# for the logistic model which represents how far
# away is the person from paying the loan. The 
# formula uses fields: loan_amnt, total_pymnt,
# issue_d, last_pymnt and term.

a = x_dat$loan_amnt 
b = x_dat$total_pymnt
c = x_dat$issue_d
d = x_dat$last_pymnt_d
e = as.numeric(x_dat$term)
f = ( abs(a - b) /(a))
g = ((c + e - d) /e)

logist1 <- glm(factor(y_dat) ~ f+g, family = binomial)

a = test_dat$loan_amnt 
b = test_dat$total_pymnt
c = test_dat$issue_d
d = test_dat$last_pymnt_d
e = as.numeric(test_dat$term)
f = ( abs(a - b) /(a))
g = ((c + e - d) /e)

test <- data.frame(f,g)
logist_predic1 <- predict(logist1, newdata <- test,
  type = "response")

loan_dat <- 0
I <- 0

# Create train, test and validation sets
reps <- floor(nrow(x_dat)/3)
ii <- rep(1:3, each = reps)
ii <- c(ii, rep(1:3, length.out = nrow(x_dat)-reps*3))

set.seed(23)
ii <- sample(ii)
x_dat <- cbind(ii,x_dat)
y_dat <- cbind(ii,y_dat)

x_train <- x_dat[ii==1, ]
y_train <- y_dat[ii==1, ]

x_valid <- x_dat[ii==2, ]
y_valid <- y_dat[ii==2, ]

x_test <- x_dat[ii==3, ]
y_test <- y_dat[ii==3, ]


# I have divided my training set to three parts.
# train, test and validation. I run logistic
# regression on each. 
a = x_train$loan_amnt 
b = x_train$total_pymnt
c = x_train$issue_d
d = x_train$last_pymnt_d
e = as.numeric(x_train$term)
f = ( abs(a - b) /(a))
g = ((c + e - d) /e)

logist2 <- glm(factor(y_train[,-1]) ~ f+g,
  family = binomial)

a = x_valid$loan_amnt 
b = x_valid$total_pymnt
c = x_valid$issue_d
d = x_valid$last_pymnt_d
e = as.numeric(x_valid$term)
f = ( abs(a - b) /(a))
g = ((c + e - d) /e )

test <- data.frame(f,g)
logist_predic2 <- predict(logist2, newdata <- test,
  type = "response")

a = x_test$loan_amnt 
b = x_test$total_pymnt
c = x_test$issue_d
d = x_test$last_pymnt_d
e = as.numeric(x_test$term)
f = ( abs(a - b) /(a))
g = ((c + e - d) /e)

test <- data.frame(f,g)
logist_predic3 <- predict(logist2, newdata <- test,
  type = "response")

# I remove the fields taht I used in building the logistic
# regression from my data.
x_dat <- x_dat[,-which(names(x_dat) %in% 
  c("total_pymnt", "loan_amnt", "issue_d",	
  "last_pymnt_d", "term"))]

test_dat <- test_dat[,-which(names(test_dat) %in% 
  c("total_pymnt", "loan_amnt", "issue_d",
  "last_pymnt_d", "term"))]

x_train <- x_train[,-which(names(x_train) %in% 
  c("total_pymnt", "loan_amnt", "issue_d",
  "last_pymnt_d", "term"))]
  
x_valid <- x_valid[,-which(names(x_valid) %in% 
  c("total_pymnt", "loan_amnt", "issue_d",
  "last_pymnt_d", "term"))]

x_test <- x_test[,-which(names(x_test) %in% 
  c("total_pymnt", "loan_amnt", "issue_d",
  "last_pymnt_d", "term"))]


# This function performs boosting on decision
# trees.
# Inputs:  The validation/test set, the true 
# labels for the validation/test set, the x
# and y part of the 
# training set.
# Output:  The prediction for the labels of
# validation/test set.
boosting_model <- function(xtrain, ytrain, xtest,
  ytest,maxDepth) {  
  ytrain <- as.factor(ytrain)
  train <- cbind(xtrain, ytrain)

  for (i in 1:length(string_cols)) {
    train[string_cols[i]] = as.factor(train[[string_cols[i]]])
    levels(train[[string_cols[i]]]) = 
      union(levels(train[[string_cols[i]]]), 
      levels(as.factor(xtest[[string_cols[i]]])))
  }
  dec_tree <- rpart.control(cp = -1, maxdepth=maxDepth)
  boost <- boosting(ytrain ~ ., data=train, mfinal = 8,
    control=dec_tree)
  print(boost$importance)
  xtest$ytrain <- factor(c("0"), levels = c("0", "1"))
  pred <- predict(boost, newdata = xtest)
  return(pred)
}

# I graph the test errors for different values
# of maxDepth
#depthValues <- seq(1,30, by = 2)
#m <- length(depthValues)
#valid_errors <- c()

#for (i in 1:m) {
#  predic <- boosting_model(x_train[,-1], y_train[,-1],
#    x_valid[,-1], y_valid[,-1], depthValues[i])
#  I <- (predic$prob[,2]+logist_predic2)/2
#  I <- (I > 0.5)
#  valid_errors[i] <- mean(I != as.numeric(y_valid[,-1]))
#  print(valid_errors[i])
#}

#pdf("/home/reza/LendUp/err_maxDepth.pdf")
#plot(valid_errors ~ depthValues, col = "red", pch = 19,
#ylab = "Error on Validation Set", xlab = "Depth of the Tree",
#main = "Error Versus Max Depth of the Tree")
#dev.off()

# The plot shows 23 is the best value for maxDepth.
# I use this value of maxDepth in my model to predict the
# labels for the test data. Here is the test error for that.
predic <- boosting_model(x_train[,-1], y_train[,-1], 
  x_test[,-1], y_test[,-1], 5)

I <- (predic$prob[,2]+logist_predic3)/2
I <- (I > 0.5)

print(mean(I != as.numeric(y_test[,-1]))) # the error: 0.05978716
 
# Here is the prediction on the data points with label
# "Current" or "In Grace Period". Here 1 represents
# person paying the loan back and 0 otherwise. Since
# I dont have teh true labels for test_dat for now
# I have set it to y_test[,-1]. But you can use your
# predictions from your modeland check it against my model.
#predic <- boosting_model(x_dat[,-1], y_dat[,-1], 
#  test_dat, y_test[,-1], 23)
  
#I <- (predic$prob[,2]+logist_predic1)/2
#final_predic <- (I > 0.5)
#sum(final_predic == 1) 


