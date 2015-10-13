# Reza Asad
# September 25th, 2015
# LendUp Data Challenge Question 1

library("googleVis")

# get the data
loan_dat <- read.csv("/home/reza/LendUp/LoanStats3b.csv",
  skip = 1, header = TRUE, stringsAsFactors = FALSE)

# The fields int_rate and revol_util contain string
# of percent value. This converts string of percent
# value to numeric.
loan_dat["int_rate"] = as.numeric(sub("%", "",
  loan_dat$int_rate))
loan_dat["revol_util"] = as.numeric(sub("%", "",
  loan_dat$revol_util))


numeric_cols <- c("id", "member_id", "loan_amnt",
  "funded_amnt", "funded_amnt_inv", "installment",
  "dti", "delinq_2yrs", "inq_last_6mths","open_acc",
  "pub_rec", "revol_bal", "out_prncp","out_prncp_inv",
  "total_pymnt", "total_pymnt_inv","total_rec_prncp",
  "total_rec_int", "total_rec_late_fee","recoveries",
  "collection_recovery_fee", "last_pymnt_amnt",
  "mths_since_last_delinq","collections_12_mths_ex_med",
  "policy_code", "annual_inc", "mths_since_last_record",
  "total_acc", "int_rate", "revol_util",
  "mths_since_last_major_derog")

string_cols <- c("term", "grade", "sub_grade",
  "verification_status", "pymnt_plan", "purpose",
  "title", "emp_length", "home_ownership","emp_title",
  "zip_code", "addr_state", "initial_list_status",
  "url", "loan_status", "desc")

date_cols <- c("next_pymnt_d","issue_d", "earliest_cr_line",
  "last_pymnt_d", "last_credit_pull_d")
 
# This function takes a data frame and the requested
# column of that data frame.
# Input:   A data frame and a column name of the
#	   data frame. 
# Output:  An interactive histogram pops in the browser
#          if the column is numeric. A bar plot is shown 
#          if the input is a column of strings. A summary
#          statistics if the input is numeric and the
#          number of distinct values if the input is
#          string.
# Notes: * If the input is in string_cols and the number
#          of unique values are more than 100, it prints
#          "The input data is very diverse" along with a
#          summary showing number of distinct values.
#	 * If the column does not belong to the data frame
#          provided, a message shows up "The column you
#          asked does not exist".
col_stats <- function(data, col_name) {
  if(is.element(col_name, numeric_cols)) {
    col_values = as.numeric(data[[col_name]])
    df = data.frame(col_values)
    Hist <- gvisHistogram(df, options=list(legend=
      "{ position: 'top', maxLines: 2 }", height = 500, 
      title = paste(c("Distibution of", col_name),
      collapse=" ")))
    plot(Hist)
    print(summary(as.numeric(data[[col_name]])))
  } else if(is.element(col_name, string_cols)) {
    if(length(unique(data[[col_name]])) > 100) {
    print("The input data is very diverse")
    print(summary(as.factor(data[[col_name]])))
    } else {
      dev.new(width = 12, height = 5)
      barplot(table(data[col_name]), xlab = col_name,
	main = paste(c("distibution of", col_name),
	collapse=" "), 
	col = "light blue", ylab = "Density", cex.names = 0.7,
	ylim = c(0,max(summary(as.factor(data[[col_name]])))))
      print(summary(as.factor(data[[col_name]])))
    }
  } else if(is.element(col_name, date_cols)) {
      date <- as.Date(paste("01-", loan_dat[[col_name]], sep = ""),
	format = "%d-%b-%Y")
      dev.new(width = 12, height = 5)
      hist(as.Date(date, format = "%b-%Y"), breaks = "months",
	format = "%b %Y", col = "light blue", xlab = col_name, 
	main = paste(c("distibution of", col_name)))
      print(summary(as.factor(data[[col_name]])))
  } else {
      print("The column you asked does not exist.")
  }
}

      col_name <- readline(prompt = "Enter the column name
	you are interested in: ")
      col_stats(loan_dat, col_name)


