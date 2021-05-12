# EDA

#data = read.csv("C:/Users/mua377n/Downloads/archive/Loan_status_2007-2020Q3.gzip")
data = data[-1]
print(dim(data))
colnames(data)

# missing values plot for different variables

missing_values = apply(is.na(data),2, sum )/nrow(data)
barplot(missing_values,legend.text = 'variables', main='NAs for different variables')

#treating loan_status

loan_status_keep = c(unique(data$loan_status)[1],
                     unique(data$loan_status)[2],
                     unique(data$loan_status)[6],
                     unique(data$loan_status)[7],
                     unique(data$loan_status)[9],
                     unique(data$loan_status)[10])
data_new = data[data$loan_status %in% loan_status_keep,]
remove(data)

ggplot(data = data_new)+geom_bar(mapping = aes(x = loan_status))

# dropping temporal variables, since they were not available at origination

drop_colums = c('last_credit_pull_d','last_fico_range_high','last_fico_range_low','total_pymnt',
                'total_pymnt_inv','recoveries','collection_recovery_fee','out_prncp','out_prncp_inv',
                'total_rec_prncp','total_rec_int','last_pymnt_d','last_pymnt_amnt','next_pymnt_d',
                'total_rec_late_fee','hardship_flag','hardship_amount',
                'orig_projected_additional_accrued_interest','hardship_payoff_balance_amount',
                'hardship_last_payment_amount','debt_settlement_flag','hardship_type','hardship_reason',
                'hardship_status','deferral_term','hardship_start_date','hardship_end_date','payment_plan_start_date',
                'hardship_length','hardship_dpd','hardship_loan_status','installment','pymnt_plan',                        'acc_now_delinq','url','earliest_cr_line','last_fico_range_low','last_fico_range_high','policy_code','zip_code',
                'sec_app_fico_range_low','sec_app_fico_range_high','sec_app_earliest_cr_line','fico_range_low', 'fico_range_high',
                'annual_inc_joint','dti_joint','verification_status_joint', 'revol_bal_joint','sec_app_inq_last_6mths',
                'sec_app_mort_acc','sec_app_open_acc','sec_app_revol_util','sec_app_open_act_il',
                'sec_app_num_rev_accts','sec_app_chargeoff_within_12_mths','sec_app_collections_12_mths_ex_med')

data_new = data_new[,!names(data_new)%in% drop_colums]

# data_new = data_new %>% select (-one_of(drop_colums)

#stripping % from character type and converting to numeric
data_new$int_rate = as.numeric(sub("%", "", data_new$int_rate))
data_new$revol_util = as.numeric(sub("%", "", data_new$revol_util))
data_new$id = as.integer(data_new$id)

# creating target Y variable coFlag
data_new$coFlag = ifelse (data_new$loan_status %in% c(loan_status_keep[1], loan_status_keep[5], loan_status_keep[3]), 0, 1)

str(data_new)


# visualizing grade
ggplot(data = data_new)+geom_bar(mapping = aes(x = grade))

# visualizing home ownership
ggplot(data = data_new)+geom_bar(mapping = aes(x = home_ownership))

# visualizing term
ggplot(data = data_new)+geom_bar(mapping = aes(x = term))

# visualizing emp_length
ggplot(data = data_new)+geom_bar(mapping = aes(x = emp_length))


# visualizing Y variable
ggplot(data = data_new)+geom_bar(mapping = aes(x = coFlag))
ggplot(data_new, mapping= aes(coFlag))+geom_bar(mapping = aes(color= loan_status))

# grades in each coFlag
ggplot(data_new, mapping= aes(coFlag))+geom_bar(mapping = aes(color = grade))

# employment length in each coFlag
ggplot(data_new, mapping= aes(coFlag))+geom_bar(mapping = aes(color = emp_length))

# home ownership in each coFlag
ggplot(data_new, mapping= aes(coFlag))+geom_bar(mapping = aes(color = home_ownership))

# purpose in each coFlag
ggplot(data_new, mapping= aes(coFlag))+geom_bar(mapping = aes(color = purpose))

# verification status in each coFlag
ggplot(data_new, mapping= aes(coFlag))+geom_bar(mapping = aes(color = verification_status))

# seprating continuous and categorical variables
all_vars <- unlist(lapply(data_new,class))

#continuous variable
cont_var = all_vars[all_vars=='integer' | all_vars == 'numeric']
cont_var = cont_var[-73]

#categorical variable
cat_var = all_vars[all_vars=='character']

summary(data_new[,names(data_new) %in% names(cont_var)])

# loan dollar volume by states
loan_by_state <- data_new %>% 
  group_by(addr_state) %>%
  summarize(`Total Loans ($)` = sum(loan_amnt)/1e6) %>%
  arrange(desc(`Total Loans ($)`))
colnames(loan_by_state) <- c("region","value")
top4_states <- round(100*sum(loan_by_state$value[1:4])/sum(loan_by_state$value),1)
data("state.regions")
loan_by_state$region <- sapply(loan_by_state$region, function(state_code) {
  inx <- grep(pattern = state_code, x = state.regions$abb)
  state.regions$region[inx]
})
print(state_choropleth(loan_by_state, title = "Total Loan Volume by State - Millions $"))

# grade against coFlag
ggplot(data = data_new) + geom_count(mapping = aes(x = coFlag, y = grade))

# emp length against coFlag
ggplot(data = data_new) + geom_count(mapping = aes(x = coFlag, y = emp_length))

# home ownership against coFlag
ggplot(data = data_new) + geom_count(mapping = aes(x = coFlag, y = home_ownership))

# purpose against coFlag
ggplot(data = data_new) + geom_count(mapping = aes(x = coFlag, y = purpose))

# term against coFlag
ggplot(data = data_new) + geom_count(mapping = aes(x = coFlag, y = term))

# interest rate against loan statues
ggplot(data=data_new, mapping = aes(int_rate)) + geom_freqpoly(mapping = aes(color = loan_status), binwidth =1)
