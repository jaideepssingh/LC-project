######          Lending Club Default Model - 1. EDA"          ######

# Loading required packages

library('dplyr',quietly = TRUE)
library('ggplot2',quietly = TRUE)
library('gridExtra',quietly = TRUE)
library('grid',quietly = TRUE)
library("rgdal",quietly = TRUE)
library("choroplethrMaps",quietly = TRUE)
library("choroplethr",quietly = TRUE)
library('ClustOfVar', quietly = TRUE)
library('cluster', quietly = TRUE)

# Reading data
data = read.csv("C:/Users/mua377n/Downloads/archive/Loan_status_2007-2020Q3.gzip")
#head(data,2)
data$id = as.integer(data$id)
colnames(data)

# Missing values
missing_values = (sort(apply(is.na(data),2, sum ),
                       TRUE))/nrow(data)
head(missing_values, 10)

loan_status_keep = c(unique(data$loan_status)[1],
                     unique(data$loan_status)[2],
                     unique(data$loan_status)[6],
                     unique(data$loan_status)[7],
                     unique(data$loan_status)[9],
                     unique(data$loan_status)[10])
data_new = data[data$loan_status %in% loan_status_keep,]
remove(data)

summary statistics for continuous vairables
```{r}
all_vars <- unlist(lapply(data_new,class))
custom_summary <- function(var) {
  
  if(!any(is.na(var))) {
    res <- c(summary(var),"NA's"=0)
  } else {
    res <- summary(var)
  }
  return(res)
}
cont_info <- lapply(data_new[,all_vars == "numeric"], custom_summary) 
cont_names <- names(all_vars[all_vars == "numeric"])
cont_info <- lapply(1:length(cont_info), function(inx) {
  
  new_vect <- c(cont_names[inx],round(cont_info[[inx]],2))
  
  names(new_vect)[1] <- "Var Name"
  
  new_vect
  
}); cont_info <- do.call(rbind,cont_info)
head(cont_info,10)
```

summary statistics for categorical variable
```{r}
cat_info <- lapply(1:sum(all_vars == "character"), function(inx) {
  
  Category <- data_new[,names(all_vars[all_vars == "character"])[inx]]
  
  counts_df <- data.frame(table(Category)) %>% arrange(desc(Freq))
  counts_df$Category <- as.character(counts_df$Category)
  
  if(nrow(counts_df) > 5) {
    
    counts_df$Freq[5] <- sum(counts_df$Freq[5:nrow(counts_df)])
    counts_df$Category[5] <- "Other"
    
    counts_df <- counts_df[1:5,]
  } 
  
  df <- data.frame(Name = names(all_vars[all_vars == "character"])[inx],
                   counts_df, stringsAsFactors = F)
  df$`Freq %` <- round(100*df$Freq/sum(df$Freq))
  
  df
  
}) %>% bind_rows()
head(cat_info,10)
```

dti variable cleanup
```{r}
data_new[,c("recoveries","collection_recovery_fee")] = NULL
dti_raw <- data_new$dti
dens1 <- qplot(dti_raw, fill = I("dodgerblue4"), 
               alpha = I(0.4), col = I("grey29")) + xlab("dti full range") + ylab("Count")
dens2 <- qplot(dti_raw[dti_raw > 0 & dti_raw < 15], fill = I("dodgerblue4"), 
               alpha = I(0.4), col = I("grey29")) + xlab("0 < dti < 15") + ylab("Count")
dens3 <- qplot(dti_raw[dti_raw > 0 & dti_raw < 50], fill = I("dodgerblue4"), 
               alpha = I(0.7), col = I("grey29")) + xlab("0 < dti < 50") + ylab("Count")
dens4 <- qplot(dti_raw[dti_raw > 50 & dti_raw < 9999], fill = I("dodgerblue4"), 
               alpha = I(0.4), col = I("grey29")) + xlab("50 < dti < 9999") + ylab("Count")
grid.arrange(dens1, dens2, dens3, dens4,
             top = textGrob("DTI Histograms (30 bins)"), 
             widths = c(4,4), heights = c(4,4))
data_new$dti_new[data_new$dti < 0 | data_new$dti > 50] = NA
```

```{r}
data_new$emp_title[data_new$emp_title %in% c("RN","Rn","rn","nurse","Nurse")] <- "Registered Nurse"
data_new$emp_title[is.na(data_new$emp_title)] <- "Not Available"

loan_by_emp <- data_new %>% 
  group_by(emp_title) %>% 
  summarize(`Total Loans ($)` = sum(loan_amnt)) %>%
  arrange(desc(`Total Loans ($)`))

loan_by_emp$emp_title <- paste0(loan_by_emp$emp_title," - ",paste0(round(100*loan_by_emp$`Total Loans ($)`/sum(loan_by_emp$`Total Loans ($)`),1),"%"))

loan_by_emp_plot <- ggplot(loan_by_emp[1:10,], aes(x = reorder(emp_title,-`Total Loans ($)`), y = (`Total Loans ($)`)/1e6,  fill = I("dodgerblue4"),
                                                   alpha = I(rep(0.7,10)),col = I("grey29"))) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) +xlab("Job Title - % of Total") +
  ylab("Total Loans - Millions of $")
print(loan_by_emp_plot)
```



```{r}
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
print(state_choropleth(loan_by_state, title = "           Total Loan Volume by State - Millions $"))
```

```{r}
drop_colums = c('X', 'last_credit_pull_d','last_fico_range_high','last_fico_range_low','total_pymnt',
                'total_pymnt_inv','recoveries','collection_recovery_fee','out_prncp','out_prncp_inv',
                'total_rec_prncp','total_rec_int','last_pymnt_d','last_pymnt_amnt','next_pymnt_d',
                'total_rec_late_fee','hardship_flag','hardship_amount',
                'orig_projected_additional_accrued_interest','hardship_payoff_balance_amount',
                'hardship_last_payment_amount','debt_settlement_flag','hardship_type','hardship_reason',
                'hardship_status','deferral_term','hardship_start_date','hardship_end_date','payment_plan_start_date',
                'hardship_length','hardship_dpd','hardship_loan_status','installment','pymnt_plan',                        'acc_now_delinq','url','oldest_credit_date','earliest_cr_line','last_fico_range_low','last_fico_range_high','policy_code','zip_code',
                'sec_app_fico_range_low','sec_app_fico_range_high','sec_app_earliest_cr_line','fico_range_low', 'fico_range_high',
                'annual_inc_joint','dti_joint','verification_status_joint', 'revol_bal_joint','sec_app_inq_last_6mths',
                'sec_app_mort_acc','sec_app_open_acc','sec_app_revol_util','sec_app_open_act_il',
                'sec_app_num_rev_accts','sec_app_chargeoff_within_12_mths','sec_app_collections_12_mths_ex_med')

data_clean = data_new[,!names(data_new)%in% drop_colums]
```

creating target variable coFlag
```{r}
data_clean$coFlag = ifelse (data_clean$loan_status %in% c(loan_status_keep[1], loan_status_keep[5], loan_status_keep[3]), 0, 1)
```

seperating cont and cat variables
```{r}
sapply(data_clean, is.numeric)

```

correlation heatmap
```{r}
pairs()
```

variable clustering / produces a dendogram
```{r}
tree = hclusvar(xquant, xval)
plot(tree)

```
