#####################################
##                                 ##
## Variable Selection              ##
##                                 ##
#####################################

# correlation matrix
m = data_new %>% mutate_if(is.character,as.factor)
m = m%>% mutate_if (is.factor, as.numeric)
m = m[,-1]
m = cor(m, use = 'pairwise.complete.obs')
print(m)

selected_vars = NA
for (i in c(1:86)) {
  if (abs(m[i,86]) >= 5/100) {selected_vars = rbind(selected_vars,rownames(m)[i])}
}
selected_vars = selected_vars[-1]
selected_vars = selected_vars[-29]

#Correlation heat map
corrplot(m[selected_vars,selected_vars], method="color", tl.col= 'black', is.corr = FALSE)

#pairwise correlogram

# hierarchial variable clustering 

# k means clustering

# weight of evidence and information value

# chi square

