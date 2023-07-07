


# Plot simulated prevalence through time:

ggplot() +
  geom_line(data=simDat, aes(x=time, y=P))+
  geom_point(data= myDat, aes(x=time, y=sampPrev)) + 
  geom_errorbar(data= myDat, aes(x=time, ymin=lci, ymax=uci)
  ) +
  theme_bw() +
  labs(y="Prevalence", x="Days")



list_of_MLE_fits 

# Create an empty data frame with column names
# Column names for the data frame
col_names <- c("sampling", "beta_ratio", "alpha_ratio")
mle_plot_df <- data.frame(matrix(NA, ncol = length(col_names), nrow = 3))
colnames(mle_plot_df) <- col_names
mle_plot_df$sampling <- c("Every_Year", "Every10Years", "Every20Years")

df1 <- list_of_MLE_fits

count =1
for (df in list_of_MLE_fits){
  # beta ratio
  beta_ratio = (df[['Beta']] - df[['MLE_beta']])/ df[['Beta']]
  mle_plot_df[['beta_ratio']][[count]] <- beta_ratio
  
  #alpha_ratio
  alpha_ratio = (df[['alpha']] - df[['MLE_alpha']])/ df[['alpha']]
  mle_plot_df[['alpha_ratio']][[count]] <- alpha_ratio
  
  count = count+1
}

library(tidyr)

long_df <- gather(mle_plot_df, beta_ratio, value, beta_ratio:alpha_ratio, factor_key=TRUE)

  
ggplot(long_df, aes(x=beta_ratio, y= sampling, fill= value))+
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)






