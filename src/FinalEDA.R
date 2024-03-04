library(ggplot2)
library(dplyr)
library(readr)
library(plotly)
library(igraph)
library(tidyr)
library(tidyverse)
library(gridExtra)
library(tm)
library(wordcloud)
library(RColorBrewer)
TheDataset <- read_csv("TheDataset.csv")
dataset <- TheDataset

category_counts <- table(dataset$fund_category.x)
sorted_counts <- sort(category_counts, decreasing = TRUE)
top_n <- 10
if(length(sorted_counts) > top_n){
  sorted_counts <- sorted_counts[1:top_n]
}
pie(sorted_counts, main = "Distribution of Top perfoming Mutual Fund Categories", col = rainbow(length(sorted_counts)))

category_etfcounts <- table(dataset$fund_category.y)
sorted_etfcounts <- sort(category_etfcounts, decreasing = TRUE)
top_n2 <- 10
if(length(sorted_etfcounts) > top_n2){
  sorted_etfcounts <- sorted_etfcounts[1:top_n2]
}
pie(sorted_etfcounts, main = "Distribution of Top ten perfoming ETF Fund Categories", col = rainbow(length(sorted_etfcounts)))

size_counts <- dataset %>%
  group_by(size_type.x) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)
ggplot(size_counts, aes(x = "", y = percentage, fill = size_type.x)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  theme_void() +  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                                                           position = position_stack(vjust = 0.5)) +  ggtitle("Distribution of Fund Sizes")

rating_counts <- dataset %>%
  group_by(morningstar_overall_rating) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)
ggplot(rating_counts, aes(x = "", y = percentage, fill = factor(morningstar_overall_rating))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  ggtitle("Distribution of Morningstar Overall Ratings")



top_funds <- dataset %>%
  arrange(desc(fund_return_1year.x)) %>%
  top_n(round(0.1 * n()), fund_return_1year.x)

average_allocation <- top_funds %>%
  summarise(AvgStocks = mean(asset_stocks.x, na.rm = TRUE),
            AvgBonds = mean(asset_bonds.x, na.rm = TRUE),
            AvgCash = mean(asset_cash, na.rm = TRUE)) %>%
  gather('Asset', 'Percentage', AvgStocks:AvgCash)

ggplot(average_allocation, aes(x = Asset, y = Percentage, fill = Asset)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Asset Allocation in Top ten Performing Funds", x = "Asset Type", y = "Average Allocation (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")





aggregated_data <- dataset %>%
  group_by(fund_family.x) %>%
  summarise(TotalAssets = sum(total_net_assets.x, na.rm = TRUE)) %>%
  arrange(desc(TotalAssets)) %>%
  slice(1:10)  # take only the top 10 fund families
ggplot(aggregated_data, aes(x = reorder(fund_family.x, -TotalAssets), y = TotalAssets)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Top 10 Fund Families by Total Net Assets", x = "Fund Family", y = "Total Net Assets") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




top_categories <- dataset %>%
  group_by(fund_category.x) %>%
  summarise(AverageYTD = mean(year_to_date_return, na.rm = TRUE)) %>%
  top_n(10, AverageYTD) %>%
  arrange(desc(AverageYTD))
filtered_data <- dataset %>%
  inner_join(top_categories, by = "fund_category.x")
ggplot(filtered_data, aes(x = fund_category.x, y = year_to_date_return)) +
  stat_summary(fun = mean, geom = "bar") +
  labs(title = "Average Year-to-Date Return by Top 10 perfoming Fund Categories", 
       x = "Fund Category", 
       y = "Average YTD Return") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Assuming the column names are: fund_family.x and fund_annual_report_net_expense_ratio.x
top_fund_families <- dataset %>%
  group_by(fund_family.x) %>%
  summarise(TotalAssets = sum(total_net_assets.x, na.rm = TRUE)) %>%
  top_n(10, TotalAssets)

top_fund_family_data <- dataset %>%
  filter(fund_family.x %in% top_fund_families$fund_family.x)

ggplot(top_fund_family_data, aes(x = fund_family.x, y = fund_annual_report_net_expense_ratio.x)) +
  geom_boxplot() +
  labs(title = "Expense Ratios of Top 10 Fund Families by Total Net Assets",
       x = "Fund Family", y = "Expense Ratio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




ggplot(dataset, aes(x = '', y = year_to_date_return)) +
  geom_boxplot() +
  labs(title = "Boxplot of Year-to-Date Return", y = "Year-to-Date Return", x = "")

# Boxplot for Fund Annual Report Net Expense Ratio
ggplot(dataset, aes(x = '', y = fund_annual_report_net_expense_ratio.x)) +
  geom_boxplot() +
  labs(title = "Boxplot of Fund Annual Report Net Expense Ratio", y = "Expense Ratio", x = "")

# Boxplot for Morningstar Overall Rating
ggplot(dataset, aes(x = '', y = morningstar_overall_rating)) +
  geom_boxplot() +
  labs(title = "Boxplot of Morningstar Overall Rating", y = "Morningstar Rating", x = "")




selected_data <- dataset[, c('fund_sector_consumer_defensive.x', 
                             'fund_sector_energy.x', 
                             'fund_sector_financial_services.x', 
                             'fund_sector_healthcare.x', 
                             'fund_sector_industrials.x',
                             'fund_sector_technology.x')]
long_data <- gather(selected_data, key = "Sector", value = "Value")
ggplot(long_data, aes(x = Sector, y = Value)) + 
  geom_boxplot(outliers = FALSE) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Boxplots for Selected Sectors", 
       y = "Value", 
       x = "Sectors")




selected_data <- dataset[, c('fund_sector_consumer_defensive.x', 
                             'fund_sector_energy.x', 
                             'fund_sector_financial_services.x')]
long_data <- gather(selected_data, key = "Sector", value = "Value")
ggplot(long_data, aes(x = Value, fill = Sector)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~Sector, scales = "free_y", ncol = 1) + 
  labs(title = "Smooth Histograms for Selected Sectors", 
       x = "Value", 
       y = "Density")




selected_data <- dataset[, c('fund_sector_healthcare.x', 
                             'fund_sector_industrials.x',
                             'fund_sector_technology.x')]

long_data <- gather(selected_data, key = "Sector", value = "Value")

ggplot(long_data, aes(x = Value, fill = Sector)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~Sector, scales = "free_y", ncol = 1) + 
  labs(title = "Smooth Histograms for Selected Sectors", 
       x = "Value", 
       y = "Density")




long_data <- dataset %>%
  select(fund_alpha_5years.x, fund_sector_consumer_defensive.x, fund_sector_energy.x, 
         fund_sector_financial_services.x, fund_sector_healthcare.x, 
         fund_sector_industrials.x, fund_sector_real_estate.x) %>%
  gather(key = "Fund_Family", value = "Value", -fund_alpha_5years.x)

ggplot(long_data, aes(x = Value, y = fund_alpha_5years.x, color = Fund_Family)) + 
  geom_point(aes(color = Fund_Family), alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) + 
  facet_wrap(~Fund_Family, scales = "free", ncol = 2) + 
  labs(title = "Regression Plots for fund_alpha_5years.x vs. Selected Fund Families", 
       x = "Fund Family Value", 
       y = "5 Year Alpha")



long_data <- dataset %>%
  select(fund_stdev_5years.x, fund_sector_consumer_defensive.x, fund_sector_energy.x, 
         fund_sector_financial_services.x, fund_sector_healthcare.x, 
         fund_sector_industrials.x, fund_sector_real_estate.x) %>%
  gather(key = "Fund_Family", value = "Value", -fund_stdev_5years.x)

ggplot(long_data, aes(x = Value, y = fund_stdev_5years.x, color = Fund_Family)) + 
  geom_point(aes(color = Fund_Family), alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) + 
  facet_wrap(~Fund_Family, scales = "free", ncol = 2) + 
  labs(title = "Regression Plots for fund_stdev_5years.x vs. Selected Fund Families", 
       x = "Fund Family Value", 
       y = "5 Year Std. deviation")




longdata3 <- dataset %>%
  select(fund_return_ytd.x, fund_sector_consumer_defensive.x, fund_sector_energy.x, 
         fund_sector_financial_services.x, fund_sector_healthcare.x, 
         fund_sector_industrials.x, fund_sector_real_estate.x) %>%
  gather(key = "Fund_Family", value = "Value", -fund_return_ytd.x)

ggplot(longdata3, aes(x = Value, y = fund_return_ytd.x, color = Fund_Family)) + 
  geom_point(aes(color = Fund_Family), alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) + 
  facet_wrap(~Fund_Family, scales = "free", ncol = 2) + 
  labs(title = "Regression Plots for fund_return_ytd.x vs. Selected Fund Families", 
       x = "Fund Family Value", 
       y = "Fund Returns - YTD")





plot2 <- ggplot(dataset, aes(x=top10_holdings_total_assets.x, y=fund_alpha_5years.x)) + 
  geom_point(aes(color = "Data"), size = 2) +     # Scatter plot2 points
  geom_smooth(method = 'lm', color = 'black') +     # Regression line
  labs(title="Regression plot2", 
       x="Top 10 Holdings as % of Total Assets", 
       y="Fund Alpha - 5Years ", 
       color="Legend") +
  theme_minimal()

# Print the plot2
print(plot2)


filtered_dataset <- dataset %>% filter(fund_beta_5years.x > -25)
plot2 <- ggplot(dataset, aes(x=top10_holdings_total_assets.x, y=fund_beta_5years.x)) + 
  geom_point(aes(color = "Data"), size = 2) +     # Scatter plot2 points
  geom_smooth(method = 'lm', color = 'black') +     # Regression line
  labs(title="Regression plot2", 
       x="Top 10 Holdings as % of Total Assets", 
       y="Fund Beta - 5Years ", 
       color="Legend") +
  theme_minimal()

# Print the plot2
print(plot2)




plot <- ggplot(dataset, aes(x=top10_holdings_total_assets.x, y=fund_return_ytd.x)) + 
  geom_point(aes(color = "Data"), size = 2) +     # Scatter plot points
  geom_smooth(method = 'lm', color = 'black') +     # Regression line
  labs(title="Regression Plot", 
       x="Top 10 Holdings as % of Total Assets", 
       y="Fund Return YTD", 
       color="Legend") +
  theme_minimal()

# Print the plot
print(plot)



# Load necessary libraries
library(ggplot2)
library(dplyr)

# Remove outliers
filtered_dataset <- dataset %>% filter(fund_beta_5years.x > -20)

# Create the plot without outliers
plot2 <- ggplot(filtered_dataset, aes(x=top10_holdings_total_assets.x, y=fund_beta_5years.x)) + 
  geom_point(aes(color = "Data"), size = 2) +     # Scatter plot2 points
  geom_smooth(method = 'lm', color = 'black') +     # Regression line
  labs(title="Regression plot2 (without outliers)", 
       x="Top 10 Holdings as % of Total Assets", 
       y="Fund Beta - 5Years ", 
       color="Legend") +
  theme_minimal()

# Print the plot2
print(plot2)










dataset$bin_consumer_defensive <- cut(dataset$fund_sector_consumer_defensive.x, breaks = 10, labels = FALSE)

p1 <- ggplot(dataset, aes(x = bin_consumer_defensive, y = fund_sector_energy.x)) +
  geom_tile(aes(fill = fund_sector_financial_services.x), color = "black") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Relationship between Consumer Defensive & Energy with Financial Services as Fill")
print(p1)

dataset$bin_healthcare <- cut(dataset$fund_sector_healthcare.x, breaks = 10, labels = FALSE)
p2 <- ggplot(dataset, aes(x = bin_consumer_defensive, y = fund_sector_healthcare.x)) +
  geom_tile(aes(fill = fund_sector_real_estate.x), color = "black") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Relationship between Consumer Defensive & Healthcare with Real Estate as Fill")
print(p2)

dataset$bin_industrials <- cut(dataset$fund_sector_industrials.x, breaks = 10, labels = FALSE)
p3 <- ggplot(dataset, aes(x = bin_consumer_defensive, y = fund_sector_industrials.x)) +
  geom_tile(aes(fill = fund_sector_technology.x), color = "black") +
  scale_fill_gradient(low = "white", high = "green") +
  labs(title = "Relationship between Consumer Defensive & Industrials with Technology as Fill")
print(p3)

dataset$bin_financial_services <- cut(dataset$fund_sector_financial_services.x, breaks = 10, labels = FALSE)
p4 <- ggplot(dataset, aes(x = bin_financial_services, y = fund_sector_energy.x)) +
  geom_tile(aes(fill = fund_sector_utilities.x), color = "black") +
  scale_fill_gradient(low = "white", high = "purple") +
  labs(title = "Relationship between Financial Services & Energy with Utilities as Fill")
print(p4)

dataset$bin_technology <- cut(dataset$fund_sector_technology.x, breaks = 10, labels = FALSE)
p5 <- ggplot(dataset, aes(x = bin_technology, y = fund_sector_energy.x)) +
  geom_tile(aes(fill = fund_price_book_ratio.x), color = "black") +
  scale_fill_gradient(low = "white", high = "yellow") +
  labs(title = "Relationship between Technology & Energy with Price Book Ratio as Fill")
print(p5)




p1 <- ggplot() + 
  geom_density(data=dataset, aes(x=fund_sector_consumer_defensive.x, fill="Consumer Defensive"), alpha=0.5) +
  geom_density(data=dataset, aes(x=fund_sector_energy.x, fill="Energy"), alpha=0.5) +
  geom_density(data=dataset, aes(x=fund_sector_financial_services.x, fill="Financial Services"), alpha=0.5) +
  labs(title="Overlayed Density Plot for Various Sectors", x="Value", y="Density") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal()

p2 <- ggplot() + 
  geom_density(data=dataset, aes(x=fund_sector_healthcare.x, fill="Healthcare"), alpha=0.5) +
  geom_density(data=dataset, aes(x=fund_sector_industrials.x, fill="Industrials"), alpha=0.5) +
  geom_density(data=dataset, aes(x=fund_sector_technology.x, fill="Technology"), alpha=0.5) +
  labs(title="Overlayed Density Plot for Various Sectors", x="Value", y="Density") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal()

print(p1)
print(p2)
