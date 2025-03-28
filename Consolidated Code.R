# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)

library(lubridate)
library(caret)
library(glmnet)
library(randomForest)


# Load the data from the Excel file
file_path <- file.choose()
df_2009_2010 <- read_excel(file_path, sheet = "Year 2009-2010")
df_2010_2011 <- read_excel(file_path, sheet = "Year 2010-2011")

# Combine both datasets for a holistic analysis
df_combined <- bind_rows(df_2009_2010, df_2010_2011)

# View the first few rows and structure of the data
head(df_combined, 10)
str(df_combined)

df_combined <- df_combined %>%
  rename(Customer_ID = `Customer ID`)


# ----- Data Checks 

missing_values_summary <- colSums(is.na(df_combined))
print("Missing values in each column:")
print(missing_values_summary)

# Invoice   StockCode Description    Quantity InvoiceDate       Price Customer ID     Country 
#   0           0        4382           0           0           0      243007           0 


# --- We can see above that Description and CustomerID have NULLS



# Check for duplicate rows
duplicate_rows <- df_combined[duplicated(df_combined), ]
print(paste("Number of duplicate rows:", nrow(duplicate_rows)))
# -- 34335

# Check for negative or zero values in important columns
# Quantity column (should be positive for normal transactions)
negative_quantity <- df_combined %>% filter(Quantity <= 0)
print(paste("Number of rows with negative or zero Quantity:", nrow(negative_quantity)))
# -- 22950

# Price column (should be positive for valid transactions)
zero_or_negative_price <- df_combined %>% filter(Price <= 0)
print(paste("Number of rows with zero or negative Price:", nrow(zero_or_negative_price)))
# -- 6207

# Check for unique and meaningful values in important columns

unique_invoices <- length(unique(df_combined$Invoice))
unique_stock_codes <- length(unique(df_combined$StockCode))
unique_customers <- length(unique(df_combined$Customer_ID, na.rm = TRUE))

print(paste("Number of unique invoices:", unique_invoices)) # -- 53628
print(paste("Number of unique stock codes:", unique_stock_codes)) # -- 5304
print(paste("Number of unique customers:", unique_customers)) # -- 5943

#Check for outliers in numerical columns using summary statistics
summary(df_combined$Quantity)
summary(df_combined$Price)

# Checking the max and min date range
# -- 2009-12-01 to 2011-12-09"

summary(df_combined)



# ---- Data Cleaning (1) -- FOR STOCKCODE
#---------------------------------------------------------------------------------------------------------------------#

#-- Function to check if the stock code rows contains only letters
contains_only_letters <- function(x) {
  grepl("^[A-Za-z]+$", x)
}

# Filter out rows where StockCode has only letters
df_combined <- df_combined %>%
  filter(!contains_only_letters(StockCode))

print(df_combined)



# ---- Data Cleaning (2) (NULL VALUE TREATMENT) -- FOR DESCRIPTION
#---------------------------------------------------------------------------------------------------------------------#
#---- Checking for NULLS in Description, since we will segment and get the Product categories as well

missing_description <- df_combined %>%
  filter(is.na(Description))

print(missing_description)

num_missing_descriptions <- nrow(missing_description)


df_combined <- df_combined %>%
  group_by(StockCode) %>%
  mutate(Description = ifelse(is.na(Description), first(Description[!is.na(Description)]), Description)) %>%
  ungroup()

# -- Rechecking the results
missing_description <- df_combined %>%
  filter(is.na(Description))

print(missing_description)

num_missing_descriptions <- nrow(missing_description)
# -- 360

# -- The above rows present do not have any other row with same StockCode, hence we will proceed with dropping these values

# -- Dropping rows with no description after treatment
df_combined <- df_combined %>%
  filter(!is.na(Description))

#---------------------------------------------------------------------------------------------------------------------#


# ---- Data Cleaning (3)  (NULL VALUE TREATMENT) -- FOR CUSTOMER ID

# Identify rows where Customer_ID is NA, but other rows with the same Invoice have a non-null Customer_ID
df_identified <- df_combined %>%
  group_by(Invoice) %>%
  filter(any(!is.na(Customer_ID)) & is.na(Customer_ID)) %>%
  ungroup()

# -- NO SUCH ROWS


# -- Since we cannot replace the Missing Cutomer ID values with Mean, median or mode as its a categorical data
# -- We are just categorizing all of them into one bucket of UNKNOWN, we will later figure out what can be done of it

df_combined <- df_combined %>%
  mutate(Customer_ID = ifelse(is.na(Customer_ID), "Unknown", Customer_ID))


missing_values_count <- colSums(is.na(df_combined))
print(missing_values_count)

# ----- No NULLS anymore

#---------------------------------------------------------------------------------------------------------------------#


# ---- Data Cleaning (4)  OUTLIER TREATMENT

summary(df_combined)


# Calculate quantiles for Price
price_quantiles <- quantile(df_combined$Price, probs = seq(0, 1, 0.05), na.rm = TRUE)  
print(price_quantiles)

# Calculate quantiles for Quantity
quantity_quantiles <- quantile(df_combined$Quantity, probs = seq(0, 1, 0.05), na.rm = TRUE)  
print(quantity_quantiles)

# -- For both Price and Quantity
# -- As we can see that we have outlier at quantile 100, while the 95% qunatile is quite a realistic number
# -- Hence we will proceed with replacing the 100th qunatile with the 95th qunatile values to avoid bias in future analysis 


# -- Replacing the 100th qunatile with 95th quantile

df_combined <- df_combined %>%
  mutate(
    Price = ifelse(Price > quantile(Price, 0.95, na.rm = TRUE), quantile(Price, 0.95, na.rm = TRUE), Price),
    Price = ifelse(Price < quantile(Price, 0.05, na.rm = TRUE), quantile(Price, 0.05, na.rm = TRUE), Price),
    
    Quantity = ifelse(Quantity > quantile(Quantity, 0.95, na.rm = TRUE), quantile(Quantity, 0.95, na.rm = TRUE), Quantity),
    Quantity = ifelse(Quantity < quantile(Quantity, 0.05, na.rm = TRUE), quantile(Quantity, 0.05, na.rm = TRUE), Quantity)
  )

summary(df_combined)

#---------------------------------------------------------------------------------------------------------------------#


# ---- Data Cleaning (5) -- MISC Data Cleaning

# -- Trimming the column
df_combined <- df_combined %>%
  mutate(Description = trimws(Description))

#---------------------------------------------------------------------------------------------------------------------#


# ---- Data Cleaning (6) -- DUPLICATE ROWS (across all columns)
# -- We may only need to treat the duplicate rows across all columns

# Find duplicated rows across all columns
duplicate_rows <- df_combined[duplicated(df_combined), ]

num_duplicates <- nrow(duplicate_rows)

# -- We have 34,272 duplicate rows (this count DOES NOT include the first occurence of the duplicate as well)

# Group by all columns to check for duplicated patterns
grouped_duplicates <- df_combined %>%
  group_by(Invoice, StockCode, Description, Quantity, InvoiceDate, Price, Customer_ID, Country) %>%
  filter(n() > 1)  

print(grouped_duplicates)

nrow(grouped_duplicates) # -- 67,117 (this count includes the first occurence of the duplicate as well)

# -- From the above we can see that we do have duplicates across all columns
# -- We will be keeping only the first occurence of the row and remove all the subsequent duplicate rows

# -- Count of total rows BEFORE removal of duplicates
nrow(df_combined) # -- 1061534


df_combined <- df_combined %>%
  distinct(.keep_all = TRUE)

# -- Count of total rows AFTER removal of duplicates
nrow(df_combined) # -- 1027262

1061534 - 1027262 # -- 34,272 DUPLICATE ROWS HAVE BEEN REMOVED

#---------------------------------------------------------------------------------------------------------------------#



# ---- Data Cleaning (7) -- REMOVING TEXT FROM STOCKCODE COLUMN

#-- We could observe that in StockCode column, to differentiate a product of same StockCode with different colours
#-- The data has concatenated a alphabet at the end of the StockCode number to differentiate. 
#-- We will try to replace the unique alphabet with a unique number so that it becomes easier to perform any numeric manipulations

df_filtered <- df_combined %>%
  filter(str_detect(StockCode, "[A-Za-z]$"))  # Ensure StockCode ends with a letter



# Process only filtered rows
df_processed <- df_filtered %>%
  mutate(
    NumericStockCode = str_extract(StockCode, "^[0-9]+")  
  ) %>%
  group_by(NumericStockCode) %>% # -- Cannot group by description since the descriptions are different
  mutate(
    RowNumber = row_number(),  
    NewStockCode = paste(NumericStockCode, RowNumber, sep="")  # if needed to highlight, use "-" 
  ) %>%
  ungroup()

print(df_processed)

# -- Handling the duplicates in the Processed data before we join the data with the combined data
df_processed <- df_processed %>%
  group_by(Invoice, StockCode) %>%
  summarize(NewStockCode = first(NewStockCode))

df_processed <- df_processed %>%
  distinct(Invoice, StockCode, .keep_all = TRUE)


# Merge the processed data -- (FOR NOW MAKING A NEW DATA FRAME. CAN BE CONFIRMED IF NEEDED)
df_combined <- df_combined %>%
  left_join(df_processed %>% select(Invoice, StockCode, NewStockCode), by = c("Invoice", "StockCode")) %>%
  mutate(
    StockCode = ifelse(!is.na(NewStockCode), NewStockCode, StockCode)  
  ) %>%
  select(-NewStockCode)  


print(df_combined)



pattern <- "([?]+|missing|lost|damages|display)"

df_combined <- df_combined %>%
  filter(!str_detect(Description, pattern) & nchar(Description) > 2)

# -- Final Check

summary(df_combined)





# ------------------------------------------------- EDA ----------------------------------------------------- #


# Price
ggplot(df_combined, aes(x = Price)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Price Distribution", x = "Price", y = "Count") +
  theme_minimal()



# Quantity
ggplot(df_combined, aes(x = Quantity)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  labs(title = "Quantity Distribution", x = "Quantity", y = "Count") +
  theme_minimal()


# -- Correlation between Price and Quantity

cor(df_combined$Price, df_combined$Quantity, use = "complete.obs")
# -- -0.3557035

ggplot(df_combined, aes(x = Quantity, y = Price)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Scatter Plot of Quantity vs. Price", x = "Quantity", y = "Price") +
  theme_minimal()



# ----------------------- COUNTRY WISE BREAKDOWN OF NUMBER OF ROWS-----------------------


country_counts <- df_combined %>%
  group_by(Country) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = (Count / sum(Count)) ) %>%
  arrange(desc(Count))

print(country_counts)

# -- Since we have 92% of data from only UK and we do not want to skew the results due to the presence of other
# -- countries data, we will filter out only the data for UK and peform operations on the same


df_combined <- df_combined %>%
  filter(Country == "United Kingdom")

nrow(df_combined) #-- 944,581




# Summarise the total sales per month
df_combined %>%
  group_by(Month = format(InvoiceDate, "%Y-%m")) %>%
  summarise(Total_Sales = sum(Price * Quantity, na.rm = TRUE)) %>%
  mutate(Month = as.Date(paste0(Month, "-01"))) %>%  # Convert Month to a Date type for continuous plotting
  ggplot(aes(x = Month, y = Total_Sales)) +
  geom_line(color = "darkblue", linewidth = 1) +  # Use linewidth instead of size
  labs(title = "Monthly Sales Trend", x = "Month", y = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate the x-axis labels
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")  # Show each month on the x-axis




# -- Create a target variable (Total Sales)
df_combined <- df_combined %>%
  mutate(Total_Sales = Price * Quantity)  # Total Sales as a new column






#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#
#-------------------CUSTOMER SEGMENTATION-------------
#------------------------------------------------------------------------------------------------------------------------------------#



library(dplyr)
library(lubridate)
library(ggplot2)
library(cluster)

# Assuming df_combined is your dataset, and it has been cleaned properly
# df_combined should have at least: Invoice, StockCode, Description, Quantity, InvoiceDate, Price, Customer_ID, Country


# Calculate RFM Metrics for each Customer

# Recency: Days since the last transaction
# Assuming the latest date in the dataset is the max InvoiceDate
max_date <- max(df_combined$InvoiceDate, na.rm = TRUE)


# ------- We only have 5,364 distinct customer IDs
distinct_customers <- df_combined %>%
  distinct(Customer_ID)

nrow(distinct_customers)
nrow(df_combined)

unknown_customer_count <- df_combined %>%
  filter(Customer_ID == "Unknown") %>%
  summarise(Count = n())

# -- We have 229,847 Unknown Customers


rfm_data <- df_combined %>%
  group_by(Customer_ID) %>%
  summarise(
    Recency = as.numeric(difftime(max_date, max(InvoiceDate), units = "days")),
    Frequency = n_distinct(Invoice), 
    Monetary = sum(Total_Sales) 
  )


# ------ Removing customers without a Customer ID since we will not be able to segment them
rfm_data <- rfm_data %>%
  filter(Customer_ID != "Unknown")


rfm_scaled <- scale(rfm_data[, -1])

rfm_weighted <- rfm_scaled %>%
  as.data.frame() %>%
  mutate(
    Weighted_Recency = Recency * 0.30,
    Weighted_Frequency = Frequency * 0.30,
    Weighted_Monetary = Monetary * 0.40
  )

rfm_weighted <- rfm_weighted %>%
  mutate(
    RFM_Score = Weighted_Recency + Weighted_Frequency + Weighted_Monetary
  )

rfm_for_clustering <- rfm_weighted %>%
  select(Weighted_Recency, Weighted_Frequency, Weighted_Monetary)



set.seed(123)
wss <- sapply(1:10, function(k){
  kmeans(rfm_for_clustering, centers = k, nstart = 25)$tot.withinss
})

plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters (K)", ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method for Optimal K")

set.seed(123)
kmeans_result <- kmeans(rfm_for_clustering, centers = 4, nstart = 25) # --- Going with 4 to get maximum customers mapped

rfm_data$Cluster <- kmeans_result$cluster

ggplot(rfm_data, aes(x = Recency, y = Monetary, color = as.factor(Cluster))) +
  geom_point(size = 3, alpha = 0.6) +
  labs(title = "Customer Segments (K-Means Clustering)", x = "Recency (Days)", y = "Monetary (Total Sales)") +
  theme_minimal()

cluster_summary <- rfm_data %>%
  group_by(Cluster) %>%
  summarise(
    Avg_Recency = mean(Recency),
    Avg_Frequency = mean(Frequency),
    Avg_Monetary = mean(Monetary),
    Count = n()
  )

print(cluster_summary)

#   Cluster Avg_Recency Avg_Frequency Avg_Monetary Count
#1       1        28.5         32.5         9742.   389
#2       2       470.           2.51         533.  1784
#3       3        73.7          6.09        1429.  3163
#4       4        26.2        131.         49707.    27


# ---- Out of 5,363 distinct customers, we have segmented 5,363 customers



# --- Integrating the data into the df_combined data set

df_combined_with_clusters <- df_combined %>%
  left_join(rfm_data %>% select(Customer_ID, Cluster), by = "Customer_ID")

head(df_combined_with_clusters)

summary(df_combined_with_clusters$Cluster)

sum(is.na(df_combined_with_clusters$Cluster)) # --- 229,847 Unsegmented customers


missing_in_rfm <- df_combined %>%
  filter(!Customer_ID %in% rfm_data$Customer_ID)


null_customers <- df_combined_with_clusters %>%
  filter((Customer_ID == "Unknown") & is.na(Cluster))

nrow(null_customers) # -- 229,847

229847 - 229847 # -- these many customers have not gotten segmented 0




#------ Add a new column for the Cluster Name based on the cluster number
df_combined_with_clusters <- df_combined_with_clusters %>%
  mutate(Cluster_Name = case_when(
    Cluster == 1 ~ "Engaged and High-Spenders",
    Cluster == 2 ~ "At-Risk Customers",
    Cluster == 3 ~ "Potential Loyal Customers",
    Cluster == 4 ~ "Recently High Spending Customers"
  ))


head(df_combined_with_clusters)








#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#
#-------------------SALES FORECASTING-------------
#------------------------------------------------------------------------------------------------------------------------------------#


df_combined_with_clusters$InvoiceDate <- as.POSIXct(df_combined_with_clusters$InvoiceDate, format="%Y-%m-%d %H:%M:%S")

# extract year, month, week, and day
df_combined_with_clusters$Year <- year(df_combined_with_clusters$InvoiceDate)
df_combined_with_clusters$Month <- month(df_combined_with_clusters$InvoiceDate)
df_combined_with_clusters$Week <- isoweek(df_combined_with_clusters$InvoiceDate)
df_combined_with_clusters$Day <- day(df_combined_with_clusters$InvoiceDate)

df_combined_with_clusters$Year_Week <- df_combined_with_clusters$Year * 100 + df_combined_with_clusters$Week  



#-------------------------

#----- Since we are seeing a lot of rows where clusters are null, we are trying the below process
# --- We will assign clusters based on the ratio, hence the overall ratio of clusters will still stay the same

sum(is.na(df_combined_with_clusters$Cluster)) #-- 229575

cluster_proportions <- df_combined_with_clusters %>%
  filter(!is.na(Cluster)) %>%
  group_by(Cluster) %>%
  summarise(Count = n()) %>%
  mutate(Cluster_Proportion = Count / sum(Count))

print(cluster_proportions)
#Cluster  Count Cluster_Proportion
#<int>  <int>              <dbl>
#  1       1 222687             0.312 
#2       2  73262             0.103 
#3       3 350825             0.491 
#4       4  67960             0.0951



set.seed(42) 
missing_cluster_rows <- is.na(df_combined_with_clusters$Cluster)

cluster_proportions$Cluster <- as.character(cluster_proportions$Cluster)

# Sample clusters for missing values based on the calculated proportions
df_combined_with_clusters$Cluster[missing_cluster_rows] <- sample(
  cluster_proportions$Cluster, 
  size = sum(missing_cluster_rows), 
  replace = TRUE, 
  prob = cluster_proportions$Cluster_Proportion
)

sum(is.na(df_combined_with_clusters$Cluster)) # -- 0

#-------------------------



# Aggregate data at the week level
df_weekly_aggregated <- df_combined_with_clusters %>%
  group_by(Year, Week, Cluster) %>%
  summarise(Total_Sales = sum(Total_Sales, na.rm = TRUE), 
            Quantity = sum(Quantity, na.rm = TRUE),
            Price = mean(Price, na.rm = TRUE))

df_weekly_aggregated <- df_weekly_aggregated %>%
  arrange(Year, Week)

df_weekly_aggregated$Year_Week <- df_weekly_aggregated$Year * 100 + df_weekly_aggregated$Week  # Creates a Year_Week indicator


# --- Adding Month Column
df_weekly_aggregated <- df_weekly_aggregated %>%
  left_join(
    df_combined_with_clusters %>%
      group_by(Year, Week, Cluster) %>% 
      summarise(Month = first(Month), .groups = "drop"), 
    by = c("Year", "Week", "Cluster")
  )



split_year_week <- quantile(df_weekly_aggregated$Year_Week, probs = 0.8)
# -- 201129

trainData <- df_weekly_aggregated[df_weekly_aggregated$Year_Week <= split_year_week, ]
testData  <- df_weekly_aggregated[df_weekly_aggregated$Year_Week > split_year_week, ]


#--------------------- LINEAR REGRESSION

model_lr <- lm(Total_Sales ~  Year + Month + Week  + Cluster, data = trainData)

summary(model_lr)

predictions_lr <- predict(model_lr, newdata = testData)

rmse_manual <- sqrt(mean((testData$Total_Sales - predictions_lr)^2, na.rm = TRUE))

mae_manual <- mean(abs(testData$Total_Sales - predictions_lr), na.rm = TRUE)

ss_total <- sum((testData$Total_Sales - mean(testData$Total_Sales, na.rm = TRUE))^2, na.rm = TRUE)  
ss_residual <- sum((testData$Total_Sales - predictions_lr)^2, na.rm = TRUE)  
r2_manual <- 1 - (ss_residual / ss_total)

cat("RMSE: ", rmse_manual, "\n") # -- RMSE:  24187.66 
cat("MAE: ", mae_manual, "\n") # -- MAE:  16706.41 
cat("R-squared: ", r2_manual, "\n") # --- R-squared:  0.5389421






#---------------------IMPROVED LINEAR REGRESSION

# --- Adding lag values at a week level

df_weekly_aggregated <- df_weekly_aggregated %>%
  arrange(Year, Week)

df_weekly_aggregated <- df_weekly_aggregated %>%
  group_by(Cluster) %>%
  mutate(Total_Sales_Lag_1 = lag(Total_Sales, 1),   # Lag by 1 week within each Cluster
         Total_Sales_Lag_4 = lag(Total_Sales, 4),   # Lag by 4 weeks within each Cluster
         Total_Sales_Lag_52 = lag(Total_Sales, 52)) # Lag by 52 weeks within each Cluster


df_weekly_aggregated <- df_weekly_aggregated %>%
  ungroup()

summary(df_weekly_aggregated$Total_Sales_Lag_1)
summary(df_weekly_aggregated$Total_Sales_Lag_4)
summary(df_weekly_aggregated$Total_Sales_Lag_52)



split_year_week <- quantile(df_weekly_aggregated$Year_Week, probs = 0.8)


trainData <- df_weekly_aggregated[df_weekly_aggregated$Year_Week <= split_year_week, ]
testData  <- df_weekly_aggregated[df_weekly_aggregated$Year_Week > split_year_week, ]




lr_model_improved <- lm(Total_Sales ~ Year + Month + Week + Cluster + 
                          Year * Cluster + Month * Cluster + Week * Cluster +
                          Total_Sales_Lag_1 + Total_Sales_Lag_4 + Total_Sales_Lag_52, 
                        data = trainData)


summary(lr_model_improved)

predictions_improved_lr <- predict(lr_model_improved, newdata = testData)

rmse_improved_lr <- sqrt(mean((testData$Total_Sales - predictions_improved_lr)^2, na.rm = TRUE))
mae_improved_lr <- mean(abs(testData$Total_Sales - predictions_improved_lr), na.rm = TRUE)
ss_total_enhanced <- sum((testData$Total_Sales - mean(testData$Total_Sales, na.rm = TRUE))^2, na.rm = TRUE)
ss_residual_enhanced <- sum((testData$Total_Sales - predictions_improved_lr)^2, na.rm = TRUE)
r2_improved_lr <- 1 - (ss_residual_enhanced / ss_total_enhanced)

# Output the results
cat("Enhanced RMSE: ", rmse_improved_lr, "\n")
# -- Enhanced RMSE:  17015.5, 16136.95, 15084.66

cat("Enhanced MAE: ", mae_improved_lr, "\n")
# -- Enhanced MAE:  11186.68, 10289.57, 10179.06

cat("Enhanced R-squared: ", r2_improved_lr, "\n")
# -- Enhanced R-squared:  0.7718308, 0.7947843, 0.820676


plot_data <- data.frame(
  Actual = testData$Total_Sales,
  Predicted = predictions_improved_lr
)


ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "blue") + 
  labs(title = "Actual vs Predicted Sales", x = "Actual Sales", y = "Predicted Sales") +
  theme_minimal()







#---------------------RANDOM FOREST

#--------------Imputing the NAs present in the LAG columns with the corresponding TOTAL_SALES values

trainData_imputed <- trainData %>%
  mutate(Total_Sales_Lag_1 = ifelse(is.na(Total_Sales_Lag_1), Total_Sales, Total_Sales_Lag_1),
         Total_Sales_Lag_4 = ifelse(is.na(Total_Sales_Lag_4), Total_Sales, Total_Sales_Lag_4),
         Total_Sales_Lag_52 = ifelse(is.na(Total_Sales_Lag_52), Total_Sales, Total_Sales_Lag_52))

testData_imputed <- testData %>%
  mutate(Total_Sales_Lag_1 = ifelse(is.na(Total_Sales_Lag_1), Total_Sales, Total_Sales_Lag_1),
         Total_Sales_Lag_4 = ifelse(is.na(Total_Sales_Lag_4), Total_Sales, Total_Sales_Lag_4),
         Total_Sales_Lag_52 = ifelse(is.na(Total_Sales_Lag_52), Total_Sales, Total_Sales_Lag_52))




# --------After removing the NA values, we were enfing up working with only half of the data, hence imputing the data as above

set.seed(42)

rf_model <- randomForest(Total_Sales ~ Year + Month + Week + Cluster + 
                           Total_Sales_Lag_1 + Total_Sales_Lag_4 + Total_Sales_Lag_52, 
                         data = trainData_imputed, importance = TRUE)


predictions_rf <- predict(rf_model, newdata = testData_imputed)

rmse_rf <- sqrt(mean((testData_imputed$Total_Sales - predictions_rf)^2, na.rm = TRUE))
mae_rf <- mean(abs(testData_imputed$Total_Sales - predictions_rf), na.rm = TRUE)
ss_total_rf <- sum((testData_imputed$Total_Sales - mean(testData_imputed$Total_Sales, na.rm = TRUE))^2, na.rm = TRUE)
ss_residual_rf <- sum((testData_imputed$Total_Sales - predictions_rf)^2, na.rm = TRUE)
r2_rf <- 1 - (ss_residual_rf / ss_total_rf)


cat("Random Forest RMSE: ", rmse_rf, "\n")
# ---- Random Forest RMSE:  14662.1 

cat("Random Forest MAE: ", mae_rf, "\n")
# ---- Random Forest MAE:  10250.06

cat("Random Forest R-squared: ", r2_rf, "\n")
# ---- Random Forest R-squared:  0.8305817 


plot_data_rf <- data.frame(
  Actual = testData$Total_Sales,
  Predicted = predictions_rf
)

ggplot(plot_data_rf, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "blue") +  
  labs(title = "Random Forest: Actual vs Predicted Sales", x = "Actual Sales", y = "Predicted Sales") +
  theme_minimal()






#---------------------RANDOM FOREST WITH K FOLD CROSS VALIDATION



set.seed(42)  

train_control <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation


rf_model_cv <- train(Total_Sales ~ Year + Month + Week + Cluster + 
                       Total_Sales_Lag_1 + Total_Sales_Lag_4 + Total_Sales_Lag_52,
                     data = trainData_imputed,
                     method = "rf",
                     trControl = train_control)


predictions_rf_cv <- predict(rf_model_cv, newdata = testData_imputed)


rmse_rf_cv <- sqrt(mean((testData_imputed$Total_Sales - predictions_rf_cv)^2, na.rm = TRUE))
mae_rf_cv <- mean(abs(testData_imputed$Total_Sales - predictions_rf_cv), na.rm = TRUE)
ss_total_rf_cv <- sum((testData_imputed$Total_Sales - mean(testData_imputed$Total_Sales, na.rm = TRUE))^2, na.rm = TRUE)
ss_residual_rf_cv <- sum((testData_imputed$Total_Sales - predictions_rf_cv)^2, na.rm = TRUE)
r2_rf_cv <- 1 - (ss_residual_rf_cv / ss_total_rf_cv)


cat("Random Forest with CV RMSE: ", rmse_rf_cv, "\n")
#-- Random Forest with CV RMSE:  14081.25 

cat("Random Forest with CV MAE: ", mae_rf_cv, "\n")
#-- Random Forest with CV MAE:  9589.029 

cat("Random Forest with CV R-squared: ", r2_rf_cv, "\n")
#-- Random Forest with CV R-squared:  0.8437391 



plot_data_rf_cv <- data.frame(
  Actual = testData_imputed$Total_Sales,
  Predicted = predictions_rf_cv
)

ggplot(plot_data_rf_cv, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "blue") +  # Perfect prediction line
  labs(title = "Random Forest with K-Fold CV: Actual vs Predicted Sales", x = "Actual Sales", y = "Predicted Sales") +
  theme_minimal()








# Combine the training and test data into one dataset
full_data <- rbind(trainData_imputed, testData_imputed)

# Train the Random Forest model on the entire dataset
set.seed(42)

train_control <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

rf_model_full <- train(Total_Sales ~ Year + Month + Week + Cluster + 
                         Total_Sales_Lag_1 + Total_Sales_Lag_4 + Total_Sales_Lag_52,
                       data = full_data,
                       method = "rf",
                       trControl = train_control)

# Create future data for the next year (52 weeks)
latest_year_week <- max(full_data$Year_Week)
latest_year <- max(full_data$Year)
latest_week <- max(full_data$Week)

future_weeks <- (latest_week + 1):(latest_week + 52)
future_weeks <- (future_weeks - 1) %% 52 + 1
future_years <- latest_year + ((latest_week + 1):(latest_week + 52) - 1) %/% 52

future_year_week <- future_years * 100 + future_weeks
future_data <- expand.grid(
  Year = future_years,
  Month = rep(1:12, length.out = length(future_weeks)),
  Week = future_weeks,
  Cluster = unique(full_data$Cluster)
)

future_data$Year_Week <- rep(future_year_week, length(unique(full_data$Cluster)))

# Add lag features to future data based on the latest available historical data
for (cluster in unique(full_data$Cluster)) {
  cluster_data <- full_data[full_data$Cluster == cluster, ]
  last_sales <- tail(cluster_data$Total_Sales, n = 52)
  
  future_data[future_data$Cluster == cluster, "Total_Sales_Lag_1"] <- tail(last_sales, 1)
  future_data[future_data$Cluster == cluster, "Total_Sales_Lag_4"] <- tail(last_sales, 4)[1]
  future_data[future_data$Cluster == cluster, "Total_Sales_Lag_52"] <- tail(last_sales, 52)[1]
}

# Predict sales for the next 52 weeks
predictions_future_sales <- predict(rf_model_full, newdata = future_data)

# Add predicted sales to the future data
future_data$Predicted_Sales <- predictions_future_sales

# Combine historical data and future predictions for plotting
full_data_combined <- rbind(
  full_data %>% select(Year_Week, Total_Sales) %>% mutate(Source = "Actual"),
  future_data %>% select(Year_Week, Predicted_Sales) %>% rename(Total_Sales = Predicted_Sales) %>% mutate(Source = "Predicted")
)

# Plot the combined historical and future sales
ggplot(full_data_combined, aes(x = Year_Week, y = Total_Sales, color = Source, group = Source)) +
  geom_line(size = 1) +
  labs(title = "Total Sales at Week Level: Actual vs Predicted",
       x = "Year-Week",
       y = "Total Sales") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#
#-------------------CUSTOMER LIFETIME VALUE-------------
#------------------------------------------------------------------------------------------------------------------------------------#




# Define the training period and future period for evaluation
train_period_end <- as.POSIXct("2010-12-31")  
future_period_end <- as.POSIXct("2011-12-31")


trainData <- df_combined_with_clusters %>%
  filter(InvoiceDate <= train_period_end) %>%
  group_by(Customer_ID) %>%
  summarise(
    Total_Sales = sum(Total_Sales, na.rm = TRUE),
    Num_Purchases = n(),
    Recency = max(InvoiceDate),
    Avg_Price = mean(Price, na.rm = TRUE),
    Avg_Quantity = mean(Quantity, na.rm = TRUE)
  )

# Ensure Recency is converted to the time since the last purchase
trainData$Recency <- as.numeric(difftime(train_period_end, trainData$Recency, units = "days"))


lr_model_clv <- lm(Total_Sales ~ Num_Purchases + Recency + Avg_Price + Avg_Quantity, data = trainData)

# Future data: aggregate data AFTER the training period for evaluation
futureData <- df_combined_with_clusters %>%
  filter(InvoiceDate > train_period_end & InvoiceDate <= future_period_end) %>%
  group_by(Customer_ID) %>%
  summarise(
    Actual_Future_Sales = sum(Total_Sales, na.rm = TRUE),  # This is the actual value you're predicting
    Num_Purchases = n(),  # Number of purchases in the future period
    Recency = max(InvoiceDate),  # Time since the last purchase in the future period
    Avg_Price = mean(Price, na.rm = TRUE),
    Avg_Quantity = mean(Quantity, na.rm = TRUE)
  )


futureData$Recency <- as.numeric(difftime(future_period_end, futureData$Recency, units = "days"))


futureData$Predicted_CLV <- predict(lr_model_clv, newdata = futureData)


rmse_future <- sqrt(mean((futureData$Actual_Future_Sales - futureData$Predicted_CLV)^2, na.rm = TRUE))
mae_future <- mean(abs(futureData$Actual_Future_Sales - futureData$Predicted_CLV), na.rm = TRUE)


ss_total <- sum((futureData$Actual_Future_Sales - mean(futureData$Actual_Future_Sales, na.rm = TRUE))^2, na.rm = TRUE)
ss_residual <- sum((futureData$Actual_Future_Sales - futureData$Predicted_CLV)^2, na.rm = TRUE)
r2_future <- 1 - (ss_residual / ss_total)



cat("RMSE for CLV prediction: ", rmse_future, "\n")
#--- RMSE for CLV prediction:  1892.282 

cat("MAE for CLV prediction: ", mae_future, "\n")
#--- MAE for CLV prediction:  762.2938 

cat("R-squared for CLV prediction: ", r2_future, "\n")
#--- R-squared for CLV prediction:  0.9877187 



plot_data <- data.frame(
  Actual = futureData$Actual_Future_Sales,
  Predicted = futureData$Predicted_CLV
)

ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "blue") +
  labs(title = "CLV Prediction: Actual vs Predicted", x = "Actual Future Sales", y = "Predicted CLV") +
  theme_minimal()






#------------------------PREDICTING USING FULL DATA


str(df_combined_with_clusters)


df_combined_with_clusters <- df_combined_with_clusters %>%
  mutate(InvoiceDate = as.POSIXct(InvoiceDate, format = "%Y-%m-%d"))


missing_dates <- sum(is.na(df_combined_with_clusters$InvoiceDate))
cat("Number of missing InvoiceDate values:", missing_dates, "\n")


df_combined_with_clusters <- df_combined_with_clusters %>%
  filter(!is.na(InvoiceDate))


max_date <- max(df_combined_with_clusters$InvoiceDate)
cat("Latest date in the dataset (numeric timestamp):", max_date, "\n")


max_date <- as.POSIXct(max_date, origin = "1970-01-01", tz = "UTC")
cat("Latest date in the dataset (human-readable):", max_date, "\n")


full_data <- df_combined_with_clusters %>%
  group_by(Customer_ID) %>%
  summarise(
    Total_Sales = sum(Total_Sales, na.rm = TRUE),
    Num_Purchases = n(),
    Recency = as.numeric(difftime(max(max_date), max(InvoiceDate), units = "days")),  # Time since last purchase
    Avg_Price = mean(Price, na.rm = TRUE),
    Avg_Quantity = mean(Quantity, na.rm = TRUE)
  )


str(full_data)


lr_model_full_clv <- lm(Total_Sales ~ Num_Purchases + Recency + Avg_Price + Avg_Quantity, data = full_data)

summary(lr_model_full_clv)

# Future data for prediction: the next year (similar to sales forecasting approach)
future_year_data <- df_combined_with_clusters %>%
  filter(InvoiceDate > max_date & InvoiceDate <= max_date + years(1)) %>%
  group_by(Customer_ID) %>%
  summarise(
    Num_Purchases = n(),
    Recency = max(InvoiceDate),
    Avg_Price = mean(Price, na.rm = TRUE),
    Avg_Quantity = mean(Quantity, na.rm = TRUE)
  )


future_year_data$Recency <- as.numeric(difftime(max_date + years(1), future_year_data$Recency, units = "days"))


future_year_data$Predicted_CLV <- predict(lr_model_full_clv, newdata = future_year_data)


df_combined_with_clusters <- df_combined_with_clusters %>%
  left_join(future_year_data %>% select(Customer_ID, Predicted_CLV), by = "Customer_ID")


print(df_combined_with_clusters)







#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#
#-------------------CUSTOMER CHURN-------------
#------------------------------------------------------------------------------------------------------------------------------------#


library(caret)
library(pROC)

#------- ANALYSIS BEFORE MOVING INTO CHURN -----------#

max_date <- max(df_combined$InvoiceDate, na.rm = TRUE)

customer_recency <- df_combined %>%
  group_by(Customer_ID) %>%
  summarise(Last_Purchase_Date = max(InvoiceDate),
            Recency = as.numeric(difftime(max_date, max(InvoiceDate), units = "days"))) %>%
  ungroup()


customer_recency <- customer_recency %>%
  mutate(Inactivity_Period = case_when(
    Recency <= 90 ~ "0-3 months",
    Recency <= 180 ~ "3-6 months",
    Recency <= 270 ~ "6-9 months",
    Recency <= 365 ~ "9-12 months",
    Recency > 365 ~ "More than 1 year"
  ))


inactivity_summary <- customer_recency %>%
  group_by(Inactivity_Period) %>%
  summarise(Num_Customers = n())

print(inactivity_summary)
#  Inactivity_Period Num_Customers
#1 0-3 months                 2636
#2 3-6 months                  527
#3 6-9 months                  436
#4 9-12 months                 284
#5 More than 1 year           1481





#------------------------------------

cutoff_date <- max(df_combined_with_clusters$InvoiceDate) - months(3)
#-- "2011-09-09 12:49:00 UTC"




# Assumption -  customers who haven't purchased in the last 3 months have churned
cutoff_date <- as.Date("2011-09-09")  

df_combined_with_churn <- df_combined_with_clusters %>%
  group_by(Customer_ID) %>%
  summarise(
    Last_Purchase_Date = max(InvoiceDate),
    Recency = as.numeric(difftime(cutoff_date, max(InvoiceDate), units = "days")),  # Correct Recency
    Frequency = n_distinct(Invoice),
    Monetary = sum(Total_Sales),
    Churn = ifelse(max(InvoiceDate) <= cutoff_date, 1, 0)  # 1 = Churn, 0 = Not churned
  )

# Ensure Recency is converted to the time since the last purchase
# df_combined_with_churn$Recency <- as.numeric(difftime(cutoff_date, df_combined_with_churn$Recency, units = "days"))


df_combined_with_churn <- df_combined_with_churn %>%
  filter(Customer_ID != "Unknown")


df_combined_with_churn <- df_combined_with_churn %>%
  mutate(
    Recency = scale(Recency),        
    Frequency = scale(Frequency),     
    Monetary = scale(Monetary)        
  )

set.seed(123)
train_index <- createDataPartition(df_combined_with_churn$Churn, p = 0.8, list = FALSE)
train_data <- df_combined_with_churn[train_index, ]
test_data <- df_combined_with_churn[-train_index, ]



#------------------------- LOGISTIC REGRESSION -------------------------------#


logistic_model <- glm(Churn ~ Recency + Frequency + Monetary, data = train_data, family = binomial)

summary(logistic_model)


test_data$Churn_Prob <- predict(logistic_model, newdata = test_data, type = "response")

test_data$Predicted_Churn <- ifelse(test_data$Churn_Prob > 0.5, 1, 0)

conf_matrix <- confusionMatrix(as.factor(test_data$Predicted_Churn), as.factor(test_data$Churn))
print(conf_matrix)
#Reference
#Prediction   0   1
#0 512   3
#1   1 556


head(test_data %>% select(Customer_ID, Churn_Prob, Predicted_Churn))



roc_curve <- roc(test_data$Churn, test_data$Churn_Prob)
auc(roc_curve)  # Area under the curve: 1
plot(roc_curve, main = "ROC Curve for Churn Prediction")


#---------IMPORTANT CALL OUT:

# We understand that the AUC being 1 is unrealistic, however we suspect that the reason behind it is
# that we are removing the rows with NULL Customer_IDs which removes majority of our data
# With this smaller sample we then split the customers into 80-20 for train and test
# This even smaller train and test is one of the reason for the extremely accurate results
# Again, this would have given better results if we had more customer related columns which could have potentially provided insights


table(df_combined_with_churn$Churn)





#-----------------------------CHURN WITH RIDGE TO MINIMIZE THE OVERFITTING

X_train <- model.matrix(Churn ~ Recency + Frequency + Monetary, data = train_data)[, -1]  # Exclude intercept
y_train <- train_data$Churn


set.seed(123)
cv_model <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 0)  


cat("Best Lambda (Regularization Strength):", cv_model$lambda.min, "\n")


X_test <- model.matrix(Churn ~ Recency + Frequency + Monetary, data = test_data)[, -1]

summary(X_test)

test_data$Churn_Prob <- predict(cv_model, newx = X_test, type = "response", s = "lambda.min")


test_data$Predicted_Churn <- ifelse(test_data$Churn_Prob > 0.5, 1, 0)

# Confusion Matrix and Model Evaluation
conf_matrix <- confusionMatrix(as.factor(test_data$Predicted_Churn), as.factor(test_data$Churn))
print(conf_matrix)

#Reference
#Prediction   0   1
#0 513  79
#1   0 480

# AUC and ROC curve
roc_curve <- roc(test_data$Churn, test_data$Churn_Prob)
auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n") # -- AUC: 0.9983192 
plot(roc_curve, main = "ROC Curve for Churn Prediction with Ridge Regularization")








#------------------- TRAINING ON THE WHOLE DATASET





cutoff_date <- as.Date("2011-09-09")  

# Create churn flag and key variables: Recency, Frequency, Monetary
df_combined_with_churn <- df_combined_with_clusters %>%
  group_by(Customer_ID) %>%
  summarise(
    Last_Purchase_Date = max(InvoiceDate),
    Recency = as.numeric(difftime(cutoff_date, max(InvoiceDate), units = "days")),  # Correct Recency
    Frequency = n_distinct(Invoice),
    Monetary = sum(Total_Sales),
    Churn = ifelse(max(InvoiceDate) <= cutoff_date, 1, 0)  # 1 = Churn, 0 = Not churned
  )

# Remove customers with unknown IDs
df_combined_with_churn <- df_combined_with_churn %>%
  filter(Customer_ID != "Unknown")

df_combined_with_churn <- df_combined_with_churn %>%
  mutate(
    Recency = scale(Recency),        
    Frequency = scale(Frequency),     
    Monetary = scale(Monetary)        
  )

X_full <- model.matrix(Churn ~ Recency + Frequency + Monetary, data = df_combined_with_churn)[, -1]
y_full <- df_combined_with_churn$Churn


# Fit Ridge logistic regression on the entire dataset
set.seed(123)
full_model <- glmnet(X_full, y_full, family = "binomial", alpha = 0, lambda = cv_model$lambda.min)



df_combined_with_churn$Churn_Prob <- predict(full_model, newx = X_full, type = "response")




#---------------------TO PREDICT FOR FUTURE USING SIMULATED DATA


future_data_churn <- data.frame(
  Customer_ID = paste0("Future_Customer_", 1:100000),
  Recency = sample(30:800, 100000, replace = TRUE),  
  Frequency = sample(1:20, 100000, replace = TRUE),  
  Monetary = sample(100:20000, 100000, replace = TRUE)
)


future_data_churn <- future_data_churn %>%
  mutate(
    Recency = scale(Recency),
    Frequency = scale(Frequency),
    Monetary = scale(Monetary)
  )


X_future <- model.matrix(~ Recency + Frequency + Monetary, data = future_data_churn)[, -1]
future_data_churn$Churn_Prob <- predict(full_model, newx = X_future, type = "response")


future_data_churn$Predicted_Churn <- ifelse(future_data_churn$Churn_Prob > 0.5, 1, 0)


head(future_data_churn %>% select(Customer_ID, Recency, Frequency, Monetary, Churn_Prob, Predicted_Churn))





avg_churn_original <- mean(df_combined_with_churn$Churn_Prob, na.rm = TRUE)
avg_churn_future <- mean(future_data_churn$Churn_Prob, na.rm = TRUE)

cat("Average Churn Probability - Original Data: ", avg_churn_original, "\n")
# --- Average Churn Probability - Original Data:  0.5071788 

cat("Average Churn Probability - Future Data: ", avg_churn_future, "\n")
# --- Average Churn Probability - Future Data:  0.5487214

# Compare the distribution of churn probabilities
ggplot() +
  geom_density(aes(x = df_combined_with_churn$Churn_Prob, color = "Original Data"), size = 1) +
  geom_density(aes(x = future_data_churn$Churn_Prob, color = "Future Data"), size = 1) +
  labs(title = "Churn Probability Distribution: Original vs Future Data", x = "Churn Probability", y = "Density") +
  scale_color_manual(values = c("Original Data" = "blue", "Future Data" = "red")) +
  theme_minimal()
