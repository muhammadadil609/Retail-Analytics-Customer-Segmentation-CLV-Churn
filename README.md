# Retail Analytics: Customer Segmentation, Sales Forecasting, CLV & Churn Prediction

This project uses transactional data from a UK-based online retailer to deliver actionable insights in customer segmentation, sales forecasting, lifetime value prediction, and churn modeling. The work was completed as part of my Master’s in Quantitative Management – Business Analytics at Duke University.

---

## 📘 Project Overview

The goal was to help the retailer:
- Understand and segment their customer base
- Forecast future sales
- Predict customer lifetime value (CLV)
- Identify customers at risk of churning

We used a combination of unsupervised learning, regression, and classification techniques to answer these business questions.

---

## 📦 Dataset

- **Source**: [UK Online Retail Dataset – Kaggle](https://www.kaggle.com/datasets/shashanks1202/retail-transactions-online-sales-dataset)
- **Size**: 1M+ transactions (2010–2011)
- **Key Features**: Invoice, Stock Code, Quantity, Price, InvoiceDate, Customer ID, Country

After cleaning and filtering, we focused on ~944,000 UK-based transactions.

---

## 🧹 Data Cleaning Highlights

- Removed duplicates and non-product entries
- Filled missing descriptions and handled missing customer IDs
- Capped outliers in price and quantity
- Standardized product codes and retained only UK-based data

---

## 📊 Key Analytical Workstreams

### 1. Customer Segmentation (RFM + K-Means)
- Segmented customers using Recency, Frequency, and Monetary value
- Applied K-Means clustering (K=4 based on the Elbow Method)
- Identified:
  - High-value loyal customers
  - At-risk customers
  - Moderate but engaged buyers
  - Recent big spenders

### 2. Sales Forecasting
- Built two models:
  - **Linear Regression**: R² ≈ 0.77
  - **Random Forest** with lag features: R² ≈ 0.84
- Used 5-fold cross-validation to validate performance

### 3. Customer Lifetime Value (CLV)
- Linear regression using average purchase behavior
- Achieved R² ≈ 0.98 with low error (RMSE ≈ 1892)

### 4. Churn Prediction
- Logistic regression with ridge regularization
- AUC ≈ 0.998 – highly predictive using RFM-based features

---

## 🧠 Tools & Technologies

- **Language**: R
- **Libraries**: `dplyr`, `ggplot2`, `caret`, `randomForest`, `glmnet`
- **IDE**: RStudio

---

## 📁 Files Included

- `Consolidated Code.R`: All analysis and modeling
- `Term Project Report.docx`: Final written report

---

## 💼 Business Value

- Enables more targeted marketing and retention strategies
- CLV model supports revenue forecasting and resource allocation
- Forecasting model improves inventory and campaign planning
- Churn detection allows for proactive customer outreach

---

## 👋 Final Thoughts

This project was a valuable learning experience in applying data science to real-world business challenges. It brought together modeling, segmentation, and interpretation in a way that directly connects to business decisions.

Feel free to explore the code or reach out with questions!
