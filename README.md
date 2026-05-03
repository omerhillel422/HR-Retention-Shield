# HR-Retention-Shield
A machine learning-based Decision Support System (DSS) to predict employee attrition and provide proactive retention strategies.

# Employee Attrition Prediction & Decision Support System (DSS)

## Overview
This project addresses the strategic challenge of **Employee Attrition** using Machine Learning. It provides a data-driven approach to identify employees at high risk of leaving and suggests proactive retention strategies. The project concludes with the implementation of the **'HR Retention Shield'**, a decision support tool for HR managers.

## Key Features
* [cite_start]**Predictive Modeling:** Comparison of three algorithms: Logistic Regression, Decision Trees, and Random Forest[cite: 202].
* [cite_start]**Feature Engineering:** Developed custom metrics such as `LoyaltyRatio` (Tenure vs. Total Experience) and `TotalSatisfaction`[cite: 147, 148, 152].
* [cite_start]**Business Logic:** Implemented a RAG (Red-Amber-Green) risk model that translates probability scores into operational recommendations[cite: 233, 245].
* [cite_start]**High Accuracy:** The final Logistic Regression model achieved an **88.4% accuracy rate**[cite: 219, 267].

## Tech Stack
* **Language:** R
* [cite_start]**Libraries:** `tidyverse` (Data manipulation), `caret` (ML framework), `ggplot2` (Visualization), `randomForest`, `rpart`[cite: 165, 201].

## Insights from Data (EDA)
* [cite_start]**Overtime as a Critical Factor:** Employees working overtime show a significantly higher attrition rate[cite: 174, 252].
* [cite_start]**Income Sensitivity:** Lower monthly income strongly correlates with a higher probability of leaving, especially for junior-to-mid-level roles[cite: 181, 257].
* [cite_start]**Age & Tenure:** Younger employees in their first years at the company represent the highest flight risk[cite: 262, 263].

## Project Structure
- `final_project_HR.R`: Complete R script including data cleaning, EDA, modeling, and DSS implementation.
- `HR_Decision_Support_Report.pdf`: Detailed technical and business report.

## How to Use
1. Clone the repository.
2. Ensure the `HR_data.csv` is in the working directory.
3. Run the R script to generate the **HR Retention Shield Dashboard**.

---
[cite_start]*Developed as part of the "AI in Decision Making" course at Azrieli College of Engineering.* [cite: 48, 49, 52]
