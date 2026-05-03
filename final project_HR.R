# התקנת החבילות הנדרשות
install.packages("corrplot") # נצטרך את זה בהמשך למתאמים
install.packages("corrplot")
# טעינת הספריות
library(tidyverse)
library(corrplot)
# 1. טעינת הנתונים
dataset <- read.csv("HR_data.csv")

# 2. הצצה ראשונה לנתונים
glimpse(dataset)

# 3. בדיקת משתנה המטרה שלנו (Attrition)
# אנחנו רוצים לראות כמה עזבו (Yes) וכמה נשארו (No)
table(dataset$Attrition)

#############################################################
### שלב ניקוי וסידור הנתונים
############################################################

# 1. ניקוי עמודות שיש בהן רק ערך אחד זהה
dataset_clean <- dataset %>% select(-Over18, -EmployeeCount, -StandardHours)

# 2. טיפול במשתנה המטרה (yes - 1, No - 0) (Attrition)
dataset_clean$Attrition <- ifelse(dataset_clean$Attrition == "Yes", 1, 0)

# 3. המרת משתנים קטגוריאליים ל-Factors
dataset_clean <- dataset_clean %>% mutate_if(is.character, as.factor)

# בדיקה שהניקוי עבד
cat("מימדים לאחר ניקוי:", dim(dataset_clean), "\n") # אמור לרדת ל-32 עמודות

# חישובים וטרנספורמציות נוספות

# יצירת 3 משתנים חדשים
dataset_clean <- dataset_clean %>%
  mutate(
    LoyaltyRatio = YearsAtCompany / (TotalWorkingYears + 0.01),
    # 2. טרנספורמציה ופונקציית לוגיקה: "קבוצת גיל" (AgeGroup)
    AgeGroup = case_when(
      Age < 30 ~ "Young",
      Age >= 30 & Age <= 50 ~ "Mid-Senior",
      Age > 50 ~ "Senior"
    ),
    # 3. חישוב ציון כולל: "מדד שביעות רצון כללי" (TotalSatisfaction)
    # (הופכים אותם חזרה למספרים לצורך החיבור ואז סוכמים)
    TotalSatisfaction = as.numeric(EnvironmentSatisfaction) + 
      as.numeric(JobSatisfaction) + 
      as.numeric(RelationshipSatisfaction) + 
      as.numeric(WorkLifeBalance)
  )

# הופכים את קבוצת הגיל לפקטור (כי יצרנו טקסט חדש)
dataset_clean$AgeGroup <- as.factor(dataset_clean$AgeGroup)

# בדיקה שנוספו העמודות החדשות
glimpse(dataset_clean %>% select(LoyaltyRatio, AgeGroup, TotalSatisfaction))

##################################################################
### EDA שלב
#################################################################

# 1. בדיקת התפלגות משתנה המטרה: כמה באמת עזבו?
ggplot(dataset_clean, aes(x = factor(Attrition), fill = factor(Attrition))) +
  geom_bar() +
  labs(title = "Employee Attrition Count (0=Stay, 1=Left)",
       x = "Attrition Status",
       y = "Count") +
  theme_minimal()

# 2. נבדוק האם אנשים שעושים שעות נוספות נוטים לעזוב יותר
ggplot(dataset_clean, aes(x = OverTime, fill = factor(Attrition))) +
  geom_bar(position = "fill") +
  labs(title = "Attrition by OverTime Status",
       y = "Proportion",
       x = "OverTime") +
  scale_fill_manual(values = c("skyblue", "red"), name = "Left?") +
  theme_minimal()

# 3. נשתמש ב-Boxplot כדי לראות הבדלים בשכר בין מי שנשאר למי שעזב
ggplot(dataset_clean, aes(x = factor(Attrition), y = MonthlyIncome, fill = factor(Attrition))) +
  geom_boxplot() +
  labs(title = "Monthly Income Distribution by Attrition",
       x = "Left? (0=No, 1=Yes)",
       y = "Monthly Income") +
  theme_minimal()

# 4. נראה אילו משתנים מספריים הכי קשורים לעזיבה ונבחר את העיקריים
nums <- dataset_clean %>% 
  select(Attrition, Age, MonthlyIncome, YearsAtCompany, DistanceFromHome)
cor_matrix <- cor(nums)
corrplot(cor_matrix, method = "circle", type = "upper", tl.cex = 0.8)

##################################################################
### ML שלב
#################################################################

# 1. התקנת חבילות למודלים
install.packages("caret")          # לחלוקת נתונים וביצועים
install.packages("rpart")          # לעץ החלטה
install.packages("rpart.plot") # לציור העץ
install.packages("randomForest") # ליער אקראי

library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)

# 2. וידוא שמשתנה המטרה הוא Factor
dataset_clean$Attrition <- as.factor(dataset_clean$Attrition)

# 3. חלוקת הנתונים ל-Train (70%) ו-Test (30%)
set.seed(123)
trainIndex <- createDataPartition(dataset_clean$Attrition, p = .7, 
                                  list = FALSE, 
                                  times = 1)

trainData <- dataset_clean[ trainIndex,]
testData  <- dataset_clean[-trainIndex,]

cat("Train size:", nrow(trainData), "\n")
cat("Test size:", nrow(testData), "\n")

# בניית המודלים
# מודל 1: רגרסיה לוגיסטית (Logistic Regression)
# משתמשים ב-glm (Generalized Linear Model) עם משפחת binomial
model_log <- glm(Attrition ~ ., data = trainData, family = "binomial")
print("מודל רגרסיה לוגיסטית נוצר בהצלחה")

# מודל 2: עץ החלטה (Decision Tree)
model_tree <- rpart(Attrition ~ ., data = trainData, method = "class")
rpart.plot(model_tree, main="Decision Tree for Attrition") # נצייר אותו כבר עכשיו
print("מודל עץ החלטה נוצר בהצלחה")

# מודל 3: יער אקראי (Random Forest)
model_rf <- randomForest(Attrition ~ ., data = trainData, ntree=100, importance=TRUE)
print("מודל יער אקראי נוצר בהצלחה")

##################################################################
### שלב הערכת המודלים ובחירת המודל המנצח
#################################################################

# 1. תחזית עם רגרסיה לוגיסטית
# המודל נותן הסתברות (מספר בין 0 ל-1). אם זה מעל 0.5 נחליט שהעובד עוזב
pred_prob_log <- predict(model_log, newdata = testData, type = "response")
pred_class_log <- ifelse(pred_prob_log > 0.5, 1, 0)
pred_class_log <- as.factor(pred_class_log) # המרה לפקטור לצורך השוואה

# 2. תחזית עם עץ החלטה
pred_class_tree <- predict(model_tree, newdata = testData, type = "class")

# 3. תחזית עם יער אקראי
pred_class_rf <- predict(model_rf, newdata = testData, type = "class")


# יצירת מטריצות בלבול (בדיקת ציונים)
print("תוצאות רגרסיה לוגיסטית")
cm_log <- confusionMatrix(pred_class_log, testData$Attrition)
print(cm_log$overall['Accuracy'])

print("תוצאות עץ החלטה")
cm_tree <- confusionMatrix(pred_class_tree, testData$Attrition)
print(cm_tree$overall['Accuracy'])

print("תוצאות יער אקראי")
cm_rf <- confusionMatrix(pred_class_rf, testData$Attrition)
print(cm_rf$overall['Accuracy'])

##################################################################
### שלב יישום מערכת תומכת החלטה (DSS Implementation) - מודל הרמזור
#################################################################

# יצירת דוח "HR Retention Shield"
# אנחנו לוקחים את ההסתברות שחישב המודל (pred_prob_log) וממיינים לפי הכללים שהגדרנו

dss_report <- testData %>%
  mutate(Risk_Score = pred_prob_log) %>%  # הציון הרציף (0 עד 1)
  
  # שלב הלוגיקה: חלוקה לקטגוריות (RAG - Red, Amber, Green)
  mutate(Risk_Level = case_when(
    Risk_Score <= 0.30 ~ "Green (Safe)",       # אזור ירוק
    Risk_Score > 0.30 & Risk_Score <= 0.70 ~ "Yellow (Warning)", # אזור צהוב
    Risk_Score > 0.70 ~ "Red (High Risk)"      # אזור אדום
  )) %>%
  
  # שלב ההמלצה לפעולה (Action Item) לפי הטקסט בדוח
  mutate(Recommendation = case_when(
    Risk_Level == "Green (Safe)" ~ "No Action Needed (Retention Routine)",
    Risk_Level == "Yellow (Warning)" ~ "Soft Intervention: Manager Check-in",
    Risk_Level == "Red (High Risk)" ~ "URGENT: Offer Retention Plan (Salary/Role)"
  )) %>%
  
  # נבחר את העמודות הרלוונטיות לתצוגה למנהלים
  select(Risk_Level, Risk_Score, Recommendation, 
         MonthlyIncome, OverTime, Age, JobRole) %>%
  
  # נמיין כדי שהמקרים הדחופים (אדום) יהיו למעלה
  arrange(desc(Risk_Score))

# --- הדפסת הדשבורד למנהלים ---
print("--- HR Retention Shield Dashboard ---")

# נציג את 15 העובדים הראשונים בסיכון הכי גבוה (האזור האדום והצהוב הגבוה)
print(head(dss_report, 15))

# בונוס: נראה כמה עובדים יש בכל קבוצה (סטטיסטיקה למנהל)
print("--- Summary by Risk Level ---")
print(table(dss_report$Risk_Level))

# נשתמש ביער האקראי כדי לראות ויזואלית מה הכי חשוב
varImpPlot(model_rf, 
           type = 2, 
           main = "Top Factors Influencing Attrition",
           n.var = 10, # נציג רק את 10 המשפיעים ביותר
           col = "darkblue")

















