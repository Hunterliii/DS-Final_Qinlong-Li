library(readxl)

library(xts)

library(dplyr)

data <- read_excel("D:/DS/Final/US_PCE_training.xlsx")

##处理数据
data <- na.omit(data)

# 转化成长数据
data_transposed <- t(data)

# 将转置的矩阵转换为数据框
data_transposed <- as.data.frame(data_transposed)

# 设置新的列名
colnames(data_transposed) <- data_transposed[1, ]  # 将第一行的值设置为列名

# 删除现在作为列名的第一行
data_transposed <- data_transposed[-1, ]
# 重新排序列，使第二列成为第一列
data_transposed <- data_transposed[c(2, 1, 3:ncol(data_transposed))]

# 转换所有列为数值类型
data_transposed[] <- lapply(data_transposed, function(x) as.numeric(as.character(x)))

# 检查转换后的数据类型
str(data_transposed)

#构造inflation rate



Newdata <- data_transposed


Newdata <- Newdata %>%
  mutate(across(
    .cols = -month,  # 排除月份列
    .fns = ~ (log(.) - log(lag(.))) * 12,
    .names = "{.col}_log_diff_12"
  ))

#将inflation rate移动数据第一列
# 假设 data_transformed 是包含我们之前计算得到的列的数据框
# 首先重命名列
Newdata <- Newdata %>%
  rename('inflation rate' = 'Personal consumption expenditures_log_diff_12')

# 然后将重命名的列移动到第一列位置
Newdata <- Newdata %>%
  relocate(`inflation rate`, .before = 1)


#为每列变量生成24月的滞后项
# 首先生成滞后变量并添加到临时数据框中
temp_data <- Newdata %>%
  mutate(across(
    .cols = -month,  # 排除月份列
    .fns = list(lag1 = ~lag(., 1), lag2 = ~lag(., 2), lag3 = ~lag(., 3),
                lag4 = ~lag(., 4), lag5 = ~lag(., 5), lag6 = ~lag(., 6),
                lag7 = ~lag(., 7), lag8 = ~lag(., 8), lag9 = ~lag(., 9),
                lag10 = ~lag(., 10), lag11 = ~lag(., 11), lag12 = ~lag(., 12),
                lag13 = ~lag(., 13), lag14 = ~lag(., 14), lag15 = ~lag(., 15),
                lag16 = ~lag(., 16), lag17 = ~lag(., 17), lag18 = ~lag(., 18),
                lag19 = ~lag(., 19), lag20 = ~lag(., 20), lag21 = ~lag(., 21),
                lag22 = ~lag(., 22), lag23 = ~lag(., 23), lag24 = ~lag(., 24)),
    .names = "{.col}_{.fn}"
  ))

# 从临时数据框中选择所有生成的滞后变量列，创建一个新的数据框
data_transposed_lag <- temp_data %>%
  select(matches("lag(1[0-9]|2[0-4]|[1-9])$"))



#创建前三个月的平均值
library(dplyr)
library(zoo)

# 创建临时数据框，先将所有变量延后一个月
temp_data <- Newdata %>%
  mutate(across(
    .cols = -month,  # 排除月份列
    .fns = ~lag(.),  # 延后一个月
    .names = "{.col}_lag"
  )) %>%
  # 计算延后数据的滚动平均值
  mutate(across(
    .cols = ends_with("_lag"),  # 选择延后的列
    .fns = list(rollmean3 = ~rollmean(x = ., k = 3, fill = NA, align = "right")),
    .names = "{.col}_rollmean3"
  ))

# 从临时数据框中选择所有生成的平均变量列，创建一个新的数据框
data_rollmean3 <- temp_data %>%
  select(ends_with("_rollmean3"))

#创建前六个月的平均值
temp_data <- Newdata %>%
  mutate(across(
    .cols = -month,  # 排除月份列
    .fns = ~lag(.),  # 延后一个月
    .names = "{.col}_lag"
  )) %>%
  # 计算延后数据的滚动平均值
  mutate(across(
    .cols = ends_with("_lag"),  # 选择延后的列
    .fns = list(rollmean6 = ~rollmean(x = ., k = 6, fill = NA, align = "right")),
    .names = "{.col}_rollmean6"
  ))

# 从临时数据框中选择所有生成的平均变量列，创建一个新的数据框
data_rollmean6 <- temp_data %>%
  select(ends_with("_rollmean6"))




#创建前12个月的平均值
temp_data <- Newdata %>%
  mutate(across(
    .cols = -month,  # 排除月份列
    .fns = ~lag(.),  # 延后一个月
    .names = "{.col}_lag"
  )) %>%
  # 计算延后数据的滚动平均值
  mutate(across(
    .cols = ends_with("_lag"),  # 选择延后的列
    .fns = list(rollmean12 = ~rollmean(x = ., k = 12, fill = NA, align = "right")),
    .names = "{.col}_rollmean12"
  ))

# 从临时数据框中选择所有生成的平均变量列，创建一个新的数据框
data_rollmean12 <- temp_data %>%
  select(ends_with("_rollmean12"))


#创建前24个月的平均值
temp_data <- Newdata %>%
  mutate(across(
    .cols = -month,  # 排除月份列
    .fns = ~lag(.),  # 延后一个月
    .names = "{.col}_lag"
  )) %>%
  # 计算延后数据的滚动平均值
  mutate(across(
    .cols = ends_with("_lag"),  # 选择延后的列
    .fns = list(rollmean24 = ~rollmean(x = ., k = 24, fill = NA, align = "right")),
    .names = "{.col}_rollmean24"
  ))

# 从临时数据框中选择所有生成的平均变量列，创建一个新的数据框
data_rollmean24 <- temp_data %>%
  select(ends_with("_rollmean24"))



#合并特征
# 使用bind_cols直接横向合并所有数据框
all_data_combined <- bind_cols(Newdata, data_rollmean3, data_rollmean6, 
                               data_rollmean12, data_rollmean24, data_transposed_lag)

all_data_combined <- all_data_combined %>%
  select(-c(3:411))


all_data_combined <- na.omit(all_data_combined)

#标准化X
X <- all_data_combined[,-(1:2)]

X_scaled <- scale(X)

Y <- all_data_combined[,1]

X_scaled <- as.matrix(X_scaled)

###进行lasso回归

# 安装和加载glmnet包
if (!require(glmnet)) {
  install.packages("glmnet", dependencies = TRUE)
  library(glmnet)
}

# 假设Y已经存在于你的环境中，并且与X_scaled的行数相同
# 拟合LASSO回归模型1
# 利用信息到提前1月，前24个月的数据预测
set.seed(123)
lasso_model <- glmnet(X_scaled, Y, family = "gaussian", alpha = 1)

# 可视化系数路径
plot(lasso_model, xvar = "lambda", label = TRUE)

# 进行交叉验证
cv_lasso <- cv.glmnet(X_scaled, Y, family = "gaussian", alpha = 1)
# 绘制交叉验证结果
plot(cv_lasso)

# 最佳lambda
best_lambda <- cv_lasso$lambda.min
# 使用最佳lambda重新拟合模型
best_lasso_model <- glmnet(X_scaled, Y, family = "gaussian", alpha = 1, lambda = best_lambda)

# 获取最优模型的系数
coef(best_lasso_model)

# 获取模型系数
coefficients_best <- coef(best_lasso_model, s = "lambda.min")

# 计算非零系数的数量
non_zero_coeffs_count <- sum(coefficients_best != 0)

# 打印非零系数的数量
print(non_zero_coeffs_count)





# 拟合LASSO回归模型2
# 利用信息到提前3月，前24个月的数据预测

# 从data_transposed_lag数据集中删除具有_lag1和_lag2尾缀的列
data_transposed_lag_3months  <- data_transposed_lag %>%
  select(-matches(".*_lag[12]$"))

#3个月平均值
library(dplyr)
library(zoo)

# 创建临时数据框，先将所有变量延后三个月
temp_data <- Newdata %>%
  mutate(across(
    .cols = -month,  # 排除月份列
    .fns = ~lag(., 3),  # 延后三个月
    .names = "{.col}_lag3"
  )) %>%
  # 计算延后数据的滚动平均值
  mutate(across(
    .cols = ends_with("_lag3"),  # 选择延后的列
    .fns = list(rollmean3 = ~rollmean(x = ., k = 3, fill = NA, align = "right")),
    .names = "{.col}_rollmean3"
  ))

# 从临时数据框中选择所有生成的平均变量列，创建一个新的数据框
data_rollmean3_3months <- temp_data %>%
  select(ends_with("_rollmean3"))

##创建前六个月的平均值
library(dplyr)
library(zoo)

# 创建临时数据框，先将所有变量延后三个月
temp_data <- Newdata %>%
  mutate(across(
    .cols = -month,  # 排除月份列
    .fns = ~lag(., 3),  # 延后三个月
    .names = "{.col}_lag3"
  )) %>%
  # 计算延后数据的滚动平均值
  mutate(across(
    .cols = ends_with("_lag3"),  # 选择延后的列
    .fns = list(rollmean6 = ~rollmean(x = ., k = 6, fill = NA, align = "right")),
    .names = "{.col}_rollmean6"
  ))

# 从临时数据框中选择所有生成的平均变量列，创建一个新的数据框
data_rollmean6_3months  <- temp_data %>%
  select(ends_with("_rollmean6"))

#创建前12个月的平均值
# 创建临时数据框，先将所有变量延后三个月
temp_data <- Newdata %>%
  mutate(across(
    .cols = -month,  # 排除月份列
    .fns = ~lag(., 3),  # 延后三个月
    .names = "{.col}_lag3"
  )) %>%
  # 计算延后数据的滚动平均值
  mutate(across(
    .cols = ends_with("_lag3"),  # 选择延后的列
    .fns = list(rollmean12 = ~rollmean(x = ., k = 12, fill = NA, align = "right")),
    .names = "{.col}_rollmean12"
  ))

# 从临时数据框中选择所有生成的平均变量列，创建一个新的数据框
data_rollmean12_3months  <- temp_data %>%
  select(ends_with("_rollmean12"))

#创建前24个月的平均值
# 创建临时数据框，先将所有变量延后三个月
temp_data <- Newdata %>%
  mutate(across(
    .cols = -month,  # 排除月份列
    .fns = ~lag(., 3),  # 延后三个月
    .names = "{.col}_lag3"
  )) %>%
  # 计算延后数据的滚动平均值
  mutate(across(
    .cols = ends_with("_lag3"),  # 选择延后的列
    .fns = list(rollmean24 = ~rollmean(x = ., k = 24, fill = NA, align = "right")),
    .names = "{.col}_rollmean24"
  ))

# 从临时数据框中选择所有生成的平均变量列，创建一个新的数据框
data_rollmean24_3months  <- temp_data %>%
  select(ends_with("_rollmean24"))

#合并特征
# 使用bind_cols直接横向合并所有数据框
all_data_combined_3months <- bind_cols(Newdata, data_rollmean3_3months, data_rollmean6_3months, 
                               data_rollmean12_3months, data_rollmean24_3months, data_transposed_lag_3months)

all_data_combined_3months <- all_data_combined_3months %>%
  select(-c(3:411))


all_data_combined_3months  <- na.omit(all_data_combined_3months )

#标准化X
X_3months <- all_data_combined_3months[,-(1:2)]

X_scaled_3months <- scale(X_3months)

Y_3months <- all_data_combined_3months[,1]

X_scaled_3months <- as.matrix(X_scaled_3months)

# 利用信息到提前3月，前24个月的数据预测
set.seed(123)
lasso_model_3months <- glmnet(X_scaled_3months, Y_3months, family = "gaussian", alpha = 1)

# 可视化系数路径
plot(lasso_model_3months, xvar = "lambda", label = TRUE)

# 进行交叉验证
cv_lasso_3months <- cv.glmnet(X_scaled_3months, Y_3months, family = "gaussian", alpha = 1)
# 绘制交叉验证结果
plot(cv_lasso_3months)

# 最佳lambda
best_lambda_3months <- cv_lasso_3months$lambda.min
# 使用最佳lambda重新拟合模型
best_lasso_model_3months <- glmnet(X_scaled_3months, Y_3months, family = "gaussian", alpha = 1, lambda = best_lambda_3months)

# 获取最优模型的系数
coef(best_lasso_model_3months)

# 获取模型系数
coefficients_best_3months <- coef(best_lasso_model_3months, s = "lambda.min")

# 计算非零系数的数量
non_zero_coeffs_count_3months <- sum(coefficients_best_3months != 0)

# 打印非零系数的数量
print(non_zero_coeffs_count_3months)



### 拟合LASSO回归模型3
# 利用信息到提前12月，前24个月的数据预测
# 从data_transposed_lag数据集中删除具有_lag1和_lag2尾缀的列
data_transposed_lag_12months  <- data_transposed_lag %>%
  select(-matches(".*_lag(1[01]|[1-9])$"))


#3个月平均值
library(dplyr)
library(zoo)

# 创建临时数据框，先将所有变量延后12个月
temp_data <- Newdata %>%
  mutate(across(
    .cols = -month,  # 排除月份列
    .fns = ~lag(., 12),  # 延后12个月
    .names = "{.col}_lag12"
  )) %>%
  # 计算延后数据的滚动平均值
  mutate(across(
    .cols = ends_with("_lag12"),  # 选择延后的列
    .fns = list(rollmean3 = ~rollmean(x = ., k = 3, fill = NA, align = "right")),
    .names = "{.col}_rollmean3"
  ))

# 从临时数据框中选择所有生成的平均变量列，创建一个新的数据框
data_rollmean3_12months <- temp_data %>%
  select(ends_with("_rollmean3"))

##创建前六个月的平均值


# 创建临时数据框，先将所有变量延后三个月
temp_data <- Newdata %>%
  mutate(across(
    .cols = -month,  # 排除月份列
    .fns = ~lag(., 12),  # 延后三个月
    .names = "{.col}_lag12"
  )) %>%
  # 计算延后数据的滚动平均值
  mutate(across(
    .cols = ends_with("_lag12"),  # 选择延后的列
    .fns = list(rollmean6_12months  = ~rollmean(x = ., k = 6, fill = NA, align = "right")),
    .names = "{.col}_rollmean6"
  ))

# 从临时数据框中选择所有生成的平均变量列，创建一个新的数据框
data_rollmean6_12months   <- temp_data %>%
  select(ends_with("_rollmean6"))

#创建前12个月的平均值
# 创建临时数据框，先将所有变量延后三个月
temp_data <- Newdata %>%
  mutate(across(
    .cols = -month,  # 排除月份列
    .fns = ~lag(., 12),  # 延后三个月
    .names = "{.col}_lag12"
  )) %>%
  # 计算延后数据的滚动平均值
  mutate(across(
    .cols = ends_with("_lag12"),  # 选择延后的列
    .fns = list(rollmean12_12months  = ~rollmean(x = ., k = 12, fill = NA, align = "right")),
    .names = "{.col}_rollmean12"
  ))

# 从临时数据框中选择所有生成的平均变量列，创建一个新的数据框
data_rollmean12_12months   <- temp_data %>%
  select(ends_with("_rollmean12"))

#创建前24个月的平均值
# 创建临时数据框，先将所有变量延后三个月
temp_data <- Newdata %>%
  mutate(across(
    .cols = -month,  # 排除月份列
    .fns = ~lag(., 12),  # 延后三个月
    .names = "{.col}_lag12"
  )) %>%
  # 计算延后数据的滚动平均值
  mutate(across(
    .cols = ends_with("_lag12"),  # 选择延后的列
    .fns = list(rollmean24_12months  = ~rollmean(x = ., k = 24, fill = NA, align = "right")),
    .names = "{.col}_rollmean24"
  ))

# 从临时数据框中选择所有生成的平均变量列，创建一个新的数据框
data_rollmean24_12months  <- temp_data %>%
  select(ends_with("_rollmean24"))

#合并特征
# 使用bind_cols直接横向合并所有数据框
all_data_combined_12months  <- bind_cols(Newdata, data_rollmean3_12months , data_rollmean6_12months , 
                                       data_rollmean12_12months , data_rollmean24_12months , data_transposed_lag_12months )

all_data_combined_12months  <- all_data_combined_12months  %>%
  select(-c(3:411))


all_data_combined_12months   <- na.omit(all_data_combined_12months  )

#标准化X
X_12months  <- all_data_combined_12months [,-(1:2)]

X_scaled_12months  <- scale(X_12months )

Y_12months  <- all_data_combined_12months [,1]

X_scaled_12months  <- as.matrix(X_scaled_12months )

# 利用信息到提前12月，前24个月的数据预测
set.seed(123)
lasso_model_12months  <- glmnet(X_scaled_12months , Y_12months , family = "gaussian", alpha = 1)

# 可视化系数路径
plot(lasso_model_12months , xvar = "lambda", label = TRUE)

# 进行交叉验证
cv_lasso_12months  <- cv.glmnet(X_scaled_12months , Y_12months , family = "gaussian", alpha = 1)
# 绘制交叉验证结果
plot(cv_lasso_12months )

# 最佳lambda
best_lambda_12months  <- cv_lasso_12months $lambda.min
# 使用最佳lambda重新拟合模型
best_lasso_model_12months  <- glmnet(X_scaled_12months , Y_12months , family = "gaussian", alpha = 1, lambda = best_lambda_12months )

# 获取最优模型的系数
coef(best_lasso_model_12months )

# 获取模型系数
coefficients_best_12months  <- coef(best_lasso_model_12months , s = "lambda.min")

# 计算非零系数的数量
non_zero_coeffs_count_12months <- sum(coefficients_best_12months != 0)

# 打印非零系数的数量
print(non_zero_coeffs_count_12months )

