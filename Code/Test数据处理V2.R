#Test 数据处理V2
# 加载数据
testing_data <- read_excel("D:/DS/Final/US_PCE_testing_fake.xlsx")

##处理数据
testing_data <- na.omit(testing_data)

# 转化成长数据
testing_data <- t(testing_data)

# 将转置的矩阵转换为数据框
testing_data <- as.data.frame(testing_data)

# 设置新的列名
colnames(testing_data) <- testing_data[1, ]  # 将第一行的值设置为列名

# 删除现在作为列名的第一行
testing_data <- testing_data[-1, ]
# 重新排序列，使第二列成为第一列
testing_data <- testing_data[c(2, 1, 3:ncol(testing_data))]

# 转换所有列为数值类型
testing_data[] <- lapply(testing_data, function(x) as.numeric(as.character(x)))

data_transposed_test <- rbind(data_transposed, testing_data)

Newdata_test <- data_transposed_test

Newdata_test <- Newdata_test %>%
  mutate(across(
    .cols = -month,  # 排除月份列
    .fns = ~ (log(.) - log(lag(.))) * 12,
    .names = "{.col}_log_diff_12"
  ))

Newdata_test <- Newdata_test %>%
  rename('inflation rate' = 'Personal consumption expenditures_log_diff_12')

# 然后将重命名的列移动到第一列位置
Newdata_test <- Newdata_test %>%
  relocate(`inflation rate`, .before = 1)

#为每列变量生成24月的滞后项
# 首先生成滞后变量并添加到临时数据框中
temp_data_test <- Newdata_test %>%
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
data_transposed_lag_test <- temp_data_test %>%
  select(matches("lag(1[0-9]|2[0-4]|[1-9])$"))



#创建前三个月的平均值
library(dplyr)
library(zoo)

# 创建临时数据框，先将所有变量延后一个月
temp_data_test <- Newdata_test %>%
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
data_rollmean3_test <- temp_data_test %>%
  select(ends_with("_rollmean3"))



#创建前六个月的平均值
temp_data_test <- Newdata_test %>%
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
data_rollmean6_test <- temp_data_test %>%
  select(ends_with("_rollmean6"))


#创建前12个月的平均值
temp_data_test <- Newdata_test %>%
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
data_rollmean12_test <- temp_data_test %>%
  select(ends_with("_rollmean12"))



#创建前24个月的平均值
temp_data_test <- Newdata_test %>%
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
data_rollmean24_test <- temp_data_test %>%
  select(ends_with("_rollmean24"))


#合并特征
# 使用bind_cols直接横向合并所有数据框
all_data_combined_test <- bind_cols(Newdata_test, data_rollmean3_test, data_rollmean6_test, 
                               data_rollmean12_test, data_rollmean24_test, data_transposed_lag_test)

all_data_combined_test <- all_data_combined_test %>%
  select(-c(3:411))


all_data_combined_test <- na.omit(all_data_combined_test)

#标准化X
X_test <- all_data_combined_test[,-(1:2)]

X_scaled_test <- scale(X_test)

Y_test <- all_data_combined_test[,1]

X_scaled_test <- as.matrix(X_scaled_test)




### 数据整理2
# 利用信息到提前3月，前24个月的数据预测
# 从data_transposed_lag数据集中删除具有_lag1和_lag2尾缀的列
data_transposed_lag_3months_test  <- data_transposed_lag_test %>%
  select(-matches(".*_lag[12]$"))

# 创建临时数据框，先将所有变量延后三个月
temp_data_test  <- Newdata_test  %>%
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
data_rollmean3_3months_test  <- temp_data_test  %>%
  select(ends_with("_rollmean3"))



# 创建临时数据框，先将所有变量延后三个月
temp_data_test <- Newdata_test %>%
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
data_rollmean6_3months_test  <- temp_data_test %>%
  select(ends_with("_rollmean6"))

#创建前12个月的平均值
# 创建临时数据框，先将所有变量延后三个月
temp_data_test <- Newdata_test %>%
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
data_rollmean12_3months_test  <- temp_data_test %>%
  select(ends_with("_rollmean12"))

#创建前24个月的平均值
# 创建临时数据框，先将所有变量延后三个月
temp_data_test <- Newdata_test %>%
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
data_rollmean24_3months_test  <- temp_data_test %>%
  select(ends_with("_rollmean24"))

#合并特征
# 使用bind_cols直接横向合并所有数据框
all_data_combined_3months_test  <- bind_cols(Newdata_test , data_rollmean3_3months_test , data_rollmean6_3months_test , 
                                       data_rollmean12_3months_test , data_rollmean24_3months_test , data_transposed_lag_3months_test )

all_data_combined_3months_test  <- all_data_combined_3months_test  %>%
  select(-c(3:411))


all_data_combined_3months_test   <- na.omit(all_data_combined_3months_test  )

#标准化X
X_3months_test  <- all_data_combined_3months_test [,-(1:2)]

X_scaled_3months_test  <- scale(X_3months_test )

Y_3months_test  <- all_data_combined_3months_test [,1]

X_scaled_3months_test  <- as.matrix(X_scaled_3months_test )

### 拟合LASSO回归模型3
# 利用信息到提前12月，前24个月的数据预测
# 从data_transposed_lag数据集中删除具有_lag1和_lag2尾缀的列
data_transposed_lag_12months_test   <- data_transposed_lag_test  %>%
  select(-matches(".*_lag(1[01]|[1-9])$"))

# 创建临时数据框，先将所有变量延后12个月
temp_data_test <- Newdata_test %>%
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
data_rollmean3_12months_test <- temp_data_test %>%
  select(ends_with("_rollmean3"))


# 创建临时数据框，先将所有变量延后三个月
temp_data_test <- Newdata_test %>%
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
data_rollmean6_12months_test   <- temp_data_test %>%
  select(ends_with("_rollmean6"))


#创建前12个月的平均值
# 创建临时数据框，先将所有变量延后三个月
temp_data_test <- Newdata_test %>%
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
data_rollmean12_12months_test   <- temp_data_test %>%
  select(ends_with("_rollmean12"))




#创建前24个月的平均值
# 创建临时数据框，先将所有变量延后三个月
temp_data_test <- Newdata_test %>%
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
data_rollmean24_12months_test  <- temp_data_test %>%
  select(ends_with("_rollmean24"))

#合并特征
# 使用bind_cols直接横向合并所有数据框
all_data_combined_12months_test  <- bind_cols(Newdata_test, data_rollmean3_12months_test , data_rollmean6_12months_test , 
                                         data_rollmean12_12months_test , data_rollmean24_12months_test , data_transposed_lag_12months_test )

all_data_combined_12months_test  <- all_data_combined_12months_test  %>%
  select(-c(3:411))


all_data_combined_12months_test   <- na.omit(all_data_combined_12months_test  )

#标准化X
X_12months_test  <- all_data_combined_12months_test [,-(1:2)]

X_scaled_12months_test  <- scale(X_12months_test )

Y_12months_test  <- all_data_combined_12months_test [,1]

X_scaled_12months_test  <- as.matrix(X_scaled_12months_test )
