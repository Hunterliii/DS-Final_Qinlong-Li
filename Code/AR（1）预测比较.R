#比较AR（1）模型

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

data_transposed <- data_transposed[,-207]
data_transposed_test <- rbind(data_transposed, testing_data)

data_transposed_test$inflation_rate <- (log(data_transposed_test$`Personal consumption expenditures`) -
                                     log(lag(data_transposed_test$`Personal consumption expenditures`))) * 12

# 提取Y_test的后50行作为新的目标数据

Future_data<- data_transposed_test[(nrow(data_transposed_test)-49):nrow(data_transposed_test),]
# 保留Future_data的第一列和最后一列
Future_data<- Future_data[, c(1, ncol(Future_data))]


# 使用预测的时间和对应的预测值创建数据框
forecast_df <- data.frame(
  Time = time_forecast,
  Predicted_Inflation_Rate = forecast_original
)
# 删除forecast_df的第一行
forecast_df <- forecast_df[-1, ]
# 重命名列
names(forecast_df)[names(forecast_df) == "Time"] <- "month"
# 设置第一行的值为733，后续递增
forecast_df$month <- 733 + (0:(nrow(forecast_df) - 1))

# 假设 Future_data 中的时间列也命名为 'months'，并且有一个名为 'Actual_Inflation' 的列
# 合并数据框
combined_data <- merge(forecast_df, Future_data, by = "month", all = TRUE)

# 重命名列以区分预测和实际值

names(combined_data)[names(combined_data) == "inflation_rate"] <- "Actual_Inflation_Rate"

library(ggplot2)

# 绘制图形
ggplot(data = combined_data, aes(x = month)) +
  geom_line(aes(y = Actual_Inflation_Rate, colour = "Actual Inflation")) +
  geom_line(aes(y = Predicted_Inflation_Rate, colour = "Predicted Inflation")) +
  scale_colour_manual(values = c("Actual Inflation" = "blue", "Predicted Inflation" = "red")) +
  labs(title = "Actual vs Predicted Inflation Rates", x = "Months", y = "Inflation Rate") +
  theme_minimal()

mse_AR <- mean((combined_data$Actual_Inflation_Rate -  combined_data$Predicted_Inflation_Rate)^2)
print(paste("均方误差为:", mse_AR))
