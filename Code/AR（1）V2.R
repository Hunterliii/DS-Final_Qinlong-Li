library(readxl)
if (!requireNamespace("xts", quietly = TRUE)) {
  install.packages("xts")
}
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

data_transposed$inflation_rate <- (log(data_transposed$`Personal consumption expenditures`) -
                              log(lag(data_transposed$`Personal consumption expenditures`))) * 12

inflation_ts <- ts(data_transposed$inflation_rate, frequency = 12)  # Assuming monthly data with annual frequency

inflation_ts <- na.omit(inflation_ts)


# 检验原始数据的平稳性1
plot(inflation_ts , main="Time Series Plot", xlab="Time", ylab="Inflation Rate")

# 检验原始数据的平稳性2
acf(inflation_ts , main="ACF of Inflation Rate")

#检验原始数据的平稳性3
# 安装并加载需要的包
if (!requireNamespace("tseries", quietly = TRUE)) {
  install.packages("tseries")
}
library(tseries)

# 进行ADF检验
adf.test(inflation_ts, alternative = "stationary")

# 进行KPSS检验
kpss.test(inflation_ts)

# 使用PP检验
pp.test(inflation_ts)

# 一阶差分
diff_inflation_ts <- diff(inflation_ts, differences = 1)


diff_inflation_ts <- na.omit(diff_inflation_ts)
# 检查差分后的数据
plot(diff_inflation_ts, main="First Difference of Inflation Rate")


# ADF测试
adf_result_diff <- adf.test(diff_inflation_ts, alternative = "stationary")
print(adf_result_diff)

# PP测试
pp_result_diff <- pp.test(diff_inflation_ts)
print(pp_result_diff)

# KPSS测试
kpss_result_diff <- kpss.test(diff_inflation_ts)
print(kpss_result_diff)

#检验原始数据的平稳性
acf(diff_inflation_ts, main="ACF of Inflation Rate")



# 拟合ARIMA模型
library(forecast)
model_arima <- auto.arima(diff_inflation_ts)

# 预测未来50个月
forecast_diff <- forecast(model_arima, h=50)

# 逆向差分
last_original_value <- tail(inflation_ts, n=1)  # 取原始序列的最后一个值

# 初始化预测结果数组，长度等于预测月份+1，第一个元素为最后一个实际值
forecast_original <- numeric(length = length(forecast_diff$mean) + 1)
forecast_original[1] <- last_original_value

# 通过逐步累加预测的差分恢复原始尺度的预测值
for (i in 2:length(forecast_original)) {
  forecast_original[i] <- forecast_original[i - 1] + forecast_diff$mean[i - 1]
}

# 绘图
time_original <- time(inflation_ts)
time_forecast <- seq(max(time_original), by = 1/12, length.out = length(forecast_original))

# 绘制图像
plot(time_original, inflation_ts, type = 'l', col = 'blue', xlab = '时间', ylab = '通胀率', main = '通胀率及其预测')
lines(time_forecast, forecast_original, col = 'red')
legend("topleft", legend = c("原始通胀率", "预测通胀率"), col = c("blue", "red"), lty = 1, cex = 0.8)



















