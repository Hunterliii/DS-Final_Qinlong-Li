#GBM预测

# 在新数据中验证GBM模型
# 提取X_scaled_test的后50行作为新的测试数据
X_test_sub <- X_scaled_test[(nrow(X_scaled_test)-49):nrow(X_scaled_test), ]

# 提取Y_test的后50行作为新的目标数据
Y_test_sub <- Y_test[(length(Y_test)-49):length(Y_test)]

# 使用最优GBM模型进行预测
predictions_gbm <- predict(gbm_model, newdata = X_test_sub)

# 可视化实际值与预测值
plot(Y_test_sub, type = "o", col = "blue", xlab = "Months", ylab = "Inflation Rate", main = "Comparison of GBM model prediction results")
lines(predictions_gbm, type = "o", col = "red")
legend("topright", legend = c("True Value", "Prediction Value"), col = c("blue", "red"), lty = 1)

# 计算并打印预测的准确性指标，例如均方误差
mse <- mean((Y_test_sub - predictions_gbm)^2)
print(paste("均方误差为:", mse))



# 在新数据中验证GBM模型，预测未来3个月
# 提取X_scaled_3months_test的后50行作为新的测试数据
X_test_sub_3months <- X_scaled_3months_test[(nrow(X_scaled_3months_test)-49):nrow(X_scaled_3months_test), ]

# 提取Y_3months_test的后50行作为新的目标数据
Y_test_sub_3months <- Y_3months_test[(length(Y_3months_test)-49):length(Y_3months_test)]

# 使用最优GBM模型进行预测
predictions_3months_gbm <- predict(gbm_model_3months, newdata = X_test_sub_3months)

# 可视化实际值与预测值
plot(Y_test_sub_3months, type = "o", col = "blue", xlab = "Months", ylab = "Inflation Rate", main = "Comparison of GBM 3-Month Prediction Results")
lines(predictions_3months_gbm, type = "o", col = "red")
legend("topright", legend = c("True Value", "Prediction Value"), col = c("blue", "red"), lty = 1)

# 计算并打印预测的准确性指标，例如均方误差
mse_3months_gbm <- mean((Y_test_sub_3months - predictions_3months_gbm)^2)
print(paste("均方误差为:", mse_3months_gbm))




# 在新数据中验证GBM模型，预测未来12个月
# 提取X_scaled_12months_test的后50行作为新的测试数据
X_test_sub_12months <- X_scaled_12months_test[(nrow(X_scaled_12months_test)-49):nrow(X_scaled_12months_test), ]

# 提取Y_12months_test的后50行作为新的目标数据
Y_test_sub_12months <- Y_12months_test[(length(Y_12months_test)-49):length(Y_12months_test)]

# 使用最优GBM模型进行预测
predictions_12months_gbm <- predict(gbm_model_12months, newdata = X_test_sub_12months)

# 可视化实际值与预测值
plot(Y_test_sub_12months, type = "o", col = "blue", xlab = "Months", ylab = "Inflation Rate", main = "Comparison of GBM 12-Month Prediction Results")
lines(predictions_12months_gbm, type = "o", col = "red")
legend("topright", legend = c("True Value", "Prediction Value"), col = c("blue", "red"), lty = 1)

# 计算并打印预测的准确性指标，例如均方误差
mse_12months_gbm <- mean((Y_test_sub_12months - predictions_12months_gbm)^2)
print(paste("均方误差为:", mse_12months_gbm))

