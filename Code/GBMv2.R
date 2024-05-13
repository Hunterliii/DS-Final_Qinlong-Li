if (!require(caret)) {
  install.packages("caret")
  library(caret)
}
if (!require(doParallel)) {
  install.packages("doParallel")
  library(doParallel)
}
if (!require(gbm)) {
  install.packages("gbm")
  library(gbm)
}
# 注册并行后台
numCores <- detectCores()
cl <- makeCluster(numCores - 1)  # 保留一个核心
registerDoParallel(cl)

# 设置训练控制
train_control <- trainControl(
  method = "cv",  # 使用交叉验证
  number = 10,    # 10折交叉验证
  allowParallel = TRUE  # 允许并行计算
)

# 定义要尝试的梯度提升模型参数网格
tune_grid <- expand.grid(
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(10, 20),
  shrinkage = c(0.001, 0.005),
  n.trees = 100  # 固定树的数量为100
)

# 训练梯度提升模型
gbm_model <- train(
  x = X_scaled, 
  y = Y, 
  method = "gbm", 
  tuneGrid = tune_grid,
  trControl = train_control,
  verbose = FALSE
)

# 关闭并行处理
stopCluster(cl)
registerDoSEQ()

# 打印模型输出
print(gbm_model)

# 可视化模型的性能
results <- gbm_model$results
plot <- ggplot(results, aes(x = interaction.depth, y = RMSE, color = factor(n.minobsinnode), shape = factor(shrinkage))) +
  geom_point(size = 4) +
  scale_color_manual(values = c("red", "blue"), labels = c("10", "20")) +
  scale_shape_manual(values = c(17, 15), labels = c("0.001", "0.005")) +
  labs(
    x = "Depth of Tree",
    y = "RMSE",
    title = "Model Performance by Tree Depth and Parameters",
    color = "Min. Obs in Node",
    shape = "Shrinkage"
  ) +
  theme_minimal()

# 显示图
print(plot)




# 注册并行后台
numCores <- detectCores()
cl <- makeCluster(numCores - 1)  # 保留一个核心
registerDoParallel(cl)

# 设置训练控制
train_control <- trainControl(
  method = "cv",  # 使用交叉验证
  number = 10,    # 10折交叉验证
  allowParallel = TRUE  # 允许并行计算
)

# 定义要尝试的梯度提升模型参数网格
tune_grid <- expand.grid(
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(10, 20),
  shrinkage = c(0.001, 0.005),
  n.trees = 100  # 固定树的数量为100
)

# 训练梯度提升模型
gbm_model_3months <- train(
  x = X_scaled_3months, 
  y = Y_3months, 
  method = "gbm", 
  tuneGrid = tune_grid,
  trControl = train_control,
  verbose = FALSE
)

# 关闭并行处理
stopCluster(cl)
registerDoSEQ()

# 打印模型输出
print(gbm_model_3months)

# 可视化模型的性能
results_3months <- gbm_model_3months$results
plot_3months <- ggplot(results_3months, aes(x = interaction.depth, y = RMSE, color = factor(n.minobsinnode), shape = factor(shrinkage))) +
  geom_point(size = 4) +
  scale_color_manual(values = c("red", "blue"), labels = c("10", "20")) +
  scale_shape_manual(values = c(17, 15), labels = c("0.001", "0.005")) +
  labs(
    x = "Depth of Tree",
    y = "RMSE",
    title = "Model Performance by Tree Depth and Parameters",
    color = "Min. Obs in Node",
    shape = "Shrinkage"
  ) +
  theme_minimal()

# 显示图
print(plot_3months)



# 注册并行后台
numCores <- detectCores()
cl <- makeCluster(numCores - 1)  # 保留一个核心
registerDoParallel(cl)

# 设置训练控制
train_control <- trainControl(
  method = "cv",  # 使用交叉验证
  number = 10,    # 10折交叉验证
  allowParallel = TRUE  # 允许并行计算
)

# 定义要尝试的梯度提升模型参数网格
tune_grid <- expand.grid(
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(10, 20),
  shrinkage = c(0.001, 0.005),
  n.trees = 100  # 固定树的数量为100
)

# 训练梯度提升模型
gbm_model_12months <- train(
  x = X_scaled_12months, 
  y = Y_12months, 
  method = "gbm", 
  tuneGrid = tune_grid,
  trControl = train_control,
  verbose = FALSE
)

# 关闭并行处理
stopCluster(cl)
registerDoSEQ()

# 打印模型输出
print(gbm_model_12months)

# 可视化模型的性能
results_12months <- gbm_model_12months$results
plot_12months <- ggplot(results_12months, aes(x = interaction.depth, y = RMSE, color = factor(n.minobsinnode), shape = factor(shrinkage))) +
  geom_point(size = 4) +
  scale_color_manual(values = c("red", "blue"), labels = c("10", "20")) +
  scale_shape_manual(values = c(17, 15), labels = c("0.001", "0.005")) +
  labs(
    x = "Depth of Tree",
    y = "RMSE",
    title = "Model Performance by Tree Depth and Parameters",
    color = "Min. Obs in Node",
    shape = "Shrinkage"
  ) +
  theme_minimal()

# 显示图
print(plot_12months)