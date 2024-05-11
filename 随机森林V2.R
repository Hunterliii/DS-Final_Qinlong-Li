#随机森林预测
#数据处理和lasso一样
if (!require(randomForest)) {
  install.packages("randomForest")
  library(randomForest)
}

# 可能还需要安装和加载ggplot2用于可视化
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require(caret)) {
  install.packages("caret")
  library(caret)
}

if (!require(doParallel)) {
  install.packages("doParallel")
  library(doParallel)
}


# 注册并行后台，设置使用的核心数
numCores <- parallel::detectCores()  # 检测可用的核心数
cl <- makeCluster(numCores - 1)  # 保留一个核心用于系统操作
registerDoParallel(cl)

# 设置训练控制，使用交叉验证
train_control <- trainControl(
  method = "cv",
  number = 10,
  allowParallel = TRUE  # 允许并行计算
)

# 定义要尝试的随机森林参数网格
tune_grid <- expand.grid(
  mtry = c(10, 100, 107, 115, 500, 574, 1000, 1148),  # 根据建议尝试的mtry值
  splitrule = c("variance"),  # 对回归任务常使用的分裂规则
  min.node.size = c(1, 5, 10)  # 最小节点大小
)

# 训练模型
rf_tuned_model <- train(
  x = X_scaled,
  y = Y,
  method = "ranger",  # 使用ranger包，对大数据集友好
  trControl = train_control,
  tuneGrid = tune_grid,
  metric = "RMSE",  # 使用均方根误差作为性能评估标准
  num.trees = 100  # 指定使用100棵树
)

# 停止并行后台
stopCluster(cl)
registerDoSEQ()  # 返回到单核运算

# 查看最佳模型的结果
print(rf_tuned_model)

# 绘制RMSE随mtry变化的图
performance_df <- rf_tuned_model$results

# 绘制模型性能图，不包含连线，点根据 min.node.size 区分颜色
ggplot(performance_df, aes(x = mtry, y = RMSE, color = factor(min.node.size))) +
  geom_point() +  # 只添加点，不添加线
  scale_color_brewer(palette = "Set1", name = "min.node.size") +  # 使用预定义的颜色板，并命名图例
  labs(title = "Model Performance Over mtry", x = "mtry", y = "RMSE") +
  theme_minimal()




# 注册并行后台，设置使用的核心数
numCores <- parallel::detectCores()  # 检测可用的核心数
cl <- makeCluster(numCores - 1)  # 保留一个核心用于系统操作
registerDoParallel(cl)

# 设置训练控制，使用交叉验证
train_control <- trainControl(
  method = "cv",
  number = 10,
  allowParallel = TRUE  # 允许并行计算
)

# 定义要尝试的随机森林参数网格
tune_grid <- expand.grid(
  mtry = c(10, 100, 107, 115, 500, 574, 1000, 1148),  # 根据建议尝试的mtry值
  splitrule = c("variance"),  # 对回归任务常使用的分裂规则
  min.node.size = c(1, 5, 10)  # 最小节点大小
)

# 训练模型
rf_tuned_model_3months <- train(
  x = X_scaled_3months,
  y = Y_3months,
  method = "ranger",  # 使用ranger包，对大数据集友好
  trControl = train_control,
  tuneGrid = tune_grid,
  metric = "RMSE",  # 使用均方根误差作为性能评估标准
  num.trees = 100  # 指定使用100棵树
)

# 停止并行后台
stopCluster(cl)
registerDoSEQ()  # 返回到单核运算

# 查看最佳模型的结果
print(rf_tuned_model_3months)

# 绘制RMSE随mtry变化的图
performance_df_3months <- rf_tuned_model_3months$results

# 绘制模型性能图，不包含连线，点根据 min.node.size 区分颜色
ggplot(performance_df_3months, aes(x = mtry, y = RMSE, color = factor(min.node.size))) +
  geom_point() +  # 只添加点，不添加线
  scale_color_brewer(palette = "Set1", name = "min.node.size") +  # 使用预定义的颜色板，并命名图例
  labs(title = "Model Performance Over mtry", x = "mtry", y = "RMSE") +
  theme_minimal()



# 注册并行后台，设置使用的核心数
numCores <- parallel::detectCores()  # 检测可用的核心数
cl <- makeCluster(numCores - 1)  # 保留一个核心用于系统操作
registerDoParallel(cl)

# 设置训练控制，使用交叉验证
train_control <- trainControl(
  method = "cv",
  number = 10,
  allowParallel = TRUE  # 允许并行计算
)

# 定义要尝试的随机森林参数网格
tune_grid <- expand.grid(
  mtry = c(10, 100, 107, 115, 500, 574, 1000, 1148),  # 根据建议尝试的mtry值
  splitrule = c("variance"),  # 对回归任务常使用的分裂规则
  min.node.size = c(1, 5, 10)  # 最小节点大小
)

# 训练模型
rf_tuned_model_12months <- train(
  x = X_scaled_12months,
  y = Y_12months,
  method = "ranger",  # 使用ranger包，对大数据集友好
  trControl = train_control,
  tuneGrid = tune_grid,
  metric = "RMSE",  # 使用均方根误差作为性能评估标准
  num.trees = 100  # 指定使用100棵树
)

# 停止并行后台
stopCluster(cl)
registerDoSEQ()  # 返回到单核运算

# 查看最佳模型的结果
print(rf_tuned_model_12months)

# 绘制RMSE随mtry变化的图
performance_df_12months <- rf_tuned_model_12months$results

# 绘制模型性能图，不包含连线，点根据 min.node.size 区分颜色
ggplot(performance_df_12months, aes(x = mtry, y = RMSE, color = factor(min.node.size))) +
  geom_point() +  # 只添加点，不添加线
  scale_color_brewer(palette = "Set1", name = "min.node.size") +  # 使用预定义的颜色板，并命名图例
  labs(title = "Model Performance Over mtry", x = "mtry", y = "RMSE") +
  theme_minimal()
