#用于预测步数对时间的滞后效应，以分析实际数据中误差数据数量对滞后相关性的影响
#通过滞后步长（lag）的方式，利用LSTM（长短期记忆）神经网络捕捉时间序列的滞后效应，并通过注意力机制增强模型的预测能力。
library(keras)
library(tensorflow)
library(dplyr)
library(reticulate)
#py_config()
#virtualenv_create(envname = "r-tensorflow")
#use_virtualenv("r-tensorflow", required = TRUE)
#py_install(packages = c("tensorflow-macos", "tensorflow-metal"))

#step_data: 包含步数信息，每分钟一次。
#heart_data: 包含心率信息，每五秒一次。->需进行时间对齐
load("~/Desktop/multi-omics/data_smooth/data/step_data.rda")   
load("~/Desktop/multi-omics/data_smooth/data/heart_data.rda") 

#对时间序列进行对齐，使用平均值对心率数据进行插值
aligned_data <- step_data %>%
  left_join(heart_data, by = "time") %>%
  tidyr::fill(heart, .direction = "downup")  #最近邻插值法

#归一化数据，转换为转换为均值为0，标准差为1的标准正态分布 为LSTM作准备
x_data <- scale(aligned_data$step)
y_data <- scale(aligned_data$heart)
time_steps <- length(x_data)

#创建滞后数据，输入 X 应该有多个滞后时间点
lag <- 10  # 滞后步长
x_train <- array(data = NA, dim = c(time_steps - lag, lag, 1)) #构造模型的输入，使用 lag 个历史时间点的步数数据构成输入样本。
y_train <- y_data[(lag + 1):time_steps] #构造模型的输出，即滞后步长之后的心率数据。

for (i in 1:(time_steps - lag)) {
  x_train[i, , ] <- x_data[i:(i + lag - 1)]
}

#构建 LSTM 模型
input <- layer_input(shape = c(lag, 1)) #lag个时间步长 1个特征（步数）

lstm_layer <- input %>%
  layer_lstm(units = 64, return_sequences = TRUE) %>%
  layer_dropout(rate = 0.2) %>%
  layer_lstm(units = 32, return_sequences = FALSE) %>%
  layer_dropout(rate = 0.2)
#layer_lstm(units = 64)：第一个LSTM层，包含64个单元（隐藏层神经元），return_sequences = TRUE 表示该层输出每个时间步长的隐藏状态，适合在后续的LSTM层继续处理序列。
#layer_dropout(rate = 0.2)：Dropout层，随机丢弃20%的神经元，防止模型过拟合。
#layer_lstm(units = 32, return_sequences = FALSE)：第二个LSTM层，包含32个单元，return_sequences = FALSE 表示该层只输出最后一个时间步长的隐藏状态，适合之后的全连接层处理。

# 添加注意力机制
#attention_weights <- lstm_layer %>%
#layer_dense(units = 1, activation = "tanh") %>%
#layer_softmax()  # 使用 softmax 计算注意力权重
#context_vector <- layer_multiply(list(lstm_layer, attention_weights))

# 输出层，用于预测滞后相关性的目标值
output <- lstm_layer %>%
  layer_dense(units = 1, activation = "linear")

# 定义模型
model <- keras_model(inputs = input, outputs = output)

# 编译模型
model %>% compile(
  optimizer = 'adam',
  loss = 'mean_squared_error',
  metrics = c('mae')
)

#optimizer = 'adam'：使用 Adam 优化器，适合处理时间序列的复杂优化问题。
#metrics = c('mae')：使用平均绝对误差（MAE）作为评估指标。
#loss = 'mean_squared_error'：损失函数为均方误差（MSE），用于回归任务。

# 查看模型架构
summary(model)

# 训练模型
#用apple M1大概需要五分钟
#成功识别Apple M1 设备，并且配置了 Metal 作为 TensorFlow 使用的 GPU 设备。
#设备信息:
#设备: Apple M1
#系统内存: 8.00 GB
#最大缓存大小: 2.67 GB
#设备创建:
#设备: TensorFlow 设备 (/job:localhost/replica:0/task:0/device:GPU:0)
#设备类型: METAL（适用于 Apple 设备的 GPU 支持）
#验证 TensorFlow GPU 支持
#import tensorflow as tf
#print("Num GPUs Available: ", len(tf.config.list_physical_devices('GPU')))
history <- model %>% fit(
  x_train, y_train,
  epochs = 50, #训练50个周期
  batch_size = 32,
  validation_split = 0.2, #用20%的数据验证模型性能
  verbose = 1
)

# 评估模型
loss <- model %>% evaluate(x_train, y_train)
cat("模型的损失: ", loss, "\n")

# 预测滞后效应，使用训练好的模型
predictions <- model %>% predict(x_train)

# 可视化滞后效应预测结果
plot(y_train, type = "l", col = "blue", lwd = 2, ylab = "Heart Rate (Y)", xlab = "Time")
lines(predictions, col = "red", lwd = 2)
legend("topright", legend = c("Actual", "Predicted"), col = c("blue", "red"), lwd = 2)

# 保存到绝对路径下的子文件夹 models
model %>% save_model_tf("~/Desktop/multi-omics/DL_laggedcor/models/my_model.keras")


