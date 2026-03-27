# =============================================================
# MẠNG NƠ-RON NHẬN DẠNG CHỮ SỐ VIẾT TAY – MNIST
# Kiến trúc : Input(784) -> Hidden1(128) -> Hidden2(64) -> Output(10)
# Hàm kích hoạt: ReLU (lớp ẩn) + Softmax (lớp output)
# Hàm lỗi    : Cross-Entropy Loss
# Tối ưu hóa : Mini-batch Gradient Descent + Early Stopping
# Dữ liệu    : MNIST (tải tự động qua package dslabs hoặc keras)
# =============================================================


# ================================================================
# PHẦN 0: CÀI ĐẶT VÀ TẢI PACKAGE
# ================================================================
# Cài lần đầu (bỏ comment nếu chưa cài):
# install.packages("dslabs")    # chứa dataset MNIST nhỏ gọn
# install.packages("ggplot2")
# install.packages("reshape2")  # dùng để vẽ confusion matrix dạng heatmap

library(dslabs)    # mnist dataset
library(ggplot2)
library(reshape2)  # melt() để chuyển ma trận sang long format cho ggplot2


# ================================================================
# PHẦN 1: TẢI VÀ TIỀN XỬ LÝ DỮ LIỆU MNIST
# ================================================================
# MNIST là bộ dữ liệu chuẩn trong học máy gồm:
#   - 60.000 ảnh xám kích thước 28x28 pixel để huấn luyện
#   - 10.000 ảnh để kiểm tra
#   - Mỗi ảnh là chữ số từ 0 đến 9 (10 lớp)
#   - Mỗi pixel có giá trị từ 0 (đen) đến 255 (trắng)
#
# Sau khi load, mỗi ảnh được "trải phẳng" (flatten) thành
# vector 784 chiều (28 * 28 = 784) để đưa vào mạng.

cat("Dang tai du lieu MNIST...\n")
mnist <- read_mnist()   # tải từ package dslabs (tự động download lần đầu)

# --- Tách train / test gốc ---
X_train_full <- mnist$train$images   # shape: (60000, 784), giá trị 0–255
y_train_full <- mnist$train$labels   # vector số nguyên 0–9, dài 60000

X_test  <- mnist$test$images         # shape: (10000, 784)
y_test  <- mnist$test$labels         # vector số nguyên 0–9, dài 10000

# --- Chuẩn hóa pixel về khoảng [0, 1] ---
# Chia cho 255 giúp:
#   1. Gradient không bị quá lớn/nhỏ khi huấn luyện
#   2. Tăng tốc hội tụ của gradient descent
#   3. Giảm nguy cơ overflow trong hàm exp() của softmax
X_train_full <- X_train_full / 255
X_test       <- X_test  / 255

cat(sprintf("Train full : %d mau, %d features\n", nrow(X_train_full), ncol(X_train_full)))
cat(sprintf("Test       : %d mau, %d features\n", nrow(X_test),  ncol(X_test)))

# --- Dùng tập con nhỏ hơn để chạy nhanh (tùy chọn) ---
# Với toàn bộ 60k mẫu và 128 ẩn, mỗi epoch mất ~10–30 giây trên CPU.
# Đặt USE_SUBSET = FALSE để dùng toàn bộ dữ liệu (khuyến nghị cho kết quả thực tế).
USE_SUBSET  <- TRUE
SUBSET_SIZE <- 10000   # số mẫu lấy từ train_full (bao gồm cả val)

if (USE_SUBSET) {
  set.seed(42)
  idx          <- sample(nrow(X_train_full), SUBSET_SIZE)
  X_train_full <- X_train_full[idx, ]
  y_train_full <- y_train_full[idx]
  cat(sprintf("Su dung subset: %d mau\n", SUBSET_SIZE))
}

# ================================================================
# PHẦN 2: CHIA TRAIN / VALIDATION / TEST
# ================================================================
# Chiến lược chia dữ liệu:
#   Train      : 80% train_full – dùng để cập nhật trọng số
#   Validation : 20% train_full – theo dõi overfitting, early stopping
#   Test       : tập riêng biệt – đánh giá cuối cùng, KHÔNG được nhìn trong lúc train
#
# Tại sao cần 3 tập?
#   - Nếu chỉ dùng train+test: ta sẽ vô tình "tune" model theo test set
#     → ước lượng hiệu năng thực tế bị lạc quan (data leakage)
#   - Validation giúp chọn hyperparameter và early stopping mà không
#     "dùng" test set, giữ test set thực sự "unseen"

set.seed(123)
n_total  <- nrow(X_train_full)
val_ratio <- 0.2

val_idx   <- sample(n_total, floor(n_total * val_ratio))
train_idx <- setdiff(seq_len(n_total), val_idx)

X_train <- X_train_full[train_idx, ]
y_train <- y_train_full[train_idx]

X_val   <- X_train_full[val_idx, ]
y_val   <- y_train_full[val_idx]

cat(sprintf("\nTap Train      : %d mau (%.0f%%)\n", nrow(X_train), (1 - val_ratio) * 100))
cat(sprintf("Tap Validation : %d mau (%.0f%%)\n",  nrow(X_val),   val_ratio * 100))
cat(sprintf("Tap Test       : %d mau (rieng biet)\n\n", nrow(X_test)))


# ================================================================
# PHẦN 3: CHUYỂN NHÃN SANG ONE-HOT ENCODING
# ================================================================
# Mạng có 10 đầu ra (softmax), mỗi đầu ra là xác suất thuộc về 1 chữ số.
# Hàm lỗi cross-entropy yêu cầu nhãn dạng one-hot:
#   Nhãn 3  →  [0, 0, 0, 1, 0, 0, 0, 0, 0, 0]
#   Nhãn 7  →  [0, 0, 0, 0, 0, 0, 0, 1, 0, 0]
#
# Ma trận one-hot có shape (n_samples, 10).

to_one_hot <- function(labels, num_classes = 10) {
  n   <- length(labels)
  mat <- matrix(0, nrow = n, ncol = num_classes)
  # Gán 1 vào đúng cột tương ứng với nhãn
  # labels + 1 vì R đánh index từ 1, nhãn MNIST từ 0
  mat[cbind(seq_len(n), labels + 1)] <- 1
  mat
}

Y_train <- to_one_hot(y_train)   # shape: (n_train, 10)
Y_val   <- to_one_hot(y_val)     # shape: (n_val,   10)
Y_test  <- to_one_hot(y_test)    # shape: (10000,   10)


# ================================================================
# PHẦN 4: HÀM KÍCH HOẠT
# ================================================================

# --- ReLU (Rectified Linear Unit) ---
# Công thức: ReLU(x) = max(0, x)
# Ưu điểm so với Sigmoid cho lớp ẩn:
#   - Không bị vanishing gradient khi x > 0 (đạo hàm = 1)
#   - Tính toán rất nhanh (chỉ so sánh với 0)
#   - Thường hội tụ nhanh hơn trong thực tế
# Nhược điểm:
#   - "Dying ReLU": neuron có thể "chết" (output luôn = 0) nếu
#     nhận input âm liên tục → thường dùng learning rate nhỏ
relu <- function(x) {
  pmax(0, x)   # pmax: element-wise max so với 0
}

# --- Đạo hàm ReLU ---
# ReLU'(x) = 1 nếu x > 0, = 0 nếu x <= 0
# Dùng trong backpropagation để tính gradient qua lớp ẩn
relu_derivative <- function(x) {
  (x > 0) * 1   # trả về ma trận 0/1 cùng kích thước với x
}

# --- Softmax ---
# Công thức: softmax(z_i) = exp(z_i) / sum(exp(z_j))
# Mục đích: chuyển vector số thực thành phân phối xác suất (tổng = 1)
# Dùng ở lớp output để dự đoán xác suất thuộc từng lớp (0–9)
#
# Trick ổn định số học: trừ max trước khi exp()
#   softmax(z) = softmax(z - max(z))  (kết quả giống nhau)
#   → tránh overflow khi z rất lớn (exp(1000) = Inf trong R)
softmax <- function(z) {
  # z có shape (n_samples, 10)
  z_shifted <- z - apply(z, 1, max)          # trừ max theo từng hàng (mẫu)
  exp_z     <- exp(z_shifted)
  exp_z / rowSums(exp_z)                     # chia cho tổng theo từng hàng
}


# ================================================================
# PHẦN 5: HÀM LỖI CROSS-ENTROPY
# ================================================================
# Cross-Entropy Loss phù hợp với bài toán phân loại nhiều lớp + softmax:
#   L = -mean( sum_k [ y_k * log(y_hat_k) ] )
#
# Trong đó:
#   y_k     : nhãn one-hot (1 hoặc 0)
#   y_hat_k : xác suất dự đoán từ softmax (0 < y_hat_k < 1)
#
# Tại sao không dùng MSE cho bài phân loại?
#   - MSE + Softmax tạo ra gradient phức tạp, hội tụ chậm
#   - Cross-Entropy + Softmax cho gradient đơn giản: (y_hat - y)
#     → đây là lý do backprop lớp output rất gọn

cross_entropy_loss <- function(Y_hat, Y) {
  # Clip để tránh log(0) = -Inf
  eps   <- 1e-15
  Y_hat <- pmax(pmin(Y_hat, 1 - eps), eps)
  -mean(rowSums(Y * log(Y_hat)))
}


# ================================================================
# PHẦN 6: KHỞI TẠO THAM SỐ MẠNG (XAVIER INITIALIZATION)
# ================================================================
# Kiến trúc mạng:
#   Input  (784)  →  Hidden1 (128)  →  Hidden2 (64)  →  Output (10)
#
# Xavier / Glorot Initialization:
#   W ~ Uniform[-sqrt(6/(fan_in+fan_out)), +sqrt(6/(fan_in+fan_out))]
#
# Tại sao không dùng runif()?
#   - Nếu trọng số quá lớn: gradient explode (đặc biệt với nhiều lớp)
#   - Nếu trọng số quá nhỏ: gradient vanish, mạng học rất chậm
#   - Xavier giữ phương sai activation ổn định qua các lớp
#
# Bias khởi tạo bằng 0 là thông lệ phổ biến (không gây symmetry breaking
# vì trọng số đã khác nhau)

nn_init_mnist <- function(n_input = 784, n_h1 = 128, n_h2 = 64, n_output = 10) {
  
  xavier <- function(fan_in, fan_out) {
    limit <- sqrt(6 / (fan_in + fan_out))
    matrix(runif(fan_in * fan_out, -limit, limit), nrow = fan_in, ncol = fan_out)
  }
  
  list(
    # Lớp 1: Input -> Hidden1
    W1 = xavier(n_input, n_h1),          # shape: (784, 128)
    b1 = matrix(0, nrow = 1, ncol = n_h1),   # shape: (1, 128) – broadcast theo batch
    
    # Lớp 2: Hidden1 -> Hidden2
    W2 = xavier(n_h1, n_h2),             # shape: (128, 64)
    b2 = matrix(0, nrow = 1, ncol = n_h2),   # shape: (1, 64)
    
    # Lớp 3: Hidden2 -> Output
    W3 = xavier(n_h2, n_output),         # shape: (64, 10)
    b3 = matrix(0, nrow = 1, ncol = n_output) # shape: (1, 10)
  )
}


# ================================================================
# PHẦN 7: LAN TRUYỀN THUẬN (FEEDFORWARD)
# ================================================================
# Luồng tính toán:
#
#   Z1 = X  %*% W1 + b1  (broadcast b1 theo n_batch hàng)
#   A1 = ReLU(Z1)         shape: (batch, 128)
#
#   Z2 = A1 %*% W2 + b2
#   A2 = ReLU(Z2)         shape: (batch, 64)
#
#   Z3 = A2 %*% W3 + b3
#   A3 = Softmax(Z3)      shape: (batch, 10)  ← xác suất dự đoán

feedforward_mnist <- function(params, X) {
  # Lớp 1
  Z1 <- X  %*% params$W1 + matrix(1, nrow(X), 1) %*% params$b1
  A1 <- relu(Z1)
  
  # Lớp 2
  Z2 <- A1 %*% params$W2 + matrix(1, nrow(A1), 1) %*% params$b2
  A2 <- relu(Z2)
  
  # Lớp 3 (output)
  Z3 <- A2 %*% params$W3 + matrix(1, nrow(A2), 1) %*% params$b3
  A3 <- softmax(Z3)
  
  # Trả về cả các giá trị trung gian để dùng trong backprop
  list(Z1 = Z1, A1 = A1,
       Z2 = Z2, A2 = A2,
       Z3 = Z3, A3 = A3)
}


# ================================================================
# PHẦN 8: LAN TRUYỀN NGƯỢC (BACKPROPAGATION)
# ================================================================
# Áp dụng chain rule từ lớp output ngược về lớp đầu vào.
#
# --- Lớp Output (Softmax + Cross-Entropy) ---
# Gradient kết hợp của Cross-Entropy và Softmax cực kỳ gọn:
#   dL/dZ3 = (A3 - Y) / n_batch
# (Đây là lý do Softmax + Cross-Entropy được dùng cùng nhau)
#
# --- Lớp Ẩn (ReLU) ---
# dL/dZ_k = dL/dA_k * ReLU'(Z_k)
# Trong đó ReLU'(Z_k) = 1 nếu Z_k > 0, = 0 nếu Z_k <= 0
#
# --- Gradient trọng số và bias ---
# dL/dW_k = A_{k-1}.T %*% dZ_k        (A_{k-1} là input của lớp k)
# dL/db_k = colMeans(dZ_k)            (trung bình theo batch)
#
# --- L2 Regularization (Weight Decay) ---
# Thêm penalty vào gradient: dL/dW += lambda * W
# Mục đích: phạt trọng số lớn, giảm overfitting
# lambda = 0 để tắt regularization

backprop_mnist <- function(params, cache, X, Y, lr = 0.01, lambda = 1e-4) {
  n <- nrow(X)   # kích thước batch hiện tại
  
  # ---- Gradient lớp 3 (Output) ----
  dZ3 <- (cache$A3 - Y) / n       # shape: (n, 10)
  dW3 <- t(cache$A2) %*% dZ3 + lambda * params$W3
  db3 <- matrix(colMeans(dZ3), nrow = 1)
  
  # ---- Gradient lớp 2 (Hidden2) ----
  dA2 <- dZ3 %*% t(params$W3)
  dZ2 <- dA2 * relu_derivative(cache$Z2)
  dW2 <- t(cache$A1) %*% dZ2 + lambda * params$W2
  db2 <- matrix(colMeans(dZ2), nrow = 1)
  
  # ---- Gradient lớp 1 (Hidden1) ----
  dA1 <- dZ2 %*% t(params$W2)
  dZ1 <- dA1 * relu_derivative(cache$Z1)
  dW1 <- t(X) %*% dZ1 + lambda * params$W1
  db1 <- matrix(colMeans(dZ1), nrow = 1)
  
  # ---- Cập nhật trọng số (Gradient Descent) ----
  # W_new = W_old - lr * dW
  # (Trừ vì muốn đi ngược chiều gradient để giảm loss)
  params$W1 <- params$W1 - lr * dW1
  params$b1 <- params$b1 - lr * db1
  params$W2 <- params$W2 - lr * dW2
  params$b2 <- params$b2 - lr * db2
  params$W3 <- params$W3 - lr * dW3
  params$b3 <- params$b3 - lr * db3
  
  params
}


# ================================================================
# PHẦN 9: EARLY STOPPING (Tái sử dụng từ XOR, đã tổng quát hóa)
# ================================================================
early_stopping_init <- function(patience = 10, min_delta = 1e-4) {
  list(
    patience      = patience,
    min_delta     = min_delta,
    best_loss     = Inf,
    best_params   = NULL,   # lưu trọng số tốt nhất để khôi phục sau khi dừng
    counter       = 0,
    stopped_epoch = NA
  )
}

early_stopping_check <- function(es, current_loss, epoch, params) {
  if (current_loss < es$best_loss - es$min_delta) {
    es$best_loss   <- current_loss
    es$best_params <- params   # snapshot trọng số tốt nhất
    es$counter     <- 0
  } else {
    es$counter <- es$counter + 1
  }
  
  should_stop <- es$counter >= es$patience
  if (should_stop) es$stopped_epoch <- epoch
  list(es = es, stop = should_stop)
}


# ================================================================
# PHẦN 10: HUẤN LUYỆN VỚI MINI-BATCH GRADIENT DESCENT
# ================================================================
# Mini-batch GD là trung gian giữa:
#   - Batch GD    : dùng toàn bộ dữ liệu → ổn định nhưng chậm mỗi bước
#   - Stochastic  : dùng 1 mẫu → nhanh nhưng nhiễu, dao động nhiều
#   - Mini-batch  : dùng BATCH_SIZE mẫu → cân bằng tốc độ và ổn định
#
# Mỗi epoch:
#   1. Xáo trộn (shuffle) dữ liệu train ngẫu nhiên
#      → tránh model học theo thứ tự cố định (bias)
#   2. Chia thành các mini-batch có kích thước BATCH_SIZE
#   3. Với mỗi mini-batch: feedforward → tính loss → backprop → cập nhật
#   4. Loss train = trung bình loss của tất cả mini-batch trong epoch
#   5. Sau mỗi epoch: tính loss validation → kiểm tra early stopping

# --- Siêu tham số ---
set.seed(42)
LEARNING_RATE <- 0.05    # bước học: quá lớn → không hội tụ, quá nhỏ → chậm
BATCH_SIZE    <- 256     # kích thước mini-batch
EPOCHS        <- 50      # số vòng lặp tối đa
LAMBDA        <- 1e-4    # hệ số L2 regularization
PATIENCE      <- 8       # early stopping patience (số epoch chờ)
MIN_DELTA     <- 1e-4    # ngưỡng cải thiện tối thiểu

# --- Khởi tạo ---
params <- nn_init_mnist(n_input = 784, n_h1 = 128, n_h2 = 64, n_output = 10)
es     <- early_stopping_init(patience = PATIENCE, min_delta = MIN_DELTA)

train_loss_hist <- numeric(EPOCHS)
val_loss_hist   <- numeric(EPOCHS)
actual_epochs   <- EPOCHS

cat("Bat dau huan luyen MNIST...\n")
cat(strrep("=", 58), "\n")
cat(sprintf("%-8s %-16s %-16s %-10s\n", "Epoch", "Train Loss", "Val Loss", "ES Counter"))
cat(strrep("-", 58), "\n")

for (epoch in 1:EPOCHS) {
  
  # --- Bước 1: Shuffle dữ liệu train ---
  shuffle_idx <- sample(nrow(X_train))
  X_shuf      <- X_train[shuffle_idx, ]
  Y_shuf      <- Y_train[shuffle_idx, ]
  
  # --- Bước 2: Mini-batch training ---
  n_batches    <- floor(nrow(X_shuf) / BATCH_SIZE)
  batch_losses <- numeric(n_batches)
  
  for (b in 1:n_batches) {
    start <- (b - 1) * BATCH_SIZE + 1
    end   <- min(b * BATCH_SIZE, nrow(X_shuf))
    
    X_batch <- X_shuf[start:end, ]
    Y_batch <- Y_shuf[start:end, ]
    
    # Forward pass
    cache <- feedforward_mnist(params, X_batch)
    
    # Tính loss cho batch này
    batch_losses[b] <- cross_entropy_loss(cache$A3, Y_batch)
    
    # Backward pass + cập nhật trọng số
    params <- backprop_mnist(params, cache, X_batch, Y_batch,
                             lr = LEARNING_RATE, lambda = LAMBDA)
  }
  
  # --- Bước 3: Tính loss toàn bộ train và validation ---
  train_loss_hist[epoch] <- mean(batch_losses)
  
  val_cache             <- feedforward_mnist(params, X_val)
  val_loss_hist[epoch]  <- cross_entropy_loss(val_cache$A3, Y_val)
  
  # --- Bước 4: In tiến trình ---
  cat(sprintf("Epoch %2d/%d | Train: %.4f | Val: %.4f | Counter: %d\n",
              epoch, EPOCHS,
              train_loss_hist[epoch],
              val_loss_hist[epoch],
              es$counter))
  
  # --- Bước 5: Kiểm tra Early Stopping ---
  # Theo dõi VAL LOSS (không phải train loss) để tránh overfitting
  check <- early_stopping_check(es, val_loss_hist[epoch], epoch, params)
  es    <- check$es
  
  if (check$stop) {
    actual_epochs <- epoch
    cat(strrep("=", 58), "\n")
    cat(sprintf(">> Early Stopping tai epoch %d\n", epoch))
    cat(sprintf("   Val Loss tot nhat : %.4f (epoch %d)\n",
                es$best_loss, epoch - PATIENCE))
    # Khôi phục trọng số tốt nhất (trước khi val loss tăng trở lại)
    params <- es$best_params
    cat("   Da khoi phuc trong so tot nhat.\n")
    break
  }
}

# Cắt phần chưa điền
train_loss_hist <- train_loss_hist[1:actual_epochs]
val_loss_hist   <- val_loss_hist[1:actual_epochs]

if (is.na(es$stopped_epoch)) {
  cat(strrep("=", 58), "\n")
  cat(sprintf("Hoan thanh %d epochs (khong kich hoat Early Stopping)\n", EPOCHS))
}


# ================================================================
# PHẦN 11: ĐÁNH GIÁ TRÊN TẬP TEST
# ================================================================
# Chú ý: chỉ đánh giá trên test set MỘT LẦN DUY NHẤT sau khi
#         huấn luyện xong, để có ước lượng khách quan nhất.

cat("\nDang danh gia tren tap Test...\n")

# Forward pass trên toàn bộ test set
test_cache  <- feedforward_mnist(params, X_test)
Y_hat_test  <- test_cache$A3                         # xác suất (10000, 10)

# Dự đoán nhãn: lấy lớp có xác suất cao nhất (argmax)
# which.max() cho 1 hàng; apply(..., 1, which.max) cho toàn ma trận
# Trừ 1 vì R index từ 1, nhãn MNIST từ 0
y_pred_test <- apply(Y_hat_test, 1, which.max) - 1   # vector (10000,)
y_true_test <- y_test

# ---- Accuracy ----
accuracy_test <- mean(y_pred_test == y_true_test)

# ---- Accuracy từng lớp (per-class) ----
per_class_acc <- sapply(0:9, function(cls) {
  idx <- y_true_test == cls
  mean(y_pred_test[idx] == y_true_test[idx])
})

# ---- Confusion Matrix ----
# cm[i, j] = số mẫu thực sự là lớp i nhưng bị dự đoán là lớp j
# Đường chéo chính = dự đoán đúng
# Ngoài đường chéo = nhầm lẫn giữa các chữ số
cm <- table(True      = y_true_test,
            Predicted = y_pred_test)

# ---- Test Loss ----
test_loss <- cross_entropy_loss(Y_hat_test, Y_test)

# ---- In kết quả ----
cat(strrep("=", 52), "\n")
cat("  KET QUA DANH GIA TREN TAP TEST\n")
cat(strrep("=", 52), "\n")

metrics_df <- data.frame(
  Chi_so  = c(
    "Test Accuracy",
    "Test Loss (Cross-Entropy)",
    "Val Loss tot nhat",
    "So epoch thuc chay",
    "Accuracy chu so 0",
    "Accuracy chu so 1",
    "Accuracy chu so 2",
    "Accuracy chu so 3",
    "Accuracy chu so 4",
    "Accuracy chu so 5",
    "Accuracy chu so 6",
    "Accuracy chu so 7",
    "Accuracy chu so 8",
    "Accuracy chu so 9"
  ),
  Gia_tri = c(
    sprintf("%.4f  (%.2f%%)", accuracy_test, accuracy_test * 100),
    sprintf("%.4f",           test_loss),
    sprintf("%.4f",           es$best_loss),
    sprintf("%d / %d",        actual_epochs, EPOCHS),
    sprintf("%.2f%%", per_class_acc[1] * 100),
    sprintf("%.2f%%", per_class_acc[2] * 100),
    sprintf("%.2f%%", per_class_acc[3] * 100),
    sprintf("%.2f%%", per_class_acc[4] * 100),
    sprintf("%.2f%%", per_class_acc[5] * 100),
    sprintf("%.2f%%", per_class_acc[6] * 100),
    sprintf("%.2f%%", per_class_acc[7] * 100),
    sprintf("%.2f%%", per_class_acc[8] * 100),
    sprintf("%.2f%%", per_class_acc[9] * 100),
    sprintf("%.2f%%", per_class_acc[10] * 100)
  )
)
print(metrics_df, row.names = FALSE)
cat(strrep("=", 52), "\n")


# ================================================================
# PHẦN 12: VẼ ĐỒ THỊ
# ================================================================

# ---- 12A: Loss Curve – Train vs Validation ----
# Phân tích đồ thị:
#   - Train loss và val loss đều giảm → mô hình đang học tốt
#   - Val loss bắt đầu tăng trong khi train loss vẫn giảm → overfitting
#   - Early stopping dừng tại điểm val loss tốt nhất

df_loss <- data.frame(
  epoch      = rep(1:actual_epochs, 2),
  loss       = c(train_loss_hist, val_loss_hist),
  tap        = rep(c("Train", "Validation"), each = actual_epochs)
)

p_loss <- ggplot(df_loss, aes(x = epoch, y = loss, color = tap, linetype = tap)) +
  geom_line(linewidth = 0.9) +
  geom_point(data = subset(df_loss, epoch == actual_epochs),
             size = 2.5, shape = 19) +
  {if (!is.na(es$stopped_epoch))
    geom_vline(xintercept = es$stopped_epoch,
               color = "tomato", linetype = "dashed",
               linewidth = 0.75, alpha = 0.8)} +
  scale_color_manual(values = c("Train" = "steelblue", "Validation" = "coral3")) +
  scale_linetype_manual(values = c("Train" = "solid", "Validation" = "dashed")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  labs(
    title    = "Loss Function: Train vs Validation",
    subtitle = sprintf("MNIST Neural Network  |  Best Val Loss: %.4f  |  Epochs: %d/%d",
                       es$best_loss, actual_epochs, EPOCHS),
    x        = "Epoch",
    y        = "Cross-Entropy Loss",
    color    = "Tap du lieu",
    linetype = "Tap du lieu",
    caption  = ifelse(!is.na(es$stopped_epoch),
                      paste("Early Stopping tai epoch", es$stopped_epoch),
                      "Chay du so epoch")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray45", size = 11),
    plot.caption  = element_text(color = "tomato", size = 10),
    legend.position = "top"
  )

print(p_loss)


# ---- 12B: Confusion Matrix Heatmap ----
# Đọc confusion matrix:
#   - Màu đậm trên đường chéo → dự đoán đúng nhiều
#   - Ô ngoài đường chéo có màu → các cặp chữ số hay bị nhầm
#   - VD: 4 nhầm với 9, 3 nhầm với 8 là thường thấy trong MNIST

cm_df     <- as.data.frame(cm)
# Thêm cột tỷ lệ: số lần nhầm / tổng mẫu thực của lớp đó
cm_df$pct <- with(cm_df, {
  total_per_class <- tapply(Freq, True, sum)
  Freq / total_per_class[as.character(True)] * 100
})

p_cm <- ggplot(cm_df, aes(x = Predicted, y = True, fill = pct)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = ifelse(Freq > 0,
                               sprintf("%d\n(%.1f%%)", Freq, pct),
                               "")),
            size = 2.8, color = "gray20") +
  scale_fill_gradient(low = "#EAF3DE", high = "#3B6D11",
                      name = "% trong\ntu\u0300ng lo\u01a1\u0301p") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    title    = "Confusion Matrix – MNIST Test Set",
    subtitle = sprintf("Test Accuracy: %.2f%%  |  %d mau test",
                       accuracy_test * 100, nrow(X_test)),
    x        = "Du doan (Predicted)",
    y        = "Thuc te (True)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray45", size = 11),
    panel.grid    = element_blank(),
    axis.ticks    = element_blank(),
    legend.position = "right"
  )

print(p_cm)


# ---- 12C: Per-class Accuracy Bar Chart ----
df_class <- data.frame(
  chu_so   = factor(0:9),
  accuracy = per_class_acc * 100
)

p_class <- ggplot(df_class, aes(x = chu_so, y = accuracy, fill = accuracy)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = sprintf("%.1f%%", accuracy)),
            vjust = -0.5, size = 3.5, fontface = "bold") +
  geom_hline(yintercept = accuracy_test * 100,
             color = "tomato", linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = 10.2, y = accuracy_test * 100,
           label = sprintf("TB: %.1f%%", accuracy_test * 100),
           color = "tomato", hjust = 1, size = 3.5) +
  scale_fill_gradient(low = "#B5D4F4", high = "#0C447C", guide = "none") +
  scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) +
  labs(
    title    = "Accuracy theo tung chu so (0–9)",
    subtitle = "Duong ke do = accuracy trung binh tren toan bo test set",
    x        = "Chu so",
    y        = "Accuracy (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray45", size = 11),
    panel.grid.major.x = element_blank()
  )

print(p_class)

cat("\nHoan tat! Da ve 3 bieu do:\n")
cat("  1. Loss curve (Train vs Validation)\n")
cat("  2. Confusion Matrix heatmap\n")
cat("  3. Per-class Accuracy\n")