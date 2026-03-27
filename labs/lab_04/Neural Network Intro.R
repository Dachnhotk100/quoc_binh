# =============================================================
# MẠNG NƠ-RON HỌC BÀI TOÁN XOR 
# Kiến trúc: Input(2) -> Hidden(4) -> Output(1)
# Hàm kích hoạt: Sigmoid
# Thuật toán tối ưu: Gradient Descent + Early Stopping
# =============================================================


# ================================================================
# PHẦN 1: DỮ LIỆU ĐẦU VÀO
# ================================================================
# Bài toán XOR (Exclusive OR):
#   0 XOR 0 = 0  |  0 XOR 1 = 1
#   1 XOR 0 = 1  |  1 XOR 1 = 0
# Đây là bài toán KHÔNG tuyến tính, tức là không thể dùng
# một đường thẳng (perceptron đơn) để phân loại được.
# Cần ít nhất 1 lớp ẩn để giải quyết.

X <- matrix(c(0,0,
              0,1,
              1,0,
              1,1), nrow = 4, ncol = 2, byrow = TRUE)
# X có shape (4, 2): 4 mẫu, mỗi mẫu có 2 đặc trưng

y <- matrix(c(0, 1, 1, 0), nrow = 4, ncol = 1)
# y có shape (4, 1): nhãn tương ứng theo phép XOR


# ================================================================
# PHẦN 2: HÀM KÍCH HOẠT SIGMOID VÀ ĐẠO HÀM
# ================================================================

# --- Sigmoid ---
# Công thức: sigma(x) = 1 / (1 + e^(-x))
# Tính chất:
#   - Đầu ra luôn nằm trong khoảng (0, 1) -> phù hợp bài toán xác suất
#   - Khi x rất lớn: sigma(x) -> 1
#   - Khi x rất nhỏ: sigma(x) -> 0
#   - Điểm uốn tại x = 0: sigma(0) = 0.5
sigmoid <- function(x) {
  1 / (1 + exp(-x))
}

# --- Đạo hàm của Sigmoid ---
# Công thức: sigma'(x) = sigma(x) * (1 - sigma(x))
# Tính chất:
#   - Dùng trong backpropagation để tính gradient
#   - QUAN TRỌNG: tham số x ở đây là ĐẦU RA của sigmoid (đã tính trước),
#     KHÔNG phải đầu vào gốc -> tránh tính lại sigmoid lần nữa
#   - Đạo hàm lớn nhất tại sigma(x)=0.5, nhỏ dần về 2 đầu
#     -> gây ra vấn đề "vanishing gradient" khi mạng sâu
sigmoid_derivative <- function(x) {
  x * (1 - x)
}


# ================================================================
# PHẦN 3: KHỞI TẠO MẠNG NƠ-RON
# ================================================================
# Mạng có 3 lớp:
#   - Lớp đầu vào (Input layer) : 2 node  (2 đặc trưng của X)
#   - Lớp ẩn (Hidden layer)     : 4 node  (siêu tham số, có thể thay đổi)
#   - Lớp đầu ra (Output layer)  : 1 node  (xác suất thuộc lớp 1)
#
# Trọng số (weights):
#   - weights1: ma trận (2 x 4) – kết nối Input -> Hidden
#   - weights2: ma trận (4 x 1) – kết nối Hidden -> Output
#   - Khởi tạo ngẫu nhiên trong [0, 1] bằng runif()
#   - Không dùng 0 vì sẽ làm tất cả neuron học giống nhau (symmetry breaking)

nn_init <- function(x, y) {
  list(
    input    = x,
    weights1 = matrix(runif(ncol(x) * 4), nrow = ncol(x), ncol = 4),
    weights2 = matrix(runif(4 * 1),        nrow = 4,        ncol = 1),
    y        = y,
    output   = matrix(0, nrow = nrow(y), ncol = ncol(y)),
    layer1   = NULL   # sẽ được gán trong feedforward
  )
}


# ================================================================
# PHẦN 4: LAN TRUYỀN THUẬN (FEEDFORWARD)
# ================================================================
# Mục tiêu: Tính đầu ra của mạng dựa trên trọng số hiện tại
#
# Bước 1 – Tính đầu ra lớp ẩn (layer1):
#   z1     = X %*% weights1        (phép nhân ma trận, shape: 4x4)
#   layer1 = sigmoid(z1)           (áp dụng activation function)
#
# Bước 2 – Tính đầu ra lớp output:
#   z2     = layer1 %*% weights2   (shape: 4x1)
#   output = sigmoid(z2)           (xác suất dự đoán)
#
# Ký hiệu %*% trong R = phép nhân ma trận (dot product)
# Ký hiệu *  trong R = nhân từng phần tử (element-wise)

feedforward <- function(nn) {
  nn$layer1 <- sigmoid(nn$input  %*% nn$weights1)  # shape: 4x4
  nn$output <- sigmoid(nn$layer1 %*% nn$weights2)  # shape: 4x1
  nn
}


# ================================================================
# PHẦN 5: LAN TRUYỀN NGƯỢC (BACKPROPAGATION)
# ================================================================
# Mục tiêu: Tính gradient của hàm lỗi theo từng trọng số,
#           sau đó cập nhật trọng số theo hướng giảm lỗi.
#
# Hàm lỗi sử dụng: MSE = mean((y - output)^2)
#
# ---- Quy tắc chain rule (đạo hàm hợp) ----
#
# Gradient của lỗi theo weights2:
#   dL/dw2 = dL/dOutput * dOutput/dz2 * dz2/dw2
#           = 2*(y - output) * sigmoid'(output) * layer1.T
#   -> d_weights2 = layer1.T  %*%  [2*(y-output) * sigmoid'(output)]
#
# Gradient của lỗi theo weights1:
#   dL/dw1 = dL/dLayer1 * dLayer1/dz1 * dz1/dw1
#   Trong đó:
#     dL/dLayer1 = [2*(y-output) * sigmoid'(output)] %*% weights2.T
#   -> d_weights1 = input.T  %*%  (dL/dLayer1 * sigmoid'(layer1))
#
# Cập nhật trọng số (Gradient Descent, learning_rate = 1):
#   weights = weights + d_weights
#   (Cộng vì lỗi = y - output: gradient đang chỉ hướng TĂNG của output)

backprop <- function(nn) {
  # --- Tính delta tại lớp output ---
  # delta_output = 2*(y - output) * sigmoid'(output)  shape: 4x1
  delta_output <- 2 * (nn$y - nn$output) * sigmoid_derivative(nn$output)
  
  # --- Gradient theo weights2 ---
  # t(layer1) shape (4x4) %*% delta_output (4x1) = (4x1)
  d_weights2 <- t(nn$layer1) %*% delta_output
  
  # --- Lan truyền ngược qua lớp ẩn ---
  # delta_hidden = (delta_output %*% weights2.T) * sigmoid'(layer1)
  #   delta_output (4x1) %*% t(weights2) (1x4) = (4x4)
  #   nhân element-wise với sigmoid'(layer1) (4x4)
  delta_hidden <- (delta_output %*% t(nn$weights2)) * sigmoid_derivative(nn$layer1)
  
  # --- Gradient theo weights1 ---
  # t(input) shape (2x4) %*% delta_hidden (4x4) = (2x4)
  d_weights1 <- t(nn$input) %*% delta_hidden
  
  # --- Cập nhật trọng số ---
  nn$weights1 <- nn$weights1 + d_weights1
  nn$weights2 <- nn$weights2 + d_weights2
  nn
}


# ================================================================
# PHẦN 6: EARLY STOPPING
# ================================================================
# Vấn đề: Nếu huấn luyện quá nhiều epoch, mạng có thể bị
#         "overfitting" – học thuộc dữ liệu huấn luyện nhưng
#         tổng quát hóa kém trên dữ liệu mới.
#
# Giải pháp – Early Stopping:
#   Dừng huấn luyện sớm khi loss không cải thiện đáng kể
#   sau một số epoch liên tiếp (gọi là "patience").
#
# Cách hoạt động:
#   1. Theo dõi loss tốt nhất (best_loss) đã đạt được
#   2. Nếu loss hiện tại cải thiện > min_delta so với best_loss
#      -> cập nhật best_loss, đặt lại bộ đếm patience về 0
#   3. Nếu loss KHÔNG cải thiện đủ -> tăng bộ đếm counter lên 1
#   4. Khi counter vượt ngưỡng patience -> dừng huấn luyện
#
# Tham số:
#   patience  : số epoch chờ trước khi dừng (mặc định: 500)
#   min_delta : mức cải thiện tối thiểu được coi là có ý nghĩa (mặc định: 1e-6)

early_stopping_init <- function(patience = 500, min_delta = 1e-6) {
  list(
    patience      = patience,
    min_delta     = min_delta,
    best_loss     = Inf,   # khởi tạo bằng vô cực để bất kỳ loss nào cũng tốt hơn
    counter       = 0,     # đếm số epoch liên tiếp không cải thiện
    stopped_epoch = NA     # epoch thực sự dừng lại (NA nếu chạy đủ)
  )
}

# Kiểm tra điều kiện dừng sau mỗi epoch
# Trả về: list(es = trạng_thái_cập_nhật, stop = TRUE/FALSE)
early_stopping_check <- function(es, current_loss, epoch) {
  if (current_loss < es$best_loss - es$min_delta) {
    # Loss cải thiện đủ -> cập nhật best_loss và reset counter
    es$best_loss <- current_loss
    es$counter   <- 0
  } else {
    # Loss không cải thiện -> tăng counter
    es$counter <- es$counter + 1
  }
  
  should_stop <- es$counter >= es$patience
  if (should_stop) es$stopped_epoch <- epoch
  
  list(es = es, stop = should_stop)
}


# ================================================================
# PHẦN 7: HUẤN LUYỆN MẠNG
# ================================================================

set.seed(42)   # Đặt seed để kết quả tái lập được giữa các lần chạy

# --- Khởi tạo ---
nn             <- nn_init(X, y)
epochs         <- 10000
es             <- early_stopping_init(patience = 500, min_delta = 1e-6)
loss_history   <- numeric(epochs)
actual_epochs  <- epochs   # sẽ được cập nhật nếu dừng sớm

cat("Bat dau huan luyen...\n")
cat(strrep("=", 42), "\n")

for (i in 1:epochs) {
  
  # Bước 1: Lan truyền thuận -> tính output hiện tại
  nn <- feedforward(nn)
  
  # Bước 2: Lan truyền ngược -> cập nhật trọng số
  nn <- backprop(nn)
  
  # Bước 3: Tính và lưu MSE loss epoch này
  loss_history[i] <- mean((nn$y - nn$output)^2)
  
  # Bước 4: In tiến trình sau mỗi 1000 epochs
  if ((i - 1) %% 1000 == 0) {
    cat(sprintf("Epoch %5d | Loss = %.6f\n", i - 1, loss_history[i]))
  }
  
  # Bước 5: Kiểm tra Early Stopping
  check <- early_stopping_check(es, loss_history[i], epoch = i)
  es    <- check$es
  
  if (check$stop) {
    actual_epochs <- i
    cat(strrep("=", 42), "\n")
    cat(sprintf(">> Early Stopping tai epoch %d\n", i))
    cat(sprintf("   Loss tot nhat dat duoc : %.6f\n", es$best_loss))
    cat(sprintf("   Khong cai thien sau %d epochs lien tiep\n", es$patience))
    break
  }
}

# Cắt bỏ phần loss chưa điền (nếu dừng sớm)
loss_history <- loss_history[1:actual_epochs]

if (is.na(es$stopped_epoch)) {
  cat(strrep("=", 42), "\n")
  cat(sprintf("Hoan thanh %d epochs (khong kich hoat Early Stopping)\n", epochs))
}

# Tính đầu ra cuối cùng với trọng số đã huấn luyện
nn <- feedforward(nn)


# ================================================================
# PHẦN 8: ĐÁNH GIÁ MÔ HÌNH
# ================================================================
# Ngưỡng phân loại: output >= 0.5 -> dự đoán lớp 1, ngược lại lớp 0
threshold   <- 0.5
y_pred_prob <- as.vector(nn$output)
y_pred      <- ifelse(y_pred_prob >= threshold, 1, 0)
y_true      <- as.vector(nn$y)

# ---- Confusion Matrix ----
# TP (True Positive)  : dự đoán 1, thực tế 1 (dự đoán ĐÚNG lớp dương)
# TN (True Negative)  : dự đoán 0, thực tế 0 (dự đoán ĐÚNG lớp âm)
# FP (False Positive) : dự đoán 1, thực tế 0 (báo động giả – Type I Error)
# FN (False Negative) : dự đoán 0, thực tế 1 (bỏ sót – Type II Error)
TP <- sum(y_pred == 1 & y_true == 1)
TN <- sum(y_pred == 0 & y_true == 0)
FP <- sum(y_pred == 1 & y_true == 0)
FN <- sum(y_pred == 0 & y_true == 1)
n  <- length(y_true)

# ---- Tính các chỉ số ----

# Accuracy: tỷ lệ dự đoán đúng / tổng số mẫu
# Phù hợp khi dữ liệu cân bằng; dễ hiểu nhưng có thể gây hiểu nhầm nếu mất cân bằng lớp
accuracy  <- (TP + TN) / n

# Precision: trong tất cả mẫu được dự đoán là 1, bao nhiêu % thực sự là 1
# Quan trọng khi chi phí FP cao (VD: spam filter, cảnh báo y tế)
precision <- ifelse((TP + FP) > 0, TP / (TP + FP), NA)

# Recall (Sensitivity): trong tất cả mẫu thực sự là 1, bao nhiêu % được phát hiện đúng
# Quan trọng khi chi phí FN cao (VD: phát hiện bệnh, gian lận)
recall    <- ifelse((TP + FN) > 0, TP / (TP + FN), NA)

# F1-Score: trung bình điều hòa của Precision và Recall
# Cân bằng giữa 2 chỉ số; hữu ích khi dữ liệu mất cân bằng
f1        <- ifelse(!is.na(precision) & !is.na(recall) & (precision + recall) > 0,
                    2 * precision * recall / (precision + recall), NA)

# MSE (Mean Squared Error): trung bình bình phương sai số
# Phạt nặng các sai số lớn; đơn vị = bình phương đơn vị của y
mse       <- mean((y_true - y_pred_prob)^2)

# RMSE (Root MSE): căn bậc hai của MSE
# Cùng đơn vị với y; dễ diễn giải hơn MSE
rmse      <- sqrt(mse)

# MAE (Mean Absolute Error): trung bình sai số tuyệt đối
# Ít nhạy với outlier hơn MSE; đơn vị = đơn vị của y
mae       <- mean(abs(y_true - y_pred_prob))

# ---- In bảng kết quả dự đoán ----
cat("\n")
cat(strrep("=", 52), "\n")
cat("  BANG KET QUA DU DOAN\n")
cat(strrep("=", 52), "\n")

result_df <- data.frame(
  X1        = X[, 1],
  X2        = X[, 2],
  Y_thuc_te = y_true,
  Xac_suat  = round(y_pred_prob, 4),
  Y_du_doan = y_pred,
  Ket_qua   = ifelse(y_pred == y_true, "Dung", "Sai")
)
print(result_df, row.names = FALSE)

# ---- In bảng các chỉ số đánh giá ----
cat("\n")
cat(strrep("=", 52), "\n")
cat("  BANG CAC CHI SO DANH GIA MO HINH\n")
cat(strrep("=", 52), "\n")

metrics_df <- data.frame(
  Chi_so  = c(
    "Accuracy",
    "Precision",
    "Recall (Sensitivity)",
    "F1-Score",
    "MSE",
    "RMSE",
    "MAE",
    "Epochs thuc chay",
    "Loss cuoi cung",
    "Loss tot nhat (ES)"
  ),
  Gia_tri = c(
    sprintf("%.4f  (%.1f%%)", accuracy,  accuracy  * 100),
    sprintf("%.4f",           precision),
    sprintf("%.4f",           recall),
    sprintf("%.4f",           f1),
    sprintf("%.6f",           mse),
    sprintf("%.6f",           rmse),
    sprintf("%.6f",           mae),
    sprintf("%d / %d",        actual_epochs, epochs),
    sprintf("%.6f",           loss_history[actual_epochs]),
    sprintf("%.6f",           es$best_loss)
  )
)
print(metrics_df, row.names = FALSE)
cat(strrep("=", 52), "\n")

# ---- Confusion Matrix ----
cat("\nConfusion Matrix:\n")
cm_df <- data.frame(
  row.names      = c("Thuc te: 0", "Thuc te: 1"),
  Du_doan_0 = c(TN, FN),
  Du_doan_1 = c(FP, TP)
)
print(cm_df)


# ================================================================
# PHẦN 9: VẼ ĐỒ THỊ LOSS FUNCTION
# ================================================================

# == Cách 1: Base R – không cần cài thêm package ==
par(mar = c(4.5, 4.5, 3.5, 1.5))

plot(
  1:actual_epochs, loss_history,
  type  = "l",
  col   = "steelblue",
  lwd   = 1.8,
  xlab  = "Epoch",
  ylab  = "MSE Loss (log scale)",
  main  = "Loss Function qua cac Epoch – XOR Neural Network",
  # Trục Y logarithm: loss giảm nhanh ở đầu rồi chậm dần,
  # dùng log giúp thấy toàn bộ quá trình hội tụ rõ hơn
  log   = "y",
  panel.first = grid(col = "gray88", lty = "dotted")
)

# Đường kẻ dọc tại điểm Early Stopping (nếu có)
if (!is.na(es$stopped_epoch)) {
  abline(v   = es$stopped_epoch,
         col = "tomato", lty = 2, lwd = 1.5)
  text(x      = es$stopped_epoch,
       y      = max(loss_history) * 0.5,
       labels = paste("Early Stop\nEpoch", es$stopped_epoch),
       col    = "tomato", cex = 0.8, pos = 4)
}

# Đánh dấu điểm loss cuối cùng
points(actual_epochs, loss_history[actual_epochs],
       pch = 19, col = "tomato", cex = 1.4)
text(actual_epochs, loss_history[actual_epochs],
     labels = sprintf("%.4f", loss_history[actual_epochs]),
     pos = 2, col = "tomato", cex = 0.85)

legend("topright",
       legend = c("MSE Loss", "Early Stop"),
       col    = c("steelblue", "tomato"),
       lty    = c(1, 2), lwd = 1.5, cex = 0.85, bty = "n")


# == Cách 2: ggplot2 – phù hợp cho báo cáo ==
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  
  df_loss <- data.frame(epoch = 1:actual_epochs, loss = loss_history)
  
  p <- ggplot(df_loss, aes(x = epoch, y = loss)) +
    geom_line(color = "steelblue", linewidth = 0.8, alpha = 0.9) +
    geom_point(data = df_loss[actual_epochs, ],
               color = "tomato", size = 3, shape = 19) +
    {if (!is.na(es$stopped_epoch))
      geom_vline(xintercept = es$stopped_epoch,
                 color = "tomato", linetype = "dashed", linewidth = 0.8)} +
    scale_y_log10() +
    scale_x_continuous(labels = scales::comma) +
    labs(
      title    = "Loss Function qua cac Epoch",
      subtitle = sprintf(
        "XOR Neural Network  |  MSE cuoi: %.4f  |  Epochs: %d/%d",
        loss_history[actual_epochs], actual_epochs, epochs),
      x       = "Epoch",
      y       = "MSE Loss (log scale)",
      caption = ifelse(!is.na(es$stopped_epoch),
                       paste("Early Stopping tai epoch", es$stopped_epoch),
                       "Chay du so epoch quy dinh")
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray45", size = 11),
      plot.caption  = element_text(color = "tomato",  size = 10),
      panel.grid.minor = element_line(color = "gray92")
    )
  
  print(p)
} else {
  message("Goi y: Cai ggplot2 de co do thi dep hon: install.packages('ggplot2')")
}

