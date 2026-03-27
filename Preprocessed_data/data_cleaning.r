# KHỞI TẠO VÀ ĐỌC DỮ LIỆU THÔ
library(tidyverse)
library(caret)

# Tạo danh sách 1560 tên cột (gồm index, 3 biến kích thước, 1555 biến text và nhãn)
col_names <- c("index", "height", "width", "aspect_ratio", paste0("feature_", 1:1555), "class")

# Đọc dữ liệu (Bỏ qua dòng đầu tiên, nhận diện các chuỗi "?" là NA)
dataset <- read.csv("add.csv", header = FALSE, skip = 1, 
                    na.strings = c("?", " ?", "  ?"), col.names = col_names)

# Ép kiểu dữ liệu và loại bỏ cột index không mang giá trị dự đoán
clean_data <- dataset %>%
  mutate(
    height = as.numeric(height),
    width = as.numeric(width),
    aspect_ratio = as.numeric(aspect_ratio),
    class = as.factor(class)
  ) %>%
  filter(class == "ad." | class == "nonad.") %>%
  select(-index) # Bỏ cột index

# Dọn dẹp lại các level ẩn của biến phân loại (nếu có)
clean_data$class <- droplevels(clean_data$class)

# CHIA TẬP TRAIN/TEST (CẮT DỮ LIỆU TRƯỚC KHI TÍNH TOÁN)
set.seed(123)

# Dùng createDataPartition để cắt tầng (Stratified Splitting), giữ nguyên tỷ lệ 86/14
trainIndex <- createDataPartition(clean_data$class, p = 0.8, list = FALSE)
train_data <- clean_data[trainIndex, ]
test_data  <- clean_data[-trainIndex, ]

# ĐIỀN KHUYẾT (IMPUTATION) CHỐNG LEAKAGE
# Chỉ tính giá trị Median từ tập Train
med_height <- median(train_data$height, na.rm = TRUE)
med_width  <- median(train_data$width, na.rm = TRUE)
med_aspect <- median(train_data$aspect_ratio, na.rm = TRUE)

# Hàm điền khuyết
impute_missing <- function(df) {
  df$height[is.na(df$height)] <- med_height
  df$width[is.na(df$width)] <- med_width
  df$aspect_ratio[is.na(df$aspect_ratio)] <- med_aspect
  return(df)
}

# Áp dụng hàm điền khuyết cho cả Train và Test
train_data <- impute_missing(train_data)
test_data  <- impute_missing(test_data)

# CHUẨN HÓA (STANDARDIZATION / SCALING) CHỐNG LEAKAGE
scale_params <- preProcess(train_data[, c("height", "width", "aspect_ratio")], 
                           method = c("center", "scale"))

# Áp dụng bộ tham số scale đó lên cả tập Train và tập Test
train_data <- predict(scale_params, train_data)
test_data  <- predict(scale_params, test_data)

# Loại bỏ các dòng trùng lặp trong tập Train (Noise reduction)
train_data <- unique(train_data)

# LƯU TRỮ VÀ KIỂM TRA
cat("\nQuá trình chuẩn hóa hoàn tất, dữ liệu đã sạch và sẵn sàng.\n")
cat("Kích thước tập Train:", nrow(train_data), "dòng, ", ncol(train_data), "cột\n")
cat("Kích thước tập Test:", nrow(test_data), "dòng, ", ncol(test_data), "cột\n")

write.csv(train_data, "train.csv", row.names = FALSE)
write.csv(test_data, "test.csv", row.names = FALSE)