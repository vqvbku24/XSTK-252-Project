# ================================================
# DATA CLEANING CHO INTERNET ADVERTISEMENT (Random Forest)
# ================================================

library(tidyverse)
library(caret)

# Tạo danh sách tên cột
col_names <- c("index", "height", "width", "aspect_ratio",
               paste0("feature_", 1:1555), "class")

# Đọc dữ liệu thô
dataset <- read.csv("add.csv",
                    header = FALSE,
                    skip = 1,
                    na.strings = c("?", " ?", "  ?"),
                    col.names = col_names)

# Làm sạch và chuyển kiểu
clean_data <- dataset |>
  mutate(
    height       = as.numeric(height),
    width        = as.numeric(width),
    aspect_ratio = as.numeric(aspect_ratio),
    class        = as.factor(class)
  ) |>
  filter(class == "ad." | class == "nonad.") |>
  select(-index)

clean_data$class <- droplevels(clean_data$class)

# ====================== CHIA TRAIN / TEST ======================
set.seed(123)
train_index <- createDataPartition(clean_data$class, p = 0.8, list = FALSE)
train_data <- clean_data[train_index, ]
test_data  <- clean_data[-train_index, ]

# ====================== IMPUTATION ======================
# Chỉ tính median trên tập Train (tránh leakage)
med_height <- median(train_data$height, na.rm = TRUE)
med_width  <- median(train_data$width, na.rm = TRUE)
med_aspect <- median(train_data$aspect_ratio, na.rm = TRUE)

impute_numeric <- function(df) {
  df$height[is.na(df$height)]       <- med_height
  df$width[is.na(df$width)]         <- med_width
  df$aspect_ratio[is.na(df$aspect_ratio)] <- med_aspect
  return(df)
}

train_data <- impute_numeric(train_data)
test_data  <- impute_numeric(test_data)

# ====================== XỬ LÝ NA Ở CÁC CỘT BINARY ======================
# Tất cả feature_1 đến feature_1555 đều là binary 0/1 → thay NA = 0
binary_cols <- paste0("feature_", 1:1555)

train_data[binary_cols][is.na(train_data[binary_cols])] <- 0
test_data[binary_cols][is.na(test_data[binary_cols])]   <- 0
# ====================== LOẠI BỎ DUPLICATE ======================
train_data <- unique(train_data)

# ====================== KIỂM TRA CUỐI CÙNG ======================
cat("\n=== HOÀN TẤT PREPROCESSING ===\n")
cat("Train size :", nrow(train_data), "dòng,", ncol(train_data), "cột\n")
cat("Test size  :", nrow(test_data),  "dòng,", ncol(test_data),  "cột\n")

# Kiểm tra còn NA không
cat("Còn NA trong Train:", sum(is.na(train_data)), "\n")
cat("Còn NA trong Test :", sum(is.na(test_data)), "\n")

# ====================== LƯU FILE ======================
write.csv(train_data, "train.csv", row.names = FALSE)
write.csv(test_data,  "test.csv",  row.names = FALSE)

cat("Đã lưu train.csv và test.csv thành công! (không còn NA)\n")