##  Ch04 ソースコード

# CSVファイルを読みこむ
dau <- read.csv("section4-dau.csv", header = T, stringsAsFactors = F)
head(dau)
user.info <- read.csv("section4-user_info.csv", header = T, stringsAsFactors = F)
head(user.info)

# DAUのデータにuser.infoデータをくっつける
dau.user.info <- merge(dau, user.info, by = c("user_id", "app_name"))
head(dau.user.info)

# セグメント分析（性別で集計）
dau.user.info$log_month <- substr(dau.user.info$log_date, 1, 7)
table(dau.user.info[, c("log_month", "gender")])

# セグメント分析（年代で集計）
table(dau.user.info[, c("log_month", "generation")])

# セグメント分析（性別×年代で集計）
library(reshape2)
dcast(dau.user.info, log_month ~ gender + generation,
  value.var = "user_id",
  length
)

# セグメント分析（デバイスごとで集計）
table(dau.user.info[, c("log_month", "device_type")])

# セグメント分析の結果を可視化する

# 日付別デバイス別にユーザ数を算出する
dau.user.info.device.summary <- ddply(dau.user.info, .(log_date, device_type), summarize, dau = length(user_id))
# 日付型に変換
dau.user.info.device.summary$log_date <- as.Date(dau.user.info.device.summary$log_date)
# 時系列のトレンドグラフの描画
library(ggplot2)
library(scales)
limits <- c(0, max(dau.user.info.device.summary$dau))
ggplot(dau.user.info.device.summary, aes(x = log_date, y = dau, col = device_type, lty = device_type, shape = device_type)) +
  geom_line(lwd = 1) +
  geom_point(size = 4) +
  scale_y_continuous(label = comma, limits = limits)
