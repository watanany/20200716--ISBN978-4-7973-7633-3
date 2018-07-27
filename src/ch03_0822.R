##  Ch03 ソースコード（2014/08/22に新しいバージョンのggplot2でヒストグラムが出力されないのを修正）

# CSVファイルを読みこむ
dau <- read.csv("section3-dau.csv", header = T, stringsAsFactors = F)
head(dau)
dpu <- read.csv("section3-dpu.csv", header = T, stringsAsFactors = F)
head(dpu)
install <- read.csv("section3-install.csv", header = T, stringsAsFactors = F)
head(install)

# DAUのデータにInstallデータをくっつける
dau.install <- merge(dau, install, by = c("user_id", "app_name"))
head(dau.install)

# 上記のデータに、さらにDPUデータをくっつける

dau.install.payment <- merge(dau.install, dpu, by = c(
  "log_date",
  "app_name", "user_id"
), all.x = T)
head(dau.install.payment)


head(na.omit(dau.install.payment))


# 非課金ユーザの課金額にゼロを入れる

dau.install.payment$payment[is.na(dau.install.payment$payment)] <- 0
head(dau.install.payment)

# 月次で集計する

# 月のカラム追加
dau.install.payment$log_month <- substr(dau.install.payment$log_date, 1, 7)
dau.install.payment$install_month <- substr(dau.install.payment$install_date, 1, 7)
install.packages(plyr)
library(plyr)
mau.payment <- ddply(dau.install.payment,
  .(log_month, user_id, install_month), # グループ化
  summarize, # 集計コマンド
  payment = sum(payment) # paymentの合計
)
head(mau.payment)

# 新規ユーザか既存ユーザかの区分を追加する

# 新規ユーザーと既存ユーザーの識別
mau.payment$user.type <- ifelse(mau.payment$install_month == mau.payment$log_month,
  "install", "existing"
)
mau.payment.summary <- ddply(mau.payment,
  .(log_month, user.type), # グループ化
  summarize, # 集計コマンド
  total.payment = sum(payment) # paymentの合計
)
head(mau.payment)
head(mau.payment.summary)

# グラフによりデータを可視化する（geom_bar()　→　geom_bar(stat="identity")に修正。2014/08/22）
library(ggplot2)
library(scales)
ggplot(mau.payment.summary, aes(
  x = log_month, y = total.payment,
  fill = user.type
)) + geom_bar(stat = "identity") + scale_y_continuous(label = comma)

# old_theme = theme_update(
# axis.title.x = theme_text(family="HiraKakuProN-W3"),
# axis.title.y = theme_text(family="HiraKakuProN-W3", angle=90),
# plot.title = theme_text(family="HiraKakuProN-W3", size=14.4))

ggplot(
  mau.payment[mau.payment$payment > 0 & mau.payment$user.type == "install", ],
  aes(x = payment, fill = log_month)
) + geom_histogram(position = "dodge", binwidth = 2000)
