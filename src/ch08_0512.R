##  Ch08 ソースコード

# データを読みこむための関数の作成
library(plyr)
library(foreach)

readTsvDates <- function(base.dir, app.name, date.from, date.to) {
  date.from <- as.Date(date.from)
  date.to <- as.Date(date.to)
  dates <- seq.Date(date.from, date.to, by = "day")
  x <- ldply(foreach(day = dates, combine = rbind) %do% {
    read.csv(sprintf("%s/%s/%s/data.tsv", base.dir, app.name, day),
      header = T,
      sep = "\t", stringsAsFactors = F
    )
  })
  x
}

# DAUを読みこむ関数
readDau <- function(app.name, date.from, date.to = date.from) {
  data <- readTsvDates(
    "sample-data/section8/daily/dau", app.name,
    date.from, date.to
  )
  data
}
# DPUを読みこむ関数
readDpu <- function(app.name, date.from, date.to = date.from) {
  data <- readTsvDates(
    "sample-data/section8/daily/dpu", app.name,
    date.from, date.to
  )
  data
}
# 行動データを読みこむ関数
readActionDaily <- function(app.name, date.from, date.to = date.from) {
  data <- readTsvDates(
    "sample-data/section8/daily/action", app.name,
    date.from, date.to
  )
  data
}

# データの読みこみ

# DAU
dau <- readDau("game-01", "2013-05-01", "2013-10-31")
head(dau)
# DPU
dpu <- readDpu("game-01", "2013-05-01", "2013-10-31")
head(dpu)
# Action
user.action <- readActionDaily("game-01", "2013-10-31", "2013-10-31")
head(user.action)

# DAUにDPUをくっつける

# 課金データのマージ
dau2 <- merge(dau, dpu[, c("log_date", "user_id", "payment"), ],
  by = c("log_date", "user_id"), all.x = T
)
# 課金フラグを付ける
dau2$is.payment <- ifelse(is.na(dau2$payment), 0, 1)
head(dau2)
# 非課金のレコードの課金額に0を入れる
dau2$payment <- ifelse(is.na(dau2$payment), 0, dau2$payment)
head(dau2)

# 月次で集計

# 月のカラムを作成
dau2$log_month <- substr(dau2$log_date, 1, 7)
# 月次集計
mau <- ddply(dau2, .(log_month, user_id), summarize,
  payment
  = sum(payment),
  access_days = length(log_date)
)
head(mau)

# ランキング帯の決定
library(ykmeans)
library(ggplot2)
library(scales)
# A47がランキングポイント
user.action2 <- ykmeans(user.action, "A47", "A47", 3)
# 各クラスタの人数
table(user.action2$cluster)

# ランキングポイントの分布
ggplot(
  arrange(user.action2, desc(A47)),
  aes(
    x = 1:length(user_id), y = A47,
    col = as.factor(cluster), shape = as.factor(cluster)
  )
) +
  geom_line() +
  xlab("user") +
  ylab("Ranking point") +
  scale_y_continuous(label = comma) +
  ggtitle("Ranking Point") +
  theme(legend.position = "none")

# ランキング上位に絞る
user.action.h <- user.action2[
  user.action2$cluster >= 2,
  names(user.action)
]

# 主成分分析の実行

# 機械学習用のライブラリ
# 前処理に便利な関数があるため、それを利用する
library(caret)
user.action.f <- user.action.h[, -c(1:4)]
row.names(user.action.f) <- user.action.h$user_id
head(user.action.f)
# 情報量がゼロに近い変数の削除
nzv <- nearZeroVar(user.action.f)
user.action.f.filterd <- user.action.f[, -nzv]
# 変数間の相関が高いものを削除
user.action.cor <- cor(user.action.f.filterd)
highly.cor.f <- findCorrelation(user.action.cor, cutoff = .7)
user.action.f.filterd <- user.action.f.filterd[, -highly.cor.f]
# 主成分分析の実行
# pca
user.action.pca.base <- prcomp(user.action.f.filterd, scale = T)
user.action.pca.base$rotation

# クラスタリングの実行
user.action.pca <- data.frame(user.action.pca.base$x)
keys <- names(user.action.pca)
user.action.km <- ykmeans(user.action.pca, keys, "PC1", 3:6)
table(user.action.km$cluster)

ggplot(
  user.action.km,
  aes(x = PC1, y = PC2, col = as.factor(cluster), shape = as.factor(cluster))
) +
  geom_point()

# クラスタごとに平均を算出する
user.action.f.filterd$cluster <- user.action.km$cluster
user.action.f.center <-
  ldply(lapply(
    sort(unique(user.action.f.filterd$cluster)),
    function(i) {
      x <- user.action.f.filterd[
        user.action.f.filterd$cluster == i,
        -ncol(user.action.f.filterd)
      ]
      apply(x, 2, function(d) mean(d))
    }
  ))

# レーダーチャート用のデータの作成
library(fmsb)
# レーダーチャート用にデータを整形する関数
createRadarChartDataFrame <- function(df) {
  df <- data.frame(df)
  dfmax <- apply(df, 2, max) + 1
  dfmin <- apply(df, 2, min) - 1
  as.data.frame(rbind(dfmax, dfmin, df))
}
# 相関が高い変数を除外する
df <- user.action.f.center[, -(ncol(user.action.f.center) - 1)]
df.cor <- cor(df)
df.highly.cor <- findCorrelation(df.cor, cutoff = 0.91)
# 解釈しやすい数になるように手で調整
df.filterd <- df[, -df.highly.cor]
# レーダーチャート用のデータ作成
df.filterd <- createRadarChartDataFrame(scale(df.filterd))
names(df.filterd)

names(df.filterd) <- c(
  "レベル", "救援回数", "被救援回数", "ボス討伐数",
  "バトル回数", "プレイ回数"
)

par(family="HiraKakuProN-W3") #日本語が文字化けする場合にはフォントを指定する
radarchart(df.filterd, seg = 5, plty = 1:5, plwd = 4, pcol = rainbow(5))
legend("topright", legend = 1:5, col = rainbow(5), lty = 1:5)

# クラスタごとにKPIを算出

user.action.f.filterd$user_id <-
  as.numeric(rownames(user.action.f.filterd))
user.action.kpi <- merge(user.action.f.filterd, mau, by = "user_id")
ddply(user.action.kpi, .(cluster), summarize,
  arpu = round(mean(payment)),
  access_days = round(mean(access_days))
)
