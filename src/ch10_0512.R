##  Ch10 ソースコード

# 中間データ読み込み用の関数の作成
library(plyr)
library(foreach)
readActionHourly <- function(app.name, date.from, date.to) {
  date.from <- as.Date(date.from)
  date.to <- as.Date(date.to)
  ldply(foreach(
    day = seq.Date(date.from, date.to, by = "day"),
    combine = rbind
  ) %do% {
    f <- sprintf("sample-data/section10/action_hourly/%s/%s/action_hourly.tsv", app.name, day)
    read.csv(f, header = T, stringsAsFactors = F, sep = "\t")
  })
}
# 作成した関数によるデータの読みこみ
action.hourly <- readActionHourly("game-01", "2013-08-01", "2013-08-08")
head(action.hourly)

# 時間帯が列になるように整形
library(reshape2)
dates <- unique(action.hourly$log_date)
train.list <- lapply(1:(length(dates) - 1), function(i) {
  day <- dates[i]
  x <- action.hourly[action.hourly$log_date == day, ]
  df <- dcast(x, user_id ~ log_hour,
    value.var = "count",
    function(x) {
      ifelse(sum(x) >= 7, 1, 0)
    }
  )
  names(df) <- c("user_id", paste0("p", i, "_", 0:23))
  df
})
head(train.list[[1]])
# 説明用のデータを作成
train.data <- train.list[[1]]
for (i in 2:length(train.list)) {
  df <- train.list[[i]]
  train.data <- merge(train.data, df, by = "user_id", all.x = T)
  train.data[is.na(train.data)] <- 0
}
names(train.data)
# 答えとなるデータを作成
ans0 <- action.hourly[action.hourly$log_date == dates[length(dates)], ]
# 時間帯が列になるように整形
ans <- dcast(ans0, user_id ~ log_hour, value.var = "count", function(x) {
  ifelse(sum(x) >= 7, 1, 0)
})
names(ans) <- c("user_id", paste0("a_", 0:23))
head(ans)
# 説明用のデータとくっつける
train.data <- merge(train.data, ans, by = "user_id", all.x = T)
train.data[is.na(train.data)] <- 0
names(train.data)
# 時間帯ごとにモデルを構築（ランダムフォレスト）
library(randomForest)
library(caret)

# 学習のカスタマイズ
fit.control <- trainControl(method = "LGOCV", p = 0.75, number = 30)
# ランダムフォレストによる学習
rf.fit.list <- lapply(0:23, function(h) {
  df <- train.data[, c(paste0("p", 1:7, "_", h), paste0("a_", h))]
  df1 <- df[df[, ncol(df)] == 1, ]
  df0.orig <- df[df[, ncol(df)] == 0, ]
  df0 <- df0.orig[sample(1:nrow(df0.orig), nrow(df1)), ]
  df <- rbind(df1, df0)
  fit <- train(
    x = df[, -ncol(df)], y = df[, ncol(df)],
    method = "rf", preProcess = c("center", "scale"),
    trControl = fit.control
  )
  fit
})
# 作成したモデルの予測値と実際の値との比較結果の表示
for (i in 0:23) {
  result.rf <- predict(rf.fit.list[[(i + 1)]]$finalModel, train.data)
  result.rf.pred <- ifelse(result.rf >= 0.5, 1, 0)
  print(confusionMatrix(result.rf.pred, train.data[, paste0(
    "a_",
    i
  )]))
}