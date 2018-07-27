##  Ch09 ソースコード

# データを読みこむための関数の作成
library(plyr)
library(foreach)

# 利用開始日の取得
readInstall <- function(app.name, target.day) {
  base.dir <- "sample-data/section9/snapshot/install"
  f <- sprintf("%s/%s/%s/install.csv", base.dir, app.name, target.day)
  read.csv(f, header = T, stringsAsFactors = F)
}

# DAU の取得
readDau <- function(app.name, date.from, date.to) {
  date.from <- as.Date(date.from)
  date.to <- as.Date(date.to)
  dates <- seq.Date(date.from, date.to, by = "day")
  ldply(foreach(day = dates, combine = rbind) %do% {
    base.dir <- "sample-data/section9/daily/dau"
    f <- sprintf("%s/%s/%s/dau.csv", base.dir, app.name, day)
    read.csv(f, header = T, stringsAsFactors = F)
  })
}

# 行動ログの取得
readAction <- function(app.name, action.name, date.from, date.to) {
  date.from <- as.Date(date.from)
  date.to <- as.Date(date.to)
  dates <- seq.Date(date.from, date.to, by = "day")
  ldply(foreach(day = dates, combine = rbind) %do% {
    base.dir <- "sample-data/section9/daily/action"
    f <- sprintf(
      "%s/%s/%s/%s/%s.csv",
      base.dir, app.name, action.name, day, action.name
    )
    read.csv(f, header = T, stringsAsFactors = F)
  })
}


# install
install <- readInstall("game-01", "2013-09-30")
head(install)
# DAU
dau <- readDau("game-01", "2013-06-01", "2013-09-30")
head(dau)


# battle
battle <- readAction("game-01", "battle", "2013-06-01", "2013-08-31")
head(battle)
# message
msg <- readAction("game-01", "message", "2013-06-01", "2013-08-31")
head(msg)
# help
hlp <- readAction("game-01", "help", "2013-06-01", "2013-08-31")
head(hlp)

# ログイン密度の算出
# DAUに利用開始日の情報をマージ
dau.inst <- merge(dau, install, by = "user_id", suffixes = c("", ".inst"))
head(dau.inst)
# データを利用開始後7?13日に絞る
dau.inst$log_date <- as.Date(dau.inst$log_date)
dau.inst$log_date.inst <- as.Date(dau.inst$log_date.inst)
dau.inst$elapsed_days <- as.numeric(dau.inst$log_date -
  dau.inst$log_date.inst)
dau.inst.7_13 <-
  dau.inst[dau.inst$elapsed_days >= 7 &
    dau.inst$elapsed_days <= 13, ]
head(dau.inst.7_13)
# ログイン密度の算出
dau.inst.7_13.login.ds <- ddply(dau.inst.7_13, .(user_id), summarize,
  density = length(log_date) / 7
)
head(dau.inst.7_13.login.ds)

# 分析対象ユーザにログイン密度をマージ
target.install <- install [install$log_date >= "2013-06-01" &
  install$log_date <= "2013-08-25", ]
# 対象ユーザにログイン密度をマージ
target.install.login.ds <-
  merge(target.install, dau.inst.7_13.login.ds,
    by = "user_id", all.x = T
  )
target.install.login.ds$density <-
  ifelse(is.na(target.install.login.ds$density), 0,
    target.install.login.ds$density
  )
head(target.install.login.ds)


# 戦いに関するデータ作成
# 戦闘とインストールデータのマージ
battle.inst <- merge(battle, install, by = "user_id", suffixes = c(
  "",
  ".inst"
))
head(battle.inst)
# 戦闘とインストール日の差の計算
battle.inst$log_date <- as.Date(battle.inst$log_date)
battle.inst$log_date.inst <- as.Date(battle.inst$log_date.inst)
battle.inst$elapsed_days <- as.numeric(battle.inst$log_date -
  battle.inst$log_date.inst)
# 戦闘とインストール日が1週間以内のものに絞る
battle.inst2 <- battle.inst[battle.inst$elapsed_days >= 0 &
  battle.inst$elapsed_days <= 6, ]
# インストール経過日数を列にする
library(reshape2)
battle.inst2$elapsed_days <- paste0("d", battle.inst2$elapsed_days)
battle.inst2.cast <- dcast(battle.inst2, user_id ~ elapsed_days,
  value.var = "count", sum
)
head(battle.inst2.cast)

# 比率データの作成、PCAデータの作成
# 比率のデータを作成
battle.inst2.cast.prop <- battle.inst2.cast
battle.inst2.cast.prop[, -1] <-
  battle.inst2.cast.prop[, -1] / rowSums(battle.inst2.cast.prop[, -1])
head(battle.inst2.cast.prop)
# PCA
b.pca <- prcomp(battle.inst2.cast[, -1], scale = T)
summary(b.pca)
battle.inst2.cast.pca <- data.frame(
  user_id = battle.inst2.cast$user_id,
  b.pca$x
)
head(battle.inst2.cast.pca)


# メッセージデータの作成
# メッセージとインストールデータのマージ
msg.inst <- merge(msg, install, by = "user_id", suffixes = c("", ".inst"))
head(msg.inst)
# メッセージとインストール日の差の計算
msg.inst$log_date <- as.Date(msg.inst$log_date)
msg.inst$log_date.inst <- as.Date(msg.inst$log_date.inst)
msg.inst$elapsed_days <- as.numeric(msg.inst$log_date -
  msg.inst$log_date.inst)
# メッセージとインストール日が1週間以内のものに絞る
msg.inst2 <- msg.inst[msg.inst$elapsed_days >= 0 & msg.inst$elapsed_days
<= 6, ]

# インストール経過日数を列にする
msg.inst2$elapsed_days <- paste0("d", msg.inst2$elapsed_days)
msg.inst2.cast <- dcast(msg.inst2, user_id ~ elapsed_days,
  value.var =
    "count", sum
)
head(msg.inst2.cast)

# 比率データの作成、PCAデータの作成
# 比率のデータを作成
msg.inst2.cast.prop <- msg.inst2.cast
msg.inst2.cast.prop[, -1] <- msg.inst2.cast.prop[
  ,
  -1
] / rowSums(msg.inst2.cast.prop[, -1])
head(msg.inst2.cast.prop)
# PCA
m.pca <- prcomp(msg.inst2.cast[, -1], scale = T)
summary(m.pca)
msg.inst2.cast.pca <- data.frame(user_id = msg.inst2.cast$user_id, m.pca$x)
head(msg.inst2.cast.pca)

# 協力データの作成
# 協力とインストールデータのマージ
hlp.inst <- merge(hlp, install, by = "user_id", suffixes = c("", ".inst"))
head(hlp.inst)
# 協力とインストール日の差の計算
hlp.inst$log_date <- as.Date(hlp.inst$log_date)
hlp.inst$log_date.inst <- as.Date(hlp.inst$log_date.inst)
hlp.inst$elapsed_days <- as.numeric(hlp.inst$log_date -
  hlp.inst$log_date.inst)
# 協力とインストール日が1週間以内のものに絞る
hlp.inst2 <- hlp.inst[hlp.inst$elapsed_days >= 0 & hlp.inst$elapsed_days <=
  6, ]
# インストール経過日数を列にする
hlp.inst2$elapsed_days <- paste0("d", hlp.inst2$elapsed_days)
hlp.inst2.cast <- dcast(hlp.inst2, user_id ~ elapsed_days,
  value.var =
    "count", sum
)
head(hlp.inst2.cast)

# 比率データの作成、PCAデータの作成
# 比率のデータを作成
hlp.inst2.cast.prop <- hlp.inst2.cast
hlp.inst2.cast.prop[, -1] <- hlp.inst2.cast.prop[
  ,
  -1
] / rowSums(hlp.inst2.cast.prop[, -1])
head(hlp.inst2.cast.prop)
# PCA
h.pca <- prcomp(hlp.inst2.cast[, -1], scale = T)
summary(h.pca)
hlp.inst2.cast.pca <- data.frame(user_id = hlp.inst2.cast$user_id, h.pca$x)
head(hlp.inst2.cast.pca)

# 行動ログに対するクラスタリング
# クラスタデータ作成関数
createClusterData <- function(aname, x, x.prop, x.pca) {
  set.seed(10)
  df <- ldply(foreach(i = 3:6, combine = rbind) %do% {
    km <- kmeans(x[, -1], i)
    km.prop <- kmeans(x.prop[, -1], i)
    km.pca <- kmeans(x.pca[, -1], i)
    data.frame(
      user_id = x$user_id, cluster.type = sprintf(
        "%s%02d",
        aname,
        i
      ), freq.cluster.id = km$cluster, prop.cluster.id =
        km.prop$cluster,
      pca.cluster.id = km.pca$cluster
    )
  })
  cluster.melt <- melt(df, id.vars = c("user_id", "cluster.type"))
  dcast(cluster.melt, user_id ~ cluster.type + variable)
}
# バトル
battle.cluster <- createClusterData(
  "battle", battle.inst2.cast,
  battle.inst2.cast.prop,
  battle.inst2.cast.pca
)
head(battle.cluster)
# メッセージ
msg.cluster <- createClusterData(
  "msg", msg.inst2.cast,
  msg.inst2.cast.prop,
  msg.inst2.cast.pca
)
head(msg.cluster)
# 協力
hlp.cluster <- createClusterData(
  "hlp", hlp.inst2.cast,
  hlp.inst2.cast.prop,
  hlp.inst2.cast.pca
)
head(hlp.cluster)


# クラスタのマージ
# cluster data
cluster.data <- merge(target.install.login.ds, battle.cluster,
  by = "user_id", all.x = T
)
cluster.data <- merge(cluster.data, msg.cluster, by = "user_id", all.x = T)
cluster.data <- merge(cluster.data, hlp.cluster, by = "user_id", all.x = T)
cluster.data[is.na(cluster.data)] <- 0
head(cluster.data)

# ログイン密度が昇順になるように振り直す

# 列になってるクラスターを行の形で持たせる
cluster.data.melt <- melt(cluster.data[, -c(2:6)], id.vars = c(
  "user_id",
  "density"
))
# クラスター種別クラスター番号ごとに、平均ログイン密度を計算
cluster.data.avg <- ddply(cluster.data.melt, .(variable, value), summarize,
  average.density = mean(density)
)
head(cluster.data.avg)
# 新しいクラスタ番号を付与
cluster.data.avg <- arrange(cluster.data.avg, variable, average.density)
cluster.data.avg <- ddply(cluster.data.avg, .(variable), transform,
  value2
  = sort(value)
)
# 新しいクラスター番号をマージ
cluster.data.melt2 <- merge(cluster.data.melt, cluster.data.avg,
  by =
    c("variable", "value")
)
head(cluster.data.melt2)

# クラスター種別を列に持たせる形に整形
cluster.data2 <- dcast(cluster.data.melt2, user_id + density ~ variable,
  value.var = "value2"
)
head(cluster.data2)

# 決定木分析の実行、可視化
library(rpart)
fit <- rpart(density ~ ., cluster.data2[, -1])
print(fit)
library(partykit)
plot(as.party(fit), tp_args = T)

# 「協力」のPCAクラスタの詳細
cluster.data3 <- cluster.data.melt2[
  cluster.data.melt2$variable ==
    "hlp04_pca.cluster.id",
  c("user_id", "average.density", "value2")
]
names(cluster.data3)[3] <- "cluster"
hlp.inst2.cast.prop2 <- merge(hlp.inst2.cast.prop, cluster.data3,
  by =
    "user_id"
)
table(hlp.inst2.cast.prop2$cluster)

# クラスタごとの平均ログイン密度の算出
hlp.inst2.cast.summary <- ddply(hlp.inst2.cast.prop2, .(cluster),
  summarize,
  login.density = average.density[1], d0 = sum(d0) / length(user_id),
  d1 = sum(d1) / length(user_id),
  d2 = sum(d2) / length(user_id), d3 = sum(d3) / length(user_id),
  d4 = sum(d4) / length(user_id),
  d5 = sum(d5) / length(user_id), d6 = sum(d6) / length(user_id)
)
hlp.inst2.cast.summary

# クラスタ別、ログイン密度の可視化
library(ggplot2)
ggplot(hlp.inst2.cast.summary, aes(x = cluster, y = login.density)) +
  geom_line() +
  geom_point()

# クラスタ別、日ごとの「協力」行動の可視化
hlp.inst2.cast.summary.melt <- melt(hlp.inst2.cast.summary[, -2], id.vars = "cluster")
hlp.inst2.cast.summary.melt$days <- as.numeric(substr(hlp.inst2.cast.summary.melt$variable, 2, 3))
hlp.inst2.cast.summary.melt$cluster <- as.factor(hlp.inst2.cast.summary.melt$cluster)
ggplot(hlp.inst2.cast.summary.melt, aes(x = days, y = value, col = cluster)) + geom_line() + geom_point()
