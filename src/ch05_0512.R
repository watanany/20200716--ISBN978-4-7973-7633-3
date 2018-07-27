##  Ch05 ソースコード

# ab.test.impデータにab.test.goalデータをくっつける
# データの読みこみ
ab.test.imp <- read.csv("section5-ab_test_imp.csv", header = T, stringsAsFactors = F)
ab.test.goal <- read.csv("section5-ab_test_goal.csv", header = T, stringsAsFactors = F)
# ab.test.impにab.test.goalをくっつける
ab.test.imp <- merge(ab.test.imp, ab.test.goal, by = "transaction_id", all.x = T, suffixes = c("", ".g"))
head(ab.test.imp)

#
# クリックフラグの追加
ab.test.imp$is.goal <- ifelse(is.na(ab.test.imp$user_id.g), 0, 1)
head(ab.test.imp)

#
# クリック率の計算
library(plyr)
ddply(ab.test.imp, .(test_case), summarize,
  cvr = sum(is.goal) / length(user_id)
)

# χ2乗検定の実行
chisq.test(ab.test.imp$test_case, ab.test.imp$is.goal)

#
# 日付ごとテストケースごとにクリック率を算出
ab.test.imp.summary <-
  ddply(ab.test.imp, .(log_date, test_case), summarize,
    imp = length(user_id),
    cv = sum(is.goal),
    cvr = sum(is.goal) / length(user_id)
  )
# テストケースごとのクリック率を算出
ab.test.imp.summary <-
  ddply(ab.test.imp.summary, .(test_case), transform,
    cvr.avg = sum(cv) / sum(imp)
  )
head(ab.test.imp.summary)

# テストケースごとのクリック率の時系列グラフ
library(ggplot2)
library(scales)
ab.test.imp.summary$log_date <- as.Date(ab.test.imp.summary$log_date)
limits <- c(0, max(ab.test.imp.summary$cvr))
ggplot(ab.test.imp.summary, aes(x = log_date, y = cvr, col = test_case, lty = test_case, shape = test_case)) +
  geom_line(lwd = 1) +
  geom_point(size = 4) +
  geom_line(aes(y = cvr.avg, col = test_case)) +
  scale_y_continuous(label = percent, limits = limits)
