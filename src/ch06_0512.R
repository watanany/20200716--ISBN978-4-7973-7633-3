##  Ch06 ソースコード

# CSVファイルを読みこむ
ad.data <- read.csv("./ad_result.csv", header = T, stringsAsFactors = F)
ad.data

# TVCMの広告費用とInstallの散布図をつくる
library(ggplot2)
library(scales)

ggplot(ad.data, aes(x = tvcm, y = install)) + geom_point() + 
xlab("TVの広告費") + ylab("新規インストール") + 
scale_x_continuous(label = comma) +
scale_y_continuous(label = comma)

# 雑誌媒体の広告費用とInstallの散布図をつくる
ggplot(ad.data, aes(x = magazine, y = install)) + geom_point() + 
xlab("雑誌の広告費") + ylab("新規インストール") + 
scale_x_continuous(label = comma) + 
scale_y_continuous(label = comma)

# 回帰分析の実行
fit <- lm(install ~ ., data = ad.data[, c("install", "tvcm", "magazine")])
fit

# 回帰分析の結果の解釈
summary(fit)


