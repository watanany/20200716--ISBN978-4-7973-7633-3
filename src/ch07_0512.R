##  Ch07 ソースコード

# CSVファイルを読みこむ
dau <- read.csv("section7-dau.csv", header = T, stringsAsFactors = F)
head(dau)

# ユーザごとに移行した人かどうかのデータの整理
# MAU
mau <- unique(dau[, c("region_month", "device", "user_id")])
# FP MAU
fp.mau <- unique(dau[dau$device == "FP", c(
  "region_month", "device",
  "user_id"
)])
# SP MAU
sp.mau <- unique(dau[dau$device == "SP", c(
  "region_month", "device",
  "user_id"
)])

# 1月と2月で分ける
fp.mau1 <- fp.mau[fp.mau$region_month == "2013-01", ]
fp.mau2 <- fp.mau[fp.mau$region_month == "2013-02", ]
sp.mau1 <- sp.mau[sp.mau$region_month == "2013-01", ]
sp.mau2 <- sp.mau[sp.mau$region_month == "2013-02", ]
# 1月携帯電話からの利用で2月にアクセスがあるかどうか
mau$is_access <- 1
fp.mau1 <- merge(fp.mau1, mau[
  mau$region_month == "2013-02",
  c("user_id", "is_access")
], by = "user_id", all.x = T)
fp.mau1$is_access[is.na(fp.mau1$is_access)] <- 0
head(fp.mau1)

# 1月は携帯電話からの利用で2月にも携帯電話からの利用があったかどうか
fp.mau2$is_fp <- 1
fp.mau1 <- merge(fp.mau1, fp.mau2[, c("user_id", "is_fp")],
  by = "user_id",
  all.x = T
)
fp.mau1$is_fp[is.na(fp.mau1$is_fp)] <- 0
head(fp.mau1)

# 1月は携帯電話からの利用で2月にスマートフォンから利用があったかどうか
sp.mau2$is_sp <- 1
fp.mau1 <- merge(fp.mau1, sp.mau2[, c("user_id", "is_sp")],
  by = "user_id", all.x = T
)
fp.mau1$is_sp[is.na(fp.mau1$is_sp)] <- 0
head(fp.mau1)

# 1月携帯電話利用で2月は利用なし、あるいはスマートフォン利用があったユーザ
fp.mau1 <- fp.mau1[fp.mau1$is_access == 0 | fp.mau1$is_sp == 1, ]
head(fp.mau1)

# 日ごとにゲームにアクセスしたかどうかのデータを整える
library(reshape2)
fp.dau1 <- dau[dau$device == "FP" & dau$region_month == "2013-01", ]
fp.dau1$is_access <- 1
fp.dau1.cast <- dcast(fp.dau1, user_id ~ region_day,
  value.var =
    "is_access", function(x) as.character(length(x))
)
names(fp.dau1.cast)[-1] <- paste0("X", 1:31, "day")
head(fp.dau1.cast)

fp.dau1.cast <- merge(fp.dau1.cast, fp.mau1[, c("user_id", "is_sp")],
  by = "user_id"
)
head(fp.dau1.cast)

table(fp.dau1.cast$is_sp)

# ロジスティック回帰分析によるモデル作成
fit.logit <- step(glm(is_sp ~ .,
  data = fp.dau1.cast[, -1],
  family = binomial
))
summary(fit.logit)

# 作成されたモデルを利用した予測
# SP移行確率
fp.dau1.cast$prob <- round(fitted(fit.logit), 2)
# SPに移行したかどうかの予測
fp.dau1.cast$pred <- ifelse(fp.dau1.cast$prob > 0.5, 1, 0)
head(fp.dau1.cast)

# 予測と実測
table(fp.dau1.cast[, c("is_sp", "pred")])

# 予測結果からユーザ群を推測する
fp.dau1.cast1 <- fp.dau1.cast[fp.dau1.cast$is_sp == 1 & fp.dau1.cast$pred
== 1, ]
head(fp.dau1.cast1[order(fp.dau1.cast1$prob, decreasing = T), ])

fp.dau1.cast2 <- fp.dau1.cast[fp.dau1.cast$is_sp == 0 & fp.dau1.cast$pred
== 1, ]
head(fp.dau1.cast2[order(fp.dau1.cast2$prob, decreasing = T), ])

fp.dau1.cast3 <- fp.dau1.cast[fp.dau1.cast$is_sp == 0 & fp.dau1.cast$pred
== 0, ]
head(fp.dau1.cast3[order(fp.dau1.cast3$prob), ])
