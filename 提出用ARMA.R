install.packages("tseries")
install.packages("forecast")
install.packages("dplyr")

setwd("C:/Users/npcta/OneDrive/Desktop/seminar_game/raw_data")
df <- read.csv("nintendo_tokyo.csv",fileEncoding = "CP932",stringsAsFactors = FALSE
)

df <- df[, -c(2, 4, 5, 6, 7)]
library(dplyr)
df <- df %>%
  mutate(
    始値 = as.numeric(gsub(",", "", 始値))
  )

df <- df %>% arrange(日付け)  # 古い → 新しい
df$Date <- as.Date(df$日付け)


# データをts形式に変換
yt <- (df[2:9544,2] - df[1:9543,2]) / df[1:9543,2]
yt <- ts(yt)
Yt <- ts(df[1:9544,2])
#Yt_open <- Yt[, "始値"] #日付け列を削除
#values_vec <- as.numeric(Yt_open) #ベクトル化
#prime <- values_vec #タグ付け
roc <- yt #タグ付け
prime <- Yt
# プロット
plot(prime, main = "株価")
plot(roc, main = "株価変化率")

# ADF検定
library(tseries)
adf.test(prime)
adf.test(roc)

# ARMA(1,1)
ARMA <- arma(roc, order = c(1,1))
summary(ARMA)$aic

# ARMAモデルの次数の決定
AIC <- function(p,q){
  ARMA <- arma(yt, order = c(p,q))
  summary(ARMA)$aic
}

AIC(1,0);AIC(2,0);AIC(3,0)
AIC(1,1);AIC(2,1);AIC(3,1)
AIC(1,2);AIC(2,2);AIC(3,2)
AIC(1,3);AIC(2,3);AIC(3,3)

# 自動表示バージョン
AIC <- function(p,q){result <- arma(yt,order=c(p,q))
return(summary(result)$aic)
}
d <- c(1,1)
aic <- AIC(1,1)
for(p in 1:3){
  for(q in 1:3){
    aic_pq <- AIC(p,q)
    if(aic_pq < aic){
      aic <- aic_pq
      d <- c(p,q)}}}
#AICの一番低いp,qの表示(参考)
sprintf("p=%d,q=%d",d[1],d[2])

AIC_fun <- function(p, q){
  fit <- arima(yt, order = c(p, 0, q), method = "ML")
  AIC(fit)
}
d <- c(1,1)
aic <- AIC_fun(1,1)

for(p in 1:3){
  for(q in 1:3){
    aic_pq <- AIC_fun(p,q)
    if(aic_pq < aic){
      aic <- aic_pq
      d <- c(p,q)
    }
  }
}

sprintf("p=%d, q=%d", d[1], d[2])



# 推定結果
dat <- na.omit(roc)
result <- arma(roc, order = c(3,2))
summary(result)

# 推定結果の当てはまり
seqplot.ts(roc, fitted(result))

# 将来予測
library(forecast)
ARMA <- arima(roc, method = "CSS", order = c(1,0,1))
yt1 <- forecast(ARMA, h=100)
plot(yt1)

# 次の日(2025/10/15)の株価の予想
head(Yt)
tail(Yt)
summary(Yt)

(1+yt1$mean[1])*12155.00

#実際の始値 12495
