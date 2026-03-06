# tseriesやvarsを読み込む、データを導入
install.packages("tseries")
install.packages("vars")
install.packages("dplyr")

setwd("C:/Users/npcta/OneDrive/Desktop/seminar_game/raw_data")

#データ結合
nintendo <- read.csv("nintendo_tokyo.csv",fileEncoding = "CP932",stringsAsFactors = FALSE)
sony     <- read.csv("sony_tokyo.csv",fileEncoding = "CP932",stringsAsFactors = FALSE)

nintendo <- nintendo[, -c(2, 4, 5, 6, 7)]
library(dplyr)
nintendo <- nintendo %>%
  mutate(
    始値 = as.numeric(gsub(",", "", 始値))
  )

sony <- sony[, -c(2, 4, 5, 6, 7)]
library(dplyr)
sony <- sony %>%
  mutate(
    始値 = as.numeric(gsub(",", "", 始値))
  )

nintendo$Date <- as.Date(nintendo$日付け)
sony$Date     <- as.Date(sony$日付け)

library(dplyr)

df <- inner_join(
  nintendo %>% select(Date, nt = 始値),
  sony     %>% select(Date, st = 始値),
  by = "Date"
)

df <- df %>% arrange(Date)

# 任天堂
nt <- (df[2:9544,2] - df[1:9543,2]) / df[1:9543,2]
nt <- ts(nt)
Nt <- ts(df[1:9544,2])

plot(Nt, main = "任天堂株価")
plot(nt, main = "任天堂株価 変化率")

library(tseries)
Nt <- na.omit(Nt)
adf.test(Nt)
nt <- na.omit(nt)
adf.test(nt)

# sony
st <- (df[2:9544,3] - df[1:9543,3]) / df[1:9543,3]
st <- ts(st)
St <- ts(df[1:9544,3])

plot(St, main = "sony株価")
plot(st, main = "sony株価 変化率")

library(tseries)
St <- na.omit(St)
adf.test(St)
st <- na.omit(st)
adf.test(st)

dat <- cbind(nt,st) # 2つの時系列をひとつにまとめる

# VARモデル次数選択
library(vars)
aic <- function(p) AIC(VAR(dat, p, type = "const"))
aic(1); aic(2); aic(3); aic(4); aic(5)    

# # 自動表示バージョン
# aic <- function(p) return(AIC(VAR(dat,p,type = "const")))
# p0 <- 1
# aic1 <- aic(1)
# for(p in 2:20){
#   aic2 <- aic(p)
#   if(aic2 < aic1){
#     p0 <- p
#     aic1 <- aic2
#   }
# }
# sprintf("p0=%d",p0)

# VAR(3)
var3 <- VAR(dat, 3, type = "const")
summary(var3)

# Granger Test(Win用)
library(vars)

# 任天堂 → ソニー
causality(var3, cause = "nt")

# ソニー → 任天堂
causality(var3, cause = "st")


# インパルス応答
plot(irf(var3, n.ahead = 6))