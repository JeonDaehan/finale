##### 치킨 #####
# 1
# H0 : ChMcnt 유무에 따라 Likes에 차이가 없다
# H1 : ChMcnt 유무에 따라 Likes에 차이가 있다

ch <- read.csv("D:/Acorn/final_project/data/test_ch.csv")

shapiro.test(ch$Likes[ch$ChMcnt==0])
shapiro.test(ch$Likes[ch$ChMcnt==1])

library(moonBook)
moonBook::densityplot(Likes~ChMcnt,data=ch)   # 정규성 만족 x

var.test(Likes~ChMcnt,data=ch)   # 등분산 만족 x

# T-TEST
wilcox.test(Likes~ChMcnt,data=ch)   

# 결과
# ChMcnt(치킨맛집 키워드) 유무에 따라 Likes에 차이가 있다고 할 수 있다

# 2
# H0 : Stargram 유무에 따라 Likes에 차이가 없다
# H1 : Stargram 유무에 따라 Likes에 차이가 있다

shapiro.test(ch$Likes[ch$Stargramcnt==0])
shapiro.test(ch$Likes[ch$Stargramcnt==1])

moonBook::densityplot(Likes~Stargramcnt,data=ch) 

var.test(Likes~Stargramcnt,data=ch)   # 등분산 만족 x

# T-TEST
wilcox.test(Likes~Stargramcnt,data=ch) 


# 결과
# Stargramcnt(맛스타그램 또는 먹스타그램 키워드 포함) 유무에 따라 Likes에 차이가 있다고 할 수 있다

# ----------------------------------------------------
# 광고|협찬|제공 1, 일상 2, 이외 0
# H0 : 광고 및 일상 키워드 포함 여부에 따라 좋아요 평균에 차이가 없다
# H1 : 광고 및 일상 키워드 포함 여부에 따라 좋아요 평균에 차이가 있다
ch<- read.csv("D:/Acorn/final_project/data/test_ch_addaily.csv")
str(ch)
ch <- ch[,-1]

shapiro.test(ch$Likes[ch$Ad_.Daliy_sum==0])
shapiro.test(ch$Likes[ch$Ad_.Daliy_sum==1])
shapiro.test(ch$Likes[ch$Ad_.Daliy_sum==2])   # 3개다 정규분포 만족 x

# 정규분포를 확인하는 또 다른 방법
out <- aov(Likes ~ Ad_.Daliy_sum, data=ch)
resid(out) # aov에서 잔차만 뽑아라
shapiro.test(resid(out))  # 정규분포 만족 x

# bartlett.test(Likes ~ Ad_.Daliy_sum, data=ch)  # 등분산성 만족 x

# 분산분석 (정규성을 만족하지 않으므로 kruskal 사용)
kruskal.test(Likes ~ Ad_.Daliy_sum, data=ch)    # 차이가 있다
# 사후 검정
library(pgirmess)

kruskalmc(ch$Likes, ch$Ad_.Daliy_sum)

cor.test(ch$Likes[ch$Ad_.Daliy_sum==1], ch$Likes[ch$Ad_.Daliy_sum==2])

library(dplyr)
ad1 <- ch$Likes[ch$Ad_.Daliy_sum==1]
ad2 <- ch$Likes[ch$Ad_.Daliy_sum==2]

ch %>% group_by(Ad_.Daliy_sum) %>% summarise(mean_Likes=mean(Likes))




##### 떡복이 #####
# 1
# H0 : TpkMcnt 유무에 따라 Likes에 차이가 없다
# H1 : TpkMcnt 유무에 따라 Likes에 차이가 있다

tpk <- read.csv("D:/Acorn/final_project/data/test_tpk.csv")

shapiro.test(tpk$Likes[tpk$TpkMcnt==0])
shapiro.test(tpk$Likes[tpk$TpkMcnt==1])

library(moonBook)
moonBook::densityplot(Likes~TpkMcnt,data=tpk)   # 정규성 만족 x

var.test(Likes~TpkMcnt,data=tpk)   # 등분산 만족 x

# T-TEST
wilcox.test(Likes~TpkMcnt,data=tpk)   

# 결과
# ChMcnt(치킨맛집 키워드) 유무에 따라 Likes에 차이가 있다고 할 수 있다

# 2
# H0 : Stargram 유무에 따라 Likes에 차이가 없다
# H1 : Stargram 유무에 따라 Likes에 차이가 있다

shapiro.test(tpk$Likes[tpk$Stargramcnt==0])
shapiro.test(tpk$Likes[tpk$Stargramcnt==1])

moonBook::densityplot(Likes~Stargramcnt,data=tpk) 

var.test(Likes~Stargramcnt,data=tpk)   # 등분산 만족 x

# T-TEST
wilcox.test(Likes~Stargramcnt,data=tpk) 


# 결과
# Stargramcnt(맛스타그램 또는 먹스타그램 키워드 포함) 유무에 따라 Likes에 차이가 있다고 할 수 있다


##### 치킨/떡볶이 합친거 #####

# 1
# H0 : CMcnt 유무에 따라 Likes에 차이가 없다
# H1 : CMcnt 유무에 따라 Likes에 차이가 있다

ck <- read.csv("D:/Acorn/final_project/data/test_ck.csv")

shapiro.test(ck$Likes[ck$CMcnt==0])
shapiro.test(ck$Likes[ck$CMcnt==1])

library(moonBook)
moonBook::densityplot(Likes~CMcnt,data=ck)   # 정규성 만족 x

var.test(Likes~CMcnt,data=ck)   # 등분산 만족 x

# T-TEST
wilcox.test(Likes~CMcnt,data=ck)   

# 결과
# ChMcnt(치킨맛집 키워드) 유무에 따라 Likes에 차이가 있다고 할 수 있다

# 2
# H0 : Stargram 유무에 따라 Likes에 차이가 없다
# H1 : Stargram 유무에 따라 Likes에 차이가 있다

shapiro.test(ck$Likes[ck$Stargramcnt==0])
shapiro.test(ck$Likes[ck$Stargramcnt==1])

moonBook::densityplot(Likes~Stargramcnt,data=ch) 

var.test(Likes~Stargramcnt,data=ck)   # 등분산 만족 x

# T-TEST
wilcox.test(Likes~Stargramcnt,data=ck) 

# 결과
# Stargramcnt(맛스타그램 또는 먹스타그램 키워드 포함) 유무에 따라 Likes에 차이가 있다고 할 수 있다


