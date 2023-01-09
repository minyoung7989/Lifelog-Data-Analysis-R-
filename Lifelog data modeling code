library(readxl)
dat <- read_excel("lifelog.xlsx")

###로지스틱회귀분석
result <- glm(dat$smoke~dat$gripForce+dat$`sit-ups counts`+dat$`broad jump_cm`,
              family = binomial, data = dat)
summary(result)

round(exp(result$coefficients),2)
###회귀분석
result2 <- lm(dat$gripForce~dat$age+dat$height_cm+dat$weight_kg)
summary(result2)

###나이대에 따른 악력 차이 비교 
dat <- transform(dat,
                 newage = cut(dat$age, breaks = c(0,30,40,50,60,70),
                              include.lowest = TRUE,
                              right = FALSE,
                              labels = c("20대","30대","40대","50대","60대")))

agedata <-aggregate(dat$gripForce ~ dat$newage, data = dat, mean)
resultage <-data.frame(agedata)

barplot(resultage$dat.gripForce~resultage$dat.newage, 
        ylim = c(0,44),main = "나이대별 악력",
        xlab = "나이",ylab = "악력",
        col= "skyblue")
text(0.7,40,"37.6")
text(1.9,42.8,"40.71",col="red")
text(3.1,39,"36.86")
text(4.3,34.8,"32.54")
text(5.5,34.8,"32.58")

###svm 전 전처리
newagedata <- ifelse(dat$age>=40, "40대후", "40대전")
dat$newage <- newagedata
dat$newage <- as.factor(dat$newage)
dat <- dat[,-8]
dat <- dat[,5:8]




###SVM
colnames(dat)
levels(dat$newage)

library(e1071)
nrow(dat)
train <- sample(1:13393, 10000)
sv <- svm(dat$newage ~., data = dat, subset = train, type = "C-classification")
summary(sv)

predict(sv, dat[-train, ])

datatable <- table(dat$newage[-train], predict(sv, dat[-train, ]))

plot(sv, data = dat)

sum(datatable[row(datatable) == col(datatable)])/sum(datatable)


###KNN
library(class)
train.data <- dat[1:10000, -4] 
train.class <- dat$newage[1:10000]

test.data <- dat[10001:13393, -4]
test.class <- dat$newage[10001:13393]

test <- knn(train.data, test.data, train.class, k = 3)
testtable <- table(test, Actual = test.class)

sum(testtable[row(testtable) == col(testtable)])/sum(testtable)
