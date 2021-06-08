dataDublin=read.csv("./dublin_property.csv",header=T,stringsAsFactors = TRUE)
View(dataDublin)
str(dataDublin)

dataDublin$area[which(dataDublin$area=="null")]=NA
dataDublin$bed[which(dataDublin$bed=="null")]=NA
data=na.omit(dataDublin)

data$price=as.numeric(gsub('[,€]', '', data$price))
data$area=as.numeric(gsub('[ac,m²]','',data$area))
data$bed=as.numeric(gsub('[Bed]','',data$bed))
#apartment type is 1, house type is 0
data$type=factor(data$type,levels = c("Apartment","Bungalow","Detached","Duplex","End of Terrace",
                                      "Semi-D","Terrace","Townhouse"),
                labels=c("1","0","0","1","0","0","0","1"))
#
data$district=as.numeric(gsub('[W]','6',data$district))

View(data)
str(data)
summary(data)
table(data$type)
table(data$bed)

#creating a random sample for training and testing data
set.seed(1234)
data_rand=data[order(runif(2389)),]

data_train=data_rand[1:1911, ]
data_test=data_rand[1912:2389, ]

#build model
model=glm(type ~ district +area + bed +price,
                   data=data_train,family="binomial")
summary(model)

modeldata=glm(type ~ district +area + bed + latitude + longitude + price
              ,data=data_train,family = "binomial")
summary(modeldata)
coef(modeldata)
#Y=1.005762e+00*1 + 9.974900e-01*2 + 1.082732e+01*3 + 5.242799e-01*4 + 6.687032e-01*5 + 1.000000e+00*6 
exp(coef(modeldata))

exp(confint(modeldata))

library(pROC)
pre=predict(modeldata,data)
modelroc=roc(data$type,pre)
#AUC=0.88, good model
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)

newdata=data.frame(district=8,
                   area=33,
                   bed=1,
                   latitude=53.34129, 
                   longitude=-6.28018,
                   price=195000)

newdata1=data.frame(district=24,
                    area=68,
                    bed=3,
                    price=280000)

prediction=predict(modeldata,newdata,type="response")
prediction1=predict(model,newdata1,type="response")
print(prediction)
print(prediction1)
