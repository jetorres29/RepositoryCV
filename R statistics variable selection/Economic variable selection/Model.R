library(psych)
library(MASS)
library(xlsx)

# Read Data ---------------------------------------------------------------

gettingTheData <- function(filePath) {
  data <- na.omit(read.table(filePath, header = TRUE, sep = ","))
}

Data <- gettingTheData("datanew.csv")
janitor::clean_names(Data)
Price <- gettingTheData("pricesnew.csv")

# First Model ------------------------------------------------------------

y <- as.matrix(Data[,1])
X <- as.matrix(Data[,2:51])
Data_Test_200 <- data.frame(y,scale(X))

# Models ------------------------------------------------------------------

# BIC / PMP / MSE
results <- matrix(0, nrow = 50,ncol=10)
for (i in 1:10) {
  data_part <- caret::createDataPartition(y = Data_Test_200[,1], times = 10, p = 0.67, list = F)
  data_test <- Data_Test_200[data_part[,i], ]
  y_test <- as.matrix((data_test[,1]))
  X_test <- as.matrix((data_test[,2:51]))
  model_3 <- BMA::bic.glm(X_test, y_test, glm.family = gaussian(link = "identity"),maxCol=51,
                          strict = FALSE, OR = 20, OR.fix = 2, nbest = 10,
                          occam.window = TRUE)
  results[,i] <- model_3$probne0
}

count<-apply(matrix(sapply(results,function(x){if (x >= 30) {1} else {0}}),50,10),1,sum)
results<- cbind(results,count)
colnames(results)<-c("CV1","CV2","CV3","CV4","CV5","CV6","CV7","CV8","CV9","CV10","conteo")
rownames(results)<-names(data.frame(X))
results

sele_1 <- c(which(results[,11]>0))

model_4 <- BMA::bic.glm(scale(X[,sele_1]), y, glm.family = gaussian(link = "identity"),maxCol=51,
                        strict = FALSE, OR = 20, OR.fix = 2, nbest = 10,
                        occam.window = TRUE)
summary(model_4)


sele_3 <- c(which(model_4$probne0>30))
namesele3<-model_4$namesx[sele_3]
sele_3 <- sele_1[sele_3]


model_5 <- BMA::bic.glm(scale(X[,sele_3]), y, glm.family = gaussian(link = "identity"),maxCol=51,
                        strict = FALSE, OR = 50, OR.fix = 2, nbest = 100,
                        occam.window = TRUE)
summary(model_5)

nvar<-length(sele_3)
mod<-cbind(c(1:nvar),model_5$probne0,sele_3)
costos<-Price[mod[,3],2]
#mod<-mod[order(mod[,2],decreasing = TRUE),]

Grid<-lapply(mod[,2], function(i)c(TRUE,FALSE))
regMat <- expand.grid(Grid)
regMat <- regMat[-dim(regMat)[1],]
regressors<-rownames(mod)

Models<- apply(regMat, 1, function(x) as.formula(paste("y ~", paste(regressors[x],collapse=" + "))))
Models[[length(Models)+1]]<-as.formula(paste("y ~", paste(names(Data_Test_200)[-1],collapse=" + ")))

estimates<-lapply(1:length(Models), function(i) {glm(Models[[i]], data=Data_Test_200, family=gaussian(link = "identity"))})
bics<-unlist(lapply(1:length(estimates), function(i) {stats::BIC(estimates[[i]])}))
mse<- unlist(lapply(1:length(estimates), function(i) {Metrics::mse(predict(estimates[[i]],data.frame(scale(X)),type = "response"),y)}))
R2<- unlist(lapply(1:length(estimates), function(i) {caret::R2(predict(estimates[[i]],data.frame(scale(X)),type = "response"),y)}))
costotal<- unlist(lapply(1:nrow(regMat), function(x) {sum(costos[unlist(regMat[x,])])}))
costotal<-c(costotal,sum(Price[,2]))
nnew<-as.integer(18400000/costotal)

final<-as.matrix(cbind(bics,mse,costotal))
final<-apply(final,2,scales::rescale)
final<-final^2
metrica<-sqrt(apply(final,1,sum))

parametros<-unlist(lapply(1:length(estimates), function(i) {length(estimates[[i]][["coefficients"]])-1}))
costotalbase<-1000
#biccosto<-bics-parametros*log(nrow(Data_Test_200))+(costotal/costotalbase)*log(nrow(Data_Test_200))
biccosto<-bics-parametros*log(nrow(Data_Test_200))+(costotal/(92000/50))*log(nrow(Data_Test_200))

nombres<- apply(regMat, 1, function(x) paste("y ~", paste(regressors[x],collapse=" + ")))
nombres[length(nombres)+1]<-as.character(paste("y ~", paste(names(Data_Test_200)[-1],collapse=" + ")))

Tabla<-as.matrix(cbind(nombres,mse,bics,costotal,metrica,biccosto,nnew))

Tabla

results

summary(model_4)

summary(model_5)

