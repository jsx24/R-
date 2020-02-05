# data set is collected form 60 Standard Metropolitan Statistical Areasthe in United States. 
# 17 variables and 60 observations\


dataf<-read.csv(file="/Users/jiasheng/Desktop/finaldata.csv",head=T,sep=',')
dataf4<-dataf[,c(2:16)]
dataf1<-dataf[,c(2:14)]
dataf2<-dataf[,c(2:13,15)]
dataf3<-dataf[,c(2:16)]
alm<-lm(log(HCPot)~.,data=dataf1)
blm<-lm(log(NOxPot)~.,data=dataf2)
clm<-lm(S02Pot~.,data=dataf3)
plot(alm)
plot(blm)
plot(clm)
cof<-cor(dataf1)
summary(alm)
# correlation matrix plot 
library(corrplot)
corrplot(cof, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
alias(alm)
plot(alm)
# here is the code for cross validation, I didn't use this result in my report
# optional for people to use 
x_vars<-model.matrix(Mortality~.,data=dataf1)[,-5]
y_var<-dataf1$Mortality
lambda_seq<-10^seq(5,-5,by=-.1)
set.seed(86)
train = sample(1:nrow(x_vars), nrow(x_vars)/2)
x_test = (-train)
y_test = y_var[-train]
cv_output <- cv.glmnet(x_vars[train,], y_var[train], 
                       alpha = 1, lambda = lambda_seq)
best_lam <- cv_output$lambda.min
best_lam
# here is the lasso code, I didn't use it in my final report 
lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, lambda = best_lam)
pred <- predict(lasso_best, s = best_lam, newx = x_vars[-train,])
final <- cbind(y_var[-train], pred)
head(final)
cutoff <- 4/((nrow(dataf1)-length(alm$coefficients)-2))
plot(fit, which=4, cook.levels=cutoff)
# influence plot, for outlier test 
influencePlot(alm, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )
influencePlot(blm, id.method="identify", main="Influence Plot",
              sub="Circle size is proportial to Cook's Distance" )
influencePlot(clm, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )
vif(alm)
vif(blm)
vif(clm)
fd1<-dataf1[-c(12,29,49,50,59)]
fd2<-dataf2[-c(7,29,38,50,59)]
fd3<-dataf3[-c(12,29,38,40,59)]
# test for log transformation, but not necessary
falm<-lm(log(HCPot)~.,data=fd1)
fblm<-lm(log(NOxPot)~.,data=fd2)
fclm<-lm(S02Pot~.,data=fd3)
plot(falm)
summary(falm)
vif(falm)

aplot<-dataf4 %>%
  gather(-Mortality, key = "some_var_name", value = "some_value_name") %>%
  ggplot(aes(x = some_value_name, y = Mortality)) +
  geom_point() +
  facet_wrap(~ some_var_name, scales = "free")+
  theme_bw()
aplot
fl1<-lm(Mortality~Rain+Education+X.NonWhite+S02Pot,data=dataf4)
influencePlot(fl1, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )
dataf41<-dataf4[-c(2,12,28,32,37,40),]
fl2<-lm(Mortality~Rain+Education+X.NonWhite+S02Pot,data=dataf41)
summary(fl2)
plot(fl2)
w<-1 / lm(abs(fl2$residuals) ~ fl2$fitted.values)$fitted.values^2
lmw <- lm(Mortality~Rain+Education+X.NonWhite+S02Pot, 
          data = dataf41, 
          weights = w)
plot(lmw)
summary(lmw)
require(lmtest)
vif(lmw)
plot(fl2)
SD<-
  lmw1<-lm(Mortality~Rain+Education+X.NonWhite+S02Pot, 
           data = dataf41, 
           weights = 1/SD^2)
bptest(fl2)
# relative weight, and this fuction could generate the plot 
relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import),1, drop=FALSE]
  dotchart(import$Weights, labels=row.names(import),
           
           xlab="% of R-Square", pch=19,
           
           main="Relative Importance of Predictor Variables",
           
           sub=paste("Total R-Square=", round(rsquare, digits=3)),
           
           ...)
  
  return(import)
}
relweights(fl3, col="blue")
vif(fl2)
anova(fl2)
summary(fl2)
fl3<-lm(Mortality~Rain+Education+X.NonWhite+S02Pot,data=dataf4)
summary(fl3)
anova(fl3)
dwtest(fl3)
bptest(fl3)
