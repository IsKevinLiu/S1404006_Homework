if (!require(mgcv)) install.packages("mgcv")
library(mgcv)

set.seed(23)
n <- 500 # 样本量
x1 <- rnorm(n,mean = 0,sd=1)
x2 <- rnorm(n,mean = 0,sd=1)
z1 <- runif(n, min=-3, max=3)
z2 <- runif(n, min=-3, max=3)
eta <- 0.5*x1 + 0.5*x2 + z1*z1 + sin(z2)
mu <- exp(eta) / (1 + exp(eta))
y <- rbinom(n, size=1, prob=mu)

df <- data.frame(x1, x2, z1, z2 ,y)
write.csv(df, file = "sim_data_2.csv", row.names = FALSE)

model1 <- gam(y ~ x1 + x2 + s(z1) + s(z2), family = binomial(link = 'logit'))
all_coeffs <- coef(model1)
linear_coeffs_direct <- all_coeffs[!names(all_coeffs) %in% grep("s\\(", names(all_coeffs), value = TRUE)]
print(0.5-unname(linear_coeffs_direct[2:3])) # 输出线性部分的系数残差
summary(model1)

model2 <- gam(y ~ x1 + x2 + s(z1) + s(z2), family = binomial(link = 'logit'), method="REML")
all_coeffs <- coef(model2)
linear_coeffs_direct <- all_coeffs[!names(all_coeffs) %in% grep("s\\(", names(all_coeffs), value = TRUE)]
print(0.5-unname(linear_coeffs_direct[2:3])) # 输出线性部分的系数残差
summary(model2)


par(mfrow=c(2,2))
plot(model1, select = 1, seWithMean = TRUE, residuals = TRUE, rug = TRUE, shade=TRUE,shade.col=2)
plot(model2, select = 1, seWithMean = TRUE, residuals = TRUE, rug = TRUE, shade=TRUE,shade.col=2)

plot(model1, select = 2, seWithMean = TRUE, residuals = TRUE, rug = TRUE, shade=TRUE,shade.col=2)
plot(model2, select = 2, seWithMean = TRUE, residuals = TRUE, rug = TRUE, shade=TRUE,shade.col=2)