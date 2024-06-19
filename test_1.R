if (!require(mgcv)) install.packages("mgcv")
library(mgcv)

set.seed(123)
n <- 500 # 样本量
x1 <- rnorm(n)
x2 <- rnorm(n)
z <- runif(n, min=-3, max=3)
eta <- 0.5*x1 + 0.5*x2 + sin(z)
mu <- exp(eta) / (1 + exp(eta))
y <- rbinom(n, size=1, prob=mu)

df <- data.frame(x1, x2, z ,y)
write.csv(df, file = "sim_data_1.csv", row.names = FALSE)


model1 <- gam(y ~ x1 + x2 + s(z), family = binomial(link = 'logit'))
all_coeffs <- coef(model1)
linear_coeffs_direct <- all_coeffs[!names(all_coeffs) %in% grep("s\\(", names(all_coeffs), value = TRUE)]
print(0.5-unname(linear_coeffs_direct[2:3])) # 输出线性部分的系数残差
summary(model1)

model2 <- gam(y ~ x1 + x2 + s(z), family = binomial(link = 'logit'), method="REML")
all_coeffs <- coef(model2)
linear_coeffs_direct <- all_coeffs[!names(all_coeffs) %in% grep("s\\(", names(all_coeffs), value = TRUE)]
print(0.5-unname(linear_coeffs_direct[2:3])) # 输出线性部分的系数残差
summary(model2)


par(mfrow=c(1,2)) 
plot(model1, select = 1, seWithMean = TRUE, residuals = TRUE, rug = TRUE, shade=TRUE,shade.col=2)
plot(model2, select = 1, seWithMean = TRUE, residuals = TRUE, rug = TRUE, shade=TRUE,shade.col=2)