#Lecture 29 R code
lambda <-  3
n <-  100
x <-  rep(n, lambda)

lambdaMLE <-  1/mean(x)

B <-  1e6
lambdaboot <-  rep(NA, B)

for (i in 1:B) {
  xboot = rexp(n, lambdaMLE)
  lambdaboot[i]= 1/mean(xboot)
}


head(lambdaboot)
head(lambdaMLE)
hist(lambdaboot, 100)
class(lambdaboot)
ub <- quantile(lambdaboot, 0.975)
lb <- quantile(lambdaboot, 0.025)


lambdaboot.df <-  as.data.frame(lambdaboot)
ggplot(data = lambdaboot.df, mapping = aes(x= lambdaboot)) +
  geom_histogram(bins = 100)+
  labs(x= "MLE Estimates", y = "count")+
  geom_vline(xintercept = ub)+
  geom_vline(xintercept = lb)+
  theme_classic()
