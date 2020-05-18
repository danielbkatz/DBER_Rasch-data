npers <- 1000
nitems <- 15

abil <- rnorm(npers, 0, 1)
diff <- matrix(runif(nitems, -3, 3), ncol=1)

View(diff)

discrim <- matrix(data=c(15, -2, .99, 1.1, 1, rep(1, 10)), ncol=1)
items <- as.data.frame(cbind(diff, discrim))
names(items) <- c("d", "a")
probs <- lapply(abil, function(x)exp(items$a*(x)-items$d)/(1+exp(items$a*(x)-items$d)))

                
rand <- matrix(runif(nitems*npers), ncol=nitems, nrow=npers)
probs <- do.call("rbind", probs)
resp <- 1* (probs > rand)

write.csv(resp, "data/dichotomous.csv")

mod2 <- tam.mml(resp)
fit2 <- tam.fit(mod2)
View(fit2$itemfit)

plot(mod2)
