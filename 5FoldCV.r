set.seed (1)
x=rnorm (100)
y=x-2* x^2+ rnorm (100)

library(boot)
Data = data.frame(x, y)

cv.error.5=rep (0,5)
for(i in 1:5){
  glm1.fit = glm((y ~ x))
  cv.error.5[i]=cv.glm(Data ,glm1.fit ,K=5) $delta [1]
}
cv.error.5


cv.error.5=rep (0,5)
for(i in 1:5){
  glm2.fit = glm(y ~ poly(x,2))
  cv.error.5[i]=cv.glm (Data ,glm2.fit ,K=5) $delta [1]
}
cv.error.5


cv.error.5=rep (0,5)
for(i in 1:5){
  glm3.fit = glm(y ~ poly(x,3))
  cv.error.5[i]=cv.glm (Data ,glm3.fit ,K=5) $delta [1]
}
cv.error.5


cv.error.5=rep (0,5)
for(i in 1:5){
  glm4.fit = glm(y ~ poly(x,4))
  cv.error.5[i]=cv.glm (Data ,glm4.fit ,K=5) $delta [1]
}
cv.error.5
glm1.fit$aic
x<- c(1,2,3,4)
y <- c(glm1.fit$aic,glm2.fit$aic,glm3.fit$aic,glm4.fit$aic)
plot(x,y)
lines(x,y)
