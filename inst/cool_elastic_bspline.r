library(splines)
library(glmnet)
x <- seq(0, 1, 0.001)
y <- sin(5*x) + 2*x^2 + 0.8*x - 2 + rnorm(length(x),0,.1)

mod <- lm( y ~ x + bs(x,degree=5))

summary(mod)

xg <- cbind(x,bs(x,degree=5))
modg <- cv.glmnet(x=xg,y=y,relax=TRUE,times=50)

plot(modg)
modg

coef(modg,lambda="lambda.min",gamma="gamma.min")

pr <- predict(modg, newx = xg, gamma = "gamma.min", s = "lambda.min")


plot( x,y,col='red')
points( x,pr, pch = 3, cex = .1)


ym <- splines2::iSpline(x,degree = 1)
plot(x,ym[,2])


yb <- splines::bs(x,
  knots = c(
    #.1,.2,.21,.22,.3,.4,.5,.6,.8
    #0,.15
    knots
  ),
  #df = dff
  #,Boundary.knots = c(0,1)
  )
plot(x,yb[,1],col=1,ylim=c(-1,max(yb)))
for (i in 2:ncol(yb)) {
 points(x,yb[,i],col=i) 
}


yb <- splines::ns(x,
                      # knots = c(
                      #   #0,.1,.2,.21,.22,.3,.4,.5,.6,.9
                      #   #0,.15
                      #   knots
                      # ),
                  df = 11
                  #,Boundary.knots = c(0,1)
)
plot(x,yb[,1],col=1,ylim=c(-1,max(yb)))
for (i in 2:ncol(yb)) {
  points(x,yb[,i],col=i) 
}



x <- seq(2,12,length.out=100)
yb <- splines::bs(x,
                  # knots = c(
                  #   #0,.1,.2,.21,.22,.3,.4,.5,.6,.9
                  #   #0,.15
                  #   knots
                  # ),
                  df = 3
                  #degree = 3
                  #,Boundary.knots = c(0,1)
)
plot(x,yb[,1],col=1,ylim=c(-1,max(yb)))
for (i in 2:ncol(yb)) {
  points(x,yb[,i],col=i) 
}

yc <- yb %*% c(6,32,0)
plot(x,yc)

