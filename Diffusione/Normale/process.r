nxt <- function(d, mu=0., sigma=1.) {
  n <- length(d$x)
  d$x <- d$x + rnorm(n,mu,sigma)
  d$y <- d$y + rnorm(n,mu,sigma)
  return(d)
}

plot.snap <- function(d, i, prefix, edge=10.0) {
  png(sprintf("%s_%5.5d.png", prefix, i), width=760, height=760)
  plot(d$x, d$y, xlim=c(-edge/2.,edge/2.), ylim=c(-edge/2.,edge/2.), main="", xlab="", ylab="", cex=0.2, col="grey")
  dev.off()
}

snaps <- function(n, n.iter, prefix, mu=0.0, sigma=1.0, edge=10.0) {
  std.x <- numeric(n.iter)
  std.y <- numeric(n.iter)
  d <- data.frame(x=numeric(n), y=numeric(n))
  for(i in 1:n.iter) {
    plot.snap(d, i, prefix, edge)
    d <- nxt(d, mu, sigma)
    std.x[i] <- sd(d$x)
    std.y[i] <- sd(d$y)
    print(sprintf("Iteration %d made", i))
  }
  return(list(data=d,std.x=std.x,std.y=std.y))
}

process.normal <- function(n.part=3000, n.snaps=900) {
  par(mfrow=c(1,1))
  l<-snaps(n.part, n.snaps, "normal/snap", 0.0, 0.03, 10)
  d <- l$data
  png(file="normal/marginal.png", width=760*2, height=760)
  par(mfrow=c(1,2))
  hist(d$x, main="", xlab="x", ylab="Densità", prob=TRUE, col="pink")
  lines(density(d$x),lw=2,col="red")
  mu <- mean(d$x)
  s  <- sd(d$x)
  x.min <- -10.0/2
  x.max <-  10.0/2
  x <- seq(from=x.min,to=x.max,by=10/100)
  lines(x, dnorm(x,mu,s), col="black", lw=2)
  rug(jitter(d$x))
  hist(d$y, main="", xlab="y", ylab="Densità", prob=TRUE, col="pink")
  lines(density(d$y),lw=2,col="red")
  mu <- mean(d$y)
  s  <- sd(d$y)
  y.min <- -10.0/2
  y.max <-  10.0/2
  y <- seq(from=y.min,to=y.max,by=10/100)
  lines(y, dnorm(y,mu,s), col="black", lw=2)
  rug(jitter(d$y))
  par(mfrow=c(1,1))
  dev.off()
  png(file="normal/devstd.png", height=720, width=720)
  par(mfrow=c(2,1))
  plot((1:n.snaps)/30, l$std.x, type="l", xlab="Tempo (s)", ylab="Dev.std. asse X (m)", col="red")
  plot((1:n.snaps)/30, l$std.y, type="l", xlab="Tempo (s)", ylab="Dev.std. asse Y (m)", col="red")
  par(mfrow=c(1,1))
  dev.off()
}
