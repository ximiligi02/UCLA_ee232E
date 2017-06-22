library('igraph')
#library('netrw')
library('ggplot2')

#read and open file
g<-read.graph("facebook_combined.txt",format="ncol",directed=FALSE)

#connectivity, diameter and degree
conn<-is.connected(g)
d<-diameter(g)
deg<-degree(g)
dd<-degree.distribution(g)
plot(dd,type = "h", main = "Degree Distribution of the FB Graph",xlab = "degree", ylab = "density")

#check some possible models
r <- hist(degree(g), breaks=seq(0, by=1 , length.out=max(deg)+1))
rdata = data.frame(x=r$mids, y=r$density)
models<-list(lm(y ~ log(x),data = rdata),
             nls(y ~ I(1/x*a) + b*x, data = rdata, start = list(a = 1, b = 1)),
             nls(y ~ I(1/x*a)+b, data=rdata, start = list(a=1,b=1)),
             nls(y ~ I(exp(1)^(a + b * x)), data = rdata, start = list(a=0,b=0)))

# have a visual sense of the fitness of models
ggplot(rdata, aes(x, y)) + geom_point(size = 1)+
  stat_smooth(method = "lm", formula = as.formula(models[[1]]), size = 1, se = FALSE, colour = "red")+
  stat_smooth(method = "nls", formula = as.formula(models[[2]]), data=rdata, start = list(a=1,b=1), size = 1, se = FALSE, colour = "blue")+
  stat_smooth(method = "nls", formula = as.formula(models[[3]]), data=rdata, start = list(a=1,b=1), size = 1, se = FALSE, colour = "yellow")+
  stat_smooth(method = "nls", formula = as.formula(models[[4]]), data=rdata, start = list(a=0,b=0), size = 1, se = FALSE, colour = "green")

#after the judgement we choose the fourth model
summary(models[[4]])

#model: y=exp(1)^(-3.594_0.029*x)
fdata = data.frame(x=r$mids, y=exp(1)^(-3.594-0.029*r$mids))

#mean square error and average degree
MSE=sum((rdata$y-fdata$y)^2)/max(deg)
d_average = mean(deg)

