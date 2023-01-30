options(max.print = 15)
iris1<-iris
iris2<-iris
iris3<-iris
l<-list(iris1,iris2,iris3)
names(l)<-c("a","b","c")
l<-list(iris,iris,iris)
names(l)<-c("a","b","c")
n<-c("A","B","C","D","E")
l1 <- lapply(l, setNames, n)
z<-c("xyear")
fum<-function(x){
  colnames(x)<-paste(z,names(x),sep = "+")
  x<-print(x)
}
a<-map(l1,fum)
