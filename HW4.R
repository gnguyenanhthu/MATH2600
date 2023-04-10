#### HW 4 ############################

####PROBLEM 3.1.16
petable<-function(pa,pb,pc,pe){
  #Create table 2 in section 3.1 of text
  #sensitivity analysis on college population estimates
  #True values:
  #pa is # students aged 18-22  (30% of students)
  #pb is # students over 23     (3% of students)
  #pc is # students under 18    (1% of students)
  #pe is absolute percentage error
  dpa<-pe*pa
  dpb<-pe*pb
  dpc<-pe*pc
  Pa<-c(rep("+",4), rep("-",4))
  Pap<-c(rep(pa+dpa,4), rep(pa-dpa,4))
  Pb<-rep(c(rep("+",2), rep("-",2)),2)
  Pbp<-rep(c(rep(pb+dpb,2), rep(pb-dpb,2)),2)
  Pc<-rep(c("+","-"),4)
  Pcp<-rep(c(pc+dpc,pc-dpc),4)
  pmat<-cbind(Pap,Pbp,Pcp) 
  pt<-pmat%*%c(0.3, 0.03, 0.01)
  Ppredict<-pt
  Ptrue<-rep(0.3*pa+0.03*pb+0.01*pc, length(Pap))
  Error<-round(Ppredict-Ptrue,2)
  PercentError<-round(Error/Ptrue,2)
  print(cbind(Pa,Pb,Pc,pmat,Ppredict,Ptrue,Error,PercentError))
}
print("Problem 3.1.16")
petable(10,80,50,0.1)

#####PROBLEM 3.2.18
regression<-function(x,y){
  #finds equation for regression line
  #input: x and y are arrays for input and output
  #output: m and b, the slope and y-intercept of regression line
  n<-length(x)
  m<-(n*sum(x*y)-sum(x)*sum(y))/(n*sum(x^2)-(sum(x))^2)
  b<-(sum(y)-m*sum(x))/n
  return(c(m,b))
}

print("Problem 3.2.18 (see graph)")
#test regression code using data from 3.2.1
x<-seq(1,6)
y<-c(0.8,2.4,4,5.1,7.3,9.4)
fitline<-regression(x,y)
print(fitline)
layout(matrix(c(1,1,2,2),4,1,byrow=TRUE))
plot(x,y)
lines(fitline[1]*x+fitline[2])
title("Problem 3.2.18")

######PROBLEM 3.3.7
rolldice<-function(){
  maxRsq<-0
  roll<-seq(1,5)
  for (i in 1:100){
    first<-ceiling(6*runif(5))
    second<-ceiling(6*runif(5))
    rollfit<-regression(first,second)
    #need to handle error if denominator of Rsq is 0
    if(abs(sum((second-sum(second)/5)^2))>0.001){
      Rsq<-sum((rollfit[1]*first+rollfit[2]-sum(second)/5)^2)/sum((second-sum(second)/5)^2)
    } else {
      Rsq<-0
    }
    if(Rsq>maxRsq){
      maxRsq<-Rsq
      maxfirst<-first
      maxsecond<-second
      maxrollfit<-rollfit
    }
  }
  print(cbind(roll,maxfirst,maxsecond))
  return(c(maxrollfit,maxRsq))
}

print("Problem 3.3.7")
roll<-rolldice()
print(roll)

######PROBLEM 3.4.14
print("Problem 3.4.14")
multipleregression<-function(x,y,z){
  #find multivariable linear regression equation and 
  #R^2
  #inputs: x, y, and z where z=a*x+b*y+c
  #outputs: a, b, c and R^2
  n<-length(x)
  multimat<-rbind(c(sum(x^2), sum(x*y),sum(x)),
                  c(sum(x*y), sum(y^2),sum(y)),
                  c(sum(x), sum(y), n))
  multiRHS<-rbind(sum(x*z),sum(y*z),sum(z))
  abc<-solve(multimat)%*%multiRHS
  zmod<-abc[1]*x+abc[2]*y+abc[3]
  Rsq<-sum((zmod-sum(z)/n)^2)/
    sum((z-sum(z)/n)^2)
  return(c(abc,Rsq))
}

#Test multipleregression function using data from Example 1
height<-c(68,70,71,68,68,76,73.5,75.5,73,72)
waist<-c(34,32,31,29,34,34,38,34,36,32)
weight<-c(160,160,150,120,175,190,205,215,185,170)
fitplane<-multipleregression(waist,height,weight)
predictedweight<-fitplane[1]*waist+fitplane[2]*height+fitplane[3]
print(cbind(waist,height,weight,predictedweight))
print(fitplane)

####PROBLEM 3.5.16
t<-seq(0,20)
ft<-(((1+sqrt(5))/2)^t-((1-sqrt(5))/2)^t)/sqrt(5)
print("Problem 3.5.16")
print(cbind(t,ft))

#####PROBLEM 3.6.9
epicycle<-function(v1,v2){
  t<-seq(0,2*pi,2*pi/100)
  xm<-4*cos(v1*t)+cos(v2*t)
  ym<-4*sin(v1*t)+sin(v2*t)
  plot(xm,ym, lty=1)
  title("Problem 3.6.9")
  return(cbind(xm,ym))
}

epicycle(1,6)

