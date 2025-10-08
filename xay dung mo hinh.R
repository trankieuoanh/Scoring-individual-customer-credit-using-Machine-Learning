

library(MASS)
library(tidyverse)
library(splines)
library(gam)
View(Boston)

str(Boston)
attach(Boston)

reg1=lm(medv~rad)
summary(reg1)

Boston$rad=as.factor(Boston$rad)
reg2=lm(medv~rad)
summary(reg2)

#dùng plot để mô tả giá nhà 
Boston%>% ggplot(aes(rad, medv))+geom_boxplot()
# cho 6,4 vào 1 nhóm
# 1,5 vào 1 nhóm     7,3 vào 1 nhóm   

Boston%>% ggplot(aes(reorder(rad,medv), medv))+geom_boxplot() # nhóm cho mình 
# Với biến liên tục

# Với biến rời rạc
ggplot(Boston, aes(lstat, medv))+ 
  geom_point(shape=21, fill="lightblue")+
  geom_line(alpha=0.4)+
  geom_smooth(method="lm", se=FALSE)+
  geom_smooth(method="gam",se=FALSE, color="red")





# Khi xây dựng MH, có 2 sai số giảm thiểu được: Phương sai, độ chệch
# Sai số của MH= Var(f^)+(f-E(f^))^2 = Phương sai của MH ước lượng+^2 của sai lệch giữa
# giá trị actual và kì vọng của f^2


# Sự khác nhau giữa 3 đường:
# MH zich zac: 
# Xanh 
# Đỏ
#  Xây dựng MH:
# cách 1:tăng bậc: y=beta+betaX1+beta2^2+beta3X^3+...
# Cách 2: Splines: đa thứ từng phần: chia dữ liệu thành các phần, mỗi phần dùng
# một đa thức ( thường là hàm bậc 3)
# Nếu chia thành 2 phần: 2 đường mô tả cho từng phần dữ liệu:nhưng bị gãy => có 2 đa thức
# p(1): Beta01+...
# p(2): Beta02+....
# Thêm ràng buộc: Cho p1(c)=p2(c),p1'(c)=p2'(c), p1''(c)=p2''(c),.. ( giảm 3 tham số)
# Số tham sô cho cả 2 đa thức sẽ giảm
# Nếu chia data làm 3: 2 điểm cắt: 12 parameter, sẽ giảm đi 6
# Nếu có k điểm cắt:chia thành (k+1) phần, có 4(k+1) phần, số paremeter giảm đi: 3k
# sô parameter dùng : 4(k+1)-3k=k+4
# Khi x-> -vc, +vc thì hàm bậc 2,3 không phản ánh đúng
# => thêm 2 rb hàm bậc 1 khi x -> vc()(natural splines) ( mỗi rb giảm 2 tham số)
# => Tổng số tham số k+4-2-2=k

RMSE=function(y,y.hat)sqrt(mean((y-y.hat)^2))



cb.slines=lm(medv~bs(lstat,knots=median(lstat)), data=Boston )
y.pred=predict(cb.slines, Boston)
RMSE(Boston$medv,y.pred)

cb.slines=lm(medv~bs(lstat,knots=quantile(lstat, c(0.33,0.66))), data=Boston )
y.pred=predict(cb.slines, Boston)
RMSE(Boston$medv,y.pred)
# hàm bs()

cb.slines=lm(medv~ns(lstat,knots=quantile(lstat, c(0.2,0.4,0.6,0.8))), data=Boston )
y.pred=predict(cb.slines, Boston)
RMSE(Boston$medv,y.pred)
# Ham ns()

ggplot(Boston, aes(lstat, medv))+ 
  geom_point(shape=21, fill="lightblue")+
  geom_line(alpha=0.4)+
  geom_smooth(method="lm", formula = y~ns(x,knots=quantile(lstat, c(0.2,0.4,0.6,0.8))),
              se=FALSE, color="red")

# MH GAM
smooth.s=gam(medv~s(lstat, df=5.8)+
               s(rm, df=9.6), data=Boston)
y.pred=predict(smooth.s, Boston)
RMSE(Boston$medv, y.pred)
# bậc tự do df= số parameter-1, chọn đến n-1


# Tree-based model
RSS=function(y,y.hat){sum((y-y.hat)^2)}
cut.point=8
R1=filter(Boston, lstat<cut.point)
R2=filter(Boston, lstat>cut.point)
mu1=mean(R1$medv)
mu2=mean(R2$medv)

RSS(R1$medv,mu1)+ RSS(R2$medv,mu2)


RSS=function(y,y.hat){sum((y-y.hat)^2)}
lstat.range=sort(unique(Boston$lstat))
n=length(lstat.range)
cut.point.list=(lstat.range[1:(n-1)]+lstat.range[2:(n)])/2
saiso=rep(0,(n-1))
for(j in 1:(n-1)){
  cut.point=cut.point.list[j]
  R1=filter(Boston, lstat<cut.point)
  R2=filter(Boston, lstat>=cut.point)
  mu1=mean(R1$medv)
  mu2=mean(R2$medv)
  saiso[j]=RSS(R1$medv,mu1)+ RSS(R2$medv,mu2)
  
}

plot(cut.point.list, saiso, type="l")






cut.point=8
R1=filter(Boston, lstat<cut.point)
R2=filter(Boston, lstat>cut.point)
mu1=mean(R1$medv)
mu2=mean(R2$medv)

RSS(R1$medv,mu1)+ RSS(R2$medv,mu2)












Boston=mutate(rad.group=rad1)

rad1=Boston$rad
rad1[Boston$rad==24]<-"G1"
rad1[Boston$rad %in% c(6,4)]<-"G2"
rad1[Boston$rad %in% c(1,5,2)]<-"G3"
rad1[Boston$rad %in% c(7,3,8)]<-"G4"
 reg3=lm(medv~rad, data=Boston )
 summary(reg3)
 
# MH giá nhà phụ thuộc medv
 
 Boston%>% ggplot(aes(lstat, medv))+
   geom_point(shape=21, fill="lightblue")+
 theme_minimal()
 
 
Boston%>% ggplot(aes(lstat, medv))+
  geom_point(shape=21, fill="lightblue")+
  geom_smooth(method="lm",formula=y~x, se=FALSE, limestyle=2, color="red")
  theme_minimal()
 
  
  
  
library(caret)
K=5
index=createFolds(Boston$medv,k=K)

cv.error=rep(0,K) # vector luu sai so

for(i in 1:K){
  test.index=index[[i]]
  test=Boston[test.index,] # dữ liệu kiểm tra MH( test)
  train=Boston[-test.index,] # data huấn luyện mh( train)
  lm1=lm(medv~lstat, data=train) # xây dựng mh ( hồi quy theo lstat)
  medv.pred= predict(lm1,test) # dự đoán trên data test
  cv.error[i]= RMSE(medv.pred, test$medv)  # sai số trên data test
}

mean(cv.error)

for(i in 1:K){
  test.index=index[[i]]
  test=Boston[test.index,] # dữ liệu kiểm tra MH( test)
  train=Boston[-test.index,] # data huấn luyện mh( train)
  lm1=lm(medv~., data=train) # xây dựng mh ( hồi quy theo tất cả các biến)
  medv.pred= predict(lm1,test) # dự đoán trên data test
  cv.error[i]= RMSE(medv.pred, test$medv)  # sai số trên data test
}

mean(cv.error)
# sai số là 6148
str(index)

# phương pháp thêm biến
 # P số biến của data
# Bước 1: lựa chọn MH 1 biến tốt nhất ( chạy P mh)
# B2: lựa chọn MH có 2 biến tốt nhất: có C2 P biến ( chạy P-1 mh)
#B3: chọn MH có 3 biến best( phải có 2 biến ở B2)
# ....
# MH có p biến best

# So sánh giữa các best với nhau

# Phương pháp ngược lại: Backward
#B1: MH có p biến có best
# B2: MH có p-1 biến có best ( bỏ đi 1 trong p biến)
#...
# MH có 1 biến best


  
  
  
  
  
  
  
