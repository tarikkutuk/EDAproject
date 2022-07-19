library(readxl)
veri <- read_excel("C:/Users/tarik/Desktop/okul/veri analizi/veri.xlsx")
View(veri)

veri<-as.data.frame(veri)
veri$sex<-factor(veri$sex, levels = c(0,1),labels=c("Kadın","Erkek"))
veri$restecg<-factor(veri$restecg, levels =c(0,1,2), labels = c("Normal","ST-T Dalga Anormalliği","Hipertrofi") )
veri$slope<-factor(veri$slope, levels=c(0,1,2), labels=c("Yukarı Eğimli","Düz","Aşağı Eğimli"))
veri$target<-factor(veri$target, levels = c(0,1), labels= c("Sağlıklı","Hasta"))
summary(veri)

library(tidyverse)
glimpse(veri)

library(funModeling)
niceller<-veri[,c(1,3,4)]
niteller<-veri[,c(2,5,6,7)]
profiling_num(veri)
deneme<-t(profiling_num(niceller))
deneme<-as.data.frame(deneme)
plot_num(veri)
freq(niteller)

#eksik gözlem kontrolü
colSums(is.na(veri))
#veride eksik gözlem yok
#orijinal veriyi bozmamak için veriyi çoğaltıp, kopyasıyla işlem yapılacak
kopya<-veri

kopya[sample(1:nrow(kopya),floor(nrow(kopya)*0.05)),"age"]<- NA
kopya[sample(1:nrow(kopya),floor(nrow(kopya)*0.03)),"slope"]<- NA
colSums(is.na(kopya))

#1 nicel 1 nitel değişkende rasgele kayıp gözlem oluşturuldu

library(mice)
md.pattern(kopya)

library(VIM)
library(ISLR)
aggr(kopya,col=c("navyblue","orange"),numbers=TRUE, sortVars=TRUE, labels=names(kopya),cex.axis=.7,gap=3,ylab=c("Missing Ratio","Missing Pattern"))

#başka bir yol (raporda yok)
#library(funModeling)
#df_status(kopya)
#d_na<-df_status(kopya)
#d_na[,c("variable","p_na")]
eksikler_age<-which(is.na(kopya$age))
eksikler_slope<-which(is.na(kopya$slope))


#eksik gözlem tamamlama yöntemleri
#ortalama ile doldurma
ort_ile<-kopya
hist(ort_ile$age, main = "işlemden önce")
ort_ile$age[is.na(ort_ile$age)]<-mean(ort_ile$age,na.rm=TRUE)
hist(ort_ile$age,main = "işlemden sonra")

#Tahmine Dayalı Doldurma Yontemleri###
#KNN (K-Nearest Neighbor)
library(DMwR2)
knn_ile<-kopya
anyNA(knn_ile)
knn_imp<-knnImputation(knn_ile, k=5, meth="median")
anyNA(knn_imp)
eksikler_age #eksikler
a<-knn_imp$age[eksikler_age]  # Yaş için doldurulmus degerleri
b<-veri$age[eksikler_age]  #orijinal datadaki degerler
c<-knn_imp$slope[eksikler_slope] #slope için doldurlan
d<-veri$slope[eksikler_slope] #orijinal datadaki değeri

karsilastirma<-data.frame(a,b)
karsilastirma<-t(karsilastirma)
table(c,d) #doldurulmuş ve gerçek çapraz tablosu


hist(kopya$age)
hist(knn_imp$age,main = "knn ile")
hist(veri$age, main = "orijinal")
var(veri$age)
var(knn_imp$age)
mean(knn_imp$age)
mean(veri$age)

#Regresyon
library(Hmisc)
regkopya<-kopya
impute_arg<- aregImpute(age ~ sex + trestbps + thalach  + target, data = kopya, n.impute = 1) #restecg ve eksik gözlemi olmayan tüm değişkenler
impute_arg$imputed$age
a<-which(is.na(regkopya$age))
regkopya[a,]$age<-impute_arg$imputed$age

atanan_reg<-impute_arg$imputed$age
karsilastirma<-data.frame(atanan_reg,b)
karsilastirma<-t(karsilastirma)
hist(regkopya$age, main = "regresyon yöntemiyle")
hist(veri$age,main = "orijinal")
var(veri$age)
var(regkopya$age)
mean(veri$age)
mean(regkopya$age)

#karar ağacı
library(rpart)
karar_agc<-kopya
rtree<-rpart(age ~ sex + trestbps + thalach + target, karar_agc, method = "anova")
library(rattle)
fancyRpartPlot(rtree, cex= 0.8)

karar_agc$age<-ifelse(is.na(karar_agc$age), predict(rtree,karar_agc,type = "vector"),karar_agc$age)
a<-karar_agc$age[eksikler_age]
karsilastirma<-data.frame(a,b)
karsilastirma<-t(karsilastirma)
hist(veri$age, main = "orijinal")
hist(karar_agc$age, main = "karar ağacı ile")
var(veri$age)
var(karar_agc$age)
mean(veri$age)
mean(karar_agc$age)

rtree_2 <- rpart(slope ~ sex + trestbps + thalach + target, karar_agc, method="class")
library(rpart.plot)
rpart.plot(rtree_2,cex=0.5)
karar_agc$slope<- ifelse(is.na(karar_agc$slope), predict(rtree_2, karar_agc, type = "class"), karar_agc$slope)
karar_agc$slope<-factor(karar_agc$slope, levels=c(1,2,3), labels=c("Yukarı Eğimli","Düz","Aşağı Eğimli"))
colSums(is.na(karar_agc))
c<-karar_agc$slope[eksikler_slope]
karsilastirma<-data.frame(c,d)
table(c,d)

#yaş değişkeni kategorikleştirmek için

veri2<-veri
percentile00<-min(veri2$age)
percentile100<-max(veri2$age)
percentile33<- quantile(veri2$age, 0.3333)
percentile67<- quantile(veri2$age, 0.6667)

quantile(veri2$age)


veri2$kt_age[veri2$age>= percentile00 & veri2$age< percentile33]<-"0"
veri2$kt_age[veri2$age>= percentile33 & veri2$age< percentile67]<-"1"
veri2$kt_age[veri2$age>= percentile67 & veri2$age<= percentile100]<-"2"
veri2$kt_age<-factor(veri2$kt_age, levels = c(0,1,2), labels = c("genç","orta yaşlı","yaşlı"))
summary(veri2)

freq(veri2$kt_age)

#train-test veriyi bölmek için

set.seed(8367325)
trainIndex <- sample(1:nrow(veri2), size = round(0.8*nrow(veri2)), replace=FALSE)
train_veri <- veri2[trainIndex ,]
test_veri <- veri2[-trainIndex ,]

train_veri<-as.data.frame(train_veri)
test_veri<-as.data.frame(test_veri)

library("openxlsx")
write.xlsx(train_veri, 'train_veri.xlsx')
write.xlsx(test_veri, 'test_veri.xlsx')

summary(train_veri)
summary(test_veri)

library(dplyr)
describeBy(train_veri, train_veri$target)

library(ggplot2)
ggplot(train_veri, aes(x=target,y=train_veri$trestbps, fill=kt_age))+
  geom_boxplot()   


ggplot(train_veri, aes(x=target,y=thalach, fill=kt_age))+
  geom_boxplot()   

#Değişim ölçüleri

#değişim katsayısı karşılaştırmaları
stdev<-sd(train$Age)
mean<-mean(train$Age)
Degisim_kats_age<-(stdev/mean)*100

age_degisim_kts<-(sd(train_veri$age)/mean(train_veri$age))*100
trestbps_degisim_kts<-(sd(train_veri$trestbps)/mean(train_veri$trestbps))*100
thalach_degisim_kts<-(sd(train_veri$thalach)/mean(train_veri$thalach))*100
degisimkatsayileri<-c(age_degisim_kts, trestbps_degisim_kts, thalach_degisim_kts)

karsilastirma3<-data.frame(degisimkatsayileri, colnames(niceller))
karsilastirma3

sd_dk <- function(x) {c(std<-sd(x), dk<-(sd(x)/mean(x))*100)}
tapply(train_veri$age, train_veri$target, sd_dk)

sort <- train[order(train$Age),]
medianf<-median(sort$Age)
sort$fmed<-abs(sort$Age-medianf)
sort2 <- sort[order(sort$fmed),]
mad<-median(sort2$fmed)


#mad değerleri
median_age<-median(train_veri$age)
median_trestbps<-median(train_veri$trestbps)
median_thalach<-median(train_veri$thalach)

yas_mad<-median(abs(train_veri$age-median_age))
trestbps_mad<-median(abs(train_veri$trestbps-median_trestbps))
thalach_mad<-median(abs(train_veri$trestbps-median_thalach))

mads<-data.frame(yas_mad,trestbps_mad,thalach_mad)
mads

#kuyruklar
sol <- function(x) {
  c(quantile(x, probs = 1/2) , 
    quantile(x, probs = 1/4),
    quantile(x, probs =1/8 ),
    quantile(x,probs=1/16),
    quantile(x,probs=1/32),
    quantile(x,probs=1/64)
  )
}

sag <- function(x) {
  c(quantile(x, probs = 1/2) , 
    quantile(x, probs = 3/4),
    quantile(x, probs = 7/8),
    quantile(x,probs=15/16),
    quantile(x,probs=31/32),
    quantile(x,probs=63/64)
  )
}

h<-tapply(train_veri$age, train_veri$target, sol)
mrg_age<-as.data.frame(cbind(h[[1]],h[[2]]))
colnames(mrg_age)<-c("Hasta","Sağlıklı")
mrg_age$fark<-abs(mrg_age$Hasta-mrg_age$Sağlıklı)
mrg_age

i<-tapply(train_veri$age, train_veri$target, sag)
mrg_age2<-as.data.frame(cbind(i[[1]],i[[2]]))
colnames(mrg_age2)<-c("Hasta","Sağlıklı")
mrg_age2$fark<-abs(mrg_age2$Hasta-mrg_age2$Sağlıklı)
mrg_age2

### Kesilmiş ortalama
mean(train_veri$age, trim = 0.1)
nrow(train_veri)- (as.integer(2*0.1*nrow(train_veri)))

mean(train_veri$trestbps)
mean(train_veri$trestbps, trim = 0.1)
nrow(train_veri)- (as.integer(2*0.1*nrow(train_veri)))

mean(train_veri$thalach)
mean(train_veri$thalach, trim = 0.1)
nrow(train_veri)- (as.integer(2*0.1*nrow(train_veri)))

# geometrik ortalamalar
geometric.mean(train_veri$age)
geometric.mean(train_veri$trestbps)
geometric.mean(train_veri$thalach)

#GİNİ

gini <- function(a,b) {
  a1 <- (a/(a+b))**2
  b1 <- (b/(a+b))**2
  x<-1-(a1 + b1)
  return(x)
}
gini2 <- function(a,b,c) {
  a1 <- (a/(a+b+c))**2
  b1 <- (b/(a+b+c))**2
  c1 <- (c/(a+b+c))**2
  x<-1-(a1 + b1+c1)
  return(x)
}

sex_freq<-as.data.frame(table(train_veri$sex))
sex_gini<-gini(sex_freq[1,2],sex_freq[2,2]) / ((2-1)/2)

restecg_freq<-as.data.frame(table(train_veri$restecg))
restecg_gini<-gini2(restecg_freq[1,2],restecg_freq[2,2],restecg_freq[3,2])/((3-1)/3)

slope_freq<-as.data.frame(table(train_veri$slope))
slope_gini<-gini2(slope_freq[1,2],slope_freq[2,2],slope_freq[3,2])/((3-1)/3)

target_freq<-as.data.frame(table(train_veri$target))
target_gini<-gini(target_freq[1,2],target_freq[2,2])/((2-1)/2)

kt_age_freq<-as.data.frame(table(train_veri$kt_age))
kt_age_gini<-gini2(kt_age_freq[1,2],kt_age_freq[2,2],kt_age_freq[3,2])

nitel_isimler<-c("sex","restecg","slope","target","kt_age")
gini_degerleri<-c(sex_gini,restecg_gini,slope_gini,target_gini,kt_age_gini)
gini_degerleri<-round(gini_degerleri,2)
ginies<-as.data.frame(gini_degerleri,nitel_isimler)
ginies  

#ENTROPİ
entropy<-function(base,a,b) {
  var <-  abs(((a)/(a+b))*log(((a)/(a+b)),base))-(((b)/(a+b))*log(((b)/(a+b)),base))
  return(var)
}

entropy2<-function(base,a,b,c) {
  var <-  abs(((a)/(a+b+c))*log(((a)/(a+b+c)),base))-(((b)/(a+b+c))*log(((b)/(a+b+c)),base))-(((c)/(a+b+c))*log(((c)/(a+b+c)),base))
  return(var)
}

ent_sex<-entropy(10,sex_freq[1,2],sex_freq[2,2])
z_ent_sex<-ent_sex/log(2,10)

ent_restecg<-entropy2(10, restecg_freq[1,2],restecg_freq[2,2],restecg_freq[3,2])
z_ent_restecg<-ent_restecg/log(3,10)

ent_slope<-entropy2(10, slope_freq[1,2],slope_freq[2,2],slope_freq[3,2])
z_ent_slope<-ent_slope/log(3,10)

ent_target<-entropy(10, target_freq[1,2],target_freq[2,2])
z_ent_target<-ent_target/log(2,10)

ent_kt_age<-entropy2(10, kt_age_freq[1,2], kt_age_freq[2,2],kt_age_freq[3,2])
z_ent_kt_age<-ent_kt_age/log(2,10)

entropi_degerleri<-c(ent_sex,ent_restecg,ent_slope,ent_target,ent_kt_age)
entropi_z_degerleri<-c(z_ent_sex,z_ent_restecg,z_ent_slope,z_ent_target,z_ent_kt_age)
entropies<-data.frame(nitel_isimler,entropi_degerleri, entropi_z_degerleri)
entropies

#grafiklerle inceleme
train_veri$no<-c(1:nrow(train_veri))
plot_num(train_veri[,c(1,3,4)])




library(tidyverse)
ggplot(train, aes(Age,FEV))+
  geom_point(size=2,shape=21,stroke=1,color="dodgerblue1", fill="white")+
  geom_smooth(method = "lm", col="darkblue",se = FALSE)
ggplot(train_veri aes(train_veri$age, train_veri$thalach))+
  geom_point(size=2, shape=20, stroke=1 color="dodgerblue1", fill="white")+
  geom_smooth(method = "lm" , col="darkblue", se= FALSE)

library(tidyverse)

j<-ggplot(train_veri, aes(age,thalach, color=target))+
  geom_point(size=1,alpha=0.8)+
  geom_text(size=3,label= train_veri$no ,nudge_x=0.25,
            nudge_y=0.25, check_overlap=T)+
  geom_smooth(method = "loess", col="red",se = FALSE)
ggplotly(j)


k<-ggplot(train_veri, aes(age,trestbps, color=target))+
  geom_point(size=1,alpha=0.8)+
  geom_text(size=3,label=train_veri$no,nudge_x=0.25,
            nudge_y=0.25, check_overlap=T)+
  geom_smooth(method = "loess", col="red",se = FALSE)
ggplotly(k)

l<-ggplot(train_veri, aes(trestbps,thalach, color=target))+
  geom_point(size=1,alpha=0.8)+
  geom_text(size=3,label=train_veri$no,nudge_x=0.25,
            nudge_y=0.25, check_overlap=T)+
  geom_smooth(method = "loess", col="red",se = FALSE)
ggplotly(l)
plot_num(niceller)

gr<-ggplot(train_veri,aes(x=thalach,y=trestbps))+
  geom_point()+
  geom_text(size=3,label=train_veri$no,nudge_x=0.25,
            nudge_y=0.25, check_overlap=T)+
  geom_smooth(method=lm,col="brown1", se=FALSE)

ggMarginal(gr,type="histogram",fill="darksalmon")

#corelasyon matrisi
niceller_train<-train_veri[,c(1,3,4)]
cor(niceller_train, method = "kendall")#Korelasyon degerleri
plot(niceller_train)
ggpairs(niceller_train)#yogunluk+sacılım+corr


chart.Correlation(niceller_train, histogram=TRUE, pch=19, method = "kendall")
#ortanca izi çizimi

m<-train_veri %>%group_by(kt_age) %>%
  summarize(Q1=quantile (thalach, probs=0.25), Median=quantile (thalach, probs=0.50), Q3=quantile(thalach, probs=0.75), DAG=Q3-Q1)
m
ggplot(m, aes(kt_age,Median))+
  geom_point(size=3,alpha=0.6)

ggplot(m, aes(x=DAG,y=Median, color=kt_age, group=1))+
  geom_point(size=4,alpha=0.6, color="blue")+
  geom_line(color="red")



#Konum Varyans Çizimleri
n<-train_veri %>%group_by(restecg) %>%
  summarize(Q1=quantile (trestbps, probs=0.25), Trestbps_median=quantile (trestbps, probs=0.50), Q3=quantile(trestbps, probs=0.75), DAG=Q3-Q1)
n

ggplot(n, aes(x=restecg,y=DAG, color=restecg, group=3))+
  geom_point(size=4,alpha=0.6)+
  geom_line(color="black")

#ETKILESIM 
#2 değişenin yani cinsiyet ile yaşın birlikte etkisi var mı yok mu bunu incelemek icin:
etk_yas_cinsiyet<-train_veri%>%
  group_by(sex,kt_age)%>% 
  summarise(Medyan_trestbps=median(trestbps))
etk_yas_cinsiyet

ggplot(etk_yas_cinsiyet, aes(x = kt_age, y = Medyan_trestbps,color=sex, group=sex)) +
  geom_line() +
  geom_point()

#dönüşüm
#yaş için dönüşüme gerek yok
#trestbps için:

trestbps_tukey<-transformTukey(train_veri$trestbps,plotit=TRUE)
#lambda -0.725 çıktı
train_veri$trestbps2<- -1*(train_veri$trestbps)^-0.725
hist(train_veri$trestbps2)
mshapiro_test(train_veri$trestbps2)
hist(train_veri$trestbps)

#kan basıncı için de üzeri -1*x^-0,725 dönüşümü yapılmış ve normal dağılmıştır.
#outlier tespiti ve grafikler için
outlierr_func <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
outlierr_func(train_veri$trestbps2)

train_veri <- train_veri %>%  group_by(target) %>%
  mutate(outlierr_func=ifelse(outlierr_func(trestbps2), trestbps2, as.numeric(NA)))

train_veri$no[which(is.na(train_veri$outlierr_func))] <- as.numeric(NA)



#z<-ggplot(train_veri, aes(age,trestbps2, color=target))+
 # geom_point(size=1,alpha=0.8)+
  #geom_text(size=3,label=train_veri$no, color="black")+
  #geom_smooth(method = "loess", col="red",se = FALSE)
ggplotly(z)

ggplot(train_veri, aes(y=trestbps2, x=factor(target),fill=target))+
  geom_boxplot() + 
  geom_text(aes(label=no),na.rm=TRUE,nudge_x=0.15,size=3.5)

profiling_num(train_veri$trestbps2)

#THALACH
train_veri$no2<-c(1:nrow(train_veri))
#kalp atış hızı da sola çarpıktır. yine k>1 olacak şekilde x^k dönüşümü denenecektir.
thalach_tukey<-transformTukey(train_veri$thalach, plotit = T)
#lambda şu şu bulunmuş

train_veri$thalach2<-train_veri$thalach^2.2
hist(train_veri$thalach, main = "Dönüşüm Öncesi")
hist(train_veri$thalach2, main = "Dönüşüm Sonrası")

train_veri <- train_veri %>%  group_by(target) %>%
  mutate(outlierr_func2=ifelse(outlierr_func(thalach2), thalach2, as.numeric(NA)))

train_veri$no2[which(is.na(train_veri$outlierr_func2))] <- as.numeric(NA)

ggplot(train_veri, aes(y=thalach2, x=factor(target),fill=target))+
  geom_boxplot() + 
  geom_text(aes(label=no2),na.rm=TRUE,nudge_x=0.15,size=3.5)


#aykırı olduğu tespit edilen gözlemler ile grafik vs için oluşturulan geçici sütunlar siliniyor.
train_veri_son<-train_veri[-c(160,231,76),-c(9,10,13,14,15)]
train_veri_son

chart.Correlation(niceller, histogram=TRUE, pch=19)
chart.Correlation(train_veri_son[,c(9,10,1)])

#Karar ağacı ile modelleme

#önce daha önce yapılan dönüşümler test kümesi için de yapılmalı
test_veri$trestbps2<- -1*(test_veri$trestbps)^-0.725
test_veri$thalach2<-test_veri$thalach^2.2



d_tree<-rpart(target~ age + sex+ trestbps2+ thalach2+ restecg+ slope, data = train_veri_son, method = "class")
rpart.plot(d_tree)

confmattrain<-table(predict(d_tree, train_veri_son, type = "class"), train_veri_son$target)
confusionMatrix(confmattrain, positive="Sağlıklı")

confmattest<-table(predict(d_tree,test_veri, type = "class"), test_veri$target)
confusionMatrix(confmattest, positive = "Sağlıklı")

d_tree2<-rpart(target~ age+sex+trestbps+thalach+restecg+slope, data=train_veri_son,method = "class" )
rpart.plot(d_tree2)

confmattrain2<-table(predict(d_tree2, train_veri_son,type = "class"), train_veri_son$target)
confusionMatrix(confmattrain2, positive = "Sağlıklı" )

confmattest2<-table(predict(d_tree2,test_veri,type = "class"), test_veri$target)
confusionMatrix(confmattest2, positive = "Sağlıklı")


freq(veri2$kt_age)
