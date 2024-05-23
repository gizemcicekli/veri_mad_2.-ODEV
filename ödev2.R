#BİL524 Veri Madenciliği Ödevler Hafta - 4: 21.03.2024 / Platformlar, 
#R Temel Bilgiler ve Veri Özetleme (Teslim: 27.03.24)

#Temel Fonksiyonlar
getwd()

# İki kare farkını hesaplayan fonksiyon

iki_kare_farkı<-function(x,y) {
  
  return(x^2-y^2)}

# Fonksiyonu kullanarak iki kare farkını hesaplama

print(iki_kare_farkı(5,4))

#açılımı  

açılım<- function(x,y) {
  return((x-y)*(x+y) )}

print(açılım(5,4))


#----------------
# while döngüsü 

a <- 9             #a=9 dan başlasın

while(a > 5) {     #a 5 ten büyük olana kadar 
  print(a)         #a yı yazdır
  a <- a-1         # her satırda bir azaltarak 
}

# while döngüsü içeren fonksiyon b

ben <- function(n) {     # ben adında fonksiyon n değişkenini alsın  
  b <- 1                 #b 1 den başlasın
  while (b <= n) {       #b n'e eşit olana kadar fonksiyon çalışsın   
    
    print(paste("gizem", b))  #gizem yanına b yı yazdır
    b <- b + 1  }             #her saturda b 1 artsın
}

# Fonksiyonu çağırma
ben(5) 

#----------------

# SPSS'den veri1 setini çağırma

veri <- read_sav("C:/Users/MİHRİBAN/Desktop/sp/Veri1.sav")

View(veri)  #veriyi görüntüleme
head(veri) # verinin  ilk altı gözlemini (satırını)
dim(veri) #verinin boyutunu (satır ve sütun sayısını)
ncol(veri) #satır sayısı

help (factor)

install.packages("dplyr") #veri manipülasyonu ve dönüşümleri paketi
library(dplyr)            #paketi etkinleştirir

yeniad <- rename(veri, egitim_durumu = egitim, medenihal=medeni) #sutun ismi değiştirildi

names(veri)
names(yeniad)

install.packages("readr")
library(readr)

yeniad2 <- rename(yeniad, medeniii=medenihal, eğitimmm=egitim_durumu)

#yeniad2 datasını SPSS formatında bir dosyaya yaz. 
write_sav(yeniad2, path= "C:\\Users\\MİHRİBAN\\Desktop\\aeıi\\veri1.sav")  


ls() # oluşturulan nesneleri gösterir

install.packages("tidyverse") 
library(tidyverse)



veri2<-data.frame(ChickWeight) #airquality veri setinden veri2'yi oluştma

tavukagırlık <- rename(veri2,agırlık=weight, zaman=Time, tavuk=Chick, beslenme=Diet)
veri2<-data.frame(tavukagırlık)

dim(veri2)

head(veri2)

head(ChickWeight)

veri2_tibble=as_tibble(veri2)

head(veri2_tibble)

veri2yeni=dplyr::rename_all(veri2_tibble, toupper)

names(veri)

veri$cinsiyet

veri$meslek

veri2yeni$BESLENME

veri_cinsiyet=veri$cinsiyet

veri_egit=select(veri, egitim)

pull(select(veri,yas))

veri_yas=pull(select(veri,yas))

veri_yas_fil=filter(veri, yas>35 |yas<24) #"yas" sütununda değeri 35'ten büyük olan veya 24'ten 
                                          # küçük olan gözlemleri filtreler.

library(dplyr)

veri_piped <- veri %>%
  filter(yas > 24 & cinsiyet == 1) %>%
  select(egitim, medeni)

veri2 $ yenisütun=veri2 $ agırlık/2   #yenisütun ağırlık ikiye bölündü
view(veri2)

veri2=tavukagırlık           #yenisütun ile bozulan ilk veri eski haline çağrldı
view(veri2)

veri2_mut <- mutate(veri2, yenisutun =agırlık / 4) #yenisütun ağırlık 4 e bölündü

yaskat <- mutate(veri,
                 yaskateg= ifelse(yas<=24,
                                  "genç",
                                  ifelse(yas<=45,
                                         "orta",
                                         "yaslı")
                                    
                                    ))

arrange(veri2, desc(agırlık))    # ağırlığı küçükten büyüye sırala



transmute(veri2, axc=agırlık*3,tavuk, beslenme) # ağırlık*3 ve diğer iki değişken

head(transmute(veri2, axc=agırlık*3,tavuk, beslenme)) # üst kodun ilk 6 sütunu





