local_hospital <- read.csv("/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/健保醫事服務機構位置/健保特約_地區醫院_complete_地址分割核對.csv", header = TRUE, sep = ",")
medical_center <- read.csv("/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/健保醫事服務機構位置/健保特約_醫學中心_地址解析結果_完整資料.csv", header = TRUE, sep = ",")
medical_center_reference <- read.csv("/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/健保醫事服務機構位置/39280-健保署-健保特約醫事機構-醫學中心_ 健保特約醫事機構_醫學中心.csv", header = TRUE, sep = ",")
clinic <- read.csv("/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/健保醫事服務機構位置/健保特約_診所_地址解析結果_完整資料.csv", header = TRUE, sep = ",")

city_coding <- read.csv("/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/縣市代碼表.csv", header = TRUE, sep = ",")

Keelung_City_housenumber <- read.csv("/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/門牌座標資料/114年基隆市門牌_202506.CSV", header = TRUE, sep = ",")
Taipei_housenumber <- read.csv("/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/門牌座標資料/114年臺北市門牌位置數值資料_20250704.CSV", header = TRUE, sep = ",")
NewTaipei_housenumber <- read.csv("/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/門牌座標資料/114年新北市門牌位置數值資料.CSV", header = TRUE, sep = ",")
Taoyuan_City_housenumber <- read.csv("/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/門牌座標資料/114年桃園市門牌位置.CSV", header = TRUE, sep = ",")
Hsinchu_County_housenumber <- read.csv("/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/門牌座標資料/113年新竹縣門牌位置.CSV", header = TRUE, sep = ",")
Hsinchu_City_housenumber <- read.csv("/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/門牌座標資料/114年新竹市門牌座標.CSV", header = TRUE, sep = ",")
Miaoli_County_housenumber <- read.csv("/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/門牌座標資料/113年苗栗縣門牌座標資料.CSV", header = TRUE, sep = ",")
Taichung_City_housenumber <- read.csv("/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/門牌座標資料/114年3月門牌_台中市_TGOS_66_WGS84.CSV", header = TRUE, sep = ",")
Changhua_County_housenumber <- read.csv(
  "/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/門牌座標資料/彰化縣門牌點位資料1140501.CSV", 
  header = TRUE, 
  sep = ",",
  fileEncoding = "Big5"
)

library(purrr)
library(readr)

# 雲林縣檔案合併
files_to_read <- c(
  "/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/門牌座標資料/雲林縣門牌資料1.CSV",
  "/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/門牌座標資料/雲林縣門牌資料2.CSV",
  "/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/門牌座標資料/雲林縣門牌資料3.CSV"
)
# 讀取並合併
Yunlin_County_housenumber <- files_to_read %>%
  map_dfr(~ read_csv(.x, locale = locale(encoding = "UTF-8")))

library(stringr)
library(dplyr)

# 假設「號」的欄位名稱是 "號"
Yunlin_County_housenumber <- Yunlin_County_housenumber %>%
  mutate(號 = str_replace_all(號, "I-", "號"))

library(readr)
guess_encoding("/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/門牌座標資料/114年台東縣市門牌座標.CSV")

Chiayi_County_housenumber <- read.csv(
  "/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/門牌座標資料/嘉義縣門牌位置_202502.CSV", 
  header = TRUE, 
  sep = ",",
  fileEncoding = "Big5"
)

Tainan_City_housenumber <- read.csv(
  "/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/門牌座標資料/113年臺南市門牌坐標資料.CSV", 
  header = TRUE, 
  sep = ",",
  fileEncoding = "UTF-8"
)

Kaohsiung_City_housenumber <- read.csv(
  "/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/門牌座標資料/113年高雄市門牌座標_TWD97.CSV", 
  header = TRUE, 
  sep = ",",
  fileEncoding = "UTF-8"
)

Pingtung_County_housenumber <- read.csv(
  "/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/門牌座標資料/113年屏東縣門牌.CSV", 
  header = TRUE, 
  sep = ",",
  fileEncoding = "UTF-8"
)

Penghu_County_housenumber <- read.csv(
  "/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/門牌座標資料/114年澎湖縣門牌位置.CSV", 
  header = TRUE, 
  sep = ",",
  fileEncoding = "UTF-8"
)

Kinmen_County_housenumber <- read.csv(
  "/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/門牌座標資料/113年金門縣門牌位置.CSV", 
  header = TRUE, 
  sep = ",",
  fileEncoding = "UTF-8"
)

Taitung_County_housenumber <- read.csv(
  "/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/門牌座標資料/114年台東縣市門牌座標.CSV", 
  header = TRUE, 
  sep = ",",
  fileEncoding = "Big5"
)

# 檢查資料基本資訊
dim(Taitung_County_housenumber)  # 查看行列數
head(Taitung_County_housenumber)  # 查看前幾行
colnames(Taitung_County_housenumber)  # 查看欄位名稱
str(Taitung_County_housenumber)  # 查看資料結構