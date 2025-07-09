# 門牌資料集處理指引 - 多縣市智能匹配系統

## 📋 指引目的

本指引說明如何為多縣市智能匹配系統準備和處理門牌資料集，確保系統能正確識別不同縣市的資料格式並達到最佳匹配效果。

## 🎯 核心要求

### 必要欄位
每個門牌資料集必須包含以下三個核心欄位：
1. **X座標欄位** - 橫向座標 (TWD97格式)
2. **Y座標欄位** - 縱向座標 (TWD97格式)  
3. **街道欄位** - 街路段資訊

### 建議欄位
以下欄位可提升匹配精度：
- **村里欄位** - 村里名稱
- **地區欄位** - 地區資訊

## 📊 標準欄位命名規範

### 座標欄位命名 (按優先順序)
```
X座標: 橫坐標 > TWD97橫坐標 > x_3826 > X座標 > x座標 > X > x
Y座標: 縱坐標 > TWD97縱坐標 > y_3826 > Y座標 > y座標 > Y > y
```

### 街道欄位命名 (按優先順序)
```
街道: 街.路段 > 街路段 > 街_路段 > 街和路段 > 地址 > 街.路段.
```

### 其他欄位命名
```
村里: 村里 > village
地區: 地區 > area
```

## ⚠️ 常見問題與解決方案

### 1. 座標欄位問題

**問題**: 系統顯示「架構異常」
```
❌ XX縣 - 架構異常
```

**檢查方法**:
```r
# 檢查欄位名稱
colnames(你的資料集名稱)

# 檢查座標欄位內容
head(你的資料集名稱$橫坐標)
head(你的資料集名稱$縱坐標)
```

**常見原因**:
- 座標欄位名稱不符合規範
- 座標值為NA或非數值
- 座標值超出台灣合理範圍

**解決方案**:
```r
# 重新命名欄位
names(你的資料集)[names(你的資料集) == "舊欄位名"] <- "橫坐標"
names(你的資料集)[names(你的資料集) == "舊欄位名"] <- "縱坐標"

# 轉換為數值格式
你的資料集$橫坐標 <- as.numeric(你的資料集$橫坐標)
你的資料集$縱坐標 <- as.numeric(你的資料集$縱坐標)

# 過濾異常座標
你的資料集 <- 你的資料集[
  !is.na(你的資料集$橫坐標) & 
  !is.na(你的資料集$縱坐標) &
  你的資料集$橫坐標 > 100000 & 你的資料集$橫坐標 < 400000 &
  你的資料集$縱坐標 > 2000000 & 你的資料集$縱坐標 < 3000000,
]
```

## ⚠️ 特殊格式注意事項

### 🔧 需要特殊處理的縣市

#### 1. 新竹市 (`Hsinchu_City_housenumber`)
**特殊之處**: 使用 `地址` 欄位而非 `街.路段`
```r
# 系統自動處理方式
# 從地址欄位提取街道資訊
門牌道路 = sapply(地址, function(addr) {
  road_match <- regexpr("[^區鎮鄉縣]{1,15}[路街道大道]", addr)
  if(road_match[1] != -1) {
    raw_road <- substr(addr, road_match[1], road_match[1] + attr(road_match, "match.length") - 1)
    return(str_replace_all(raw_road, "台", "臺"))
  }
  return("")
})
```

#### 2. 澎湖縣 (`Penghu_County_housenumber`)
**特殊之處**: 街道欄位名稱為 `街.路段.` (多一個點)
```r
# 欄位對應
X座標: 橫坐標
Y座標: 縱坐標  
街道: 街.路段.  # 注意多了一個點
```

#### 3. 台中市 (`Taichung_City_housenumber`)
**特殊之處**: 欄位數最多 (13欄)，資料最豐富
- 包含額外的分析欄位
- 可能有更詳細的地理資訊

#### 4. 基隆市 (`Keelung_City_housenumber`)
**特殊之處**: 欄位數較少 (10欄)
- 可能缺少某些輔助欄位
- 但核心匹配欄位完整

### 📋 各縣市標準欄位結構

#### 標準11欄結構 (大多數縣市)
```
1. 省市縣市代碼
2. 鄉鎮市區代碼  
3. 村里
4. 鄰
5. 街.路段      # 核心街道欄位
6. 地區
7. 巷
8. 弄
9. 號樓
10. 橫坐標      # 核心X座標
11. 縱坐標      # 核心Y座標
```

#### 新竹市特殊結構 (7欄)
```
1. [欄位1]
2. [欄位2]
3. 村里
4. [欄位4]
5. 地址         # 特殊: 使用地址而非街.路段
6. 橫坐標       # 核心X座標
7. 縱坐標       # 核心Y座標
```

#### 台中市豐富結構 (13欄)
```
1-11. 標準欄位 (同上)
12-13. 額外分析欄位
```

### 🎯 座標品質檢查標準

#### TWD97座標合理範圍
```r
# X座標 (橫坐標) 合理範圍
100,000 < X < 400,000

# Y座標 (縱坐標) 合理範圍  
2,000,000 < Y < 3,000,000

# 檢查程式碼
valid_coords <- 資料集$橫坐標 > 100000 & 資料集$橫坐標 < 400000 &
                資料集$縱坐標 > 2000000 & 資料集$縱坐標 < 3000000
```

#### 各縣市座標範圍參考
- **北部** (台北、新北、桃園): X: 250K-330K, Y: 2.74M-2.80M
- **中部** (台中、彰化、南投): X: 190K-250K, Y: 2.64M-2.71M  
- **南部** (台南、高雄、屏東): X: 170K-230K, Y: 2.43M-2.58M
- **東部** (台東、花蓮): X: 280K-350K, Y: 2.55M-2.68M
- **離島** (澎湖、金門): X: 160K-210K, Y: 2.60M-2.63M

## 🔧 資料集準備檢查清單

### 載入前檢查
- [ ] 確認資料集為 data.frame 格式
- [ ] 檢查必要欄位是否存在且命名正確
- [ ] 確認座標值為數值格式且在合理範圍內
- [ ] 檢查街道資訊完整性

### 載入後驗證
```r
# 執行架構檢查
schema <- detect_housenumber_schema("你的資料集名稱")
print(schema)

# 確認 is_valid = TRUE
if(schema$is_valid) {
  cat("✅ 資料集架構正常\n")
} else {
  cat("❌ 需要修正以下問題:\n")
  if(is.na(schema$x_col)) cat("- 找不到X座標欄位\n")
  if(is.na(schema$y_col)) cat("- 找不到Y座標欄位\n") 
  if(is.na(schema$street_col)) cat("- 找不到街道欄位\n")
}
```

## 📁 縣市資料集對照表

目前支援的縣市及對應資料集：

### 有獨立資料集 (17個)

#### 🏙️ 直轄市

**高雄市** (`Kaohsiung_City_housenumber`)
- 資料筆數: 1,240,906 筆
- 欄位數: 11 欄
- X座標: `橫坐標` | Y座標: `縱坐標` | 街道: `街.路段`
- 特色: 資料量最大，覆蓋完整

**新北市** (`NewTaipei_housenumber`)
- 資料筆數: 1,961,175 筆
- 欄位數: 11 欄
- X座標: `橫坐標` | Y座標: `縱坐標` | 街道: `街.路段`
- 特色: 全台資料量最大

**臺北市** (`Taipei_housenumber`)
- 資料筆數: 1,147,845 筆
- 欄位數: 11 欄
- X座標: `橫坐標` | Y座標: `縱坐標` | 街道: `街.路段`
- 特色: 首都資料，品質優良

**臺中市** (`Taichung_City_housenumber`)
- 資料筆數: 1,292,650 筆
- 欄位數: 13 欄
- X座標: `橫坐標` | Y座標: `縱坐標` | 街道: `街.路段`
- 特色: 欄位數最多，資料最豐富

**臺南市** (`Tainan_City_housenumber`)
- 資料筆數: 833,424 筆
- 欄位數: 11 欄
- X座標: `橫坐標` | Y座標: `縱坐標` | 街道: `街.路段`
- 特色: 古都資料，歷史街道多

**桃園市** (`Taoyuan_City_housenumber`)
- 資料筆數: 1,068,369 筆
- 欄位數: 11 欄
- X座標: `橫坐標` | Y座標: `縱坐標` | 街道: `街.路段`
- 特色: 機場城市，發展迅速

#### 🏘️ 一般縣市

**嘉義縣** (`Chiayi_County_housenumber`)
- 資料筆數: 196,971 筆
- 欄位數: 11 欄
- X座標: `橫坐標` | Y座標: `縱坐標` | 街道: `街.路段`
- 特色: 系統優化基準縣市，匹配率96.54%

**新竹市** (`Hsinchu_City_housenumber`) ⚠️
- 資料筆數: 210,396 筆
- 欄位數: 7 欄
- X座標: `橫坐標` | Y座標: `縱坐標` | 街道: `地址` (特殊)
- 特色: 科技城，使用地址欄位而非街.路段

**新竹縣** (`Hsinchu_County_housenumber`)
- 資料筆數: 258,967 筆
- 欄位數: 11 欄
- X座標: `橫坐標` | Y座標: `縱坐標` | 街道: `街.路段`
- 特色: 科技產業聚集區

**苗栗縣** (`Miaoli_County_housenumber`)
- 資料筆數: 224,282 筆
- 欄位數: 11 欄
- X座標: `橫坐標` | Y座標: `縱坐標` | 街道: `街.路段`
- 特色: 山區較多，地形複雜

**彰化縣** (`Changhua_County_housenumber`)
- 資料筆數: 467,023 筆
- 欄位數: 11 欄
- X座標: `橫坐標` | Y座標: `縱坐標` | 街道: `街.路段`
- 特色: 中部重要縣市，資料量大

**雲林縣** (`Yunlin_County_housenumber`)
- 資料筆數: 196,605 筆
- 欄位數: 11 欄
- X座標: `橫坐標` | Y座標: `縱坐標` | 街道: `街.路段`
- 特色: 農業縣，鄉村地區多

**屏東縣** (`Pingtung_County_housenumber`)
- 資料筆數: 327,457 筆
- 欄位數: 11 欄
- X座標: `橫坐標` | Y座標: `縱坐標` | 街道: `街.路段`
- 特色: 南台灣重要縣市

**臺東縣** (`Taitung_County_housenumber`)
- 資料筆數: 50,192 筆
- 欄位數: 11 欄
- X座標: `橫坐標` | Y座標: `縱坐標` | 街道: `街.路段`
- 特色: 東部縣市，資料量較小
- 完整欄位: 省市縣市代碼, 鄉鎮市區代碼, 村里, 鄰, 街.路段, 地區, 巷, 弄, 號樓, 橫坐標, 縱坐標

**澎湖縣** (`Penghu_County_housenumber`) ⚠️
- 資料筆數: 39,683 筆
- 欄位數: 11 欄
- X座標: `橫坐標` | Y座標: `縱坐標` | 街道: `街.路段.` (特殊)
- 特色: 離島縣市，街道欄位多一個點
- 完整欄位: 省市縣市代碼, 鄉鎮市區代碼, 村里, 鄰, 街.路段., 地區, 巷, 弄, 號樓, 橫坐標, 縱坐標

**基隆市** (`Keelung_City_housenumber`)
- 資料筆數: 192,093 筆
- 欄位數: 10 欄
- X座標: `橫坐標` | Y座標: `縱坐標` | 街道: `街.路段`
- 特色: 港都城市，欄位數較少

**金門縣** (`Kinmen_County_housenumber`)
- 資料筆數: 32,005 筆
- 欄位數: 11 欄
- X座標: `橫坐標` | Y座標: `縱坐標` | 街道: `街.路段`
- 特色: 離島縣市，資料量最小

### 無獨立資料集 (5個)
- **嘉義市**: 併入嘉義縣處理或需另外取得資料
- **南投縣**: 山區縣市，需另外取得資料
- **宜蘭縣**: 東北部縣市，需另外取得資料  
- **花蓮縣**: 東部縣市，需另外取得資料
- **連江縣**: 馬祖地區，需另外取得資料

### 📊 資料集統計摘要

| 分類 | 數量 | 總資料筆數 | 平均筆數 |
|-----|------|-----------|----------|
| 直轄市 | 6個 | 7,343,769筆 | 1,223,962筆 |
| 一般縣市 | 11個 | 2,424,677筆 | 220,425筆 |
| 特殊格式 | 2個 | 249,079筆 | 124,540筆 |
| 總計 | 17個 | 9,768,446筆 | 574,614筆 |

## 🚀 新增縣市資料集步驟

### 1. 準備資料
```r
# 載入新的縣市資料
新縣市_housenumber <- read.csv("新縣市門牌資料.csv", fileEncoding = "UTF-8")

# 檢查和調整欄位名稱
colnames(新縣市_housenumber)
names(新縣市_housenumber)[names(新縣市_housenumber) == "X"] <- "橫坐標"
names(新縣市_housenumber)[names(新縣市_housenumber) == "Y"] <- "縱坐標"
```

### 2. 更新對照表
在 `get_city_dataset_mapping()` 函數中添加：
```r
"新縣市名" = "新縣市_housenumber"
```

### 3. 測試驗證
```r
# 測試架構偵測
test_schema <- detect_housenumber_schema("新縣市_housenumber")
print(test_schema)

# 系統準備狀態檢查
readiness <- check_multi_city_readiness()
```

## 📊 品質標準

### 優秀資料集標準 (匹配率 ≥ 95%)
- 座標資訊完整且準確
- 街道命名規範統一
- 資料覆蓋範圍廣泛
- 很少有NA值

### 需改善資料集指標 (匹配率 < 80%)
- 大量座標缺失
- 街道名稱不規範
- 資料品質不佳

## 🔍 快速檢查程式碼

### 一鍵檢查縣市資料集狀態
```r
# 快速檢查所有載入的門牌資料集
check_all_housenumber_datasets <- function() {
  housenumber_datasets <- ls(pattern = "housenumber", envir = .GlobalEnv)
  
  cat("=== 所有門牌資料集檢查 ===\n")
  cat("找到", length(housenumber_datasets), "個資料集\n\n")
  
  for(dataset in housenumber_datasets) {
    obj <- get(dataset, envir = .GlobalEnv)
    if(is.data.frame(obj)) {
      cat("📁", dataset, "\n")
      cat("  筆數:", format(nrow(obj), big.mark = ","), "筆\n")
      cat("  欄數:", ncol(obj), "欄\n")
      cat("  欄位:", paste(colnames(obj)[1:min(5, ncol(obj))], collapse = ", "))
      if(ncol(obj) > 5) cat("...")
      cat("\n")
      
      # 檢查座標欄位
      has_x <- any(c("橫坐標", "TWD97橫坐標", "x_3826") %in% colnames(obj))
      has_y <- any(c("縱坐標", "TWD97縱坐標", "y_3826") %in% colnames(obj))
      has_street <- any(c("街.路段", "街.路段.", "地址") %in% colnames(obj))
      
      if(has_x && has_y && has_street) {
        cat("  狀態: ✅ 架構完整\n")
      } else {
        cat("  狀態: ❌ 架構異常 -")
        if(!has_x) cat(" 缺X座標")
        if(!has_y) cat(" 缺Y座標") 
        if(!has_street) cat(" 缺街道")
        cat("\n")
      }
      cat("\n")
    }
  }
}

# 執行檢查
check_all_housenumber_datasets()
```

### 檢查特定資料集詳細資訊
```r
# 詳細檢查特定資料集
check_dataset_details <- function(dataset_name) {
  if(!exists(dataset_name, envir = .GlobalEnv)) {
    cat("❌ 資料集不存在:", dataset_name, "\n")
    return(NULL)
  }
  
  data <- get(dataset_name, envir = .GlobalEnv)
  
  cat("=== 資料集詳細檢查:", dataset_name, "===\n")
  cat("資料筆數:", format(nrow(data), big.mark = ","), "\n")
  cat("欄位數量:", ncol(data), "\n")
  cat("完整欄位:", paste(colnames(data), collapse = ", "), "\n\n")
  
  # 檢查座標
  coord_cols <- colnames(data)[grepl("座標|坐標", colnames(data))]
  if(length(coord_cols) > 0) {
    cat("座標欄位:", paste(coord_cols, collapse = ", "), "\n")
    for(col in coord_cols) {
      if(is.numeric(data[[col]])) {
        cat("  ", col, "範圍:", round(min(data[[col]], na.rm = TRUE)), "~", 
            round(max(data[[col]], na.rm = TRUE)), "\n")
        cat("  ", col, "NA數量:", sum(is.na(data[[col]])), "\n")
      }
    }
  }
  
  # 檢查街道
  street_cols <- colnames(data)[grepl("街|路|地址", colnames(data))]
  if(length(street_cols) > 0) {
    cat("\n街道相關欄位:", paste(street_cols, collapse = ", "), "\n")
    for(col in street_cols) {
      na_count <- sum(is.na(data[[col]]) | data[[col]] == "")
      cat("  ", col, "空值數量:", na_count, "(", round(na_count/nrow(data)*100, 1), "%)\n")
    }
  }
  
  # 執行架構偵測
  cat("\n架構偵測結果:\n")
  schema <- detect_housenumber_schema(dataset_name)
  print(schema)
}

# 使用範例
# check_dataset_details("Taitung_County_housenumber")
# check_dataset_details("Penghu_County_housenumber")
```

## 💡 最佳實務建議

1. **欄位命名一致性**: 儘量使用標準命名規範
2. **資料清理**: 載入前先進行基本的資料清理
3. **編碼統一**: 確保使用UTF-8編碼
4. **測試驗證**: 每次修改後都要測試架構偵測
5. **文檔記錄**: 記錄特殊處理方式和已知問題

## 📞 技術支援

如遇到本指引未涵蓋的問題，請：
1. 執行完整的架構檢查
2. 記錄錯誤訊息和資料樣本
3. 聯繫技術團隊協助處理

---

**版本**: 1.0  
**更新日期**: 2025-07-09  
**適用系統**: 多縣市智能匹配系統 v2.0