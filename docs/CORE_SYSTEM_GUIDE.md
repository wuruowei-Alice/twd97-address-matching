# 核心匹配系統使用指南

> 🔧 **推薦系統** - 穩定可靠，96.08%匹配率，適合所有用戶

## 📋 目錄

- [系統概述](#系統概述)
- [快速開始](#快速開始)
- [詳細使用](#詳細使用)
- [輸出說明](#輸出說明)
- [QGIS整合](#qgis整合)
- [故障排除](#故障排除)
- [最佳實務](#最佳實務)

## 系統概述

核心匹配系統是經過實戰驗證的穩定版本，具有以下特色：

### 🏆 核心特色
- **96.08% 匹配率** - 28,705筆診所實測結果
- **即開即用** - 無需複雜參數調整
- **完善錯誤處理** - 零系統崩潰
- **記憶體優化** - 8-12GB即可運行
- **完整輸出** - 包含所有診所的匹配狀態

### 🎯 技術優勢
- **雙層匹配策略** - 道路匹配(53%) + 區域匹配(47%)
- **完整地址標識合併** - 突破性技術，大幅提升匹配率
- **批次處理** - 記憶體穩定，適合大量資料
- **TWD97座標** - 台灣官方座標系統，QGIS直接相容

## 快速開始

### 前置準備

1. **環境檢查**
```r
# 檢查R版本（需要4.0以上）
R.version.string

# 安裝必要套件
install.packages(c("dplyr", "stringr"))
library(dplyr)
library(stringr)
```

2. **資料準備**
確保您的診所資料包含以下欄位：
- `醫事機構名稱` - 診所名稱
- `地址` - 完整地址
- `完整地址標識` - 唯一地址識別碼

3. **門牌資料集**
確保已載入需要的縣市門牌資料，例如：
```r
# 檢查可用的門牌資料集
available_datasets <- ls(pattern = "housenumber")
print(available_datasets)
```

### 一鍵執行

```r
# 載入核心匹配系統
source("R/core_matching_system.R")

# 🥇 一鍵最佳執行（強烈推薦）
final_results <- run_best_matching("clinic")

# 查看結果摘要
print(final_results$stats)
```

**就這麼簡單！** 🎉

## 詳細使用

### 執行選項

系統提供多種執行選項，適應不同需求：

#### 1. 最佳匹配（推薦）
```r
# 🥇 最佳匹配：96.08%匹配率 + 完整診所資料
final_results <- run_best_matching("clinic")
```
- ✅ 自動執行基礎匹配 + 地址標識合併
- ✅ 輸出完整診所資料（含未匹配診所）
- ✅ 提供未匹配分析

#### 2. 高精度匹配
```r
# 🏆 高精度匹配：使用完整地址標識技術
high_results <- run_high_precision_matching("clinic")
```
- ✅ 分步驟顯示處理過程
- ✅ 詳細的匹配品質報告
- ✅ 適合需要了解處理細節的用戶

#### 3. 標準匹配
```r
# 🎯 標準匹配：基礎功能
standard_results <- run_standard_matching("clinic")
```
- ✅ 僅基礎匹配功能
- ✅ 輸出匹配成功的診所
- ✅ 適合簡單需求

#### 4. 快速匹配
```r
# ⚡ 快速匹配：大批次處理
fast_results <- run_fast_matching("clinic")
```
- ✅ 較大的批次大小（500筆/批）
- ✅ 處理速度更快
- ✅ 適合大量資料快速處理

### 進階自訂

如果需要自訂參數：

```r
# 自訂基礎匹配
base_results <- production_ready_matching(
    clinic_data_name = "clinic",
    batch_size = 300,                    # 批次大小
    output_base_name = "自訂匹配",        # 輸出檔名
    debug_mode = FALSE                   # 除錯模式
)

# 自訂地址標識合併
final_results <- export_with_address_id(
    original_clinic_data = "clinic",
    matching_results = base_results,
    output_filename = "完整結果.csv"
)
```

## 輸出說明

### 主要輸出檔案

執行後會生成以下檔案：

#### 1. 完整診所資料（主要使用）
```
完整診所資料_地址標識合併_YYYYMMDD_HHMM.csv
```

**欄位說明**：
- `醫事機構名稱` - 診所名稱
- `地址` - 原始地址
- `完整地址標識` - 唯一地址識別碼
- `匹配狀態` - "成功" 或 "未匹配"
- `匹配方式` - "道路匹配" 或 "區域匹配"
- `TWD97_X` - TWD97橫座標
- `TWD97_Y` - TWD97縱座標
- `座標系統` - "TWD97"
- `匹配時間` - 處理時間戳記

#### 2. 基礎匹配結果（技術分析用）
```
基礎匹配_YYYYMMDD_HHMM.csv
```
- 僅包含匹配成功的診所
- 用於技術分析和驗證

#### 3. 未匹配清單（改善參考）
```
基礎匹配_未匹配_YYYYMMDD_HHMM.csv
```
- 包含無法匹配的診所
- 用於後續手動處理或系統改善

### 統計報告

執行完成後會顯示詳細統計：

```
📊 最終統計:
總診所數: 28,705 筆
匹配成功: 27,579 筆
未匹配: 1,126 筆
有座標: 27,579 筆
匹配率: 96.08 %
座標率: 96.08 %

📊 匹配方式統計:
  匹配方式     n
1 道路匹配 14658
2 區域匹配 12921

📍 座標範圍檢查:
X座標範圍: 162107.1 ~ 343324.4 
Y座標範圍: 2434062 ~ 2791202 
✅ 座標範圍符合TWD97格式
```

## QGIS整合

### 基本匯入

1. **開啟QGIS**
2. **載入資料**
   - 圖層 → 新增圖層 → 新增分隔文字圖層
   - 選擇：`完整診所資料_地址標識合併_YYYYMMDD_HHMM.csv`

3. **座標設定**
   - X欄位：`TWD97_X`
   - Y欄位：`TWD97_Y`
   - 幾何CRS：`EPSG:3826` (TWD97 / TM2 zone 121)

4. **完成匯入**
   - 🗺️ 診所點位完美顯示在台灣地圖上！

### 進階視覺化

#### 篩選匹配成功的診所
```sql
"匹配狀態" = '成功'
```

#### 依匹配方式分色
在圖層屬性 → 符號系統中設定：
```sql
CASE 
    WHEN "匹配方式" = '道路匹配' THEN 'blue'   -- 高精度，藍色
    WHEN "匹配方式" = '區域匹配' THEN 'orange' -- 中精度，橙色
    ELSE 'gray'                               -- 未匹配，灰色
END
```

#### 依縣市分組
```sql
-- 篩選特定縣市
"地址" LIKE '%台北市%'

-- 或使用完整地址標識的前5碼（縣市代碼）
left("完整地址標識", 5) = '63000'  -- 台北市
```

### 圖例設定建議

| 符號 | 顏色 | 說明 | 精度等級 |
|------|------|------|----------|
| ● | 藍色 | 道路匹配 | 高精度 |
| ● | 橙色 | 區域匹配 | 中精度 |
| ● | 灰色 | 未匹配 | 無座標 |

## 故障排除

### 常見問題

#### 1. 記憶體不足
**錯誤訊息**：
```
Error: cannot allocate vector of size XXX Gb
```

**解決方案**：
```r
# 方案A：減少批次大小
results <- production_ready_matching(
    clinic_data_name = "clinic",
    batch_size = 200  # 從預設300降至200
)

# 方案B：釋放記憶體
rm(list = ls()[grepl("housenumber", ls())])  # 清理門牌資料
gc()  # 強制記憶體回收

# 方案C：分步處理
# 先處理一半資料
clinic_part1 <- clinic[1:(nrow(clinic)/2), ]
results_part1 <- run_best_matching("clinic_part1")
```

#### 2. 找不到診所資料
**錯誤訊息**：
```
❌ 找不到醫療資料集: clinic
```

**解決方案**：
```r
# 檢查資料名稱
ls()  # 查看所有物件

# 如果資料名稱不同
results <- run_best_matching("your_clinic_data_name")

# 如果需要載入資料
clinic <- read.csv("your_clinic_data.csv", fileEncoding = "UTF-8")
```

#### 3. 找不到門牌資料集
**錯誤訊息**：
```
❌ 未找到任何門牌資料集
```

**解決方案**：
```r
# 檢查門牌資料集
available_datasets <- ls(pattern = "housenumber")
print(available_datasets)

# 如果沒有，需要先載入門牌資料
# 例如：
load("Taipei_housenumber.RData")
load("NewTaipei_housenumber.RData")
```

#### 4. 匹配率異常低
**症狀**：匹配率 < 80%

**診斷步驟**：
```r
# 1. 檢查診所資料格式
head(clinic)
colnames(clinic)

# 2. 檢查地址欄位
summary(nchar(clinic$地址))  # 地址長度統計
sum(is.na(clinic$地址))      # 空白地址數量

# 3. 檢查完整地址標識
length(unique(clinic$完整地址標識))  # 唯一地址數量

# 4. 開啟除錯模式
debug_results <- production_ready_matching(
    clinic_data_name = "clinic",
    debug_mode = TRUE
)
```

#### 5. 座標範圍異常
**症狀**：座標不在台灣範圍內

**檢查方法**：
```r
# 檢查座標範圍
coords_summary <- results$complete_data %>%
    filter(!is.na(TWD97_X), !is.na(TWD97_Y)) %>%
    summarise(
        X_min = min(TWD97_X), X_max = max(TWD97_X),
        Y_min = min(TWD97_Y), Y_max = max(TWD97_Y)
    )

# 正常的TWD97範圍
# X: 100,000 ~ 400,000
# Y: 2,000,000 ~ 3,000,000

print(coords_summary)
```

### 除錯技巧

#### 開啟詳細除錯
```r
# 開啟除錯模式獲得更多資訊
debug_results <- production_ready_matching(
    clinic_data_name = "clinic",
    debug_mode = TRUE
)
```

#### 分析未匹配資料
```r
# 使用內建分析工具
unmatched_analysis <- enhanced_unmatched_analysis(results)

# 查看未匹配統計
print(unmatched_analysis$summary)

# 查看未匹配樣本
head(unmatched_analysis$unmatched_data)
```

#### 批次大小調整指南

| 記憶體 | 建議批次大小 | 預期處理時間 |
|--------|-------------|-------------|
| 8GB | 150-200 | 較慢但穩定 |
| 16GB | 300 (預設) | 平衡效能 |
| 32GB+ | 500+ | 快速處理 |

## 最佳實務

### 資料準備建議

1. **診所資料品質檢查**
```r
# 檢查必要欄位
required_cols <- c("醫事機構名稱", "地址", "完整地址標識")
missing_cols <- setdiff(required_cols, colnames(clinic))
if(length(missing_cols) > 0) {
    cat("缺少欄位:", paste(missing_cols, collapse = ", "), "\n")
}

# 檢查資料完整性
cat("空白醫事機構名稱:", sum(is.na(clinic$醫事機構名稱)), "筆\n")
cat("空白地址:", sum(is.na(clinic$地址)), "筆\n")
cat("空白地址標識:", sum(is.na(clinic$完整地址標識)), "筆\n")
```

2. **門牌資料集確認**
```r
# 確認支援的縣市
supported_cities <- c(
    "臺北市", "新北市", "桃園市", "臺中市", "臺南市", "高雄市",
    "基隆市", "新竹市", "新竹縣", "苗栗縣", "彰化縣", "雲林縣",
    "嘉義縣", "屏東縣", "臺東縣", "澎湖縣", "金門縣"
)

# 檢查診所分布
clinic_cities <- clinic %>%
    mutate(城市 = str_extract(地址, paste(supported_cities, collapse = "|"))) %>%
    count(城市, sort = TRUE)

print(clinic_cities)
```

### 效能優化建議

1. **記憶體優化**
```r
# 處理前清理記憶體
gc()

# 分批處理大量資料
if(nrow(clinic) > 50000) {
    cat("建議分批處理大量資料\n")
    # 可考慮分成多個批次執行
}
```

2. **硬體配置建議**
- **8-12GB RAM**：標準配置，適合一般使用
- **16GB+ RAM**：推薦配置，效能更佳
- **SSD硬碟**：提升檔案讀寫速度
- **多核心CPU**：