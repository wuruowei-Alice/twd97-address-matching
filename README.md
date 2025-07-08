# TWD97診所匹配系統

[![R Version](https://img.shields.io/badge/R-%E2%89%A54.0-blue.svg)](https://cran.r-project.org/)
[![Match Rate](https://img.shields.io/badge/Match%20Rate-96.08%25-brightgreen.svg)]()
[![License](https://img.shields.io/badge/License-MIT-yellow.svg)]()
[![Coordinate System](https://img.shields.io/badge/Coordinate-TWD97-orange.svg)]()

> 🏆 **業界頂尖水準**的台灣診所地址匹配系統，實現 **96.08%** 匹配率，支援TWD97座標輸出，可直接用於QGIS地圖分析。

## 🎯 系統選擇指南

本專案提供兩套匹配系統，請根據您的需求選擇：

| 系統 | 匹配率 | 適用場景 | 記憶體需求 | 推薦對象 |
|------|--------|----------|------------|----------|
| **🔧 核心匹配系統** | **96.08%** | 一般使用、穩定可靠 | 8-12GB | ✅ **推薦新手** |
| **⚙️ 採樣控制系統** | 可調控 | 研究實驗、效能調優 | 可調控 | 🔬 進階用戶 |

## ⚡ 快速開始

### 選項A：核心匹配系統（推薦）

```r
# 載入核心系統
source("R/core_matching_system.R")

# 一鍵最佳執行
results <- run_best_matching("clinic")

# 查看結果
cat("匹配率:", results$stats$match_rate, "%\n")
```

### 選項B：採樣控制系統（進階）

```r
# 載入採樣控制系統
source("R/enhanced_sampling_system.R")

# 使用最佳採樣數量（實證發現：20,000筆最佳）
results <- modify_sampling_size(
    clinic_data_name = "clinic",
    max_housenumber_per_city = 20000
)
```

## 🏆 核心成果

**實測結果**（28,705筆診所）：
- ✅ **匹配率**：96.08%（27,579筆成功）
- ✅ **處理速度**：~300筆/分鐘
- ✅ **座標精度**：100% TWD97標準
- ✅ **QGIS相容**：直接匯入使用

**雙層匹配策略**：
- 🎯 **道路匹配**：53.0%（高精度）
- 🎯 **區域匹配**：47.0%（中精度）

## 🚀 系統功能

### 🔧 核心匹配系統
- ✨ **即開即用**：一鍵執行，無需調參
- 🛡️ **穩定可靠**：完善錯誤處理，零崩潰
- 📊 **完整輸出**：包含所有診所的匹配狀態
- 🗺️ **QGIS整合**：TWD97座標，直接可用

### ⚙️ 採樣控制系統
- 🎛️ **彈性調控**：可調整門牌採樣數量
- 🔬 **實驗友善**：支援不同採樣策略測試
- 📈 **效能優化**：記憶體使用可控
- 💡 **研究價值**：包含採樣悖論發現

## 📊 採樣悖論發現

我們意外發現了地理編碼的重要規律：

| 採樣數量 | 匹配率 | 成功匹配數 | 結論 |
|---------|--------|------------|------|
| **20,000筆** | **96.08%** | **27,579筆** | 🏆 **最佳平衡點** |
| 100,000筆 | 77.85% | 22,346筆 | ❌ 過度採樣反效果 |

**核心發現**：適量的高品質採樣 > 大量的混合品質採樣

## 📍 QGIS使用

### 快速匯入
1. **載入CSV**：選擇輸出的匹配結果檔案
2. **座標設定**：X欄位=`TWD97_X`，Y欄位=`TWD97_Y`
3. **CRS設定**：`EPSG:3826` (TWD97 TM2)
4. **完成**：🗺️ 完美顯示在台灣地圖上！

### 進階視覺化
```sql
-- 篩選匹配成功的診所
"匹配狀態" = '成功'

-- 依匹配方式分色
CASE 
    WHEN "匹配方式" = '道路匹配' THEN 'blue'   -- 高精度
    WHEN "匹配方式" = '區域匹配' THEN 'orange' -- 中精度
END
```

## 📚 完整文件

| 文件 | 說明 | 適用對象 |
|------|------|----------|
| 📖 [核心匹配系統使用指南](docs/CORE_SYSTEM_GUIDE.md) | 詳細使用說明、故障排除 | 所有用戶 |
| ⚙️ [採樣控制系統使用指南](docs/SAMPLING_SYSTEM_GUIDE.md) | 採樣參數調整、效能優化 | 進階用戶 |
| 🗺️ [QGIS整合指南](docs/QGIS_INTEGRATION.md) | 地圖製作、視覺化技巧 | GIS用戶 |
| 📋 [API參考文件](docs/API_REFERENCE.md) | 函數詳細說明 | 開發者 |
| 🔬 [技術細節](docs/TECHNICAL_DETAILS.md) | 演算法、架構設計 | 研究者 |

## 🛠️ 環境需求

### 基本需求
- **R版本**：4.0 或以上
- **必要套件**：`dplyr`, `stringr`
- **記憶體**：8GB 以上
- **資料格式**：診所資料需包含 `醫事機構名稱`、`地址`、`完整地址標識`

### 推薦配置
- **記憶體**：16GB（高採樣模式）
- **處理器**：多核心CPU（批次處理更快）
- **硬碟**：5GB可用空間（輸出檔案）

## 🔍 故障排除

### 常見問題
| 問題 | 解決方案 | 文件連結 |
|------|----------|----------|
| 記憶體不足 | 減少批次大小或採樣數量 | [核心系統指南](docs/CORE_SYSTEM_GUIDE.md#記憶體優化) |
| 匹配率過低 | 檢查門牌資料集 | [採樣系統指南](docs/SAMPLING_SYSTEM_GUIDE.md#匹配率優化) |
| 座標異常 | 驗證TWD97範圍 | [技術細節](docs/TECHNICAL_DETAILS.md#座標驗證) |

### 技術支援
- 🐛 **Bug回報**：[GitHub Issues](https://github.com/wuruowei-Alice/twd97-address-matching/issues)
- 💬 **使用討論**：[GitHub Discussions](https://github.com/wuruowei-Alice/twd97-address-matching/discussions)
- 📧 **直接聯絡**：wuruowei582@gmail.com

## 🎯 使用範例

### 基本使用（5分鐘上手）
```r
# 1. 載入系統
source("R/core_matching_system.R")

# 2. 執行匹配
results <- run_best_matching("clinic")

# 3. 檢查結果
print(results$stats)
# 匹配率: 96.08%
# 成功匹配: 27,579筆
```

### 進階使用（效能調優）
```r
# 1. 載入採樣控制系統
source("R/enhanced_sampling_system.R")

# 2. 測試不同採樣數量
test_results <- c()
for(size in c(15000, 20000, 25000)) {
    result <- modify_sampling_size(max_housenumber_per_city = size)
    test_results[as.character(size)] <- result$stats$rate
}

# 3. 找到最佳採樣點
best_size <- names(which.max(test_results))
print(paste("最佳採樣數量:", best_size))
```

## 🌟 應用場景

### 醫療研究
- 🏥 **診所分布分析** - 了解醫療資源分布
- 📊 **可及性研究** - 計算就醫便利性
- 🎯 **政策制定** - 醫療政策空間影響

### 商業應用  
- 💼 **市場分析** - 醫療市場競爭分析
- 🏢 **選址決策** - 最佳診所位置選擇
- 📱 **APP開發** - 就醫導航系統

### 學術研究
- 🔬 **地理編碼** - 採樣策略優化研究
- 📈 **空間分析** - 健康地理學研究
- 💡 **方法論** - 地址匹配演算法改進

## 🤝 貢獻指南

我們歡迎各種形式的貢獻！

### 快速貢獻
1. 🍴 **Fork** 此專案
2. 🌿 **創建分支** (`git checkout -b feature/your-feature`)
3. 💾 **提交變更** (`git commit -m 'Add your feature'`)
4. 📤 **推送分支** (`git push origin feature/your-feature`)
5. 🔀 **開啟 Pull Request**

### 貢獻類型
- 🐛 **Bug修復** - 回報或修復問題
- ✨ **功能增強** - 新功能建議與實作
- 📚 **文件改善** - 使用說明完善
- 🧪 **測試案例** - 增加測試覆蓋
- 🌍 **國際化** - 多語言支援

## 📄 授權

本專案採用 [MIT License](LICENSE) 開源授權

## 👨‍💻 作者

**wuruowei-Alice**
- 🐙 GitHub: [@wuruowei-Alice](https://github.com/wuruowei-Alice)
- 📧 Email: wuruowei582@gmail.com
- 💼 專長: GIS地址匹配、空間資料分析、R語言開發

## 🙏 致謝

- 🗺️ **台灣內政部** - TWD97座標系統標準
- 📊 **R Core Team** - 優秀的R語言環境  
- 🏥 **衛生福利部** - 醫事機構開放資料
- 🏠 **各縣市政府** - 門牌資料集提供

---

<div align="center">

### ⭐ 如果這個專案對您有幫助，請給我們一個Star！

**[📖 開始使用](docs/CORE_SYSTEM_GUIDE.md)** | **[⚙️ 進階功能](docs/SAMPLING_SYSTEM_GUIDE.md)** | **[🗺️ QGIS整合](docs/QGIS_INTEGRATION.md)**

*最後更新：2025年7月8日*

</div>
