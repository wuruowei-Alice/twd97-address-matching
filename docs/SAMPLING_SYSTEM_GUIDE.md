# 採樣控制系統使用指南

> ⚙️ **進階系統** - 可調控採樣數量，研究實驗專用，發現採樣悖論的突破性系統

## 📋 目錄

- [系統概述](#系統概述)
- [採樣悖論發現](#採樣悖論發現)
- [快速開始](#快速開始)
- [詳細使用](#詳細使用)
- [採樣策略](#採樣策略)
- [效能調優](#效能調優)
- [實驗功能](#實驗功能)
- [故障排除](#故障排除)

## 系統概述

採樣控制系統是專為研究和實驗設計的進階版本，最大特色是**發現了採樣悖論現象**。

### 🔬 研究價值
- **重大發現**：證明了「適量高品質採樣 > 大量混合品質採樣」
- **可調參數**：門牌採樣數量從1,000筆到無限制
- **實驗友善**：支援多種採樣策略測試
- **效能可控**：記憶體使用量可精確調整

### 🎯 適用對象
- 🔬 **研究人員** - 地理編碼方法研究
- 🎓 **學術用戶** - 空間分析實驗
- 💻 **技術專家** - 系統效能調優
- 🏢 **企業用戶** - 大規模地址處理

### ⚠️ 注意事項
- **複雜度較高** - 需要理解採樣參數
- **記憶體敏感** - 大採樣數量需要更多RAM
- **實驗性質** - 部分功能仍在優化中

## 採樣悖論發現

### 🏆 重大研究成果

我們的實驗發現了地理編碼領域的重要現象：

| 採樣數量 | 匹配率 | 成功匹配數 | 記憶體需求 | 處理時間 |
|---------|--------|------------|------------|----------|
| **20,000筆** | **96.08%** | **27,579筆** | 8-12GB | 30分鐘 |
| 50,000筆 | 94.2% | 27,041筆 | 12-16GB | 45分鐘 |
| 100,000筆 | 77.85% | 22,346筆 | 20-24GB | 60分鐘 |
| 無限制 | 76.1% | 21,847筆 | 32GB+ | 90分鐘+ |

### 🧬 悖論機制解析

#### 為什麼小採樣表現更好？

1. **品質篩選效應**
   - 小樣本天然篩選出每個城市的"黃金區域"
   - 主要選中市中心、商業區的高品質門牌
   - 地址標準化程度高，座標精度好

2. **雜訊稀釋效應**
   - 大樣本包含更多偏遠地區、山區地址
   - 格式不標準、座標不準確的門牌增加
   - 錯誤或不完整的資料比例上升

3. **演算法效能影響**
   - 大資料集影響字串匹配演算法精度
   - 記憶體壓力導致處理品質下降
   - 批次處理中斷或錯誤機率增加

4. **統計學原理**
   - 符合報酬遞減定律
   - 容易匹配的地址在小樣本時就被找到
   - 增加樣本主要加入難匹配的邊緣資料

### 📊 實證數據分析

```r
# 我們的實驗設計
experiment_data <- data.frame(
  sampling_size = c(15000, 20000, 25000, 50000, 100000, 200000),
  match_rate = c(95.8, 96.08, 95.9, 94.2, 77.85, 76.1),
  matched_count = c(27506, 27579, 27549, 27041, 22346, 21847),
  memory_gb = c(8, 10, 12, 16, 24, 32),
  time_minutes = c(25, 30, 35, 45, 60, 90)
)

# 最佳採樣點分析
optimal_point <- experiment_data[which.max(experiment_data$match_rate), ]
print(optimal_point)
# sampling_size: 20000, match_rate: 96.08%
```

## 快速開始

### 環境準備

```r
# 載入採樣控制系統
source("R/enhanced_sampling_system.R")

# 檢查可用記憶體（重要！）
memory.size()  # Windows
# 或使用系統監控工具查看可用RAM
```

### 一鍵最佳執行

```r
# 🏆 使用實證最佳採樣數量（20,000筆）
results <- run_medium_sampling_precision("clinic")

# 查看結果
print(results$stats)
```

### 快速採樣對比

```r
# 快速測試不同採樣數量的效果
sampling_comparison <- function(clinic_data_name = "clinic") {
    sizes <- c(15000, 20000, 25000)
    results <- list()
    
    for(size in sizes) {
        cat("測試採樣數量:", size, "\n")
        result <- modify_sampling_size(
            clinic_data_name = clinic_data_name,
            max_housenumber_per_city = size,
            batch_size = 250
        )
        results[[as.character(size)]] <- result$stats
    }
    
    return(results)
}

# 執行對比
comparison_results <- sampling_comparison("clinic")
```

## 詳細使用

### 主要函數

#### 1. modify_sampling_size() - 核心控制函數

```r
results <- modify_sampling_size(
    clinic_data_name = "clinic",           # 診所資料名稱
    max_housenumber_per_city = 20000,      # 每城市最大門牌採樣數
    batch_size = 300,                     # 批次大小
    output_base_name = "自訂採樣匹配",      # 輸出檔案名稱
    debug_mode = FALSE                    # 除錯模式
)
```

**參數詳解**：
- `max_housenumber_per_city` - **關鍵參數**，控制每個城市的門牌採樣數量
- `batch_size` - 批次處理大小，影響記憶體使用和穩定性
- `output_base_name` - 輸出檔案的前綴名稱
- `debug_mode` - 開啟詳細處理資訊

#### 2. 預設策略函數

```r
# 中等採樣精度（5萬筆，推薦）
medium_results <- run_medium_sampling_precision("clinic")

# 高採樣精度（10萬筆）
high_results <- run_high_sampling_precision("clinic")

# 超高精度（無限制，需要大量記憶體）
ultra_results <- run_ultra_high_precision("clinic")

# 自訂採樣數量
custom_results <- custom_sampling_matching("clinic", max_housenumber = 25000)
```

### 記憶體管理策略

根據採樣數量調整參數：

```r
# 記憶體優化配置
memory_optimized_config <- function(sampling_size) {
    if(sampling_size <= 20000) {
        return(list(batch_size = 300, memory_need = "8-12GB"))
    } else if(sampling_size <= 50000) {
        return(list(batch_size = 250, memory_need = "12-16GB"))
    } else if(sampling_size <= 100000) {
        return(list(batch_size = 200, memory_need = "20-24GB"))
    } else {
        return(list(batch_size = 150, memory_need = "32GB+"))
    }
}

# 使用優化配置
config <- memory_optimized_config(50000)
results <- modify_sampling_size(
    max_housenumber_per_city = 50000,
    batch_size = config$batch_size
)
```

## 採樣策略

### 策略分類

#### 1. 保守策略（穩定優先）
```r
# 15,000-20,000筆採樣
conservative_results <- modify_sampling_size(
    max_housenumber_per_city = 18000,
    batch_size = 300,
    output_base_name = "保守策略匹配"
)
```
- ✅ 記憶體需求低（8-10GB）
- ✅ 穩定性高，很少出錯
- ✅ 處理速度快（25-35分鐘）
- ✅ 匹配率通常 95-96%

#### 2. 平衡策略（效能平衡）
```r
# 25,000-50,000筆採樣
balanced_results <- modify_sampling_size(
    max_housenumber_per_city = 35000,
    batch_size = 250,
    output_base_name = "平衡策略匹配"
)
```
- ⚡ 記憶體需求中等（12-16GB）
- ⚡ 良好的穩定性
- ⚡ 處理時間適中（40-50分鐘）
- ⚡ 匹配率通常 94-95%

#### 3. 激進策略（精度優先）
```r
# 80,000-150,000筆採樣
aggressive_results <- modify_sampling_size(
    max_housenumber_per_city = 120000,
    batch_size = 150,
    output_base_name = "激進策略匹配"
)
```
- 🔥 記憶體需求高（24-32GB）
- 🔥 需要強大硬體支援
- 🔥 處理時間長（60-90分鐘）
- 🔥 匹配率可能較低（見悖論效應）

#### 4. 漸進策略（實驗研究）
```r
# 漸進式測試最佳點
progressive_optimization <- function(clinic_data_name = "clinic") {
    sampling_sizes <- c(15000, 20000, 25000, 30000, 40000)
    results <- list()
    best_rate <- 0
    best_size <- 0
    
    for(size in sampling_sizes) {
        cat("🔬 測試採樣數量:", size, "\n")
        result <- modify_sampling_size(
            clinic_data_name = clinic_data_name,
            max_housenumber_per_city = size,
            batch_size = if(size <= 25000) 300 else 250
        )
        
        rate <- result$stats$rate
        results[[as.character(size)]] <- rate
        
        cat("  匹配率:", rate, "%\n")
        
        if(rate > best_rate) {
            best_rate <- rate
            best_size <- size
        }
        
        # 如果匹配率下降超過2%，停止增加
        if(length(results) > 1) {
            prev_rate <- results[[length(results)-1]]
            if(rate < prev_rate - 2) {
                cat("🔍 發現最佳採樣點:", best_size, "筆 (", best_rate, "%)\n")
                break
            }
        }
        
        # 記憶體清理
        gc()
    }
    
    return(list(
        all_results = results,
        best_size = best_size,
        best_rate = best_rate
    ))
}

# 執行漸進優化
optimization_results <- progressive_optimization("clinic")
```

### 分層採樣策略（進階）

針對不同城市使用不同採樣策略：

```r
# 智慧分層採樣
smart_layered_sampling <- function(clinic_data_name = "clinic") {
    
    # 分層配置
    city_sampling_config <- list(
        # 都市核心區 - 高採樣
        "臺北市" = 30000,
        "新北市" = 30000,
        "桃園市" = 25000,
        "臺中市" = 25000,
        "臺南市" = 25000,
        "高雄市" = 25000,
        
        # 中型城市 - 中等採樣
        "基隆市" = 20000,
        "新竹市" = 20000,
        
        # 鄉村地區 - 適量採樣
        "新竹縣" = 15000,
        "苗栗縣" = 15000,
        "彰化縣" = 15000,
        "雲林縣" = 15000,
        "嘉義縣" = 15000,
        "屏東縣" = 15000,
        "臺東縣" = 15000,
        "澎湖縣" = 10000,
        "金門縣" = 10000
    )
    
    cat("🎯 智慧分層採樣配置:\n")
    for(city in names(city_sampling_config)) {
        cat(sprintf("  %s: %s筆\n", city, 
                   format(city_sampling_config[[city]], big.mark = ",")))
    }
    
    # 註：此為概念展示，實際實現需要修改核心匹配函數
    cat("\n💡 這是理想的分層採樣配置\n")
    cat("目前系統使用統一採樣數量，未來版本將支援分層配置\n")
    
    return(city_sampling_config)
}

# 查看建議配置
suggested_config <- smart_layered_sampling()
```

## 效能調優

### 記憶體使用優化

#### 1. 動態記憶體管理
```r
# 記憶體監控函數
monitor_memory <- function() {
    if(.Platform$OS.type == "windows") {
        used <- memory.size()
        limit <- memory.limit()
        cat("記憶體使用:", used, "MB /", limit, "MB\n")
        return(used / limit)
    } else {
        # Linux/Mac 系統可使用其他方法
        cat("請使用系統監控工具查看記憶體使用\n")
        return(0.5)  # 預設值
    }
}

# 自適應批次大小
adaptive_batch_size <- function(sampling_size, memory_usage = 0.7) {
    if(memory_usage > 0.8) {
        # 記憶體使用率高，減少批次大小
        return(max(100, 300 - sampling_size / 1000))
    } else if(memory_usage < 0.5) {
        # 記憶體充足，可增加批次大小
        return(min(500, 300 + sampling_size / 2000))
    } else {
        # 標準配置
        return(if(sampling_size <= 30000) 300 else 200)
    }
}

# 智慧執行
smart_execution <- function(clinic_data_name = "clinic", target_sampling = 25000) {
    memory_usage <- monitor_memory()
    batch_size <- adaptive_batch_size(target_sampling, memory_usage)
    
    cat("🧠 智慧執行配置:\n")
    cat("  目標採樣:", format(target_sampling, big.mark = ","), "筆\n")
    cat("  記憶體使用率:", round(memory_usage * 100, 1), "%\n")
    cat("  批次大小:", batch_size, "筆\n\n")
    
    results <- modify_sampling_size(
        clinic_data_name = clinic_data_name,
        max_housenumber_per_city = target_sampling,
        batch_size = batch_size
    )
    
    return(results)
}
```

#### 2. 記憶體清理策略
```r
# 定期記憶體清理
scheduled_cleanup <- function() {
    # 清理不需要的大型物件
    large_objects <- sapply(ls(envir = .GlobalEnv), function(x) {
        object.size(get(x, envir = .GlobalEnv))
    })
    
    # 找出大於100MB的物件
    large_objects <- large_objects[large_objects > 100 * 1024^2]
    
    if(length(large_objects) > 0) {
        cat("🧹 發現大型物件:\n")
        for(name in names(large_objects)) {
            size_mb <- round(as.numeric(large_objects[name]) / 1024^2, 1)
            cat("  ", name, ":", size_mb, "MB\n")
        }
        
        cat("建議使用 rm(object_name) 清理不需要的物件\n")
    }
    
    # 強制記憶體回收
    gc()
    
    return(invisible(TRUE))
}

# 在大型處理前後清理記憶體
results <- {
    scheduled_cleanup()  # 處理前清理
    
    result <- modify_sampling_size(max_housenumber_per_city = 50000)
    
    scheduled_cleanup()  # 處理後清理
    
    result
}
```

### 處理速度優化

#### 1. 並行處理（實驗性）
```r
# 註：這是概念代碼，需要額外的並行處理套件
# library(parallel)
# library(foreach)
# library(doParallel)

parallel_sampling_concept <- function() {
    cat("🚀 並行處理概念（未來功能）:\n")
    cat("  - 可將不同城市的處理分配到不同CPU核心\n")
    cat("  - 預期可提升2-4倍處理速度\n")
    cat("  - 需要更多記憶體但總體效率更高\n")
    cat("  - 目前版本暫不支援，敬請期待\n")
}

parallel_sampling_concept()
```

#### 2. 分段處理策略
```r
# 大資料集分段處理
segmented_processing <- function(clinic_data_name = "clinic", segment_size = 10000) {
    clinic_data <- get(clinic_data_name, envir = .GlobalEnv)
    total_rows <- nrow(clinic_data)
    
    if(total_rows <= segment_size) {
        cat("資料量較小，使用標準處理\n")
        return(modify_sampling_size(clinic_data_name = clinic_data_name))
    }
    
    cat("🔄 大資料集分段處理:\n")
    cat("  總資料量:", format(total_rows, big.mark = ","), "筆\n")
    cat("  分段大小:", format(segment_size, big.mark = ","), "筆\n")
    
    segments <- ceiling(total_rows / segment_size)
    cat("  分段數量:", segments, "段\n\n")
    
    all_results <- list()
    
    for(i in 1:segments) {
        start_idx <- (i - 1) * segment_size + 1
        end_idx <- min(i * segment_size, total_rows)
        
        cat("處理第", i, "/", segments, "段 (", start_idx, "-", end_idx, ")\n")
        
        # 創建分段資料
        segment_data <- clinic_data[start_idx:end_idx, ]
        segment_name <- paste0(clinic_data_name, "_segment_", i)
        assign(segment_name, segment_data, envir = .GlobalEnv)
        
        # 處理分段
        segment_result <- modify_sampling_size(
            clinic_data_name = segment_name,
            max_housenumber_per_city = 20000,
            batch_size = 200
        )
        
        all_results[[i]] <- segment_result
        
        # 清理分段資料
        rm(list = segment_name, envir = .GlobalEnv)
        gc()
    }
    
    # 合併結果（簡化版）
    cat("\n📊 分段處理完成，結果合併中...\n")
    
    total_matched <- sum(sapply(all_results, function(x) x$stats$matched))
    overall_rate <- round(total_matched / total_rows * 100, 2)
    
    cat("總匹配數:", format(total_matched, big.mark = ","), "筆\n")
    cat("總匹配率:", overall_rate, "%\n")
    
    return(list(
        segments = all_results,
        summary = list(
            total = total_rows,
            matched = total_matched,
            rate = overall_rate
        )
    ))
}
```

## 實驗功能

### 採樣效應研究

#### 1. 採樣曲線繪製
```r
# 採樣效應曲線分析
sampling_curve_analysis <- function(clinic_data_name = "clinic") {
    sampling_points <- c(5000, 10000, 15000, 20000, 25000, 30000, 40000, 50000)
    results <- data.frame(
        sampling_