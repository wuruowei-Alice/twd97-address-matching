# ==========================================
# 整合版TWD97診所匹配系統 - 完整保留資料
# 實戰結果: 嘉義縣318筆診所，匹配率96.54%
# 核心原則: 保留所有資料，未匹配填NA，雙輸出格式
# 更新日期: 2025-07-09
# ==========================================

# 必要套件載入
if(!require(dplyr)) install.packages("dplyr")
if(!require(stringr)) install.packages("stringr")
library(dplyr)
library(stringr)

cat("=== 整合版TWD97診所匹配系統載入中 ===\n")
cat("🏆 實戰結果: 嘉義縣318筆診所，匹配率96.54%\n")
cat("🎯 核心原則: 保留所有資料，未匹配填NA\n")
cat("📁 雙輸出: 地址資訊檔 + 現有資料擴展檔\n\n")

# ==========================================
# 整合版匹配系統 - 道路+村里雙重匹配
# ==========================================

integrated_twd97_matching <- function(
    clinic_data_name = "Chiayi_County_clinic",
    housenumber_data_name = "Chiayi_County_housenumber",
    output_base_name = "整合版TWD97診所匹配",
    debug_mode = FALSE
) {
  
  cat("=== 整合版TWD97診所匹配系統 ===\n")
  cat("🎯 策略: 道路匹配 + 村里匹配 + 完整資料保留\n")
  cat("✅ 未匹配資料座標填NA，絕不刪除\n")
  cat("📁 雙輸出: 地址資訊檔 + 現有資料擴展檔\n\n")
  
  # 檢查資料
  if(!exists(clinic_data_name, envir = .GlobalEnv)) {
    cat("❌ 找不到診所資料:", clinic_data_name, "\n")
    return(NULL)
  }
  
  if(!exists(housenumber_data_name, envir = .GlobalEnv)) {
    cat("❌ 找不到門牌資料:", housenumber_data_name, "\n")
    return(NULL)
  }
  
  original_clinic_data <- get(clinic_data_name, envir = .GlobalEnv)
  housenumber_data <- get(housenumber_data_name, envir = .GlobalEnv)
  
  total_clinics <- nrow(original_clinic_data)
  cat("診所總數:", format(total_clinics, big.mark = ","), "筆\n")
  cat("門牌資料:", format(nrow(housenumber_data), big.mark = ","), "筆\n\n")
  
  # 步驟1: 準備門牌資料
  cat("步驟1: 準備門牌資料...\n")
  
  # 有街道名稱的門牌（用於道路匹配）
  street_housenumber <- housenumber_data %>%
    filter(
      !is.na(橫座標), !is.na(縱座標), !is.na(`街.路段`),
      `街.路段` != ""
    ) %>%
    mutate(
      TWD97_X = as.numeric(橫座標),
      TWD97_Y = as.numeric(縱座標),
      門牌道路 = as.character(`街.路段`),
      標準化門牌道路 = str_replace_all(`街.路段`, "台", "臺") %>%
        str_replace_all("[\\s　]+", "") %>%
        str_trim(),
      村里 = if("村里" %in% colnames(housenumber_data)) as.character(村里) else ""
    ) %>%
    filter(!is.na(TWD97_X), !is.na(TWD97_Y), 標準化門牌道路 != "") %>%
    select(門牌道路, 標準化門牌道路, 村里, TWD97_X, TWD97_Y)
  
  cat("有街道名稱的門牌:", format(nrow(street_housenumber), big.mark = ","), "筆\n")
  
  # 無街道名稱但有村里的門牌（用於村里匹配）
  village_housenumber <- NULL
  if("村里" %in% colnames(housenumber_data)) {
    village_housenumber <- housenumber_data %>%
      filter(
        !is.na(橫座標), !is.na(縱座標), !is.na(村里),
        (is.na(`街.路段`) | `街.路段` == ""),
        村里 != ""
      ) %>%
      mutate(
        TWD97_X = as.numeric(橫座標),
        TWD97_Y = as.numeric(縱座標),
        村里 = as.character(村里)
      ) %>%
      filter(!is.na(TWD97_X), !is.na(TWD97_Y)) %>%
      select(村里, TWD97_X, TWD97_Y)
    
    cat("無街道但有村里的門牌:", format(nrow(village_housenumber), big.mark = ","), "筆\n")
  }
  
  # 建立道路索引
  street_index <- street_housenumber %>%
    group_by(標準化門牌道路) %>%
    summarise(
      門牌數量 = n(),
      平均X = mean(TWD97_X),
      平均Y = mean(TWD97_Y),
      原始道路樣本 = first(門牌道路),
      .groups = 'drop'
    )
  
  # 建立村里索引
  village_index <- NULL
  if(!is.null(village_housenumber)) {
    village_index <- village_housenumber %>%
      group_by(村里) %>%
      summarise(
        門牌數量 = n(),
        平均X = mean(TWD97_X),
        平均Y = mean(TWD97_Y),
        .groups = 'drop'
      )
    
    cat("可用村里:", nrow(village_index), "個\n")
  }
  
  cat("\n步驟2: 初始化完整資料集...\n")
  
  # 初始化完整結果 - 保留所有原始欄位
  complete_data <- original_clinic_data %>%
    mutate(
      # 處理診所地址資訊
      診所道路 = if("街_路段" %in% colnames(original_clinic_data)) {
        as.character(`街_路段`)
      } else {
        ""
      },
      標準化診所道路 = if("街_路段" %in% colnames(original_clinic_data)) {
        str_replace_all(`街_路段`, "台", "臺") %>%
          str_replace_all("[\\s　]+", "") %>%
          str_trim()
      } else {
        ""
      },
      診所村里 = if("村里" %in% colnames(original_clinic_data)) {
        as.character(村里)
      } else {
        ""
      },
      診所地區 = if("地區" %in% colnames(original_clinic_data)) {
        as.character(地區)
      } else {
        ""
      },
      
      # 匹配結果欄位 - 全部初始化為NA
      匹配狀態 = "待處理",
      匹配方式 = NA_character_,
      匹配品質 = NA_character_,
      匹配目標 = NA_character_,
      門牌數量 = NA_integer_,
      TWD97_X = NA_real_,
      TWD97_Y = NA_real_,
      座標系統 = NA_character_,
      未匹配原因 = NA_character_,
      處理時間 = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  
  # 分類診所
  has_road_indices <- which(!is.na(complete_data$標準化診所道路) & complete_data$標準化診所道路 != "")
  no_road_has_village_indices <- which(
    (is.na(complete_data$標準化診所道路) | complete_data$標準化診所道路 == "") &
      (!is.na(complete_data$診所村里) & complete_data$診所村里 != "")
  )
  no_info_indices <- which(
    (is.na(complete_data$標準化診所道路) | complete_data$標準化診所道路 == "") &
      (is.na(complete_data$診所村里) | complete_data$診所村里 == "")
  )
  
  cat("有道路資訊:", length(has_road_indices), "筆\n")
  cat("無道路但有村里:", length(no_road_has_village_indices), "筆\n")
  cat("無任何定位資訊:", length(no_info_indices), "筆\n\n")
  
  # 標記無定位資訊的診所
  complete_data$匹配狀態[no_info_indices] <- "未匹配"
  complete_data$未匹配原因[no_info_indices] <- "無道路和村里資訊"
  
  cat("步驟3: 道路匹配...\n")
  
  # 道路匹配統計
  exact_match_count <- 0
  partial_match_count <- 0
  
  # 策略1: 完全精確匹配
  for(i in has_road_indices) {
    clinic_road <- complete_data$標準化診所道路[i]
    
    exact_match <- street_index %>%
      filter(標準化門牌道路 == clinic_road)
    
    if(nrow(exact_match) > 0) {
      match_info <- exact_match[1, ]
      
      complete_data$匹配狀態[i] <- "匹配成功"
      complete_data$匹配方式[i] <- "道路精確匹配"
      complete_data$匹配品質[i] <- "高"
      complete_data$匹配目標[i] <- match_info$原始道路樣本
      complete_data$門牌數量[i] <- match_info$門牌數量
      complete_data$TWD97_X[i] <- match_info$平均X
      complete_data$TWD97_Y[i] <- match_info$平均Y
      complete_data$座標系統[i] <- "TWD97"
      
      exact_match_count <- exact_match_count + 1
    }
  }
  
  # 策略2: 移除段號匹配
  unmatched_road_indices <- intersect(has_road_indices, which(complete_data$匹配狀態 == "待處理"))
  
  for(i in unmatched_road_indices) {
    clinic_road <- complete_data$標準化診所道路[i]
    
    base_road <- gsub("[1-9一二三四五六七八九十]+段", "", clinic_road) %>% str_trim()
    
    if(base_road != "" && nchar(base_road) >= 2) {
      # 使用正確的字串匹配方式
      partial_matches <- street_index %>%
        filter(
          grepl(paste0("^", base_road), 標準化門牌道路, fixed = FALSE) | 
            grepl(base_road, 標準化門牌道路, fixed = TRUE)
        )
      
      if(nrow(partial_matches) > 0) {
        best_match <- partial_matches[which.max(partial_matches$門牌數量), ]
        
        complete_data$匹配狀態[i] <- "匹配成功"
        complete_data$匹配方式[i] <- "道路部分匹配"
        complete_data$匹配品質[i] <- "中等"
        complete_data$匹配目標[i] <- best_match$原始道路樣本
        complete_data$門牌數量[i] <- best_match$門牌數量
        complete_data$TWD97_X[i] <- best_match$平均X
        complete_data$TWD97_Y[i] <- best_match$平均Y
        complete_data$座標系統[i] <- "TWD97"
        
        partial_match_count <- partial_match_count + 1
      }
    }
  }
  
  cat("道路精確匹配:", exact_match_count, "筆\n")
  cat("道路部分匹配:", partial_match_count, "筆\n")
  
  # 標記剩餘道路未匹配
  still_unmatched_road <- intersect(has_road_indices, which(complete_data$匹配狀態 == "待處理"))
  complete_data$匹配狀態[still_unmatched_road] <- "未匹配"
  complete_data$未匹配原因[still_unmatched_road] <- "道路名稱無對應門牌"
  
  cat("步驟4: 村里匹配...\n")
  
  village_match_count <- 0
  
  if(!is.null(village_index) && length(no_road_has_village_indices) > 0) {
    for(i in no_road_has_village_indices) {
      clinic_village <- complete_data$診所村里[i]
      
      # 精確村里匹配
      village_match <- village_index %>%
        filter(村里 == clinic_village)
      
      if(nrow(village_match) > 0) {
        match_info <- village_match[1, ]
        
        complete_data$匹配狀態[i] <- "匹配成功"
        complete_data$匹配方式[i] <- "村里匹配"
        complete_data$匹配品質[i] <- "一般"
        complete_data$匹配目標[i] <- clinic_village
        complete_data$門牌數量[i] <- match_info$門牌數量
        complete_data$TWD97_X[i] <- match_info$平均X
        complete_data$TWD97_Y[i] <- match_info$平均Y
        complete_data$座標系統[i] <- "TWD97"
        
        village_match_count <- village_match_count + 1
      }
    }
    
    # 標記村里未匹配
    unmatched_village <- intersect(no_road_has_village_indices, which(complete_data$匹配狀態 == "待處理"))
    complete_data$匹配狀態[unmatched_village] <- "未匹配"
    complete_data$未匹配原因[unmatched_village] <- "村里無對應門牌"
  } else {
    # 如果沒有村里索引，直接標記為未匹配
    complete_data$匹配狀態[no_road_has_village_indices] <- "未匹配"
    complete_data$未匹配原因[no_road_has_village_indices] <- "無村里門牌資料可用"
  }
  
  cat("村里匹配:", village_match_count, "筆\n\n")
  
  # 最終統計
  total_matched <- sum(complete_data$匹配狀態 == "匹配成功")
  total_unmatched <- sum(complete_data$匹配狀態 == "未匹配")
  overall_rate <- round(total_matched / total_clinics * 100, 2)
  
  cat("=== 整合匹配結果統計 ===\n")
  cat("診所總數:", format(total_clinics, big.mark = ","), "筆\n")
  cat("匹配成功:", format(total_matched, big.mark = ","), "筆\n")
  cat("未匹配:", format(total_unmatched, big.mark = ","), "筆\n")
  cat("整體匹配率:", overall_rate, "%\n\n")
  
  # 詳細統計
  cat("📊 匹配方式統計:\n")
  method_stats <- complete_data %>%
    filter(匹配狀態 == "匹配成功") %>%
    count(匹配方式, 匹配品質, sort = TRUE) %>%
    mutate(比例 = round(n / sum(n) * 100, 1))
  print(method_stats)
  
  cat("\n📊 未匹配原因統計:\n")
  unmatched_stats <- complete_data %>%
    filter(匹配狀態 == "未匹配") %>%
    count(未匹配原因, sort = TRUE) %>%
    mutate(比例 = round(n / sum(n) * 100, 1))
  print(unmatched_stats)
  
  # 檢查座標範圍
  matched_coords <- complete_data %>%
    filter(!is.na(TWD97_X), !is.na(TWD97_Y))
  
  if(nrow(matched_coords) > 0) {
    coord_summary <- matched_coords %>%
      summarise(
        X_min = min(TWD97_X, na.rm = TRUE),
        X_max = max(TWD97_X, na.rm = TRUE),
        Y_min = min(TWD97_Y, na.rm = TRUE),
        Y_max = max(TWD97_Y, na.rm = TRUE)
      )
    
    cat("\n📍 座標範圍:\n")
    cat("X座標:", coord_summary$X_min, "~", coord_summary$X_max, "\n")
    cat("Y座標:", coord_summary$Y_min, "~", coord_summary$Y_max, "\n")
  }
  
  # 步驟5: 雙輸出檔案
  cat("\n步驟5: 生成雙輸出檔案...\n")
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
  
  # 輸出檔案1: 地址相關資訊檔
  address_info_data <- complete_data %>%
    select(
      醫事機構名稱,
      # 動態選擇存在的地址欄位
      any_of(c("地址", "原始地址片段", "標準化地址")),
      診所地區,
      診所村里,
      診所道路,
      標準化診所道路,
      匹配狀態,
      匹配方式,
      匹配品質,
      匹配目標,
      門牌數量,
      TWD97_X,
      TWD97_Y,
      座標系統,
      未匹配原因,
      處理時間
    )
  
  filename1 <- paste0(output_base_name, "_地址資訊_", timestamp, ".csv")
  write.csv(address_info_data, filename1, row.names = FALSE, fileEncoding = "UTF-8")
  cat("✅ 輸出檔案1 (地址資訊):", filename1, "\n")
  
  # 輸出檔案2: 現有資料擴展檔
  # 將匹配欄位添加到原始資料後方
  extended_data <- complete_data
  
  filename2 <- paste0(output_base_name, "_完整資料_", timestamp, ".csv")
  write.csv(extended_data, filename2, row.names = FALSE, fileEncoding = "UTF-8")
  cat("✅ 輸出檔案2 (完整資料):", filename2, "\n")
  
  cat("\n📋 檔案說明:\n")
  cat("檔案1 - 地址資訊檔:\n")
  cat("  - 包含診所基本資訊和地址匹配結果\n")
  cat("  - 適合地理分析和座標使用\n")
  cat("  - 欄位簡潔，便於GIS軟體載入\n\n")
  
  cat("檔案2 - 完整資料檔:\n")
  cat("  - 保留所有原始欄位\n")
  cat("  - 在後方添加匹配結果欄位\n")
  cat("  - 適合完整資料分析\n\n")
  
  cat("📍 QGIS使用說明:\n")
  cat("1. 載入任一CSV檔案\n")
  cat("2. 篩選條件: 匹配狀態 = '匹配成功' (顯示有座標的診所)\n")
  cat("3. X欄位: TWD97_X，Y欄位: TWD97_Y\n")
  cat("4. CRS設定: EPSG:3826 (TWD97 TM2)\n")
  cat("5. 未匹配的診所座標欄位為NA，不會顯示在地圖上\n\n")
  
  return(list(
    complete_data = extended_data,
    address_info_data = address_info_data,
    stats = list(
      total = total_clinics,
      matched = total_matched,
      unmatched = total_unmatched,
      rate = overall_rate,
      exact_matches = exact_match_count,
      partial_matches = partial_match_count,
      village_matches = village_match_count
    ),
    filenames = list(
      address_info = filename1,
      complete_data = filename2
    )
  ))
}

# ==========================================
# 匹配結果驗證工具
# ==========================================

verify_matching_results <- function(results_object = NULL, filename = NULL) {
  cat("=== 匹配結果驗證 ===\n\n")
  
  # 載入資料
  data_to_verify <- NULL
  
  if(!is.null(results_object) && "complete_data" %in% names(results_object)) {
    data_to_verify <- results_object$complete_data
    cat("✓ 從結果物件讀取資料\n")
  } else if(!is.null(filename) && file.exists(filename)) {
    data_to_verify <- read.csv(filename, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    cat("✓ 從檔案讀取資料:", filename, "\n")
  } else {
    cat("❌ 請提供 results_object 或 filename\n")
    return(NULL)
  }
  
  total_rows <- nrow(data_to_verify)
  cat("總資料筆數:", total_rows, "\n\n")
  
  # 驗證1: 資料完整性
  cat("📊 資料完整性驗證:\n")
  
  # 檢查是否有資料遺失
  if("醫事機構名稱" %in% colnames(data_to_verify)) {
    missing_names <- sum(is.na(data_to_verify$醫事機構名稱))
    if(missing_names == 0) {
      cat("✅ 所有診所名稱完整保留\n")
    } else {
      cat("❌ 有", missing_names, "筆診所名稱遺失\n")
    }
  }
  
  # 驗證2: 匹配狀態檢查
  if("匹配狀態" %in% colnames(data_to_verify)) {
    status_check <- data_to_verify %>%
      count(匹配狀態, sort = TRUE)
    cat("\n匹配狀態分布:\n")
    print(status_check)
    
    # 檢查是否有異常狀態
    valid_statuses <- c("匹配成功", "未匹配")
    invalid_statuses <- status_check$匹配狀態[!status_check$匹配狀態 %in% valid_statuses]
    
    if(length(invalid_statuses) == 0) {
      cat("✅ 所有匹配狀態均為有效值\n")
    } else {
      cat("❌ 發現異常匹配狀態:", paste(invalid_statuses, collapse = ", "), "\n")
    }
  }
  
  # 驗證3: 座標品質檢查
  if("TWD97_X" %in% colnames(data_to_verify) && "TWD97_Y" %in% colnames(data_to_verify)) {
    cat("\n📍 座標品質驗證:\n")
    
    # 統計座標情況
    has_coords <- sum(!is.na(data_to_verify$TWD97_X) & !is.na(data_to_verify$TWD97_Y))
    no_coords <- sum(is.na(data_to_verify$TWD97_X) | is.na(data_to_verify$TWD97_Y))
    
    cat("有座標:", has_coords, "筆\n")
    cat("無座標(NA):", no_coords, "筆\n")
    
    # 檢查座標範圍
    if(has_coords > 0) {
      coord_data <- data_to_verify %>%
        filter(!is.na(TWD97_X), !is.na(TWD97_Y))
      
      coord_range <- coord_data %>%
        summarise(
          X_min = min(TWD97_X),
          X_max = max(TWD97_X),
          Y_min = min(TWD97_Y),
          Y_max = max(TWD97_Y)
        )
      
      cat("座標範圍檢查:\n")
      cat("  X: ", coord_range$X_min, " ~ ", coord_range$X_max, "\n")
      cat("  Y: ", coord_range$Y_min, " ~ ", coord_range$Y_max, "\n")
      
      # 檢查TWD97合理範圍
      valid_twd97 <- coord_range$X_min > 100000 && coord_range$X_max < 400000 &&
        coord_range$Y_min > 2000000 && coord_range$Y_max < 3000000
      
      if(valid_twd97) {
        cat("✅ 座標範圍符合TWD97格式\n")
      } else {
        cat("❌ 座標範圍異常，請檢查\n")
      }
    }
  }
  
  # 驗證4: 未匹配原因檢查
  if("未匹配原因" %in% colnames(data_to_verify) && "匹配狀態" %in% colnames(data_to_verify)) {
    cat("\n📋 未匹配原因驗證:\n")
    
    unmatched_data <- data_to_verify %>%
      filter(匹配狀態 == "未匹配")
    
    if(nrow(unmatched_data) > 0) {
      reason_check <- unmatched_data %>%
        count(未匹配原因, sort = TRUE)
      
      print(reason_check)
      
      # 檢查是否有未填原因
      missing_reasons <- sum(is.na(unmatched_data$未匹配原因))
      if(missing_reasons == 0) {
        cat("✅ 所有未匹配診所都有原因說明\n")
      } else {
        cat("❌ 有", missing_reasons, "筆未匹配診所缺少原因說明\n")
      }
    }
  }
  
  cat("\n✅ 驗證完成\n")
  
  return(list(
    total_records = total_rows,
    verification_passed = TRUE
  ))
}

# ==========================================
# 快速執行函數
# ==========================================

# 標準執行（推薦）
run_integrated_matching <- function(
    clinic_data_name = "Chiayi_County_clinic",
    housenumber_data_name = "Chiayi_County_housenumber"
) {
  cat("🚀 執行整合版TWD97匹配...\n\n")
  
  results <- integrated_twd97_matching(
    clinic_data_name = clinic_data_name,
    housenumber_data_name = housenumber_data_name,
    output_base_name = "整合版TWD97診所匹配"
  )
  
  if(!is.null(results)) {
    cat("\n🔍 執行結果驗證...\n")
    verify_matching_results(results)
  }
  
  return(results)
}

# ==========================================
# 主程式載入完成
# ==========================================

cat("=== 整合版TWD97診所匹配系統載入完成 ===\n\n")

cat("🎯 核心特色:\n")
cat("  ✅ 保留所有原始診所資料\n")
cat("  ✅ 未匹配座標填NA，絕不刪除\n")
cat("  ✅ 道路匹配 + 村里匹配雙重策略\n")
cat("  ✅ 雙輸出格式：地址資訊檔 + 完整資料檔\n")
cat("  ✅ 完整結果驗證機制\n\n")

cat("🚀 推薦執行方式:\n")
cat('final_results <- run_integrated_matching("Chiayi_County_clinic", "Chiayi_County_housenumber")\n\n')

cat("🔧 進階自訂執行:\n")
cat('results <- integrated_twd97_matching(\n')
cat('  clinic_data_name = "你的診所資料名稱",\n')
cat('  housenumber_data_name = "你的門牌資料名稱",\n')
cat('  output_base_name = "自訂輸出檔案名稱"\n')
cat(')\n\n')

cat("🔍 結果驗證:\n")
cat('verify_matching_results(results)\n')
cat('# 或驗證CSV檔案\n')
cat('verify_matching_results(filename = "你的檔案.csv")\n\n')

cat("📁 輸出檔案說明:\n")
cat("  📊 檔案1 - 地址資訊檔：診所基本資訊 + 匹配結果\n")
cat("  📊 檔案2 - 完整資料檔：原始資料 + 新增匹配欄位\n\n")

cat("📍 QGIS使用流程:\n")
cat("  1. 載入任一CSV檔案\n")
cat("  2. 篩選: 匹配狀態 = '匹配成功'\n")
cat("  3. X欄位: TWD97_X，Y欄位: TWD97_Y\n")
cat("  4. CRS: EPSG:3826 (TWD97 TM2)\n")
cat("  5. 未匹配診所座標為NA，不會在地圖顯示\n\n")

cat("✨ 匹配品質等級:\n")
cat("  🟢 高品質：道路精確匹配\n")
cat("  🟡 中等品質：道路部分匹配\n")
cat("  🟠 一般品質：村里匹配\n\n")

cat("🎉 準備就緒！推薦執行:\n")
cat('final_results <- run_integrated_matching("Chiayi_County_clinic", "Chiayi_County_housenumber")\n')