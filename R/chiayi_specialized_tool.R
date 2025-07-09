# 最終強化匹配系統 - 針對剩餘9筆未匹配進行優化
if(!require(purrr)) install.packages("purrr")
library(purrr)

final_enhanced_matching <- function(
    clinic_data_name = "clinic",
    housenumber_data_name = "Chiayi_County_housenumber",
    output_base_name = "最終強化匹配結果"
) {
  
  cat("=== 最終強化匹配系統 ===\n")
  cat("🎯 目標：將97.17%提升至99%+\n")
  cat("🔧 針對性優化：村里別名 + 地址解析 + 模糊匹配\n\n")
  
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
  
  cat("診所資料:", nrow(original_clinic_data), "筆\n")
  cat("門牌資料:", nrow(housenumber_data), "筆\n\n")
  
  # 🆕 建立村里別名對應表
  village_aliases <- data.frame(
    查詢名稱 = c("仁里", "雙福村", "三塊厝", "猿樹"),
    標準名稱 = c("仁里村", "雙福里", "三塊厝里", "猿樹里"),
    備註 = c("義竹鄉", "民雄鄉", "大林鎮", "東石鄉"),
    stringsAsFactors = FALSE
  )
  
  # 🆕 街道名稱清理規則
  street_cleaning_rules <- data.frame(
    原始模式 = c("民生社區忠孝街", ".*社區(.+)"),
    清理後 = c("忠孝街", "\\1"),
    stringsAsFactors = FALSE
  )
  
  # 預處理門牌資料 (與前版相同)
  cat("🔧 預處理門牌資料...\n")
  
  # 1. 有街道名稱的門牌
  street_housenumber <- housenumber_data %>%
    filter(
      !is.na(橫座標), !is.na(縱座標),
      !is.na(`街.路段`), `街.路段` != ""
    ) %>%
    mutate(
      TWD97_X = as.numeric(橫座標),
      TWD97_Y = as.numeric(縱座標),
      標準化街道 = str_replace_all(`街.路段`, "台", "臺") %>%
        str_replace_all("[\\s　]+", "") %>%
        str_trim(),
      村里 = as.character(村里)
    ) %>%
    filter(!is.na(TWD97_X), !is.na(TWD97_Y), 標準化街道 != "")
  
  # 2. 只有村里的門牌
  village_only_housenumber <- housenumber_data %>%
    filter(
      !is.na(橫座標), !is.na(縱座標),
      (is.na(`街.路段`) | `街.路段` == ""),
      !is.na(村里), 村里 != ""
    ) %>%
    mutate(
      TWD97_X = as.numeric(橫座標),
      TWD97_Y = as.numeric(縱座標),
      標準化村里 = str_replace_all(村里, "台", "臺") %>%
        str_replace_all("[\\s　]+", "") %>%
        str_trim()
    ) %>%
    filter(!is.na(TWD97_X), !is.na(TWD97_Y), 標準化村里 != "")
  
  cat("有街道的門牌:", nrow(street_housenumber), "筆\n")
  cat("只有村里的門牌:", nrow(village_only_housenumber), "筆\n")
  
  # 🆕 建立擴展村里索引 (包含別名)
  extended_village_index <- village_only_housenumber %>%
    group_by(標準化村里) %>%
    summarise(
      門牌數量 = n(),
      平均X = mean(TWD97_X),
      平均Y = mean(TWD97_Y),
      .groups = 'drop'
    )
  
  # 檢查門牌資料中的村里名稱
  available_villages <- unique(extended_village_index$標準化村里)
  cat("門牌資料中的村里數量:", length(available_villages), "個\n")
  
  # 🆕 進階地址解析函數
  enhanced_address_parsing <- function(address) {
    if(is.na(address) || address == "") return(list(village = "", street = ""))
    
    # 清理地址
    clean_addr <- gsub("嘉義縣", "", address)
    clean_addr <- gsub("[鄉鎮市區]", "", clean_addr)
    
    # 提取村里 - 多種模式
    village_patterns <- c(
      "([^0-9]+村)",
      "([^0-9]+里)",
      "([^0-9]+社區)",
      "([^0-9]+厝)",
      "([^0-9]+樹)"
    )
    
    village <- ""
    for(pattern in village_patterns) {
      matches <- regmatches(clean_addr, gregexpr(pattern, clean_addr, perl = TRUE))[[1]]
      if(length(matches) > 0) {
        village <- matches[1]
        break
      }
    }
    
    # 提取街道
    street_patterns <- c(
      "([^0-9]+街)",
      "([^0-9]+路)",
      "([^0-9]+道)",
      "([^0-9]+巷)"
    )
    
    street <- ""
    for(pattern in street_patterns) {
      matches <- regmatches(clean_addr, gregexpr(pattern, clean_addr, perl = TRUE))[[1]]
      if(length(matches) > 0) {
        street <- matches[1]
        break
      }
    }
    
    return(list(village = str_trim(village), street = str_trim(street)))
  }
  
  # 篩選嘉義縣診所並初始化結果
  chiayi_clinics <- original_clinic_data %>%
    filter(grepl("嘉義縣", 地址))
  
  result_data <- chiayi_clinics %>%
    mutate(
      # 🆕 進階地址解析
      解析結果 = map(地址, enhanced_address_parsing),
      解析村里 = map_chr(解析結果, ~ .x$village),
      解析街道 = map_chr(解析結果, ~ .x$street),
      
      # 原有欄位處理
      標準化街道 = str_replace_all(ifelse(is.na(`街_路段`), "", `街_路段`), "台", "臺") %>%
        str_replace_all("[\\s　]+", "") %>%
        str_trim(),
      
      標準化村里 = str_replace_all(ifelse(is.na(村里), "", 村里), "台", "臺") %>%
        str_replace_all("[\\s　]+", "") %>%
        str_trim(),
      
      # 🆕 綜合村里資訊 (優先使用現有欄位，其次用解析結果)
      最終村里 = case_when(
        標準化村里 != "" ~ 標準化村里,
        解析村里 != "" ~ 解析村里,
        TRUE ~ ""
      ),
      
      # 🆕 綜合街道資訊
      最終街道 = case_when(
        標準化街道 != "" ~ 標準化街道,
        解析街道 != "" ~ 解析街道,
        TRUE ~ ""
      ),
      
      # 匹配結果欄位
      匹配狀態 = "待處理",
      匹配方式 = NA_character_,
      匹配品質 = NA_character_,
      匹配目標 = NA_character_,
      匹配信心度 = NA_real_,
      門牌數量 = NA_integer_,
      TWD97_X = NA_real_,
      TWD97_Y = NA_real_,
      座標系統 = NA_character_,
      未匹配原因 = NA_character_,
      處理時間 = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ) %>%
    select(-解析結果)  # 移除臨時欄位
  
  # 建立索引
  street_index <- street_housenumber %>%
    group_by(標準化街道) %>%
    summarise(
      門牌數量 = n(),
      平均X = mean(TWD97_X),
      平均Y = mean(TWD97_Y),
      .groups = 'drop'
    )
  
  # 匹配統計
  match_stats <- list(
    street_exact = 0,
    street_partial = 0,
    street_cleaned = 0,
    village_exact = 0,
    village_alias = 0,
    village_fuzzy = 0,
    address_parsing = 0,
    unmatched = 0
  )
  
  cat("\n步驟1: 街道精確匹配...\n")
  
  # 階段1：街道精確匹配
  for(i in 1:nrow(result_data)) {
    if(result_data$匹配狀態[i] != "待處理") next
    
    if(result_data$最終街道[i] != "") {
      exact_match <- street_index %>%
        filter(標準化街道 == result_data$最終街道[i])
      
      if(nrow(exact_match) > 0) {
        match_info <- exact_match[1,]
        
        result_data$匹配狀態[i] <- "匹配成功"
        result_data$匹配方式[i] <- "街道精確匹配"
        result_data$匹配品質[i] <- "高"
        result_data$匹配目標[i] <- result_data$最終街道[i]
        result_data$匹配信心度[i] <- 1.0
        result_data$門牌數量[i] <- match_info$門牌數量
        result_data$TWD97_X[i] <- match_info$平均X
        result_data$TWD97_Y[i] <- match_info$平均Y
        result_data$座標系統[i] <- "TWD97"
        
        match_stats$street_exact <- match_stats$street_exact + 1
      }
    }
  }
  
  cat("  街道精確匹配:", match_stats$street_exact, "筆\n")
  
  cat("步驟2: 街道清理匹配...\n")
  
  # 🆕 階段2：街道清理匹配
  for(i in 1:nrow(result_data)) {
    if(result_data$匹配狀態[i] != "待處理") next
    
    if(result_data$最終街道[i] != "") {
      original_street <- result_data$最終街道[i]
      
      # 應用清理規則
      cleaned_street <- original_street
      for(j in 1:nrow(street_cleaning_rules)) {
        pattern <- street_cleaning_rules$原始模式[j]
        replacement <- street_cleaning_rules$清理後[j]
        cleaned_street <- gsub(pattern, replacement, cleaned_street)
      }
      
      if(cleaned_street != original_street) {
        cleaned_match <- street_index %>%
          filter(標準化街道 == cleaned_street)
        
        if(nrow(cleaned_match) > 0) {
          match_info <- cleaned_match[1,]
          
          result_data$匹配狀態[i] <- "匹配成功"
          result_data$匹配方式[i] <- "街道清理匹配"
          result_data$匹配品質[i] <- "中高"
          result_data$匹配目標[i] <- paste0(original_street, " → ", cleaned_street)
          result_data$匹配信心度[i] <- 0.9
          result_data$門牌數量[i] <- match_info$門牌數量
          result_data$TWD97_X[i] <- match_info$平均X
          result_data$TWD97_Y[i] <- match_info$平均Y
          result_data$座標系統[i] <- "TWD97"
          
          match_stats$street_cleaned <- match_stats$street_cleaned + 1
        }
      }
    }
  }
  
  cat("  街道清理匹配:", match_stats$street_cleaned, "筆\n")
  
  cat("步驟3: 村里精確匹配...\n")
  
  # 階段3：村里精確匹配
  for(i in 1:nrow(result_data)) {
    if(result_data$匹配狀態[i] != "待處理") next
    
    if(result_data$最終村里[i] != "") {
      exact_match <- extended_village_index %>%
        filter(標準化村里 == result_data$最終村里[i])
      
      if(nrow(exact_match) > 0) {
        match_info <- exact_match[1,]
        
        result_data$匹配狀態[i] <- "匹配成功"
        result_data$匹配方式[i] <- "村里精確匹配"
        result_data$匹配品質[i] <- "中等"
        result_data$匹配目標[i] <- result_data$最終村里[i]
        result_data$匹配信心度[i] <- 0.8
        result_data$門牌數量[i] <- match_info$門牌數量
        result_data$TWD97_X[i] <- match_info$平均X
        result_data$TWD97_Y[i] <- match_info$平均Y
        result_data$座標系統[i] <- "TWD97"
        
        match_stats$village_exact <- match_stats$village_exact + 1
      }
    }
  }
  
  cat("  村里精確匹配:", match_stats$village_exact, "筆\n")
  
  cat("步驟4: 村里別名匹配...\n")
  
  # 🆕 階段4：村里別名匹配
  for(i in 1:nrow(result_data)) {
    if(result_data$匹配狀態[i] != "待處理") next
    
    if(result_data$最終村里[i] != "") {
      original_village <- result_data$最終村里[i]
      
      # 檢查別名對應
      alias_row <- village_aliases %>%
        filter(查詢名稱 == original_village)
      
      if(nrow(alias_row) > 0) {
        standard_name <- alias_row$標準名稱[1]
        
        alias_match <- extended_village_index %>%
          filter(標準化村里 == standard_name)
        
        if(nrow(alias_match) > 0) {
          match_info <- alias_match[1,]
          
          result_data$匹配狀態[i] <- "匹配成功"
          result_data$匹配方式[i] <- "村里別名匹配"
          result_data$匹配品質[i] <- "中等"
          result_data$匹配目標[i] <- paste0(original_village, " → ", standard_name)
          result_data$匹配信心度[i] <- 0.85
          result_data$門牌數量[i] <- match_info$門牌數量
          result_data$TWD97_X[i] <- match_info$平均X
          result_data$TWD97_Y[i] <- match_info$平均Y
          result_data$座標系統[i] <- "TWD97"
          
          match_stats$village_alias <- match_stats$village_alias + 1
        }
      }
    }
  }
  
  cat("  村里別名匹配:", match_stats$village_alias, "筆\n")
  
  cat("步驟5: 村里模糊匹配...\n")
  
  # 🆕 階段5：村里模糊匹配
  for(i in 1:nrow(result_data)) {
    if(result_data$匹配狀態[i] != "待處理") next
    
    if(result_data$最終村里[i] != "" && nchar(result_data$最終村里[i]) >= 2) {
      original_village <- result_data$最終村里[i]
      
      # 模糊匹配
      fuzzy_candidates <- extended_village_index %>%
        mutate(
          相似度 = sapply(標準化村里, function(village) {
            # 計算相似度
            if(grepl(gsub("[村里]$", "", original_village), village)) {
              return(0.8)
            }
            max_len <- max(nchar(original_village), nchar(village))
            if(max_len == 0) return(0)
            edit_dist <- adist(original_village, village)[1,1]
            return(1 - edit_dist / max_len)
          })
        ) %>%
        filter(相似度 >= 0.7) %>%
        arrange(desc(相似度))
      
      if(nrow(fuzzy_candidates) > 0) {
        match_info <- fuzzy_candidates[1,]
        
        result_data$匹配狀態[i] <- "匹配成功"
        result_data$匹配方式[i] <- "村里模糊匹配"
        result_data$匹配品質[i] <- "一般"
        result_data$匹配目標[i] <- paste0(original_village, " ≈ ", match_info$標準化村里)
        result_data$匹配信心度[i] <- match_info$相似度
        result_data$門牌數量[i] <- match_info$門牌數量
        result_data$TWD97_X[i] <- match_info$平均X
        result_data$TWD97_Y[i] <- match_info$平均Y
        result_data$座標系統[i] <- "TWD97"
        
        match_stats$village_fuzzy <- match_stats$village_fuzzy + 1
      }
    }
  }
  
  cat("  村里模糊匹配:", match_stats$village_fuzzy, "筆\n")
  
  # 標記未匹配
  for(i in 1:nrow(result_data)) {
    if(result_data$匹配狀態[i] == "待處理") {
      result_data$匹配狀態[i] <- "未匹配"
      
      if(result_data$最終街道[i] == "" && result_data$最終村里[i] == "") {
        result_data$未匹配原因[i] <- "無法解析街道和村里資訊"
      } else if(result_data$最終街道[i] != "") {
        result_data$未匹配原因[i] <- "街道名稱在門牌資料中不存在"
      } else {
        result_data$未匹配原因[i] <- "村里名稱在門牌資料中不存在"
      }
      
      match_stats$unmatched <- match_stats$unmatched + 1
    }
  }
  
  # 統計結果
  total_matched <- sum(unlist(match_stats[1:7]))
  match_rate <- round(total_matched / nrow(result_data) * 100, 2)
  
  cat("\n=== 最終強化匹配結果 ===\n")
  cat("診所總數:", nrow(result_data), "筆\n")
  cat("匹配成功:", total_matched, "筆\n")
  cat("未匹配:", match_stats$unmatched, "筆\n")
  cat("匹配率:", match_rate, "%\n\n")
  
  cat("📊 詳細匹配統計:\n")
  cat("街道精確匹配:", match_stats$street_exact, "筆\n")
  cat("街道清理匹配:", match_stats$street_cleaned, "筆\n")
  cat("村里精確匹配:", match_stats$village_exact, "筆\n")
  cat("村里別名匹配:", match_stats$village_alias, "筆\n")
  cat("村里模糊匹配:", match_stats$village_fuzzy, "筆\n")
  cat("未匹配:", match_stats$unmatched, "筆\n")
  
  # 輸出檔案
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
  filename <- paste0(output_base_name, "_", timestamp, ".csv")
  write.csv(result_data, filename, row.names = FALSE, fileEncoding = "UTF-8")
  
  cat("\n✅ 輸出檔案:", filename, "\n")
  
  return(list(
    result_data = result_data,
    match_stats = match_stats,
    match_rate = match_rate,
    filename = filename
  ))
}

# 快速執行
run_final_enhanced_matching <- function() {
  cat("🚀 執行最終強化匹配...\n\n")
  
  results <- final_enhanced_matching(
    clinic_data_name = "clinic",
    housenumber_data_name = "Chiayi_County_housenumber",
    output_base_name = "最終強化匹配結果"
  )
  
  return(results)
}