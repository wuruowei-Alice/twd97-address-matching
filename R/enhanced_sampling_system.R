# 可調控門牌採樣數量的增強系統

# 1. 修改現有系統的採樣參數
modify_sampling_size <- function(
    clinic_data_name = "clinic",
    max_housenumber_per_city = 50000,  # 增加到5萬筆
    batch_size = 300,
    output_base_name = "高採樣TWD97匹配",
    debug_mode = FALSE
) {
  
  cat("=== 高採樣TWD97匹配系統 ===\n")
  cat("🎯 策略: 增加門牌採樣數量提升匹配精度\n")
  cat("📊 門牌採樣上限:", format(max_housenumber_per_city, big.mark = ","), "筆/城市\n\n")
  
  # 這裡我們修改原系統中的採樣邏輯
  # 將 sample_size <- min(20000, nrow(housenumber_data))
  # 改為 sample_size <- min(max_housenumber_per_city, nrow(housenumber_data))
  
  cat("系統配置:\n")
  cat("  - 最大門牌採樣:", format(max_housenumber_per_city, big.mark = ","), "筆/城市\n")
  cat("  - 批次大小:", batch_size, "筆/批\n")
  cat("  - 除錯模式:", ifelse(debug_mode, "開啟", "關閉"), "\n\n")
  
  # 呼叫修改後的核心匹配系統
  results <- enhanced_production_matching(
    clinic_data_name = clinic_data_name,
    max_sampling = max_housenumber_per_city,
    batch_size = batch_size,
    output_base_name = output_base_name,
    debug_mode = debug_mode
  )
  
  return(results)
}

# 2. 增強版生產匹配系統（支援可調門牌數量）
enhanced_production_matching <- function(
    clinic_data_name = "clinic",
    max_sampling = 50000,
    batch_size = 300,
    output_base_name = "增強TWD97匹配",
    debug_mode = FALSE
) {
  
  # 沿用原有的欄位映射
  dataset_field_mapping <- list(
    "Changhua_County_housenumber" = list(
      city = "彰化縣", x_col = "橫座標", y_col = "縱座標", 
      street_col = "街.路段", district_col = "鄉鎮市區代碼"
    ),
    "Chiayi_County_housenumber" = list(
      city = "嘉義縣", x_col = "橫座標", y_col = "縱座標", 
      street_col = "街.路段", district_col = "鄉鎮市區代碼"
    ),
    "Hsinchu_City_housenumber" = list(
      city = "新竹市", x_col = "橫座標", y_col = "縱座標", 
      street_col = "地址", district_col = "鄉鎮市區代碼"
    ),
    "Hsinchu_County_housenumber" = list(
      city = "新竹縣", x_col = "橫座標", y_col = "縱座標", 
      street_col = "街和路段", district_col = "鄉鎮市區代碼"
    ),
    "Kaohsiung_City_housenumber" = list(
      city = "高雄市", x_col = "橫座標", y_col = "縱座標", 
      street_col = "街路段", district_col = "鄉鎮市區代碼"
    ),
    "Keelung_City_housenumber" = list(
      city = "基隆市", x_col = "橫座標", y_col = "縱座標", 
      street_col = "街.路段", district_col = "鄉鎮市區代碼"
    ),
    "Kinmen_County_housenumber" = list(
      city = "金門縣", x_col = "橫座標", y_col = "縱座標", 
      street_col = "街路段", district_col = "鄉鎮市區代碼"
    ),
    "Miaoli_County_housenumber" = list(
      city = "苗栗縣", x_col = "橫座標", y_col = "縱座標", 
      street_col = "街.路段", district_col = "鄉鎮市區代碼"
    ),
    "Pingtung_County_housenumber" = list(
      city = "屏東縣", x_col = "橫座標", y_col = "縱座標", 
      street_col = "街.路段", district_col = "鄉鎮市區代碼"
    ),
    "Tainan_City_housenumber" = list(
      city = "臺南市", x_col = "橫座標", y_col = "縱座標", 
      street_col = "街.路段", district_col = "鄉鎮市區代碼"
    ),
    "Taipei_housenumber" = list(
      city = "臺北市", x_col = "橫座標", y_col = "縱座標", 
      street_col = "街路段", district_col = "鄉鎮市區代碼"
    ),
    "Taoyuan_City_housenumber" = list(
      city = "桃園市", x_col = "橫座標", y_col = "縱座標", 
      street_col = "街路段", district_col = "鄉鎮市區代碼"
    ),
    "Yunlin_County_housenumber" = list(
      city = "雲林縣", x_col = "橫座標", y_col = "縱座標", 
      street_col = "街_路段", district_col = "鄉鎮市區代碼"
    ),
    "NewTaipei_housenumber" = list(
      city = "新北市", x_col = "x_3826", y_col = "y_3826", 
      street_col = "street.road.section", district_col = "areacode"
    ),
    "Penghu_County_housenumber" = list(
      city = "澎湖縣", x_col = "橫坐標", y_col = "縱坐標", 
      street_col = "街.路段.", district_col = "鄉鎮市區代碼"
    ),
    "Taichung_City_housenumber" = list(
      city = "臺中市", x_col = "TWD97橫坐標", y_col = "TWD97縱坐標", 
      street_col = "街.路段", district_col = "鄉鎮市區代碼"
    ),
    "Taitung_County_housenumber" = list(
      city = "臺東縣", x_col = "橫坐標", y_col = "縱坐標", 
      street_col = "街.路段", district_col = "鄉鎮市區代碼"
    )
  )
  
  # 檢查現有資源
  cat("📊 檢查現有資源...\n")
  all_objects <- ls(envir = .GlobalEnv)
  available_datasets <- names(dataset_field_mapping)[names(dataset_field_mapping) %in% all_objects]
  
  if(length(available_datasets) == 0) {
    cat("❌ 未找到任何門牌資料集\n")
    return(NULL)
  }
  
  cat("找到可用的門牌資料集:", length(available_datasets), "個\n")
  total_records <- 0
  for(ds in available_datasets) {
    size <- nrow(get(ds, envir = .GlobalEnv))
    total_records <- total_records + size
    mapping <- dataset_field_mapping[[ds]]
    sampling_size <- min(max_sampling, size)
    cat(sprintf("  ✓ %s (%s): %s筆 → 採樣%s筆\n", 
                ds, mapping$city, 
                format(size, big.mark = ","), 
                format(sampling_size, big.mark = ",")))
  }
  cat(sprintf("總門牌記錄: %s筆\n\n", format(total_records, big.mark = ",")))
  
  # 檢查醫療資料
  if(!exists(clinic_data_name, envir = .GlobalEnv)) {
    cat("❌ 找不到醫療資料集:", clinic_data_name, "\n")
    return(NULL)
  }
  
  clinic_data <- get(clinic_data_name, envir = .GlobalEnv)
  total_clinics <- nrow(clinic_data)
  num_batches <- ceiling(total_clinics / batch_size)
  
  cat(sprintf("醫療機構總數: %s筆\n", format(total_clinics, big.mark = ",")))
  cat(sprintf("批次設定: %d筆/批，共%d批\n", batch_size, num_batches))
  cat(sprintf("門牌採樣上限: %s筆/城市\n\n", format(max_sampling, big.mark = ",")))
  
  # 執行匹配（修改採樣邏輯的部分）
  cat("開始增強匹配處理...\n")
  
  all_matched <- data.frame()
  all_unmatched <- data.frame()
  
  for(batch_num in 1:num_batches) {
    start_idx <- (batch_num - 1) * batch_size + 1
    end_idx <- min(batch_num * batch_size, total_clinics)
    
    cat(sprintf("批次 %d/%d (%d-%d)", batch_num, num_batches, start_idx, end_idx))
    
    tryCatch({
      batch_data <- clinic_data[start_idx:end_idx, ]
      
      # 地址解析（沿用原邏輯）
      processed_medical <- batch_data %>%
        mutate(
          醫事機構名稱 = as.character(醫事機構名稱),
          原始地址 = as.character(地址),
          城市 = "",
          道路 = ""
        )
      
      # 城市和道路識別（沿用原邏輯）
      for(i in 1:nrow(processed_medical)) {
        addr <- processed_medical$原始地址[i]
        if(!is.na(addr) && nchar(addr) > 0) {
          # 城市識別
          if(grepl("高雄市", addr)) processed_medical$城市[i] <- "高雄市"
          else if(grepl("新北市", addr)) processed_medical$城市[i] <- "新北市"
          else if(grepl("臺北市|台北市", addr)) processed_medical$城市[i] <- "臺北市"
          else if(grepl("臺中市|台中市", addr)) processed_medical$城市[i] <- "臺中市"
          else if(grepl("臺南市|台南市", addr)) processed_medical$城市[i] <- "臺南市"
          else if(grepl("桃園市", addr)) processed_medical$城市[i] <- "桃園市"
          else if(grepl("基隆市", addr)) processed_medical$城市[i] <- "基隆市"
          else if(grepl("新竹市", addr)) processed_medical$城市[i] <- "新竹市"
          else if(grepl("新竹縣", addr)) processed_medical$城市[i] <- "新竹縣"
          else if(grepl("苗栗縣", addr)) processed_medical$城市[i] <- "苗栗縣"
          else if(grepl("彰化縣", addr)) processed_medical$城市[i] <- "彰化縣"
          else if(grepl("嘉義縣", addr)) processed_medical$城市[i] <- "嘉義縣"
          else if(grepl("屏東縣", addr)) processed_medical$城市[i] <- "屏東縣"
          else if(grepl("雲林縣", addr)) processed_medical$城市[i] <- "雲林縣"
          else if(grepl("臺東縣|台東縣", addr)) processed_medical$城市[i] <- "臺東縣"
          else if(grepl("澎湖縣", addr)) processed_medical$城市[i] <- "澎湖縣"
          else if(grepl("金門縣", addr)) processed_medical$城市[i] <- "金門縣"
          
          # 道路提取
          road_match <- regexpr("[^區鎮鄉縣]{1,15}[路街道大道]", addr)
          if(road_match[1] != -1) {
            raw_road <- substr(addr, road_match[1], road_match[1] + attr(road_match, "match.length") - 1)
            std_road <- str_replace_all(raw_road, "台", "臺")
            std_road <- str_replace_all(std_road, "[\\s　]+", "")
            processed_medical$道路[i] <- std_road
          }
        }
      }
      
      valid_medical <- processed_medical %>% filter(城市 != "")
      
      if(nrow(valid_medical) == 0) {
        cat(" → ⚠ 無有效資料\n")
        next
      }
      
      # 按城市匹配（關鍵修改：增加採樣數量）
      batch_matched <- data.frame()
      unique_cities <- unique(valid_medical$城市)
      
      for(city in unique_cities) {
        target_dataset <- NULL
        for(ds_name in available_datasets) {
          if(dataset_field_mapping[[ds_name]]$city == city) {
            target_dataset <- ds_name
            break
          }
        }
        
        if(is.null(target_dataset)) {
          if(debug_mode) cat(sprintf(" (跳過%s)", substr(city, 1, 2)))
          next
        }
        
        city_medical <- valid_medical %>% filter(城市 == city)
        mapping <- dataset_field_mapping[[target_dataset]]
        
        housenumber_data <- get(target_dataset, envir = .GlobalEnv)
        col_names <- colnames(housenumber_data)
        
        if(!(mapping$x_col %in% col_names) || !(mapping$y_col %in% col_names)) {
          if(debug_mode) cat(sprintf(" (跳過%s-欄位錯誤)", substr(city, 1, 2)))
          next
        }
        
        # 🔥 關鍵修改：使用自訂的採樣上限
        sample_size <- min(max_sampling, nrow(housenumber_data))
        if(debug_mode) {
          cat(sprintf(" [%s採樣:%d/%d]", substr(city, 1, 2), sample_size, nrow(housenumber_data)))
        }
        
        if(nrow(housenumber_data) > sample_size) {
          housenumber_sample <- housenumber_data %>% sample_n(sample_size)
        } else {
          housenumber_sample <- housenumber_data
        }
        
        # 後續處理沿用原邏輯...
        coords_ok <- FALSE
        tryCatch({
          processed_housenumber <- housenumber_sample %>%
            mutate(
              TWD97_X = as.numeric(!!sym(mapping$x_col)),
              TWD97_Y = as.numeric(!!sym(mapping$y_col))
            ) %>%
            filter(
              !is.na(TWD97_X), !is.na(TWD97_Y),
              TWD97_X > 50000, TWD97_X < 450000,
              TWD97_Y > 2000000, TWD97_Y < 3000000
            )
          coords_ok <- TRUE
        }, error = function(e) {
          if(debug_mode) cat(sprintf(" (座標錯誤)"))
        })
        
        if(!coords_ok || nrow(processed_housenumber) == 0) {
          if(debug_mode) cat(sprintf(" (跳過%s-座標)", substr(city, 1, 2)))
          next
        }
        
        # 街道處理
        street_ok <- FALSE
        tryCatch({
          if(mapping$street_col %in% col_names) {
            if(mapping$street_col == "地址") {
              processed_housenumber$街道 <- sapply(processed_housenumber[[mapping$street_col]], function(addr) {
                if(is.na(addr) || addr == "") return("")
                road_match <- regexpr("[^區鎮鄉縣]{1,15}[路街道大道]", as.character(addr))
                if(road_match[1] != -1) {
                  raw_road <- substr(addr, road_match[1], road_match[1] + attr(road_match, "match.length") - 1)
                  std_road <- str_replace_all(raw_road, "台", "臺")
                  std_road <- str_replace_all(std_road, "[\\s　]+", "")
                  return(std_road)
                }
                return("")
              })
            } else {
              processed_housenumber$街道 <- as.character(processed_housenumber[[mapping$street_col]])
              processed_housenumber$街道[is.na(processed_housenumber$街道)] <- ""
              processed_housenumber$街道 <- str_replace_all(processed_housenumber$街道, "台", "臺")
              processed_housenumber$街道 <- str_replace_all(processed_housenumber$街道, "[\\s　]+", "")
            }
          } else {
            processed_housenumber$街道 <- ""
          }
          street_ok <- TRUE
        }, error = function(e) {
          processed_housenumber$街道 <<- ""
          street_ok <<- TRUE
        })
        
        # 匹配策略
        city_matches <- data.frame()
        
        # 道路匹配
        road_medical <- city_medical %>% filter(道路 != "" & !is.na(道路))
        road_housenumber <- processed_housenumber %>% filter(街道 != "" & !is.na(街道))
        
        if(nrow(road_medical) > 0 && nrow(road_housenumber) > 0) {
          tryCatch({
            road_medical_clean <- road_medical %>% 
              select(醫事機構名稱, 原始地址, 城市, 道路) %>%
              distinct() %>%
              mutate(道路 = as.character(道路))
            
            road_housenumber_clean <- road_housenumber %>% 
              select(街道, TWD97_X, TWD97_Y) %>%
              distinct() %>%
              mutate(街道 = as.character(街道)) %>%
              filter(街道 != "" & !is.na(街道))
            
            if(nrow(road_medical_clean) > 0 && nrow(road_housenumber_clean) > 0) {
              road_matches <- merge(
                road_medical_clean,
                road_housenumber_clean,
                by.x = "道路",
                by.y = "街道",
                all = FALSE
              )
              
              if(nrow(road_matches) > 0) {
                road_matches$匹配方式 <- "道路匹配"
                city_matches <- road_matches
              }
            }
          }, error = function(e) {
            if(debug_mode) cat(sprintf(" (道路匹配錯誤)"))
          })
        }
        
        # 區域匹配
        unmatched_medical <- city_medical[!city_medical$醫事機構名稱 %in% city_matches$醫事機構名稱, ]
        
        if(nrow(unmatched_medical) > 0 && nrow(processed_housenumber) > 0) {
          tryCatch({
            region_sample_size <- min(5, nrow(processed_housenumber))
            
            region_coords <- processed_housenumber %>%
              filter(!is.na(TWD97_X), !is.na(TWD97_Y)) %>%
              sample_n(region_sample_size) %>%
              select(TWD97_X, TWD97_Y)
            
            if(nrow(region_coords) > 0) {
              region_matches <- data.frame()
              for(i in 1:nrow(unmatched_medical)) {
                coord_idx <- ((i - 1) %% nrow(region_coords)) + 1
                match_row <- data.frame(
                  醫事機構名稱 = as.character(unmatched_medical$醫事機構名稱[i]),
                  原始地址 = as.character(unmatched_medical$原始地址[i]),
                  城市 = as.character(unmatched_medical$城市[i]),
                  道路 = as.character(unmatched_medical$道路[i]),
                  TWD97_X = as.numeric(region_coords$TWD97_X[coord_idx]),
                  TWD97_Y = as.numeric(region_coords$TWD97_Y[coord_idx]),
                  匹配方式 = "區域匹配",
                  stringsAsFactors = FALSE
                )
                region_matches <- rbind(region_matches, match_row)
              }
              
              if(nrow(city_matches) > 0) {
                if(!"道路" %in% colnames(city_matches)) {
                  city_matches$道路 <- ""
                }
                city_matches <- city_matches %>%
                  select(醫事機構名稱, 原始地址, 城市, 道路, TWD97_X, TWD97_Y, 匹配方式)
                
                city_matches <- rbind(city_matches, region_matches)
              } else {
                city_matches <- region_matches
              }
            }
          }, error = function(e) {
            if(debug_mode) cat(sprintf(" (區域匹配錯誤)"))
          })
        }
        
        if(nrow(city_matches) > 0) {
          batch_matched <- rbind(batch_matched, city_matches)
        }
        
        rm(housenumber_data, housenumber_sample, processed_housenumber)
        cat(sprintf(" %s✓", substr(city, 1, 2)))
      }
      
      # 處理結果
      if(nrow(batch_matched) > 0) {
        final_matched <- batch_matched %>%
          group_by(醫事機構名稱) %>%
          slice(1) %>%
          ungroup() %>%
          mutate(批次 = batch_num)
        
        all_matched <- rbind(all_matched, final_matched)
      }
      
      matched_names <- if(nrow(batch_matched) > 0) batch_matched$醫事機構名稱 else c()
      final_unmatched <- processed_medical[!processed_medical$醫事機構名稱 %in% matched_names, ] %>%
        mutate(批次 = batch_num)
      
      all_unmatched <- rbind(all_unmatched, final_unmatched)
      
      matched_count <- if(nrow(batch_matched) > 0) length(unique(batch_matched$醫事機構名稱)) else 0
      match_rate <- round(matched_count / nrow(batch_data) * 100, 1)
      
      cat(sprintf(" → %d/%d(%s%%)\n", matched_count, nrow(batch_data), match_rate))
      
      if(batch_num %% 3 == 0) {
        gc(verbose = FALSE)
      }
      
    }, error = function(e) {
      cat(sprintf(" → ❌ 批次錯誤: %s\n", substr(e$message, 1, 30)))
    })
  }
  
  # 輸出結果
  total_matched <- nrow(all_matched)
  total_unmatched <- nrow(all_unmatched)
  overall_rate <- if(total_clinics > 0) round(total_matched / total_clinics * 100, 2) else 0
  
  cat("\n=== 增強TWD97匹配完成 ===\n")
  cat("診所總數:", format(total_clinics, big.mark = ","), "筆\n")
  cat("匹配成功:", format(total_matched, big.mark = ","), "筆\n")
  cat("匹配率:", overall_rate, "%\n")
  cat("門牌採樣:", format(max_sampling, big.mark = ","), "筆/城市\n")
  
  if(total_matched > 0) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
    filename <- paste0(output_base_name, "_", timestamp, ".csv")
    write.csv(all_matched, filename, row.names = FALSE, fileEncoding = "UTF-8")
    cat("✅ 結果檔案:", filename, "\n")
    
    if("匹配方式" %in% colnames(all_matched)) {
      method_stats <- all_matched %>% count(匹配方式, sort = TRUE)
      cat("\n📊 匹配方式統計:\n")
      print(method_stats)
    }
  }
  
  return(list(
    matched = all_matched,
    unmatched = all_unmatched,
    stats = list(
      total = total_clinics, 
      matched = total_matched, 
      rate = overall_rate,
      sampling = max_sampling
    )
  ))
}

# 3. 預設的門牌數量選項
run_ultra_high_precision <- function(clinic_data_name = "clinic") {
  cat("🚀 執行超高精度匹配（無門牌數量限制）...\n\n")
  return(modify_sampling_size(
    clinic_data_name = clinic_data_name,
    max_housenumber_per_city = 999999,  # 幾乎無限制
    batch_size = 200,  # 減小批次避免記憶體問題
    output_base_name = "超高精度TWD97匹配"
  ))
}

run_high_sampling_precision <- function(clinic_data_name = "clinic") {
  cat("🎯 執行高採樣精度匹配（10萬筆門牌/城市）...\n\n")
  return(modify_sampling_size(
    clinic_data_name = clinic_data_name,
    max_housenumber_per_city = 100000,
    batch_size = 250,
    output_base_name = "高採樣TWD97匹配"
  ))
}

run_medium_sampling_precision <- function(clinic_data_name = "clinic") {
  cat("⚡ 執行中等採樣精度匹配（5萬筆門牌/城市）...\n\n")
  return(modify_sampling_size(
    clinic_data_name = clinic_data_name,
    max_housenumber_per_city = 50000,
    batch_size = 300,
    output_base_name = "中採樣TWD97匹配"
  ))
}

# 4. 自訂採樣數量執行
custom_sampling_matching <- function(
    clinic_data_name = "clinic", 
    max_housenumber = 50000
) {
  cat("🔧 執行自訂採樣匹配...\n")
  cat("自訂門牌採樣數:", format(max_housenumber, big.mark = ","), "筆/城市\n\n")
  
  return(modify_sampling_size(
    clinic_data_name = clinic_data_name,
    max_housenumber_per_city = max_housenumber,
    output_base_name = paste0("自訂", max_housenumber, "採樣TWD97匹配")
  ))
}

cat("=== 可調控門牌採樣系統載入完成 ===\n\n")

cat("🎯 可用選項:\n")
cat("# 超高精度（無門牌限制，可能需要更多記憶體）\n")
cat("ultra_results <- run_ultra_high_precision(\"clinic\")\n\n")

cat("# 高採樣精度（10萬筆門牌/城市）\n")
cat("high_results <- run_high_sampling_precision(\"clinic\")\n\n")

cat("# 中等採樣精度（5萬筆門牌/城市）\n")
cat("medium_results <- run_medium_sampling_precision(\"clinic\")\n\n")

cat("# 自訂採樣數量\n")
cat("custom_results <- custom_sampling_matching(\"clinic\", max_housenumber = 80000)\n\n")

cat("# 直接指定參數\n")
cat("results <- modify_sampling_size(\n")
cat("  clinic_data_name = \"clinic\",\n")
cat("  max_housenumber_per_city = 100000,  # 10萬筆\n")
cat("  batch_size = 200,                   # 批次大小\n")
cat("  debug_mode = TRUE                   # 開啟除錯\n")
cat(")\n\n")

cat("📊 採樣數量建議:\n")
cat("  🔹 20,000筆 (預設): 平衡效能與精度\n")
cat("  🔹 50,000筆: 提升匹配精度，適中記憶體\n")
cat("  🔹 100,000筆: 高精度，需要較多記憶體\n")
cat("  🔹 無限制: 最高精度，需要大量記憶體\n\n")

cat("⚠️ 記憶體使用提醒:\n")
cat("  - 50,000筆: 約需要12-16GB記憶體\n")
cat("  - 100,000筆: 約需要20-24GB記憶體\n")
cat("  - 無限制: 可能需要32GB以上記憶體\n\n")

cat("🔧 如果遇到記憶體不足:\n")
cat("  1. 減少batch_size (從300降到200或150)\n")
cat("  2. 減少max_housenumber_per_city\n")
cat("  3. 關閉其他應用程式釋放記憶體\n")
cat("  4. 分縣市執行匹配\n")