# 醫事機構地址預處理腳本（修正樓層格式檢測）
# 將完整地址拆分為結構化格式以配對門牌資料

library(dplyr)
library(stringr)
library(readr)

# ====== 設定檔案路徑和欄位名稱 ======
# 請在此處修改為你的實際設定
input_file <- "/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/健保醫事服務機構位置/健保特約_醫學中心_A21030000I-D21001-003.CSV"
address_column <- "地址"
institution_name_column <- "醫事機構名稱"
encoding_type <- "UTF-8"

# ====== 輸出檔案路徑設定 ======
output_directory <- "/Users/alice/Documents/文件 - Alice's的MacBook Pro/53 屆公衛服務隊/53 屆服務隊出隊地圖/健保醫事服務機構位置"
output_file_full_name <- "健保特約_醫學中心_地址解析結果_完整資料.csv"
output_file_address_name <- "健保特約_醫學中心
_僅地址解析.csv"

# 建立完整的輸出檔案路徑
output_file_full <- file.path(output_directory, output_file_full_name)
output_file_address <- file.path(output_directory, output_file_address_name)

# 檢查並建立輸出資料夾
if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)
  cat("已建立輸出資料夾：", output_directory, "\n")
} else {
  cat("輸出資料夾已存在：", output_directory, "\n")
}

# ====== 工具函數 ======

# 全形數字轉換函數（增強版）
convert_fullwidth_numbers <- function(text) {
  fullwidth_nums <- c("０", "１", "２", "３", "４", "５", "６", "７", "８", "９")
  halfwidth_nums <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  
  for (i in seq_along(fullwidth_nums)) {
    text <- str_replace_all(text, fullwidth_nums[i], halfwidth_nums[i])
  }
  
  # 處理各種連字符號，統一為半形連字符
  text <- str_replace_all(text, "[－—−─―]", "-")
  
  # 處理全形句號
  text <- str_replace_all(text, "．", ".")
  
  return(text)
}

# ====== 地址解析函數 ======

# 智能地址分割函數（修正樓層檢測）
smart_address_split <- function(address) {
  # 移除多餘空格並轉換數字
  address <- str_trim(address)
  address <- convert_fullwidth_numbers(address)
  
  cat("=== 開始分析地址 ===\n")
  cat("原始地址:", address, "\n")
  
  # 優先檢查樓層格式（必須先於門牌號分割檢查）
  floor_patterns <- c(
    # 地上+樓層格式（重點修正）
    "^(.+[路街道大道])([0-9]+)號地上([0-9、,，]+)樓$",
    "^(.+[路街道大道])([0-9]+)號地下([0-9、,，]+)樓$",
    "^(.+[路街道大道])([0-9]+)號([0-9、,，]+)樓$",
    # 門牌號+樓層範圍
    "^(.+[路街道大道])([0-9]+)號([0-9]+至[0-9]+樓)$",
    "^(.+[路街道大道])([0-9]+)號([0-9、,，]+至[0-9、,，]+樓)$",
    # 括號內樓層格式
    "^(.+[路街道大道])([0-9-]+)號（[0-9１-９一-十]+樓?至[0-9１-９一-十]+樓）$",
    "^(.+[路街道大道])([0-9-]+)號\\([0-9１-９一-十]+樓?至[0-9１-９一-十]+樓\\)$",
    # 複雜樓層格式（地下至地上）
    "^(.+[路街道大道])([0-9]+)號地下[0-9]+樓至地上[0-9]+樓$"
  )
  
  # 檢查是否為樓層格式
  is_floor_format <- FALSE
  for (pattern in floor_patterns) {
    if (str_detect(address, pattern)) {
      is_floor_format <- TRUE
      cat("  → 檢測到樓層格式，不進行拆分\n")
      break
    }
  }
  
  # 如果是樓層格式，直接返回原地址
  if (is_floor_format) {
    cat("最終結果:", address, "\n")
    cat("=== 分析完成（樓層格式）===\n\n")
    return(c(address))
  }
  
  # 按主要分隔符分割（只有在非樓層格式時才執行）
  main_separators <- c("及", "，", ",", "、", "；", ";")
  address_parts <- c(address)
  
  for (sep in main_separators) {
    if (str_detect(address, fixed(sep))) {
      address_parts <- str_split(address, fixed(sep))[[1]]
      break
    }
  }
  
  address_parts <- str_trim(address_parts)
  address_parts <- address_parts[address_parts != ""]
  
  cat("初步分割:", paste(address_parts, collapse = " | "), "\n")
  
  # 處理每個部分
  final_addresses <- c()
  
  for (i in seq_along(address_parts)) {
    part <- address_parts[i]
    cat("處理部分", i, ":", part, "\n")
    
    # 再次檢查每個部分是否為樓層格式
    part_is_floor_format <- FALSE
    for (pattern in floor_patterns) {
      if (str_detect(part, pattern)) {
        part_is_floor_format <- TRUE
        cat("  → 發現樓層格式（不拆分）\n")
        break
      }
    }
    
    if (part_is_floor_format) {
      final_addresses <- c(final_addresses, part)
      next
    }
    
    # 檢查門牌號範圍格式（如266～274號、266-274號）
    range_pattern <- "^(.+[路街道大道])([0-9]+)[～~-]([0-9]+)號"
    
    # 檢查句號分隔的多門牌號格式（如18.20.22.23號）- 支援全形句號
    dot_separated_pattern <- "^(.+[路街道大道])([0-9.．]+)號$"
    
    # 檢查逗號分隔的多門牌號格式（如2、6號）- 但排除樓層格式
    comma_separated_pattern <- "^(.+[路街道大道])([0-9、,，]+)號$"
    
    # 檢查複雜格式：包含後續門牌號（確保不是樓層）
    complex_pattern1 <- "^(.+[路街道大道])([0-9]+)號([0-9-]+樓)([0-9、,，]+)號$"
    
    if (str_detect(part, comma_separated_pattern) && 
        !str_detect(part, "[0-9]+號[0-9、,，]+樓") &&
        !str_detect(part, "[0-9]+號地上[0-9、,，]+樓") &&
        !str_detect(part, "[0-9]+號地下[0-9、,，]+樓")) {
      cat("  → 發現逗號分隔門牌號格式\n")
      
      # 檢查是否包含樓層資訊（需要特別處理）
      floor_info_match <- str_extract(address, "[，,]\\s*地下[0-9]+層至地上[0-9]+層|[，,]\\s*[0-9]+至[0-9]+樓|[，,]\\s*地上[0-9、,，]+樓")
      
      matches <- str_match(part, comma_separated_pattern)
      
      if (!is.na(matches[1])) {
        road_name <- matches[2]
        numbers_str <- matches[3]
        
        cat("    路名:", road_name, "\n")
        cat("    門牌號字串:", numbers_str, "\n")
        
        # 分割門牌號（按逗號分隔）
        numbers <- str_split(numbers_str, "[、,，]+")[[1]]
        numbers <- str_trim(numbers)
        numbers <- numbers[numbers != ""]
        
        cat("    分離的門牌號:", paste(numbers, collapse = ", "), "\n")
        
        # 為每個門牌號創建地址
        for (num in numbers) {
          if (str_detect(num, "^[0-9]+$")) {
            num_val <- as.numeric(num)
            if (num_val > 0 && num_val <= 9999) {
              addr <- paste0(road_name, num, "號")
              
              # 如果有樓層資訊，添加到每個地址
              if (!is.na(floor_info_match)) {
                floor_clean <- str_replace(floor_info_match, "^[，,]\\s*", "")
                addr <- paste0(addr, floor_clean)
              }
              
              final_addresses <- c(final_addresses, addr)
              cat("    生成地址:", addr, "\n")
            }
          }
        }
      }
      
    } else if (str_detect(part, dot_separated_pattern)) {
      cat("  → 發現句號分隔門牌號格式\n")
      
      matches <- str_match(part, dot_separated_pattern)
      
      if (!is.na(matches[1])) {
        road_name <- matches[2]
        numbers_str <- matches[3]
        
        cat("    路名:", road_name, "\n")
        cat("    門牌號字串:", numbers_str, "\n")
        
        # 分割門牌號（按句號分隔，支援全形和半形句號）
        numbers <- str_split(numbers_str, "[.．]+")[[1]]
        numbers <- str_trim(numbers)
        numbers <- numbers[numbers != ""]
        
        cat("    分離的門牌號:", paste(numbers, collapse = ", "), "\n")
        
        # 檢查是否為複雜的句號分隔格式（超過3個門牌號則標記為需要手動確認）
        if (length(numbers) > 3) {
          cat("    ⚠️  發現複雜句號分隔格式（超過3個門牌號）:", numbers_str, "需要手動確認\n")
        }
        
        # 為每個門牌號創建地址
        for (num in numbers) {
          if (str_detect(num, "^[0-9]+$")) {
            num_val <- as.numeric(num)
            if (num_val > 0 && num_val <= 9999) {
              addr <- paste0(road_name, num, "號")
              final_addresses <- c(final_addresses, addr)
              cat("    生成地址:", addr, "\n")
            }
          }
        }
        
        # 如果是複雜格式，將所有生成的地址標記為需要手動確認
        if (length(numbers) > 3) {
          # 這裡需要在後續處理中標記，因為我們在這個函數中無法直接設定individual address的標記
          cat("    → 此組地址將在後續處理中標記為需要手動確認\n")
        }
      }
      
    } else if (str_detect(part, range_pattern)) {
      cat("  → 發現門牌號範圍格式\n")
      
      matches <- str_match(part, range_pattern)
      
      if (!is.na(matches[1])) {
        road_name <- matches[2]
        start_num <- as.numeric(matches[3])
        end_num <- as.numeric(matches[4])
        
        cat("    路名:", road_name, "\n")
        cat("    起始門牌號:", start_num, "\n")
        cat("    結束門牌號:", end_num, "\n")
        
        # 生成範圍內的所有門牌號
        if (end_num > start_num && (end_num - start_num) <= 20) {
          for (num in start_num:end_num) {
            addr <- paste0(road_name, num, "號")
            final_addresses <- c(final_addresses, addr)
          }
        } else {
          # 範圍太大或異常，保持原格式
          final_addresses <- c(final_addresses, part)
        }
      }
      
    } else if (str_detect(part, complex_pattern1)) {
      cat("  → 發現複雜格式1（樓層+後續門牌）\n")
      
      matches <- str_match(part, complex_pattern1)
      
      if (!is.na(matches[1])) {
        road_name <- matches[2]
        first_number <- matches[3]
        floor_info <- matches[4]
        additional_str <- matches[5]
        
        # 構建第一個地址（含樓層）
        first_addr <- paste0(road_name, first_number, "號", floor_info)
        final_addresses <- c(final_addresses, first_addr)
        
        # 處理後續門牌號
        if (!is.na(additional_str) && nchar(additional_str) > 0) {
          extra_numbers <- str_split(additional_str, "[、,，]+")[[1]]
          extra_numbers <- str_trim(extra_numbers)
          extra_numbers <- extra_numbers[extra_numbers != ""]
          
          for (num in extra_numbers) {
            if (str_detect(num, "^[0-9]+$")) {
              num_val <- as.numeric(num)
              if (num_val > 0 && num_val <= 9999) {
                extra_addr <- paste0(road_name, num, "號")
                final_addresses <- c(final_addresses, extra_addr)
              }
            }
          }
        }
      }
      
    } else {
      cat("  → 一般格式，直接使用\n")
      final_addresses <- c(final_addresses, part)
    }
  }
  
  cat("最終結果:", paste(final_addresses, collapse = " | "), "\n")
  cat("=== 分析完成 ===\n\n")
  
  return(final_addresses)
}

# 單一地址預處理函數
preprocess_single_address <- function(address) {
  # 移除多餘空格和標點符號
  address <- str_trim(address)
  address <- str_replace_all(address, "[，,、；;]", "")
  address <- convert_fullwidth_numbers(address)
  
  # 縣市別提取（增強版，支援桃園縣等舊制縣市）
  county_city <- case_when(
    str_detect(address, "^臺北市|^台北市") ~ "臺北市",
    str_detect(address, "^新北市") ~ "新北市",
    str_detect(address, "^桃園市") ~ "桃園市",
    str_detect(address, "^桃園縣") ~ "桃園縣",
    str_detect(address, "^臺中市|^台中市") ~ "臺中市",
    str_detect(address, "^臺中縣|^台中縣") ~ "臺中縣",
    str_detect(address, "^臺南市|^台南市") ~ "臺南市",
    str_detect(address, "^臺南縣|^台南縣") ~ "臺南縣",
    str_detect(address, "^高雄市") ~ "高雄市",
    str_detect(address, "^高雄縣") ~ "高雄縣",
    str_detect(address, "^基隆市") ~ "基隆市",
    str_detect(address, "^新竹市") ~ "新竹市",
    str_detect(address, "^嘉義市") ~ "嘉義市",
    str_detect(address, "^新竹縣") ~ "新竹縣",
    str_detect(address, "^苗栗縣") ~ "苗栗縣",
    str_detect(address, "^彰化縣") ~ "彰化縣",
    str_detect(address, "^南投縣") ~ "南投縣",
    str_detect(address, "^雲林縣") ~ "雲林縣",
    str_detect(address, "^嘉義縣") ~ "嘉義縣",
    str_detect(address, "^屏東縣") ~ "屏東縣",
    str_detect(address, "^宜蘭縣") ~ "宜蘭縣",
    str_detect(address, "^花蓮縣") ~ "花蓮縣",
    str_detect(address, "^臺東縣|^台東縣") ~ "臺東縣",
    str_detect(address, "^澎湖縣") ~ "澎湖縣",
    str_detect(address, "^金門縣") ~ "金門縣",
    str_detect(address, "^連江縣") ~ "連江縣",
    TRUE ~ NA_character_
  )
  
  # 行政區提取（支援鄉鎮市）
  district_pattern <- paste0(county_city, "(.{1,4}?[區鄉鎮市])")
  district_match <- str_extract(address, district_pattern)
  district <- str_replace(district_match, county_city, "")
  
  # 移除縣市和區的部分
  address_core <- address
  if (!is.na(county_city)) {
    address_core <- str_replace(address_core, paste0("^", str_escape(county_city)), "")
  }
  if (!is.na(district)) {
    address_core <- str_replace(address_core, paste0("^", str_escape(district)), "")
  }
  
  cat("    → 縣市:", ifelse(is.na(county_city), "無", county_city), "\n")
  cat("    → 地區:", ifelse(is.na(district), "無", district), "\n")
  cat("    → 地址核心:", address_core, "\n")
  
  # 提取村里資訊
  village_li <- NA_character_
  neighbor_num <- NA_character_
  
  village_li_pattern <- "^(.{1,4}?[村里])"
  village_li_match <- str_extract(address_core, village_li_pattern)
  if (!is.na(village_li_match)) {
    village_li <- village_li_match
    address_core <- str_replace(address_core, paste0("^", str_escape(village_li_match)), "")
    
    # 檢查鄰的資訊
    neighbor_pattern <- "^([0-9]+鄰)"
    neighbor_match <- str_extract(address_core, neighbor_pattern)
    if (!is.na(neighbor_match)) {
      neighbor_num <- str_extract(neighbor_match, "[0-9]+")
      address_core <- str_replace(address_core, paste0("^", str_escape(neighbor_match)), "")
    }
  }
  
  # 提取路名+段（支援中文數字和阿拉伯數字）
  road_patterns <- c(
    # 中文數字段
    ".+?路[一二三四五六七八九十]+段",
    ".+?大道[一二三四五六七八九十]+段",
    ".+?街[一二三四五六七八九十]+段",
    # 阿拉伯數字段
    ".+?路[0-9]+段",
    ".+?大道[0-9]+段",
    ".+?街[0-9]+段",
    # 無段數的路名
    ".+?路",
    ".+?大道",
    ".+?街",
    ".+?道"
  )
  
  road_full <- NA_character_
  road_base <- NA_character_
  road_section_num <- NA_character_
  
  for (pattern in road_patterns) {
    match <- str_extract(address_core, pattern)
    if (!is.na(match)) {
      road_full <- match
      
      # 分離路名和段數
      if (str_detect(match, "[一二三四五六七八九十]+段$")) {
        # 中文數字段
        road_base <- str_replace(match, "[一二三四五六七八九十]+段$", "")
        road_section_chinese <- str_extract(match, "[一二三四五六七八九十]+(?=段$)")
        road_section_num <- road_section_chinese
      } else if (str_detect(match, "[0-9]+段$")) {
        # 阿拉伯數字段
        road_base <- str_replace(match, "[0-9]+段$", "")
        road_section_num <- str_extract(match, "[0-9]+(?=段$)")
      } else {
        # 無段數
        road_base <- match
        road_section_num <- NA_character_
      }
      break
    }
  }
  
  # 提取巷弄號樓
  remaining_address <- address_core
  if (!is.na(road_full)) {
    remaining_address <- str_replace(address_core, str_escape(road_full), "")
  }
  
  # 提取巷
  lane_match <- str_extract(remaining_address, "[0-9]+巷")
  lane_num <- if (!is.na(lane_match)) str_extract(lane_match, "[0-9]+") else NA_character_
  
  # 提取弄
  alley_match <- str_extract(remaining_address, "[0-9]+弄")
  alley_num <- if (!is.na(alley_match)) str_extract(alley_match, "[0-9]+") else NA_character_
  
  # 提取號（包含「號」字，檢測各種異常格式）
  number_match <- str_extract(remaining_address, "[0-9]+號")
  number_full <- if (!is.na(number_match)) number_match else NA_character_
  manual_check_flag <- FALSE
  
  # 檢查連字符格式（需要手動確認）
  range_number_match <- str_extract(remaining_address, "[0-9]+-[0-9]+號")
  if (!is.na(range_number_match)) {
    number_full <- range_number_match
    manual_check_flag <- TRUE
    cat("    ⚠️  發現連字符格式:", range_number_match, "需要手動確認\n")
  }
  
  # 檢查波浪號範圍格式（需要手動確認）
  wave_range_match <- str_extract(remaining_address, "[0-9]+[～~][0-9]+號")
  if (!is.na(wave_range_match)) {
    number_full <- wave_range_match
    manual_check_flag <- TRUE
    cat("    ⚠️  發現範圍格式:", wave_range_match, "需要手動確認\n")
  }
  
  # 檢查中文數字與阿拉伯數字混合格式（需要手動確認）
  mixed_number_patterns <- c(
    "[一二三四五六七八九十百千萬][0-9０-９]+號",
    "[0-9０-９]+[一二三四五六七八九十百千萬]+號",
    "[一二三四五六七八九十百千萬]+[0-9０-９]+[一二三四五六七八九十百千萬]+號"
  )
  
  for (pattern in mixed_number_patterns) {
    mixed_match <- str_extract(remaining_address, pattern)
    if (!is.na(mixed_match)) {
      number_full <- mixed_match
      manual_check_flag <- TRUE
      cat("    ⚠️  發現中文阿拉伯數字混合格式:", mixed_match, "需要手動確認\n")
      break
    }
  }
  
  # 檢查純中文數字格式（也需要手動確認轉換）
  chinese_number_match <- str_extract(remaining_address, "[一二三四五六七八九十百千萬]+號")
  if (!is.na(chinese_number_match) && is.na(number_full)) {
    number_full <- chinese_number_match
    manual_check_flag <- TRUE
    cat("    ⚠️  發現純中文數字格式:", chinese_number_match, "需要手動確認\n")
  }
  
  # 如果還是沒找到，謹慎地找數字
  if (is.na(number_full)) {
    temp_addr <- str_replace_all(remaining_address, "[0-9]+-[0-9]+至[0-9]+-[0-9]+樓|[0-9]+至[0-9]+樓|[0-9]+-[0-9]+樓|[0-9]+樓|[B][0-9]*", "")
    if (!is.na(lane_num)) {
      temp_addr <- str_replace(temp_addr, paste0(lane_num, "巷"), "")
    }
    if (!is.na(alley_num)) {
      temp_addr <- str_replace(temp_addr, paste0(alley_num, "弄"), "")
    }
    
    first_number <- str_extract(temp_addr, "^[0-9]+")
    if (!is.na(first_number)) {
      num_value <- as.numeric(first_number)
      if (num_value <= 9999) {
        number_full <- paste0(first_number, "號")
      }
    }
  }
  
  # 樓層提取（支援各種複雜格式，包括括號格式和地上樓層）
  floor_patterns <- c(
    # 地上樓層格式（新增）
    "地上[0-9、,，]+樓",
    "地下[0-9、,，]+樓",
    # 括號內樓層格式
    "（[0-9１-９一-十]+樓至[0-9１-９一-十]+樓）",
    "\\([0-9１-９一-十]+樓至[0-9１-９一-十]+樓\\)",
    "（[0-9１-９一-十]+至[0-9１-９一-十]+樓）",
    "\\([0-9１-９一-十]+至[0-9１-９一-十]+樓\\)",
    "（地上[0-9、,，]+樓）",
    "\\(地上[0-9、,，]+樓\\)",
    # 複雜樓層格式（需要手動確認）
    "地下[0-9]+樓至地上[0-9]+樓",
    "[0-9]+-[0-9]+至[0-9]+-[0-9]+樓",
    "[0-9]+-[0-9]+樓",
    "[0-9]+至[0-9]+樓",
    "[0-9、,，]+樓",
    "[0-9]+樓",
    "[B][0-9]*"
  )
  
  floor <- NA_character_
  floor_needs_check <- FALSE
  
  for (pattern in floor_patterns) {
    floor_match <- str_extract(remaining_address, pattern)
    if (!is.na(floor_match)) {
      floor <- floor_match
      
      # 檢查是否為需要手動確認的樓層格式
      needs_manual_check_patterns <- c(
        "地下.+至地上.+樓",           # 地下至地上樓層
        "地上[0-9、,，]+樓",          # 地上多樓層格式（新增）
        "地下[0-9、,，]+樓",          # 地下多樓層格式（新增）
        "[0-9、,，]+樓",              # 多樓層格式（如1、2、6、8、9樓）
        "[0-9]+至[0-9]+樓",          # 樓層範圍格式
        "[0-9]+-[0-9]+樓"            # 連字符樓層格式
      )
      
      for (check_pattern in needs_manual_check_patterns) {
        if (str_detect(floor_match, check_pattern)) {
          floor_needs_check <- TRUE
          manual_check_flag <- TRUE
          cat("    ⚠️  發現複雜樓層格式:", floor_match, "需要手動確認\n")
          break
        }
      }
      
      cat("    → 找到樓層格式:", floor, "\n")
      break
    }
  }
  
  # 構建街_路段欄位
  street_segment <- paste0(
    ifelse(is.na(road_base), "", road_base),
    ifelse(is.na(road_section_num), "", paste0(road_section_num, "段"))
  )
  
  # 返回結果
  return(data.frame(
    村里 = village_li,
    鄰 = neighbor_num,
    街_路段 = street_segment,
    地區 = district,
    巷 = lane_num,
    弄 = alley_num,
    號 = number_full,
    縣市 = county_city,
    樓 = floor,
    原始地址片段 = address,
    需要手動確認 = manual_check_flag,
    stringsAsFactors = FALSE
  ))
}

# 多地址處理函數
preprocess_multiple_addresses <- function(address, row_id, institution_name) {
  cat("=== 處理機構：", institution_name, "===\n")
  
  # 使用智能分割函數
  address_parts <- smart_address_split(address)
  
  # 檢查是否為複雜句號分隔格式（用於後續標記）
  original_address_converted <- convert_fullwidth_numbers(address)
  is_complex_dot_format <- str_detect(original_address_converted, "[0-9.．]+號$") && 
    length(str_split(str_extract(original_address_converted, "[0-9.．]+(?=號$)"), "[.．]+")[[1]]) > 3
  
  # 處理每個地址
  results <- list()
  base_info <- list(縣市 = NA, 地區 = NA, 村里 = NA, 鄰 = NA, 路名 = NA, 段數 = NA)
  
  for (i in seq_along(address_parts)) {
    single_address <- address_parts[i]
    
    processed <- preprocess_single_address(single_address)
    
    # 如果是複雜句號分隔格式，標記為需要手動確認
    if (is_complex_dot_format) {
      processed$需要手動確認 <- TRUE
      cat("    ⚠️  複雜句號分隔格式，標記為需要手動確認\n")
    }
    
    if (i == 1) {
      # 記錄基本資訊
      if (!is.na(processed$縣市)) base_info$縣市 <- processed$縣市
      if (!is.na(processed$地區)) base_info$地區 <- processed$地區
      if (!is.na(processed$村里)) base_info$村里 <- processed$村里
      if (!is.na(processed$鄰)) base_info$鄰 <- processed$鄰
      
      street_segment <- processed$街_路段
      if (!is.na(street_segment) && street_segment != "") {
        if (str_detect(street_segment, "[0-9]+段$")) {
          base_info$路名 <- str_replace(street_segment, "[0-9]+段$", "")
          base_info$段數 <- str_extract(street_segment, "[0-9]+(?=段$)")
        } else {
          base_info$路名 <- street_segment
          base_info$段數 <- NA
        }
      }
    } else {
      # 補全缺失資訊（改進多縣市地址邏輯）
      
      # 檢查當前地址是否已經包含完整的縣市資訊
      has_complete_county_info <- !is.na(processed$縣市) && !is.na(processed$地區)
      
      if (has_complete_county_info) {
        cat("    → 發現完整縣市資訊，不沿用前一個地址的縣市\n")
        cat("      縣市:", processed$縣市, "地區:", processed$地區, "\n")
        # 不沿用，保持當前解析的縣市和地區
      } else {
        # 只在沒有完整縣市資訊時才沿用
        if (is.na(processed$縣市) && !is.na(base_info$縣市)) {
          processed$縣市 <- base_info$縣市
          cat("    → 沿用縣市:", base_info$縣市, "\n")
        }
        
        if (is.na(processed$地區) && !is.na(base_info$地區)) {
          processed$地區 <- base_info$地區
          cat("    → 沿用地區:", base_info$地區, "\n")
        }
        
        # 村里沿用邏輯
        if (is.na(processed$村里) && !is.na(base_info$村里)) {
          processed$村里 <- base_info$村里
          cat("    → 沿用村里:", base_info$村里, "\n")
        }
        
        # 鄰沿用邏輯
        if (is.na(processed$鄰) && !is.na(base_info$鄰)) {
          processed$鄰 <- base_info$鄰
          cat("    → 沿用鄰:", base_info$鄰, "\n")
        }
      }
      
      # 改進的路段沿用邏輯（只在沒有完整地址時沿用）
      if ((is.na(processed$街_路段) || processed$街_路段 == "") && 
          !str_detect(single_address, "[路街道大道]") &&
          !has_complete_county_info) {
        processed$街_路段 <- paste0(
          ifelse(is.na(base_info$路名), "", base_info$路名),
          ifelse(is.na(base_info$段數), "", paste0(base_info$段數, "段"))
        )
        cat("    → 沿用路段:", processed$街_路段, "\n")
      }
      
      # 巷的沿用邏輯（只在沒有完整地址時沿用）
      if (is.na(processed$巷) && !is.na(results[[1]]$巷) && 
          !str_detect(single_address, "[路街道大道]") &&
          !str_detect(single_address, "[巷弄]") &&
          !has_complete_county_info) {
        
        # 檢查是否為純門牌號格式
        if (str_detect(single_address, "^[0-9]+號?$")) {
          processed$巷 <- results[[1]]$巷
          cat("    → 沿用巷:", processed$巷, "\n")
        }
      }
    }
    
    # 添加序號和統計資訊
    processed$地址序號 <- i
    processed$總地址數 <- length(address_parts)
    processed$機構列號 <- row_id
    processed$機構名稱 <- institution_name
    
    results[[i]] <- processed
  }
  
  return(do.call(rbind, results))
}

# ====== 測試函數 ======

# 測試樓層格式不被錯誤拆分
test_floor_vs_address_splitting <- function() {
  cat("=== 測試樓層格式 vs 地址拆分 ===\n")
  
  test_cases <- c(
    # 應該保持為單一地址（樓層格式）
    "新北市新莊區思源路２號１、４、５、６、７樓",
    "臺南市中西區金華路三段167號1、2、3、4樓", 
    "台北市大安區復興南路二段100號2、3、4樓",
    "高雄市路竹區北嶺六路100號地上1、2、6、8、9樓",  # 重點測試案例
    
    # 應該拆分為多個地址（多門牌號）
    "新北市三重區重新路２段２、６號，地下１層至地上９層",
    "臺南市民生路二段18.20.22.23號",
    "台北市大安區復興南路二段100號及102號"
  )
  
  for (i in seq_along(test_cases)) {
    cat("\n--- 測試案例", i, "---\n")
    test_address <- test_cases[i]
    cat("測試地址：", test_address, "\n")
    
    # 判斷預期結果
    is_floor_format <- str_detect(test_address, "[0-9]+號地上[0-9、,，]+樓$") ||
      str_detect(test_address, "[0-9]+號地下[0-9、,，]+樓$") ||
      str_detect(test_address, "[0-9]+號[0-9、,，]+樓$")
    expected_single_address <- is_floor_format
    
    cat("預期結果：", ifelse(expected_single_address, "單一地址（樓層格式）", "多個地址（門牌號拆分）"), "\n")
    
    # 測試分割結果
    parts <- smart_address_split(test_address)
    cat("分割後地址數量：", length(parts), "\n")
    
    # 驗證結果
    if (expected_single_address && length(parts) == 1) {
      cat("✓ 正確：樓層格式保持為單一地址\n")
    } else if (!expected_single_address && length(parts) > 1) {
      cat("✓ 正確：多門牌號成功拆分\n")
    } else {
      cat("❌ 錯誤：處理結果不符預期\n")
    }
    
    # 顯示分割結果
    for (j in seq_along(parts)) {
      cat("  地址", j, ":", parts[j], "\n")
    }
    
    # 如果是樓層格式，測試樓層解析
    if (expected_single_address && length(parts) == 1) {
      result <- preprocess_single_address(parts[1])
      cat("  解析結果 - 號:", ifelse(is.na(result$號), "無", result$號), "\n")
      cat("  解析結果 - 樓:", ifelse(is.na(result$樓), "無", result$樓), "\n")
      cat("  需要手動確認:", ifelse(is.na(result$需要手動確認), "FALSE", result$需要手動確認), "\n")
    }
  }
  
  cat("\n=== 測試完成 ===\n")
}

# ====== 主要處理流程 ======

# 執行測試
cat("=== 開始測試 ===\n")
test_floor_vs_address_splitting()

# 讀取醫事機構資料
cat("\n=== 讀取資料檔案 ===\n")
cat("正在讀取檔案：", input_file, "\n")
medical_data <- read_csv(input_file, locale = locale(encoding = encoding_type))

# 檢查欄位
cat("檢查欄位...\n")
cat("檔案中的欄位名稱：\n")
print(names(medical_data))

if (!address_column %in% names(medical_data)) {
  stop("錯誤：找不到地址欄位 '", address_column, "'")
}

if (!institution_name_column %in% names(medical_data)) {
  stop("錯誤：找不到機構名稱欄位 '", institution_name_column, "'")
}

cat("✓ 地址欄位：", address_column, "\n")
cat("✓ 機構名稱欄位：", institution_name_column, "\n")
cat("✓ 總資料筆數：", nrow(medical_data), "\n")

# 批次處理所有地址
cat("\n=== 批次處理地址 ===\n")
all_results <- list()

for (i in seq_len(nrow(medical_data))) {
  address <- medical_data[[address_column]][i]
  institution_name <- medical_data[[institution_name_column]][i]
  
  if (!is.na(address) && address != "") {
    result <- preprocess_multiple_addresses(address, i, institution_name)
    all_results[[i]] <- result
    
    if (i %% 10 == 0) {
      cat("已處理", i, "/", nrow(medical_data), "筆資料\n")
    }
  }
}

# 合併所有結果
cat("合併處理結果...\n")
processed_addresses <- do.call(rbind, all_results)

# 與原始資料合併
cat("與原始資料合併...\n")
final_data <- medical_data[processed_addresses$機構列號, ] %>%
  bind_cols(processed_addresses %>% select(-機構列號, -機構名稱))

# 建立標準化地址
final_data <- final_data %>%
  mutate(
    標準化地址 = paste0(
      ifelse(is.na(縣市), "", 縣市),
      ifelse(is.na(地區), "", 地區),
      ifelse(is.na(街_路段), "", 街_路段),
      ifelse(is.na(巷), "", paste0(巷, "巷")),
      ifelse(is.na(弄), "", paste0(弄, "弄")),
      ifelse(is.na(號), "", 號)
    ),
    完整地址標識 = paste0(.[[institution_name_column]], "_地址", 地址序號),
    門牌比對地址 = paste0(
      ifelse(is.na(巷), "", paste0(巷, "巷")),
      ifelse(is.na(弄), "", paste0(弄, "弄")),
      ifelse(is.na(號), "", 號)
    )
  )

# ====== 輸出結果 ======

# 儲存完整資料
write_csv(final_data, output_file_full, na = "")
cat("完整資料已儲存為：", output_file_full, "\n")

# 儲存地址專用資料
address_only_data <- final_data %>%
  select(
    all_of(institution_name_column),
    原始地址片段,
    地址序號,
    總地址數,
    縣市,
    地區,
    街_路段,
    村里,
    鄰,
    巷,
    弄,
    號,
    樓,
    需要手動確認,
    標準化地址,
    完整地址標識,
    門牌比對地址
  )

write_csv(address_only_data, output_file_address, na = "")
cat("地址專用資料已儲存為：", output_file_address, "\n")

# ====== 統計報告 ======

cat("\n=== 處理統計 ===\n")
cat("原始機構數：", length(unique(final_data[[institution_name_column]])), "\n")
cat("處理後總筆數：", nrow(final_data), "\n")
cat("多地址機構數：", sum(final_data$總地址數 > 1), "\n")
cat("成功提取縣市：", sum(!is.na(final_data$縣市)), "\n")
cat("成功提取地區：", sum(!is.na(final_data$地區)), "\n")
cat("成功提取街_路段：", sum(!is.na(final_data$街_路段) & final_data$街_路段 != ""), "\n")
cat("成功提取門牌號：", sum(!is.na(final_data$號)), "\n")
cat("成功提取樓層：", sum(!is.na(final_data$樓)), "\n")
cat("需要手動確認的筆數：", sum(final_data$需要手動確認, na.rm = TRUE), "\n")

# 顯示需要手動確認的案例（擴展說明）
manual_check_cases <- final_data %>%
  filter(需要手動確認 == TRUE) %>%
  select(all_of(institution_name_column), 原始地址片段, 號, 樓, 標準化地址)

if(nrow(manual_check_cases) > 0) {
  cat("\n=== 需要手動確認的地址格式 ===\n")
  cat("以下地址包含特殊格式，請確認正確的信息：\n")
  print(manual_check_cases)
  cat("\n說明：\n")
  cat("- 連字符格式「42-50號」：可能是「42之50號」或「42號到50號」\n")
  cat("- 範圍格式「266～274號」：可能是連續門牌號\n")
  cat("- 中文數字混合「三五０號」：應該是「350號」\n")
  cat("- 純中文數字「三百五十號」：應該轉換為阿拉伯數字\n")
  cat("- 複雜樓層格式「地下1樓至地上13樓」：需要確認樓層範圍\n")
  cat("- 地上多樓層格式「地上1、2、6、8、9樓」：需要確認具體樓層\n")
  cat("- 多樓層格式「1、2、3、4樓」：需要確認具體樓層\n")
} else {
  cat("\n✓ 沒有需要手動確認的特殊格式！\n")
}

# 顯示多地址案例
multi_address_cases <- final_data %>%
  filter(總地址數 > 1) %>%
  arrange(.data[[institution_name_column]], 地址序號) %>%
  select(all_of(institution_name_column), 地址序號, 總地址數, 原始地址片段, 標準化地址)

if(nrow(multi_address_cases) > 0) {
  cat("\n=== 多地址案例（前10筆）===\n")
  print(head(multi_address_cases, 10))
}

# 顯示處理失敗的案例
failed_cases <- final_data %>%
  filter(is.na(縣市) | is.na(地區) | is.na(號)) %>%
  select(all_of(institution_name_column), 原始地址片段, 標準化地址)

if(nrow(failed_cases) > 0) {
  cat("\n=== 需要手動檢查的地址 ===\n")
  print(failed_cases)
} else {
  cat("\n✓ 所有地址都成功解析！\n")
}

cat("\n=== 處理完成 ===\n")
cat("1. 完整資料檔案：", output_file_full, "\n")
cat("2. 地址專用檔案：", output_file_address, "\n")
cat("3. 輸出資料夾：", output_directory, "\n")