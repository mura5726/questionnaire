# main4

## Preparation

### read path 

data_path <- paste(getwd(), "/ローデータ_csv", sep = "")
result_path <- paste(getwd(), "/集計結果", sep = "")
ctg_brand_excelname <- "調査カテゴリ_ブランド一覧.xlsx"

### read library 
library(tidyverse)
library(data.table)
library(readxl)
library(xlsx)
library(rJava)
library(rex)

### set parameter
# source("global.R")
value_col <- c("q1_ninti", "q2_pre_ninti", "q3_kyomi_kanshin", "q4_kokan", "q5_riyo_iko", "q6_suisyo_iko")
gnd_age <- c("01teen", "02男性20代", "03男性30代", "04男性40代", "05男性50代", "06男性60代", "07女性20代", "08女性30代", "09女性40代", "10女性50代", "11女性60代")

### define function
# source("ctg_dup.R")
ctg_dup <- function(data, split){
  setwd(data_path)
  gax <- read.csv(data, header = F) %>% as.tibble()
  gax_ctg <- NULL
  for(k in 1:dim(gax)[1]){
    gax_ctg <- c(gax_ctg, (gax[k,2] %>% as.matrix() %>% strsplit(split) %>% unlist())[1])
  }
  d = gax_ctg[!duplicated(gax_ctg)] %>% tibble()
  colnames(d) <- "ctg"
  d
}

## make result data frame

### read ctg_brand.csv & reshape it
setwd(data_path)
# ctg_brand <- read.csv("ctg_brand.csv") %>% as.tibble()
ctg_brand <- read_excel(ctg_brand_excelname)
num_ctg <- substr(as.matrix(ctg_brand[, 1]), 1, 2)
num_brand <- substr(as.matrix(ctg_brand[, 3]), 1, 2)
colnames(num_brand) <- "num_brand"
ctg_brand[, 1] <- substr(as.matrix(ctg_brand[, 1]), 3, nchar(as.matrix(ctg_brand[, 1])))
brand <- substr(as.matrix(ctg_brand[, 3]), 3, nchar(as.matrix(ctg_brand[, 3])))
ctg_brand <- cbind(num_ctg, ctg_brand, brand)
ctg_brand[, 4] <- num_brand
colnames(ctg_brand) <- c("num_ctg", "ctg", "company", "num_brand", "brand")
len_cb <- dim(ctg_brand)[1]

### set gender age file names
data_ga_list <- c("data201711_ga.csv", "data201712_ga.csv") # optional

### dupricated category labels in each monthly files
ga11_ctg_dup <- ctg_dup(data_ga_list[1], "／") 
ga12_ctg_dup <- ctg_dup(data_ga_list[2], "／")
ga_ctg_dup_list <- rbind(ga11_ctg_dup, ga12_ctg_dup)

### read category lables
ga11 <- read.csv(data_ga_list[1], header = F) %>% as.tibble()
ga12 <- read.csv(data_ga_list[2], header = F) %>% as.tibble()
ga_list <- rbind(ga11, ga12)
len_galist <- dim(ga_list)[1]

### split string & extract category names
str_galist <- matrix(0, len_galist, 3)
for(j in 1:3){
  for(i in 1:len_galist){
    str_galist[i, j] <- unlist(strsplit(as.matrix(ga_list[i, 2]),"／"))[j]
  }
}  
ga_list <- cbind(ga_list[, 1], str_galist)
colnames(ga_list) <- c("id", "ctg", "sex", "age")
ga_list <- as.tibble(ga_list)
len_galist <- dim(ga_list)[1]
ncols_galist <- dim(ga_list)[2]
### count rows(sex & age) from each ctg
rowcount_mat <- ga_list %>% group_by(ctg) %>% summarise(count = n())

### make rowcount matrix by month
rowcount_mat <- rowcount_mat %>%
  mutate(ctg = as.character(ctg))
rowcount_mat_11 <- left_join(ga11_ctg_dup, rowcount_mat, by = "ctg")
rowcount_mat_12 <- left_join(ga12_ctg_dup, rowcount_mat, by = "ctg")
rowcount_mat_list <- list(rowcount_mat_11, rowcount_mat_12)

### make keys
ga_ctg_bra <- matrix(0, 20 * len_galist, 4) %>% as.tibble() # 20 is numeber of questions 

accum_rc <- NULL
for(m in 1:8){
  rowcount1 <- (rowcount_mat %>% filter(ctg == (ga_ctg_dup_list %>% as.matrix())[m]))[1,2] %>% as.integer()
  accum_rc <- sum(accum_rc, rowcount1)
  for(l in 1:ncols_galist){
    ga_ctg_bra[((accum_rc - rowcount1) * 20  + 1) : (accum_rc * 20), l] <- rep(as.matrix(ga_list[(accum_rc - rowcount1 + 1) : accum_rc, l]), length =  20 * rowcount1) # attention to arithmetic 
    # print(c((((accum_rc - rowcount1) * 20  + 1) ), (accum_rc * 20), (accum_rc - rowcount1 + 1), accum_rc, 20 * rowcount1))
  }
}
v1 <- NULL
for(k in 1:len_cb){
  rowcount2 <- rowcount_mat[as.matrix(rowcount_mat["ctg"]) == as.matrix(ctg_brand[k, 2])[1], 2] %>% as.integer()
  v1 <- rbind(v1, t(matrix(rep(as.matrix(ctg_brand[k, ]), rowcount2), 5, rowcount2)))
}
keys <- cbind(ga_ctg_bra, v1)[, -2] %>% as.tibble()
colnames(keys) <- c("group", "gender", "age", "num_ctg", "ctg", "company", "num_brand", "brand" )
keys <- keys %>%
  mutate(num_ctg = as.numeric(num_ctg)) %>% 
  mutate(num_brand = as.numeric(num_brand)) %>% ## conver num_ctg and num_brand to numeric
  mutate(gp_br = paste(group, "_", num_brand, sep = "")) ## make col group + num_brand
len_keys <- dim(keys)[1]
value <- matrix(0, len_keys, 6) %>% as.tibble()
colnames(value) <- value_col
result <- keys %>% bind_cols(value)

## set ctg array
v_ctg <- c("a", "b", "c", "d")

## make data list
data_list <- c("data201711.csv", "data201712.csv") # optional
len_dl <- length(data_list)

## sparate keys by month
keys11 <- keys %>% filter(ctg %in% as.vector(as.matrix(ga11_ctg_dup)))
keys12 <- keys %>% filter(ctg %in% as.vector(as.matrix(ga12_ctg_dup)))
keys_list <- list(keys11, keys12) # length(keys_list) == len_dl > TRUE

## sparate result by month
result11 <- result %>% filter(ctg %in% as.vector(as.matrix(ga11_ctg_dup)))
result12 <- result %>% filter(ctg %in% as.vector(as.matrix(ga12_ctg_dup)))
result_list <- list(result11, result12)

## column name as variable
group <- "group"
num_ctg <- "num_ctg" 
num_brand <- "num_brand"
gp_br <- "gp_br"

for(l in 1:len_dl){
  ## read data
  setwd(data_path)
  ex_data <- read.csv(data_list[l]) %>% as.tibble()
  ex_keys <- keys_list[[l]]
  ex_result <- result_list[[l]]
  ex_rowcount_mat <- rowcount_mat_list[[l]]
  cn_data <- ex_data %>% colnames()
  len_id <- (ex_data %>% dim())[1] ### number of id
  len_keys <- (ex_keys %>% dim())[1] ### number of keys
  len_rowcount_mat <- (ex_rowcount_mat %>% dim())[1] ### number of rowcount mat 
  accum_rc3 <- NULL

  ## make vector for rowcount
  brand_times = as.vector(as.matrix(ex_rowcount_mat["count"]))
  cs_brand_times = c(0, cumsum(brand_times))

  ## summarize by colmuns from ex_data
  d1 = 
    ex_data %>% select(group, dplyr::matches(".q1_")) %>% select(-contains("snt")) %>% 
    group_by(group) %>% mutate_all(funs(. <= 3))
  
  d2 =
    ex_data %>% select(group, dplyr::matches(".q2_")) %>% select(-contains("snt")) %>% 
    group_by(group) %>% mutate_all(funs(. >= 9))
  
  d3 =
    ex_data %>% select(group, dplyr::matches(".q3_")) %>% select(-contains("snt")) %>% 
    group_by(group) %>% mutate_all(funs(. <= 2))
  
  d4 =
    ex_data %>% select(group, dplyr::matches(".q4_")) %>% select(-contains("snt")) %>% 
    group_by(group) %>% mutate_all(funs(. <= 2))
  
  d5 =
    ex_data %>% select(group, dplyr::matches(".q5_")) %>% select(-contains("snt")) %>% 
    group_by(group) %>% mutate_all(funs(. <= 2))
  
  d6 =
    ex_data %>% select(group, dplyr::matches(".q6_")) %>% select(-contains("snt")) %>% 
    group_by(group) %>% mutate_all(funs(. <= 2))
  
  
  x1 = 
    apply(d1[,-1], 2, function(v){
      tapply(v, d1$group, sum, na.rm = T)
    }
    )
  
  x2 = 
    apply(d2[,-1], 2, function(v){
      tapply(v, d2$group, sum, na.rm = T)
    }
    )
  
  x3 = 
    apply(d3[,-1], 2, function(v){
      tapply(v, d3$group, sum, na.rm = T)
    }
    )
  
  x4 = 
    apply(d4[,-1], 2, function(v){
      tapply(v, d4$group, sum, na.rm = T)
    }
    )
  
  x5 = 
    apply(d5[,-1], 2, function(v){
      tapply(v, d5$group, sum, na.rm = T)
    }
    )
  
  x6 = 
    apply(d6[,-1], 2, function(v){
      tapply(v, d6$group, sum, na.rm = T)
    }
    )
  
  x = list(x1, x2, x3, x4, x5, x6)
  
  xlist = 
    lapply(x, function(w){
      list(w[(cs_brand_times[1] + 1) : cs_brand_times[2], 1:20], w[(cs_brand_times[2] + 1) : cs_brand_times[3], 21:40],
           w[(cs_brand_times[3] + 1) : cs_brand_times[4], 41:60], w[(cs_brand_times[4] + 1) : cs_brand_times[5], 61:80])
    })
  
  ex_result =
    ex_result %>% mutate(q1_ninti = (unlist(xlist[1]) / 100)) %>% mutate(q2_pre_ninti = (unlist(xlist[2]) / 100)) %>% 
    mutate(q3_kyomi_kanshin = (unlist(xlist[3]) / 100)) %>% mutate(q4_kokan = (unlist(xlist[4]) / 100)) %>% 
    mutate(q5_riyo_iko = (unlist(xlist[5]) / 100)) %>% mutate(q6_suisyo_iko = (unlist(xlist[6]) / 100))

  
  if(l == 1){
    ex_result11 <- ex_result 
  }else if(l == 2){
    ex_result12 <- ex_result
  }else{
    print("コードを書き直しましょう")
  }
}
result_all <- bind_rows(ex_result11, ex_result12)

# write result
setwd(result_path)
print(paste("the result is saved in ", getwd(), sep = ""))
write.csv(result_all, "アンケート集計結果2.csv")

