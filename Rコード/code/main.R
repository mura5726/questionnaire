# main

## read path 
# source("\\\\center/Client/Prospects/Quaras/05_data/09_集計/20_アンケートデータ集計/Rコード/code/")
code_path = "\\\\center/Client/Prospects/Quaras/05_data/09_集計/20_アンケートデータ集計/Rコード/code"
data_path = "\\\\center/Client/Prospects/Quaras/05_data/09_集計/20_アンケートデータ集計"


## library 
setwd(code_path)
source("library.R")

## set parameter
source("global.R") 
gnd_age = c("01teen", "02男性20代", "03男性30代", "04男性40代", "05男性50代", "06男性60代", "07女性20代", "08女性30代", "09女性40代", "10女性50代", "11女性60代") # this vector cannot be defined from the source

## define function
source("cuans.R")

## make result data frame
setwd(data_path)
ctg_brand = read.csv("ctg_brand.csv") %>% as.tibble()
len_cb = dim(ctg_brand)[1]
len_ga = length(gnd_age) # gnd_age : defined at global.R
ga_ctg_bra = matrix(0, len_cb * len_ga, 4) %>% as.tibble()

ga_ctg_bra[ ,1] = rep(gnd_age, length = len_ga * len_cb)
ga_ctg_bra[ ,2] = rep(as.matrix(ctg_brand[, 1]), 11)
ga_ctg_bra[ ,3] = rep(as.matrix(ctg_brand[, 2]), 11)
ga_ctg_bra[ ,4] = rep(as.matrix(ctg_brand[, 3]), 11)

value = matrix(0 ,dim(ga_ctg_bra)[1], 6) %>% as.tibble()
colnames(value) = value_col

## set ctg vector
v_ctg = c("a", "b", "c", "d")

## make data list
data_list = c("data201711.csv", "data201712.csv")
len_dl = length(data_list)

for(l in 1:len_dl){
  
  ## read data
  setwd(data_path)
  data = read.csv(data_list[l]) %>% as.tibble()
  cn_data = data %>% colnames()
  len_id = (data %>% dim())[1] ### id number

  for(k in 1:len_ga){
    
    # extra rows by group
    # data["group"] = (data["group"] %% len_ga) + 1
    data = data %>% filter(group == k)
    
    for(j in 1:4){
      
      ## loop each ctg
      ctg = v_ctg[j]
      switch(ctg,
        "a" =  for(q in 1:6){
          for(i in 1:20){
            ex_col = cn_data == paste(ctg, "q", q, "_", i, sep = "")
            d = data[ex_col] %>% na.omit()
            value[i + ((j - 1) * 20) + ((k - 1) * len_ga) + ((l - 1) * 80), q] = cuans(q) # cuans() : defined function from source
          }
          # print(i + ((j - 1) * 20) + ((k - 1) * len_ga) + ((l - 1) * 80))
          print(c(ctg ,q ,i, k, l))
        },
        "b" =  for(q in 1:6){
          for(i in 1:20){
            ex_col = cn_data == paste(ctg, "q", q, "_", i, sep = "")
            d = data[ex_col] %>% na.omit()
            value[i + ((j - 1) * 20) + ((k - 1) * len_ga) + ((l - 1) * 80), q] = cuans(q) # cuans() : defined function from source
          }
          # print(i + ((j - 1) * 20) + ((k - 1) * len_ga) + ((l - 1) * 80))
          print(c(ctg ,q , i, k, l))
        },
        "c" =  for(q in 1:6){
          for(i in 1:20){
            ex_col = cn_data == paste(ctg, "q", q, "_", i, sep = "")
            d = data[ex_col] %>% na.omit()
            value[i + ((j - 1) * 20) + ((k - 1) * len_ga) + ((l - 1) * 80), q] = cuans(q) # cuans() : defined function from source
          }
          # print(i + ((j - 1) * 20) + ((k - 1) * len_ga) + ((l - 1) * 80))
          print(c(ctg ,q ,i, k, l))
        },
        "d" =  for(q in 1:6){
          for(i in 1:20){
            ex_col = cn_data == paste(ctg, "q", q, "_", i, sep = "")
            d = data[ex_col] %>% na.omit()
            value[i + ((j - 1) * 20) + ((k - 1) * len_ga) + ((l - 1) * 80), q] = cuans(q) # cuans() : defined function from source
          }
          # print(i + ((j - 1) * 20) + ((k - 1) * len_ga) + ((l - 1) * 80))
          print(c(ctg ,q ,i, k, l))
        }
      )  
    }
  }
}
## write result
result = ga_ctg_bra %>% bind_cols(value)
cn_reslt = result %>% colnames()
write.csv(result, "result.csv")
