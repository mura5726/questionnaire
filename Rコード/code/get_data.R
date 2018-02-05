get_data = function(ex_result, q, exc, gp, i){
  switch(q,
         
         if(dim(exc)[1] == 0){
           ex_result <- ex_result %>% 
             mutate(q1_ninti = if_else(gp_br == as.factor(paste(gp, "_", i, sep = "")), as.double(NA), q1_ninti))
           # print(ex_result)
         }else{
           ex_result <- ex_result %>%
             mutate(q1_ninti = if_else(gp_br == as.factor(paste(gp, "_", i, sep = "")), as.double(cuans(q, exc) / 100), q1_ninti))
           # print(ex_result)
         },
         
         if(dim(exc)[1] == 0){
           ex_result <- ex_result %>%
             mutate(q2_pre_ninti = if_else(gp_br == as.factor(paste(gp, "_", i, sep = "")), as.double(NA), q2_pre_ninti))
           # print(ex_result)
         }else{
           ex_result <- ex_result %>%
             mutate(q2_pre_ninti = if_else(gp_br == as.factor(paste(gp, "_", i, sep = "")), as.double(cuans(q, exc) / 100), q2_pre_ninti))
           # print(ex_result)
         },
         
         if(dim(exc)[1] == 0){
           ex_result <- ex_result %>% 
             mutate(q3_kyomi_kanshin = if_else(gp_br == as.factor(paste(gp, "_", i, sep = "")), as.double(NA), q3_kyomi_kanshin))
         }else{
           ex_result <- ex_result %>% 
             mutate(q3_kyomi_kanshin = if_else(gp_br == as.factor(paste(gp, "_", i, sep = "")), as.double(cuans(q, exc) / 100), q3_kyomi_kanshin))
         },
         
         if(dim(exc)[1] == 0){
           ex_result <- ex_result %>% 
             mutate(q4_kokan = if_else(gp_br == as.factor(paste(gp, "_", i, sep = "")), as.double(NA), q4_kokan))
         }else{
           ex_result <- ex_result %>% 
             mutate(q4_kokan = if_else(gp_br == as.factor(paste(gp, "_", i, sep = "")), as.double(cuans(q, exc) / 100), q4_kokan))
         },
         
         if(dim(exc)[1] == 0){
           ex_result <- ex_result %>% 
             mutate(q5_riyo_iko = if_else(gp_br == as.factor(paste(gp, "_", i, sep = "")), as.double(NA), q5_riyo_iko))
         }else{
           ex_result <- ex_result %>% 
             mutate(q5_riyo_iko = if_else(gp_br == as.factor(paste(gp, "_", i, sep = "")), as.double(cuans(q, exc) / 100), q5_riyo_iko))
         },
         
         if(dim(exc)[1] == 0){
           ex_result <- ex_result %>% 
             mutate(q6_suisyo_iko = if_else(gp_br == as.factor(paste(gp, "_", i, sep = "")), as.double(NA), q6_suisyo_iko))
         }else{
           ex_result <- ex_result %>% 
             mutate(q6_suisyo_iko = if_else(gp_br == as.factor(paste(gp, "_", i, sep = "")), as.double(cuans(q, exc) / 100), q6_suisyo_iko))
         }
  )
}
