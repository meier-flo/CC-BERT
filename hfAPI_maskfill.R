call_hf_inference_maskfill<-function(prompt_with_mask){
token<-"INSERT HUGGING FACE TOKEN HERE"  
  require(tidyverse)
  require(httr2)
  
content <- "https://api-inference.huggingface.co/models/Maltehb/danish-bert-botxo" %>% 

request() %>% 
    req_auth_bearer_token(token) %>% 
    req_body_json(list(
      inputs = prompt_with_mask
    )) %>%
  req_throttle(rate = 10 / 60)%>%
  req_retry(is_transient = \(content) resp_status(content) %in% c(429, 500, 503),
            max_seconds = 6000)%>%
  req_perform()
  #count <- 0
  print(resp_status(content))
  
#while(resp_status(content) %in% c(429, 500, 503)){
#  print('ENTER WHILE')
#    content <- "https://api-inference.huggingface.co/models/Maltehb/danish-bert-botxo" %>% 
#      request() %>% 
#      req_auth_bearer_token(token) %>% 
#      req_body_json(list(
#        inputs = prompt_with_mask
#      )) %>%  req_perform()
#    print('Trying....')
#    count <- count+1
#    Sys.sleep(count*5)
#  }
  
result<-content%>%resp_body_json(simplifyVector = TRUE)
print('Call successfull!')
return(result)
}


