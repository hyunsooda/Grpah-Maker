args = commandArgs(trailingOnly=TRUE)

#gameUrl = 'http://news.naver.com/main/list.nhn?mode=LS2D&mid=shm&sid1=105&sid2=229'
#computerUrl = 'http://news.naver.com/main/list.nhn?mode=LS2D&mid=shm&sid1=105&sid2=283'
#snsUrl = 'http://news.naver.com/main/list.nhn?mode=LS2D&mid=shm&sid1=105&sid2=226'
#ITUrl = 'http://news.naver.com/main/list.nhn?mode=LS2D&mid=shm&sid1=105&sid2=230'
#scienceUrl = 'http://news.naver.com/main/list.nhn?mode=LSD&mid=sec&sid1=105'
#socialUrl = 'http://news.naver.com/main/list.nhn?mode=LSD&mid=sec&sid1=102'
#ecnoomy = 'http://news.naver.com/main/list.nhn?mode=LSD&mid=sec&sid1=101'


URLS = c('http://news.naver.com/main/list.nhn?mode=LS2D&mid=shm&sid1=105&sid2=229',
            'http://news.naver.com/main/list.nhn?mode=LS2D&mid=shm&sid1=105&sid2=283',
            'http://news.naver.com/main/list.nhn?mode=LS2D&mid=shm&sid1=105&sid2=226',
            'http://news.naver.com/main/list.nhn?mode=LS2D&mid=shm&sid1=105&sid2=230',
            'http://news.naver.com/main/list.nhn?mode=LSD&mid=sec&sid1=105',
            'http://news.naver.com/main/list.nhn?mode=LSD&mid=sec&sid1=102',
            'http://news.naver.com/main/list.nhn?mode=LSD&mid=sec&sid1=101')


library(rvest)
library(dplyr)

html = read_html(URLS[as.numeric(args[1])])
doc = html_nodes(html, css = '.type06_headline') %>%
  html_nodes('dt') %>%
  html_nodes('a') %>%
  html_text()

links = html_nodes(html, css = '.type06_headline') %>%
  html_nodes('dt') %>%
  html_nodes('a') %>%
  html_attr('href')
links = unique(links)

company = html_nodes(html, css = '.type06_headline') %>%
  html_nodes(css = '.writing') %>%
  html_text()

for(i in 1:length(company)) {
  company[i] = paste0('[',company[i], ':')
}
companyCnt = 1

newdate = html_nodes(html, css = '.type06_headline') %>%
  html_nodes(css = '.is_new') %>%
  html_text()
newdate = gsub('\t','',newdate)
newdateCnt = 1

outdate = html_nodes(html, css = '.type06_headline') %>%
  html_nodes(css = '.is_outdated') %>%
  html_text()
outdate = gsub('\t','',outdate)
outdateCnt = 1


#doc = doc[2:length(doc)]
doc = strsplit(doc, " ")
arr = c('')

cnt = 1
flag = 0
for(i in 1:length(doc)) {
  for(j in 1:length(doc[[i]])) {
    where = regexpr('\r', doc[[i]][j])
    if(regexpr('동영상기사', doc[[i]][j]) < 0 ) {
      if(regexpr('\"', doc[[i]][j]) > 0) {
        doc[[i]][j] = gsub('\"','',doc[[i]][j])
        doc[[i]][j] = gsub('\r','',doc[[i]][j])
        doc[[i]][j] = gsub('\n','',doc[[i]][j])
        doc[[i]][j] = gsub('\t','',doc[[i]][j])
        #doc[[i]][j] = substr(doc[[i]][j], 2, nchar(doc[[i]][j]))
      }
      if(where > 0 && where != 1) {
        if(is.na(arr[cnt]))
          arr[cnt] = ''
        arr[cnt] = paste(arr[cnt], substr(doc[[i]][j], 1, where - 1))
        flag = 1
      }
      else if(where != 1){
        if(is.na(arr[cnt])) 
          arr[cnt] = doc[[i]][j]
        else 
          arr[cnt] = paste(arr[cnt], doc[[i]][j], sep=' ')
        flag = 1
      }  
    }
  }
  if(flag) {
    arr[cnt] = paste(arr[cnt], company[companyCnt])
    if(!is.na(newdate[newdateCnt])) {
      newdate[newdateCnt] = paste0(newdate[newdateCnt], ']')
      arr[cnt] = paste(arr[cnt], newdate[newdateCnt])
      arr[cnt] = gsub('"','',arr[cnt])
      arr[cnt] = paste(arr[cnt], links[cnt])
      newdateCnt = newdateCnt + 1
    } else {
      outdate[outdateCnt] = paste0(outdate[outdateCnt], ']')
      arr[cnt] = paste(arr[cnt], outdate[outdateCnt])
      arr[cnt] = gsub('"','',arr[cnt])
      arr[cnt] = paste(arr[cnt], links[cnt])
      outdateCnt = outdateCnt + 1
    }
    cnt = cnt+1
    companyCnt = companyCnt+1
    flag = 0
  }
}

arr

