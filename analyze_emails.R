library(rjson)
library(stringr)
library(lubridate)
library(plyr)
library(ggplot2)
library(gridExtra)
library(tm)
library(wordcloud)
library(slam)
library(NLP)
library(openNLP)
library(caret)
library(topicmodels)
library(qdap)
options(stringsAsFactors = F)

sent = fromJSON(file = 'sent.json')
table(unlist(lapply(sent, length))) #all 4
sent_na = lapply(sent, lapply, function(x) ifelse(is.null(x), NA, x))

sent_df = data.frame(matrix(unlist(sent_na), byrow = T, nrow = length(sent_na)))
names(sent_df) = names(sent[[1]])
save(sent_df, file = 'sent.RData')

#extract year
sent_df$yr = year(ymd_hms(sent_df$ts))

#plot email count per year
msg_yr = ddply(sent_df, .(yr), summarize, msg_count = length(body))
p_msg_yr = 
  ggplot(msg_yr, aes(x = yr, y = msg_count)) +
  geom_bar(stat = 'identity', fill = '#74c476') +
  ggtitle('Number of emails per year') +
  scale_x_continuous(breaks = 2008:2014, name = 'Year') +
  scale_y_continuous(name = 'Number of emails')

ggsave(p_msg_yr, file = 'msg_yr.jpeg')

#extract email addresses from dest
sent_df$dest_email = str_extract_all(sent_df$dest, '[[:alnum:].]+@[[:alnum:].]+')

#count the number of recepients in each email
l = sapply(sent_df$dest_email, length)

#to analyze the recepients year each, need to first duplicate the year field accordingly
yr_d = rep(sent_df$yr, l)

#now combine the duplicated year with dest email
dest_yr = data.frame(cbind(yr_d, unlist(sent_df$dest_email)))
names(dest_yr) = c('yr', 'dest')

#find top 10 recepient per year
dest_yr_tot = ddply(dest_yr, .(yr, dest), summarize, dest_tot = length(dest))
dest_yr_tot = dest_yr_tot[order(dest_yr_tot$yr, dest_yr_tot$dest_tot, decreasing = T), ]
dest_yr_top = ddply(dest_yr_tot, .(yr), function(x) x[1:10, ])

#create aliases
uni_dest = unique(dest_yr_top$dest)
alias = c('omitted')

dest_alias = data.frame(cbind(uni_dest, alias))
names(dest_alias) = c('dest', 'alias')
dest_yr_top = merge(dest_yr_top, dest_alias, by = 'dest', all.x = T)

#plot top recepients per year
p_dest_yr = list()
for (i in 2008:2014) {
  dat = subset(dest_yr_top, yr == i)
  dat = dat[order(dat$dest_tot), ]
  lev = dat$alias
  dat$alias = factor(dat$alias, levels = lev)
  
  p_dest_yr[[i-2007]] = 
    ggplot(dat, aes(x = alias, y = dest_tot)) +
    geom_bar(stat = 'identity', fill = '#3182bd') +
    coord_flip() + ggtitle(i) +
    scale_y_discrete(name = '', breaks = NULL) + scale_x_discrete(name = '')
}
grid.arrange(p_dest_yr[[1]], p_dest_yr[[2]], p_dest_yr[[3]],
             p_dest_yr[[4]], p_dest_yr[[5]], p_dest_yr[[6]],
             p_dest_yr[[7]], ncol = 2)

#for email body, only keep the most recent message i replied to
#usually ends before my prior messages as indicated by my own email address
msg_end = str_locate(sent_df$body, '<own email address>')
msg_end[is.na(msg_end)] = 0
sent_df$body_tr = str_sub(sent_df$body, 1, msg_end[, 1]-1)

#clean body (strip embedded email addresses and keep only letters)
body = gsub('[[:alnum:].]+@[[:alnum:].]+', ' ', sent_df$body_tr)
body = gsub('[^[:alpha:]]', ' ', body)

rm_space = function(x) {
  x = gsub('^ +', '', x)
  x = gsub(' +$', '', x)
  x = gsub(' +', ' ', x)  
}

body = rm_space(body)
sent_df$body_tr = body
save(sent_df, file = 'sent.RData')

#create corpus and remove stopwords
c = Corpus(VectorSource(body))
c_clean = tm_map(c, removeWords, c(stopwords('SMART')))
c_clean = tm_map(c_clean, rm_space)
lens = sapply(c_clean, nchar)
c_clean[which(lens == 0)] = NULL

#tag all words to identify noun
pos_tag = function(x) {
  gc() #clean garbage to free up memory space (otherwise an error may be thrown out reporting memory shortage)
  sent_token_annotator = Maxent_Sent_Token_Annotator()
  word_token_annotator = Maxent_Word_Token_Annotator()
  a = annotate(x, list(sent_token_annotator, word_token_annotator))
  
  pos_tag_annotator = Maxent_POS_Tag_Annotator()
  a = annotate(x, pos_tag_annotator, a)
  w = subset(a, type == 'word')
  
  #pick out nouns
  n = which(unlist(w$features) == 'NN' | unlist(w$features) == 'NNS')
  start = w$start[n]
  end = w$end[n]
  
  nouns = ''
  for (i in 1:length(n)) {
    nouns = paste(nouns, str_sub(x, start[i], end[i]))    
  }
  return(nouns)
}

c_n = lapply(c_clean, pos_tag)

#remove additional frequent, irrelevant words (e.g., 'mail', 'http')
rm_wd = function(x) {
  x = str_replace_all(x, '[a-z]*mail[a-z]*', '')
  x = str_replace_all(x, '[a-z]*ttp[a-z]*', '')
  return(x)
}
c_n = lapply(c_n, rm_wd)
c_n = lapply(c_n, rm_space)
save(c_n, file = 'c_n.RData')

#construct document-term matrix
c_n = Corpus(VectorSource(c_n))
dtm = DocumentTermMatrix(c_n, control = list(minWordLength = 3))
dtm = dtm[which(row_sums(dtm) > 0), ]
save(dtm, file = 'dtm.RData')

#lda
#determine the optimal number of topics via cv
#create 10 folds
ind = 1:nrow(dtm)
set.seed(2014)
ind = ind[order(rnorm(length(ind)))]
nf = ceiling(length(ind) / 10)
f = {}
for (i in 1:10) {
  f[[i]] = ind[seq((i-1)*nf+1, min(i*nf, length(ind)))]
}

#10-fold cv
lda_eval = data.frame(fold = integer(), topic = integer(), perplex = numeric())
for (i in 1:length(f)) {
  for (k in seq(2, 20, 2)) {
    cat(i, k, '\n')
    dtm_train = dtm[-f[[i]], ]
    dtm_test = dtm[f[[i]], ]
    
    set.seed(2014)
    lda_train = LDA(dtm_train, k)
    lda_test = LDA(dtm_test, model = lda_train)
    
    lda_eval = rbind(lda_eval, c(i, k, perplexity(lda_test)))
  }
}
names(lda_eval) = c('fold', 'topic', 'perplex')
pp = ggplot(lda_eval, aes(x = topic, y = perplex, colour = as.factor(fold), group = as.factor(fold))) + geom_line()
ggsave(pp, file = 'perplex.jpg')

#16 appears to be the optimal split
set.seed(2014)
lda_m = LDA(dtm, 16)
lda_topics = posterior(lda_m)$topics
lda_terms = posterior(lda_m)$terms

#word cloud
words = names(lda_terms[1, ])
for (i in 1:16) {
  png(paste0('lda', i, '.png'), width = 400, height = 400)
  wordcloud(words, lda_terms[i, ], max.words = 200, random.order = F, col = brewer.pal(8, "Dark2"))
  dev.off()
}

#sentiment
sent_df$sentiment = sapply(sent_df$body_tr, function(x) polarity(x)[[1]]$polarity)

#plot average sentiment per year
sent_yr = ddply(sent_df, .(yr), summarize, mean_sent = mean(sentiment, na.rm = T))
p_sent =
  ggplot(sent_yr, aes(x = yr, y = mean_sent)) +
  geom_line(colour = '#f768a1') +
  ggtitle('Mean sentiment per year') +
  scale_x_continuous(name = 'Year') +
  scale_y_continuous(name = 'Mean sentiment')

ggsave(p_sent, file = 'sent.jpeg')

#t-test and wilcoxon text
sent_df$grad = as.integer(sent_df$yr >= 2011)

sink('test.txt')
t.test(sentiment ~ grad, data = sent_df)
wilcox.test(sentiment ~ grad, data = sent_df)
sink()

save(sent_df, file = 'sent_df.RData')