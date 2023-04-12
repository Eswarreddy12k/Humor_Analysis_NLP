#system("sudo apt-get -y install libmagick++-dev", intern=TRUE)
install.packages("magick", verbose=TRUE,lib = '/kaggle/working')  # install magick package

install.packages("ggimage") # install ggimage package
install.packages("RWeka",lib = '/kaggle/working' ) # install RWeka package

install.packages("readr")


install.packages("rlang")
library(readr)
library(tidyverse)
library(tm)
library(wordcloud)
library(wordcloud2)
library(tidytext)
library(textdata)
library(reshape2)
library(knitr)
library(gridExtra)
library(grid)
library(magick)
library(igraph)
library(ggraph)
library(ggsci)
library(devtools)
library(RWeka, lib = "/kaggle/working")
library(viridis)
library(patchwork)
library(circlize)
library(networkD3)
library(stringr)
library(IRdisplay)


data <- read.csv('C:/Users/eswar/Downloads/fg_dataset1_3.csv', fileEncoding = "latin1")
data2 <- read.csv('C:/Users/eswar/Downloads/Family_guy_dialog.csv')

afinn <- read.csv('C:/Users/eswar/Downloads/Afinn.csv')
bing  <-  read_csv('C:/Users/eswar/Downloads/Bing.csv')
nrc   <- read_csv('C:/Users/eswar/Downloads/NRC.csv')

head(data,3)




corpus2 <- Corpus(VectorSource(data2$dialog))
tdm2 <- TermDocumentMatrix(corpus2, control = list(removePunctuation = TRUE,
                                                 stopwords = TRUE))
lda2 <- LDA(tdm2, k = 10)
topics2 <- topics(lda2)

data2_train=matrix(c(0,0,0,0,0,0,0,0,0,0,0))
data2_train_avg=matrix(c(0,0,0,0,0,0,0,0,0,0,0))
for(i in 1:1372){
  if(data2$seasons[i]=="season 1"){
    data2_train[1]=data2_train[1]+topics2[i];
    data2_train_avg[1]=data2_train_avg[1]+1
  }
  if(data2$seasons[i]=="season 2"){
    data2_train[2]=data2_train[2]+topics2[i];
    data2_train_avg[2]=data2_train_avg[2]+1
  }
  if(data2$seasons[i]=="season 3"){
    data2_train[3]=data2_train[3]+topics2[i];
    data2_train_avg[3]=data2_train_avg[3]+1
  }
  if(data2$seasons[i]=="season 4"){
    data2_train[4]=data2_train[4]+topics2[i];
    data2_train_avg[4]=data2_train_avg[4]+1
  }
  if(data2$seasons[i]=="season 5"){
    data2_train[5]=data2_train[5]+topics2[i];
    data2_train_avg[5]=data2_train_avg[5]+1
  }
  if(data2$seasons[i]=="season 6"){
    data2_train[6]=data2_train[6]+topics2[i];
    data2_train_avg[6]=data2_train_avg[6]+1
  }
  if(data2$seasons[i]=="season 7"){
    data2_train[7]=data2_train[7]+topics2[i];
    data2_train_avg[7]=data2_train_avg[7]+1
  }
  if(data2$seasons[i]=="season 8"){
    data2_train[8]=data2_train[8]+topics2[i];
    data2_train_avg[8]=data2_train_avg[8]+1
  }
  if(data2$seasons[i]=="season 9"){
    data2_train[9]=data2_train[9]+topics2[i];
    data2_train_avg[9]=data2_train_avg[9]+1
  }
  if(data2$seasons[i]=="season 10"){
    data2_train[10]=data2_train[10]+topics2[i];
    data2_train_avg[10]=data2_train_avg[10]+1
  }
  if(data2$seasons[i]=="season 12"){
    data2_train[11]=data2_train[11]+topics2[i];
    data2_train_avg[11]=data2_train_avg[11]+1
  }
  
}
for(i in 1:11){
  data2_train[i]=data2_train[i]/data2_train_avg[i]
}
data2_train


plot(data2_train)
lines(data2_train,col="blue")










library(topicmodels)

data <- na.omit(data)
nrow(data)
summary(data)
# Convert text to lowercase
for(i in 1:72996){
  data[i,3] <- tolower(data[i,3])
}
# Remove punctuation and numbers
for(i in 1:72996){
  data[i,3] <- gsub("[[:punct:][:digit:]]", " ", data[i,3])
  data[i,3] <- gsub("\n", " ", data[i,3])
  print(i)
}


# Remove stop words
for(i in 1:72996){
  data[i,3] <- removeWords(data[i,3], stopwords("english"))
  print(i)
}


# Stem or lemmatize words
for(i in 1:72996){
  data[i,3] <- stemDocument(data[i,3])
  
    print(i)
  
  
}
dim(data)
data$dialouge

# assume "dialogues" is a dataframe with a "text" column containing the dialogue text
corpus <- Corpus(VectorSource(data$dialouge))


tdm <- TermDocumentMatrix(corpus)
inspect(tdm)

lda <- LDA(tdm, k = 10)
lda


topics <- topics(lda)



# Look for topics that contain humorous content
length(topics)


s_train=c()
s_test=c()
s_train_avg_calc=matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

s_train_dataset=matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),nrow = 136,ncol = 2,rownames=episodewise)
dim(s_train_dataset)

dim(data)

fac1=7*(12405/136);
fac2=28*(12405/136);
fac3=59*(12405/136);
fac4=88*(12405/136);
fac5=106*(12405/136);
fac6=119*(12405/136);
fac7=136*(12405/136);

episodewise=c()
for(i in 1:136){
  if(i<=7){
    strs=paste0("s1e",i,"")
    episodewise=c(episodewise,strs)
  }
  else if(i<=28){
    strs=paste0("s2e",i-7,"")
    episodewise=c(episodewise,strs)
  }
  else if(i<=59){
    strs=paste0("s3e",i-28,"")
    episodewise=c(episodewise,strs)
  }
  else if(i<=88){
    strs=paste0("s4e",i-59,"")
    episodewise=c(episodewise,strs)
  }
  else if(i<=106){
    strs=paste0("s5e",i-88,"")
    episodewise=c(episodewise,strs)
  }
  else if(i<=119){
    strs=paste0("s6e",i-106,"")
    episodewise=c(episodewise,strs)
  }
  else if(i<=136){
    strs=paste0("s7e",i-119,"")
    episodewise=c(episodewise,strs)
  }
}
episodewise

for(i in 1:length(topics)){
  if(i<fac1){
    epi=1+(i-0)/93;
    
    s_train_dataset[epi]=s_train_dataset[epi]+topics[i];
    s_train_avg_calc[epi]=s_train_avg_calc[epi]+1
  }
  else if(i<fac2){
    epi=1+7+((i-fac1)/93);
    s_train_dataset[epi]=s_train_dataset[epi]+topics[i];
    s_train_avg_calc[epi]=s_train_avg_calc[epi]+1
  }
  else if(i<fac3){
    epi=1+28+((i-fac2)/93);
    s_train_dataset[epi]=s_train_dataset[epi]+topics[i];
    s_train_avg_calc[epi]=s_train_avg_calc[epi]+1
  }
  else if(i<fac4){
    epi=1+59+((i-fac3)/93);
    s_train_dataset[epi]=s_train_dataset[epi]+topics[i];
    s_train_avg_calc[epi]=s_train_avg_calc[epi]+1
  }
  else if(i<fac5){
    epi=1+88+((i-fac4)/93);
    s_train_dataset[epi]=s_train_dataset[epi]+topics[i];
    s_train_avg_calc[epi]=s_train_avg_calc[epi]+1
  }
  else if(i<fac6){
    epi=1+106+((i-fac5)/93);
    s_train_dataset[epi]=s_train_dataset[epi]+topics[i];
    s_train_avg_calc[epi]=s_train_avg_calc[epi]+1
  }
  else if(i<fac7){
    epi=1+119+((i-fac6)/93);
    s_train_dataset[epi]=s_train_dataset[epi]+topics[i];
    s_train_avg_calc[epi]=s_train_avg_calc[epi]+1
  }
  
}
length(s_train_dataset)
length(episodewise)
rownames(s_train_dataset)=episodewise
for(i in 1:136){
  s_train_dataset[i,1]=s_train_dataset[i,1]/s_train_avg_calc[i];
}
s_train_dataset




s1_ratings=c(7.7,7.5,7.6,7.4,7.4,7.2,7.5)
length(s1_ratings)
s2_ratings=c(7.6,
             7.3,
             8.1,
             7.4,
             7.7,
             8.2,
             7.3,
             8,
             7.3,
             7.4,
             7.2,
             7.5,
             8.1,
             7.8,
             7.4,
             6.9,
             7.7,
             8.1,
             7.4,
             8.1,
             7.2)
length(s2_ratings)
s3_ratings=c(
  8,
  7.6,
  7.7,
  7.5,
  7.6,
  7.6,
  7.7,
  7.6,
  7.7,
  8,
  7.3,
  7.4,
  6.7,
  6.8,
  7.4,
  7.1,
  7.3,
  8.1,
  8.1,
  7.4,
  7.6,
  7.6,
  7.6,
  7.4,
  7.4,
  7.6,7.7,
  7.8,
  7.5,
  7.4,7.5)
length(s3_ratings)
s4_ratings=c(
  7.8,
  7.6,
  7.5,
  7.4,
  7.6,
  8.2,
  7.4,
  7.5,
  7.6,
  7.3,
  7.5,
  7.7,
  7.2,
  8.5,
  7.7,
  7.7,
  8,
  7.3,
  7.2,
  7.7,
  7.5,
  7.6,
  7.4,
  7.5,
  7.2,
  7.8,
  7.7,
  7.6,
  7.6
)
length(s4_ratings)
s5_ratings=c(8.2,
             7.4,
             7.5,
             7.6,
             7.7,
             7.3,
             7.5,
             7.9,
             7.9,
             7.6,
             7.6,
             7.7,
             7.3,
             7.2,
             7.2,
             7.3,
             7.5,
             8.4
)
length(s5_ratings)
s6_ratings=c(8.2,
             7.2,
             7.4,
             8.3,
             8.3,
             6.9,
             7.3,
             7.6,
             7.4,
             7.3,
             7.2,
             6.8,
             7.6
             
)
length(s6_ratings)
s7_ratings=c(6.8,
             8,
             8.1,
             7.5,
             7.6,
             7.1,
             7.6,
             7.5,
             6.4,
             6.9,
             7.7,
             7.7,
             7.9,
             7.3,
             8.2,
             7.1,
             7.0)
combined_ratings=c(s1_ratings,s2_ratings,s3_ratings,s4_ratings,s5_ratings,s6_ratings,s7_ratings)

for(i in 1:136){
  s_train_dataset[i,2]=combined_ratings[i]
}
s_train_dataset


s_train_split=s_train_dataset[1:80,]
s_train_split

s_test_split=s_train_dataset[81:136,]


y=c()
for(i in 1:80){
  y=c(y,s_train_split[i,2])
}
y

x=c()
for(i in 1:80){
  x=c(x,s_train_split[i,1])
}
x


model <- lm(y~x)
print(summary(model))

testx=c()
for(i in 1:56){
  testx=c(testx,s_test_split[i,1])
  
  
}

test=data.frame(x=testx)
prediction=((predict(model,newdata = test)-7.5)*100)

plot(prediction)
lines(prediction,col="blue")

plot(s_test_split[,2])
lines(s_test_split[,2],col="blue")

plot(s_train_dataset[,2])
lines(s_train_dataset[,2],col="blue")


test=data.frame(x=10)
print(predict(model,newdata = test))


scripts=data2
tokens <- scripts %>% 
  mutate(dialogue = as.character(scripts$dialog)) %>% 
  unnest_tokens(word, dialogue)


tokens %>% head(5) %>% select(character, word)

tokens %>% 
  # append the bing sentiment and prepare the data
  inner_join(bing, "word") %>%
  count(word, sentiment, sort=T) %>% 
  acast(word ~ sentiment, value.var = "n", fill=0) %>% 
  
  # wordcloud
  comparison.cloud(colors=c("#ff6f69", "#3bd6c6"), max.words = 80,scale = c(3,2))

# Add a dope image on the plot
image <- image_read("https://i.postimg.cc/yNHdYYNf/Stewie.jpg")
grid.raster(image, x=0.85, y=0.31, height=0.27)

tokens %>% 
  inner_join(nrc, "word") %>% 
  count(sentiment, word, sort=T) %>% 
  group_by(sentiment) %>% 
  arrange(desc(n)) %>% 
  slice(1:7) %>% 
  
  # Plot: 
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend = F) +
  facet_wrap(~sentiment, scales = "free_y") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  coord_flip() +
  theme_light() +
  labs(x="Word", y="Frequency", title="Sentiment split by most frequent words")+ 
  theme(text = element_text(size = 20))


# The dope image:
image_family = image_read("https://i.postimg.cc/Hs8767Jk/family1.jpg")
grid.raster(image_family, x=0.78, y=0.19, height=0.30)


net_plot <- tokens %>% 
  # get 'bing' and filter the data
  inner_join(bing, "word") %>% 
  filter(character %in% c("Peter","Lois","Stewie","Meg","Brian","Chris")) %>% 
  
  # sum number of words per sentiment and character
  count(sentiment, character) %>% 
  group_by(character, sentiment) %>% 
  summarise(sentiment_sum = sum(n),.groups = 'drop') %>% 
  ungroup()

# column names
colnames(net_plot) <- c("source", "target", "value")

nodes <- data.frame(name=c(as.character(net_plot$source), as.character(net_plot$target)) %>% unique())

# With networkD3, connection must be provided using id

net_plot$IDsource=match(net_plot$source, nodes$name)-1 
net_plot$IDtarget=match(net_plot$target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#fcff00FF","#ffce74FF","#ffe8a1FF","#81ff81FF","#b6b6ffFF","#20b2aaFF","#ff6f69FF","#88d8b0FF"])'


df_net <- as.data.frame(net_plot)

# Make the Network
sankey_network_plot <- sankeyNetwork(Links = df_net, Nodes = nodes,height = 500, width = 800,
                                     Source = "IDsource", Target = "IDtarget",
                                     Value = "value", NodeID = "name", 
                                     sinksRight=FALSE, colourScale=ColourScal, nodeWidth=25, fontSize=15, nodePadding=15)



sankey_network_plot 

tokens %>% 
  # Data:
  filter(character %in% c("Peter","Lois","Stewie","Meg","Brian","Chris")) %>% 
  inner_join(nrc, "word") %>% 
  count(character, sentiment, sort=T) %>%
  
  
  # Plot:
  ggplot(aes(x=sentiment, y=n)) +
  geom_col(aes(fill=sentiment), show.legend = F) +
  facet_wrap(~character, scales="free_x") +
  labs(x="Sentiment", y="Frequency", title="The Family guy Moods") +
  theme_light()+
  coord_flip() +
  theme(text = element_text(size = 23))


# The dope image:
image = image_read("https://i.postimg.cc/vBTdbJBV/sad-brain.jpg")
grid.raster(image, x=0.94, y=0.18, height=0.17)
