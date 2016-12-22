library(ggplot2)
df = read.csv("Serial Killers Data.csv", stringsAsFactors = TRUE)

m_df = df[,c("Code", "Type", "MethodDescription", "DateDeath")]
m_df$DateDeath = as.character(m_df$DateDeath)

m_df$DateDeath[nchar(m_df$DateDeath) == 8] = substr(m_df$DateDeath[nchar(m_df$DateDeath) == 8], 5, 8)
m_df$DateDeath[nchar(m_df$DateDeath) == 9] = substr(m_df$DateDeath[nchar(m_df$DateDeath) == 9],6,9)
m_df$DateDeath[nchar(m_df$DateDeath) == 10] = substr(m_df$DateDeath[nchar(m_df$DateDeath) == 10], 7,10)
m_df$DateDeath = as.numeric(m_df$DateDeath)
m_df = subset(m_df, m_df$DateDeath <= 2016 & m_df$DateDeath >= 1980) #removing noise

motives_list = c("Financial Gain","Attention","Enjoyment","Anger","Mental Illness","cult","Avoid arrest","Gang activity","Convenience","Wildwest Outlaw", "Multiple Motivations")

#Separating methods
m_df$MethodDescription = as.character(m_df$MethodDescription)
m_df$methods = strsplit(m_df$MethodDescription, ",")

for (i in 1:nrow(m_df)){
  for (j in m_df[i, "methods"][[1]]){
    
    if (!(j == "")){
      if (!(j %in% colnames(m_df))){
        m_df[,j] = 0
      }
      m_df[i,j] = 1
    }
    
  }
}

m_df$DateDeath = as.factor(m_df$DateDeath)
m_df$MethodDescription = NULL
m_df$methods = NULL

for (i in 1:nrow(m_df)){
  code_int = as.integer(m_df$Code[i])
  if (!is.na(code_int)){
    col_name = motives_list[code_int]
    if (!(col_name %in% colnames(m_df))){
      m_df[,col_name] = 0
    }
    m_df[i,col_name] = 1
  }
}

#Correlation of motives with weapons
cor(m_df[-c(1:23)], m_df[,4:22])

motives = as.data.frame(cbind(m_df$code_int, m_df$DateDeath))
colnames(motives) = c("Motive", "Year")
motives$Motive = as.factor(motives$Motive)

motives$freq = 1
motives_freq = aggregate(freq ~ Year+Motive, data = motives, FUN = sum)

ggplot(motives_freq, aes(Year, freq, group = Motive, col = Motive)) + 
  geom_line()+
  ggtitle("Motive Variance over the Years") +
  labs(x = "Years", y = "Number of reported homicides")+
  scale_color_hue(labels = motives_list)+
  theme_bw()

