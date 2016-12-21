df = read.csv("Serial Killers Data.csv", stringsAsFactors = TRUE)

m_df = df[,c("Code", "Type", "MethodDescription", "DateDeath")]

m_df$DateDeath = as.character(m_df$DateDeath)

m_df$DateDeath[nchar(m_df$DateDeath) == 8] = substr(m_df$DateDeath[nchar(m_df$DateDeath) == 8], 5, 8)
m_df$DateDeath[nchar(m_df$DateDeath) == 9] = substr(m_df$DateDeath[nchar(m_df$DateDeath) == 9],6,9)
m_df$DateDeath[nchar(m_df$DateDeath) == 10] = substr(m_df$DateDeath[nchar(m_df$DateDeath) == 10], 7,10)

m_df$DateDeath = as.numeric(m_df$DateDeath)
m_df = subset(m_df, m_df$DateDeath <= 2016) #removing noise
