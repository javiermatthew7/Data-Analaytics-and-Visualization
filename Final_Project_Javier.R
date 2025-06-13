# Final Project #
#Matthew Javier



#################################################################################
# Load Libraries #
library(dplyr) # data manipulation
library(ggpubr) # creating plots 
library(ggeasy) # easy plot tweaks
library(ggplot2) # datasets (i.e diamonds)
library(scales) # tweaking axis labels, e.g. adding a dollar sign or commas
library(smallstuff) # Handling Outliers
library(RColorBrewer)
#################################################################################

# Loading Dataset

df = read.csv(file = 'data/youth_smoking_drug_data_10000_rows_expanded.csv')
#Familiarizing with the Dataset.
head(df)
str(df)
summary(df$Smoking_Prevalence)


### Changing the columns into factor for easier plotting

df <- df %>%
  mutate(
    Year = as.factor(Year),
    Age_Group = as.factor(Age_Group),
    Gender = as.factor(Gender),
    Socioeconomic_Status = as.factor(Socioeconomic_Status),
    School_Programs = as.factor(School_Programs),
    Access_to_Counseling = as.factor(Access_to_Counseling),
    Substance_Education = as.factor(Substance_Education)
  )

str(df)



#1 horizontal boxplot
# Average smoking prevalence for each Age Group and Gender
df_summary <- df %>%
  group_by(Age_Group, Gender) %>%
  summarize(Avg_Smoking_Prevalence = mean(Smoking_Prevalence, na.rm = TRUE))

#calculate mean smoking prevalence for the new aggregated data
mean_smoking_prevalence <- mean(df_summary$Avg_Smoking_Prevalence, na.rm = TRUE)

# horizontal bar plot and adjustments
horizontal<-ggbarplot(
    data = df_summary,
    x = 'Age_Group',
    y = 'Avg_Smoking_Prevalence',
    orientation = 'horizontal',
    xlab = 'Age Group',
    ylab = 'Average Smoking Prevalence (%)',
    color = 'Gender',
    fill = 'Gender',
    palette = c('purple','lightpink','lightblue'),
    title = 'Youth Smoking Prevalence by Age Group and Gender',
    subtitle ='Average smoking prevalence among different age groups, segmented by gender',
    ggtheme = theme_minimal()
  ) + 
    geom_hline(
      yintercept = mean_smoking_prevalence,
      linetype = 5,
      color = 'red'
    ) +
    font(
    "title", size = 20, face = 'bold'
    ) +
    font(
      "subtitle", size = 12, face = 'italic'
    )

horizontal


### line graph
#2

# Average Smoking Prevalence Over Time by Age Group
df_time <- df %>%
  group_by(Year, Age_Group) %>%
  summarize(Avg_Smoking_Prevalence = mean(Smoking_Prevalence, na.rm = TRUE))

#Making the line graph and adjustments 

line<-ggplot(df_time, aes(x = Year, y = Avg_Smoking_Prevalence, color = Age_Group, group = Age_Group)) +
    geom_line(size = 1) +
    labs(
      x = 'Year',
      y = 'Average Smoking Prevalence (%)',
      title = 'Average Smoking Prevalence Over Time by Age Group',
      subtitle = 'Trends in smoking prevalence across different age groups over time'
    ) +
    theme_minimal() +
    font("title", size = 20, face = 'bold') +
    font("subtitle", size = 12, face = 'italic')


line


# 3. Boxplot of Smoking Prevalence by Socioeconomic Status and Drug Experimentation

#mutate for drug experimentation from 1-10 to low medium high
df <- df %>%
  mutate(Drug_Experimentation_Level = cut(Drug_Experimentation, breaks = 3, labels = c("Low", "Medium", "High")))

#boxplot and adjustments
bplot<-ggboxplot(
    data = df,
    x = 'Socioeconomic_Status', 
    y = 'Smoking_Prevalence', 
    fill = 'Drug_Experimentation_Level',  
    palette = c('lightblue', 'dodgerblue', 'lightgreen'),  
    ylab = 'Smoking Prevalence (%)',
    xlab = 'Socioeconomic Status',
    title = 'Smoking by Socioeconomic and Drug Experimentation',
    subtitle = 'Box plot showing smoking prevalence segmented by socioeconomic status and drug experimentation',
    ggtheme = theme_classic()
  ) + 
    font(
      "title", size = 20, face = 'bold'
    ) +
    font(
      "subtitle", size = 12, face = 'italic'
    ) +
    font(
      "y.text", size = 8, face = 'italic'
    )

bplot


##
# 4. Heatmap of Smoking Prevalence by Age Group and Drug Experimentation


#grouping
df_heatmap <- df %>%
  group_by(Mental_Health, Drug_Experimentation_Level) %>%
  summarize(Avg_Smoking_Prevalence = mean(Smoking_Prevalence, na.rm = TRUE))

#heatmap and adjustments
heatmap<-ggplot(df_heatmap, aes(x = Mental_Health, y = Drug_Experimentation_Level, fill = Avg_Smoking_Prevalence)) +
    geom_tile(color = 'white') +
    scale_fill_gradient(low = 'lightblue', high = 'darkred') +
    labs(
      x = 'Mental Health Score',
      y = 'Drug Experimentation Level',
      fill = 'Avg Smoking Prevalence (%)',
      title = 'Heatmap of Smoking by Mental Health and Drug',
      subtitle = 'Average smoking prevalence across mental health scores and drug experimentation levels',
      caption = 'Source: https://www.kaggle.com/datasets/waqi786/youth-smoking-and-drug-dataset/data'
    ) +
    theme_minimal() +
    font("title", size = 20, face = 'bold') +
    font("subtitle", size = 12, face = 'italic'
        )

heatmap


#Combining all the plots


combined_plot <- ggarrange(
  heatmap, line, 
  bplot, horizontal,  
  labels = c("A", "B", "C", "D"), 
  ncol = 2, nrow = 2,
  heights = c(1.5, 1) #making the heatmap stand out more.
)
combined_plot

