library(plyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(treemap)
library(d3treeR)
library(reshape2)
library(data.table)

survey_df <- read.csv("https://raw.githubusercontent.com/kefortney/Burlington-Data-Scientists-Learning-Series-101--Surveys-and-Python-R/master/datasurvey.csv",header = TRUE, fileEncoding = "UTF-8", sep = ",",na.strings=c("","NA"))
colnames(survey_df)[14] <- "SAS.SPSS.STATA.JMP" 
colnames(survey_df)[16] <- "Spreadsheets"

educ_df <- survey_df %>% count(What.s.your.highest.academic.degree.) %>% as.data.frame()

educ_df$educ_label <- paste(educ_df$What.s.your.highest.academic.degree., educ_df$n, sep = ":")

# plt1 is treemap for Educational Level

plt1_1 <- treemap(educ_df,
        index="educ_label",
        vSize="n",
        type="index",
        title = "Survey Respondent Highest Academic Degree"
        
)

plt1 <- d3tree2( plt1_1 ,  rootname = "# of respondents by education level" )

time_df1 <- as.data.frame(colSums(survey_df[36:43],na.rm = TRUE))
time_df2 <- setDT(time_df1, keep.rownames = TRUE)[]
colnames(time_df2)[1] <- "proposed_time"
colnames(time_df2)[2] <- "respondent_count"

# plt2 is a plot for most favorable meeting timeslots

plt2 <- ggplot(time_df2, aes(x = reorder(proposed_time, -respondent_count), y = respondent_count)) + theme_wsj() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        geom_text(aes(label=respondent_count),hjust=-0.5, vjust=-0.5) + ggtitle("Preferred meeting times")

tool_df1 <- melt(survey_df[12:24], id.vars = survey_df[0])
colnames(tool_df1)[1] <- "tool_name"
colnames(tool_df1)[2] <- "use_frequency"
tool_df2 <- tool_df1 %>% group_by(tool_name, use_frequency) %>%  summarise(n = n())

# plt3 is a plot for analyzing which tools are used more frequently

plt3 <- ggplot(tool_df2, aes(x = use_frequency, y = tool_name)) + 
  geom_point(aes(size = n)) + theme_fivethirtyeight() + 
  geom_text(aes(label=n),hjust=-0.5, vjust=-0.5) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Tool Usage") + theme(plot.title = element_text(hjust = 0.5))

# plt4 is a plot for What Best Describes You question

df_purpose1 <-as.data.frame(table(survey_df$What.best.describes.you.))
plt4 <- plot_ly(df_purpose1,labels = ~Var1, values = ~Freq) %>%
  add_pie(hole = 0.6) %>%
  layout(title = "What Best Describes You",  showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% layout(legend = list(x = 100, y = -1))