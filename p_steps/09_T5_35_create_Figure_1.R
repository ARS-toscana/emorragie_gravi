########################################################%##
#                                                          #
####  Generate D6_Figure_1  ####
#                                                          #
########################################################%##


# author: Rosa Gini


# v 1.0 12 Dec 2024

# per month

# v 0.1 23 Nov 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D6_Figure_1"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  thisdirfig <- thisdiroutput
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- direxp
  thisdiroutput <- dirD6
  thisdirfig <- dirfig
}

# import input datasets

processing <- readRDS(file.path(thisdirinput, "D5_IR.rds"))

input_fig <- processing[Age_LevelOrder == 99 & Gender_LevelOrder == 99 & Time_LevelOrder == 1,.(Time_LabelValue,bleeding_narrow_b,IR_bleeding_narrow,lb_bleeding_narrow,ub_bleeding_narrow,bleeding_broad_b,IR_bleeding_broad,lb_bleeding_broad,ub_bleeding_broad)]
# 
# 
# # Convert Time_LabelValue to Date format for better control in ggplot
# convert_quarter_to_date <- function(quarter) {
#   sapply(quarter, function(q) {
#     year <- substr(q, 1, 4)
#     quarter_num <- substr(q, 6, 7)
#     month <- switch(quarter_num,
#                     "Q1" = "01",
#                     "Q2" = "04",
#                     "Q3" = "07",
#                     "Q4" = "10")
#     ymd(paste0(year, month, "01"))
#   })
# }
#
# # Apply the function to your data.table
# input_fig[, quarter_date := convert_quarter_to_date(Time_LabelValue)]
# input_fig[, quarter_date := as.Date(quarter_date, origin = "1970-01-01")]

input_fig[, date := as.Date(paste0(Time_LabelValue,"-01"))]
input_fig <- input_fig[ date >= study_start_date & date <= study_end_date, ]


# Start the ggplot
p <- ggplot(input_fig, aes(x = date)) +
  
  geom_rect(aes(xmin = start_date_period[["1b"]], xmax = end_date_period[["1b"]], ymin = -Inf, ymax = Inf), fill = "orange", alpha = 0.9) +
  geom_text(aes(x = end_date_period[["1b"]], label = "Restrizioni Covid", y = Inf), hjust = 1.1, vjust = 1.5, angle = 90, color = "orange") +

  # Add the shaded areas for narrow and broad bleeding
  geom_ribbon(aes(ymin = lb_bleeding_narrow, ymax = ub_bleeding_narrow), fill = "lightgrey", alpha = 0.3) +
  geom_ribbon(aes(ymin = lb_bleeding_broad, ymax = ub_bleeding_broad), fill = "grey", alpha = 0.5) +

  # Add the incidence rate lines
  geom_line(aes(y = IR_bleeding_narrow), color = "lightgrey", size = 1) +
  geom_line(aes(y = IR_bleeding_broad), color = "darkgrey", size = 1) +

  # Add annotations for specific dates and periods
  geom_vline(xintercept = as.Date(start_date_period[["2"]]), color = "darkblue", linetype = "dashed") +
  geom_text(aes(x = as.Date(start_date_period[["2"]]), label = "Antidoto in Toscana", y = Inf), hjust = 1.1, vjust = 1.5, angle = 90, color = "darkblue") +

  geom_vline(xintercept = start_date_period[["3"]], color = "darkgreen", linetype = "dashed") +
  geom_text(aes(x = start_date_period[["3"]], label = "Linee guida", y = Inf), hjust = 1.1, vjust = 1.5, angle = 90, color = "darkgreen") +

  # Set axis labels and plot title
  labs(x = "Mese", y = "Incidenza di sanguinamenti per 100 anni-persona",
  title = "") +
  theme_minimal() +  
  scale_x_date(labels = scales::date_format("%Y-%m"), date_breaks = "1 month") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Print the plot
print(p)

namefig <- "Figura_1.pdf"

ggsave(file.path(thisdirfig, namefig), plot = p, width = 18, height = 8, device = "pdf")



