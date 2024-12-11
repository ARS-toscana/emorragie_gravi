########################################################%##
#                                                          #
####  Generate D6_Figure_1  ####
#                                                          #
########################################################%##


# author: Rosa Gini

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

input_fig <- processing[Age_LevelOrder == 99 & Gender_LevelOrder == 99 & Time_LevelOrder == 2,.(Time_LabelValue,bleeding_narrow_b,IR_bleeding_narrow,lb_bleeding_narrow,ub_bleeding_narrow,bleeding_broad_b,IR_bleeding_broad,lb_bleeding_broad,ub_bleeding_broad)]


# Convert Time_LabelValue to Date format for better control in ggplot
convert_quarter_to_date <- function(quarter) {
  sapply(quarter, function(q) {
    year <- substr(q, 1, 4)
    quarter_num <- substr(q, 6, 7)
    month <- switch(quarter_num,
                    "Q1" = "01",
                    "Q2" = "04",
                    "Q3" = "07",
                    "Q4" = "10")
    ymd(paste0(year, month, "01"))
  })
}

# Apply the function to your data.table
input_fig[, quarter_date := convert_quarter_to_date(Time_LabelValue)]
input_fig[, quarter_date := as.Date(quarter_date, origin = "1970-01-01")]

# Start the ggplot
p <- ggplot(input_fig, aes(x = quarter_date)) +
  
  geom_rect(aes(xmin = as.Date("2020-03-05"), xmax = as.Date("2021-06-21"), ymin = -Inf, ymax = Inf), fill = "orange", alpha = 0.9) +
  geom_text(aes(x = as.Date("2021-06-21"), label = "Restrizioni Covid", y = Inf), hjust = 1.1, vjust = 1.5, angle = 90, color = "orange") +

  # Add the shaded areas for narrow and broad bleeding
  geom_ribbon(aes(ymin = lb_bleeding_narrow, ymax = ub_bleeding_narrow), fill = "lightgrey", alpha = 0.3) +
  geom_ribbon(aes(ymin = lb_bleeding_broad, ymax = ub_bleeding_broad), fill = "grey", alpha = 0.5) +

  # Add the incidence rate lines
  geom_line(aes(y = IR_bleeding_narrow), color = "lightgrey", size = 1) +
  geom_line(aes(y = IR_bleeding_broad), color = "darkgrey", size = 1) +

  # Add annotations for specific dates and periods
  geom_vline(xintercept = as.Date(start_date_period[["2"]]), color = "darkblue", linetype = "dashed") +
  geom_text(aes(x = as.Date(start_date_period[["2"]]), label = "Antidoto in Toscana", y = Inf), hjust = 1.1, vjust = 1.5, angle = 90, color = "darkblue") +

  geom_vline(xintercept = as.Date("2023-07-31"), color = "green", linetype = "dashed") +
  geom_text(aes(x = as.Date("2023-07-31"), label = "Linee guida", y = Inf), hjust = 1.1, vjust = 1.5, angle = 90, color = "green") +

  # Set axis labels and plot title
  labs(x = "Trimestre", y = "Incidenza di sanguinamenti per 100 anni-persona",
  title = "") +
  theme_minimal() +  
  scale_x_date(labels = scales::date_format("%Y-%m"), date_breaks = "3 month") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Print the plot
print(p)

namefig <- "Figura_1.pdf"

ggsave(file.path(thisdirfig, namefig), plot = p, width = 11, height = 8, device = "pdf")




# fwrite(input_fig,"C:/temp/temp.csv")

# for (event in c("bleeding_narrow","bleeding_broad")) {
#   name_cols <- paste0(c("IR", "lb", "ub"),"_",event)
#   processing[, (name_cols) := exactPoiCI(processing, paste0(event,"_b"), paste0("PY_",event), conversion_factor = 1, per = 100)]  
# }
# 
# 
# 
# ################################
# # clean
# 
# tokeep <- c("Gender_LabelValue", "Gender_LevelOrder", "Age_LabelValue", "Age_LevelOrder", "Time_LabelValue", "Time_LevelOrder", "bleeding_narrow_b", "IR_bleeding_narrow", "lb_bleeding_narrow", "ub_bleeding_narrow",   "bleeding_broad_b", "IR_bleeding_broad", "lb_bleeding_broad", "ub_bleeding_broad")
# 
# 
# processing <- processing[, ..tokeep]
# 
# setorderv(
#   processing,
#   c("Gender_LevelOrder", "Age_LevelOrder","Time_LevelOrder","Time_LabelValue")
# )
# 
# 
# #########################################
# # save
# 
# outputfile <- processing
# 
# nameoutput <- "D5_IR"
# nameoutputext <- paste0(nameoutput,".rds")
# assign(nameoutput, outputfile)
# saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
# 
