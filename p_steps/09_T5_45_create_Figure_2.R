########################################################%##
#                                                          #
####  Generate D6_Figure_2  ####
#                                                          #
########################################################%##


# author: Rosa Gini

# v 1.0 12 Dec 2024

# monthly

# v 0.1 2 Dec 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D6_Figure_2"
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

input_fig <- readRDS(file.path(thisdirinput, "D5_dispensings_AA.rds"))

# input_fig[,Q := paste0("Q",as.character(ceiling((month + 0)/3)))]
# 
input_fig[, monthstr := paste0("0",as.character(month))]
input_fig[, len := nchar(monthstr)]


input_fig[, monthstr := substr(
  monthstr,
  nchar(monthstr) - 1, 
  nchar(monthstr))]

input_fig[,Time_LabelValue := paste0(as.character(year),"-",monthstr)]

# aggregate

input_fig <- input_fig[, .(
  Nvials = sum(Nvials, na.rm = TRUE),
  Ndisp = sum(Ndisp, na.rm = TRUE)
), by = Time_LabelValue]

# remove 0s

input_fig <- input_fig[Nvials == 0, Nvials := NA_real_ ]

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
  
  # geom_rect(aes(xmin = as.Date("2020-03-05"), xmax = as.Date("2021-06-21"), ymin = -Inf, ymax = Inf), fill = "orange", alpha = 0.9) +
  # geom_text(aes(x = as.Date("2021-06-21"), label = "Restrizioni Covid", y = Inf), hjust = 1.1, vjust = 1.5, angle = 90, color = "orange") +
# 
#   # Add the shaded areas for narrow and broad bleeding
#   geom_ribbon(aes(ymin = lb_bleeding_narrow, ymax = ub_bleeding_narrow), fill = "lightgrey", alpha = 0.3) +
#   geom_ribbon(aes(ymin = lb_bleeding_broad, ymax = ub_bleeding_broad), fill = "grey", alpha = 0.5) +

  # Add the lines of Nvials and Ndisp
  geom_line(aes(y = Ndisp), color = "lightgrey", size = 1) +
  geom_line(aes(y = Nvials), color = "darkgrey", size = 1) +

  # Add annotations for specific dates and periods
  geom_vline(xintercept = start_date_period[["2"]], color = "darkblue", linetype = "dashed") +
  geom_text(aes(x = start_date_period[["2"]], label = "Antidoto in Toscana", y = Inf), hjust = 1.1, vjust = 1.5, angle = 90, color = "darkblue") +

  geom_vline(xintercept = start_date_period[["3"]], color = "darkgreen", linetype = "dashed") +
  geom_text(aes(x = start_date_period[["3"]], label = "Linee guida", y = Inf), hjust = 1.1, vjust = 1.5, angle = 90, color = "darkgreen") +

  # Set axis labels and plot title
  labs(x = "Mese", y = "Numero di erogazioni e numero di fiale",
  title = "") +
  theme_minimal() +  
  scale_x_date(labels = scales::date_format("%Y-%m"), date_breaks = "1 month") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Print the plot
print(p)

namefig <- "Figura_2.pdf"

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
