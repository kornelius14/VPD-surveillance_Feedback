#Title    : Weekly feedback of VPD surveillance performance
#Authors  : Kornelius Son
#Update   : 11/10/2024


# AFP (Acute Flaccid Paralysis) -------------------------------------------

#recall function
library(ggplot2)
library(tidyverse)
library(dplyr)
library(deSolve)
library(readxl)
library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)
install.packages("deSolve")
library(sf)
library(flextable)

#read data
AFP <- read_excel("c:/Users/sonk/Documents/Kornelius Son/Sintax R/AFP R.xlsx", 
                  sheet = "AFP_CASE")
view(AFP)
print(AFP)
AFP %>% 
     select(No_Epid, sex, umur, Kabupaten) %>% print()


# Time series graph -------------------------------------------------------

# set date
AFP <- AFP %>%
     mutate(tanggal_notifikasi = as.Date(tanggal_notifikasi, format = "%Y-%m-%d"), # Ensure it's Date format
            month = floor_date(tanggal_notifikasi, unit = "month")) # Extract the month

print(AFP$tanggal_notifikasi)

# convert to month
AFP_monthly <- AFP %>%
     group_by(month, klasifikasi_final) %>%
     summarise(count = n(), .groups = 'drop')
str(AFP_monthly$month)

# creating time series graph ----------------------------------------------

ggplot(AFP_monthly, aes(x = month, y = count, fill = klasifikasi_final)) +
     geom_vline(xintercept = as.Date("2024-01-01"), linetype = "solid", color = "black", size = 0.5) +
     geom_col() + # Stacked bars by default
     labs(
          title = "Distribusi kasus AFP (2023-2024)",
          sutbtitle= "berdasarkan klasifikasi akhir",
          x = "Bulan",
          y = "Freq",
          fill = "Klasifikasi Akhir") +
     scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + # Formatting month labels
     theme_minimal() +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotate x-axis labels for readability


library(ggplot2)

ggplot(AFP_monthly, aes(x = month, y = count, fill = klasifikasi_final)) +
     geom_vline(xintercept = as.Date("2024-01-01"), linetype = "solid", color = "black", size = 0.5) +
     geom_col(position = "stack") + # Stacked bars with outlines
     labs(
          title = "Distribusi kasus AFP (2023-2024)",
          subtitle = "Berdasarkan klasifikasi final",
          x = "Bulan",
          y = "Frequency",
          fill = "Klasifikasi Akhir"
     ) +
     scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
     theme_minimal() +
     theme(
          axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid.major = element_line(size = 0.5, color = "gray90"),
          legend.position = "right"
     ) +
     scale_fill_manual(values = c("turquoise4", "salmon")) # Custom colors for categories



# Creating AFP map ------------------------------------------------------------

library(sf)
afp <-st_read("c:/Users/sonk/Documents/Kornelius Son/Sintax R/Kab._Provinsi_Papua.shp")
View(afp)
class(afp)
head(afp)

View(afp)
afp <- afp%>%
     mutate(NPAFP=c(0.0, 27.0, 3.3, 6.0, 7.7, 0.0, 6.0, 0.0, 0.0))
afp <-afp%>%
     mutate(specimen=c(0.0, 77.8, 100.0, 100.0, 60.0, 0.0, 100.0, 0.0, 100.0))
afp <- afp %>%
     mutate(NPAFP_interval = cut(NPAFP, breaks = c(-Inf, 3, Inf),
                                 labels = c("<3/100,000 pop 15 yo", ">=3/100,000 pop 15 yo")))
afp <- afp %>%
     mutate(spec_interval = cut(specimen, breaks = c(-Inf, 80, Inf),
                                labels = c("<80%", ">=80%")))
# Plot NPAFP rate map
ggplot(afp) +
     geom_sf(aes(fill = NPAFP_interval),  
             color = "black", 
             alpha = 1, 
             size = 0.3) +
     # Add labels for Kabupaten
     geom_sf_label(aes(label=Kabupaten), 
                   size = 3, 
                   nudge_x = 0.1, 
                   nudge_y = 0.1, 
                   check_overlap = TRUE, 
                   color = "black") +
     scale_fill_manual (
          values = c("salmon", "turquoise"), 
                       name = "NPAFP Rates") +       
     coord_sf() +
     labs(
          title = "Non Polio AFP Rate",   # Title of the plot
          subtitle = "Minggu 45, 2024",
          fill = "Kabupaten"               # Legend label for NPAFP
     ) +
     
     # Aesthetic theme for minimalistic appearance
     theme_minimal()

# Plot adequate specimen map
ggplot(afp) +
     geom_sf(aes(fill = spec_interval),  
             color = "black", 
             alpha = 1, 
             size = 0.3) +
     # Add labels for Kabupaten
     geom_sf_label(aes(label=Kabupaten), 
                   size = 3, 
                   nudge_x = 0.1, 
                   nudge_y = 0.1, 
                   check_overlap = TRUE, 
                   color = "black") +
     scale_fill_manual (
          values = c("salmon", "turquoise"), 
          name = "NPAFP Rates") +       
     coord_sf() +
     labs(
          title = "Non Polio AFP Rate",   # Title of the plot
          subtitle = "Minggu 45, 2024",
          fill = "Kabupaten"               # Legend label for NPAFP
     ) +
     
     # Aesthetic theme for minimalistic appearance
     theme_minimal()



# Measles-Rubella ---------------------------------------------------------
# import data
campak <- read_excel("c:/Users/sonk/Documents/Kornelius Son/Sintax R/campakR.xlsx", 
                     sheet = "campak")

# convert day to month
monthly_breaks <- seq.Date(from = as.Date("2024-01-01"), # set start time
                           to = as.Date("2024-12-31"), # set end time
                           by = "months") # time distribution by month
monthly_breaks   # print
campak$Tgl_rash <-as.Date(campak$Tgl_rash)

#creating time seris graph
ggplot(data = campak) + 
     geom_histogram(
          mapping = aes(
               x = Tgl_rash,
               group = Final,       # set data to be grouped by hospital
               fill = Final         # bar fill (inside color) by hospital
          ),
          breaks = monthly_breaks,   # sequence of weekly Monday bin breaks for whole outbreak
          closed = "left",          # count cases from start of breakpoint
          color = "black"           # Color around bars
     ) +
     theme(
          text = element_text(family = "Calibri", size = 11),  # Set font family and size
          axis.title = element_text(size = 11),               # Font size for axis titles
          axis.text = element_text(size = 11),                 # Font size for axis text
          plot.title = element_text(size = 11, face = "bold"), # Font size and style for title
          plot.subtitle = element_text(size = 11),             # Font size for subtitle
          plot.caption = element_text(size = 11),
          legend.text = element_text(size = 10),                # Font size for legend text
          legend.title = element_text(size = 12)                # Font size for legend title# Font size for caption
     ) +
     labs(
          title    = "Sebaran Kasus Campak, 2024",
          subtitle = "Onset berdasarkan bulan",
          x        = "Onset Period",
          y        = "Freq",
          caption  = str_glue("n = {nrow(campak)}; Case onsets range from {format(min(campak$Tgl_rash, na.rm = TRUE), format = '%a %d %b %Y')} to {format(max(campak$Tgl_rash, na.rm = TRUE), format = '%a %d %b %Y')}\n{nrow(campak %>% filter(is.na(Tgl_rash)))} cases with missing dates")
     )

# Table summary
summary_table3 <- campak %>%
     select(JK, group, Kabupaten, imunisasi, Final) %>%  # Select relevant columns including the year
     tbl_summary(
          by = Final  # Stratify by the year of 'tanggal_notifikasi'
     ) %>%
     as_flex_table()
print(summary_table3)

summary_table3 %>%
     autofit() %>%  # Ensure one line per row and auto-fit the table layout
     save_as_docx(path = "c:/Users/sonk/Documents/Kornelius Son/Sintax R/summary_table2.docx")


library(usethis)
use_data(afp, overwrite=TRUE)