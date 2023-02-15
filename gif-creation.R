library(readxl)
library(tidyverse)
library(gganimate)
library(magick)
library(ggthemes)

ap_participation <- read_xls(path = "data/Annual-AP-Program-Participation-1956-2020_1.xls")

# Modified column names because they were '...1', '...2', etc
colnames(ap_participation) <- c("year", "schools", "students",
                                "no data", "exams", "colleges")

# Selected needed columns since 'no data' was just empty
ap_participation = subset(ap_participation, select = c("year", "schools", "students", "exams", "colleges"))

# Kept only rows that contain data, got rid of empty rows and rows with text
ap_participation <- ap_participation[-c(1,2,50,51,70:76),]

# Changed 'year' rows from something like '1955-56' to '1955' so I could plot the year in an organized
# manner without too much clutter. Later, I will add 1 to each row in 'year' to get the year the exams were taken.
# This is because the exams are given in May, at the end of the school year, and the original data included the whole school year. 
ap_participation <- ap_participation |>
  mutate(year = substring(year, 1, 4))

# Changed the data types of the rows from chr to dbl, so the graphs function properly and so I can add 1 to 'year'
ap_participation$year <- as.double(ap_participation$year)
ap_participation$schools <- as.double(ap_participation$schools)
ap_participation$students <- as.double(ap_participation$students)
ap_participation$exams <- as.double(ap_participation$exams)
ap_participation$colleges <- as.double(ap_participation$colleges)

# This is where I added 1 to all the rows in 'year' (explanation on line 24-25)
ap_participation <- ap_participation |>
  mutate(year = year + 1)

# Blank string so no variable is plotted on the x-axis
# Reason: Only y is needed to be plotted for all the plots, not a y ~ x graph
name <- ""

# Creation of four individual plots that I will later combine
# sch for schools, stu for students, exa for exams, & col for colleges
sch <- ap_participation |>
  ggplot(aes(x = name, y = schools)) +
    geom_col(show.legend = FALSE, fill = "red3") +
      theme_clean() +
        transition_states(year,
                          transition_length = 2,
                          state_length = 1,
                          wrap = FALSE) +
          labs(title = "Year: {closest_state}",
               subtitle = "Schools: {scales::label_comma(accuracy = 1)(filter(ap_participation, year == closest_state) %>% pull(schools))}",
               y = "Schools",
               x = "Number of Schools\noffering AP Exams") +
            scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 6)) +
              theme(plot.background = element_blank(), plot.subtitle = element_text(size = 18, face = "plain"),
                    plot.title = element_text(size = 21), axis.text = element_text(size = 15), axis.title = element_text(size = 17))

stu <- ap_participation |>
  ggplot(aes(x = name, y = students)) +
    geom_col(show.legend = FALSE, fill = "darkgreen") +
      theme_clean() +
        transition_states(year,
                          transition_length = 2,
                          state_length = 1,
                          wrap = FALSE) +
          labs(title = "",
               subtitle = "Students: {scales::label_comma(accuracy = 1)(filter(ap_participation, year == closest_state) %>% pull(students))}",
               y = "Students",
               x = "Number of Students\ntaking AP Exams") +
            scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 6)) +
              theme(plot.background = element_blank(), plot.subtitle = element_text(size = 18, face = "plain"),
                    plot.title = element_text(size = 21), axis.text = element_text(size = 15), axis.title = element_text(size = 17))

exa <- ap_participation |>
  ggplot(aes(x = name, y = exams)) +
    geom_col(show.legend = FALSE, fill = "royalblue4") +
      theme_clean() +
        transition_states(year,
                          transition_length = 2,
                          state_length = 1,
                          wrap = FALSE) +
          labs(title = "Exams: {scales::label_comma(accuracy = 1)(filter(ap_participation, year == closest_state) %>% pull(exams))}",
               y = "Exams",
               x = "Number of Exams\nadministered by AP", 
               caption = "\n\n") +
            scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 6)) +
              theme(plot.background = element_blank(), plot.title = element_text(size = 18, face = "plain"),
                    axis.text = element_text(size = 15), axis.title = element_text(size = 17))

col <- ap_participation |>
  ggplot(aes(x = name, y = colleges)) +
    geom_col(show.legend = FALSE, fill = "goldenrod2") +
      theme_clean() +
        transition_states(year,
                          transition_length = 2,
                          state_length = 1,
                          wrap = FALSE) +
          labs(title = "Colleges: {scales::label_comma(accuracy = 1)(filter(ap_participation, year == closest_state) %>% pull(colleges))}",
               y = "Colleges",
               x = "Number of Colleges\naccepting AP Credit",
               caption = "\nCollege Board: ANNUAL AP\nPROGRAM PARTICIPATION 1956-2020") +
            scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 6)) +
              theme(plot.background = element_blank(), plot.title = element_text(size = 18, face = "plain"),
                    axis.text = element_text(size = 15), axis.title = element_text(size = 17))

# Animated the gifs separately, will combine afterward
sch_gif <- animate(sch, width = 300, height = 450, nframes = 65)
stu_gif <- animate(stu, width = 300, height = 450, nframes = 65)
exa_gif <- animate(exa, width = 300, height = 450, nframes = 65)
col_gif <- animate(col, width = 300, height = 450, nframes = 65)

# Creating new variables so I can display gifs before and after using animate()
sch_mgif <- image_read(sch_gif)
stu_mgif <- image_read(stu_gif)
exa_mgif <- image_read(exa_gif)
col_mgif <- image_read(col_gif)

# Combining the gifs so I can have them side-by-side
new_gif1 <- image_append(c(sch_mgif[1], stu_mgif[1]))
for(i in 1:65){
  combined <- image_append(c(sch_mgif[i], stu_mgif[i]))
  new_gif1 <- c(new_gif1, combined)}

new_gif2 <- image_append(c(exa_mgif[1], col_mgif[1]))
for(i in 1:65){
  combined <- image_append(c(exa_mgif[i], col_mgif[i]))
  new_gif2 <- c(new_gif2, combined)}

# Saving the new gifs
anim_save("new1.gif", animation = new_gif1)
anim_save("new2.gif", animation = new_gif2)