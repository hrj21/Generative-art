library(generativeart)
library(dplyr)
library(gganimate)

# set the paths
IMG_DIR <- "img/"
IMG_SUBDIR <- "everything/"
IMG_SUBDIR2 <- "handpicked/"
IMG_PATH <- paste0(IMG_DIR, IMG_SUBDIR)

LOGFILE_DIR <- "logfile/"
LOGFILE <- "logfile.csv"
LOGFILE_PATH <- paste0(LOGFILE_DIR, LOGFILE)

# create the directory structure
setup_directories(IMG_DIR, IMG_SUBDIR, IMG_SUBDIR2, LOGFILE_DIR)

# include a specific formula, for example:
my_formula <- list(x = quote(runif(1,-1, 1) * x_i ^ 2 - sin(y_i ^ 2)),
                   y = quote(runif(1,-1, 1) * y_i ^ 3 - cos(x_i ^ 2)))

my_formula2 <- list(x = quote(runif(1,-1, 1) * x_i ^ 2 - sin(y_i ^ 2)),
                   y = quote(runif(1,-1, 1) * y_i ^ 3 - cos(x_i ^ y_i)))

my_formula3 <- list(x = quote(runif(1,-1, 1) * x_i ^ 2 - sin(y_i ^ 2)),
                    y = quote(runif(1,-1, 1) * y_i ^ 3 - cos(abs(x_i) ^ abs(y_i))))

# change data-generating function
generate_data <- function(formula){
  print("generate data")
  df <- seq(from = -pi, to = pi, by = 0.01) %>% 
    expand.grid(x_i = ., y_i = .) %>% 
    dplyr::mutate(!!!formula)
  return(df)
}

# call the main function to create five images with a polar coordinate system
generate_img(
  formula = my_formula2,
  nr_of_img = 5,
  polar = TRUE,
  filetype = "png",
  color = "white",
  background_color = "orange"
)

set.seed(7583)
df <- generate_data(my_formula3) %>%
  mutate(row = row_number())

ggplot(df, aes(x, y)) +
  geom_point(alpha = 0.5, size = 0, shape = 21) +
  coord_polar() +
  theme_void() +
  #transition_manual(row, cumulative = TRUE) +
  transition_events(start = row,
                    end = row + 1000L,
                    enter_length = 20L,
                    exit_length = 20L) +
  enter_grow() +
  exit_fade()

anim_save("img/handpicked/7583_1000L_end.gif" ,animation = last_animation())


ggplot(df, aes(x, y)) +
  geom_point(alpha = 0.5, size = 0, shape = 21) +
  coord_polar() +
  
  theme_void() +
  #transition_manual(row, cumulative = TRUE) +
  transition_events(start = row,
                    end = row + 100000L,
                    enter_length = 20L,
                    exit_length = 20L) +
  enter_grow() +
  exit_fade()

anim_save("img/handpicked/7583_100_000L_end.gif" ,animation = last_animation())
