

rm(list = ls())

library(tictoc) # to time things out; each gif takes 1-3 minutes
library(dplyr)  # for case_when calls
library(tweenr) # for animations

# note: to make the final gif files, this script calls two system programs:
# 1. convert (http://imagemagick.org/script/index.php)
# 2. gifsicle (https://www.lcdf.org/gifsicle/man.html)

order_cats <- function (cat_vec) {
  output <- rep(0, length(cat_vec))
  levels <- sort(unique(cat_vec))
  for(i in 1:length(levels)) {
    tar <- which(cat_vec == levels[i])
    output[tar] <- 1:length(tar) + max(output)
  }
  output[is.na(cat_vec)] <- NA
  return(output)
}

dir_init <- function(path, verbose=FALSE, overwrite=TRUE){
  if(substr(path, 1, 2)!='./') stop('path argument must be formatted
    with "./" at beginning')
  contents <- dir(path, recursive=TRUE)
  if(dir.exists(path)){
    if(overwrite){
      if(verbose){
        if(length(contents)==0) print(paste('folder ', path, ' created.', sep=""))
        if(length(contents)>0) print(paste('folder ', path, ' wiped of ',
          length(contents), ' files/folders.', sep=""))
      }
      if(dir.exists(path)) unlink(path, recursive=TRUE)
      dir.create(path)
    }
  } else {
    if(verbose){
      print(paste('folder ', path, ' created.', sep=""))
    }
    dir.create(path)
  }
}

############

d <- read.csv("./citation-data-simulated.csv", stringsAsFactors = FALSE)

print("set parameters of figures")

dot_size <- 0.8
row_spacer <- 1.0
ball_spacer <- 0.03
ymax <- 4

global_mar <- c(4.1, 0, 0, 0)

row1_col <- "black"
row2_col <- "seagreen"
row3_col <- "orangered"
row4_col_a <- "firebrick1"
row4_col_b <- "firebrick3"

n_frames <- 100

row1_base <- row_spacer * 3
row2_base <- row_spacer * 2
row3_base <- row_spacer * 1
row4_base <- row_spacer * 0

print("calculate properties of the balls")

d$frame1_col <- row1_col

d$frame2_col <- case_when(
  !d$downloaded ~ row1_col,
  d$downloaded ~ row4_col_a
)

d$frame3_col <- case_when(
  !d$downloaded & !d$contacted ~ row1_col,
  d$downloaded ~ row4_col_a,
  !d$downloaded & d$contacted ~ row2_col
)

d$frame4_col <- case_when(
  !d$downloaded & !d$contacted ~ row1_col,
  d$downloaded ~ row4_col_a,
  !d$downloaded & d$contacted & !d$reply_received ~ row2_col,
  !d$downloaded & d$contacted & d$reply_received ~ row3_col
)

d$frame5_col <- case_when(
  !d$downloaded & !d$contacted ~ row1_col,
  d$downloaded ~ row4_col_a,
  !d$downloaded & d$contacted & !d$reply_received ~ row2_col,
  !d$downloaded & d$contacted & d$reply_received & !d$data_received ~ row3_col,
  !d$downloaded & d$contacted & d$reply_received & d$data_received ~ row4_col_b
)

d$row1_cat <- case_when(
  !d$downloaded & !d$contacted ~ 1,
  !d$downloaded &  d$contacted ~ 2,
   d$downloaded & !d$contacted ~ 3
)

d$row2_cat <- case_when(
  d$contacted & !d$reply_received ~ 1,
  d$contacted & d$reply_received ~ 2
)

d$row3_cat <- case_when(
  d$reply_received & !d$data_received ~ 1,
  d$reply_received & d$data_received ~ 2,
)

d$row4_cat <- case_when(
   d$downloaded ~ 1,
   d$data_received ~ 2
)

d$row1_pos <- NA
d$row2_pos <- NA
d$row3_pos <- NA
d$row4_pos <- NA

year_list <- sort(unique(d$year))

for (i in 1:length(year_list)) {
  year_cites <- which(d$year == year_list[i])
  d$row1_pos[year_cites] <- order_cats(d$row1_cat[year_cites])
  d$row2_pos[year_cites] <- order_cats(d$row2_cat[year_cites])
  d$row3_pos[year_cites] <- order_cats(d$row3_cat[year_cites])
  d$row4_pos[year_cites] <- order_cats(d$row4_cat[year_cites])
}

d$y_frame1 <- row1_base + ball_spacer * d$row1_pos

d$y_frame2 <- row1_base + ball_spacer * d$row1_pos
d$y_frame2[d$downloaded] <- row4_base + ball_spacer * d$row4_pos[d$downloaded]

d$y_frame3 <- row1_base + ball_spacer * d$row1_pos
d$y_frame3[d$contacted] <- row2_base + ball_spacer * d$row2_pos[d$contacted]
d$y_frame3[d$downloaded] <- row4_base + ball_spacer * d$row4_pos[d$downloaded]

d$y_frame4 <- row1_base + ball_spacer * d$row1_pos
d$y_frame4[d$contacted] <- row2_base + ball_spacer * d$row2_pos[d$contacted]
d$y_frame4[d$reply_received] <- row3_base + ball_spacer * d$row3_pos[d$reply_received]
d$y_frame4[d$downloaded] <- row4_base + ball_spacer * d$row4_pos[d$downloaded]

d$y_frame5 <- row1_base + ball_spacer * d$row1_pos
d$y_frame5[d$contacted] <- row2_base + ball_spacer * d$row2_pos[d$contacted]
d$y_frame5[d$reply_received] <- row3_base + ball_spacer * d$row3_pos[d$reply_received]
d$y_frame5[d$data_received] <- row4_base + ball_spacer * d$row4_pos[d$data_received]
d$y_frame5[d$downloaded] <- row4_base + ball_spacer * d$row4_pos[d$downloaded]


print("make keyframes")

dir_init("./figures")

png("./figures/keyframe_0.png", height = 3.5, width = 5, units = "in", res = 200)

par(mar = global_mar)
# fun fact: unless par is set within the png command's boundaries, it wont work!

plot(1, 1, xlim = c(1955, 2022), ylim = c(0, ymax),
  axes = FALSE, keyframe.plot = FALSE, xlab = "year of publication", ylab = "")

axis(1, at = c(seq(1960, 2010, by = 10), 2018))

lines(c(1960, 2018), c(row1_base, row1_base), col = "gray")
lines(c(1960, 2018), c(row2_base, row2_base), col = "gray")
lines(c(1960, 2018), c(row3_base, row3_base), col = "gray")
lines(c(1960, 2018), c(row4_base, row4_base), col = "gray")

dev.off()



png("./figures/keyframe_1.png", height = 3.5, width = 5, units = "in", res = 200)

par(mar = global_mar)

plot(1, 1, xlim = c(1955, 2022), ylim = c(0, ymax),
  axes = FALSE, keyframe.plot = FALSE, xlab = "year of publication", ylab = "")

axis(1, at = c(seq(1960, 2010, by = 10), 2018))

lines(c(1960, 2018), c(row1_base, row1_base), col = "gray")
lines(c(1960, 2018), c(row2_base, row2_base), col = "gray")
lines(c(1960, 2018), c(row3_base, row3_base), col = "gray")
lines(c(1960, 2018), c(row4_base, row4_base), col = "gray")

points(d$year, d$y_frame1, col = d$frame1_col, pch = 20, cex = dot_size)

dev.off()



png("./figures/keyframe_2.png", height = 3.5, width = 5, units = "in", res = 200)

par(mar = global_mar)

plot(1, 1, xlim = c(1955, 2022), ylim = c(0, ymax),
  axes = FALSE, keyframe.plot = FALSE, xlab = "year of publication", ylab = "")

axis(1, at = c(seq(1960, 2010, by = 10), 2018))

lines(c(1960, 2018), c(row1_base, row1_base), col = "gray")
lines(c(1960, 2018), c(row2_base, row2_base), col = "gray")
lines(c(1960, 2018), c(row3_base, row3_base), col = "gray")
lines(c(1960, 2018), c(row4_base, row4_base), col = "gray")

points(d$year, d$y_frame1, col = gray(0.9), pch = 20, cex = dot_size)
points(d$year, d$y_frame2, pch = 20, cex = dot_size, col = d$frame2_col)

dev.off()



png("./figures/keyframe_3.png", height = 3.5, width = 5, units = "in", res = 200)

par(mar = global_mar)

plot(1, 1, xlim = c(1955, 2022), ylim = c(0, ymax),
  axes = FALSE, keyframe.plot = FALSE, xlab = "year of publication", ylab = "")

axis(1, at = c(seq(1960, 2010, by = 10), 2018))

lines(c(1960, 2018), c(row1_base, row1_base), col = "gray")
lines(c(1960, 2018), c(row2_base, row2_base), col = "gray")
lines(c(1960, 2018), c(row3_base, row3_base), col = "gray")
lines(c(1960, 2018), c(row4_base, row4_base), col = "gray")

points(d$year, d$y_frame1, pch = 20, cex = dot_size, col = gray(0.9))
points(d$year, d$y_frame2, pch = 20, cex = dot_size, col = gray(0.9))
points(d$year, d$y_frame3, pch = 20, cex = dot_size, col = d$frame3_col)

dev.off()



png("./figures/keyframe_4.png", height = 3.5, width = 5, units = "in", res = 200)

par(mar = global_mar)

plot(1, 1, xlim = c(1955, 2022), ylim = c(0, ymax),
  axes = FALSE, keyframe.plot = FALSE, xlab = "year of publication", ylab = "")

axis(1, at = c(seq(1960, 2010, by = 10), 2018))

lines(c(1960, 2018), c(row1_base, row1_base), col = "gray")
lines(c(1960, 2018), c(row2_base, row2_base), col = "gray")
lines(c(1960, 2018), c(row3_base, row3_base), col = "gray")
lines(c(1960, 2018), c(row4_base, row4_base), col = "gray")

points(d$year, d$y_frame1, pch = 20, cex = dot_size, col = gray(0.9))
points(d$year, d$y_frame2, pch = 20, cex = dot_size, col = gray(0.9))
points(d$year, d$y_frame3, pch = 20, cex = dot_size, col = gray(0.9))
points(d$year, d$y_frame4, pch = 20, cex = dot_size, col = d$frame4_col)

dev.off()




png("./figures/keyframe_5.png", height = 3.5, width = 5, units = "in", res = 200)

par(mar = global_mar)

plot(1, 1, xlim = c(1955, 2022), ylim = c(0, ymax),
  axes = FALSE, keyframe.plot = FALSE, xlab = "year of publication", ylab = "")

axis(1, at = c(seq(1960, 2010, by = 10), 2018))

lines(c(1960, 2018), c(row1_base, row1_base), col = "gray")
lines(c(1960, 2018), c(row2_base, row2_base), col = "gray")
lines(c(1960, 2018), c(row3_base, row3_base), col = "gray")
lines(c(1960, 2018), c(row4_base, row4_base), col = "gray")

points(d$year, d$y_frame1, pch = 20, cex = dot_size, col = gray(0.9))
points(d$year, d$y_frame2, pch = 20, cex = dot_size, col = gray(0.9))
points(d$year, d$y_frame3, pch = 20, cex = dot_size, col = gray(0.9))
points(d$year, d$y_frame4, pch = 20, cex = dot_size, col = gray(0.9))
points(d$year, d$y_frame5, pch = 20, cex = dot_size, col = d$frame5_col)

dev.off()


print("create animated transitions")

dir_init("./temp")

tic("created keyframe transition 0 to 1")

fall_times <- d$row1_pos + round(rnorm(nrow(d), 0, 5))
fall_times <- fall_times + abs(min(fall_times))

add1 <- data.frame(year = d$year, y = 5, col = d$frame1_col,
  time = min(fall_times), id = 1:nrow(d), ease = "cubic-in", stringsAsFactors = FALSE)
add2 <- data.frame(year = d$year, y = d$y_frame1, col = d$frame1_col,
  time = fall_times, id = 1:nrow(d), ease = "linear", stringsAsFactors = FALSE)
add3 <- data.frame(year = d$year, y = d$y_frame1, col = d$frame1_col,
  time = max(fall_times) + 10, id = 1:nrow(d), ease = "linear", stringsAsFactors = FALSE)

df <- rbind(add1, add2, add3)

dt <- tween_elements(df, 'time', 'id', 'ease', nframes = n_frames)

dt$col <- as.character(dt$col)

for(i in 1:n_frames){

  png(paste0("./temp/ani",  sprintf("%04d", i), ".png"),
    height = 3.5, width = 5, units = "in", res = 200)

  par(mar = global_mar)

  plot(1, 1, xlim = c(1955, 2022), ylim = c(0, ymax),
    axes = FALSE, keyframe.plot = FALSE, xlab = "year of publication", ylab = "")

  axis(1, at = c(seq(1960, 2010, by = 10), 2018))

  lines(c(1960, 2018), c(row1_base, row1_base), col = "gray")
  lines(c(1960, 2018), c(row2_base, row2_base), col = "gray")
  lines(c(1960, 2018), c(row3_base, row3_base), col = "gray")
  lines(c(1960, 2018), c(row4_base, row4_base), col = "gray")

  tar <- which(dt$.frame == i)

  points(dt$year[tar], dt$y[tar], col = dt$col[tar], pch = 20, cex = dot_size)

#  text(1965, 3.5,"total sample:\n 560 citations", col = row1_col, pos = 4, cex = 0.9)

  dev.off()

}

system("convert -loop 1 -delay 2 ./temp/ani* ./figures/keyframe_0_1_trans.gif")

toc()


tic("created keyframe transition 1 to 2")

dir_init("./temp")

# set fall times
fall_times <- round(rnorm(nrow(d), 0, 5))
fall_times <- fall_times + abs(min(fall_times))
tar <- which(!is.na(d$row4_pos))
fall_times[tar] <- fall_times[tar] + d$row4_pos[tar]

add1 <- data.frame(year = d$year, y = d$y_frame1, col = d$frame1_col,
  time = min(fall_times), id = 1:nrow(d), ease = "cubic-in", stringsAsFactors = FALSE)
add2 <- data.frame(year = d$year, y = d$y_frame2, col = d$frame2_col,
  time = fall_times, id = 1:nrow(d), ease = "linear", stringsAsFactors = FALSE)
add3 <- data.frame(year = d$year, y = d$y_frame2, col = d$frame2_col,
  time = max(fall_times) + 10, id = 1:nrow(d), ease = "linear", stringsAsFactors = FALSE)

df <- rbind(add1, add2, add3)

dt <- tween_elements(df, 'time', 'id', 'ease', nframes = n_frames)

dt$col <- as.character(dt$col)

for(i in 1:n_frames){

  png(paste0("./temp/ani",  sprintf("%04d", i), ".png"),
    height = 3.5, width = 5, units = "in", res = 200)

  par(mar = global_mar)

  plot(1, 1, xlim = c(1955, 2022), ylim = c(0, ymax),
    axes = FALSE, keyframe.plot = FALSE, xlab = "year of publication", ylab = "")

  axis(1, at = c(seq(1960, 2010, by = 10), 2018))

  lines(c(1960, 2018), c(row1_base, row1_base), col = "gray")
  lines(c(1960, 2018), c(row2_base, row2_base), col = "gray")
  lines(c(1960, 2018), c(row3_base, row3_base), col = "gray")
  lines(c(1960, 2018), c(row4_base, row4_base), col = "gray")

  tar <- which(dt$.frame == i)

  points(d$year, d$y_frame1, col = gray(0.9), pch = 20, cex = dot_size)
  points(dt$year[tar], dt$y[tar], col = dt$col[tar], pch = 20, cex = dot_size)

  dev.off()

}

system("convert -delay 2 -loop 1 ./temp/ani* ./figures/keyframe_1_2_trans.gif")

toc()



tic("created keyframe transition 2 to 3")

dir_init("./temp")

# set fall times
fall_times <- round(rnorm(nrow(d), 0, 5))
fall_times <- fall_times + abs(min(fall_times))
tar <- which(!is.na(d$row2_pos))
fall_times[tar] <- fall_times[tar] + d$row2_pos[tar]

add1 <- data.frame(year = d$year, y = d$y_frame2, col = d$frame2_col,
  time = min(fall_times), id = 1:nrow(d), ease = "cubic-in", stringsAsFactors = FALSE)
add2 <- data.frame(year = d$year, y = d$y_frame3, col = d$frame3_col,
  time = fall_times, id = 1:nrow(d), ease = "linear", stringsAsFactors = FALSE)
add3 <- data.frame(year = d$year, y = d$y_frame3, col = d$frame3_col,
  time = max(fall_times) + 10, id = 1:nrow(d), ease = "linear", stringsAsFactors = FALSE)

df <- rbind(add1, add2, add3)

dt <- tween_elements(data = df, time = 'time', group = 'id', ease = 'ease', nframes = n_frames)

dt$col <- as.character(dt$col)

for(i in 1:n_frames){

  png(paste0("./temp/ani",  sprintf("%04d", i), ".png"),
    height = 3.5, width = 5, units = "in", res = 200)

  par(mar = global_mar)

  plot(1, 1, xlim = c(1955, 2022), ylim = c(0, ymax),
    axes = FALSE, keyframe.plot = FALSE, xlab = "year of publication", ylab = "")

  axis(1, at = c(seq(1960, 2010, by = 10), 2018))

  lines(c(1960, 2018), c(row1_base, row1_base), col = "gray")
  lines(c(1960, 2018), c(row2_base, row2_base), col = "gray")
  lines(c(1960, 2018), c(row3_base, row3_base), col = "gray")
  lines(c(1960, 2018), c(row4_base, row4_base), col = "gray")

  tar <- which(dt$.frame == i)

  points(d$year, d$y_frame1, col = gray(0.9), pch = 20, cex = dot_size)
  points(d$year, d$y_frame2, col = gray(0.9), pch = 20, cex = dot_size)

  points(dt$year[tar], dt$y[tar], col = dt$col[tar], pch = 20, cex = dot_size)

  dev.off()

}

system("convert -loop 1 -delay 2 ./temp/ani* ./figures/keyframe_2_3_trans.gif")

toc()



tic("created keyframe transition 3 to 4")

dir_init("./temp")

# set fall times
fall_times <- round(rnorm(nrow(d), 0, 5))
fall_times <- fall_times + abs(min(fall_times))
tar <- which(!is.na(d$row3_pos))
fall_times[tar] <- fall_times[tar] + d$row3_pos[tar]

add1 <- data.frame(year = d$year, y = d$y_frame3, col = d$frame3_col,
  time = min(fall_times), id = 1:nrow(d), ease = "cubic-in", stringsAsFactors = FALSE)
add2 <- data.frame(year = d$year, y = d$y_frame4, col = d$frame4_col,
  time = fall_times, id = 1:nrow(d), ease = "linear", stringsAsFactors = FALSE)
add3 <- data.frame(year = d$year, y = d$y_frame4, col = d$frame4_col,
  time = max(fall_times) + 10, id = 1:nrow(d), ease = "linear", stringsAsFactors = FALSE)

df <- rbind(add1, add2, add3)

dt <- tween_elements(df, 'time', 'id', 'ease', nframes = n_frames)

dt$col <- as.character(dt$col)

for(i in 1:n_frames){

  png(paste0("./temp/ani",  sprintf("%04d", i), ".png"), height = 3.5, width = 5, units = "in", res = 200)

  par(mar = global_mar)

  plot(1, 1, xlim = c(1955, 2022), ylim = c(0, ymax),
    axes = FALSE, keyframe.plot = FALSE, xlab = "year of publication", ylab = "")

  axis(1, at = c(seq(1960, 2010, by = 10), 2018))

  lines(c(1960, 2018), c(row1_base, row1_base), col = "gray")
  lines(c(1960, 2018), c(row2_base, row2_base), col = "gray")
  lines(c(1960, 2018), c(row3_base, row3_base), col = "gray")
  lines(c(1960, 2018), c(row4_base, row4_base), col = "gray")

  tar <- which(dt$.frame == i)

  points(d$year, d$y_frame1, col = gray(0.9), pch = 20, cex = dot_size)
  points(d$year, d$y_frame2, col = gray(0.9), pch = 20, cex = dot_size)
  points(d$year, d$y_frame3, col = gray(0.9), pch = 20, cex = dot_size)
  points(dt$year[tar], dt$y[tar], col = dt$col[tar], pch = 20, cex = dot_size)

  dev.off()

}

system("convert -loop 1 -delay 2 ./temp/ani* ./figures/keyframe_3_4_trans.gif")

toc()



tic("created keyframe transition 4 to 5")

dir_init("./temp")

# set fall times
fall_times <- round(rnorm(nrow(d), 0, 5))
fall_times <- fall_times + abs(min(fall_times))
tar <- which(!is.na(d$row4_pos))
fall_times[tar] <- fall_times[tar] + d$row4_pos[tar]

add1 <- data.frame(year = d$year, y = d$y_frame4, col = d$frame4_col,
  time = min(fall_times), id = 1:nrow(d), ease = "cubic-in", stringsAsFactors = FALSE)
add2 <- data.frame(year = d$year, y = d$y_frame5, col = d$frame5_col,
  time = fall_times, id = 1:nrow(d), ease = "linear", stringsAsFactors = FALSE)
add3 <- data.frame(year = d$year, y = d$y_frame5, col = d$frame5_col,
  time = max(fall_times) + 10, id = 1:nrow(d), ease = "linear", stringsAsFactors = FALSE)

df <- rbind(add1, add2, add3)

dt <- tween_elements(df, 'time', 'id', 'ease', nframes = n_frames)

dt$col <- as.character(dt$col)

for(i in 1:n_frames){

  png(paste0("./temp/ani",  sprintf("%04d", i), ".png"),
    height = 3.5, width = 5, units = "in", res = 200)

  par(mar = global_mar)

  plot(1, 1, xlim = c(1955, 2022), ylim = c(0, ymax),
    axes = FALSE, keyframe.plot = FALSE, xlab = "year of publication", ylab = "")

  axis(1, at = c(seq(1960, 2010, by = 10), 2018))

  lines(c(1960, 2018), c(row1_base, row1_base), col = "gray")
  lines(c(1960, 2018), c(row2_base, row2_base), col = "gray")
  lines(c(1960, 2018), c(row3_base, row3_base), col = "gray")
  lines(c(1960, 2018), c(row4_base, row4_base), col = "gray")

  tar <- which(dt$.frame == i)

  points(d$year, d$y_frame1, col = gray(0.9), pch = 20, cex = dot_size)
  points(d$year, d$y_frame2, col = gray(0.9), pch = 20, cex = dot_size)
  points(d$year, d$y_frame3, col = gray(0.9), pch = 20, cex = dot_size)
  points(d$year, d$y_frame4, col = gray(0.9), pch = 20, cex = dot_size)
  points(dt$year[tar], dt$y[tar], col = dt$col[tar], pch = 20, cex = dot_size)

  dev.off()

}

system("convert -loop 1 -delay 2 ./temp/ani* ./figures/keyframe_4_5_trans.gif")

toc()



unlink("./temp", recursive = TRUE)

# combine gifs into a single gif
system("gifsicle ./figures/keyframe_0_1_trans.gif ./figures/keyframe_1_2_trans.gif ./figures/keyframe_2_3_trans.gif ./figures/keyframe_3_4_trans.gif ./figures/keyframe_4_5_trans.gif --colors 256 --no-loopcount > ./figures/merged.gif")
