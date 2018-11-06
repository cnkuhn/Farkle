# Questions:
# What is average number of points in a single turn? Different for risk levels?
# What are the chances of FARKLE in a single turn? Different for risk levels?
# How many turns does is take to get to 10,000 points? Different for risk levels?
# On average, how many rolls in a turn? Different for risk levels?

roll <- function() { 
  ceiling(6 * runif(1))
}
throw <- function(p=6){
  replicate(p, roll())
}

# ADD --> IF 4 OF A KIND (OR 3 OR 5) AND OTHER DICE ARE ALL 1S OR 5S, SCORE ALL DICE AND THEN ROLL ALL 6 AGAIN
points <- function(x){
  tab <- table(x)
  if(length(names(tab)) == 6){                             # 1-2-3-4-5-6
    pts <- 3000
    ndice <- 6
    return(c(pts, ndice))
  }
  if(sum(c(4,2) %in% tab) == 2 | sum(tab == 2) == 3){      # 4-of-a-kind and pair OR 3 pairs
    pts <- 1500
    ndice <- 6 # number of dice left to roll
    return(c(pts, ndice))
  }
  if(length(names(tab)[which(tab==6)]) > 0){               # 6-of-a-kind
    pts <- 3000
    ndice <- 6
    return(c(pts, ndice))
  }
  if(length(names(tab)[which(tab==5)]) > 0){               # 5-of-a-kind
    pts <- 2000
    ndice <- length(x) - 5
    return(c(pts, ndice))
  }
  if(length(names(tab)[which(tab==4)]) > 0){               # 4-of-a-kind
    pts <- 1000
    ndice <- length(x) - 4
    return(c(pts, ndice))
  }
  if(length(names(tab)[which(tab==3)]) > 0){               # 3-of-a-kinds (might have 2)
    kind3 <- as.numeric(names(tab)[which(tab==3)])
    if(length(kind3 == 2)){
      pts <- 2500
      ndice <- 6
    }else{
      pts <- ifelse(kind3 == 1, 1000, kind3 * 100)
      ndice <- 3
    }
    return(c(pts, ndice))
  }
  if(1 %in% x){                                            # 1s - always keep all 1s
    pts <- sum(x==1) * 100
    ndice <- length(x) - sum(x==1)
    return(c(pts, ndice))
  }
  if(5 %in% x){                                            # 5s - always only keep 1
    pts <- 50
    ndice <- length(x) - 1
    return(c(pts, ndice))
  }else{
    pts <- 0
    ndice <- 0
    return(c(pts, ndice))
  }
}

# Function `turn()` returns a list of 4 lists:
  # [[1]] --> x: dice values for each roll
  # [[2]] --> pts: number of points for each roll
  # [[3]] --> ndice: number of dice to roll next
  # [[4]] --> turn_points: how many points total earned for turn (0 means FARKLE!)

turn <- function(risk_level = 3){
  x <- list()
  pts <- list()
  ndice <- list()
  i <- 1 # i is the roll number we are on in the turn
  # First throw of 6 dice
  (x[[1]] <- throw(6))
  (pts[[1]] <- points(x[[1]])[1])
  (ndice[[1]] <- points(x[[1]])[2])
  (roll_again <- ifelse(pts[[1]] > 0 & ndice[[1]] >= risk_level, TRUE, FALSE))
  # Next throws - keep rolling until points = 0 (FARKLE) or less than risk_level dice to roll
  while(roll_again == TRUE){
    i <- i + 1
    x[[i]] <- throw(ndice[[i-1]])
    pts[[i]] <- points(x[[i]])[1]
    ndice[[i]] <- ifelse(points(x[[i]])[2] == 0 & pts[[i]] != 0, 6, points(x[[i]])[2]) # If ndice[[i]] == 0 but pts[[i]] is not 0, ndice <- 6
    roll_again <- ifelse(pts[[i]] > 0 & ndice[[i]] >= risk_level, TRUE, FALSE)
  }
  (turn_points <- ifelse(pts[[i]] != 0, sum(unlist(pts)), 0))
  (turn_res <- ifelse(turn_points == 0, "FARKLE!", turn_points))
  out <- list(x, pts, ndice, turn_points)
  names(out) <- c("x", "pts", "ndice", "turn_points")
  return(out)
}

# What is average number of points in a single turn? Different for risk levels?
set.seed(1234)
nsim <- 10000
turns1 <- replicate(nsim, turn(risk_level = 3))
turn_pts1 <- unlist(turns1[seq(4, length(turns1), by = 4)])
turns2 <- replicate(nsim, turn(risk_level = 2))
turn_pts2 <- unlist(turns2[seq(4, length(turns2), by = 4)])

summary(turn_pts1)
summary(turn_pts2)

par(mfrow = c(1, 2))
boxplot(turn_pts1, ylim = c(0, 8000), main = "Low Risk", outline = FALSE)
boxplot(turn_pts2, ylim = c(0, 8000), main = "High Risk", outline = FALSE)

# What are the chances of FARKLE in a single turn? Different for risk levels?
(f_chance1 <- length(turn_pts1[which(turn_pts1 == 0)]) / nsim)
(f_chance2 <- length(turn_pts2[which(turn_pts2 == 0)]) / nsim)

# How many turns does is take to get to 10,000 points? Different for risk levels?
farkle <- function(risk_level = 3){
  game_points <- 0
  j <- 0
  while(game_points < 10000){
    j <- j + 1
    t <- turn(risk_level = risk_level)
    tp <- t[[4]]
    game_points <- game_points + tp
  }
  return(j)
}
set.seed(1234)
nsim <- 10000
nturns1 <- replicate(nsim, farkle(risk_level = 3))
nturns2 <- replicate(nsim, farkle(risk_level = 2))
summary(nturns1)
summary(nturns2)
par(mfrow=c(1,2))
boxplot(nturns1, ylim = c(0,35), main = "Low Risk", outline = FALSE)
boxplot(nturns2, ylim = c(0,35), main = "High Risk", outline = FALSE)

# On average, how many rolls in a turn? Different for risk levels?
nrolls1 <- unlist(lapply(turns1[seq(1, length(turns1), by = 4)], function(x) {length(x)}))
nrolls2 <- unlist(lapply(turns2[seq(1, length(turns1), by = 4)], function(x) {length(x)}))
summary(nrolls1)
summary(nrolls2)
par(mfrow=c(1,2))
boxplot(nrolls1, ylim = c(0,11), main = "Low Risk", outline = FALSE)
boxplot(nrolls2, ylim = c(0,11), main = "High Risk", outline = FALSE)






