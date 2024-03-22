#Transform input to list
transformInput=function(input){
  res = input
  if (is.null(input)){
    res = list()
  }
  #Is vector data type
  else if (is.vector(input) && is.atomic(input)) {
    res = list(input)
  }
  return(res)
}

RobotMoveNew <- function(height, width, treasure, bomb) {

  treasure = transformInput(treasure)
  bomb = transformInput(bomb)
  
  moves <- matrix(0, nrow = 4, ncol = 2)
  moves[1, 1] <- 1 # rozkaz dolu
  moves[2, 1] <- -1 # rozkaz nahoru
  moves[3, 2] <- 1 # rozkaz doprava
  moves[4, 2] <- -1 # rozkaz doleva
  
  object_map <- matrix("empty", nrow = height, ncol = width)
  
  #Add treasures
  for (trs in treasure) {
    object_map[trs[1], trs[2]] <- "treasure"
  }

  for (bmb in bomb) {
    #Add bombs
    object_map[bmb[1], bmb[2]] <- "bomb"
  }
  
  data <- data.frame(state_from = character(), alt = character(), state_to = character(), prob = numeric(), cost = numeric())
  
  for (i_from in 1:height) {
    
    for (j_from in 1:width) {
      
      state_from <- paste0("(", i_from, ", ", j_from, ")")
      
      if (object_map[i_from, j_from] == "empty") {
        
        for (dir_order in 1:4) {
          
          prob_map <- matrix(0, nrow = height, ncol = width)
          
          for (dir_move in 1:4) {
            
            i_move <- i_from + moves[dir_move, 1]
            if (i_move < 1) { i_move <- 1 }
            if (i_move > height) { i_move <- height }
            
            j_move <- j_from + moves[dir_move, 2]
            if (j_move < 1) { j_move <- 1 }
            if (j_move > width) { j_move <- width }
            
            if (dir_order == dir_move) {
              prob_map[i_move, j_move] <- prob_map[i_move, j_move] + 0.7
            } else {
              prob_map[i_move, j_move] <- prob_map[i_move, j_move] + 0.1
            }
            
          }
          
          alt <- c("⇓", "⇑", "⇒", "⇐")[dir_order]
          
          for (i_to in 1:height) {
            
            for (j_to in 1:width) {
              
              state_to <- paste0("(", i_to, ", ", j_to, ")")
              
              cur_prob <- prob_map[i_to, j_to]
              
              if (object_map[i_to, j_to] == "treasure") {
                cur_cost <- 1
              } else {
                cur_cost <- 0
              }
              
              data <- rbind(data, data.frame(state_from = state_from, alt = alt, state_to = state_to, prob = cur_prob, cost = cur_cost))
              
            }
            
            
          }
          
        }
        
      } else {
        
        if (object_map[i_from, j_from] == "treasure") {
          alt <- "😊"
        } else if (object_map[i_from, j_from] == "bomb") {
          alt <- "💣"
        } 
        
        for (i_to in 1:height) {
          
          for (j_to in 1:width) {
            
            state_to <- paste0("(", i_to, ", ", j_to, ")")
            
            if (i_from == i_to && j_from == j_to) {
              cur_prob <- 1
            } else {
              cur_prob <- 0
            }
            
            cur_cost <- 0
            
            data <- rbind(data, data.frame(state_from = state_from, alt = alt, state_to = state_to, prob = cur_prob, cost = cur_cost))
            
          }
          
        }
        
      }
      
    }
    
  }
  
  return(data)
  
}

#height <- 3
#width <- 4
#treasure <- c(1, 4)
#bomb <- c(1, 2)

#data <- RobotMoveNew(height, width, treasure, bomb)

#data2 <- RobotMoveNew(height, width, NULL, bomb)

#data3 <- RobotMoveNew(height, width, list(c(1,2), c(2,4)), bomb)
                      
#data4 <- RobotMoveNew(height, width, list(c(2,4)), bomb)