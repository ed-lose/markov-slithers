
library(tidyverse)
library(shiny)
library(bslib)

simulate.mc = function(tm) {
  
  # vector of states over time t
  states = c()
  
  # initialise variable for first state 
  states[1] = 1
  
  t = 1
  while (states[t] != nrow(tm)) {
    t = t + 1
    # probability vector to simulate next state X_{t+1}
    p = tm[states[t-1], ]
    
    # draw from multinomial and determine state
    states[t] <-  which(rmultinom(1, 1, p) == 1)
  }
  return(states)
}

ui <- page_fluid(
  titlePanel("Snakes and Ladders Simulator"),
  layout_sidebar(
    sidebar = sidebar(
      numericInput(
        inputId = "d",
        label = "Number of sides on dice:",
        min = 1,
        value = 6,
        step = 1,
        updateOn = "blur"
      ),
      numericInput(
        inputId = "runs",
        label = "Number of simualted runs:",
        min = 1,
        value = 20,
        step = 1,
        updateOn = "blur"
      ),
      actionButton("reset", "Reset")
    ),
    plotOutput("exPlot"),
    plotOutput("simPlot")
  )

  
)

server <- function(input, output) {
  
  # generate board with user inputs
  cal_board = reactive({
    n = 100
    
    if (input$d <= 1) {
      validate("Number of sides must be greater than 1")
    }
    
    d = input$d
    
    ladders = list(c(6,23),c(8,30),c(13,47),c(20,39),c(33,70),
                   c(37,75),c(41,62),c(57,83),c(66,89),c(77,96))
    snakes = list(c(27,10),c(55,16),c(61,14),c(69,50),c(79,5),
                  c(81,44),c(87,31),c(91,25),c(95,49),c(97,59))
    board = matrix(0,n,n)
    
    for (i in 1:n) {
      for (j in 0:min(n-i,d-1)) {
        board[i,j+i] = 1/d 
      } 
    }
    
    board = board %>%  
      cbind(rep(0,n), .) %>% 
      rbind(c(rep(0,n),1))
    
    board[,n+1][max(1,(n-(d-1))):n] = (max(1,1+d-n):d)/d
    
    rownames(board) = 0:(n)
    colnames(board) = 0:(n)
    delete = c()
    
    for (i in ladders) {
      for (j in (i[1]-(d-1)):i[1]) {
        board[j,][i[2]+1] = board[j,][i[2]+1] + 1/d
      }
      delete = c(delete, i[1])
    }
    
    for (i in snakes) {
      for (j in (i[1]-(d-1)):i[1]) {
        board[j,][i[2]+1] = board[j,][i[2]+1] + 1/d
      }
      delete = c(delete, i[1])
    }
    
    list(board = board[-(delete+1),-(delete+1)])
  })
  
  # reset
  observeEvent(input$reset, {
    updateNumericInput(inputId = "d", value = 6)
    updateNumericInput(inputId = "runs", value = 20)
  })
  
  # expected values
  output$exPlot = renderPlot({
    
    board = cal_board()$board
    
    fundamental = solve(diag(nrow(board)-1) - board[1:(nrow(board)-1),1:(nrow(board)-1)])
    ex_steps = fundamental %*% matrix(1,nrow(board)-1,1)
    
    plot(ex_steps, xlab="State", ylab="Expected steps to win", main="Number of turns to win based on state")
  })
  
  # simulated runs
  output$simPlot = renderPlot({
    board = cal_board()$board
    games = list()
    run_time = c()
    
    for (i in 1:input$runs) {
      current_game = simulate.mc(board)
      run_time = c(run_time, length(current_game)-1)
      games[[i]] = current_game
    }
    
    longest = which.max(run_time)
    shortest = which.min(run_time)
    
    plot(games[[longest]], type="l", col="red", xlab="Turns", ylab="State",
         main=paste(input$runs,"simulated runs of Snakes and Ladders"))
    lines(games[[shortest]], col="blue")
    for (i in 2:length(games)) {
      if (!(i == longest | i == shortest)) {
        lines(games[[i]], col=alpha("black",0.1))
      }
    }
  })
}

shinyApp(ui = ui, server = server)
