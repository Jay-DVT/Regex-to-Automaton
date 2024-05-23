library(shiny)
library(shinydashboard)
library(visNetwork)

ui <- fluidPage(
  titlePanel("Regular Grammar into Automaton Converter"),
  sidebarLayout(
    sidebarPanel(
      helpText("While the user types in a grammar, the program shows the corresponding automaton in real time.",
               "The grammar input format is X -> Y where X is the antecedent and Y is the consequent. Examples: S -> aA, A -> bB, B -> z.",
               "The initial state is denoted with S. In the automaton diagram, the initial state S needs to be green colored.",
               "The final state Z needs to be colored in red."),
      helpText("Example user input:"),
      helpText("S -> aA"),
      textAreaInput("regex", label = "Input your Regular Expression", value = "
S -> aA
S -> bA
A -> bB
A -> a
B -> aA
B -> bA", rows = 10, cols = 40)
    ),
    mainPanel(
      box(
        title = "Automaton",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        style = "padding: 15px; background-color: #f9f9f9; border: 1px solid #d3d3d3; border-radius: 10px;",
        visNetworkOutput("automatonPlot")
      ),
      fluidRow(
        column(
          width = 6,
          box(
            title = "Grammar Connections",
            status = "primary",
            solidHeader = TRUE,
            tableOutput("connectionsTable")
          )
        ),
        column(
          width = 6,
          box(
            title = "Errors",
            status = "danger",
            solidHeader = TRUE,
            style = "color: red;",
            textOutput("errorOutput")
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  
  parseGrammar <- function(grammarText) {
    lines <- unlist(strsplit(grammarText, "\n"))
    lines <- lines[lines != ""]
    rules <- lapply(lines, function(line) {
      parts <- unlist(strsplit(line, " -> "))
      if (length(parts) != 2 || parts[2] == "") {
        return(NULL)
      }
      list(antecedent = parts[1], consequent = parts[2])
    })
    rules <- rules[!sapply(rules, is.null)]
    return(rules)
  }
  
  checkMultipleOutgoingPaths <- function(connections) {
    errorMessages <- NULL
    transitions <- list()
    
    for (i in 1:nrow(connections)) {
      antecedent <- connections[i, "Antecedent"]
      inputChar <- substr(connections[i, "Consequent"], 1, 1)
      key <- paste(antecedent, inputChar, sep = "-")
      
      if (key %in% names(transitions)) {
        errorMessages <- c(errorMessages, paste("Node", antecedent, "has multiple outgoing paths for input", inputChar))
      } else {
        transitions[key] <- TRUE
      }
    }
    
    if (!is.null(errorMessages)) {
      return(paste(errorMessages, collapse = "\n"))
    } else {
      return(NULL)
    }
  }
  
  parsedGrammar <- reactive({
    req(input$regex)
    parseGrammar(input$regex)
  })
  
  generateConnections <- function(rules) {
    connections <- do.call(rbind, lapply(rules, function(rule) {
      data.frame(Antecedent = rule$antecedent, Consequent = rule$consequent, stringsAsFactors = FALSE)
    }))
    return(connections)
  }
  
  generateNodesEdges <- function(connections) {
    nodes <- unique(c(connections$Antecedent, unlist(lapply(connections$Consequent, function(c) {
      if (nchar(c) == 1) {
        return("Z")
      } else {
        return(substr(c, 2, 2))
      }
    }))))
    nodes <- data.frame(id = nodes, label = nodes, stringsAsFactors = FALSE)
    nodes$color <- ifelse(nodes$id == "S", "green", ifelse(nodes$id == "Z", "red", "lightblue"))
    
    edgeList <- list()
    for (i in 1:nrow(connections)) {
      rule <- connections[i, ]
      from <- rule$Antecedent
      if (nchar(rule$Consequent) == 1) {
        to <- "Z"
        label <- rule$Consequent
      } else {
        to <- substr(rule$Consequent, 2, 2)
        label <- substr(rule$Consequent, 1, 1)
      }
      edgeKey <- paste(from, to, sep = "-")
      if (edgeKey %in% names(edgeList)) {
        edgeList[[edgeKey]]$label <- paste(edgeList[[edgeKey]]$label, label, sep = ", ")
      } else {
        edgeList[[edgeKey]] <- data.frame(from = from, to = to, label = label, stringsAsFactors = FALSE)
      }
    }
    
    edges <- do.call(rbind, edgeList)
    list(nodes = nodes, edges = edges)
  }
  
  output$connectionsTable <- renderTable({
    req(parsedGrammar())
    generateConnections(parsedGrammar())
  })
  
  output$automatonPlot <- renderVisNetwork({
    req(parsedGrammar())
    connections <- generateConnections(parsedGrammar())
    errorMessages <- checkMultipleOutgoingPaths(connections)
    if (!is.null(errorMessages)) {
      return(NULL)
    }
    graphData <- generateNodesEdges(connections)
    visNetwork(graphData$nodes, graphData$edges) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE) %>%
      visPhysics(stabilization = FALSE) %>%
      visPhysics(solver = "forceAtlas2Based") %>%
      visLayout(improvedLayout = TRUE)
  })
  
  output$errorOutput <- renderText({
    req(parsedGrammar())
    connections <- generateConnections(parsedGrammar())
    errorMessages <- checkMultipleOutgoingPaths(connections)
    
    if (!is.null(errorMessages)) {
      return(errorMessages)
    }
    
    if (length(connections) == 0) {
      return("Error: No valid rules found.")
    }
    
    invalidLines <- which(sapply(parsedGrammar(), is.null))
    if (length(invalidLines) > 0) {
      return(paste("Error: Invalid input in lines:", paste(invalidLines, collapse = ", ")))
    }
  })
}

shinyApp(ui = ui, server = server)
