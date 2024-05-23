# Shiny Application

This is a Shiny application.

## Installation

To install the required packages, you can use the `DESCRIPTION` file.

### Using DESCRIPTION File

1. Install the `devtools` package if you don't have it:

    ```r
    install.packages("devtools")
    ```

2. Install the dependencies:

    ```r
    devtools::install_deps(dependencies = TRUE)
    ```
    
### Alternative

### Installing Dependencies Individually

If you prefer to install each dependency individually, use the following commands in R:

1. Installing the packages:

    ```r
    install.packages("shiny")
    install.packages("shinydashboard")
    install.packages("visNetwork")
    install.packages("htmlwidgets")
    ```


## Running the Application

To run the application, use the following command in R:

```r
shiny::runApp()
```

