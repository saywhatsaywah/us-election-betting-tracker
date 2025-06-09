# httr: This library provides functions for working with HTTP in R, allowing us to send GET and POST requests to APIs.
library(httr)

# jsonlite: This library is used to parse JSON data, converting it to and from R data frames and lists.
library(jsonlite)

# tidyverse: A collection of R packages designed for data science, including dplyr for data manipulation and ggplot2 for data visualization.
library(tidyverse)

# abettor: This package is specifically designed to interface with the Betfair API, though in this script, we're interacting with the API directly using httr.
library(abettor)


# Define a function to log in to Betfair and obtain a session token

# This function sends a POST request to the Betfair login API endpoint with the user's credentials.
# If the login is successful, it returns a session token which will be used for subsequent API requests.
login_betfair <- function(username, password, applicationKey) {
  
  # The Betfair login endpoint URL
  endpoint_login <- "https://identitysso.betfair.com/api/login"
  
  # Sending a POST request to the login endpoint with the username, password, and application key.
  response <- POST(
    url = endpoint_login,
    body = list(
      username = username,  # The username provided by the user
      password = password   # The password provided by the user
    ),
    encode = "form",  # The form encoding type for sending data in the body of the request
    add_headers(
      "X-Application" = applicationKey,  # Application key provided by Betfair for API access
      "Content-Type" = "application/x-www-form-urlencoded"  # Content type for form submission
    )
  )
  
  # Parsing the response content as JSON
  content <- content(response, as = "parsed", type = "application/json")
  
  # If a token is returned, the login is successful; otherwise, an error message is returned.
  if (!is.null(content$token)) {
    return(content$token)  # Returning the session token
  } else {
    stop("Login failed: ", content$error)  # Stopping the function and displaying an error message if login fails
  }
}

# Use the login function to obtain a session token from Betfair
session_token <- login_betfair(
  username = "YOUR BETFAIR USERNAME",        
  password = "YOUR BETFAIR PASSWORD",      
  applicationKey = "YOUR BETFAIR APPLICATION KEY"  
)

# Print the session token to check if it has been correctly obtained
print(session_token)


# Define the API endpoint for Betfair's betting exchange
endpoint <- "https://api.betfair.com/exchange/betting/json-rpc/v1"

# Define the request body for retrieving market book data
# This JSON string includes the API method to call and parameters such as market IDs and price projections.
request_body <- '[{"jsonrpc": "2.0", "method": "SportsAPING/v1.0/listMarketBook", "params": {"marketIds":["1.176878927"],"priceProjection":{"priceData":["EX_BEST_OFFERS","EX_TRADED"],"virtualise":"true"}}, "id": 1}]'

# Send the POST request to the Betfair API to retrieve market data
response <- POST(
  url = endpoint,  # The API endpoint for the request
  add_headers(
    "X-Application" = "C6qZtMjBFrzlM6x5",  # Application key for Betfair API
    "X-Authentication" = session_token,    # Session token obtained from the login function
    "Content-Type" = "application/json"    # Content type for JSON data
  ),
  body = request_body  # The body of the request containing the JSON payload
)

# Check the response by extracting and converting it to text
response_content <- content(response, as = "text")  
response_json <- fromJSON(response_content)  # Parse the JSON content into an R object

# Print the parsed response to understand its structure
print(response_json)
str(response_json)


# The data in the response contains selection IDs but no names, so a lookup table is created.

# This lookup table maps selection IDs to candidate names, allowing us to interpret the API's response data.
selectionId_table <- tibble(
  selectionId = c(10874213, 12126964),  # The selection IDs returned by the Betfair API
  Name = c("Trump", "Harris")           # Corresponding candidate names for each selection ID
)

# Process the response data and join it with the lookup table to get candidate names
final_betting_dataframe <- (response_json[["result"]][[1]])[["runners"]][[1]] %>% #This is the path to get to the bit I want
  left_join(selectionId_table) %>%  # Join the response data with the lookup table
  mutate(Date = Sys.Date()) %>%  # Add a column with the current date
  select(Date, Name, lastPriceTraded) %>%  # Select relevant columns (Date, Name, and last traded price)
  slice(1:2) %>%  # Take the first two rows (for Trump and Harris)
  pivot_wider(names_from = Name, values_from = lastPriceTraded) %>%  # Reshape data so each candidate has their own column
  mutate(
    `Trump % chance` = 100 / Trump,    # Calculate Trump's percentage chance based on odds
    `Harris % chance` = 100 / Harris   # Calculate Harris's percentage chance based on odds
  ) %>% 
  select(Date, Trump, Harris, `Trump % chance`, `Harris % chance`)  # Select the final columns to display

# The final data frame `final_betting_dataframe` now contains the date, odds, and calculated chances for Trump and Harris.
