################################################################################
# Functions
################################################################################

##Look
## for viewing information about a dataset

look <- function(field){
  
  field_class <- class(field)
  
  field_n_distinct <- length(unique(field))
  
  field_na <- sum(is.na(field))
  
  data.frame(class = field_class,
             n_distinct = field_n_distinct,
             na = field_na)
  
}



##create_hist
## creates a basic histogram

create_hist <- function(df, .x){
  
  df %>% 
    ggplot(aes(x = .data[[.x]])) +
    geom_histogram()
  
  
}


##bar_counts
## creates bar charts based on x and y values

bar_counts <- function(df, .x){
  
  df %>% 
    mutate(field = as.character(.data[[.x]])) %>% 
    count(field) %>% 
    mutate(field = fct_reorder(field, n)) %>% 
    ggplot(aes(x = n,
               y = field)) +
    geom_col() +
    theme_minimal() +
    labs(title = .x)
  
}

##crease_scatter
## creates a scatter plot comparing numeric values to determine a relationship

create_scatter <- function(df, .x, .y){
  
  df %>% 
    ggplot(aes(x = .data[[.x]],
               y = .data[[.y]])) +
    geom_point() +
    theme_minimal()
  
}

