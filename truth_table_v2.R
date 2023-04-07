# Packages                       ----
cat("\014")
rm(list = ls())
# Package Manager
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,tibble,tidyr,magrittr,rlang,stringr)

# Functions                      ----

convert_conditionality <- \(prem) {
  if((prem %>% stringr::str_count("IMPLIES")) > 1) {
    stop("Only one conditional per premise can be converted.")
  }
  b <-
    prem %>% stringr::str_split("IMPLIES") %>% unlist() %>% stringr::str_squish()
  for (i in 1:length(b)) {
    if (i == 1) {
      b[i] <-
        b[i] %>%
        stringr::str_split("") %>%
        unlist() %>% .[-1] %>%
        stringr::str_flatten()
    } else {
      b[i] <-
        b[i] %>%
        stringr::str_split("") %>%
        unlist() %>% .[-nchar(b[i])] %>%
        stringr::str_flatten()
    }
  }
  return(paste0("NOT ", b[1], " OR ", b[2]))
}

convert_biconditionality <- \(prem) {
  if ((prem %>% stringr::str_count("IFF")) > 1) {
    stop("Only one conditional per premise can be converted.")
  }
  b <-
    prem %>% stringr::str_split("IFF") %>% unlist() %>% stringr::str_squish()
  for (i in 1:length(b)) {
    if (i == 1) {
      b[i] <-
        b[i] %>%
        stringr::str_split("") %>%
        unlist() %>% .[-1] %>%
        stringr::str_flatten()
    } else {
      b[i] <-
        b[i] %>%
        stringr::str_split("") %>%
        unlist() %>% .[-nchar(b[i])] %>%
        stringr::str_flatten()
    }
  }
  return(paste0("(", b[1], " AND ", b[2], ")",
                " OR ", "(NOT ", b[1], " AND NOT ", b[2], ")"))
}

replace_custom_operators <- \(expr) {
  expr <- gsub("AND", "&", expr)
  expr <- gsub("OR" , "|", expr)
  expr <- gsub("NOT", "!", expr)
  expr <- paste0("(",expr,") ;")
  return(expr)
}

truth_tbl_validity <- \(ttbl,prems) {
  out <- ttbl %>% dplyr::ungroup() %>% dplyr::select(!!prems)
  out$prem_val <- out[,-ncol(out)] %>% rowSums()
  out$conc_val <- out[,(ncol(out)-1)] %>% rowSums()
  out %>% 
    dplyr::mutate(
      invalidity = dplyr::if_else(prem_val == (ncol(out) - 3) & conc_val == 0,
                                  TRUE,FALSE),
      validity   = dplyr::if_else(prem_val == (ncol(out) - 3) & conc_val == 1,
                                  TRUE,FALSE
      )) %>% 
    tibble::add_column(
      ttbl %>% dplyr::select(-(!!prems)), .before = 1
    )
}

make_truth_tbl <- \(vars, prems) {
  # Collect the variables
  args <-
    vars %>% lapply(\(var) var = c(F, T)) %>% rlang::set_names(vars)
  # Build initial truth table
  ttbl <- tidyr::crossing(!!!args) %>% dplyr::rowwise()
  # Convert premise statements into equivalents using only AND, NOT, and OR
  for(i in 1:length(prems)) {
    if((prems[i] %>% stringr::str_count("IMPLIES")) > 0) {
      prems[i] <- 
        prems[i] %>% convert_conditionality()
    }
  }
  for(i in 1:length(prems)) {
    if((prems[i] %>% stringr::str_count("IFF")) > 0) {
      prems[i] <- 
        prems[i] %>% convert_biconditionality()
    }
  }
  final_prems <- prems
  # Successively mutate the truth table by evaluating each premise onto it
  for (i in 1:length(final_prems)) {
    ttbl <-
      ttbl %>%
      # https://stackoverflow.com/a/49941635
      dplyr::mutate(!!!rlang::parse_exprs(replace_custom_operators(final_prems[i])))
  }
  prems <- names(ttbl)[(length(vars)+1):ncol(ttbl)]
  return(
    # Clean and return
    ttbl %>%
      dplyr::ungroup() %>%
      dplyr::rename_with( ~ prems, (length(vars) + 1):ncol(.)) %>%
      truth_tbl_validity(prems)
  )
}

is_valid_ttbl <- \(ttbl) {
  ((ttbl$invalidity %>% sum()) <= 0) & 
    ((ttbl$validity %>% sum()) > 0)
}

# Testing                        ----

# regmatches(a, gregexpr("(?=\\().*?(?<=\\))", a, perl=T))[[1]]

# Rules and Verified Experiments ----

# Rules for Statements
# - All statements containing "IMPLIES" must be fully enclosed in parentheses
# - The statements containing "IFF" must be fully enclosed in parentheses
# - Expressions cannot be duplicated: c("(NOT P OR Q)", "NOT P OR Q") is throws error
# - You can reverse it however: c("NOT Q OR P", "NOT P OR Q")
# - The count(vars) can be as many as you please

# The below works
vars <- c("P","Q")
prems <- c(
  "(P AND Q) OR (NOT P AND NOT Q)",
  "(NOT P OR Q)",
  "(P OR NOT Q) AND (Q OR NOT P)",
  "(Q AND NOT P) OR (NOT P)",
  "(P IMPLIES Q)",
  "((P OR Q) IFF (Q AND P))",
  "(P IFF Q)"
)
ttbl <- make_truth_tbl(vars,prems)
ttbl
ttbl %>% is_valid_ttbl()
# The above works
# The below works
vars <- c("B","C","L")
prems <- c(
  "(NOT B OR NOT C)",
  "(NOT (B AND C))",
  "(L OR C)",
  "(L OR NOT B)"
)
ttbl <- vars %>% make_truth_tbl(prems)
ttbl
ttbl %>% is_valid_ttbl()
# The above works
# The below works
vars <- c("P","Q","R","M")
prems <- c(
  "(P OR Q)",
  "((NOT P AND NOT Q) IMPLIES M)",
  "((P OR NOT P) IFF (R AND M))",
  "(Q AND NOT Q)",
  "((P AND R) OR M) IFF NOT (Q AND P)"
)
ttbl <- vars %>% make_truth_tbl(prems)
ttbl
ttbl %>% is_valid_ttbl()
# The above works
# The below works
vars <- c("P","Q","R")
prems <- c(
  "(NOT (P OR (Q AND R)))",
  "(NOT P AND (NOT Q OR NOT R))"
)
ttbl <- vars %>% make_truth_tbl(prems)
ttbl
ttbl %>% is_valid_ttbl()
# The above works
