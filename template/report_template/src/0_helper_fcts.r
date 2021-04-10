# Include all helper functions in this section. 


# Header with colors ------------------------------------------------------

fct_hl1 <- function(level = 3, text){
    cat(rep("#", level) %>% paste(collapse=""))
    cat('<span style="background-color:#d1e8c5">', 
        text, '</span>')
}

fct_hl2 <- function(level = 3, text){
    cat(rep("#", level) %>% paste(collapse=""))
    cat('<span style="background-color:#c5dee8">', 
        text, '</span>')
}



# Add newline -------------------------------------------------------------

fct_nl <- function(){
    cat("\n\n\\newline\n")
    cat(writeLines(""))
}


# Pandoc table wrapper ----------------------------------------------------

fct_make_table <- function(dat,...){
    dat %>%
        as.data.frame() %>%
        pandoc.table(split.tables = Inf,
                     multi.line = TRUE, ...)
    nl()
}


# thousand separator wrapper ----------------------------------------------

fct_thousand_sep <-  . %>% format(big.mark=",", scientific=FALSE)



# Cacluate Spearman rank correlation and its 95% CI -----------------------

spearman_ci <- function(data, x, y){
    # x - our measure
    # y - a vector of external measures
    temp <- data %>% select(one_of(c(x, y))) %>%
        na.omit()
    
    n <- dim(temp)[1]   # sample size
    
    r_sp <- cor(temp[, x], temp[, y], 
                use = "pairwise.complete.obs", 
                method = "spearman") %>% as.vector()   # Spearman correlation
    
    r_sp_uppr <- tanh(atanh(r_sp) + 1.96*sqrt(1/(n-3))) %>% as.vector()   # 95% CI upper bound 
    r_sp_lowr <- tanh(atanh(r_sp) - 1.96*sqrt(1/(n-3))) %>% as.vector()  # 95% CI lower bound
    
    return(data.frame(spearman_cor = r_sp,
                      ci_uppr95 = r_sp_uppr,
                      ci_lowr95 = r_sp_lowr))
}
