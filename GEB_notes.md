---
title: "GEB notes and experiments"
author: "Phileas Dazeley Gaist"
date: "9/22/2021"
output: 
  html_document:
    keep_md: true
---



# The MU-puzle (Chapter 1, p.33)

Here, we use a post-production system (created in the 1920s by American Logician Emil Post), a kind of formal system, to describe and solve a puzzle, the MU-puzzle.

'A **formal system** is an abstract structure used for inferring theorems from **axioms** according to a set of **rules.** These rules, which are used for carrying out the inference of theorems from axioms, are the logical calculus of the formal system. A formal system is essentially an "axiomatic system".' [Formal system, Wikipedia](https://en.wikipedia.org/wiki/Formal_system)

In this formal system, we start with a string, in this case "MI", and the goal to change our string to "MU". To do so, we have four rules at our disposal. Since the only characters allowed in this system are "M", "I", and "U", we will call it the MIU system.

## The rules of the game


```r
# Rule 1: If your string ends with "I", add a "U" to the end.
rule.1 <- function(string){
  new.string <- string
  
  if(substr(string, nchar(string), nchar(string)) == "I"){
    new.string <- paste(string, "U", sep = "")
  }
  return(new.string)
}

# Rule 2: If your string contains "M[n other letters]" you can add the [n other letters] to the end of the string.
rule.2 <- function(string){
  new.string <- string
  
  if(grepl("M", substr(string, 1, nchar(string)-1))){
    M.index <- grep("M", string)[1]
    new.string = paste(new.string, substr(new.string, M.index[1]+1, nchar(new.string)), sep = "")
    print(new.string)
  }
  return(new.string)
}

# Rule 3: If your string contains "III", you can replace "III" occurrences with "U"
rule.3 <- function(string){
  new.string <- string
  
  if(grepl("III", string) == TRUE){
    new.string <- str_replace_all(string, "III", "U")
  }
  return(new.string)
}

# Rule 4: If your string contains "UU", you can drop them from the string
rule.4 <- function(string){
  new.string <- string
  
  if(grepl("UU", string) == TRUE){
    new.string <- str_replace_all(string, "UU", "")
  } 
  return(new.string)
}
```

## Useful terminology:

**Theorem**: In trying to find solutions to this puzzle, we inevitably start applying rules to our starting string to create new strings. Within the formal structure of this puzzle, each string producible by the rules is a **theorem**. In formal systems, theorems are simply strings of symbols; rather than "proven", they are simply "produced" according to certain rules. The problem of this puzzle can therefore be expressed as the question: "Is "MU" a theorem of the MIU system?".

**Axiom**: "An axiom, postulate or assumption is a statement that is taken to be true, to serve as a premise or starting point for further reasoning and arguments." [Axiom, wikipedia](https://en.wikipedia.org/wiki/Axiom)



## Let's try to use Search to find the solution to this problem.


```r
rule.vect <- c(rule.1, rule.2, rule.3, rule.4)

# find the applicable rules to a string
find.applicable.rules <- function(string, rules){
  rule.vect <- rules
  applicable.rules <- c()
  
  for(rule in rule.vect){
    if(rule(string) != string){
      applicable.rules <- c(applicable.rules, rule)
    }
  }
  return(applicable.rules)
}

# apply rules and get results
apply.rules <- function(string, rules){
  results <- c()
  for(rule in rules){
    results <- c(results, rule(string))
  }
  return(results)
}

# search for new theorems
theorem.search <- function(start, rules, depth = 5){
  
  rule.vect <- rules
  theorems <-c(start)
  
  # check which rules rules apply to the start string:
  applicable.rules <- find.applicable.rules(tail(theorems, 1), rule.vect)
  # apply the applicable rules to the start string and save results as vector of candidate theorems:
  new.candidate.theorems <- apply.rules(tail(theorems, 1), rule.vect)
  print(paste("Candidates")); print(new.candidate.theorems)
  print(paste("Theorems before appending:")); print(theorems)
  # for every candidate, check if it has already been discovered
  for(candidate in new.candidate.theorems){
    # if the candidate has not been discovered, add it to the theorems vector
    if(is.element(candidate, theorems) == FALSE){
      theorems <- c(theorems, candidate)
      # if the depth parameter of the current function iteration is superior to zero, search new theorems from derivations 
      #if(depth > 0){
        #theorem.search(candidate, rule.vect, depth = depth-1)
      #}
    }
    print(paste("Theorems after appending:")); print(theorems)
  }
  return(theorems)
}

theorems <- theorem.search("MI", rule.vect, 5)
```

```
## [1] "MII"
## [1] "MII"
## [1] "Candidates"
## [1] "MIU" "MII" "MI"  "MI" 
## [1] "Theorems before appending:"
## [1] "MI"
## [1] "Theorems after appending:"
## [1] "MI"  "MIU"
## [1] "Theorems after appending:"
## [1] "MI"  "MIU" "MII"
## [1] "Theorems after appending:"
## [1] "MI"  "MIU" "MII"
## [1] "Theorems after appending:"
## [1] "MI"  "MIU" "MII"
```

```r
# check theorems vector for duplicates
as.logical(anyDuplicated(theorems))
```

```
## [1] FALSE
```

```r
# check theorems vector for goal string
is.element("MU", theorems)
```

```
## [1] FALSE
```
Using the theorem search function we can discover infinitely many theorems of the MIU system, but we have no way to know if we are getting closer to "MU", and no way to steer the search towards it. To find MU, we may need to approach this problem differently.

##


```r
# let's think more abstractly by seeing which rules work with each other:

start <- "MI"
```

