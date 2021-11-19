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

**Rules of production/rules of inference**: The rules of production or rules of inference are the rules of a system. Hofstadter refers to them as "symbol-shunting" rules. In our example, they are the rules listed under the [Rules of the game](#2-rules-of-the-game)

**Derivation**:

**Decision procedures**:

## Let's try to use Search to find the solution to this problem


```r
rule.vect <- c(rule.1, rule.2, rule.3, rule.4)

# find the applicable rules to a string
find.applicable.rules <- function(string, rules){
  rule.vect <- rules
  applicable.rules <- c()
  
  for(rule in rule.vect){
    if(rule(string) != string){
      applicable.rules <- append(applicable.rules, rule)
    }
  }
  return(applicable.rules)
}

# apply rules and get results
apply.rules <- function(string, rules){
  results <- c()
  for(rule in rules){
    results <- append(results, rule(string))
  }
  return(results)
}

# search for new theorems
theorem.search <- function(start, rules, depth = 5){
  
  rule.vect <- rules
  theorems <- c(start)
  
  sink(nullfile()) # suppress output
  applicable.rules <- find.applicable.rules(start, rule.vect)
  new.candidate.theorems <- apply.rules(start, rule.vect)
  sink() # end suppressing output
  for(candidate in new.candidate.theorems){
    if(is.element(candidate, theorems) == FALSE){
      theorems <- append(theorems, candidate)
      if(depth > 0){
        theorems.deeper <- theorem.search(candidate, rule.vect, depth = depth-1)
        for(element in theorems.deeper){
          if(is.element(element, theorems) == FALSE){
            theorems <- append(theorems, element)
          }
        }
      }
    }
  }
  return(theorems)
}

theorems <- theorem.search("MI", rule.vect, 5)
theorems
```

```
##  [1] "MI"                                                               
##  [2] "MIU"                                                              
##  [3] "MIUIU"                                                            
##  [4] "MIUIUIUIU"                                                        
##  [5] "MIUIUIUIUIUIUIUIU"                                                
##  [6] "MIUIUIUIUIUIUIUIUIUIUIUIUIUIUIUIU"                                
##  [7] "MIUIUIUIUIUIUIUIUIUIUIUIUIUIUIUIUIUIUIUIUIUIUIUIUIUIUIUIUIUIUIUIU"
##  [8] "MII"                                                              
##  [9] "MIIU"                                                             
## [10] "MIIUIIU"                                                          
## [11] "MIIUIIUIIUIIU"                                                    
## [12] "MIIUIIUIIUIIUIIUIIUIIUIIU"                                        
## [13] "MIIUIIUIIUIIUIIUIIUIIUIIUIIUIIUIIUIIUIIUIIUIIUIIU"                
## [14] "MIIII"                                                            
## [15] "MIIIIU"                                                           
## [16] "MIIIIUIIIIU"                                                      
## [17] "MIIIIUIIIIUIIIIUIIIIU"                                            
## [18] "MIIIIUIIIIUIIIIUIIIIUIIIIUIIIIUIIIIUIIIIU"                        
## [19] "MUIUUIUUIUUIU"                                                    
## [20] "MUIUUIU"                                                          
## [21] "MUIIU"                                                            
## [22] "MUIU"                                                             
## [23] "MIIIIIIII"                                                        
## [24] "MIIIIIIIIU"                                                       
## [25] "MIIIIIIIIUIIIIIIIIU"                                              
## [26] "MIIIIIIIIUIIIIIIIIUIIIIIIIIUIIIIIIIIU"                            
## [27] "MUUIIUUUIIU"                                                      
## [28] "MUUIIU"                                                           
## [29] "MIIIIIIIIIIIIIIII"                                                
## [30] "MIIIIIIIIIIIIIIIIU"                                               
## [31] "MIIIIIIIIIIIIIIIIUIIIIIIIIIIIIIIIIU"                              
## [32] "MUUUUUIU"                                                         
## [33] "MIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII"                                
## [34] "MIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIU"                               
## [35] "MIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII"
## [36] "MUUUUUUUUUUII"                                                    
## [37] "MUUUUUI"                                                          
## [38] "MUUUUUIUUUUUI"                                                    
## [39] "MUI"                                                              
## [40] "MUUII"                                                            
## [41] "MUUIIUUII"                                                        
## [42] "MUUIIUUIIU"                                                       
## [43] "MUUIIUUIIUUIIUUII"
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

Using the theorem.search function, we can discover infinitely many theorems of the MIU system, but we have no way to know if we are getting closer to "MU", and no way to steer the search towards it. To find MU, we may need to approach this problem differently.

## Thinking inside and outside the system


```r
# let's think more abstractly by seeing which rules work with each other:

start <- "MI"
```

