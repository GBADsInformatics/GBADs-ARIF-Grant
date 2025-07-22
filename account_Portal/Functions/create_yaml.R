library(yaml)



create_zero_mort <- function(current, ideal){
zero_mortality <- current
zero_mortality[names(current)] <- lapply(names(current), function(k) {
  if (startsWith(k, "Alpha_") || startsWith(k, "Health_exp_")) {
    0
  } else if (startsWith(k, "Labour_cost_") || startsWith(k, "lab_non_health")) {
    ideal[[k]]
  } else {
    current[[k]]
  }
})
names(zero_mortality) <- names(current)
return(zero_mortality)
}



# Zero AHE & Morbidity
create_zero_morb <- function(current, ideal) {
zero_morbidity <- ideal
zero_morbidity[names(ideal)] <- lapply(names(ideal), function(k) {
  if (startsWith(k, "Alpha_")) {
    current[[k]]
  } else if (startsWith(k, "Health_exp_")) {
    0
  } else {
    ideal[[k]]
  }
})
names(zero_morbidity) <- names(ideal)
return(zero_morbidity)
}

