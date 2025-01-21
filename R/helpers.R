
# Left Pad 0 - Leading Zero
LP0 <-
  function(x, width){
    # like CONVERSION_EXIT_MATN1_INPUT
    # only add leading zero's in case it is a number
    is_num <- grepl("^[0-9]+$", x)
    ifelse(
      is_num,
      stringr::str_pad(string = x, width = width, side = "left", pad = "0"),
      x
    )
  }

MATN1 <-
  function(x){
    LP0(x, 18)
  }

