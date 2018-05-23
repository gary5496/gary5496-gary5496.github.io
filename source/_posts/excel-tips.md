---
title: Excel Tips
date: 2018-03-06 13:43:46
tags: [Windows]
categories: Windows
---
Below are some tips for Microsoft Excel collected during my daily work, which may be helpful for you.

### Add leading zeros to a numeric value

	=TEXT(A1,"0000000000")

### Add leading zeros to any value

	=RIGHT(REPT("0",10)&A2,10)

The symbol `&` will be used to concatenate the strings. The function `REPT` will repeat the character for times specified.

### Look up a value from a range

	G5=VLOOKUP("string",B2:F200,2,FALSE)

The above example will look up the value `string` from the first column `B` and the current row `5` of the range `B2:F206`, and the value of `C5`, which is specified by the 3rd parameter `2`, will be returned. The fourth parameter `FALSE` means exact match.

### Convert an error value to blank

	=IFERROR(VLOOKUP(B251,A$1:I$360,8,FALSE),"")

Since the `VLOOKUP` function may return an error value if the look-up is failed, we can use the `IFERROR` function to convert the error value to blank.

### Get a substring from a string

	=MID(C2,2,3)

The above example will get the substring from string C2. It will begin at the second position, and the length will be 3.


### Summarize the value by some conditions

	=SUMIFS(A:A,B:B,">10", B:B,"<20")

The above example will summarize the values for A column, when the value in column B is greater than 10 and less than 20.
