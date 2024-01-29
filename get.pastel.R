get.pastel <- function(n) {
# n is a sequence of numbers corresponding to the list of colors
	
pastel.name <- matrix(c(
"Pastel Red","#F7977A",
"Pastel Cyan","#6ECFF6",
"Pastel Green","#82CA9D",
"Pastel Cyan Blue","#7EA7D8",
"Pastel Pea Green","#C4DF9B",
"Pastel Magenta","#F49AC2",
"Pastel Yellow Orange","#FDC68A",
"Pastel Yellow Green","#A2D39C",
"Pastel Violet","#A187BE",
"Pastel Green Cyan","#7BCDC8",
"Pastel Blue","#8493CA",
"Pastel Blue Violet","#8882BE",
"Pastel Violet Magenta","#BC8DBF",
"Pastel Magenta Red","#F6989D",
"Pastel Yellow","#FFF79A",
"Pastel Red Orange","#F9AD81"

),ncol=2,byrow=TRUE)

#if (length(n)>1) pastel.out <- pastel.name[1:n,]
#if (length(n)==1) pastel.out <- pastel.name[n,]

pastel.out <- pastel.name[n,]
pastel.out
}

