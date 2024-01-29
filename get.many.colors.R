get.many.colors <- function(n) {
# n is a sequence of numbers corresponding to the list of colors
	
many.colors.name <- matrix(c(
"Pastel Cyan","#6ECFF6",
"Pastel Red","#F7977A",
"Pastel Green","#82CA9D",
"Pastel Violet","#A187BE",
"Pastel Blue","#8493CA",
"Pastel Pea Green","#C4DF9B",
"Pastel Yellow Green","#A2D39C",
"Pastel Cyan Blue","#7EA7D8",
"Pastel Magenta","#F49AC2",
"Pastel Green Cyan","#7BCDC8",
"Pastel Blue Violet","#8882BE",
"Pastel Violet Magenta","#BC8DBF",
"Pastel Magenta Red","#F6989D",
"Pastel Red Orange","#F9AD81",
"Pastel Violet","#A187BE",
"Pastel Green Cyan","#7BCDC8",
"Pastel Yellow","#FFF79A",
"Pastel Yellow Orange","#FDC68A",
"Pastel Blue","#8493CA",
"Pastel Blue Violet","#8882BE",
"Pastel Violet Magenta","#BC8DBF",
"Pastel Magenta Red","#F6989D",
"Pastel Yellow","#FFF79A"
),ncol=2,byrow=TRUE)

#if (length(n)>1) pastel.out <- pastel.name[1:n,]
#if (length(n)==1) pastel.out <- pastel.name[n,]

many.colors.out <- many.colors.name[n,]
many.colors.out
}

