# Simple ggplot2 heatmap
# with colorBrewer "spectral" palette

doInstall <- FALSE  # Change to FALSE if you don't want packages installed.
toInstall <- c("ggplot2", "reshape2", "RColorBrewer")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)
require('rje') #cubeHelix colours
require('reshape2')
# Generate a random matrix
# This can be any type of numeric matrix,
# though we often see heatmaps of square correlation matrices.


rebin <- function(myData, binW, binL) {
  # Set up starting matrix
  b <- data.matrix(myData)
  # Convert data frame to matrix.
  matDims <- dim(b)

  print(dim(b))

  # Check and change matrix length to be a multple of the bin factors
  if (matDims[1] %% binL > 0) {
    x <- floor(matDims[1] / binL)
    lengthDiff <- matDims[1] - x * binL
    delRange <- ((matDims[1]+1)-lengthDiff):matDims[1]
    b <- b[-(delRange),]
  }
  print(dim(b))


  #get the new dimenions
  matDims <- dim(b)


  # Get the Means of the matrix reshaped to the bin factor
  b <- colMeans(matrix(b, nrow=binL),na.rm=TRUE)

  # reshape matrix and transpose back
  b <- matrix(b, nrow=matDims[1]/binL)


  matDims <- dim(myData)
  # Check and change matrix width to be a multple of the bin factors
  if (matDims[2] %% binW > 0) {
    x <- floor(matDims[2] / binW)
    lengthDiff <- matDims[2] - x * binW
    delRange <- ((matDims[2]+1)-lengthDiff):matDims[1]
    b <- b[,-(delRange)]
  }

  #get the new dimenions
  matDims <- dim(myData)

  # Get the Means of the matrix reshaped to the bin factor
  b <- rowMeans(matrix(b, ncol=binW), na.rm=TRUE)
  # reshape matrix and transpose back
  b <- matrix(b, ncol=matDims[2]/binW)



  return(b)
}
heatmap <- function(myData, binSize){
  nRow <- dim(myData)[1]
  nCol <- dim(myData)[2]

  myData <- data.matrix(myData)

  #myData <- matrix(mapfile, ncol = nCol)
  rownames(myData) <- c(1:nRow)
  colnames(myData) <- c(1:nCol)

  # # Replace with numbers that actually have a relationship:
  # for(ii in 2:ncol(myData)){  myData[, ii] <- myData[, ii-1] + rnorm(nrow(myData))  }
  # for(ii in 2:nrow(myData)){  myData[ii, ] <- myData[ii-1, ] + rnorm(ncol(myData))  }


  # For melt() to work seamlessly, myData has to be a matrix.
  longData <- melt(myData)
  head(longData, 20)

  # Optionally, reorder both the row and column variables in any order
  # Here, they are sorted by mean value
  # longData$Var1 <- factor(longData$Var1, names(sort(with(longData, by(value, Var1, mean)))))
  # longData$Var2 <- factor(longData$Var2, names(sort(with(longData, by(value, Var2, mean)))))

  # Define palette
  #myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
  #myPalette <- cubeHelix(250, start=0.3,r=-0.5,hue=2, gamma=1 )
  myPalette <- cubeHelix(250, start=0.,r=0.5,hue=2, gamma=1 )

  zp1 <- ggplot(longData,
                aes(x = Var2, y = Var1, fill = value))
  zp1 <- zp1 + geom_tile()
  zp1 <- zp1 + scale_fill_gradientn(colours = myPalette)
  # zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
  zp1 <- zp1 + scale_x_continuous(breaks = seq(from=0,to=371, by = 20))
  # zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
  zp1 <- zp1 + scale_y_continuous(breaks = seq(from=0,to=383, by = 20))
  zp1 <- zp1 + coord_equal()
  zp1 <- zp1 + theme_bw() +
  labs(title="18-07-2014 Thickness map of Au film on Pyrex on micrux electrode",
       x = "x / µm",
       y = "y / µm")
  print(zp1)  # Your plot will look different, depending on the seed
}

mapData <- read.table('2dmap.txt')


heatmap(rebin(mapData,4,4))
ggsave('18-07-2014 Thickness map of Au film on Pyrex on micrux electrode 4x4 bin.pdf')
