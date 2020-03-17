# Code to generate and display boggle boards in R
# Using recent versions of R, random number generators should behave the same across platforms
# So multiple players can play across any distance 
#   by choose a shared number for the seed, and calling the function on their own computer.
#   (although check this by generating a board and comparing). Each seed number creates a new board.

# boggle4() creates a 4x4 board, boggle5() creates a 5x5 board
# textsize() is an optional argument to make the letters bigger or smaller

#Bogglemaker:
boggle4=function(seed,
                 textsize=2){
  die=data.frame(
    d0=c('R','I','F','O','B','X'),
    d1=c('I','F','E','H','E','Y'),
    d2=c('D','E','N','O','W','S'),
    d3=c('U','T','O','K','N','D'),
    d4=c('H','M','S','R','A','O'),
    d5=c('L','U','P','E','T','S'),
    d6=c('A','C','I','T','O','A'),
    d7=c('Y','L','G','K','U','E'),
    d8=c('Qu','B','M','J','O','A'),
    d9=c('E','H','I','S','P','N'),
    d10=c('V','E','T','I','G','N'),
    d11=c('B','A','L','I','Y','T'),
    d12=c('E','Z','A','V','N','D'),
    d13=c('R','A','L','E','S','C'),
    d14=c('U','W','I','L','R','G'),
    d15=c('P','A','C','E','C','D')
  )
  ## operations:
  set.seed(seed)
  rolls=apply(die,2,function(x)sample(x,1))
  ord=sample(1:16,replace=F)
  board=matrix(rolls[ord], nrow=4, ncol=4)
  plot(0,0,
       type='n',
       ylim=c(0,4),
       xlim=c(0,4),
       xlab='',
       xaxt='n',
       yaxt='n',
       ylab='',
       bty='n')
  grid(lty=1, col='darkgrey')
  at=expand.grid((0:3)+.5,(0:3)+.5)
  text(x=at[,1], y=at[,2],
       labels=board, cex=textsize)
}

boggle5=function(seed,
                 textsize=2){
  die=data.frame(
    c('A','A','A','F','R','S'),
    c('A','A','E','E','E','E'),
    c('A','A','F','I','R','S'),
    c('A','D','E','N','N','N'),
    c('A','E','E','E','E','M'),
    c('A','E','E','G','M','U'),
    c('A','E','G','M','N','N'),
    c('A','F','I','R','S','Y'),
    c('B','J','K','Q','X','Z'),
    c('C','C','N','S','T','W'),
    c('C','E','I','I','L','T'),
    c('C','E','I','L','P','T'),
    c('C','E','I','P','S','T'),
    c('D','H','H','N','O','T'),
    c('D','H','H','L','O','R'),
    c('D','H','L','N','O','R'),
    c('D','D','L','N','O','R'),
    c('E','I','I','I','T','T'),
    c('E','M','O','T','T','T'),
    c('E','N','S','S','S','U'),
    c('F','I','P','R','S','Y'),
    c('G','O','R','R','V','W'),
    c('H','I','P','R','R','Y'),
    c('N','O','O','T','U','W'),
    c('O','O','O','T','T','U')
  )
  set.seed(seed)
  rolls=apply(die,2,function(x)sample(x,1))
  ord=sample(1:25,replace=F)
  board=matrix(rolls[ord], nrow=5, ncol=5)
  plot(0,0,
       type='n',
       ylim=c(0,5),
       xlim=c(0,5),
       xlab='',
       xaxt='n',
       yaxt='n',
       ylab='',
       bty='n')
  grid(lty=1, col='darkgrey')
  at=expand.grid((0:4)+.5,(0:4)+.5)
  text(x=at[,1], y=at[,2],
       labels=board, cex=textsize)
}


boggle4(5)
boggle5(5)
