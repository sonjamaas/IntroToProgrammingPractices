# install.package('remotes')
remotes::install_github('coolbutuseless/wordle')

puzzle <- wordle::WordleHelper$new(nchar = 5)
head(puzzle$words, 20)

# wordle can be played here: https://www.nytimes.com/games/wordle/index.html
# update the word list with your chosen word
puzzle$update("abbed", c('yellow', 'grey', 'green', 'yellow', 'grey'))
head(puzzle$words, 20)
