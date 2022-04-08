### Writing Group Randomizations
# Alec Robitaille
# Updated: July 2019 [Sept. 2017]


### Instructions ----
# If you have never run this before -
#   you need to authorize R to use your google sheets files
#   run
#googlesheets4::gs4_auth()
# and accept in the browser

# Add the initials or names of submitters + reviewer-only list below
# Hit 'SOURCE' (ctrl/cmd+shift+s) for this week's reviewers.
# If counts are not well balanced, re-run!

# Note, if you use names, it will return names and if you use initials, it will return initials.


### Data ----
suppressMessages(library(data.table))
suppressMessages(library(googlesheets4))
suppressMessages(library(googledrive))

# Load the sheet
ss <- 'https://docs.google.com/spreadsheets/d/1-V9UJqtIPYVwjB3sad8gsOPHCMoFv682l3NX0CkQszg/edit#gid=862084568'
DT <- data.table(read_sheet(ss, skip = 4, sheet = 'sign up'))

### Set reviewers and submitters ----
# Using either initials or first names
submitting <- DT[tolower(Submitting) == 'yes', Name]
notsubmitting <- DT[tolower(Submitting) == 'no' & tolower(Reviewing) == 'yes', Name]

# optionally, print counts
printCounts <- TRUE


### Setup ----
# include guests name = initials here (no need to remove them after).
# then ddd them above in notsubmitting/submitting
guests <- c('Erik' = 'EP')

# team
team <-
  c(
    'Carly' = 'CZ',
    'Gail' = 'GM',
    'Bella' = 'IR',
    'Lingshan' = 'LL',
    'Fatemeh' = 'FI',
    'Emily' = 'EK',
    'Kayleigh' = 'KHT',
    'Serena' = 'SS',
    'Michael' = 'MP',
    'Katie' = 'KP',
    'Taylor' = 'TY',
    'Nicole' = 'NY',
    'Matt' = 'MJ',
    'Maya' = 'MC'
  )

# Use names = initials svp
ltOne <- c(
  'Kieu-Nhi' = 'KNV',
  guests
)
# ------------------------------------------------------------------

### Checks ----
team <- c(team, guests)

if (!all(c(submitting, notsubmitting) %chin% c(team, names(team)))) {
  stop(
    'some names or initials in submitting or notsubmitting not found in team: \n',
    setdiff(c(submitting, notsubmitting), c(team, names(team)))
  )
}

if (anyDuplicated(c(submitting, notsubmitting)) != 0) {
  stop(
    'some names or initials in submitting or notsubmitting are repeated: \n',
    c(submitting, notsubmitting)[duplicated(c(submitting, notsubmitting))]
  )
}

### Processing ----
providedInit <- ifelse(mean(nchar(c(submitting, notsubmitting))) < 3, TRUE, FALSE)

DT <- data.table(name = if (providedInit) team else names(team))

DT[name %chin% submitting, c('submit', 'review') := TRUE]
DT[name %chin% notsubmitting, submit := TRUE]


### Sample ----
tooMany <- length(submitting) * 3 < length(reviewers)

only <- na.omit(DT, cols = c('submit', 'review'))

only[, first := sample(reviewers, .N)]
only[, second := sample(reviewers, .N)]

while (only[name == first, .N] != 0) {
  only[, first := sample(reviewers, .N)]
}

while (only[first == second | name == second, .N] != 0) {
  only[, second := sample(reviewers, .N)]
}

onetwo <- c(only$first, only$second)
more2x <- length(submitting) * 2 < length(reviewers)
someNotAssig <- !all(c(submitting, notsubmitting) %in% onetwo)

assignThird <- more2x | someNotAssig

if (assignThird) {
  only[, third := sample(reviewers, .N)]
  
  while (only[second == third | name == third | first == third, .N] != 0) {
    only[, third := sample(reviewers, .N)]
  }
}


only[first %chin% ltOne, c('first', 'second') := .(second, first)]
only[first %chin% names(ltOne), c('first', 'second') := .(second, first)]

### Counts ----
summ <- data.table(
  assigned = c(onetwo, only$third),
  lvl = c(
    rep('first', length(only$first)),
    rep('second', length(only$second)),
    rep('third', length(only$third))
  )
)

summ[, .N, .(assigned, lvl)]

### Print ----
tooMany <- tooMany | !all(c(submitting, notsubmitting) %in% summ$assigned)


if (tooMany | assignThird) {
  message('NOTE:')
  if (tooMany) {
    message('since there are too many reviewers, some are unassigned: \n', paste(reviewers[!(reviewers %in% summ$assigned)], collapse = ', '), '\n')
  }
  
  if (assignThird) {
    message('since there are >2x reviewers than submitters, \n three reviewers are assigned for each submission. \n')
  }
} else {
  message('NOTE:')
  message('nothing to note, have a great day!')
}

if (assignThird) {
  print(only[, .(name, first, second, third)])
  sheet_write(only[, .(name, first, second, third)],
              ss = ss,
              sheet = 'reviewer list')
} else {
  print(only[, .(name, first, second)])
  sheet_write(only[, .(name, first, second)],
              ss = ss,
              sheet = 'reviewer list')
}


if (printCounts) {
  message('COUNTS:')
  print(summ[, .N, assigned])
}
