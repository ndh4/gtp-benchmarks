#lang racket

(require "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         "../../../ctcs/configurable.rkt")

(provide)

(provide color? line-type line? expected-stations station? line-families)

(define/ctc-helper (color? str) (member str '("blue" "green" "orange" "red")))

(define/ctc-helper
 line-type
 '("E" "D" "C" "B" "Mattapan" "Braintree" "orange" "blue"))

(define/ctc-helper
 line-families
 `(("red" "Braintree" "Mattapan")
   ("green" "B" "C" "D" "E")
   ("orange")
   ("blue")))

(define/ctc-helper
 (line? str)
 (member
  str
  '("E"
    "D"
    "C"
    "B"
    "Mattapan"
    "Braintree"
    "orange"
    "blue"
    "line"
    "line1"
    "line2")))

(define/ctc-helper
 expected-stations
 (remove-duplicates
  '("Wonderland Station"
    "Revere Beach Station"
    "Beachmont Station"
    "Suffolk Downs Station"
    "Orient Heights Station"
    "Wood Island Station"
    "Airport Station"
    "Maverick Station"
    "Aquarium Station"
    "State Station"
    "Government Center Station"
    "Bowdoin Station"
    "Lechmere Station"
    "Science Park Station"
    "North Station"
    "Haymarket Station"
    "Government Center Station"
    "Park Street Station"
    "Boylston Street Station"
    "Arlington Station"
    "Copley Station"
    "Prudential Station"
    "Symphony Station"
    "Northeastern University Station"
    "Museum of Fine Arts Station"
    "Longwood Medical Area Station"
    "Brigham Circle Station"
    "Fenwood Road Station"
    "Mission Park Station"
    "Riverway Station"
    "Back of the Hill Station"
    "Heath Street Station"
    "Hynes Convention Center"
    "Kenmore Station"
    "St. Marys Street Station"
    "Hawes Street Station"
    "Kent Street Station"
    "St. Paul Street"
    "Coolidge Corner Station"
    "Summit Avenue Station"
    "Brandon Hall Station"
    "Fairbanks Station"
    "Washington Square Station"
    "Tappan Street Station"
    "Fenway Station"
    "Dean Road Station"
    "Englewood Avenue Station"
    "Cleveland Circle Station"
    "Longwood Station"
    "Brookline Village Station"
    "Brookline Hills Station"
    "Beaconsfield Station"
    "Reservoir Station"
    "Chestnut Hill Station D Riverside Line"
    "Newton Centre Station"
    "Newton Highlands Station"
    "Eliot Station"
    "Waban Station"
    "Woodland Station"
    "Riverside Station"
    "Blandford Street Station"
    "Boston University East Station"
    "Boston University Central Station"
    "Boston University West Station"
    "St. Paul Street"
    "Pleasant Street Station"
    "Babcock Street Station"
    "Packards Corner Station"
    "Harvard Avenue Station"
    "Griggs Street/Long Avenue Station"
    "Allston Street Station"
    "Warren Street Station"
    "Washington Street Station"
    "Sutherland Road Station"
    "Chiswick Road Station"
    "Chestnut Hill Avenue Station"
    "South Street Station"
    "Boston College Station"
    "Oak Grove Station"
    "Malden Center Station"
    "Wellington Station"
    "Assembly Station"
    "Sullivan Square Station"
    "Community College Station"
    "North Station"
    "Haymarket Station"
    "State Station"
    "Downtown Crossing Station"
    "Chinatown Station"
    "Tufts Medical Center Station"
    "Back Bay Station"
    "Massachusetts Avenue Station"
    "Ruggles Station"
    "Roxbury Crossing Station"
    "Jackson Square Station"
    "Stony Brook Station"
    "Green Street Station"
    "Forest Hills Station"
    "Alewife Station"
    "Davis Station"
    "Porter Square Station"
    "Harvard Square Station"
    "Central Square Station"
    "Kendall Station"
    "Charles/MGH Station"
    "Park Street Station"
    "Downtown Crossing Station"
    "South Station"
    "Broadway Station"
    "Andrew Station"
    "JFK/UMass Station"
    "North Quincy Station"
    "Wollaston Station"
    "Quincy Center Station"
    "Quincy Adams Station"
    "Braintree Station"
    "Savin Hill Station"
    "Fields Corner Station"
    "Shawmut Station"
    "Ashmont Station"
    "Cedar Grove Station"
    "Butler Station"
    "Milton Station"
    "Central Avenue Station"
    "Valley Road Station"
    "Capen Street Station"
    "Mattapan Station"
    "")))

(define/ctc-helper (station? str) (member str expected-stations))

