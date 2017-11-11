;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname MidiBoardMain) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rsound)
(require 2htdp/universe)

;ms is a world state
;ms includes
;volume - decimal 0.0 - 1.0
;pressed? - list of booleans that indicate what keys are being pressed
;octave - number between 0 and 8 that indicate the midi number for each key
;kit - a structure that indicates path and integer value (make-kit "vgame" 60)
;record? - boolean that indicates if the sounds are being recorded
;recordlength - frames long of the recording
;rate - a multiplier of how fast soundws are played
;tempo - how fast the metronome moves in bpm (needs to convert from frames per second)
(define-struct ms (volume pressed? octave kit record? recordlength rate tempo))




;kit is a structure from the worldstate that includes
;kitname - a string that determines the family "vgame"
;kitnumber - a whole number that deterimess the number
(define-struct kit (kitname kitnum))

;test variables defined for you guessed it, testing
(define testkit (make-kit "vgame" 50))
(define test (make-ms .5 #t 0 testkit #f 0 1 0))
(define one 44100)


;octavator is a function that converts a number of an octave into the various midi numbers
;number -> list-of-midi
(define (octavator trace) 
(cons (* trace 12) (cons (+ 1 (* trace 12)) (cons (+ 2 (* trace 12)) (cons (+ 3 (* trace 12)) (cons (+ 4 (* trace 12))
      (cons (+ 5 (* trace 12)) (cons (+ 6 (* trace 12)) (cons (+ 7 (* trace 12)) (cons (+ 8 (* trace 12)) (cons (+ 9 (* trace 12))
      (cons (+ 10 (* trace 12)) (cons (+ 11 (* trace 12)) '()))))))))))))

)
;(check-expect (octavator 3) (list 36 37 38 39 40 41 42 43 44 45 46 47))
;(check-expect (octavator 0) (list 0 1 2 3 4 5 6 7 8 9 10 11))

;selector is a helper function that calls a specific element in an octave from a list of numbers
;worldstate index -> midi
(define (selector world index)
  (list-ref (octavator (ms-octave world)) index)


)

;(check-expect (selector test 0) 0)
;(check-expect (selector test 11) 11)


;keytracker is a function that maps midi notes to keys based on the octave and produces a make-tone
;worldstate key -> rsound
(define (keytracker world key)
  (cond [(key=? "a" key)  (synth-note (kit-kitname (ms-kit world)) (kit-kitnum (ms-kit world)) (selector world 0) one)]
        [(key=? "w" key)  (synth-note (kit-kitname (ms-kit world)) (kit-kitnum (ms-kit world)) (selector world 1) one)]
        [(key=? "s" key)  (synth-note (kit-kitname (ms-kit world)) (kit-kitnum (ms-kit world)) (selector world 2) one)]
        [(key=? "e" key)  (synth-note (kit-kitname (ms-kit world)) (kit-kitnum (ms-kit world)) (selector world 3) one)]
        [(key=? "d" key)  (synth-note (kit-kitname (ms-kit world)) (kit-kitnum (ms-kit world)) (selector world 4) one)]
        [(key=? "f" key)  (synth-note (kit-kitname (ms-kit world)) (kit-kitnum (ms-kit world)) (selector world 5) one)]
        [(key=? "t" key)  (synth-note (kit-kitname (ms-kit world)) (kit-kitnum (ms-kit world)) (selector world 6) one)]
        [(key=? "g" key)  (synth-note (kit-kitname (ms-kit world)) (kit-kitnum (ms-kit world)) (selector world 7) one)]
        [(key=? "y" key)  (synth-note (kit-kitname (ms-kit world)) (kit-kitnum (ms-kit world)) (selector world 8) one)]
        [(key=? "h" key)  (synth-note (kit-kitname (ms-kit world)) (kit-kitnum (ms-kit world)) (selector world 9) one)]
        [(key=? "u" key)  (synth-note (kit-kitname (ms-kit world)) (kit-kitnum (ms-kit world)) (selector world 10) one)]
        [(key=? "j" key)  (synth-note (kit-kitname (ms-kit world)) (kit-kitnum (ms-kit world)) (selector world 11) one)]
)
  )

;(kit-kitname (ms-kit test))
;(check-expect (keytracker test "f") (synth-note "vgame" 50 5 44100))





























