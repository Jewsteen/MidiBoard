;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname MidiBoardMain) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rsound)
(require 2htdp/universe)
(require 2htdp/image)
(require racket/list)

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
(define-struct ms (volume pressed? Plength octave kit record? recordlength rate tempo))

;========================================================================
;defining function RENDER which takes a ms
;ms -> ms
;function toggles keys based on the keytracker function to appear red when a button is pressed


(define (colorKey ws index col)
  (cond
    [(equal? 1 col ) 
    (cond
      [(equal? (list-ref (ms-pressed? ws) index) #t) "red"]
      [else "black"])
    ]
    [(equal? 0 col)
     (cond
      [(equal? (list-ref (ms-pressed? ws) index) #t) "red"]
      [else "white"])
    ]
    [else "green"] 
    ))

(define (RENDER ms)
  (place-image 
   (overlay/offset (rectangle 2000 300 "solid" "black")
                   200
                   300               
   (overlay/xy
            (beside
              (rectangle 67.5 210.825 "outline" (colorKey ms 0 1))
              (rectangle 67.5 210.825 "outline" (colorKey ms 2 1))
              (rectangle 67.5 210.825 "outline" (colorKey ms 4 1))
              (rectangle 67.5 210.825 "outline" (colorKey ms 5 1))
              (rectangle 67.5 210.825 "outline" (colorKey ms 7 1))
              (rectangle 67.5 210.825 "outline" (colorKey ms 9 1))
              (rectangle 67.5 210.825 "outline" (colorKey ms 11 1)))     
            35
            0           
             (beside
                     (rectangle 43.5 140.5485 "solid" (colorKey ms 1 1))
                     (rectangle 34.5 140.5485  "solid" (colorKey ms 2 0))
                     (rectangle 43.5 140.5485 "solid" (colorKey ms 3 1))
                     (rectangle 37.5 140.5485  "solid" (colorKey ms 4 0))
                     (rectangle 37.5 140.5485  "solid" (colorKey ms 5 0))
                     (rectangle 43.5 140.5485 "solid" (colorKey ms 6 1))
                     (rectangle 34.5 140.5485  "solid" (colorKey ms 7 0))
                     (rectangle 43.5 140.5485  "solid" (colorKey ms 8 1))
                     (rectangle 34.5 140.5485  "solid" (colorKey ms 9 0))
                     (rectangle 43.5 140.5485  "solid" (colorKey ms 10 1)) 
                      )
              ))
            200
            200
            (empty-scene 750 500)
      )
  )
  

;=======================================================


;kit is a structure from the worldstate that includes
;kitname - a string that determines the family "vgame"
;kitnumber - a whole number that deterimess the number
(define-struct kit (kitname kitnum))

;test variables defined for you guessed it, testing
(define testkit (make-kit "vgame" 50))
(define test (make-ms .5 '(#f #f #f #t #f #f #f #f #f #f #f #f #f) '(-1 -1 -1 48000 -1 -1 -1 -1 -1 -1 -1 -1 -1) 0 testkit #f 0 1 0))
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


; Creates a new world state with the opposite boolean value at a given index in 
; WorldState number -> Worldstate

;(define-struct ms (volume pressed? Plength octave kit record? recordlength rate tempo))

(define (oppBool ws index)
  (make-ms (ms-volume ws) (list-set (ms-pressed? ws) index (not(list-ref (ms-pressed? ws) index )) ) (ms-Plength ws) (ms-octave ws) (ms-kit ws) (ms-record? ws) (ms-recordlength ws) (ms-rate ws) (ms-tempo ws))
  )
;(synth-note (kit-kitname (ms-kit world)) (kit-kitnum (ms-kit world)) (selector world 0) one)

;keytracker is a function that maps midi notes to keys based on the octave and produces a make-tone
;worldstate key -> rsound
(define (keytracker world key)
  (cond [(key=? "a" key)  (oppBool world 0)]
        [(key=? "w" key)  (oppBool world 1)]
        [(key=? "s" key)  (oppBool world 2)]
        [(key=? "e" key)  (oppBool world 3)]
        [(key=? "d" key)  (oppBool world 4)]
        [(key=? "f" key)  (oppBool world 5)]
        [(key=? "t" key)  (oppBool world 6)]
        [(key=? "g" key)  (oppBool world 7)]
        [(key=? "y" key)  (oppBool world 8)]
        [(key=? "h" key)  (oppBool world 9)]
        [(key=? "u" key)  (oppBool world 10)]
        [(key=? "j" key)  (oppBool world 11)]
        [else (make-ms (ms-volume world) (make-list 12 #f) (make-list 12 -1) (ms-octave world) (ms-kit world) (ms-record? world) (ms-recordlength world) (ms-rate world) (ms-tempo world))]
)
  )

;(kit-kitname (ms-kit test))
;(check-expect (keytracker test "f") (synth-note "vgame" 50 5 44100))


(define (tock y)y )


(define (Midi y)
    (big-bang y
              [on-tick tock 0.5]     
              [to-draw RENDER]
              [on-key keytracker]
              )
    )

