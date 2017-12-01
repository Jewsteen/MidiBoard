;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname BetterMidi) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;kit is a structure from the worldstate that includes
;kitname - a string that determines the family "vgame"
;kitnumber - a whole number that deterimess the number
(define-struct kit (kitname kitnum))

(define-struct pstate (p? frames))

;test variables defined for you guessed it, testing
(define testkit (make-kit "vgame" 50))
(define test (make-ms .8 (make-list 52 #f) (cons (make-list 4 (silence 1) ) '()) 6 testkit #f (silence 1) 0 133))

;========================================================================
;IndR outputs a boolean inthe current note ased on the index given.
; WorldState Number - > boolean
(define (IndR ws index)
  (list-ref (ms-pressed? ws)  (+ index (* 13 (ms-rate ws)))))


;One-True takes in a list of booleans and returns true
; if there's atleast one true value in the list
; List_of_booleans -> boolean
(define (one-true booList)
  (cond
    [(empty? booList) #f]
    [(cons? booList)
      (or (boolean=? (first booList) #t)
       (one-true (rest booList)))]
     ))
(check-expect (one-true (list #f #t #f #f)) #t)

;N-List takes an index and converts a 13 boolean section of a list into it's own list.
;List_of_booleans Number -> List_of_booleans
(define (N-List lob index)
 (take (drop lob (* index 13)) 13))

(check-expect (N-List (make-list 52 #f) 1) (make-list 13 #f))

;colorKey outputs a rectangle color based on what it's original color was
;and whether it's currently pressed or not
; WorldState(ms) number number -> string
(define (colorKey ws index col)
  (cond
    [(equal? 1 col ) 
    (cond
      [(equal?  (IndR ws index) #t) "red"]
      [else "black"])
    ]
    [(equal? 0 col)
     (cond
      [(equal? (IndR ws index) #t) "red"]
      [else "white"])
    ]
    [else "green"] 
    ))

;NoteIndex outputs the index of the onscreen note based on
;what current note is selected.
;Number Number -> Number
(define (NoteIndex cNote pos )
  (+ (* 4 (floor (/ cNote 4))) pos))

;colorsect outputs an image to show differing UI elements as colorful rectangle(s)
;depending on what rectangle is 
;WorldState Number Number -> Image
(define (colorSect ws type rect)
(cond
  [(equal? type 0)
   (beside
    (rectangle (* 100 (ms-volume ws)) 50 "solid" "green")
    (rectangle (- 100 (* 100 (ms-volume ws))) 50 "solid" "red"))
  ]
  [(equal? type 1)
     (beside
    (rectangle (* 12.5 (ms-octave ws)) 50 "solid" "green")
    (rectangle (- 100 (* 12.5 (ms-octave ws))) 50 "solid" "red"))
  ]
  [(equal? type 2)
       (cond
         [(and (one-true (N-List (ms-pressed? ws) (NoteIndex (ms-rate ws) rect))) (equal? (ms-rate ws) (NoteIndex (ms-rate ws) rect)))
              (rectangle 100 50 "solid" "blue")
          ]
         [(and (one-true (N-List (ms-pressed? ws) (NoteIndex (ms-rate ws) rect))) (not(equal? (ms-rate ws) (NoteIndex (ms-rate ws) rect))))
              (rectangle 100 50 "solid" "purple")
          ] 
         [(and (equal? false (one-true (N-List (ms-pressed? ws) (NoteIndex (ms-rate ws) rect)))) (equal? (ms-rate ws) (NoteIndex (ms-rate ws) rect)))
              (rectangle 100 50 "solid" "orange")]
         
         [else (rectangle 100 50 "solid" "yellow")]
         )
     
     ]
  [(equal? type 3)
   (beside
    (rectangle (* 100 (ms-volume ws)) 50 "solid" "green")
    (rectangle (- 100 (* 100 (ms-volume ws))) 50 "solid" "red"))
  ]
  ))


;oc outputs an key(image) based on a name and position
;String Number WorldState -> Image
(define (oc str num ms)
   (overlay/xy (rectangle 67.5 210.825 "outline" (colorKey ms num 1)) 20 140 (text str 40 "black")))


;defining function RENDER which takes a ms
;ms -> ms
;function toggles keys based on the keytracker function to appear red when a button is pressed
(define (RENDER ms)
  (place-image 
   (underlay/offset
    (overlay/offset (rectangle 2000 300 "solid" "black")
                   200
                   300               
   (overlay/xy
            (beside
              (oc "C" 0 ms)
              (oc "D" 2 ms)
              (oc "E" 4 ms)
              (oc "F" 5 ms)
              (oc "G" 7 ms)
              (oc "A" 9 ms)
              (oc "B" 11 ms)
              (oc "C" 12 ms))
            
     
            0
            0
     ;For this set of rectangles, all rectangles with col == 0 will turn red for the corresponding white key
             (beside
              (rectangle 35 140.5485 "solid" (colorKey ms 0 0))
                     (underlay/xy (rectangle 43.5 140.5485 "solid" (colorKey ms 1 1)) 0 60 (text "C#" 20 "white"))
                     (rectangle 34.5 140.5485  "solid" (colorKey ms 2 0))
                     (underlay/xy(rectangle 43.5 140.5485 "solid" (colorKey ms 3 1))0 60 (text "D#" 20 "white"))
                     (rectangle 37.5 140.5485  "solid" (colorKey ms 4 0))
                     (rectangle 37.5 140.5485  "solid" (colorKey ms 5 0))
                     (underlay/xy(rectangle 43.5 140.5485 "solid" (colorKey ms 6 1))0 60 (text "F#" 20 "white"))
                     (rectangle 34.5 140.5485  "solid" (colorKey ms 7 0))
                     (underlay/xy(rectangle 43.5 140.5485  "solid" (colorKey ms 8 1))0 60 (text "G#" 20 "white"))
                     (rectangle 34.5 140.5485  "solid" (colorKey ms 9 0))
                     (underlay/xy(rectangle 43.5 140.5485  "solid" (colorKey ms 10 1))0 60 (text "A#" 20 "white"))
                     (rectangle 35 140.5485 "solid" (colorKey ms 11 0))


                      )
              ))
    150
    -100
    
    (overlay/xy
     (beside
                (text  "Volume: "  24 "White" )
                (colorSect ms 0 0)
                (text (string-append " | " (number->string (* 100 (ms-volume ms))) "% ") 24 "White" )
               
          )
     0
     0
   (overlay/xy
     (beside
                 (text  "Octave: "  24 "White" )
                 (colorSect ms 1 1)
                 (text (string-append " | " (number->string  (ms-octave ms)) "    Tempo: " (number->string (ms-tempo ms))) 24 "White" )


             
          )
     -300
     100
     (beside

                   (text  (string-append "M: " (number->string (+(floor(/(ms-rate ms)4))1) )) 24 "White" )

      (rectangle 50 50 "solid" "black")
                (colorSect ms 2 0)
                 (rectangle 50 50 "solid" "black")
                  (colorSect ms 2 1)
                 (rectangle 50 50 "solid" "black")
                  (colorSect ms 2 2)
                 (rectangle 50 50 "solid" "black")
                  (colorSect ms 2 3)

      )
     )
     )
    )
            200
            200
            (empty-scene 750 500)
      )
  )
  

;=======================================================

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
  (list-ref (octavator (ms-octave world)) index))
;(check-expect (selector test 0) 0)

; Creates a new world state with the opposite boolean value at a given index in 
; WorldState number -> Worldstate

;oppBool creates a new pressed? list and outputs a new world state
;with an opposite boolean value at the given index
; WorldState(ms) number -> WorldState(ms)
(define (oppBool ws index)
  (make-ms (ms-volume ws)
            (list-set (ms-pressed? ws) (+ index (* 13 (ms-rate ws))) (not (IndR ws index)) )
  (ms-Plength ws) (ms-octave ws) (ms-kit ws) (ms-record? ws) (ms-recordlength ws) (ms-rate ws) (ms-tempo ws))
  )

;Octav takes in a worldstate and a number
;then changes or doesn't change the octave based on the current world
;eithering increasing or decreasing the octave
;WorldState(ms) Number -> WorldState(ms)
(define (octav world funct)
  (cond
    [(and (equal? 0 (ms-octave world))(equal? funct 0)) world]
    [(and (equal? 8 (ms-octave world))(equal? funct 1)) world]
    [ else  (make-ms (ms-volume world)(ms-pressed? world) (ms-Plength world)
                     (cond
                    [(equal? funct 0) (- (ms-octave world) 1)]
                    [else (+ (ms-octave world) 1)]
                     )
                     (ms-kit world) (ms-record? world) (ms-recordlength world) (ms-rate world) (ms-tempo world))]
))

;Vol takes in a worldstate and a number
;then changes or doesn't change the volume based on the current world
;eithering increasing or decreasing the volume
;WorldState(ms) Number -> WorldState(ms)
(define (vol world funct)
  (cond
    [(and (equal? 1.0 (ms-volume world))(equal? funct 1)) world]
    [(and (equal? 0.0 (ms-volume world))(equal? funct 0)) world]
    [ else  (make-ms
             (cond
                    [(equal? funct 0) (- (ms-volume world) 0.1)]
                    [else (+ (ms-volume world) 0.1)]
                     )
             (ms-pressed? world) (ms-Plength world) (ms-octave world) (ms-kit world) (ms-record? world) (ms-recordlength world) (ms-rate world) (ms-tempo world))]
))

;Vol takes in a worldstate and a number
;then changes or doesn't change the volume based on the current world
;eithering increasing or decreasing the volume
;WorldState(ms) Number -> WorldState(ms)
(define (temp world funct)
  (cond
    [(and (equal? 200 (ms-tempo world))(equal? funct 1)) world]
    [(and (equal? 0 (ms-tempo world))(equal? funct 0)) world]
    [ else  (make-ms (ms-volume world) (ms-pressed? world) (ms-Plength world) (ms-octave world) (ms-kit world) (ms-record? world) (ms-recordlength world) (ms-rate world)
             (cond
                    [(equal? funct 0) (- (ms-tempo world )1)]
                    [else (+ (ms-tempo world) 1)]
                     )
             )]
))

;Mchg outputs a worldstate with the correct current note and a new note to the song if necessary
;WorldState(ms) boolean -> WorldState(ms)
(define (Mchg ws next?)
  (cond
    [next?
     (cond
       [(equal? (/ (length (ms-pressed? ws)) 13) (+ 1 (ms-rate ws)))
        (make-ms (ms-volume ws) (append (ms-pressed? ws) (make-list 52 #f)) (ms-Plength ws) (ms-octave ws) (ms-kit ws) (ms-record? ws) (ms-recordlength ws) (+ (ms-rate ws) 1) (ms-tempo ws))]
       [else  (make-ms (ms-volume ws) (ms-pressed? ws) (ms-Plength ws) (ms-octave ws) (ms-kit ws) (ms-record? ws) (ms-recordlength ws) (+ (ms-rate ws) 1) (ms-tempo ws))]
       )
    ]
    [else
     (cond
       [(equal? (ms-rate ws) 0) ws]
       [else (make-ms (ms-volume ws) (ms-pressed? ws) (ms-Plength ws) (ms-octave ws) (ms-kit ws) (ms-record? ws) (ms-recordlength ws) (- (ms-rate ws) 1) (ms-tempo ws))]
      )]))

;world->list is a function that extracrts the pressed? list from a world
;world -> list-of-sounds
(define (world->list world)
    (ms-pressed? world)
)
;(check-expect (world->list test) (make-list 13 #f))


;synthcreation is a function that takes in a world state and a list and makes a synthnote
;worldstate boolean number -> synthnote
(define (synthcreation world i)
  (rs-scale (ms-volume world) (synth-note (kit-kitname (ms-kit world)) (kit-kitnum (ms-kit world)) (+ (* 12 (+ 1 (ms-octave world))) i) (round (* 48000 (/ 60 (ms-tempo world))))))
  
  )
;(check-expect (synthcreation test 2) (synth-note "vgame" 50 86 44100))


;soundconverter is a function that converts the pressed booleans into a list-of-rsounds based on the worldstate
;worldstate list-of-soounds number-> list-of-rsounds
(define (soundconverter world  listy index)
 (cond [(empty? listy) '()]
       [else (cond
               [(first listy) (cons (synthcreation world index) (soundconverter world (rest listy) (+ 1 index)))]
               [else (soundconverter world (rest listy) (+ 1 index))])])
 )
;(check-expect (soundconverter test (make-list 13 #f) 0) '())
;(check-expect (soundconverter test (list #f #f #t #t #t #f #f #f #f #f #f #f #f) 0) (list (synth-note "vgame" 50 86 44100) (synth-note "vgame" 50 87 44100) (synth-note "vgame" 50 88 44100)))

(define (LtRS ws lob index)
  (cond
    [(empty? lob) '()]
    [(> 13 (length lob)) '()]
    [else
     (cond
       [(empty? (soundconverter ws (take lob 13) 0)) (LtRS ws lob (+ index 1))]
       [else (cons (rs-overlay* (soundconverter ws (take lob 13) 0))
                   (LtRS ws (drop lob 13) (+ index 1))) ])
     ]
    ))


(define (SaveList ws)
 (make-ms (ms-volume ws) (ms-pressed? ws) (ms-Plength ws) (ms-octave ws) (ms-kit ws) (rs-append* (LtRS ws (ms-pressed? ws) 0) )  (ms-recordlength ws) (ms-rate ws) (ms-tempo ws)) )
;(check-expect (soundconverter test (list #f #f #t #t #t #f #f #f #f #f #f #f #f) 0) (list (synth-note "vgame" 50 86 (round (* 48000(/ 60 (ms-tempo test))))) (synth-note "vgame" 50 87 (round (* 48000(/ 60 (ms-tempo test))))) (synth-note "vgame" 50 88 (round (* 48000(/ 60 (ms-tempo test)))))))

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
        [(key=? "[" key) (octav world 0) ]
        [(key=? "]" key) (octav world 1) ]
         [(key=? "[" key) (octav world 0) ]
        [(key=? "]" key) (octav world 1) ]
        [(key=? "-" key) (vol world 0) ]
        [(key=? "=" key) (vol world 1) ]
        [(key=? "," key) (temp world 0) ]
        [(key=? "." key) (temp world 1) ]        
        [(key=? "escape" key) (exit) ]
        [(key=? "left" key) (Mchg world #f) ]
        [(key=? "right" key) (Mchg world #t) ]
        [(key=? "1" key) (SaveList world)]
        [(key=? "2" key) (andplay (ms-record? world) world)]
         
        [else world]
        )
  )


;Tock takes in a WorldState and plays a sound and or silence
;Based on the WorldState
;WorldState -> WorldState
(define (tock y)
y
)

;Midi is the big bang function that this program runs on.
;WorldState -> Worldstate
(define (Midi y)
    (big-bang y
              [on-tick tock 1]     
              [to-draw RENDER]
              [on-key keytracker]
              )
    )
(Midi test)