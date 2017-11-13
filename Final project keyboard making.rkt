;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Final project keyboard making|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rsound)
(require 2htdp/universe)
(require 2htdp/image)
;---------------------------------------------------------------------------------------------------------------------
;defining a function that draws the base world state for ms
;ms is a boolean
;boolean -> image
;draws the GUI for the midi keyboard

(define MIDI (bitmap/file "C:/Users/Brad/Desktop/CPE123/Midiboard.png"))

(define (RENDER ms)
  (place-image 
   (overlay/offset (rectangle 2000 300 "solid" "black")
                   200
                   300
                   

   (overlay/xy
            (beside
                   (rectangle 67.5 210.825 "outline" "black")
                   (rectangle 67.5 210.825 "outline" "black")
                   (rectangle 67.5 210.825 "outline" "black")
                   (rectangle 67.5 210.825 "outline" "black")
                   (rectangle 67.5 210.825 "outline" "black")
                   (rectangle 67.5 210.825 "outline" "black")
                   (rectangle 67.5 210.825 "outline" "black")
                   )     
            35
            0           
             (beside
                     (rectangle 43.5 140.5485 "solid" "black")
                     (rectangle 34.5 140.5485  "solid" "white")
                     (rectangle 43.5 140.5485 "solid" "black")
                     (rectangle 37.5 140.5485  "solid" "white")
                     (rectangle 37.5 140.5485  "solid" "white")
                     (rectangle 43.5 140.5485 "solid" "black")
                     (rectangle 34.5 140.5485  "solid" "white")
                     (rectangle 43.5 140.5485  "solid" "black")
                     (rectangle 34.5 140.5485  "solid" "white")
                     (rectangle 43.5 140.5485  "solid" "black") 
                      )
              )
        )
            200
            200
            (empty-scene 750 500)
      )
 )
  
;-------------------------------------------------------------------------------------------------------------------------------

#|(define (key-hit key )
  (cond [(key=? "a" key)  ]
        [(key=? "w" key)  ]
        [(key=? "s" key)  ]
        [(key=? "e" key)  ]
        [(key=? "d" key)  ]
        [(key=? "f" key)  ]
        [(key=? "t" key) ]
        [(key=? "g" key) ]
        [(key=? "y" key)  ]
        [(key=? "h" key)  ]
        [(key=? "u" key)  ]
        [(key=? "j" key)  ]
))
|#

(define (main y)
  (big-bang y
  [to-draw RENDER]
  ))
(main #t)
