;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname IDKWTF2D) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;a list of booleans is ms-pressed?
;the rsounds will be synth-notes
;b2s stands for "Boolean to Sound" and it takes a list of booleans
;and converts them into an rsound
;list of booleans -> rsound
(define (b2s ws)
  (cond[(empty? (ms-pressed? ws)) (silence 1)]
       [else
              (first (ms-pressed? ws))
              (b2s(rest (ms-pressed? ws)))                
        ]
  )
  )

(check-expect (b2s '()) (silence 1))
(check-expect (b2s (list #f #f #f #f #f #f #f #f #f #f #f #f #f)) (silence 1))
(check-expect (b2s (list #f #f #f #f #t #f #f #f #f #f #f #f #f)) (synth-note "vgame" (kit-kitname? ws) (kit-kitnum? ws) 64 ))