;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw06) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; hw06 (beginning-student language -- "simplistic" racket)
; Daniel Jordan
; see http://www.radford.edu/~itec380/2017fall/Homeworks/lists/lists.html


 (require 2htdp/image)
 (require 2htdp/universe)
#| Follow the design recipe (the final version!):
  ---- Per data type:
     1. Choose data definition
     2. Give some examples of the data
     3. Template:
        a. If handling a union type, include a cond w/ one branch per option.
        b. If handling a product-type, pull out each piece (field).
        c. Add the natural recursive call (as appropriate)
        d. Inventory: In all cases, think about what *type* (each piece) is,
           and think of some functions that process that type.

  ---- Per function:
     4. write test cases
     5. (a-d): header, purpose, contract/type, copy the template, and stub it.
     6. Inventory with values: Consider a particular test case,
        and think about what *specific* *value* of each sub-expression.
     7. complete the body
     8. run tests
|#

; Below is the provided code for collision detection
;---------------------------------------------------------------------------
; <=< : real?, real?, real? -> boolean?
; Is x in the half-open interval [a,b)?
; pre-conditon: (<= a b)
;
(define (<=< a x b) (and (<= a x) (< x b)))
(check-expect (<=< 4  7   10) #true)
(check-expect (<=< 7  7   10) #true)
(check-expect (<=< 4  7    7) #false)
(check-expect (<=< 4 -7   10) #false)
(check-expect (<=< 4  4    4) #false)
(check-expect (<=< 4  4.1  4.5) #true)
(check-expect (<=< 100000 #x314d2ef361bcd159 +inf.0) #true)
(check-expect (<=< -4.5  -4.1  -4) #true)
;(check-expect (<=< -4  -4.1  -4.5) #false) ; N.B. violates pre-condition: a > b.
; Therefore we comment out this test not because it the current-implementation fails it
; (it doesn't), but rather because including it is actually saying "any implementation
; that doesn't handle this test is an incorrect-implementation" -- which is not actually
; the case.

; overlap? : real,real,real,real, real,real,real,real -> boolean
; Does one rectangle overlap another?
; We represent a rectangle as four reals: the center x,y,  width, height.
;
; For barely-touching rectangles, we use the rather odd convention that
; a rectangle is closed along its top and left sides, but open on its
; bottom and right.
; (This way, the squares (i,j,1,1) perfectly tile the plane, for i,j in N.)
;
(define (overlap? x1 y1 w1 h1  x2 y2 w2 h2)
  ; Idea: is rect2's center inside rect1-enlarged-by-w2xh2 ?
  (and (<=< (- x1 (/ w1 2) (/ w2 2))  x2  (+ x1 (/ w1 2) (/ w2 2)))
       (<=< (- y1 (/ h1 2) (/ h2 2))  y2  (+ y1 (/ h1 2) (/ h2 2)))
       ; If I had `let`, I might write:
       #;(let* {[widths/2  (/ (+ w1 w2) 2)] ...}
            … (<=< (- x1 widths/2)  x2  (+ x1 widths/2)) …)
       ;
       ; Alternately: think of checking dist-btwn-centers, in each dimension:
       ; (< (abs (- x1 x2)) (+ (/ w1 2) (/ w2 2)))
       ; (< (abs (- y1 y2)) (+ (/ y1 2) (/ y2 2))))
       ; (This doesn't quite get the half-open intervals though.)
       ))

(check-expect (overlap?  5  5 10 10   6  6  2  2) #true)
(check-expect (overlap?  5  5 10 10  15  6 20  2) #true)
(check-expect (overlap?  5  5 10 10   6 15  2 20) #true)
(check-expect (overlap?  5  5 10 10  15 15 20 20) #true)
(check-expect (overlap?  5  5 10 10  16 16  2  2) #false)
(check-expect (overlap?  5  5 10 10  25 25 20 20) #false)
(check-expect (overlap?  5  5 10 10  -4 -4  2  2) #false)
(check-expect (overlap?  5  5 10 10 -10 -10 20 20) #true)


License: CC-BY 4.0 -- you are free to share and adapt this file
for any purpose, provided you include appropriate attribution.
    https://creativecommons.org/licenses/by/4.0/ 
    https://creativecommons.org/licenses/by/4.0/legalcode 
Including a link to the *original* file satisifies "appropriate attribution".
|#

; Below is work from hw04
;---------------------------------------------------------------------------

; Data Definition:
; A truck object contains 4 values, x position, y position, velocity
; and a facing position.
(define-struct truck (pos-x pos-y velocity pos-face))
;   make-truck : natural, natural, natural, int -> truck

; Examples of the data
(define t0 (make-truck 0 0.0 1.0 -1))
(define t1 (make-truck 1.0 0.0 1.0 -1))
(define t2 (make-truck 1000 999 0.0 1))
(define t3 (make-truck 500 300 1 1))

; constructor:
; make-truck : natural, natural, natural, int -> truck
; getters:
; truck-pos-x : truck -> natural
; truck-pos-y : truck -> natural
; truck-velocity : truck -> natural
; truck-pos-face : truck -> int

; Template:
; The template for a compound-type/struct with k fields is simply calling the k getters
; func-for-truck : truck -> ????
; Return ???
(define (func-for-truck a-truck)
  (... (truck-pos-x a-truck)
   ... (truck-pos-y a-truck)
   ... (truck-velocity a-truck)
   ... (truck-pos-face a-truck)))

; move : truck -> truck
; Returns the truck object with the pos-x increased
; BUG : function will take negative velocity, futher code will update this
;       because "negative" velocity is determined by pos-face = -1
;
(define (move a-truck)
  (make-truck (+ (truck-velocity a-truck) (truck-pos-x a-truck))
              (truck-pos-y a-truck)
              (truck-velocity a-truck)
              (truck-pos-face a-truck)))

(check-expect (move t0) t1)
(check-expect (move t1) (make-truck 2.0 0.0 1 -1))
(check-expect (move (make-truck 0 0 1 -1)) t1)
(check-expect (move (make-truck 1.5 0 1 1)) (make-truck 2.5 0 1 1))
(check-expect (move t2) t2)

; Check expects for negative velocity, velocity will be manually coded in helper
; function based on truck-pos-face being -1.
(check-expect (move (make-truck 1.5 0 -1.5 1)) (make-truck 0 0 -1.5 1))
(check-expect (move (make-truck -1000 -1000 -50 -1))
                    (make-truck -1050 -1000 -50 -1))

; Data Definition:
; A prince(ss) object contains values, x position, y position,
; facing position and a timer.
(define-struct princess (pos-x pos-y pos-face timer))
;   make-princess : natural, natural, int, natural  -> princess

; Examples of the data
(define p0 (make-princess 0.0 0.0 1 10.0))
(define p1 (make-princess 1.0 0.0 1 500.0))
(define p2 (make-princess -1000.0 -999.0 -1 1.0))
(define p3 (make-princess 0.0 0.0 1 9.0))

; constructor:
; make-princess : natural, natural, int, natural -> princess
; getters:
; princess-pos-x : princess -> natural
; princess-pos-y : princess -> natural
; princess-pos-face : princess -> int
; princess-timer : princess -> natural

; Template:
; The template for a compound-type/struct with k fields is simply calling the k getters
; func-for-princess : princess -> ????
; Return ???
(define (func-for-princess a-princess)
  (... (princess-pos-x a-princess)
   ... (princess-pos-y a-princess)
   ... (princess-pos-face a-princess)
   ... (princess-timer a-princess)))

; update-princess : princess -> princess
; Return the princess object with the time decreased
;
(define (update-princess princess)
  (if (<= (princess-timer princess) 0)
      (make-princess (random 601) (random 101) 1 10000)
      (make-princess (princess-pos-x princess) (princess-pos-y princess) 1
                     (- (princess-timer princess) 10))))

(check-expect (update-princess p0) p3)
(check-expect (update-princess p1) (make-princess 1.0 0.0 1 499.0))
(check-expect (update-princess (make-princess -1000.0 -999.0 -1 2.0)) p2)
(check-expect (update-princess (make-princess 1.0 1.0 1 998.0))
              (make-princess 1.0 1.0 1 997.0))
(check-expect (update-princess (make-princess 0.0 0.0 1 0.0))
              (make-princess 0.0 0.0 1 -1.0))

; Below is the work for hw06
;---------------------------------------------------------------------------

; Data Definition
; list-of-trucks is:
;      '(), OR
;      (make-cons [truck] [list-of-trucks]) ; "constructed list"

; Examples of data:
(define lot0 '())
(define lot1 (cons t0 (cons t1 (cons t2 '()))))
(define lot2 (cons (make-truck 0 0 0 1) (cons (make-truck 100 100 1 -1) (cons (make-truck 1 1 1 -1) '()))))
(define lot3 (cons t0 (cons t1 (cons (make-truck 363 636 1 -1) '()))))
(define lot4 (cons (first lot1) (rest lot1)))
(define lot5 (cons (make-truck 666 666 1 1) lot4 ))

; Template:
; func-for-list-of-trucks : list-of-trucks -> ???
;
(define (func-for-list-of-trucks a-lot)
  (cond [(empty? a-lot) ...]
        [(cons? a-lot) (... (first a-lot) (func-for-list-of-trucks (rest a-lot)))]))

;------------------------Test cases for move-trucks------------------------
(check-expect (move-trucks lot0) '())
(check-expect (move-trucks lot1) (cons (make-truck 1 0 1 -1)
                                       (cons (make-truck 2 0 1 -1)
                                             (cons (make-truck 1000 999 0 1) '()))))
(check-expect (move-trucks lot2) (cons (make-truck 0 0 0 1)
                                       (cons (make-truck 101 100 1 -1)
                                             (cons (make-truck 2 1 1 -1) '()))))
(check-expect (move-trucks lot3) (cons (make-truck 1 0 1 -1)
                                       (cons (make-truck 2 0 1 -1)
                                             (cons (make-truck 364 636 1 -1) '()))))
(check-expect (move-trucks lot4) (cons (make-truck 1 0 1 -1)
                                       (cons (make-truck 2 0 1 -1)
                                             (cons (make-truck 1000 999 0 1) '()))))
(check-expect (move-trucks lot5) (cons (make-truck 667 666 1 1)
                                       (cons (make-truck 1 0 1 -1)
                                             (cons (make-truck 2 0 1 -1)
                                                   (cons (make-truck 1000 999 0 1) '())))))

; move-trucks : list-of-trucks -> list-of-trucks
; Return a list of all the trucks, where each has been moved. (x-pos + velocity)
;
(define (move-trucks a-lot)
  (cond [(empty? a-lot) '()]
        [(cons? a-lot) (cons (move (first a-lot)) (move-trucks (rest a-lot)))]))

;------------------------Test cases for draw-truck------------------------
; Constant images for testing
(define bg-image (empty-scene 600 400))
(define truck-image (rectangle 32 24 "solid" "blue"))
  

(check-expect (draw-truck t0 bg-image) (place-image
                                        (rectangle 32 24 "solid" "blue")
                                        0
                                        0
                                        (empty-scene 600 400)))
(check-expect (draw-truck t2 bg-image) (place-image
                                        truck-image
                                        1000
                                        999
                                        bg-image))
(check-expect (draw-truck t3 bg-image) (place-image
                                        (rectangle 32 24 "solid" "blue")
                                        (truck-pos-x t3)
                                        (truck-pos-y t3)
                                        bg-image))

; draw-truck : truck, image -> image
; Return the same image with the truck placed on top of it. 
; BUG : will accept null values for truck and image
;
(define (draw-truck truck image)
  (place-image
  (rectangle 32 24 "solid" "blue")
  (truck-pos-x truck)
  (truck-pos-y truck)
   image))

;------------------------Test cases for draw-trucks------------------------
(check-expect (draw-trucks lot0 bg-image) bg-image)
(check-expect (draw-trucks lot1 bg-image) (place-image truck-image
                                                       (truck-pos-x t0)
                                                       (truck-pos-y t0)
                                                       (place-image truck-image
                                                                    (truck-pos-x t1)
                                                                    (truck-pos-y t1)
                                                                    (place-image truck-image
                                                                                 (truck-pos-x t2)
                                                                                 (truck-pos-y t2)
                                                                                 bg-image))))
(check-expect (draw-trucks lot2 bg-image) (place-image truck-image
                                                       0
                                                       0
                                                       (place-image truck-image
                                                                    100
                                                                    100
                                                                    (place-image truck-image
                                                                                 1
                                                                                 1
                                                                                 bg-image))))
(check-expect (draw-trucks lot3 (rectangle 100 100 "solid" "green")) (place-image truck-image
                                                                                  (truck-pos-x t0)
                                                                                  (truck-pos-y t0)
                                                                                  (place-image truck-image
                                                                                               (truck-pos-x t1)
                                                                                               (truck-pos-y t1)
                                                                                               (place-image truck-image
                                                                                                            363
                                                                                                            636
                                                                                                            (rectangle 100 100 "solid" "green")))))

; draw-trucks : list-of-trucks, image -> image
; Return a new image with every truck in the list overlayed onto it.
;
(define (draw-trucks a-lot image)
  (cond [(empty? a-lot) image]
        [(cons? a-lot) (draw-trucks (rest a-lot)
                                    (draw-truck (first a-lot) image))]))
#|
(define (draw-trucks a-lot image)
  (cond [(empty? a-lot) image]
        [(cons? a-lot) (place-image (draw-truck (first a-lot) image)
                                    (truck-pos-x (first a-lot))
                                    (truck-pos-y (first a-lot))
                                    (draw-trucks (rest a-lot) image))]))
|#

; Data Definition
(define-struct frog (pos-x pos-y pos-face))
; make-frog : natural, natural, string -> frog

;Examples of the data
(define f0 (make-frog 0 0 "right"))
(define f1 (make-frog 1 1 "left"))
(define f2 (make-frog 100 100 "down"))
(define f3 (make-frog 600 400 "up"))

; Template:
; func-for-frog : frog -> ???
(define (func-for-frog a-frog)
  (... (frog-pos-x a-frog)
   ... (frog-pos-y a-frog) 
   ... (frog-pos-face a-frog)))

;------------------------Test cases for frog-handle-key------------------------
; Constants for test cases
(define k0 "up")
(define k1 "down")
(define k2 "left")
(define k3 "right")
;(define k4 "other")

(check-expect (frog-handle-key f0 k0) (make-frog
                                       (frog-pos-x f0)
                                       (- (frog-pos-y f0) 1)
                                       k0))
(check-expect (frog-handle-key f1 k1) (make-frog
                                      (frog-pos-x f1)
                                      (+ (frog-pos-y f1) 1)
                                      k1))
(check-expect (frog-handle-key f2 k2) (make-frog
                                      99
                                      100
                                      "left"))
(check-expect (frog-handle-key f3 k3) (make-frog
                                      601
                                      400
                                      "right"))
#|(check-expect (frog-handle-key f0 k4) (make-frog
                                      (frog-pos-x f0)
                                      (frog-pos-y f0)
                                      "right"))|#

; frog-handle-key : frog, key-event -> frog
; Returns a frog with a pos-face that is equal to the key-event
;
(define (frog-handle-key frog key-event)
  (cond [(key=? key-event "up") (make-frog
                                  (frog-pos-x frog)
                                  (- (frog-pos-y frog) 1)
                                  key-event)]
         [(key=? key-event "down") (make-frog
                                    (frog-pos-x frog)
                                    (+ (frog-pos-y frog) 1)
                                    key-event)]
         [(key=? key-event "left") (make-frog
                                    (- (frog-pos-x frog) 1)
                                    (frog-pos-y frog)
                                    key-event)]
         [(key=? key-event "right") (make-frog
                                     (+ (frog-pos-x frog) 1)
                                     (frog-pos-y frog)
                                     key-event)]))

;------------------------Test cases for draw-frog------------------------
; Constants for draw-frog
(define frog-image-up (rotate 0 (triangle 32 "solid" "green")))
(define frog-image-down (rotate 180 (triangle 32 "solid" "green")))
(define frog-image-left (rotate 270 (triangle 32 "solid" "green")))
(define frog-image-right (rotate 90 (triangle 32 "solid" "green")))

(check-expect (draw-frog f0 bg-image) (place-image
                                        (get-frog-image (frog-pos-face f0))
                                        (frog-pos-x f0)
                                        (frog-pos-y f0)
                                        bg-image))


; draw-frog : frog, image -> image
; Returns an image with a frog drawn on it
;
(define (draw-frog frog image) (place-image
                                (get-frog-image (frog-pos-face frog))
                                (frog-pos-x frog)
                                (frog-pos-y frog)
                                image))


;------------------------Test cases for get-frog-image------------------------
; Constants for get-frog-image
(define s0 "up")
(define s1 "down")
(define s2 "left")
(define s3 "right")

(check-expect (get-frog-image s0) frog-image-up)
(check-expect (get-frog-image s1) frog-image-down)
(check-expect (get-frog-image s2) frog-image-left)
(check-expect (get-frog-image s3) frog-image-right)

; get-frog-image : string -> image
; Returns the appropriate direction a frog image will be facing
;
(define (get-frog-image string)
  (cond [(string=? "up" string) frog-image-up]
        [(string=? "down" string) frog-image-down]
        [(string=? "left" string) frog-image-left]
        [(string=? "right" string) frog-image-right]))

;------------------------Test cases for draw-princess------------------------
; Constant for draw-princess
(define princess-image (circle 32 "solid" "pink"))

(check-expect (draw-princess p0 bg-image) (place-image
                                           princess-image
                                           (princess-pos-x p0)
                                           (princess-pos-y p0)
                                           (empty-scene 600 400)))
(check-expect (draw-princess p1 bg-image) (place-image
                                           princess-image
                                           1
                                           0
                                           bg-image))
(check-expect (draw-princess p2 bg-image) (place-image
                                           princess-image
                                           -1000
                                           -999
                                           bg-image))
(check-expect (draw-princess p3 bg-image) (place-image
                                           (circle 32 "solid" "pink")
                                           (princess-pos-x p3)
                                           (princess-pos-y p3)
                                           bg-image))


; draw-princess : princess, image -> image
; Returns an image with a princess drawn on it
;
(define (draw-princess princess image) (place-image
                                        princess-image
                                        (princess-pos-x princess)
                                        (princess-pos-y princess)
                                        image))
                              
; Data Definition
(define-struct world (frog princess list-of-trucks))
; make-world : frog, princess, list-of-trucks -> world

;Examples of the data
(define w0 (make-world f0 p0 lot0))
(define w1 (make-world f1 p1 lot1))
(define w2 (make-world f2 p2 lot2))
(define w3 (make-world f3 p3 lot3))
(define w4 (make-world (make-frog 300 300 "up")
                       (make-princess 0 15 1 250)
                       (cons (make-truck 10 25 1 1)
                             (cons (make-truck 20 25 1 1)
                                   (cons (make-truck 30 25 1 -1) '())))))
(define w5 (make-world (make-frog 300 300 "up")
                       (make-princess 300 25 1 250)
                       (cons (make-truck 50 125 1 1)
                             (cons (make-truck 100 125 1 1)
                                   (cons (make-truck 250 220 1 -1) '())))))

; Template:
; func-for-world : world -> ???
(define (func-for-world a-world)
  (... (world-frog a-world)
       (world-princess a-world)
       (world-list-of-trucks a-world))) 

;------------------------Test cases for update-world------------------------
(check-expect (update-world w0) (make-world
                                 (make-frog 0 0 "right")
                                 (update-princess (make-princess 0.0 0.0 1 10.0))
                                 (replenish-truck
                                  (remove-off-screen-trucks
                                   (move-trucks'())))))
(check-expect (update-world w1) (make-world
                                 (make-frog 1 1 "left")
                                 (update-princess (make-princess 1.0 0.0 1 500.0))
                                 (replenish-truck
                                  (remove-off-screen-trucks
                                   (move-trucks
                                    (cons t0
                                          (cons t1
                                                (cons t2 '()))))))))
(check-expect (update-world w2) (make-world
                                 f2
                                 (update-princess p2)
                                 (replenish-truck
                                  (remove-off-screen-trucks
                                   (move-trucks lot2)))))
(check-expect (update-world w3) (make-world
                                 f3
                                 (update-princess (make-princess 0.0 0.0 1 9.0))
                                 (replenish-truck
                                  (remove-off-screen-trucks
                                   (move-trucks lot3)))))
(check-expect (update-world w4) (make-world
                                 (make-frog 300 300 "up")
                                 (update-princess (make-princess 0 15 1 250))
                                 (replenish-truck
                                  (remove-off-screen-trucks
                                   (move-trucks
                                    (cons (make-truck 10 25 1 1 )
                                          (cons (make-truck 20 25 1 1)
                                                (cons (make-truck 30 25 1 -1) '()))))))))
                                                               
; update-world : world -> world
; Returns a new world one "tick" later from the input world
;
(define (update-world a-world) (make-world
                                (world-frog a-world)
                                (update-princess (world-princess a-world))
                                (replenish-truck
                                 (remove-off-screen-trucks
                                  (move-trucks (world-list-of-trucks a-world))))))

;------------------------Test cases for world-handle-key------------------------
(check-expect (world-handle-key w0 s0) (make-world
                                        (frog-handle-key (world-frog w0) s0)
                                        (world-princess w0)
                                        (world-list-of-trucks w0)))
(check-expect (world-handle-key w1 s1) (make-world
                                       (frog-handle-key (world-frog w1) s1)
                                       (world-princess w1)
                                       (world-list-of-trucks w1)))
(check-expect (world-handle-key w2 s2) (make-world
                                        (frog-handle-key (make-frog 100 100 "down") s2)
                                        (world-princess w2)
                                        (world-list-of-trucks w2)))
(check-expect (world-handle-key w3 s3) (make-world
                                        (frog-handle-key f3 "right")
                                        (world-princess w3)
                                        (world-list-of-trucks w3)))
(check-expect (world-handle-key w4 "up") (make-world
                                        (frog-handle-key (make-frog 300 300 "up") "up")
                                        (world-princess w4)
                                        (cons (make-truck 10 25 1 1)
                                              (cons (make-truck 20 25 1 1)
                                                    (cons (make-truck 30 25 1 -1) '())))))

; world-handle-key : world, keypress -> world
; Returns a new world based on handling a single keypress event
;
(define (world-handle-key a-world key-event) (make-world
                                              (frog-handle-key (world-frog a-world) key-event)
                                              (world-princess a-world)
                                              (world-list-of-trucks a-world)))

;------------------------Test cases for draw-world------------------------
(check-expect (draw-world w0) (place-image
                               (draw-trucks (world-list-of-trucks w0)
                               (draw-frog (world-frog w0)
                               (draw-princess (world-princess w0) bg-image)))
                               300
                               200
                               bg-image))
(check-expect (draw-world w1) (place-image
                               (draw-trucks (world-list-of-trucks w1)
                               (draw-frog (world-frog w1)
                               (draw-princess (world-princess w1) bg-image)))
                               300
                               200
                               bg-image))
(check-expect (draw-world w2) (place-image
                               (draw-trucks (world-list-of-trucks w2)
                               (draw-frog (world-frog w2)
                               (draw-princess (world-princess w2) bg-image)))
                               300
                               200
                               bg-image))
(check-expect (draw-world w3) (place-image
                               (draw-trucks (world-list-of-trucks w3)
                               (draw-frog (world-frog w3)
                               (draw-princess (world-princess w3) bg-image)))
                               300
                               200
                               bg-image))
(check-expect (draw-world w4) (place-image
                               (draw-trucks (world-list-of-trucks w4)
                               (draw-frog (world-frog w4)
                               (draw-princess (world-princess w4) bg-image)))
                               300
                               200
                               bg-image))


; draw-world : world -> image
; Returns an image of a world with frog, princess and trucks drawn on it
;
(define (draw-world a-world) (place-image
                               (draw-trucks (world-list-of-trucks a-world)
                               (draw-frog (world-frog a-world)
                               (draw-princess (world-princess a-world) bg-image)))
                               300
                               200
                               bg-image))

;------------------------Test cases for truck-off-screen?------------------------
(check-expect (truck-off-screen? (make-truck 666 666 1 1)) #false)
(check-expect (truck-off-screen? t0) #true)
(check-expect (truck-off-screen? t1) #true)
(check-expect (truck-off-screen? t2) #false)
(check-expect (truck-off-screen? t3) #true)
(check-expect (truck-off-screen? (make-truck 601 401 1 -1)) #true)
(check-expect (truck-off-screen? (make-truck -100 -999 1 -1)) #false)

; truck-off-screen? : truck -> boolean
; Returns whether or not a not a given truck is entirely off
; of draw-world's returned image.
;
(define (truck-off-screen? truck) (overlap? (truck-pos-x truck) (truck-pos-y truck)
                                            32
                                            24
                                            300 200
                                            600
                                            400))

;------------------------Test cases for remove-off-screen-trucks------------------------
; Constants for testing
(define on-screen-lot0 '())
(define on-screen-lot1 (cons t0
                             (cons t1 '())))
(define on-screen-lot2 (cons (make-truck 0 0 0 1)
                             (cons (make-truck 100 100 1 -1)
                                   (cons (make-truck 1 1 1 -1) '()))))
(define on-screen-lot3 (cons t0
                             (cons t1 '())))

(check-expect (remove-off-screen-trucks lot0) on-screen-lot0)
(check-expect (remove-off-screen-trucks lot1) on-screen-lot1)
(check-expect (remove-off-screen-trucks lot2) on-screen-lot2)
(check-expect (remove-off-screen-trucks lot3) on-screen-lot3)
                                                        
; remove-off-screen-trucks : list-of-trucks -> list-of-trucks
; Return a list containing only those trucks that are not off-screen
;
(define (remove-off-screen-trucks a-lot)
  (cond [(empty? a-lot) '()]
        [(cons? a-lot) (if (truck-off-screen? (first a-lot))
                           (cons (first a-lot) (remove-off-screen-trucks (rest a-lot)))
                           (remove-off-screen-trucks (rest a-lot)))]))

;------------------------Test cases for replenish-trucks------------------------
; Constants for testing
(define lot6 (cons t0 (cons t1 (cons t3 (cons (make-truck 1 150 1 1) '())))))

(check-expect (replenish-truck lot0) (cons (make-truck 1 150 1 1) lot0))
(check-expect (replenish-truck lot1) (cons (make-truck 1 150 1 1) lot1))
(check-expect (replenish-truck lot2) (cons (make-truck 1 150 1 1) lot2))
(check-expect (replenish-truck lot3) (cons (make-truck 1 150 1 1) lot3))
(check-expect (replenish-truck lot6) lot6)


; replenish-truck : list-of-trucks -> list-of-trucks
; Sometimes adds a new truck to the given list.
;
(define (replenish-truck a-lot)
  (cond [(empty? a-lot) (cons (make-truck 1 150 1 1) '())]
        [(cons? a-lot) (if (< (length a-lot) 4)
                           (cons (make-truck 1 150 1 1) (cons (first a-lot) (rest a-lot)))
                           (cons (first a-lot) (rest a-lot)))]))

;------------------------Test cases for truck-hitting-frog?------------------------
; Constants for testing
(define t6 (make-truck 300 200 1 1))
(define t7 (make-truck 150 250 1 1 ))
(define t8 (make-truck 350 300 1 -1))
(define t9 (make-truck 599 399 1 1))
            
(define f6 (make-frog 150 232 "up"))
(define f7 (make-frog 300 200 "left"))
(define f8 (make-frog 351 301 "down"))
(define f9 (make-frog 599 399 "right"))

(check-expect (truck-hitting-frog? t6 f6) #false)
(check-expect (truck-hitting-frog? t7 f7) #false)
(check-expect (truck-hitting-frog? t8 f8) #true)
(check-expect (truck-hitting-frog? t9 f9) #true)

; truck-hitting-frog? : truck, frog -> boolean
; Returns true if a frog and truck overlap
;
(define (truck-hitting-frog? truck frog) (if (overlap?
                                              (truck-pos-x truck) (truck-pos-y truck)
                                              32
                                              24
                                              (frog-pos-x frog) (frog-pos-y frog)
                                              32
                                              32)
                                          #true
                                          #false)) 

;------------------------Test cases for trucks-hitting-frog?------------------------
; Constants for testing
(define lot7 (cons (make-truck 150 232 1 1)
                   (cons t6
                         (cons t7
                               (cons t8
                                     (cons t9 '()))))))
(define f10 (make-frog 15 20 "up"))

(check-expect (trucks-hitting-frog? lot7 f6) #true)
(check-expect (trucks-hitting-frog? lot7 f7) #true)
(check-expect (trucks-hitting-frog? lot7 f8) #true)
(check-expect (trucks-hitting-frog? lot7 f9) #true)
(check-expect (trucks-hitting-frog? lot7 f10) #false)
(check-expect (trucks-hitting-frog? (cons (make-truck 157 219 1 -1)
                                          (cons (make-truck 298 300 1 1)
                                                (cons (make-truck 400 600 1 -1) '())))
                                    f10) #false)

; trucks-hitting-frog? : list-of-trucks frog -> boolean
; Returns true if any truck in the list is hitting the given frog
;
(define (trucks-hitting-frog? a-lot frog)
  (cond [(empty? a-lot) #false]
        [(cons? a-lot) (if (truck-hitting-frog? (first a-lot) frog)
                           #true
                           (trucks-hitting-frog? (rest a-lot) frog))]))

;------------------------Test cases for game-over?------------------------
; Constants for testing
(define f11 (make-frog 15 300 "up"))
(define p11 (make-princess 15 300 1 50))
(define win-world (make-world f11 p11 lot7))
(define lost-world (make-world f6 p11 lot7))
(define playing-frog (make-frog 300 300 "up"))
(define playing-world (make-world playing-frog p11 lot7))

(check-expect (game-over? win-world) #true)
(check-expect (game-over? lost-world) #true)
(check-expect (game-over? playing-world) #false)

; game-over? : world -> boolean
; Returns true if a frog is hit by a truck, or the frog has reached the princess
;
(define (game-over? a-world) (or (trucks-hitting-frog?
                                  (world-list-of-trucks a-world)
                                  (world-frog a-world))
                                 (frog-hitting-princess?
                                  (world-frog a-world)
                                  (world-princess a-world))))

;------------------------Test cases for frog-hitting-princess?------------------------
; Constants for testing
(define p12 (make-princess 200 60 1 500))
(define p13 (make-princess 0 0 1 900))
(define p14 (make-princess 599 399 -1 1000))
; Constants for testing
(define f12 (make-frog 60 200 "up"))
(define f13 (make-frog 0 0 "left"))
(define f14 (make-frog 570 370 "right"))

(check-expect (frog-hitting-princess? f12 p12) #false)
(check-expect (frog-hitting-princess? f13 p13) #true)
(check-expect (frog-hitting-princess? f14 p14) #true)

; frog-hitting-princess? : frog, princess -> boolean
; Returns true if a frog is hitting a princess
;
(define (frog-hitting-princess? frog princess) (if (overlap?
                                                    (frog-pos-x frog)
                                                    (frog-pos-y frog)
                                                    32
                                                    32
                                                    (princess-pos-x princess)
                                                    (princess-pos-y princess)
                                                    32
                                                    32)
                                                   #true
                                                   #false))

; Main for running the frogger game
(big-bang playing-world
    [on-tick update-world]
    [on-draw draw-world]
    [on-key world-handle-key]
    [stop-when game-over?])










                                   