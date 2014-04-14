#lang racket

(require "MIRTOlib.rkt")

;; ---------------------------------------------------------
;; A simple line following control loop that uses THREE IR sensor
;; it recognizes corners
;; prints the value of IR sensors 
;; every 2 seconds (interval can be modified) and prints
;; when bump sensors are pressed/released
;; ---------------------------------------------------------
(provide
 controlLoop 
 )

(define previousTime (current-inexact-milliseconds))
(define currentTime 0)
(define valueMiddle #f)
(define valueLeft #f)
(define valueRight #f)
(define isLeft 0)
(define isRight 0)

;; the left motor counter, initially 0
(define leftCount 0)
;; the right motor counter 
(define rightCount 0)


;; How often should we print?
(define interval 1)

;; The list of IR sensors (used in map below)
(define irSensors (list 1 2 3))

;; Store previous bumper(s) state
(define previousLeft #f)
(define previousRight #f)


;; Store previous states wrt the line
(define previousStateLeft #f)
(define previousStateRight #f)


;; Define directions states wrt the line
(define goingLeft #t)
(define goingRight #f)


(define (controlLoop)
  
  (set! currentTime (current-inexact-milliseconds))
  
  ;; Print IR
  (cond ( (> (- currentTime previousTime) interval)
          ;; We use a map function to print the value
          ; (map (? (id) (printf "IR sensor ~a -> ~a; " id (getIR id))) irSensors)
          ;;(map (? (id) irSensors))
          ;(printf "isLeft ~a" isLeft)
          
          ;;We set the values of the Left, Middle and Right Sensors
          (set! valueMiddle (getIR 2))
          (set! valueRight (getIR 1))
          (set! valueLeft (getIR 3))
          (set! previousTime (current-inexact-milliseconds))
          
          
          ;; If the Right Sensor is on the balck tape, move towards the Right
          ;;
          (cond ( (> valueRight 100)
                  (set! isLeft 0)
                  (printf "Right")
                  (printf "\n") 
                  (setMotors 75 -20)
                  ))
          
          
          
          ;; If Myrtle is on the line
          (cond ( (> valueMiddle 100)
                  (printf "Middle")
                  (printf "\n") 
                  
                  ;; and the Left sensor is on Black as well
                  (cond ( (> valueLeft 100)
                          (printf "Turning Left")
                          (printf "\n")
                          (set! isLeft 1) 
                          (stopMotors)
                          ;(moveForward)
                          (setMotors 80 -80)
                          ))
                  
                  ;; or the Right sensor is on Black as well
                  (cond ( (> valueRight 100)
                          (printf "Turning Right")
                          (printf "\n")
                          (set! isRight 1) 
                          (stopMotors)
                          ;(moveForward)
                          (setMotors 80 -80)
                          ))
                  
                  ;;or both the Right and the Left sensors are on Black
                  (cond (( and (> valueRight 100)(> valueLeft 100))
                          (printf "T_Junction")
                          (printf "\n")
                          (set! isRight 1)
                          (set! isLeft 1) 
                          
                          (stopMotors)
                          ;(moveForward)
                          (setMotors 80 -80)

                          ))
                  
                  ))
          

          
          
          (cond 
            ;;if both Left and Middle Sensors are on the black tape
            ((and (> valueLeft 100) (<= valueMiddle 100))
                      (printf "Left")
                      (printf "\n") 
                      ;;move Left
                      (setMotors 20 -75)
                      ))
          
          
          ;; If all sensors are on white
          (cond ((and (< valueRight 100) (< valueLeft 100) (< valueMiddle 100))
                (stopMotors)
                ;;and the previousstate is Left
                (cond ( (= isLeft 1)
                        ;;then move to the left
                        (setMotors 20 -75)
                        )
                      ;;if instead the previousstate is Right
                      ( (= isRight 1)
                        ;;then move to the Right
                        (setMotors 75 -20)
                        )
                      ;;otherwise, just rotate
                      (else (setMotors 35 -75)
                      )
                      ))
                )
          )
        )
  
  (sleep 0.02)
  (controlLoop)
)