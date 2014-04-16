;---------------------------------
;|       David Chen @ MDX        |
;---------------------------------
;Current problems (doesnt do espeak + doesnt kill voice recoginition)
;Pre-Configurations and Global Variable Definitions===============================================================================================================

#lang racket
(require "MIRTOlib.rkt") ;Myrtle Library
(require racket/system) ;Racket System Use For CLI
(require web-server/servlet web-server/servlet-env) ;Web Server App
(require net/uri-codec)

(setup) ;Setup For Myrtle
(enableIR) ;Enable IR Sensors On Myrtle
(define verify null) ;Controller Verification

(define (say mytext)
  (process (string-append "espeak --stdout -s 150 -a 200 -ven+f3 \"" mytext "\" 2> /dev/null | aplay -q"))
  )

;=================================================================================================================================================================
(
 define (dweb x) ;Main function
  (define posts (request-bindings x)) ;Get Parameters
  
  (cond 
   
   ;Server Link
   ( (and (exists-binding? 'link posts) (exists-binding? 'v posts)) ;If link command and GUID exists
      (
       if(null? verify) ;If server verify is empty
         (and (set! verify (first (extract-bindings 'v posts))) (response/xexpr verify)) ;Set verify and response back
         (and (response/xexpr `(html (script "alert('This robot already linked!');")))) ;Response alert robot already linked
      )
      ) ;; end of link
      
   ( ;Server Unlink
    (and (exists-binding? 'unlink posts) (exists-binding? 'v posts)) ;If unlink command and GUID exists
      (
       if(null? verify) ;If server verify is empty
         (and (response/xexpr `(html (script "alert('This robot is empty!');")))) ;Response alert robot is empty
         (
          if (equal? verify (first (extract-bindings 'v posts))) ;If server verify = GUID
             (and (set! verify null) (response/xexpr "unlinked")) ;Set server verify to null and response back unlinked
             (and (response/xexpr `(html (script "alert('Verification does not match!');")))) ;Response back alert GUID does not match
             )
         )
      ) ;; end of unlink
      
  ( ;Other Commands
   (and (exists-binding? 'v posts) (equal? verify (first (extract-bindings 'v posts)))) ;Check if verification exist and match
   
   (cond 
     ( ;Get IR Sensors
      (exists-binding? 'getir posts)
      (response/xexpr `(div
                        (span ((style "margin-left:50px; color:#09F; background:black; font-size:30px; width:100px; height:100px;")) ,(number->string (getIR 3)))
                        (span ((style "margin-left:50px; color:#09F; background:black; font-size:30px; width:100px; height:100px;")) ,(number->string (getIR 2)))
                        (span ((style "margin-left:50px; color:#09F; background:black; font-size:30px; width:100px; height:100px;")) ,(number->string (getIR 1)))))
      )
     ( ;Get Bumper
      (exists-binding? 'getbump posts)
      (response/xexpr `(div
                        (img ((src ,(if (leftBump?) "img/LBumper_O.png" "img/LBumper.png")) (id "lb")))
                        (img ((src ,(if (rightBump?) "img/RBumper_O.png" "img/RBumper.png")) (id "rb")))
                        ))
      )
                        
     ( ;This is for movement
      (and (exists-binding? 'move posts) (and (exists-binding? 'motor1 posts) (exists-binding? 'motor2 posts)))
      (and ;SetMotors and return string "Moved"
       (setMotors
        (string->number (first (extract-bindings 'motor1 posts)))
        (string->number (first (extract-bindings 'motor2 posts))))
       (response/xexpr "Moved"))
      )

     ( ;This is for stop
      (exists-binding? 'stop posts)
      (and (stopMotors) (response/xexpr "Stopped")) ;Stop motor and return string "Stopped"
      )
        
     ( ;Action checker
      (and (exists-binding? 'act posts) (exists-binding? 'command posts) (equal? (first (extract-bindings 'command posts)) "capimg")) ;Toggle Image Capture
      (
       let ([imgname (number->string (current-seconds))]) ;Let image name be the current seconds
        (and (system (string-append "raspistill -t 500 -o /home/pi/ELSmyrtle/cap/" imgname ".jpeg")) ;System out capture image
             (response/xexpr `(img ((width "25%") (height "25%") (src ,(string-append "/cap/" imgname ".jpeg"))))))
        )
      )
        
     ( ;This is for speech
      (and (exists-binding? 'talk posts) (and (exists-binding? 'text posts) ))
      ;; Talk here! 
      (say (uri-decode (first (extract-bindings 'text posts))))
      (response/xexpr "Talked")

       )
        
        
        ) ;; end of cond verification exists and matches.
   ) 
  (else 
      (response/xexpr "NONE")
      )
  
  ) ;; end of big cond
  ) ;; end of dweb
               
 ;;           ( ;This is for line following
 ;;            if (and (exists-binding? 'act posts) (exists-binding? 'command posts) (equal? (first (extract-bindings 'command posts)) "lfon")) ;Toggle Line Follow ON
 ;;               (and (set! lfollow (thread (lambda () (controlLoop)))) (response/xexpr "Following Line"))
 ;;               ( ;This is for stop line following
 ;;                if (and (exists-binding? 'act posts) (exists-binding? 'command posts) (equal? (first (extract-bindings 'command posts)) "lfoff")) ;Toggle Line Follow OFF
 ;;                   (and (kill-thread lfollow) (stopMotors) (response/xexpr "Stop Following"))
                                            
                                            
                                            ;( ;This is for voice recognition
                                             ;if (and (exists-binding? 'act posts) (exists-binding? 'command posts) (equal? (first (extract-bindings 'command posts)) "talkon")) ;Toggle Voice Recognition ON
                                              ;  (and 
                                               ;  (response/xexpr "Activating...")
                                                ; (set! bgvoice2 (thread (lambda () (system "/home/pi/sphinx/pocketsphinx-0.8/src/programs/pocketsphinx_continuous -lm /home/pi/robot/language/sphinx/7743.lm -dict /home/pi/robot/language/sphinx/7743.dic > /tmp/voice.txt &")
                                                 ;                         (system "ps ax | grep sphinx > /tmp/sid.txt"))))
                                                 ;(set! talker (thread (lambda () (ToggleVoice))))
                                                 ;(response/xexpr "Talk On"))
                                                ;( ;This is for stop voice recognition
                                                 ;if (and (exists-binding? 'act posts) (exists-binding? 'command posts) (equal? (first (extract-bindings 'command posts)) "talkoff")) ;Toggle Voice Recognition OFF
                                                  ;  (and 
                                                     ;(set! bgvoice (thread (lambda () (system (string-append "kill " (number->string (read (open-input-file "/tmp/sid.txt"))))))))
                                                   ;  (killbg) ;Call kill voice recognition function
                                                     ;(kill-thread talker) ;Kill the thread of voice recognition
                                                    ; (response/xexpr "Talk Off")) ;Return string to client
                                                    ;(response/xexpr "NONE") ;This is when no command has been found above!!
                                                    ;);
;;                                                )
;;                                            )
;;                                        )
;;                                    )
;;                                )
;;                            )
;;                        )
;;                    )
;;                )
;;              (response/xexpr "NONE")
;;              )
;;      )  
;;   )
;; )
;=================================================================================================================================================================

;Toggle Web Server
(serve/servlet dweb 
               #:listen-ip #f
               #:launch-browser? #f
               #:port 80              
               #:servlet-path "/mdx"
               #:extra-files-paths (list (build-path "./")))
