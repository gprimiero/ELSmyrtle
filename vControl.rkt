;---------------------------------
;|       David Chen @ MDX        |
;---------------------------------

#lang racket
(require "oauth-single-user.rkt") ;OAUTH
(require "WeatherGrab.rkt")
(require racket/date) ;Racket Date
(require json) ;JSON Parser
( ;Provide Global Function
 provide
 ToggleVoice
 )

( ;OAUTH Connection
 define auth
  (
   new oauth-single-user%
       [consumer-key "NEEDED"]
       [consumer-secret "NEEDED"]
       [access-token "NEEDED"]
       [access-token-secret "NEEDED"]       
   )
 )

( ;Post Tweets to Twitter
 define (tweet x)
  (
   send auth
        post-request "https://api.twitter.com/1.1/statuses/update.json" ;Twitter API connection URL
        ;post-request "https://api.twitter.com/1.1/statuses/update_with_media.json"
        (list (cons 'status x))
   )
 )

( ;This function will be responsible for matching and executing the command(s) that has been parsed
 define (commandExecute s)
  (if (> (string-length s) 8) (set! s (substring s 8)) null) ;Remove unwanted text in voice result
  (
   if (string=? (string-upcase s) "TWITTER") ;Result = Twitter
      (and (tweet (string-append "Hello Twitter =: from Pi voice recognition!" (number->string (current-seconds)))) (printf "Posted\n"))
      null
      )
;  (
;   if (regexp-match #rx"WEATHER" (string-upcase s)) ;Result = Twitter
      (
       let ([i (regexp-match* #rx"ABERDEEN|ARMAGH|BANGOR|BATH|BELFAST|BIRMINGHAM|BRADFORD|BRISTOL|CAMBRIDGE
|CANTERBURY|CARDIFF|CARLISLE|CHESTER|CHICHESTER|LONDON|COVENTRY|DERBY|DUNDEE|DURHAM|EDINBURGH|ELY|EXETER|GLASGOW|GLOUCESTER|HEREFORD|INVERNESS|KINGSTON|LANCASTER|LEEDS
|LEICESTER|LICHFIELD|LINCOLN|LISBURN|LIVERPOOL|LONDONDERRY|MANCHESTER|NEWCASTLE|NEWPORT|NEWRY|NORWICH|NOTTINGHAM|OXFORD|PETERBOROUGH|PLYMOUTH|PORTSMOUTH|PRESTON|RIPON
|SALFORD|SALISBURY|SHEFFIELD|SOUTHAMPTON|ST ALBANS|ST DAVIDS|STIRLING|SUNDERLAND|SWANSEA|TRURO|WAKEFIELD|WELLS|WESTMINSTER|WINCHESTER|WOLVERHAMPTON|WORCESTER|YORK" (string-upcase s))])
        (
         if (not (empty? i))            
            (out (first i))
            null
         )
       )
;      null
;      )
  ;(write-to-file null "/tmp/voice.txt" #:exists 'replace)
 )


(define readit (open-input-file "/tmp/voice.txt")) ;Open voice file
( ;Read voice file till end on first run
 define (discardInitialContent)
  (define reader (read-line readit))
  (
   if (and (not (eof-object? reader)) (not (equal? reader "()"))) ;If not end of file
      (discardInitialContent)
      null
   )  
 )

( ;This function will loop and read the voice.txt file until it finds an command
 define (commandLoop)
  (define reader (read-line readit))
  (
   if (and (not (eof-object? reader)) (not (equal? reader "()"))) ;If not end of file
      (commandExecute reader)
      null
   )
  (commandLoop)
 )

( ;Initial Starter
 define (ToggleVoice)
  (discardInitialContent)
  (printf "Ready\n")
  (commandLoop)
 )