#lang racket
(
 provide
 out
 )
(require json) ;Require JSON Library
(require net/url) ;Require NET Lib
(require racket/system) ;Require System

(
 define (out x)
  (define WeatherJSON (string->url (string-append "http://api.openweathermap.org/data/2.5/weather?q=" x))) ;Grab Weather API
  (define SourcePort (get-pure-port WeatherJSON #:redirections 5)) ;Get JSON Port
  (define WeatherSTR (port->string SourcePort)) ;Convert Port to String
  ;(printf "Location: ~a\n" (hash-ref (string->jsexpr WeatherSTR) 'name))
  (define temp (inexact->exact (round (- (hash-ref (hash-ref (string->jsexpr WeatherSTR) 'main) 'temp) 273)))) ;Define temperature
  (define wind (inexact->exact (round (hash-ref (hash-ref (string->jsexpr WeatherSTR) 'wind) 'speed)))) ;Define wind speed
;  (system (string-append "espeak -s150 -a200 -ven+f3 'The current weather in " x " is " 
;                         (number->string temp) " degrees with wind speed of " 
;                         (number->string wind) " miles per hour ' -w/tmp/test.wav"))
  (sleep 1)
  ;Text to speech and convert to test.wav
  (system "espeak -s150 -a200 -ven+f3 'The current weather in London is 15 degrees with speed of 7 miles per hour' -w/tmp/test.wav")
  (sleep 1)
  ;Play test.wav
  (system "omxplayer --vol -800 /tmp/test.wav")
 )