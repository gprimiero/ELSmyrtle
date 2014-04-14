#lang racket

;; *******************************
;; **** RACKET FIRMATA CLIENT ****
;; *******************************
;;
;; Authors: Bob Field + Franco Raimondi + Nikos Gkorogiannis
;; {b.fields|f.raimondi|n.gkorogiannis}@mdx.ac.uk
;; 
;;
;; This code is a re-implementation of the code available here:
;; http://planet.racket-lang.org/package-source/xtofs/firmata.plt/1/0/planet-docs/firmata/index.html
;;
;; It support multiple platform (Linux, Mac, Windows) and SysEx 
;; messages for the Middlesex Robotic Platform (MIRTO aka Myrtle).


(provide is-pin-set?
         is-arduino-pin-set?
         set-pin!
         clear-pin!
         set-pin-mode!
         report-analog-pin!
         report-digital-port!
         set-arduino-pin!
         clear-arduino-pin!
         read-analog-pin
         request-version
         open-firmata
         close-firmata
         on-button-pressed
         analog-write!
         send-sysex-msg
         send-sysex-byte-msg
         send-sysex-int-msg
         motor1-read-count
         motor2-read-count
         motor1-reset-count
         motor2-reset-count
         INPUT_MODE
         OUTPUT_MODE
         ANALOG_MODE
         PWM_MODE
         SERVO_MODE
         ON
         OFF)

;; The format of SysEx messages is specific to the Myrtle platform, 
;; see https://github.com/fraimondi/myrtle
;;
;; /* Myrtle requests 
;; * ------------------------------
;; * 0  START_SYSEX (0xF0)
;; * 1  MYRTLE_DATA Command (0x7D) // 0x7D looks to be a free sysex tag
;; * 2  encoder request tag indicates the nature of the request
;; * 3  END_SYSEX (0xF7)  
;; */

(define MYRTLE-DATA #x7D)
;; The following are the possible tags for encoded requests:
(define ENCODER-REQUEST 1)
(define ENABLE-SPONTANEUS-MSGS 2)
(define DISABLE-SPONTANEUS-MSGS 3)
(define SET-SPONTANEUS-MSGS-INTERVAL 4)
(define SET-WHEEL1-SPEED 5)
(define SET-WHEEL2-SPEED 6)
(define IRSENSOR-REQUEST 7)
(define BUMPSWITCH-REQUEST 8)

;; /* Encoder data reply 
;; * ------------------------------
;; * 0  START_SYSEX (0xF0)
;; * 1  MYRTLE_DATA Command (0x7D) 
;; * 2  encoder reply tag (1)
;; * 3  Body length   number of bytes following the tag excluding END_SYSEX    
;; * 4  time_m1 bit  0-6  // duration of most recent encoder pulse for motor 1
;; * 5  time_m1 bit  7-13 // each unit is 10 microseconds (value = 0 if motor stopped)
;; * 6  count_m1 bit 0-6  // pulse count for motor 1 since last request (0 if first request)
;; * 7  count_m1 bit 7-13
;; * 8  time_m2 bit  0-6  // duration for motor 2 encoder
;; * 9  time_m2 bit  7-13
;; * 10 count_m2 bit 0-6  // pulse count for motor 2
;; * 11 count_m1 bit 7-13
;; * 12 END_SYSEX (0xF7) 
;; */
;; /* IRSENSOR_REQUEST data reply 
;; * ------------------------------
;; * 0  START_SYSEX (0xF0)
;; * 1  MYRTLE_DATA Command (0x7D) 
;; * 2  IRSENSOR_REQUEST tag (7)
;; * 3  Body length   number of bytes following the tag excluding END_SYSEX   
;; * 4  sensor1 bit  0-6  // analog read data is in bits 0-9 
;; * 5  sensor1 bit  7-13 
;; * 6  sensor2  bit 0-6 
;; * 7  sensor2  bit 7-13
;; * 8  sensor3  bit  0-6  
;; * 9  sensor3  bit  7-13
;; * 10 sensor4  bit 0-6    // reserved for future use
;; * 11 sensor4  bit 7-13
;; * 12 END_SYSEX (0xF7) 
;; */
;;
;;  /* BUMPSWITCH_REQUEST data reply 
;; * ------------------------------
;; * 0  START_SYSEX (0xF0)
;; * 1  MYRTLE_DATA Command (0x7D) 
;; * 2  BUMPSWITCH_REQUEST tag (8)
;; * 3  Body length   number of bytes following the tag excluding END_SYSEX 
;; * 4  left switch  big 0-6  // bit 0 is 1 when switch pressed, else 0
;; * 5  right switch bit 0-6 
;; * 6  reserved    bit 0-6   // reserved for future use
;; * 7  reserved    bit 0-6 
;; * 8 END_SYSEX (0xF7) 
;; */

;; Additional constants for reply
(define ENCODER-REPLY 1)
(define ENCODER-BODY-LEN 8)
(define RX-MSG-HEADER-LEN 3)
(define RX-MSG-LEN 12)
(define RX-MSG-BODY-LEN (- RX-MSG-LEN 4))
(define PULSE-WIDTH-SCALE 10)


; bit-operations
(require file/sha1)
; Needed for setting the terminal baudrate
(require racket/system)
; Needed for keeping track of the current pins
(require rnrs/bytevectors-6)

;; Operating system: can be linux, mac, win
;(define oses (list "mac" "linux" "win"))
(define (detect-os)
  (define racket-os (system-type))
  (cond
    [(equal? racket-os 'windows) "win"]
    [else 
     (let ([machine (first (string-split (system-type 'machine)))])
       (match machine
         ["Darwin" "mac"]
         ["Linux" "linux"]
         [_ (raise "Unknown OS detected in firmata")] 
         ))]))

(define os (detect-os))

;; don't connect devices through USB that expose serial interfaces at the same time as the arduino
;; (such as mobile phones with tethering on)
;; OR anything on the serial ports
(define (find-port check-port prefix num)
  (if (<= 0 num 255)
      (let ([port (string-append prefix (number->string num))])
        (if (check-port port) port (find-port check-port prefix (+ 1 num))))
      #f))

(define (find-mac-port)
  (string-append "/dev/" (path->string (first (for/list ([f (directory-list "/dev")] #:when (regexp-match? "tty.usbmodem*" f))
     f))))
  )

(define (valid-com-port port)
  (system (string-append "mode " port " > NUL")))

(define (get-port)
  (match os
    ("linux" (or (find-port file-exists? "/dev/ttyACM" 0)  (find-port file-exists? "/dev/ttyAMA" 0)))
    ("win" (find-port valid-com-port "COM" 3))
    ("mac" (find-mac-port))
    (_ (raise "Don't know how to find port on this OS"))
    ))
  

;----------------------------------------------------------------------
; Firmata Protocol Constants
;----------------------------------------------------------------------
(define DIGITAL-MESSAGE  #x90) ;;send data for a digital port
(define ANALOG-MESSAGE   #xE0) ;;send data for an analog pin (or PWM)
(define REPORT-ANALOG    #xC0) ;;enable analog input by pin #
(define REPORT-DIGITAL   #xD0) ;;enable digital input by port
(define SET-PIN-MODE     #xF4) ;;set a pin to INPUT/OUTPUT/PWM/etc
(define REPORT-VERSION   #xF9) ;;report firmware version
(define SYSTEM-RESET     #xFF) ;;reset from MIDI
(define START-SYSEX      #xF0) ;;start a MIDI SysEx message
(define END-SYSEX        #xF7) ;;end a MIDI SysEx message




(define INPUT_MODE          0)
(define OUTPUT_MODE         1)
(define ANALOG_MODE         2)
(define PWM_MODE            3)
(define SERVO_MODE          4)
(define ON                  1)
(define OFF                 0)

(define BAUDRATE        57600)
(define ANALOG-PINS        16) ;;;Firmata supports up to 16 analog pins
(define DIGITAL-PORTS      16) ;;;Firmata supports up to 16 digital ports

;----------------------------------------------------------------------
; Module constants to keep track of the input and output ports and
; the thread that is started for reading, these constants should not
; leak to the user of the library.
;----------------------------------------------------------------------
(define in                null) 
(define out               null)
(define read-thread       null)
(define registrations      '())

;; ----------------------------------------------------------------------
;; Franco: This code is specific for Myrtle motors
;; ----------------------------------------------------------------------
(define MOTOR1-COUNT 0)
(define MOTOR2-COUNT 0)

(define (motor1-reset-count) 
  (set! MOTOR1-COUNT 0)
  )

(define (motor2-reset-count) 
  (set! MOTOR2-COUNT 0)
  )

(define (motor1-read-count)
  MOTOR1-COUNT
  )

(define (motor2-read-count)
  MOTOR2-COUNT
  )


;----------------------------------------------------------------------
; Datastructures to keep track of the pins
;----------------------------------------------------------------------
(define ANALOG-IO-PINS (make-vector ANALOG-PINS))

;Every port is configured as a byte
(define DIGITAL-IO-PINS (make-bytevector DIGITAL-PORTS))

;----------------------------------------------------------------------
; Abstractions to update the internal datastructures
;----------------------------------------------------------------------
(define (update-analog-pin! pin value)
  (vector-set! ANALOG-IO-PINS pin value))

;Used by the programmer to read the latest value of an analog pin
(define (read-analog-pin pin)
  (vector-ref ANALOG-IO-PINS pin))

;Internal function to update the DIGITAL IO PINS
(define (update-digital-port! port value)
  (when (<= value 255)
    (for-each (lambda (p)
                (let ((pin (car p))
                      (f (cdr p)))
                  (when (and 
                         ;it is the same port
                         (= port (quotient pin 8))
                         ;it is currently not set
                         (not (is-arduino-pin-set? pin))
                         ;in the update it is set
                         (is-bit-set? (remainder pin 8) value))
                    (f))))
              registrations)
    (bytevector-u8-set! DIGITAL-IO-PINS port value)))

(define (is-bit-set? bit value)
  (> (bitwise-and 
      value
      ;Create the number where only the pin to be set is 1 = 1 << pin
       (bitwise-and 
        (arithmetic-shift 1 bit)
        #xFF)) 0))

;Used by the programmer to verify that a pin is set on a certain port
(define (is-pin-set? port pin)
   (is-bit-set? pin (bytevector-u8-ref DIGITAL-IO-PINS port)))


(define (read-hook)
  (printf "Read thread started ...")
  (read-loop))

(define (read-loop)
  (process-input (read-byte in))
  (read-loop))

;TODO implement sysex messages
(define (process-input data)
  (cond
    ((equal? data eof) void)
    
    ((= (bitwise-and data #xF0) ANALOG-MESSAGE)
     (let ((lsb (read-byte in))
           (msb (read-byte in))
           (analog-pin (bitwise-and data #xF)))
       (update-analog-pin! analog-pin (bitwise-ior (arithmetic-shift msb 8) lsb))))
    
    ((= (bitwise-and data #xF0) DIGITAL-MESSAGE)
     (let ((lsb (read-byte in))
           (msb (read-byte in))
           (port (bitwise-and data #xF)))
       (update-digital-port! port (bitwise-ior (arithmetic-shift msb 8) lsb))))
    
;;    ((= data REPORT-VERSION) 
;;     (let ((major (read-byte in))
;;           (minor (read-byte in)))
;;       (printf "FIRMATA VERSION DETECTED ~a.~a"  major minor)))
    
    ;; Franco 03.12.2013: reading sysex
    ((= data START-SYSEX)
     ;; Franco 28.01.2014
     ;; build the received message as a list of bytes
     (define rec-msg (list))
     (let sysex-read-loop ()
       (define cmd (read-byte in))
       (set! rec-msg (cons cmd rec-msg))
       (when (not (= cmd END-SYSEX))   
         (sysex-read-loop))
       )
     (set! rec-msg (reverse rec-msg))
     ;; (printf "DEBUG: sys-ex message is ~a\n" rec-msg)
            
     (cond ( (= (first rec-msg) MYRTLE-DATA) 
             (cond ( (= (second rec-msg) ENCODER-REPLY)
                     ;; FIXME: hardcoded message size
                     (cond ( (and (> (length rec-msg) 11) (< (length rec-msg) 20))
                            (define msgBody0 (list-ref rec-msg 3))
                            (define msgBody1 (list-ref rec-msg 4))
                            (define msgBody2 (list-ref rec-msg 5))
                            (define msgBody3 (list-ref rec-msg 6))
                            (define msgBody4 (list-ref rec-msg 7))
                            (define msgBody5 (list-ref rec-msg 8))
                            (define msgBody6 (list-ref rec-msg 9))
                            (define msgBody7 (list-ref rec-msg 10))
                            
                            (define pulse1 (* (+ (arithmetic-shift msgBody1 7) msgBody0) PULSE-WIDTH-SCALE))
                            (define count1 (convertSignedTwoByteValue (+ (arithmetic-shift msgBody3 7) msgBody2)))
                            (set! MOTOR1-COUNT (+ MOTOR1-COUNT count1))
                            ;;(printf "DEBUG SYSEX: pulse1 is ~a and count1 is ~a, msgBody2 is ~a\n" pulse1 count1 msgBody2)
                            
                            (define pulse2 (* (+ (arithmetic-shift msgBody5 7) msgBody4) PULSE-WIDTH-SCALE))
                            (define count2 (convertSignedTwoByteValue (+ (arithmetic-shift msgBody7 7) msgBody6)))
                            (set! MOTOR2-COUNT (+ MOTOR2-COUNT count2))                            
                            ;; (printf "DEBUG SYSEX: pulse2 is ~a and count2 is ~a\n" pulse2 count2)
                            )
                           (else (printf "WARNING: I receievd a sysex message of strange size: ~a\n" rec-msg))
                           )
                     ) ;; closes cond to parse ENCODER-REPLY
                           
                   )
             )
           ( (= (first rec-msg) REPORT-VERSION)
             (define major (second rec-msg))
             (define minor (third rec-msg))
             (printf "Version major and minor: ~a ~a\n" major minor)
           )
        )
     (set! rec-msg '())
     )
  )
)

(define (close-firmata) 
  (when (not (null? read-thread)) 
    (printf "Killing thread .... \n")
    (kill-thread read-thread)
    (set! read-thread null)
    (printf "Closing input .... \n")
    (close-input-port in)
    (printf "Flushing output .... \n")
    (flush-output out)
    (printf "Closing output .... \n")
    (close-output-port out)
    (set! in null)
    (set! out null)
    (printf "DrRacket-Firmata closed .... \n")))
 
(define (open-firmata)
  (define port-name (get-port))

  ;; We set the command line instruction to configure the serial port according to the OS;
  ;; we also configure the file name of the port to be opened (it is different in win)
  (define call-string null)
  (define filename null)
  (cond 
        ( (equal? os "linux") (set! call-string (string-append  "stty -F " port-name " cs8 57600 ignbrk -brkint -icrnl -imaxbel -opost -onlcr -isig -icanon -iexten -echo -echoe -echok -echoctl -echoke noflsh -ixon -crtscts"))
                         (set! filename port-name))        
        ( (equal? os "mac") (set! call-string (string-append  "stty -f " port-name " 57600 cs8 cread clocal"))
                       (set! filename port-name))
        ( (equal? os "win") (set! call-string (string-append  "mode " port-name ": baud=57600 parity=N data=8 stop=1"))                            
                       (set! filename (string-append "\\\\.\\" port-name)))
   ) ;; end of cond to set stty or mode string and filename.
  ;;(printf "DEBUG: call is ~a\n" call-string)

  (cond ( (equal? os "win") 
            (if (system call-string) ;; here we set the port
                (begin
                  (let-values ([(in-port out-port) (open-input-output-file filename)])
                    (set! in in-port)
                    (set! out out-port)
                    (file-stream-buffer-mode out 'none)
                    )
                  (sleep 3)
                  (set! read-thread (thread (lambda ()  (read-hook)))) ;; we set the reading thread
                  ;; Franco: If I don't add this line, it does not work in Win!
                  (report-analog-pin! 0 1)
                  #t)
                (error "Failed to open the connection with " port-name " verify if your microcontroller is plugged in correctly"))            
            )
        (else   
         (set! out (open-output-file port-name #:mode 'binary #:exists 'append))
         (set! in  (open-input-file  port-name #:mode 'binary))
         (file-stream-buffer-mode out 'none)
         (sleep 2)
         (if (system call-string) ;; here we set the port
             (begin
               (sleep 1)
               (set! read-thread (thread (lambda ()  (read-hook)))) ;; we set the reading thread
               ;; Franco: If I don't add this line, it does not work in Win!
               (report-analog-pin! 0 1)
               (printf "Success opening firmata\n")
               #t)
             (error "Failed to open the connection with " port-name " verify if your microcontroller is plugged in correctly"))            
            )               
         )
        ) ;; end of open-firmata
 
;----------------------------------------------------------------------
; Firmata Control Messages
;----------------------------------------------------------------------
(define (set-pin! port pin)
  (let* ((old-value (bytevector-u8-ref DIGITAL-IO-PINS port))
         (new-value (bitwise-ior old-value (arithmetic-shift 1 pin))))
    (write-byte (bitwise-ior DIGITAL-MESSAGE port) out)
    (write-byte (bitwise-and new-value #x3F) out)
    (write-byte (arithmetic-shift new-value -7) out)
    (bytevector-u8-set!  DIGITAL-IO-PINS port new-value); added - bob fields - 26.10.12
    (flush-output out)))

(define (clear-pin! port pin)
  (let* ((old-value (bytevector-u8-ref DIGITAL-IO-PINS port))
         (new-value (bitwise-and old-value (bitwise-not (arithmetic-shift 1 pin)))))
    (write-byte (bitwise-ior DIGITAL-MESSAGE port) out)
    (write-byte (bitwise-and new-value #x3F) out)
    (write-byte (arithmetic-shift new-value -7) out)
    (bytevector-u8-set!  DIGITAL-IO-PINS port new-value); added - bob fields - 26.10.12
    (flush-output out)))

; set-pin-mode!
;   pin-number (0-127)
;   mode (INPUT/OUTPUT/ANALOG/PWM/SERVO, 0/1/2/3/4)
(define (set-pin-mode! pin mode)
  (write-byte SET-PIN-MODE out)
  (write-byte pin out)
  (write-byte mode out)
  (flush-output out))
  
; report-analog-pin!
;  pin-number (0-16)
;  mode enable = 1 disable = 0
(define (report-analog-pin! pin-number mode) 
  (write-byte (bitwise-ior REPORT-ANALOG pin-number) out)
  (write-byte mode out)
  (flush-output out))
  
; report-digital-port!
;  port-number (0-16)
;  mode enable = 1 disable = 0
(define (report-digital-port! port-number mode) 
  (write-byte (bitwise-ior REPORT-DIGITAL port-number) out)
  (write-byte mode out)
  (flush-output out))

; request-version
(define (request-version)
  (write-byte REPORT-VERSION out)
  (flush-output out))

;; Franco 25.11.2013: to write PWM pins
(define (analog-write! pin value)
   (write-byte (bitwise-ior ANALOG-MESSAGE (bitwise-and pin #x0F)) out)
   (write-byte (bitwise-and value #x7F) out)
   (write-byte (arithmetic-shift value -7) out)
   (flush-output out)
  )


;; Franco 03.12.2013: to send Sysex messages
(define (send-sysex-msg cmd tag)
  (write-byte START-SYSEX out)
  (write-byte cmd out)
  (write-byte tag out)
  (write-byte END-SYSEX out)
  (flush-output out)
)

(define (send-sysex-byte-msg cmd tag value)
  (write-byte START-SYSEX out)
  (write-byte cmd out)
  (write-byte tag out)
  (write-byte value out)
  (write-byte END-SYSEX out)
  (flush-output out)
  )

(define (send-sysex-int-msg cmd tag value)
  (define val2 (convertValueAsTwo7bitBytes value))
  (define lsb (bitwise-and val2 #x7F) )
  (define msb (bitwise-and (arithmetic-shift val2 -7) #x7F))
;;  (printf "DEBUG: I am sending the following sequence: ~x ~x ~x ~x ~x\n" cmd tag lsb msb END-SYSEX)
  (write-byte START-SYSEX out)
  (write-byte cmd out)
  (write-byte tag out)
  (write-byte lsb out)
  (write-byte msb out)
  (write-byte END-SYSEX out)
  (flush-output out)
  )
  
(define (convertValueAsTwo7bitBytes value)
  (define signBit #x0)
  
  (cond ( (< value 0) 
           (set! signBit #x2000)
           (set! value (- value))
           )
         )
  (cond ( (> value #x1fff) (set! value #x1fff)))
  (set! value (bitwise-ior value signBit))
  value
)
          
  
(define (convertSignedTwoByteValue value)
  (cond ( (>= value #x2000) (- (bitwise-and value #x1fff)))
        (else value)
        )
  )

  
;----------------------------------------------------------------------
; Firmata Control Messages
;----------------------------------------------------------------------
(define (set-arduino-pin! pin)
  (set-pin! (quotient pin 8) (remainder pin 8)))

(define (clear-arduino-pin! pin)
  (clear-pin! (quotient pin 8) (remainder pin 8)))

(define (is-arduino-pin-set? pin)
  (is-pin-set? (quotient pin 8) (remainder pin 8)))

(define (on-button-pressed pin lambda)
  (set! registrations (append registrations (list (cons pin lambda))))
  (display registrations))
