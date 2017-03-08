# -----------------
(de *ACDC_Debug . NIL)

# -----------------
(class +ACDC_Scale)
# bits top qtr1 half qtr2

(dm T (B)
   (default B 58)
   (=: bits B)
   (=: top (- (** 2 B) 1))
   (=: qtr1 (+ 1 (/ (: top) 4)))
   (=: half (* 2 (: qtr1)))
   (=: qtr3 (* 3 (: qtr1)))
)

# -------------------        
(class +ACDC_Encoder)
# scale low high followBits buffer

(dm T (Scale)
   (default Scale (new '(+ACDC_Scale)))
   (=: scale Scale)
   (=: low 0)
   (=: high (; (: scale) top))
   (=: top (: high))
   (=: followBits 0)
   (=: buffer NIL) )

            
(dm outputBit> (Bit)
   (=: buffer (cons Bit (: buffer)))
   (while (gt0 (: followBits))
      (=: buffer (cons (x| 1 Bit) (: buffer)))
      (=: followBits (- (: followBits) 1)) ) )

(dm encode> (L H Total)
   (if (or (> H (: top)) 
      (>= L H) 
      (> Total (: top)))
         (throw (text "Encoder/encode: data error; Low,High,Total: @1,@2,@3 Top: @4" L H Total (: top))) )
   

   (let (Range (+ (- (: high) (: low)) 1)
      Half (; (: scale) half)
      Qtr1 (; (: scale) qtr1)
      Qtr3 (; (: scale) qtr3) ) 
      
      (=: high (+ (: low) (- (/ (* Range H) Total) 1)))
      (=: low  (+ (: low) (/ (* Range L) Total)))

      (if (> (: high) (: top)) 
         (throw "Encoder/encode: high > top") )
      (if (>= (: low)  (: high)) 
         (throw "Encoder/encode: low >= high") )

      (loop
         (NIL
            (cond
               ((< (: high) Half)
                  (outputBit> This 0) 
                  T )
               ((>= (: low) Half)
                  (outputBit> This 1)
                  (=: low  (- (: low) Half))
                  (=: high (- (: high) Half))
                  T )
               ((and (>= (: low) Qtr1) (< (: high) Qtr3))
                  (=: followBits (+ 1 (: followBits)))
                  (=: low (- (: low) Qtr1))
                  (=: high (- (: high) Qtr1))
                  T )
               (T NIL) ) )
         
         (=: low (* 2 (: low)))
         (=: high (+ 1 (* 2 (: high)))) ) ) )


(dm finalize> ()
   (=: followBits (+ 1 (: followBits)))
   (if (< (: low) (; (: scale) half))
      (outputBit> This 1)
      (outputBit> This 0) )
   (: buffer) )

# -------------------        
(class +ACDC_Decoder)
# scale low high followBits buffer eof

(dm T (Buffer Scale)
   (default Scale (new '(+ACDC_Scale)))
   (=: scale Scale)
   (=: buffer Buffer)
   (=: low 0)
   (=: high (; (: scale) top))
   (=: top (: high))
   (=: value 0)
   (=: bitCount 0) )

(dm eof> (Flg)
   (when Flg 
      (=: eof Flg) )
   (: eof) )
   
(dm decodeTarget> (Total)
   (let (
      N (- (* Total (+ (- (: value) (: low)) 1)) 1)
      D (+ (- (: high) (: low)) 1) )
      (/ N D) ) )

(dm getNextBit> ()
   (=: value (+ (: value) (or (car (: buffer)) 0)))
   (=: buffer (cdr (: buffer)) )
)
   
(dm startDecode> ()
   (=: eof NIL)
   (=: value 0)
   (let (Bits (; (: scale) bits)) 
      (do Bits
         (=: value (>> -1 (: value))) 
         (getNextBit> This) ) )
)

(dm decode> (L H Total)
   (let (Range (+ (- (: high) (: low)) 1) 
      Half (; (: scale) half)
      Qtr1 (; (: scale) qtr1)
      Qtr3 (; (: scale) qtr3) ) 
      
      (=: high (+ (: low) (- (/ (* Range H) Total) 1)))
      (=: low  (+ (: low) (/ (* Range L) Total)) )
         
      (loop
         (NIL
            (cond
               ((< (: high) Half) T)
               ((>= (: low) Half)
                  (=: value (- (: value) Half))
                  (=: low (- (: low) Half))
                  (=: high (- (: high) Half))
                  T )
               ((and (>= (: low) Qtr1) (< (: high) Qtr3))
                  (=: value (- (: value) Qtr1))
                  (=: low (- (: low) Qtr1))
                  (=: high (- (: high) Qtr1))
                  T )
               (T NIL) ) )
            
            
         (=: low (* 2 (: low)))
         (=: high (+ 1 (* 2 (: high))))

         (=: value (* 2 (: value)))
         (getNextBit> This) 
               
         (if (or (< (: value) 0) (> (: value) (: top)))
            (throw (text "Decoder error: bad value: @1" (: value)) ) ) ) ) )  

   
# ----------------------------------------
# Simple Adaptive model for encoding bytes   
(class +ACDC_BasicModel)
# counts max total eofSym

(dm T (Coder)
   (if (not Coder) 
      (throw "Arithmetic encoder required"))
   (=: coder Coder)                 # encoder|decoder instance
   (=: max 257)                     # number of symbols (0..255 + 1 eof)
   (=: total (: max))               # total probability
   (=: eofSym (+ (: max) -1))       # eof symbol
   (=: topValue (; (; (: coder) scale) top))
   (let (Tmp NIL)                   # use idx tree for model probabilities
      (for X (: max) (idx 'Tmp (list (dec X) 1) T))
      (balance 'Tmp Tmp)
      (=: counts Tmp) )
)

(dm rescale> ()
   # TODO: Rescale model; not necessary unless
   # very huge message or very low precision
   (throw "BasicModel/rescale: Insufficient precision") )

# Update the model for a symbols occurrence  
(dm update> (Sym) 
   (let (  
      Tmp (lup (: counts) Sym) )          # Lookup the symbol
      (if (not Tmp) (throw (text "Model/update: bad Sym; @1" Sym))) 
      (set (cdr Tmp) (+ 1 (cadr Tmp)))    # Destructively modify
      (=: total (+ 1 (: total)))          # Update total probability
      (if (>= (: total) (: topValue))
         (rescale> This) ) ) )

(dm computeLower> (Sym)
   (let (Sum 0 Cnt 0)
      (while (< Cnt Sym)
         (setq Sum 
            (+ Sum  
              (cadr (lup (: counts) Cnt)))
            Cnt (+ 1 Cnt) ) ) 
      Sum ) )

# Encode the given symbol
(dm emit> (Sym) 
   (let (Low (computeLower> This Sym)
      High (+ Low (cadr (lup (: counts) Sym))) )
      (encode> (: coder) Low High (: total)) ) )  

# Encode the eof symbol    
(dm emitEof> (Sym)
   (let (Low (computeLower> This (: eofSym))
      High (+ Low (cadr (lup (: counts) (: eofSym)))) ) 
      (encode> (: coder) Low High (: total)) ) )

# Get the next symbol and decode    
(dm decode> ()
   (let (
      Target (decodeTarget> (: coder) (: total))
      Sym 0
      Low 0      
      High (cadr (lup (: counts) Sym)) )

      (if (> Target (: total))
         (throw (msg (text "!! Model/decode: bad Target; @1" Target))) )

      (while (>= Target High)
         (setq 
            Sym (+ 1 Sym)
            High (+ High (cadr (lup (: counts) Sym))) ) ) 
         
      (if (= Sym (: eofSym))
         (eof> (: coder) T) )
      
      (setq Low (- High (cadr (lup (: counts) Sym))))   
      
      (decode> (: coder) Low High (: total))

      Sym ) )


# =========================================================
# Simple list based drivers
# TODO: Stream based drivers
      
# ---------------------------------------------------------
# Arithmetic Coding Compress

(de ACDC_Compress (Msg)
   (let (
      Encoder (new '(+ACDC_Encoder))
      Model (new '(+ACDC_BasicModel) Encoder) )
      
      (mapc '((C) 
         (emit> Model C) 
         (update> Model C))  Msg )
         
      (emitEof> Model)

      (reverse (finalize> Encoder)) ) )

# ---------------------------------------------------------
# Arithmetic Coding Decompress
   
(de ACDC_Decompress (Buffer)
   (let (Decoder (new '(+ACDC_Decoder) Buffer)
      Model (new '(+ACDC_BasicModel) Decoder) 
      Msg NIL)
     
      (startDecode> Decoder) 
      (loop
         (setq C (decode> Model))
         (T (eof> Decoder))
         (update> Model C)
         (setq Msg (cons C Msg)) )
         
      Msg) )

# ---------------------------------------------------------
NIL

# Test it out
: (load "acdc.l")

: (setq Msg (mapcar char (chop "she sells sea shells by the sea shore")))
-> (115 104 101 32 115 101 108 108 115 32 115 101 97 32 115 104 101 108 108 115 32 98 121 32 116 104 101 32 115 101 97 32 115 104 111 114 101)

: (pack (mapcar format (ACDC_Compress Msg)))
-> "0111001011110100101000000010101001011001000001101011101011011100010110000111101101101110001100111011111100110110000110011101010011110100100011011011001011110001100000011010100101101010111100101001111000011010011010111001011011100100110001100111110110110110010"

: (/ (length (ACDC_Compress Msg)) 8)
-> 32
: (length Msg)
-> 37

: (pack (reverse (mapcar char (ACDC_Decompress (ACDC_Compress Msg)))))                                                       
-> "she sells sea shells by the sea shore"

# Compress 64K of 'A's
: (prog (setq Msg (need (** 2 16) (char "A"))) (length Msg))
-> 65536
: (/ (length (ACDC_Compress Msg)) 8)
-> 303
: (= Msg (reverse (ACDC_Decompress (ACDC_Compress Msg))))
-> T




      
