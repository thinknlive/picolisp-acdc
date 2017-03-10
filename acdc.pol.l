# Uses a simple list to manage symbols in the Model
# -------------------------------------------------

# -----------------
(class +ACDC_Scale)
# bits top qtr1 half qtr2

(dm T (B)
   (default B 28)
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
   (=: buffer NIL)
   (=: cnt 0)
)
            
(dm outputBit> (Bit)
   # (=: buffer (cons Bit (: buffer)))
   (link Bit)
   (while (gt0 (: followBits))
      # (=: buffer (cons (x| 1 Bit) (: buffer)))
      (link (x| 1 Bit))
      (=: followBits (- (: followBits) 1)) )
)

(dm encode> (L H Total)
   (let 
      (Half (; (: scale) half)
      Qtr1 (; (: scale) qtr1)
      Qtr3 (; (: scale) qtr3)
      Top (: top)
      High (: high)
      Low (: low)
      Range (+ (- High Low) 1) )
     
      (inc (:: cnt)) 

      (if (or (> H Top) 
         (> L H) 
         (> Total Top))
            (throw (text "Encoder/encode: data error; Cnt,Low,High,Total: @1,@2,@3,@4 Top: @5" (: cnt) L H Total (: top))) )
     
      (setq
         High (+ Low (- (/ (* Range H) Total) 1))
         Low  (+ Low (+ (/ (* Range L) Total) 0)) )

      (if (> High Top) 
         (throw (text "Encoder/encode: high > top; @1 > @2" High Top) ) )

      (if (> Low High) 
         (throw (text "Encoder/encode: low > high; @1 > @2; Cnt,L,H,Total: @3,@4,@5,@6" Low High (: cnt) L H Total)  ) )

      (loop
         (NIL
            (cond
               ((< High Half)
                  (outputBit> This 0) 
                  T )
               ((>= Low Half)
                  (outputBit> This 1)
                  (dec 'Low Half)
                  (dec 'High Half)
                  T )
               ((and (>= Low Qtr1) (< High Qtr3))
                  (=: followBits (+ 1 (: followBits)))
                  (dec 'Low Qtr1)
                  (dec 'High Qtr1)
                  T )
               (T NIL) ) )
         
         (setq 
            Low (>> -1 Low)
            High (+ 1 (>> -1 High)) ) )

      (=: low Low)
      (=: high High)

) )


(dm finalize> ()
   (let 
      (Low (: low)
       Half (; (: scale) half) )

      (=: followBits (+ 1 (: followBits)))
      (if (< Low Half)
         (outputBit> This 1)
         (outputBit> This 0) ) )

      #(: buffer)
)

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
   (let 
      (Value (: value)
      High (: high)
      Low (: low)
      N (- (* Total (+ (- Value Low) 1)) 1)
      D (+ (- High Low) 1) )
      (/ N D) ) )

(dm getNextBit> ()
   (let (Buffer (: buffer)
      Bit (or (car Buffer) 0) )
      (=: buffer (cdr Buffer))
      Bit)
)
   
(dm startDecode> ()
   (=: eof NIL)
   (=: value 0)
   (let (Bits (; (: scale) bits)) 
      (do Bits
         (=: value (+ (>> -1 (: value)) (getNextBit> This))) ) ) 
)

(dm decode> (L H Total)
   (let ( 
      Top (: top) 
      Half (; (: scale) half)
      Qtr1 (; (: scale) qtr1)
      Qtr3 (; (: scale) qtr3) 
      Low (: low)
      High (: high)
      Value (: value)
      Range (+ (- High Low) 1) ) 
      
      (setq High (+ Low (- (/ (* Range H) Total) 1))
            Low  (+ Low (+ (/ (* Range L) Total) 0)) )
         
      (loop
         (NIL
            (cond
               ((< High Half) T)
               ((>= Low Half)
                  (dec 'Value Half)
                  (dec 'Low Half)
                  (dec 'High Half)
                  T )
               ((and (>= Low Qtr1) (< High Qtr3))
                  (dec 'Value Qtr1)
                  (dec 'Low Qtr1)
                  (dec 'High Qtr1)
                  T )
               (T NIL) ) )
            
            
         (setq
            Low (>> -1 Low)
            High (+ 1 (>> -1 High)) 
            Value (+ (>> -1 Value) (getNextBit> This)) )

         (if (or (< Value 0) (> Value Top))
            (throw (text "Decoder error: bad value: @1" Value)) ) )

      (=: low Low)
      (=: high High)
      (=: value Value)

) )   

   
# ----------------------------------------
# Simple Adaptive model for encoding bytes   
(class +ACDC_BasicModel)
# counts max total eofSym

(dm T (Coder)
   (if (not Coder) 
      (throw "Arithmetic encoder required"))
   (=: coder Coder)                 # encoder|decoder instance
   (=: max 258)                     # number of symbols (0..255 + 1 eof)
   (=: eofSym (+ (: max) -2))       # eof symbol
   (=: eotSym (+ (: max) -1))       # eof symbol
   (=: topValue (; (; (: coder) scale) top))

   # Maintain cumulative frequencies in list
   (=: counts (make (for X (: max) (link (list X (- X 1))))))
   (=: total (caar (nth (: counts) (inc (: eotSym)))))    # total probability
   #(show This)
)

(dm rescale> ()
   # TODO: Rescale model; not necessary unless
   # very huge message or very low precision
   #(throw "BasicModel/rescale: Insufficient precision") 
   (mapc '((V) (set V (>> 1 (+ (car V) 1)))) (: counts))
   # Ugly
   (=: total (+ (caar (: counts)) 
               (sum + (maplist 
                  '((L) (- (or (caadr L) (caar L)) (caar L))) (: counts)))))

)

# Update the model for a symbols occurrence  
(dm update> (Sym)
   (if (> Sym (: eotSym))
      (throw (text "Model/update: bad Sym: @1 >eotSym: @2" Sym (: eotSym))))
   
   (let (R (nth (: counts) (inc Sym)))
      (mapc '((V) (inc V)) R)
   )

   (inc (:: total))

   (if (>= (: total) (: topValue))
      (rescale> This) ) 
)
 
(dm computeLower> (Sym)
   (or (sum + (head Sym (: counts))) 0)
)

(dm computeRange> (Sym)
   (if (> Sym (: eofSym))
      (throw (msg (text "!! Model/computeRange: bad Sym; @1" Sym))) )

   (let
      (Low (or (caar (nth (: counts) Sym)) 0)
      High (or (caar (nth (: counts) (inc Sym))) (: total)))
   
      (list Low High))
)

# Encode the given symbol
(dm emit> (Sym)
   (let ( Range (computeRange> This Sym) )
      #(msg (text "Model/emit Sym: @1,@2 Total: @3 Range: @4,@5" (char Sym) Sym (: total) (car Range) (cadr Range)))
      (encode> (: coder) (car Range) (cadr Range) (: total)) ) 
)  

# Encode the eof symbol    
(dm emitEof> ()
   #(msg (text "Model/emitEof"))
   (emit> This (: eofSym))
) 

# Get the next symbol and decode    
(dm decode> ()
   (let (
      Target (decodeTarget> (: coder) (: total))
      Sym NIL Low NIL High NIL R NIL)

      (if (> Target (: total))
         (throw (msg (text "!! Model/decode: bad Target; @1" Target))) )

      (setq 
         R (seek '((V) (< Target (caadr V))) (: counts))
         Low (caar R)
         High (caadr R)
         Sym (cadadr R) )

      (if (= Sym (: eofSym))
         (eof> (: coder) T) )
     
      #(msg (text "Model/decode Sym: @1,@2 Total: @3 Range: @4,@5 Target: @6" (char Sym) Sym (: total) Low High Target))
      (decode> (: coder) Low High (: total))


      Sym ) 
)


# =========================================================
# Simple list based drivers
# TODO: Stream based drivers
      
# ---------------------------------------------------------
# Arithmetic Coding Compress

(de ACDC_Compress (Msg)
   (let 
      (Buffer NIL
      Encoder (new '(+ACDC_Encoder))
      Model (new '(+ACDC_BasicModel) Encoder) )
    
      (setq Buffer
         (make 
            (prog 
               (mapc '((C) 
                  (emit> Model C) 
                  (update> Model C))  Msg )
         
               (emitEof> Model)
               (finalize> Encoder) ) ) )
 
      Buffer )
)

# ---------------------------------------------------------
# Arithmetic Coding Decompress
   
(de ACDC_Decompress (Buffer)
   (let (Decoder (new '(+ACDC_Decoder) Buffer)
      Model (new '(+ACDC_BasicModel) Decoder) 
      Msg NIL 
      C NIL)

      (setq Msg
         (make
            (prog
               (startDecode> Decoder) 
               (loop
                  (setq C (decode> Model))
                  (T (eof> Decoder))
                  (update> Model C)
                  (link C)) ) ) )

      Msg)
)

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
o
: (pack (mapcar char (ACDC_Decompress (ACDC_Compress Msg))))   
-> "she sells sea shells by the sea shore"

# Compress 64K of 'A's
: (prog (setq Msg (need (** 2 16) (char "A"))) (length Msg))
-> 65536
: (/ (length (ACDC_Compress Msg)) 8)
-> 303
: (= Msg (ACDC_Decompress (ACDC_Compress Msg)))
-> T

# Compress 64K of random A-Z
: (prog (setq Msg (make (do (** 2 16) (link (rand (char "A") (char "Z")))))) (length Msg))
-> 65536
: (bench (/ (length (ACDC_Compress Msg)) 8))
-> 38786
: (bench (= Msg (ACDC_Decompress (ACDC_Compress Msg))))
-> T




      
