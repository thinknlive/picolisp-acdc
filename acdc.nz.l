
# -----------------
(class +ACDC_Scale)
# bits top qtr1 half qtr2

(dm T (B)
   # B must of at least one binary order of magnitude
   # than the size of the block being encoded
   # in order to avoid precision errors
   (default B 40) 
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
            (throw (text "Encoder/encode: data error; Cnt,Low,High,Total: @1,@2,@3,@4 Top: @5" 
                           (: cnt) L H Total (: top))) )
     
      (setq
         High (+ Low (- (/ (* Range H) Total) 1))
         Low  (+ Low (+ (/ (* Range L) Total) 0)) )

      (if (> High Top) 
         (throw (text "Encoder/encode: high > top; @1 > @2" High Top) ) )

      (if (> Low High) 
         (throw (text "Encoder/encode: low > high; @1 > @2; Cnt,L,H,Total: @3,@4,@5,@6" 
                        Low High (: cnt) L H Total)  ) )

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

# ========================================================================
# Lazy Adaptive Model
# Use ESC and total symbol count for

(class +ACDC_NZModel)

(dm T (Coder SymN)
   (if (not Coder) 
      (throw "Arithmetic encoder required"))
  
   (default SymN '(0 255))

   (=: coder Coder)               # encoder|decoder instance
   (=: minSym (car SymN))
   (=: maxSym (cadr SymN))
   (=: escSym (inc (: maxSym) 2))
   (=: eofSym (inc (: maxSym) 3))
   (=: max (: eofSym))
   (=: topValue (; (; (: coder) scale) top))

   # Maintain cumulative frequencies in binary indexed tree (fenwick tree) 
   # Initial probability needs to be non-zero.. start at 1.
   (=: bitsMask (- (** 2 32) 1))    # Max nodes in tree

   (let (MM NIL FT NIL) 
      (idx 'FT (list (: escSym) 1) T)
      (idx (:: mmodel) (list (: minSym) FT 1) T) )

   #(show This)
)

(dm prUpdate> (FT N X)
   (let (Elt NIL Ndx N)
     (setq Elt (lup FT Ndx))
     (when Elt (inc (cdr Elt) X))
   )
   FT 
)

(dm prLookup> (FT N)
   #(msg (pack "#prLookup> " N))
   #(msg (idx 'FT))
   (let (Res 0)
      (for Elt (idx 'FT) 
         (T (> (car Elt) N))
         (inc 'Res (if Elt (cadr Elt) 0) ) 
      ) Res )

   #(let (Res 0 Elt NIL Ndx (: minSym))
   #   (loop 
   #      (NIL (<= Ndx N))
   #      (setq Elt (lup FT Ndx))
   #      (when Elt (inc 'Res (cadr Elt)))
   #      (inc 'Ndx)
   #   )
   #   Res
   #)
)

(dm rescale> ()
   # TODO: Rescale model; not necessary unless
   # very huge message or very low precision
   (throw "BasicModel/rescale: Insufficient precision") 
)

(dm addSym> (Sym)
   (let (FT NIL) 
      (idx 'FT (list (: escSym) 1) T)
      (idx (:: mmodel) (list Sym FT 1) T) 
      Sym   
   )
)

# Update the model for a symbols occurrence  
(dm update> (CurSym PrvSym)
   #(msg "#update> " CurSym " " PrvSym)
   
   # Update the occurrence count for a given symbol and
   # propagate the change in cumulatives through
   # subsequent symbols, in order, as well

   (default PrvSym (: minSym))
   (let (X NIL Y NIL FT NIL Total NIL)
     
      (setq X (lup (: mmodel) PrvSym))
      (when (not X)
         # Just occurred, add symbol
         (addSym> This PrvSym)
         (setq X (lup (: mmodel) PrvSym))
      )
      (ifn X (throw (text "#update> ; data error (PrvSym); PrvSym @1" PrvSym)))
     
      (setq 
         FT (nth X 2)
         Total (nth X 3))

      (setq Y (lup (car FT) CurSym))
      (ifn Y 
         (prog
            # Add a new symbol. Should only happen on decoding
            (if (> CurSym (: max)) (throw (text "#update> ; data error (out-of-range); New Sym @1" CurSym)))
            #(msg "#emit> (+new) PrvSym: " PrvSym "; CurSym: " CurSym "; Max: " (: max))
            (idx FT (list CurSym 0) T)
            #(msg (idx FT))
         ) )

      (prUpdate> This (car FT) CurSym 1)
      (inc Total)

      (if (>= (prLookup> This (car FT) (: max)) (: topValue))
         (rescale> This) ) ) 
)
 
# Encode the given symbol
(dm emit> (CurSym PrvSym)
   #(msg "#emit> " CurSym " " PrvSym)
   (default PrvSym (: minSym))
   (let (High NIL Low NIL Total NIL
         X NIL Y NIL FT NIL)
      
      (setq X (lup (: mmodel) PrvSym))
      (when (not X)
         # Just occurred, add symbol
         (addSym> This PrvSym)
         (setq X (lup (: mmodel) PrvSym))
      )
      (ifn X (throw (text "#emit> ; data error (prvsym); PrvSym @1" PrvSym)))

      # Sym FT Total
      (setq FT (nth X 2))

      (setq Y (lup (car FT) CurSym))
      (ifn Y 
         (prog 
            # Add a new symbol
            (if (> CurSym (: max)) (throw (text "#emit> ; data error (out-of-range); New Sym @1" CurSym)))

            # Encode escape symbol
            #(msg "#emit> (new) " CurSym " --> " CurSym "; Max " (: max))
            (setq 
               Low (if (<= (: escSym) (: minSym)) 0 (prLookup> This (car FT) (dec (: escSym))))
               High (prLookup> This (car FT) (: escSym))
               Total (prLookup> This (car FT) (: max)) )
            
            #(msg (text "Model/emit (esc) Sym: @1/@2 Total: @3 Range: @4,@5" CurSym PrvSym Total Low High))
            
            (encode> (: coder) Low High Total)
            (encode> (: coder) CurSym (inc CurSym) (: max))
            
            #(msg "#emit> (+new) CurSym/PrvSym: " CurSym "/" PrvSym "; Max: " (: max))
            (idx FT (list CurSym 0) T)

            #(msg (idx FT))
        )
        (prog 
            #(msg (idx FT))
            
            #(msg "#emit> " CurSym)
            (setq 
               Low (if (<= CurSym (: minSym)) 0 (prLookup> This (car FT) (dec CurSym)))
               High (prLookup> This (car FT) CurSym)
               Total (prLookup> This (car FT) (: max)) )

            #(msg (text "Model/emit Sym: @1/@2 Total: @3 Range: @4,@5" 
            #           CurSym PrvSym Total Low High))

            (encode> (: coder) Low High Total) 
        ) ) )
)  


# Get the next symbol and decode    
(dm decode> (PrvSym)
   (default PrvSym (: minSym))
   #(msg "#PrvSym> " PrvSym)
   (let (High NIL Low NIL Total NIL
         X NIL FT NIL Sym NIL Target)
      
      (setq X (lup (: mmodel) PrvSym))
      (when (not X)
         # Just occurred, add symbol
         (addSym> This PrvSym)
         (setq X (lup (: mmodel) PrvSym))
      )
      (ifn X (throw (text "#decode> ; data error (prvsym); PrvSym @1" PrvSym)))

      # Sym FT Total
      (setq 
         FT (nth X 2)
         Total (car (nth X 3)) #(prLookup> This (car FT) (: max)) 
         Target (decodeTarget> (: coder) Total ) )

      (if (> Target Total)
         (throw (msg (text "#decoder> data error (model/decode): bad Target; @1" Target))) )

      # Given the target we determine the range into which it falls.
      (setq Low 0 High 0)
      (for Elt (idx FT)
         (setq 
            Sym (car Elt) 
            High (+ High (cadr Elt)) )
         (T (< Target High))
         (setq Low High) )

      # Update the decoder
      #(msg "#decode> Sym: " Sym "; Low: " Low "; High: " High "; Total: " Total)
      (decode> (: coder) Low High Total)

      # If 'escape' symbol...
      (if (= Sym (: escSym))
         (prog
            (setq Target (decodeTarget> (: coder) (: max)))  
            (if (> Target (: max))
               (throw (msg (text "#decoder> data error (model/decode): (esc) bad Target; @1" Target))) )

            (setq 
               Low Target  
               High (inc Target) 
               Total (: max) )

            (decode> (: coder) Low High Total) 
         
            (setq Sym Target)
            #(msg "#decode> (esc sym) Sym: " Sym "; Low: " Low "; High: " High "; Total: " Total)
            #(msg (idx FT))
         ) )

      #(msg (text "Model/decode PrvSym: @1 Sym: @2 Total: @3 Range: @4,@5 Target: @6" 
      #              PrvSym Sym Total Low High Target))

      Sym )
)

# ---------------------------------------------------------
# Arithmetic Coding Compress

(de ACDC_NZCompress (Msg SymN)
   # NOTE! There is no eof encoded here. Keep track of length separately
   (setq *ACDC_Count 0)
   #(msg "##NZCompress: SymN: " SymN)
   (let 
      (Cnt 0
         Sym NIL
         PrvSym NIL
         Buffer NIL
         Encoder (new '(+ACDC_Encoder))
         Model (new '(+ACDC_NZModel) Encoder SymN) )
   
      # Hackity hack
      # See (outputBits> ) of '+ACDC_Encoder for 'link that feeds this 'make 
      #(msg "##NZCompress: START" )
      (setq Buffer
         (make 
            (for Sym Msg
               (inc '*ACDC_Count)
               #(msg "#NZCompress: Cnt " (inc 'Cnt) "; Sym: " Sym)
               (emit> Model Sym PrvSym)
               (update> Model Sym PrvSym)
               (setq PrvSym Sym) )
            (finalize> Encoder) ) )

      Buffer )
)


# ---------------------------------------------------------
# Arithmetic Coding Decompress
   
(de ACDC_NZDecompress (Buffer SymN MsgLen)
   (setq *ACDC_Count 0)
   (let (
      Decoder (new '(+ACDC_Decoder) Buffer)
      Model (new '(+ACDC_NZModel) Decoder SymN) 
      Msg NIL 
      Sym NIL
      PrvSym NIL 
      Cnt 0)

      (setq Msg
         (make
            (startDecode> Decoder)
            (for N MsgLen
               (setq Sym (decode> Model PrvSym))
               #(msg "#NZDecompress: Cnt " (inc 'Cnt) "; Sym: " Sym)
               (update> Model Sym PrvSym)
               (link Sym)
               (inc '*ACDC_Count)
               (setq PrvSym Sym) ) ) )

      Msg )
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




      
