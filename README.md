# picolisp-acdc
## Arithmetic Coding: Basic adaptive model implementation

Implementation is based on the algorithms and code described in

> "Arithmetic Coding for Data Compression" Witten, Neal, Cleary (Communications of the ACM 1987)

and

> "Practical Impementations of Arithmetic Coding" Howard, Vitter (Brown University, Dept of Computer Science 1992)

Both papers can be found online 

</hr>

It is not fast by any measure... but it works.

~~~~
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
~~~~
