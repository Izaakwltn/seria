# Seria
## A Module for Exploring Twelve-Tone Serialism in Common Lisp

### Unless you have a tone row in mind, you can start using Seria with a randomly generated test row:

SERIA> (test-row)

#<ROW PRIME-10: 

(10 3 12 8 1 4 5 9 11 6 2 7)>

### Once you have a row, you can generate the matrix using:

SERIA> (build-matrix test-row)
#<TONE-MATRIX 
Prime:
(10 3 12 8 1 4 5 9 11 6 2 7)

Matrix:
              
(10 3 12 8 1 4 5 9 11 6 2 7)
              
(5 10 7 3 8 11 12 4 6 1 9 2)
              
(8 1 10 6 11 2 3 7 9 4 12 5)
              
(12 5 2 10 3 6 7 11 1 8 4 9)
              
(7 12 9 5 10 1 2 6 8 3 11 4)
              
(4 9 6 2 7 10 11 3 5 12 8 1)
              
(3 8 5 1 6 9 10 2 4 11 7 12)
              
(11 4 1 9 2 5 6 10 12 7 3 8)
              
(9 2 11 7 12 3 4 8 10 5 1 6)
              
(2 7 4 12 5 8 9 1 3 10 6 11)
              
(6 11 8 4 9 12 1 5 7 2 10 3)
              
(1 6 3 11 4 7 8 12 2 9 5 10)>

### Or generate a row using standard '(0 1 2 3 4 5 6 7 8 9 't e) notation:
  
SERIA> (standard-matrix test-row)
#<TONE-MATRIX 
Prime:
(10 3 12 8 1 4 5 9 11 6 2 7)

Matrix:
              
(9 2 E 7 0 3 4 8 T 5 1 6)
              
(4 9 6 2 7 T E 3 5 0 8 1)
              
(7 0 9 5 T 1 2 6 8 3 E 4)
              
(E 4 1 9 2 5 6 T 0 7 3 8)
              
(6 E 8 4 9 0 1 5 7 2 T 3)
              
(3 8 5 1 6 9 T 2 4 E 7 0)
              
(2 7 4 0 5 8 9 1 3 T 6 E)
              
(T 3 0 8 1 4 5 9 E 6 2 7)
              
(8 1 T 6 E 2 3 7 9 4 0 5)
              
(1 6 3 E 4 7 8 0 2 9 5 T)
              
(5 T 7 3 8 E 0 4 6 1 9 2)
              
(0 5 2 T 3 6 7 E 1 8 4 9)>

### And find prime/retrograde/inverse/retrograde-inverse row derivations using the following commands:
  
SERIA> (prime 5 test-row)
#<ROW PRIME-5: 

(5 10 7 3 8 11 12 4 6 1 9 2)>
 
SERIA> (retrograde 5 test-row)
#<ROW RETROGRADE-2: 

(2 9 1 6 4 12 11 8 3 7 10 5)>
  
SERIA> (inverse 5 test-row)
#<ROW INVERSE-5: 

(5 12 3 7 2 11 10 6 4 9 1 8)>
  
SERIA> (retrograde-inverse 5 test-row)
#<ROW RETROGRADE-INVERSE-8: 

(8 1 9 4 6 10 11 2 7 3 12 5)>
  
### Also, You can create a matrix using note names instead of numeric values:
SERIA> (note-matrix test-row)
#<TONE-MATRIX 
Prime:
(A D B G C D#/EB E G#/AB A#/BB F C#/DB F#/GB)

Matrix:
(A D B G C D#/EB E G#/AB A#/BB F C#/DB F#/GB)
              
(E A F#/GB D G A#/BB B D#/EB F C G#/AB C#/DB)
              
(G C A F A#/BB C#/DB D F#/GB G#/AB D#/EB B E)
              
(B E C#/DB A D F F#/GB A#/BB C G D#/EB G#/AB)
              
(F#/GB B G#/AB E A C C#/DB F G D A#/BB D#/EB)
              
(D#/EB G#/AB F C#/DB F#/GB A A#/BB D E B G C)
              
(D G E C F G#/AB A C#/DB D#/EB A#/BB F#/GB B)
              
(A#/BB D#/EB C G#/AB C#/DB E F A B F#/GB D G)
              
(G#/AB C#/DB A#/BB F#/GB B D D#/EB G A E C F)
              
(C#/DB F#/GB D#/EB B E G G#/AB C D A F A#/BB)
              
(F A#/BB G D#/EB G#/AB B C E F#/GB C#/DB A D)
              
(C F D A#/BB D#/EB F#/GB G B C#/DB G#/AB E A)
>
