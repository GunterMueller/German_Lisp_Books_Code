(FILEHEADER TOW-OF-HAN)
(PRINT '1)
(PRINT '(VERSION 0))
(DEFINEQ
(COUNT-TOH
  (LAMBDA (N)
          (COND ((EQ N 1) 1)
                ( T (ADD1 ( TIMES 2 (COUNT-TOH ( SUB1 N))))))))
)


(DEFINEQ
(COUNTDOWN
  (LAMBDA (N) 
          (COND (( ZEROP N) NIL)
                ( T ( CONS N  ( COUNTDOWN  (SUB1 N )))))))
)


(DEFINEQ
(COUNTUP
  (LAMDA (N )
      (REVERSE ( COUNTDOWN  N))))
 
)



(DEFINEQ
(RESET-TOH
   (LAMBDA ( N)   
     (SETQ C NIL)
     (SETQ rBD NIL)
     (COND (( NULL N ) ((SETQ A  (COUNTUP 4 )))
           ( T ( SETQ A ((COUNTUP N ))))))))
)
       


(DEFINEQ
(TRANSFER-TOH
   (LAMBDA (FROM TO SPARE N )
           (COND ((ZEROP N ) (PRINT  (LIST FROM  (QUOTE EMPTY))))
                 ((EQ N 1 ) (MOVEDISK-TOH (FROM TO)))
                 (T ( TRANSFER-TOH FROM SPARE TO  (SUB1 N )
                    (MOVEDISK-TOH FROM TO )
                    (TRANSFER-TOH  SPARE TO FROM (SUB1 N )))))))
)

(DEFINEQ
(MOVEDISK-TOH
   (LAMBDA  ( FROM TO )
            (PRINT (LIST (QUOTE STEP)
                         (SETQ STEP (ADD1 STEP))
                         (QUOTE MOVE)
                         (CAR (EVAL FROM))
                         (QUOTE FROM)
                         FROM
                         (QUOTE TO)
                         TO))
            (COND (( NULL (EVAL FROM)) (PRINT (LIST FROM (QUOTE EMPTY)))
                   ( OR	( NULL (EVAL TO))
                        (LESSP (CAR (EVAL FROM )))
                               (CAR (EVAL TO)))
                   (SET TO (( CONS (CAR (EVAL  FROMi) ))
                                        (EVAL TO)))
                   (SET FROM  (CDR  (EVAL FROM )))
                  (T (PRINT (LIST (QUOTE CANT)
                                  (QUOTE MOVE)
                                  (CAR (EVAL FROM))
                                  (QUOTE ONTO)
                                  (CAR (EVAL TO)))))))))









)

(DEFINEQ
(TOWER-OF-HANOI
  (LAMBDA (STEP)
      (SETQ STEP 0 )
      (TRANSFER-TOH (QUOTE A)
                    (QUOTE C)
                    (QUOTE B)
                    (LENGTH A))
       T ))

)
(PRINT 'TOW-OF-HANFNS)
(RPAQQ TOW-OF-HANFNS (COUNT-TOH COUNTDOWN COUNTUP RESET-TOH TRANSFER-TOH MOVEDISK-TOH TOWER-OF_HANOI ))
(RPAQQ TOW-OF-HANCOMS 1)
(RPAQ TOW-OF-HANGENNR 0)

STOP
