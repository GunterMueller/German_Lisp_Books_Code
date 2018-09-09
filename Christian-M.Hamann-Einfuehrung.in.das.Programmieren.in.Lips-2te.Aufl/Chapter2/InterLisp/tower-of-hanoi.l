(FILEHEADER MYFILE)
(PRINT '(EXIT))
(PRINT '(VERSION 0))
(PRINT 'MYFILEFNS)
(DEFINEQ
(FAKULTAET
  (LAMBDA (N)  ( COND  (( ZEROP N ) 1)
               ( T (TIMES N ( FAKULTAET (SUB1 N)))))))
)
;######################################
;
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
(MOVEDOSK-TOH
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

(RPAQQ MYFILEFNS (FACULTAET))
(RPAQQ MYFILECOMS (EXIT))
(RPAQ MYFILEGENNR 0)
STOP
