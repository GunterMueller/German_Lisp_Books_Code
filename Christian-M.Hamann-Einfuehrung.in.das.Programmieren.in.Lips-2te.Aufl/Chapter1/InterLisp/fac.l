(FILEHEADER MYFILE)
(PRINT '(EXIT))
(PRINT '(VERSION 0))
(PRINT 'MYFILEFNS)
(DEFINEQ
(FAKULTAET
  (LAMBDA (N)  ( COND  (( ZEROP N ) 1)
               ( T (TIMES N ( FAKULTAET (SUB1 N)))))))
)
(RPAQQ MYFILEFNS (FACULTAET))
(RPAQQ MYFILECOMS (EXIT))
(RPAQ MYFILEGENNR 0)
STOP
