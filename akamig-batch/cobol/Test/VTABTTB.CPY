      *      /*  TANMATU TABLE FILE                                */           
      *      /*  DATE  01/10/10  V-001                             */           
      *      /*  LENGTH = 240                                      */           
           02 KEY1.                                                             
              03 RECID                      PIC  X(03).                         
              03 RENNO                      PIC  9(04).                         
           02 TERM-J.                                                           
              03 TERMID                     PIC  X(04).                         
              03 PRTID                      PIC  X(04).                         
              03 TERMS                      PIC  9(01).                         
              03 ENDID                      PIC  9(01).                         
              03 JTRMID                     PIC  X(04).                         
              03 TRAN.                                                          
                 04 TRANS                   PIC  X(1)  OCCURS   60.             
              03 KNKNO                      PIC  X(03).                         
              03 FILLER                     PIC  X(156).                        
