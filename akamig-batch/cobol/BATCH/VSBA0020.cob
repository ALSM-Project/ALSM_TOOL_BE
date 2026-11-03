./ ADD NAME=VSBA0020  01.01 19980305 19980305 100754 00047 00047 TSO220
       ID  DIVISION.
       PROGRAM-ID.     'VSBA0020'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SUZUKEN 10 SYSTEM             *
      *              <> SUBSYSTEM      : SUBROUTINE                    *
      *              <> PG.OUTLINE     : SEIREKI(6) --> WAREKI(6)      *
      *                                   HENKAN                       *
      *              *  I/O.PARM       : P02-DATE                      *
      *                                                                *
      *              <> AUTHOR         : CSK-F.M                       *
      *              <> DATE           : 82/05/27                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) SONOTA    296      ÍÝº³   : 88/11/22  CSK-N.HOHMURA       *
      * (02) K-275              ÍÝº³   : 98/03/05  CSK-K.ITO           *
      * (03) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  PA9-PARM.
           COPY VPARBA9 SUPPRESS.
       01  PC5-PARM.
           COPY VPARBC5 SUPPRESS.
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *    LINKAGE SECTION                                             *
      *----------------------------------------------------------------*
       01  P02-PARM.
           COPY VPARB02 SUPPRESS.
       PROCEDURE DIVISION USING P02-PARM.
      *----------------------------------------------------------------*
      *    M1 : MAIN CONTROL                                           *
      *----------------------------------------------------------------*
       M1-MAIN-CONTROL.
           MOVE P02-DATE      TO PA9-YMDS6.
           CALL 'VSBA1090' USING PA9-PARM.
           MOVE PA9-YMDS8     TO PC5-YMDS8.
           CALL 'VSBA1250' USING PC5-PARM.
           MOVE PC5-YMDW6     TO P02-DATE.
       M1-EXIT.
           GOBACK.
      *----------------------------------------------------------------*
      *         VSBA0020 END                                           *
      *----------------------------------------------------------------*
