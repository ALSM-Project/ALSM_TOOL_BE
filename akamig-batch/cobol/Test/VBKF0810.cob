./ ADD NAME=VBKF0810  01.00 20001006 20001006 162444 00136 00136 TSO228
       ID  DIVISION.
       PROGRAM-ID.     'VBKF0810'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : KAISYU DAIKOU SYUKEI SYORI    *
      *                                                                *
      *                 * INPUT FILE   : TOKUISAKI SYUKEI DATA  (DWKA) *
      *                 * OUTPUT FILE  : KAISYU DAIKOU SYUKEI DATA     *
      *                                                         (DWKB) *
      *                                                                *
      *              <> AUTHOR         : CSK-R.TAKAYANAGI              *
      *              <> DATE           : 99/03/05                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-444              ÍÝº³   : 00/10/06  CSK-T.KINOSHITA     *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
       DATA DIVISION.
       FILE SECTION.
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(050).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(020).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  WKA-R.
           02 WKA-SKTCD                     PIC  X(07).
           02 WKA-SKNCD                     PIC  X(02).
           02 WKA-SKDKCD                    PIC  X(04).
           02 WKA-TOKCD                     PIC  X(07).
           02 WKA-KSSBT                     PIC  X(02).
           02 WKA-KSJK                      PIC  X(01).
           02 WKA-KZHKB                     PIC  9(08).
           02 WKA-KFTKB                     PIC  X(01).
           02 WKA-KFKTF                     PIC  X(01).
           02 WKA-KYTGK                     PIC S9(11)  COMP-3.
           02 WKA-KDTKCD                    PIC  X(07).
           02 FILLER                        PIC  X(04).
       01  WKB-R.
           02 WKB-KDTKCD                    PIC  X(07).
           02 WKB-KYTGK                     PIC S9(11)  COMP-3.
           02 FILLER                        PIC  X(07).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-KDTKCD             PIC  X(07).
       01  SAVE-AREA.
           02 WSV1-KEY.
              03 WSV1-KEY1.
                 04 WSV1-KDTKCD             PIC  X(07).
       01  TOTAL-AREA.
           02 WTT1-KYTGK                    PIC S9(11)  COMP-3.
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
       M0-CONTROL.
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL ( WIN1-KEY  =  HIGH-VALUE ).
           PERFORM M3-FINL THRU M3-EXIT.
       M0-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKAFILE
                OUTPUT WKBFILE.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKAFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-WKA.
           READ WKAFILE INTO WKA-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M11-EXIT.
           IF ( WKA-KFKTF  NOT =  '0'   )  OR
              ( WKA-KDTKCD     =  SPACE )
                GO TO M11-READ-WKA.
           MOVE WKA-KDTKCD     TO WIN1-KDTKCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           MOVE    SPACE       TO   WKB-R.
           INITIALIZE               WKB-R.
           MOVE    WKA-KDTKCD  TO   WKB-KDTKCD.
           MOVE    ZERO        TO   WTT1-KYTGK.
           MOVE    WIN1-KEY1   TO   WSV1-KEY1.
           PERFORM M21-SYUKEI  THRU M21-EXIT
                   UNTIL ( WIN1-KEY1  NOT =  WSV1-KEY1 ).
           MOVE    WTT1-KYTGK  TO   WKB-KYTGK.
       M2-L01.
           IF ( WTT1-KYTGK  >  9999999999 ) OR
              ( WTT1-KYTGK  <  ZERO       )
              WRITE WKB-REC    FROM WKB-R
           END-IF.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1: SYUKEI                                                *
      *----------------------------------------------------------------*
       M21-SYUKEI.
           COMPUTE WTT1-KYTGK =  WTT1-KYTGK  +  WKA-KYTGK.
       M21-L01.
           PERFORM M11-READ-WKA  THRU M11-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKAFILE
                 WKBFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKF0810 END                                           *
      *----------------------------------------------------------------*
