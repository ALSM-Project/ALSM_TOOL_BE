./ ADD NAME=VBKF0820  01.00 19990305 19990305 141644 00188 00188 TSO251
       ID  DIVISION.
       PROGRAM-ID.     'VBKF0820'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : KAISYU DAIKOU MATCHING        *
      *                                                                *
      *                 * INPUT FILE   : TOKUISAKI SYUKEI DATA  (DWKA) *
      *                                : KAISYU DAIKOU SYUKEI DATA     *
      *                                                         (DWKB) *
      *                 * OUTPUT FILE  : TOKUISAKI SYUKEI DATA  (DWKC) *
      *                                                                *
      *              <> AUTHOR         : CSK-R.TAKAYANAGI              *
      *              <> DATE           : 99/03/05                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
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
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(050).
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
       01  WKC-R.
           02 WKC-SKTCD                     PIC  X(07).
           02 WKC-SKNCD                     PIC  X(02).
           02 WKC-SKDKCD                    PIC  X(04).
           02 WKC-TOKCD                     PIC  X(07).
           02 WKC-KSSBT                     PIC  X(02).
           02 WKC-KSJK                      PIC  X(01).
           02 WKC-KZHKB                     PIC  9(08).
           02 WKC-KFTKB                     PIC  X(01).
           02 WKC-KFKTF                     PIC  X(01).
           02 WKC-KYTGK                     PIC S9(11)  COMP-3.
           02 WKC-KDTKCD                    PIC  X(07).
           02 FILLER                        PIC  X(04).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-KDTKCD             PIC  X(07).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-KDTKCD             PIC  X(07).
       01  SAVE-AREA.
           02 WSV1-KEY.
              03 WSV1-KEY1.
                 04 WSV1-KDTKCD             PIC  X(07).
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
                       WKBFILE
                OUTPUT WKCFILE.
           PERFORM M11-READ-WKA THRU M11-EXIT.
           PERFORM M12-READ-WKB THRU M12-EXIT.
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
           MOVE WKA-KDTKCD  TO WIN1-KDTKCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ WKBFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-WKB.
           READ WKBFILE INTO WKB-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M12-EXIT.
           MOVE WKB-KDTKCD  TO WIN2-KDTKCD.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF WIN1-KEY1 = WIN2-KEY1
              PERFORM M21-MATCH THRU M21-EXIT
           ELSE
              IF WIN1-KEY1 < WIN2-KEY1
                 PERFORM M22-WKAONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKBONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE    WIN1-KEY1       TO   WSV1-KEY1.
           PERFORM UNTIL ( WIN1-KEY1  NOT =  WSV1-KEY1 )
             MOVE    WKA-R         TO   WKC-R
             MOVE    '1'           TO   WKC-KFKTF
             MOVE    WKB-KYTGK     TO   WKC-KYTGK
             MOVE    WKB-KDTKCD    TO   WKC-KDTKCD
             WRITE   WKC-REC       FROM WKC-R
             PERFORM M11-READ-WKA  THRU M11-EXIT
           END-PERFORM.
       M21-L01.
           PERFORM M12-READ-WKB    THRU M12-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKAFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKAONL.
           MOVE    WIN1-KEY1       TO   WSV1-KEY1.
           PERFORM UNTIL ( WIN1-KEY1  NOT =  WSV1-KEY1 )
             MOVE    WKA-R         TO   WKC-R
             WRITE   WKC-REC       FROM WKC-R
             PERFORM M11-READ-WKA  THRU M11-EXIT
           END-PERFORM.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKBFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKBONL.
           PERFORM M12-READ-WKB  THRU M12-EXIT.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKAFILE
                 WKBFILE
                 WKCFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKF0820 END                                           *
      *----------------------------------------------------------------*
