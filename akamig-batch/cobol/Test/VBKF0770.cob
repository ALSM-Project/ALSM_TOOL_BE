./ ADD NAME=VBKF0770  01.00 20001006 20001006 162230 00244 00244 TSO228
       ID  DIVISION.
       PROGRAM-ID.     'VBKF0770'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : TOKUISAKI SYUKEI DATA SAKUSEI *
      *                                                                *
      *                 * INPUT FILE   : KAISYU YOTEI FILE DATA (DWKA) *
      *                                : TOKUISAKI JYOHO DATA   (DWKB) *
      *                 * OUTPUT FILE  : TOKUISAKI SYUKEI DATA  (DWKC) *
      *                                                                *
      *              <> AUTHOR         : CSK-R.TAKAYANAGI              *
      *              <> DATE           : 99/03/05                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-443              ÍÝº³   : 00/08/16  CSK-K.KITAJIMA      *
      * (02) K-444              ÍÝº³   : 00/10/06  CSK-T.KINOSHITA     *
      * (03) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
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
           02 FILLER                        PIC  X(110).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(040).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(050).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  WKA-R.
           COPY VTABKYF SUPPRESS.
       01  WKB-R.
           02 WKB-TOKCD                     PIC  X(07).
           02 WKB-NOSCD                     PIC  X(02).
           02 WKB-SKTCD                     PIC  X(07).
           02 WKB-KSSBT                     PIC  X(02).
           02 WKB-KSJK                      PIC  X(01).
           02 WKB-SKDKCD                    PIC  X(04).
           02 WKB-KZHKB                     PIC  9(08).
           02 WKB-KFTKB                     PIC  X(01).
           02 FILLER                        PIC  X(08).
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
                 04 WIN1-TOKCD              PIC  X(07).
                 04 WIN1-NOSCD              PIC  X(02).
                 04 WIN1-KSYBI              PIC  X(08).
                 04 WIN1-KSSBT              PIC  X(02).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-TOKCD              PIC  X(07).
                 04 WIN2-NOSCD              PIC  X(02).
                 04 WIN2-KZHKB              PIC  9(08).
                 04 WIN2-KSSBT              PIC  X(02).
       01  SAVE-AREA.
           02 WSV1-KEY.
              03 WSV1-KEY1.
                 04 WSV1-TOKCD              PIC  X(07).
                 04 WSV1-NOSCD              PIC  X(02).
                 04 WSV1-KSYBI              PIC  X(08).
                 04 WSV1-KSSBT              PIC  X(02).
       01  EDIT-AREA.
           02 WED1-KFKTF                    PIC  X(01).
       01  TOTAL-AREA.
           02 WTT1-KYTGK                    PIC S9(11)  COMP-3.
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
       M0-CONTROL.
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL ( WIN2-KEY  =  HIGH-VALUE ).
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
           MOVE TOKCD OF WKA-R     TO WIN1-TOKCD.
           MOVE NOSCD OF WKA-R     TO WIN1-NOSCD.
           MOVE KSYBI OF WKA-R     TO WIN1-KSYBI.
           MOVE KSSBT OF WKA-R     TO WIN1-KSSBT.
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
           MOVE WKB-TOKCD          TO WIN2-TOKCD.
           MOVE WKB-NOSCD          TO WIN2-NOSCD.
           MOVE WKB-KZHKB          TO WIN2-KZHKB.
           MOVE WKB-KSSBT          TO WIN2-KSSBT.
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
           PERFORM M211-EDIT-WKC   THRU M211-EXIT.
           MOVE    KDTKCD OF WKA-R TO   WKC-KDTKCD.
           MOVE    ZERO            TO   WED1-KFKTF.
           MOVE    ZERO            TO   WTT1-KYTGK.
           MOVE    WIN1-KEY1       TO   WSV1-KEY1.
           PERFORM M212-SYUKEI     THRU M212-EXIT
                   UNTIL ( WIN1-KEY1  NOT =  WSV1-KEY1 ).
       M21-L01.
           IF WED1-KFKTF  =  '0'
              MOVE '0'             TO   WKC-KFKTF
           ELSE
              MOVE '1'             TO   WKC-KFKTF
           END-IF.
           MOVE    WTT1-KYTGK      TO   WKC-KYTGK.
           WRITE   WKC-REC         FROM WKC-R.
       M21-L02.
           PERFORM M12-READ-WKB    THRU M12-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : EDIT WKCFILE                                       *
      *----------------------------------------------------------------*
       M211-EDIT-WKC.
           MOVE SPACE      TO WKC-R.
           INITIALIZE         WKC-R.
           MOVE WKB-SKTCD  TO WKC-SKTCD.
           MOVE WKB-NOSCD  TO WKC-SKNCD.
           MOVE WKB-SKDKCD TO WKC-SKDKCD.
           MOVE WKB-TOKCD  TO WKC-TOKCD.
           MOVE WKB-KSSBT  TO WKC-KSSBT.
           MOVE WKB-KSJK   TO WKC-KSJK.
           MOVE WKB-KZHKB  TO WKC-KZHKB.
           MOVE WKB-KFTKB  TO WKC-KFTKB.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2 : SYUKEI                                             *
      *----------------------------------------------------------------*
       M212-SYUKEI.
           IF ( WED1-KFKTF          =  '0' )  AND
              ( KFKTF OF WKA-R  NOT =  '0' )
              MOVE '1'           TO WED1-KFKTF
           END-IF.
           IF ( KYTGF OF WKA-R  =  '0' )
              COMPUTE WTT1-KYTGK =  WTT1-KYTGK  +  KYTGS OF WKA-R
           ELSE
              COMPUTE WTT1-KYTGK =  WTT1-KYTGK  +  KYTGT OF WKA-R
           END-IF.
       M212-L01.
           PERFORM M11-READ-WKA  THRU M11-EXIT.
       M212-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKAFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKAONL.
           MOVE    WIN1-KEY1     TO   WSV1-KEY1.
           PERFORM M11-READ-WKA  THRU M11-EXIT
                   UNTIL ( WIN1-KEY1  NOT =  WSV1-KEY1 ).
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKBFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKBONL.
           PERFORM M211-EDIT-WKC THRU M211-EXIT.
           MOVE  '0'       TO WKC-KFKTF.
           MOVE ZERO       TO WKC-KYTGK.
           MOVE SPACE      TO WKC-KDTKCD.
           WRITE WKC-REC FROM WKC-R.
       M23-L01.
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
      *         VBKF0770 END                                           *
      *----------------------------------------------------------------*
