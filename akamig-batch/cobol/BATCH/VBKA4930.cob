./ ADD NAME=VBKA4930  01.12 20030711 20031029 135616 00296 00180 MCSK049
       ID  DIVISION.
       PROGRAM-ID.     'VBKA4930'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K A                           *
      *              <> PG.OUTLINE     : YUUSOUSAKI JYOUHOU SETTEI     *
      *                                                                *
      *                 * INPUT FILE   : YUUSOUSAKI JYOUHOU DATA (DWKE)*
      *                 * INPUT FILE   : SEIKYU JYOUHOU DATA     (DWKB)*
      *                 * OUTPUT FILE  : SEIKYU JYOUHOU DATA     (DWKA)*
      *                                                                *
      *              <> AUTHOR         : CSK-C.YAMAGATA                *
      *              <> DATE           : 03/07/11                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-568  A-2199      ÍÝº³   : 03/09/29  CSK-Y.MORIO         *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKEFILE ASSIGN    TO S-DWKE.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
       DATA DIVISION.
       FILE SECTION.
       FD  WKEFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKE-REC.
           02 FILLER                        PIC  X(80).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(80).
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(80).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  WKE-R.
           02 WKE-SKYCD                     PIC  X(07).
           02 WKE-SNSCD                     PIC  X(02).
           02 WKE-SYUCD                     PIC  X(07).
           02 WKE-SYNCD                     PIC  X(02).
           02 WKE-SYHHK                     PIC  X(01).
           02 WKE-SYUSO.
              03 WKE-SIMBI                  PIC  X(02).
              03 WKE-KAINO                  PIC  X(02).
              03 WKE-BUNO                   PIC  X(03).
              03 WKE-SOKNO                  PIC  X(03).
              03 WKE-CKUKG                  PIC  X(05).
              03 WKE-STRIYU.
                 04 WKE-TRIYU               OCCURS 15
                                            PIC  X(01).
              03 WKE-STKBN                  PIC  X(01).
              03 WKE-YUBIN                  PIC  X(07).
           02 WKE-SKSIM                     PIC  9(08).
           02 WKE-ICYOK                     PIC  X(01).
           02 WKE-URKZD                     PIC S9(12)  COMP-3.
           02 FILLER                        PIC  X(07).
       01  WKB-R.
           02 WKB-SKYCD                     PIC  X(07).
           02 WKB-SNSCD                     PIC  X(02).
           02 WKB-SYUCD                     PIC  X(07).
           02 WKB-SYNCD                     PIC  X(02).
           02 WKB-SYHHK                     PIC  X(01).
           02 WKB-SYUSO.
              03 WKB-SIMBI                  PIC  X(02).
              03 WKB-KAINO                  PIC  X(02).
              03 WKB-BUNO                   PIC  X(03).
              03 WKB-SOKNO                  PIC  X(03).
              03 WKB-CKUKG                  PIC  X(05).
              03 WKB-STRIYU.
                 04 WKB-TRIYU               OCCURS 15
                                            PIC  X(01).
              03 WKB-STKBN                  PIC  X(01).
              03 WKB-YUBIN                  PIC  X(07).
           02 WKB-SKSIM                     PIC  9(08).
           02 WKB-ICYOK                     PIC  X(01).
           02 WKB-URKZD                     PIC S9(12)  COMP-3.
           02 FILLER                        PIC  X(07).
       01  WKA-R.
           02 WKA-SKYCD                     PIC  X(07).
           02 WKA-SNSCD                     PIC  X(02).
           02 WKA-SYUCD                     PIC  X(07).
           02 WKA-SYNCD                     PIC  X(02).
           02 WKA-SYHHK                     PIC  X(01).
           02 WKA-SYUSO.
              03 WKA-SIMBI                  PIC  X(02).
              03 WKA-KAINO                  PIC  X(02).
              03 WKA-BUNO                   PIC  X(03).
              03 WKA-SOKNO                  PIC  X(03).
              03 WKA-CKUKG                  PIC  X(05).
              03 WKA-STRIYU.
                 04 WKA-TRIYU               OCCURS 15
                                            PIC  X(01).
              03 WKA-STKBN                  PIC  X(01).
              03 WKA-YUBIN                  PIC  X(07).
           02 WKA-SKSIM                     PIC  9(08).
           02 WKA-ICYOK                     PIC  X(01).
           02 WKA-URKZD                     PIC S9(12)  COMP-3.
           02 FILLER                        PIC  X(07).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-SYUCD              PIC  X(07).
                 04 WIN1-SYNCD              PIC  X(02).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-SYUCD              PIC  X(07).
                 04 WIN2-SYNCD              PIC  X(02).
       01  SAVE-AREA.
           02 WSV2-KEY.
              03 WSV2-KEY1.
                 04 WSV2-SYUCD              PIC  X(07).
                 04 WSV2-SYNCD              PIC  X(02).
       01  INDEX-AREA.
           02 I                             PIC  9(08)  COMP.
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL (WIN1-KEY = HIGH-VALUE AND
                          WIN2-KEY = HIGH-VALUE).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKEFILE
                       WKBFILE
                OUTPUT WKAFILE.
           PERFORM M11-READ-WKE THRU M11-EXIT.
           PERFORM M12-READ-WKB THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKEFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-WKE.
           READ WKEFILE INTO WKE-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M11-EXIT.
           MOVE WKE-SYUCD TO WIN1-SYUCD.
           MOVE WKE-SYNCD TO WIN1-SYNCD.
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
           MOVE WKB-SYUCD TO WIN2-SYUCD.
           MOVE WKB-SYNCD TO WIN2-SYNCD.
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
                 PERFORM M22-WKEONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKBONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN2-KEY TO WSV2-KEY.
           PERFORM M211-WRITE-WKA THRU M211-EXIT
             UNTIL WIN2-KEY NOT = WSV2-KEY.
           PERFORM M11-READ-WKE THRU M11-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : WRITE WKAFILE                                      *
      *----------------------------------------------------------------*
       M211-WRITE-WKA.
           MOVE SPACE        TO WKA-R.
           INITIALIZE           WKA-R.
           MOVE WKB-SKYCD    TO WKA-SKYCD.
           MOVE WKB-SNSCD    TO WKA-SNSCD.
           MOVE WKB-SYUCD    TO WKA-SYUCD.
           MOVE WKB-SYNCD    TO WKA-SYNCD.
           MOVE '1'          TO WKA-SYHHK.
           MOVE WKE-SIMBI    TO WKA-SIMBI.
           MOVE WKE-KAINO    TO WKA-KAINO.
           MOVE WKE-BUNO     TO WKA-BUNO.
           MOVE WKE-SOKNO    TO WKA-SOKNO.
           MOVE WKE-CKUKG    TO WKA-CKUKG.
           MOVE WKE-STRIYU   TO WKA-STRIYU.
           IF WKB-ICYOK  NOT =  SPACE
              MOVE  '*'      TO WKA-TRIYU(7).
           MOVE '0'          TO WKA-STKBN.
           PERFORM M2111-SET-STKBN THRU M2111-EXIT
                VARYING I FROM 1 BY 1
                UNTIL WKA-STKBN = '1'
                OR    I   >  15.
           MOVE WKE-YUBIN    TO WKA-YUBIN.
           MOVE WKB-SKSIM    TO WKA-SKSIM.
           MOVE WKB-ICYOK    TO WKA-ICYOK.
           MOVE WKB-URKZD    TO WKA-URKZD.
           WRITE WKA-REC FROM WKA-R.
           PERFORM M12-READ-WKB THRU M12-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1.1 :SET STKBN                                         *
      *----------------------------------------------------------------*
       M2111-SET-STKBN.
           IF WKA-TRIYU(I)   = '*'
              MOVE '1' TO WKA-STKBN.
       M2111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKEFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKEONL.
           MOVE SPACE        TO WKA-R.
           INITIALIZE           WKA-R.
           MOVE WKE-SYUCD    TO WKA-SKYCD.
           MOVE WKE-SYUCD    TO WKA-SYUCD.
           MOVE WKE-SYNCD    TO WKA-SYNCD.
           MOVE '1'          TO WKA-SYHHK.
           MOVE WKE-SIMBI    TO WKA-SIMBI.
           MOVE WKE-KAINO    TO WKA-KAINO.
           MOVE WKE-BUNO     TO WKA-BUNO.
           MOVE WKE-SOKNO    TO WKA-SOKNO.
           MOVE WKE-CKUKG    TO WKA-CKUKG.
           MOVE WKE-STRIYU   TO WKA-STRIYU.
           MOVE SPACE        TO WKA-TRIYU(7).
           MOVE '0'          TO WKA-STKBN.
           PERFORM M2111-SET-STKBN THRU M2111-EXIT
                VARYING I FROM 1 BY 1
                UNTIL WKA-STKBN = '1'
                OR    I   >  15.
           MOVE WKE-YUBIN    TO WKA-YUBIN.
           MOVE WKE-SKSIM    TO WKA-SKSIM.
           WRITE WKA-REC FROM WKA-R.
           PERFORM M11-READ-WKE THRU M11-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKBFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKBONL.
           MOVE WIN2-KEY TO WSV2-KEY.
           PERFORM M231-WRITE-WKA THRU M231-EXIT
             UNTIL WIN2-KEY NOT = WSV2-KEY.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1 : WRITE WKAFILE                                      *
      *----------------------------------------------------------------*
       M231-WRITE-WKA.
           MOVE SPACE        TO WKA-R.
           INITIALIZE           WKA-R.
           MOVE WKB-SKYCD    TO WKA-SKYCD.
           MOVE WKB-SNSCD    TO WKA-SNSCD.
           MOVE WKB-SYUCD    TO WKA-SYUCD.
           MOVE WKB-SYNCD    TO WKA-SYNCD.
           MOVE '0'          TO WKA-SYHHK.
           MOVE '0'          TO WKA-STKBN.
           MOVE WKB-SKSIM    TO WKA-SKSIM.
           MOVE WKB-ICYOK    TO WKA-ICYOK.
           MOVE WKB-URKZD    TO WKA-URKZD.
           WRITE WKA-REC FROM WKA-R.
           PERFORM M12-READ-WKB THRU M12-EXIT.
       M231-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKEFILE
                 WKBFILE
                 WKAFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKA4930 END                                           *
      *----------------------------------------------------------------*
