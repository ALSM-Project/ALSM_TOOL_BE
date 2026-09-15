./ ADD NAME=VBKA4990  01.22 20030819 20031028 200719 00406 00309 MCSK065
       ID  DIVISION.
       PROGRAM-ID.     'VBKA4990'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K.A                           *
      *              <> PG.OUTLINE     : SEIKYUUSYO DATA (KAGAMIBUN)   *
      *                                  KAKUTEI                       *
      *                                                                *
      *                 * INPUT FILE   : SEIKYUUSYO DATA               *
      *                                : (SEIKYUU KAGAMI)     (DWKA)   *
      *                 * INPUT FILE   : SEIKYUU JYOUHOU DATA          *
      *                                  (TORITOME HANEI)     (DWKB)   *
      *                 * OUTPUT FILE  : SEIKYU GOUKEIHYOU             *
      *                                  SAKUSEIYOU DATA      (DWKC)   *
      *                 * OUTPUT FILE  : SEIKYUUSYO DATA               *
      *                                  (SEIKYUU KAGAMI)     (DWKD)   *
      *                 * OUTPUT FILE  : TORITOME JYOUHOU     (DWKE)   *
      *                                                                *
      *              <> AUTHOR         : CSK-C.YAMAGATA                *
      *              <> DATE           : 03/08/19                      *
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
           SELECT WKDFILE ASSIGN    TO S-DWKD.
           SELECT WKEFILE ASSIGN    TO S-DWKE.
       DATA DIVISION.
       FILE SECTION.
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(300).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(100).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(30).
       FD  WKDFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKD-REC.
           02 FILLER                        PIC  X(300).
       FD  WKEFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKE-REC.
           02 FILLER                        PIC  X(90).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  WKA-R.
           02 WKA-CTLKEY.
              03 WKA-KAINO                  PIC  X(02).
              03 WKA-YTKBN                  PIC  X(01).
              03 WKA-YSYBN.
                 04 WKA-YSYB1               PIC  X(03).
                 04 WKA-YSYB2               PIC  X(04).
              03 WKA-YSBNO                  PIC  X(03).
              03 WKA-YSSTN                  PIC  X(03).
              03 WKA-YSCKG                  PIC  X(05).
              03 WKA-YUSCD                  PIC  X(07).
              03 WKA-YUNCD                  PIC  X(02).
              03 WKA-YSALL                  PIC  9(04).
              03 WKA-INFLG                  PIC  X(01).
              03 WKA-YSTPG                  PIC  9(04).
              03 WKA-PAGEN.
                 04 WKA-PAGE1               PIC  9(04).
                 04 WKA-PAGE2               PIC  9(04).
              03 WKA-SESCD                  PIC  X(07).
              03 WKA-SQNCD                  PIC  X(02).
           02 WKA-LPDAT                     PIC  X(219).
           02 WKA-DATA20  REDEFINES  WKA-LPDAT.
              03 WKA-GNO20                  PIC  9(02).
              03 WKA-RKB20                  PIC  X(02).
              03 WKA-MESAI2.
                 04 WKA-TMSJU               PIC  X(40).
                 04 WKA-TMSJL               PIC  X(40).
                 04 WKA-NOSMJ               PIC  X(40).
                 04 WKA-SQBNO               PIC  X(03).
                 04 WKA-SQSTN               PIC  X(03).
                 04 WKA-SQCKK               PIC  X(05).
                 04 WKA-SMEYMD.
                    05 WKA-SIMYY            PIC  9(04).
                    05 WKA-SIMMM            PIC  9(02).
                    05 WKA-SIMDD            PIC  9(02).
                 04 WKA-INKAN               PIC  X(01).
                 04 FILLER                  PIC  X(75).
           02 WKA-PGSUU                     PIC  X(02).
           02 WKA-SORTJ                     PIC  X(20).
           02 FILEER                        PIC  X(03).
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
              03 WKB-YSTRYU.
                 04 WKB-YTRYU               OCCURS 15
                                            PIC  X(01).
              03 WKB-YSTKB                  PIC  X(01).
              03 WKB-YUBIN                  PIC  X(07).
           02 WKB-SEIKU.
              03 WKB-SSTRYU.
                 04 WKB-STRYU               OCCURS 15
                                            PIC  X(01).
              03 WKB-SSTKB                  PIC  X(01).
           02 WKB-SKSIM                     PIC  X(08).
           02 FILLER                        PIC  X(19).
       01  WKC-R.
           02 WKC-SKYCD                     PIC  X(07).
           02 WKC-SNSCD                     PIC  X(02).
           02 WKC-YUSCD                     PIC  X(07).
           02 WKC-YUNCD                     PIC  X(02).
           02 WKC-SYHHK                     PIC  X(01).
           02 FILLER                        PIC  X(11).
       01  WKD-R.
           02 WKD-CTLKEY.
              03 WKD-KAINO                  PIC  X(02).
              03 WKD-YTKBN                  PIC  X(01).
              03 WKD-YSYBN.
                 04 WKD-YSYB1               PIC  X(03).
                 04 WKD-YSYB2               PIC  X(04).
              03 WKD-YSBNO                  PIC  X(03).
              03 WKD-YSSTN                  PIC  X(03).
              03 WKD-YSCKG                  PIC  X(05).
              03 WKD-YUSCD                  PIC  X(07).
              03 WKD-YUNCD                  PIC  X(02).
              03 WKD-YSALL                  PIC  9(04).
              03 WKD-INFLG                  PIC  X(01).
              03 WKD-YSTPG                  PIC  9(04).
              03 WKD-PAGEN.
                 04 WKD-PAGE1               PIC  9(04).
                 04 WKD-PAGE2               PIC  9(04).
              03 WKD-SESCD                  PIC  X(07).
              03 WKD-SQNCD                  PIC  X(02).
           02 WKD-LPDAT                     PIC  X(219).
           02 WKD-PGSUU                     PIC  X(02).
           02 WKD-SORTJ                     PIC  X(20).
           02 FILEER                        PIC  X(03).
       01  WKE-R.
           02 WKE-KAINO                     PIC  X(02).
           02 WKE-YSBNO                     PIC  X(03).
           02 WKE-YSSKNO                    PIC  X(03).
           02 WKE-YSCKG                     PIC  X(05).
           02 WKE-YUCOD                     PIC  X(07).
           02 WKE-YSNCD                     PIC  X(02).
           02 WKE-YSTRYU.
              03 WKE-YTRYU                  OCCURS 15
                                            PIC  X(01).
           02 WKE-SKBNO                     PIC  X(03).
           02 WKE-SKSKNO                    PIC  X(03).
           02 WKE-SKCKG                     PIC  X(05).
           02 WKE-SESCD                     PIC  X(07).
           02 WKE-SQNCD                     PIC  X(02).
           02 WKE-SSTRYU.
              03 WKE-STRYU                  OCCURS 15
                                            PIC  X(01).
           02 WKE-SKSIM                     PIC  X(08).
           02 WKE-SYHHK                     PIC  X(01).
           02 WKE-ICHHK                     PIC  X(01).
           02 FILEER                        PIC  X(08).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-SKYCD              PIC  X(07).
                 04 WIN1-SNSCD              PIC  X(02).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-SESCD              PIC  X(07).
                 04 WIN2-SQNCD              PIC  X(02).
       01  SAVE-AREA.
           02 WSV2-KEY.
              03 WSV2-KEY1.
                 04 WSV2-SESCD              PIC  X(07).
                 04 WSV2-SQNCD              PIC  X(02).
       01  FLAG-AREA.
           02 WFL1-RKB20                    PIC  X(01).
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
           OPEN INPUT  WKAFILE
                       WKBFILE
                OUTPUT WKCFILE
                       WKDFILE
                       WKEFILE.
           PERFORM M11-READ-WKB THRU M11-EXIT.
           PERFORM M12-READ-WKA THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKBFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-WKB.
           READ WKBFILE INTO WKB-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M11-EXIT.
           MOVE WKB-SKYCD TO WIN1-SKYCD.
           MOVE WKB-SNSCD TO WIN1-SNSCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ WKAFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-WKA.
           READ WKAFILE INTO WKA-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M12-EXIT.
           MOVE WKA-SESCD TO WIN2-SESCD.
           MOVE WKA-SQNCD TO WIN2-SQNCD.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN        (MATCHING)                                 *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF WIN1-KEY1 = WIN2-KEY1
              PERFORM M21-MATCH THRU M21-EXIT
           ELSE
              IF WIN1-KEY1 < WIN2-KEY1
                 PERFORM M22-WKBONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKAONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN2-KEY1 TO WSV2-KEY1.
           MOVE SPACE     TO WFL1-RKB20.
           PERFORM M211-WRITE-WKC  THRU M211-EXIT.
           PERFORM UNTIL WIN2-KEY1 NOT = WSV2-KEY1
              PERFORM M212-WRITE-WKD  THRU M212-EXIT
              IF WKB-YSTKB     = '1'    AND
                 WKA-RKB20     = '20'   AND
                 WFL1-RKB20    = SPACE
                 PERFORM M213-WRITE-WKE1 THRU M213-EXIT
              END-IF
              PERFORM M12-READ-WKA THRU M12-EXIT
           END-PERFORM.
           PERFORM M11-READ-WKB    THRU M11-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : WRITE WKC                                          *
      *----------------------------------------------------------------*
       M211-WRITE-WKC.
           MOVE SPACE        TO WKC-R.
           MOVE WKB-SKYCD    TO WKC-SKYCD.
           MOVE WKB-SNSCD    TO WKC-SNSCD.
           MOVE WKB-SYUCD    TO WKC-YUSCD.
           MOVE WKB-SYNCD    TO WKC-YUNCD.
           MOVE WKB-SYHHK    TO WKC-SYHHK.
           WRITE WKC-REC FROM WKC-R.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2 : WRITE WKD                                          *
      *----------------------------------------------------------------*
       M212-WRITE-WKD.
           MOVE WKA-R        TO WKD-R.
           IF WKB-KAINO      NOT = SPACE
              MOVE WKB-KAINO TO WKD-KAINO.
           MOVE WKB-YSTKB    TO WKD-YTKBN.
           IF WKB-YUBIN      NOT = SPACE
              MOVE WKB-YUBIN TO WKD-YSYBN.
           IF WKB-BUNO       NOT = SPACE
              MOVE WKB-BUNO  TO WKD-YSBNO.
           IF WKB-SOKNO      NOT = SPACE
              MOVE WKB-SOKNO TO WKD-YSSTN.
           IF WKB-CKUKG      NOT = SPACE
              MOVE WKB-CKUKG TO WKD-YSCKG.
           MOVE WKB-SYUCD    TO WKD-YUSCD.
           MOVE WKB-SYNCD    TO WKD-YUNCD.
           STRING WKA-YSBNO WKA-YSSTN WKA-YSCKG WKA-SESCD WKA-SQNCD
              DELIMITED BY SIZE INTO WKD-SORTJ.
           WRITE WKD-REC FROM WKD-R.
       M212-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.3 : WRITE WKE1                                         *
      *----------------------------------------------------------------*
       M213-WRITE-WKE1.
           MOVE '1'          TO WFL1-RKB20.
           MOVE SPACE        TO WKE-R.
           IF WKB-KAINO   NOT = SPACE
              MOVE WKB-KAINO    TO WKE-KAINO
           ELSE
              MOVE WKA-KAINO    TO WKE-KAINO
           END-IF.
           IF WKB-BUNO    NOT = SPACE
              MOVE WKB-BUNO     TO WKE-YSBNO
           ELSE
              MOVE WKA-YSBNO    TO WKE-YSBNO
           END-IF.
           IF WKB-SOKNO   NOT = SPACE
              MOVE WKB-SOKNO    TO WKE-YSSKNO
           ELSE
              MOVE WKA-YSSTN    TO WKE-YSSKNO
           END-IF.
           IF WKB-CKUKG   NOT = SPACE
              MOVE WKB-CKUKG    TO WKE-YSCKG
           ELSE
              MOVE WKA-YSCKG    TO WKE-YSCKG
           END-IF.
           MOVE WKB-SYUCD    TO WKE-YUCOD.
           MOVE WKB-SYNCD    TO WKE-YSNCD.
           MOVE WKB-YSTRYU   TO WKE-YSTRYU.
           MOVE WKA-SQBNO    TO WKE-SKBNO.
           MOVE WKA-SQSTN    TO WKE-SKSKNO.
           MOVE WKA-SQCKK    TO WKE-SKCKG.
           MOVE WKA-SESCD    TO WKE-SESCD.
           MOVE WKA-SQNCD    TO WKE-SQNCD.
           MOVE WKB-SSTRYU   TO WKE-SSTRYU.
           MOVE WKB-SKSIM    TO WKE-SKSIM.
           MOVE WKB-SYHHK    TO WKE-SYHHK.
           MOVE '1'          TO WKE-ICHHK.
           WRITE WKE-REC FROM WKE-R.
       M213-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKBFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKBONL.
           IF WKB-YSTKB     = '1'    AND
              WKB-SYHHK     = '1'
              PERFORM M221-WRITE-WKE2 THRU M221-EXIT.
           PERFORM M11-READ-WKB    THRU M11-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.1 : WRITE WKE2                                         *
      *----------------------------------------------------------------*
       M221-WRITE-WKE2.
           MOVE SPACE        TO WKE-R.
           MOVE WKB-KAINO    TO WKE-KAINO.
           MOVE WKB-BUNO     TO WKE-YSBNO.
           MOVE WKB-SOKNO    TO WKE-YSSKNO.
           MOVE WKB-CKUKG    TO WKE-YSCKG.
           MOVE WKB-SYUCD    TO WKE-YUCOD.
           MOVE WKB-SYNCD    TO WKE-YSNCD.
           MOVE WKB-YSTRYU   TO WKE-YSTRYU.
           MOVE WKB-BUNO     TO WKE-SKBNO.
           MOVE WKB-SOKNO    TO WKE-SKSKNO.
           MOVE WKB-CKUKG    TO WKE-SKCKG.
           MOVE WKB-SKYCD    TO WKE-SESCD.
           MOVE WKB-SNSCD    TO WKE-SQNCD.
           MOVE WKB-SSTRYU   TO WKE-SSTRYU.
           MOVE WKB-SKSIM    TO WKE-SKSIM.
           MOVE WKB-SYHHK    TO WKE-SYHHK.
           MOVE '0'          TO WKE-ICHHK.
           WRITE WKE-REC FROM WKE-R.
       M221-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKAFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKAONL.
           DISPLAY 'R VBKA4990 SEIKYUU JYOUHOU(DWKB) NASI = '
              WIN2-SESCD WIN2-SQNCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKAFILE
                 WKBFILE
                 WKCFILE
                 WKDFILE
                 WKEFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKA4990 END                                           *
      *----------------------------------------------------------------*
