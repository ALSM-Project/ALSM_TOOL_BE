./ ADD NAME=VBKA4950  01.21 20030818 20031030 141902 00267 00196 MCSK042
       ID  DIVISION.
       PROGRAM-ID.     'VBKA4950'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K.A                           *
      *              <> PG.OUTLINE     : SEIKYU JYOUHOU DATA           *
      *                                  TORITOME HANEI.               *
      *                                                                *
      *                 * INPUT FILE   : SEIKYUSYO TORITOME DATA (DWKA)*
      *                 * INPUT FILE   : SEIKYU JYOUHOU DATA     (DWKB)*
      *                 * OUTPUT FILE  : SEIKYU JYOUHOU DATA           *
      *                                  ( TORITOME HANEI )      (DWKC)*
      *                                                                *
      *              <> AUTHOR         : CSK-C.YAMAGATA                *
      *              <> DATE           : 03/08/18                      *
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
           02 FILLER                        PIC  X(030).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(080).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(100).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  WKA-R.
           02 WKA-SKYCD                     PIC  X(07).
           02 WKA-SNSCD                     PIC  X(02).
           02 WKA-TRRYU.
              03 WKA-TRIYU                  OCCURS 15
                                            PIC  X(01).
           02 FILLER                        PIC  X(06).
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
       01  WKC-R.
           02 WKC-SKYCD                     PIC  X(07).
           02 WKC-SNSCD                     PIC  X(02).
           02 WKC-SYUCD                     PIC  X(07).
           02 WKC-SYNCD                     PIC  X(02).
           02 WKC-SYHHK                     PIC  X(01).
           02 WKC-SYUSO.
              03 WKC-SIMBI                  PIC  X(02).
              03 WKC-KAINO                  PIC  X(02).
              03 WKC-BUNO                   PIC  X(03).
              03 WKC-SOKNO                  PIC  X(03).
              03 WKC-CKUKG                  PIC  X(05).
              03 WKC-YSTRYU.
                 04 WKC-YTRYU               OCCURS 15
                                            PIC  X(01).
              03 WKC-YSTKB                  PIC  X(01).
              03 WKC-YUBIN                  PIC  X(07).
           02 WKC-SEIKU.
              03 WKC-SSTRYU.
                 04 WKC-STRYU               OCCURS 15
                                            PIC  X(01).
              03 WKC-SSTKB                  PIC  X(01).
           02 WKC-SKSIM                     PIC  X(08).
           02 FILLER                        PIC  X(19).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-SKYCD              PIC  X(07).
                 04 WIN1-SNSCD              PIC  X(02).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-SKYCD              PIC  X(07).
                 04 WIN2-SNSCD              PIC  X(02).
       01  WED-AREA.
           02 WED1-SKSIMX.
              03 WED1-SKSIM                 PIC  9(08).
       01  INDEX-AREA.
           02 I                             PIC  9(02) COMP-3.
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
                OUTPUT WKCFILE.
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
           MOVE WKA-SKYCD TO WIN2-SKYCD.
           MOVE WKA-SNSCD TO WIN2-SNSCD.
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
                 PERFORM M22-WKBONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKAONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           PERFORM M211-EDIT-WKC1 THRU M211-EXIT.
           PERFORM M11-READ-WKB THRU M11-EXIT.
           PERFORM M12-READ-WKA THRU M12-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : EDIT WKC1                                        *
      *----------------------------------------------------------------*
       M211-EDIT-WKC1.
           MOVE SPACE      TO WKC-R.
           MOVE WKB-SKYCD  TO WKC-SKYCD.
           MOVE WKB-SNSCD  TO WKC-SNSCD.
           MOVE WKB-SYUCD  TO WKC-SYUCD.
           MOVE WKB-SYNCD  TO WKC-SYNCD.
           MOVE WKB-SYHHK  TO WKC-SYHHK.
           MOVE WKB-SIMBI  TO WKC-SIMBI.
           MOVE WKB-KAINO  TO WKC-KAINO.
           MOVE WKB-BUNO   TO WKC-BUNO.
           MOVE WKB-SOKNO  TO WKC-SOKNO.
           MOVE WKB-CKUKG  TO WKC-CKUKG.
           IF WKC-SKYCD    =  WKC-SYUCD   AND
              WKC-SNSCD    =  WKC-SYNCD
              PERFORM VARYING I FROM 1  BY 1
                   UNTIL  I  >  15
                 IF WKA-TRIYU(I)  =  '*'  OR
                    WKB-TRIYU(I)  =  '*'
                    MOVE '*'  TO WKC-YTRYU(I)
                 END-IF
              END-PERFORM
           ELSE
              MOVE WKB-STRIYU TO WKC-YSTRYU
           END-IF.
           MOVE WKB-STKBN  TO WKC-YSTKB.
           MOVE WKB-YUBIN  TO WKC-YUBIN.
           MOVE WKA-TRRYU  TO WKC-SSTRYU.
           MOVE '1'        TO WKC-SSTKB.
           MOVE WKB-SKSIM  TO WED1-SKSIM.
           MOVE WED1-SKSIMX   TO WKC-SKSIM.
           WRITE WKC-REC FROM WKC-R.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKBFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKBONL.
           PERFORM M221-EDIT-WKC2 THRU M221-EXIT.
           PERFORM M11-READ-WKB THRU M11-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.1 : EDIT WKC2                                        *
      *----------------------------------------------------------------*
       M221-EDIT-WKC2.
           MOVE SPACE      TO WKC-R.
           MOVE WKB-SKYCD  TO WKC-SKYCD.
           MOVE WKB-SNSCD  TO WKC-SNSCD.
           MOVE WKB-SYUCD  TO WKC-SYUCD.
           MOVE WKB-SYNCD  TO WKC-SYNCD.
           MOVE WKB-SYHHK  TO WKC-SYHHK.
           MOVE WKB-SIMBI  TO WKC-SIMBI.
           MOVE WKB-KAINO  TO WKC-KAINO.
           MOVE WKB-BUNO   TO WKC-BUNO.
           MOVE WKB-SOKNO  TO WKC-SOKNO.
           MOVE WKB-CKUKG  TO WKC-CKUKG.
           MOVE WKB-STRIYU TO WKC-YSTRYU.
           MOVE WKB-STKBN  TO WKC-YSTKB.
           MOVE WKB-YUBIN  TO WKC-YUBIN.
           MOVE WKB-SKSIM  TO WED1-SKSIM.
           MOVE WED1-SKSIMX   TO WKC-SKSIM.
           MOVE '0'        TO WKC-SSTKB.
           WRITE WKC-REC FROM WKC-R.
       M221-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKAFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKAONL.
           DISPLAY 'R VBKA4950 SEIKYUU JYOUHOU(DWKB) NASI = '
              WIN2-SKYCD WIN2-SNSCD
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
                 WKCFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKA4950 END                                           *
      *----------------------------------------------------------------*
