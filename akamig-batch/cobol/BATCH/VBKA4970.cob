./ ADD NAME=VBKA4970  01.09 20030819 20031030 112101 00229 00206 MCSK049
       ID  DIVISION.
       PROGRAM-ID.     'VBKA4970'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K.A                           *
      *              <> PG.OUTLINE     : YUUSOUSAKI TORITOMEBUN HANEI  *
      *                                                                *
      *                 * INPUT FILE   : SEIKYU JYOUHOU DATA           *
      *                                  ( TORITOME HANEI )      (DWKA)*
      *                 * INPUT FILE   : SEIKYU JYOUHOU DATA           *
      *                                  ( YUUSOUSAKI TORITOME ) (DWKB)*
      *                 * OUTPUT FILE  : SEIKYU JYOUHOU DATA           *
      *                                  ( TORITOME HANEI )      (DWKC)*
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
       DATA DIVISION.
       FILE SECTION.
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(100).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(010).
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
           02 WKA-SYUCD                     PIC  X(07).
           02 WKA-SYNCD                     PIC  X(02).
           02 WKA-SYHHK                     PIC  X(01).
           02 WKA-SYUSO.
              03 WKA-SIMBI                  PIC  X(02).
              03 WKA-KAINO                  PIC  X(02).
              03 WKA-BUNO                   PIC  X(03).
              03 WKA-SOKNO                  PIC  X(03).
              03 WKA-CKUKG                  PIC  X(05).
              03 WKA-YSTRYU.
                 04 WKA-YTRYU               OCCURS 15
                                            PIC  X(01).
              03 WKA-YSTKB                  PIC  X(01).
              03 WKA-YUBIN                  PIC  X(07).
           02 WKA-SEIKU.
              03 WKA-SSTRYU.
                 04 WKA-STRYU               OCCURS 15
                                            PIC  X(01).
              03 WKA-SSTKB                  PIC  X(01).
           02 WKA-SKSIM                     PIC  X(08).
           02 FILLER                        PIC  X(19).
       01  WKB-R.
           02 WKB-SYUCD                     PIC  X(07).
           02 WKB-SYNCD                     PIC  X(02).
           02 FILLER                        PIC  X(01).
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
                 04 WIN1-SYUCD              PIC  X(07).
                 04 WIN1-SYNCD              PIC  X(02).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-SYUCD              PIC  X(07).
                 04 WIN2-SYNCD              PIC  X(02).
       01  SAVE-AREA.
           02 WSV1-KEY.
              03 WSV1-KEY1.
                 04 WSV1-SYUCD              PIC  X(07).
                 04 WSV1-SYNCD              PIC  X(02).
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
           MOVE WKA-SYUCD TO WIN1-SYUCD.
           MOVE WKA-SYNCD TO WIN1-SYNCD.
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
                 PERFORM M22-WKAONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKBONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN1-KEY1  TO WSV1-KEY1.
           PERFORM M211-WRITE-WKC THRU M211-EXIT
              UNTIL WIN1-KEY  NOT = WSV1-KEY1.
           PERFORM M12-READ-WKB THRU M12-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1: WRITE  WKC                                          *
      *----------------------------------------------------------------*
       M211-WRITE-WKC.
           MOVE WKA-R      TO WKC-R.
           MOVE '1'        TO WKC-YSTKB.
           WRITE WKC-REC FROM WKC-R.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKAFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKAONL.
           MOVE WIN1-KEY1  TO WSV1-KEY1.
           PERFORM M221-WRITE-WKC THRU M221-EXIT
              UNTIL WIN1-KEY  NOT = WSV1-KEY1.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.1: WRITE  WKC (WKA ONLY)                               *
      *----------------------------------------------------------------*
       M221-WRITE-WKC.
           MOVE WKA-R      TO WKC-R.
           WRITE WKC-REC FROM WKC-R.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M221-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKBFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKBONL.
           DISPLAY 'R VBKA4970 SEIKYUU JYOUHOU(DWKA) NASI = '
              WIN2-SYUCD WIN2-SYNCD
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
      *         VBKA4970 END                                           *
      *----------------------------------------------------------------*
