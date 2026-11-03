./ ADD NAME=VBKA4960  01.08 20030819 20031028 084817 00157 00215 MCSK042
       ID  DIVISION.
       PROGRAM-ID. 'VBKA4960'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K.A                           *
      *              <> PG.OUTLINE     : YUSOUSAKI TORITOMEBUN         *
      *                                  CYUUSYUTU                     *
      *                                                                *
      *                 * INPUT FILE   : SEIKYU JYOUHOU DATA           *
      *                                  (TORITOME HANEI)       (DWKA) *
      *                 * OUTPUT FILE  : SEIKYU JYOUHOU DATA           *
      *                                  (YUUSOUSAKI TORITOME)  (DWKB) *
      *                                                                *
      *              <> AUTHOR         : CSK-C.YAMAGATA                *
      *              <> DATE           : 03/08/19                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) XXXXXX XXXXXX       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
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
           02 FILLER                        PIC  X(100).
       FD  WKBFILE
           LABEL RECORD OMITTED.
       01  WKB-REC.
           02 FILLER                        PIC  X(010).
       WORKING-STORAGE SECTION.
      *-------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                  *
      *-------------------------------------------------------------*
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
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-SYUCD              PIC  X(07).
                 04 WIN1-SYNCD              PIC  X(02).
       01  SAVE-AREA.
           02 WSV1-KEY.
              03 WSV1-KEY1.
                 04 WSV1-SYUCD              PIC  X(07).
                 04 WSV1-SYNCD              PIC  X(02).
       01  FLAG-AREA.
           02 WFL1-WRITE                    PIC  X(01).
       PROCEDURE DIVISION.
      *-------------------------------------------------------------*
      *    M0 : CONTROL                                             *
      *-------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL ( WIN1-KEY = HIGH-VALUE ).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *-------------------------------------------------------------*
      *    M1 : INIT                                                *
      *-------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKAFILE
                OUTPUT WKBFILE.
       M1-L01.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M1-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M1.1 : READ WKAFILE                                      *
      *-------------------------------------------------------------*
       M11-READ-WKA.
           READ WKAFILE INTO WKA-R
                 AT END
                    MOVE HIGH-VALUE TO WIN1-KEY
                    GO TO M11-EXIT.
       M11-L01.
           MOVE WKA-SYUCD TO WIN1-SYUCD.
           MOVE WKA-SYNCD TO WIN1-SYNCD.
       M11-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2 : MAIN                                                *
      *-------------------------------------------------------------*
       M2-MAIN.
           MOVE WIN1-SYUCD TO WSV1-SYUCD.
           MOVE WIN1-SYNCD TO WSV1-SYNCD.
           MOVE SPACE      TO WFL1-WRITE.
       M2-L01.
           PERFORM M21-SYUCD-BREAK THRU M21-EXIT
               UNTIL ( WIN1-KEY1 NOT = WSV1-KEY1 ).
       M2-L02.
           IF WFL1-WRITE = '1'  THEN
              PERFORM M22-EDIT-WKB THRU M22-EXIT.
       M2-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *   M2.1 : SYUCD-BREAK                                        *
      *-------------------------------------------------------------*
       M21-SYUCD-BREAK.
           IF WFL1-WRITE = SPACE
              IF WKA-YSTKB  = '1'  OR
                 WKA-SSTKB  = '1'  THEN
                 MOVE '1'  TO  WFL1-WRITE.
           PERFORM M11-READ-WKA  THRU M11-EXIT.
       M21-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *   M2.2 : EDIT WKB                                           *
      *-------------------------------------------------------------*
       M22-EDIT-WKB.
           MOVE SPACE          TO WKB-R.
           MOVE WSV1-SYUCD     TO WKB-SYUCD.
           MOVE WSV1-SYNCD     TO WKB-SYNCD.
           WRITE WKB-REC FROM WKB-R.
       M22-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *   M3 : FINAL                                                *
      *-------------------------------------------------------------*
       M3-FINL.
           CLOSE WKAFILE
                 WKBFILE.
       M3-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *      VBKA4960  END                                          *
      *-------------------------------------------------------------*
