./ ADD NAME=VBSG4390  01.02 20180912 20180912 151041 00240 00212 MCSK004
       ID  DIVISION.
       PROGRAM-ID.     'VBSG4390'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : S                             *
      *              <> PG.OUTLINE     : HANBAI KAKAKU FILE(ZENRYO)    *
      *                                   NO SANKA TENKAI              *
      *                                                                *
      *                 * INPUT FILE   : HANBAI KAKAKU(ZENRYO)(KAKAKU  *
      *                                    IKKATU KANRISAKI TUKI)(DWKA)*
      *                                : HANBAI KAKAKU FILE(KANRI BUN) *
      *                                                          (DWKB)*
      *                 * OUTPUT FILE  : HANBAI KAKAKU FILE(SANKA BUN) *
      *                                                          (DWKC)*
      *                                                                *
      *              <> AUTHOR         : CSK-N.EGUCHI                  *
      *              <> DATE           : 18/09/12                      *
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
           02 FILLER                        PIC  X(58).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(51).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(51).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  WKA-R.
           02 WKA-TOKCD                     PIC  X(07).
           02 WKA-SYOCD                     PIC  X(09).
           02 WKA-TKYBI                     PIC  X(08).
           02 WKA-KETKB                     PIC  X(01).
           02 WKA-HTANK                     PIC S9(09)  COMP-3.
           02 WKA-SMHBK                     PIC S9(09)  COMP-3.
           02 WKA-KEIKB                     PIC  X(01).
           02 WKA-KEINO                     PIC  X(10).
           02 WKA-NOSCD                     PIC  X(02).
           02 WKA-SYOKB                     PIC  X(01).
           02 WKA-KNSKB                     PIC  X(01).
           02 WKA-KKHKB                     PIC  X(01).
           02 WKA-KIKSK                     PIC  X(07).
       01  WKB-R.
           02 WKB-TOKCD                     PIC  X(07).
           02 WKB-SYOCD                     PIC  X(09).
           02 WKB-TKYBI                     PIC  X(08).
           02 WKB-KETKB                     PIC  X(01).
           02 WKB-HTANK                     PIC S9(09)  COMP-3.
           02 WKB-SMHBK                     PIC S9(09)  COMP-3.
           02 WKB-KEIKB                     PIC  X(01).
           02 WKB-KEINO                     PIC  X(10).
           02 WKB-NOSCD                     PIC  X(02).
           02 WKB-SYOKB                     PIC  X(01).
           02 WKB-KNSKB                     PIC  X(01).
           02 WKB-KKHKB                     PIC  X(01).
       01  WKC-R.
           02 WKC-TOKCD                     PIC  X(07).
           02 WKC-SYOCD                     PIC  X(09).
           02 WKC-TKYBI                     PIC  X(08).
           02 WKC-KETKB                     PIC  X(01).
           02 WKC-HTANK                     PIC S9(09)  COMP-3.
           02 WKC-SMHBK                     PIC S9(09)  COMP-3.
           02 WKC-KEIKB                     PIC  X(01).
           02 WKC-KEINO                     PIC  X(10).
           02 WKC-NOSCD                     PIC  X(02).
           02 WKC-SYOKB                     PIC  X(01).
           02 WKC-KNSKB                     PIC  X(01).
           02 WKC-KKHKB                     PIC  X(01).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-KIKSK              PIC  X(07).
                 04 WIN1-SYOCD              PIC  X(09).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-TOKCD              PIC  X(07).
                 04 WIN2-SYOCD              PIC  X(09).
       01  SAVE-AREA.
           02 WSV1-KEY1.
              03 WSV1-KIKSK                 PIC  X(07).
              03 WSV1-SYOCD                 PIC  X(09).
           02 WSV2-KEY1.
              03 WSV2-TOKCD                 PIC  X(07).
              03 WSV2-SYOCD                 PIC  X(09).
       01  TABLE-AREA.
           02 WTB1-WKBX OCCURS 100.
              03 WTB1-WKB                   PIC  X(51).
       01  COUNT-AREA.
           02 J                             PIC S9(04) COMP.
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL WIN1-KEY = HIGH-VALUE
                      OR WIN2-KEY = HIGH-VALUE.
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
           MOVE WKA-KIKSK          TO WIN1-KIKSK.
           MOVE WKA-SYOCD          TO WIN1-SYOCD.
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
           MOVE WKB-SYOCD          TO WIN2-SYOCD.
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
           MOVE LOW-VALUE     TO TABLE-AREA.
           MOVE WIN2-KEY1     TO WSV2-KEY1.
           PERFORM M211-TABLE-SET THRU M211-EXIT
              VARYING J FROM 1 BY 1
              UNTIL WIN2-KEY1 NOT = WSV2-KEY1.
       M21-L01.
           MOVE WIN1-KEY1     TO WSV1-KEY1.
           PERFORM M212-WRITE-WKC THRU M212-EXIT
              UNTIL WIN1-KEY1 NOT = WSV1-KEY1.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : TABLE-SET                                          *
      *----------------------------------------------------------------*
       M211-TABLE-SET.
           IF J > 100
              DISPLAY 'VBSG4390 TABLE OVER ! KEY = ' WSV2-KEY1
              UPON CONSOLE
              PERFORM M3-FINL THRU M3-EXIT
              CALL 'VSBABEND'.
       M211-L01.
           MOVE WKB-R         TO WTB1-WKB(J).
           PERFORM M12-READ-WKB THRU M12-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2 : WRITE-WKC                                          *
      *----------------------------------------------------------------*
       M212-WRITE-WKC.
           PERFORM VARYING J FROM 1 BY 1
              UNTIL J > 100 OR
                    WTB1-WKB(J) = LOW-VALUE
              MOVE WTB1-WKB(J)   TO WKC-R
              MOVE WKA-TOKCD     TO WKC-TOKCD
              MOVE SPACE         TO WKC-KEINO
              MOVE SPACE         TO WKC-NOSCD
              WRITE WKC-REC FROM WKC-R
           END-PERFORM.
       M212-L01.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M212-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKAFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKAONL.
           MOVE WIN1-KEY1     TO WSV1-KEY1.
           PERFORM M11-READ-WKA THRU M11-EXIT
              UNTIL WIN1-KEY1 NOT = WSV1-KEY1.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKBFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKBONL.
           MOVE WIN2-KEY1     TO WSV2-KEY1.
           PERFORM M12-READ-WKB THRU M12-EXIT
              UNTIL WIN2-KEY1 NOT = WSV2-KEY1.
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
      *           VBSG4390 END                                         *
      *----------------------------------------------------------------*
