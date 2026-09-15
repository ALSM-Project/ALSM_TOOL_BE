./ ADD NAME=VBSG4400  01.04 20180912 20180920 114301 00280 00286 MCSK004
       ID  DIVISION.
       PROGRAM-ID.     'VBSG4400'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : S                             *
      *              <> PG.OUTLINE     : HANBAI KAKAKU FILE(JYUTYUYOU) *
      *                                  SAKUSEI                       *
      *                                                                *
      *                 * INPUT FILE   : HANBAI KAKAKU FILE(ZENRYOU)   *
      *                                                      (DWKA)    *
      *                 * INPUT FILE   : HANBAI KAKAKU FILE(SANKABUN)  *
      *                                                      (DWKD)    *
      *                 * OUTPUT FILE  : HANBAI KAKAKU FILE(JYUTYUYOU) *
      *                                                      (DWKE)    *
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
           SELECT WKDFILE ASSIGN    TO S-DWKD.
           SELECT WKEFILE ASSIGN    TO S-DWKE.
       DATA DIVISION.
       FILE SECTION.
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(51).
       FD  WKDFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKD-REC.
           02 FILLER                        PIC  X(51).
       FD  WKEFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKE-REC.
           02 FILLER                        PIC  X(50).
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
       01  WKD-R.
           02 WKD-TOKCD                     PIC  X(07).
           02 WKD-SYOCD                     PIC  X(09).
           02 WKD-TKYBI                     PIC  X(08).
           02 WKD-KETKB                     PIC  X(01).
           02 WKD-HTANK                     PIC S9(09)  COMP-3.
           02 WKD-SMHBK                     PIC S9(09)  COMP-3.
           02 WKD-KEIKB                     PIC  X(01).
           02 WKD-KEINO                     PIC  X(10).
           02 WKD-NOSCD                     PIC  X(02).
           02 WKD-SYOKB                     PIC  X(01).
           02 WKD-KNSKB                     PIC  X(01).
           02 WKD-KKHKB                     PIC  X(01).
       01  WKE-R.
           COPY VFILHJT SUPPRESS.
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-TOKSYO.
                 05 WIN1-TOKCD              PIC  X(07).
                 05 WIN1-SYOCD              PIC  X(09).
              03 WIN1-KKHKB                 PIC  X(01).
           02 WIN2-KEY.
              03 WIN2-TOKSYO.
                 05 WIN2-TOKCD              PIC  X(07).
                 05 WIN2-SYOCD              PIC  X(09).
              03 WIN2-KKHKB                 PIC  X(01).
       01  SAVE-AREA.
      *                                  /*         TOKSYO ÌÞÚ²¸ KEY */
           02 WSV2-TOKSYO.
              03 WSV2-TOKCD                 PIC  X(07).
              03 WSV2-SYOCD                 PIC  X(09).
      *                                  /*      TOKSYO Ì§°½Ä REC Ö³ */
           02 WSV2-KKHKB                    PIC  X(01).
           02 WSV2-KNSKB                    PIC  X(01).
      *                                  /*   2ÂÏ´É TOKSYO ÌÞÚ²¸ KEY */
           02 WSV2-TOKSYO2.
              03 WSV2-TOKCD2                PIC  X(07).
              03 WSV2-SYOCD2                PIC  X(09).
      *                                  /*       2ÂÏ´É Ì§°½Ä REC Ö³ */
           02 WSV2-KKHKB2                   PIC  X(01).
           02 WSV2-KNSKB2                   PIC  X(01).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL  WIN1-KEY = HIGH-VALUE.
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKAFILE
                       WKDFILE
                OUTPUT WKEFILE.
           INITIALIZE  SAVE-AREA.
           PERFORM M11-READ-WKA THRU M11-EXIT.
           PERFORM M12-READ-WKD THRU M12-EXIT.
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
           IF WKA-KNSKB = 'D'
              GO TO M11-READ-WKA.
           MOVE WKA-TOKCD TO WIN1-TOKCD.
           MOVE WKA-SYOCD TO WIN1-SYOCD.
           MOVE WKA-KKHKB TO WIN1-KKHKB.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ WKDFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-WKD.
           READ WKDFILE INTO WKD-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
      *                                     /* WKA¶ÞÜÉ  »²ºÞÉ1¹ÝÒÖ³  */
      *                                     /* 2ÂÏ´É TOKSYO KEY SAVE */
                   MOVE WSV2-TOKSYO TO WSV2-TOKSYO2
      *                                       /* 2ÂÏ´É Ì§°½ÄREC SAVE */
                   MOVE WSV2-KKHKB  TO WSV2-KKHKB2
                   MOVE WSV2-KNSKB  TO WSV2-KNSKB2
                   GO TO M12-EXIT.
           MOVE WKD-TOKCD TO WIN2-TOKCD.
           MOVE WKD-SYOCD TO WIN2-SYOCD.
           MOVE WKD-KKHKB TO WIN2-KKHKB.
           IF WIN2-TOKSYO NOT = WSV2-TOKSYO
           THEN
              IF WSV2-TOKSYO  = SPACE
              THEN
      *                                     /* WKA¶ÞÜ¶Þ »²¼®É1¹ÝÒÖ³  */
      *                                     /* 2ÂÏ´É TOKSYO KEY SAVE */
                 MOVE WIN2-TOKSYO TO WSV2-TOKSYO
                 MOVE WKD-KKHKB   TO WSV2-KKHKB
                 MOVE WKD-KNSKB   TO WSV2-KNSKB
              END-IF
      *                                     /* 2ÂÏ´É TOKSYO KEY SAVE */
              MOVE WSV2-TOKSYO TO WSV2-TOKSYO2
      *                                       /* 2ÂÏ´É Ì§°½ÄREC SAVE */
              MOVE WSV2-KKHKB  TO WSV2-KKHKB2
              MOVE WSV2-KNSKB  TO WSV2-KNSKB2
      *                                           /* TOKSYO KEY SAVE */
              MOVE WIN2-TOKSYO TO WSV2-TOKSYO
      *                                 /* TOKSYOÌÞÚ²¸ Ì§°½ÄREC SAVE */
              MOVE WKD-KKHKB   TO WSV2-KKHKB
              MOVE WKD-KNSKB   TO WSV2-KNSKB
           END-IF.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN        (MATCHING)                                 *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF WIN1-KEY = WIN2-KEY
              PERFORM M21-MATCH THRU M21-EXIT
           ELSE
              IF WIN1-KEY < WIN2-KEY
                 PERFORM M22-WKAONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKDONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           IF WKD-KNSKB = 'D'
              IF WKD-TKYBI > WKA-TKYBI
                 GO TO M21-L01
              ELSE
                 PERFORM M211-WRITE-WKE THRU M211-EXIT
              END-IF
           ELSE
              IF WKD-KKHKB = '1'
                 PERFORM M211-WRITE-WKE THRU M211-EXIT
              ELSE
                 IF (WSV2-KKHKB       = '1' ) AND
                    (WSV2-KNSKB       = '1' )
                 THEN
                    GO TO M21-L01
                 ELSE
                    IF WKD-TKYBI > WKA-TKYBI
                    THEN
                       PERFORM M211-WRITE-WKE THRU M211-EXIT
                    END-IF
                 END-IF
              END-IF
           END-IF.
       M21-L01.
           PERFORM M11-READ-WKA THRU M11-EXIT.
           PERFORM M12-READ-WKD THRU M12-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : WRITE WKEFILE                                      *
      *----------------------------------------------------------------*
       M211-WRITE-WKE.
           MOVE SPACE TO WKE-R.
           INITIALIZE    WKE-R.
           MOVE WKA-TOKCD TO HJT-TOKCD OF WKE-R.
           MOVE WKA-SYOCD TO HJT-SYOCD OF WKE-R.
           MOVE WKA-TKYBI TO HJT-TKYBI OF WKE-R.
           MOVE WKA-KETKB TO HJT-KETKB OF WKE-R.
           MOVE WKA-HTANK TO HJT-HTANK OF WKE-R.
           MOVE WKA-SMHBK TO HJT-SMHBK OF WKE-R.
           MOVE WKA-KEIKB TO HJT-KEIKB OF WKE-R.
           MOVE WKA-KEINO TO HJT-KEINO OF WKE-R.
           MOVE WKA-NOSCD TO HJT-NOSCD OF WKE-R.
           MOVE WKA-SYOKB TO HJT-SYOKB OF WKE-R.
           MOVE '0'       TO HJT-SHEKB OF WKE-R.
           WRITE WKE-REC FROM WKE-R.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKAFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKAONL.
           IF WKA-KKHKB   = '1'
           THEN
              PERFORM M211-WRITE-WKE THRU M211-EXIT
           ELSE
      *                /* ½ÃÞÆ WKBÉSAVEÆÊ Â·ÞÉREC¶Þ SAVE »ÚÃ²ÙÀÒ     */
      *                /*      2¶²Ï´É SAVE´Ø±¶Þ WKAÄ ÄÞ³²Â¶ÊÝÃ²      */
              IF (WSV2-TOKSYO2   = WIN1-TOKSYO  ) AND
                 (WSV2-KKHKB2    = '1'          ) AND
                 (WSV2-KNSKB2    = '1'          )
              THEN
                 CONTINUE
              ELSE
                 PERFORM M211-WRITE-WKE THRU M211-EXIT
              END-IF
           END-IF.
           PERFORM M11-READ-WKA   THRU M11-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKDFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKDONL.
           PERFORM M12-READ-WKD THRU M12-EXIT.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKAFILE
                 WKDFILE
                 WKEFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *           VBSG4400 END                                         *
      *----------------------------------------------------------------*
