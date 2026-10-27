./ ADD NAME=VBKA1640  01.06 20030818 20031022 161225 00189 00185 MCSK042
       ID  DIVISION.
       PROGRAM-ID.     'VBKA1640'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM      : SUZUKEN 10SYSTEM                 *
      *              <> SUBSYSTEM   :                                  *
      *              <> PG.OUTLINE  : ¾²·Õ³¼Ö ÄØÄÒ ÃÞ-À »¸¾²           *
      *                 * INPUT  FILE 1: ÄØÄÒ ÃÞ-À1        (DWKD)      *
      *                 *        FILE 2: ÄØÄÒ ÃÞ-À2        (DWKE)      *
      *                 * OUTPUT FILE 1: ¾²·­³¼® ÄØÄÒ ÃÞ-À (DWKA)      *
      *              <> AUTHOR      : SUZKEN-A.YAMAMOTO                *
      *              <> DATE        : 86/09/01                         *
      *-----------------  MAINTENANCE  HISTORY  -----------------------*
      * (01) K-275C             ÍÝº³   : 98/04/27  CSK-S.KOJIMA        *
      * (01) K-568              ÍÝº³   : 03/08/18  CSK-C.YAMAGATA      *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  SUZ-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKDFILE ASSIGN    TO S-DWKD.
           SELECT WKEFILE ASSIGN    TO S-DWKE.
       DATA DIVISION.
       FILE SECTION.
       FD  WKDFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKD-REC.
           02 FILLER                        PIC  X(30).
       FD  WKEFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKE-REC.
           02 FILLER                        PIC  X(30).
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(30).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  WKD-R.
           02 WKD-SESCD                     PIC  X(07).
           02 WKD-SQNCD                     PIC  X(02).
           02 WKD-TORIT                     OCCURS 15.
              03 WKD-TRIYU                  PIC  X(01).
           02 FILLER                        PIC  X(06).
       01  WKE-R.
           02 WKE-SESCD                     PIC  X(07).
           02 WKE-SQNCD                     PIC  X(02).
           02 WKE-TORIT                     OCCURS 15.
              03 WKE-TRIYU                  PIC  X(01).
           02 FILLER                        PIC  X(06).
       01  WKA-R.
           02 WKA-SESCD                     PIC  X(07).
           02 WKA-SQNCD                     PIC  X(02).
           02 WKA-TORIT                     OCCURS 15.
              03 WKA-TRIYU                  PIC  X(01).
           02 FILLER                        PIC  X(06).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-SESCD                 PIC  X(07).
              03 WIN1-SQNCD                 PIC  X(02).
           02 WIN2-KEY.
              03 WIN2-SESCD                 PIC  X(07).
              03 WIN2-SQNCD                 PIC  X(02).
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
           OPEN INPUT  WKDFILE
                       WKEFILE
                OUTPUT WKAFILE.
           PERFORM M11-READ-WKD THRU M11-EXIT.
           PERFORM M12-READ-WKE THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKDFILE ( ÄØÄÒ ÃÞ-À1 )                          *
      *----------------------------------------------------------------*
       M11-READ-WKD.
           READ WKDFILE INTO WKD-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M11-EXIT.
       M11-L01.
           MOVE WKD-SESCD TO WIN1-SESCD.
           MOVE WKD-SQNCD TO WIN1-SQNCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ WKEFILE ( ÄØÄÒ ÃÞ-À2 )                          *
      *----------------------------------------------------------------*
       M12-READ-WKE.
           READ WKEFILE INTO WKE-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M12-EXIT.
       M12-L01.
           MOVE WKE-SESCD TO WIN2-SESCD.
           MOVE WKE-SQNCD TO WIN2-SQNCD.
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
                 PERFORM M22-WKD-ONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKE-ONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           PERFORM M211-WKA-HENSHU THRU M211-EXIT.
           PERFORM M11-READ-WKD THRU M11-EXIT.
           PERFORM M12-READ-WKE THRU M12-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : ¾²·­³¼® ÄØÄÒ Ì§²Ù ( WKAFILE ) É HENSHU-WRITE       *
      *----------------------------------------------------------------*
       M211-WKA-HENSHU.
           MOVE SPACE          TO WKA-R.
           MOVE WKD-SESCD      TO WKA-SESCD.
           MOVE WKD-SQNCD      TO WKA-SQNCD.
           MOVE WKD-TRIYU (01) TO WKA-TRIYU (01).
           MOVE WKD-TRIYU (02) TO WKA-TRIYU (02).
           MOVE WKD-TRIYU (03) TO WKA-TRIYU (03).
           MOVE WKD-TRIYU (04) TO WKA-TRIYU (04).
           MOVE WKD-TRIYU (05) TO WKA-TRIYU (05).
           MOVE WKD-TRIYU (06) TO WKA-TRIYU (06).
           MOVE WKD-TRIYU (07) TO WKA-TRIYU (07).
           MOVE WKD-TRIYU (08) TO WKA-TRIYU (08).
           MOVE WKD-TRIYU (09) TO WKA-TRIYU (09).
           MOVE WKD-TRIYU (10) TO WKA-TRIYU (10).
           MOVE WKE-TRIYU (11) TO WKA-TRIYU (11).
           MOVE WKE-TRIYU (12) TO WKA-TRIYU (12).
           MOVE WKE-TRIYU (13) TO WKA-TRIYU (13).
           MOVE WKE-TRIYU (14) TO WKA-TRIYU (14).
           MOVE WKE-TRIYU (15) TO WKA-TRIYU (15).
           WRITE WKA-REC FROM WKA-R.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKDFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKD-ONL.
           MOVE  WKD-R   TO   WKA-R.
           WRITE WKA-REC FROM WKA-R.
           PERFORM M11-READ-WKD THRU M11-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKEFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKE-ONL.
           MOVE  WKE-R   TO   WKA-R.
           WRITE WKA-REC FROM WKA-R.
           PERFORM M12-READ-WKE THRU M12-EXIT.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKDFILE
                 WKEFILE
                 WKAFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKA1640 END                                           *
      *----------------------------------------------------------------*
