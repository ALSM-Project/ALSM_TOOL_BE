./ ADD NAME=VBKF1340  01.05 20101118 20101118 134914 00135 00164 MCSK036
       ID  DIVISION.
       PROGRAM-ID.     'VBKF1340'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : KASOU KOUZA NUMBER HYOJIYOU   *
      *                                  DATA (SEIKYUSYO) SAKUSEI      *
      *                                                                *
      *                 * INPUT FILE   : KASOU KOUZA JYOHOU DATA       *
      *                                  (SEIKYUSYO)     (DWKH)        *
      *                 * OUTPUT FILE  : KASOU KOUZA NUMBER HYOJIYOU   *
      *                                  DATA (SEIKYUSYO)(DWKI)        *
      *                                                                *
      *              <> AUTHOR         : CSK-A.KIKUCHI                 *
      *              <> DATE           : 10/11/18                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKHFILE ASSIGN    TO S-DWKH.
           SELECT WKIFILE ASSIGN    TO S-DWKI.
       DATA DIVISION.
       FILE SECTION.
       FD  WKHFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKH-REC.
           02 FILLER                        PIC  X(50).
       FD  WKIFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKI-REC.
           02 FILLER                        PIC  X(70).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  WKH-R.
           02 WKH-TOKCD                     PIC  X(07).
           02 WKH-NOSCD                     PIC  X(02).
           02 WKH-KKZNO                     PIC  X(10).
           02 WKH-GHBCD                     PIC  X(04).
           02 WKH-SESCD                     PIC  X(07).
           02 WKH-OYAKO                     PIC  X(01).
           02 WKH-SEBUN                     PIC  X(01).
           02 FILLER                        PIC  X(18).
       01  WKI-R.
           02 WKI-SESCD                     PIC  X(07).
           02 WKI-SQNCD                     PIC  X(02).
           02 WKI-GHBCD                     PIC  X(04).
           02 WKI-TRBKJ.
              03 WKI-KKZNO.
                 04 WKI-GNSCD               PIC  X(03).
                 04 WKI-KZANO               PIC  X(07).
              03 WKI-KOZKJ                  PIC  X(04).
              03 WKI-HJBSJ                  PIC  X(30).
           02 FILLER                        PIC  X(13).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL ( WKH-R = HIGH-VALUE ).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKHFILE
                OUTPUT WKIFILE.
           PERFORM M11-READ-WKH THRU M11-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKHFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-WKH.
           READ WKHFILE INTO WKH-R
                AT END
                   MOVE HIGH-VALUE TO WKH-R.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF WKH-KKZNO = SPACE
              GO TO M2-L01
           ELSE
              IF WKH-OYAKO = '2'
                 GO TO M2-L01
              ELSE
                 IF ( WKH-SEBUN = '0' )  AND
                    ( WKH-NOSCD NOT = SPACE )
                    GO TO M2-L01
                 ELSE
                    IF ( WKH-SEBUN = '1' ) AND
                       ( WKH-NOSCD = SPACE )
                       GO TO M2-L01
                    ELSE
                       PERFORM M21-WRITE-WKI THRU M21-EXIT
                    END-IF
                 END-IF
              END-IF
           END-IF.
       M2-L01.
           PERFORM M11-READ-WKH THRU M11-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : WRITE-WKI                                            *
      *----------------------------------------------------------------*
       M21-WRITE-WKI.
           MOVE SPACE        TO WKI-R.
           MOVE WKH-SESCD    TO WKI-SESCD.
           MOVE WKH-NOSCD    TO WKI-SQNCD.
           MOVE WKH-GHBCD    TO WKI-GHBCD.
           MOVE WKH-KKZNO    TO WKI-KKZNO.
           WRITE WKI-REC   FROM WKI-R.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKHFILE
                 WKIFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKF1340 END                                           *
      *----------------------------------------------------------------*
