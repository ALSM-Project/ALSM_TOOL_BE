./ ADD NAME=VBKF1370  01.06 20101202 20101202 142707 00179 00177 MCSK036
       ID  DIVISION.
       PROGRAM-ID.     'VBKF1370'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : KASOU KOUZA NUMBER HYOZI YOU  *
      *                                  DATA (SEIKYUGOUKEI HYO)       *
      *                                  SAKUSEI                       *
      *                                                                *
      *                 * INPUT FILE   : KASOU KOUZA JYOHOU WORK       *
      *                                                  (DWKH)        *
      *                 * OUTPUT FILE  : KASOU KOUZA NUMBER HYOZI YOU  *
      *                                  DATA (SEIKYUGOUKEI HYO)       *
      *                                                  (DWKI)        *
      *                                                                *
      *              <> AUTHOR         : CSK-A.KIKUCHI                 *
      *              <> DATE           : 10/12/02                      *
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
           02 FILLER                        PIC  X(70).
       FD  WKIFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKI-REC.
           02 FILLER                        PIC  X(60).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  WKH-R.
           02 WKH-SESCD                     PIC  X(07).
           02 WKH-SQNCD                     PIC  X(02).
           02 WKH-TRBKJ.
              03 WKH-KKZNO.
                 04 WKH-GNSCD               PIC  X(03).
                 04 WKH-KZANO               PIC  X(07).
              03 WKH-KOZKJ                  PIC  X(04).
              03 WKH-HJBSJ                  PIC  X(30).
           02 WKH-YUSCD                     PIC  X(07).
           02 FILLER                        PIC  X(10).
       01  WKI-R.
           02 WKI-YUSCD                     PIC  X(07).
           02 WKI-TRBKJ.
              03 WKI-KKZNO.
                 04 WKI-GNSCD               PIC  X(03).
                 04 WKI-KZANO               PIC  X(07).
              03 WKI-KOZKJ                  PIC  X(04).
              03 WKI-HJBSJ                  PIC  X(30).
           02 FILLER                        PIC  X(09).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-YUSCD              PIC  X(07).
       01  SAVE-AREA.
           02 WSV1-KEY.
              03 WSV1-KEY1.
                 04 WSV1-YUSCD              PIC  X(07).
           02 WSV1-TRBKJ.
              03 WSV1-KKZNO.
                 04 WSV1-GNSCD              PIC  X(03).
                 04 WSV1-KZANO              PIC  X(07).
              03 WSV1-KOZKJ                 PIC  X(04).
              03 WSV1-HJBSJ                 PIC  X(30).
       01  EDIT-AREA.
           02 WED1-FAST                     PIC  X(01).
           02 WED1-FLG1                     PIC  X(01).
           02 WED1-FLG2                     PIC  X(01).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL ( WIN1-KEY = HIGH-VALUE ).
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
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M11-EXIT.
           MOVE WKH-YUSCD TO WIN1-YUSCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           MOVE SPACE     TO SAVE-AREA.
           MOVE SPACE     TO EDIT-AREA.
           MOVE WIN1-KEY1 TO WSV1-KEY1.
           PERFORM M21-CHEK-WKH THRU M21-EXIT
                   UNTIL WIN1-KEY1 NOT = WSV1-KEY1.
           IF WED1-FLG1 = '1'
              GO TO M2-EXIT.
           IF WED1-FLG2 = SPACE
              PERFORM M22-WRITE1-WKI THRU M22-EXIT
           ELSE
              PERFORM M23-WRITE2-WKI THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : CHEK-WKH                                             *
      *----------------------------------------------------------------*
       M21-CHEK-WKH.
           IF WED1-FAST = SPACE
              MOVE '1'       TO WED1-FAST
              MOVE WKH-TRBKJ TO WSV1-TRBKJ.
           IF WKH-KKZNO = SPACE
              IF WED1-FLG1 NOT = '2'
                 MOVE '1'    TO WED1-FLG1
              END-IF
           ELSE
              IF WED1-FLG1 NOT  = '2'
                 MOVE '2'       TO WED1-FLG1.
           IF WKH-KKZNO NOT = WSV1-KKZNO
              IF WED1-FLG2 = SPACE
                 MOVE '1'    TO WED1-FLG2
              END-IF
           END-IF.
       M21-L01.
           PERFORM M11-READ-WKH THRU M11-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WRITE1-WKI                                           *
      *----------------------------------------------------------------*
       M22-WRITE1-WKI.
           MOVE SPACE        TO WKI-R.
           MOVE WSV1-KEY1    TO WKI-YUSCD.
           MOVE WSV1-KKZNO   TO WKI-KKZNO.
           MOVE WSV1-KOZKJ   TO WKI-KOZKJ.
           MOVE WSV1-HJBSJ   TO WKI-HJBSJ.
           WRITE WKI-REC   FROM WKI-R.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WRITE2-WKI                                           *
      *----------------------------------------------------------------*
       M23-WRITE2-WKI.
           MOVE SPACE        TO WKI-R.
           MOVE WSV1-KEY1    TO WKI-YUSCD.
           WRITE WKI-REC   FROM WKI-R.
       M23-EXIT.
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
      *         VBKF1370 END                                           *
      *----------------------------------------------------------------*
