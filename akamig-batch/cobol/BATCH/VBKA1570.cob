./ ADD NAME=VBKA1570  01.01 20150731 20190628 142448 00243 00238 MCSK012
       ID  DIVISION.
       PROGRAM-ID.     'VBKA1570'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM      : SUZUKEN 10SYSTEM                 *
      *              <> SUBSYSTEM   :                                  *
      *              <> PG.OUTLINE  : ¾²·­³¼® Íß-¼Þ ¶Ð¹À É ÎÝÔ¸        *
      *                 * INPUT  FILE 1: ²Á×Ý¾² ¾²·­³Ò²»² ÃÞ-À         *
      *                 *        FILE 2: ¾²·­³¼® Íß-¼Þ ÃÞ-À            *
      *                 * OUTPUT FILE 1: ÎÝÔ¸ºÞ ²Á×Ý¾² ¾²·­³Ò²»² ÃÞ-À  *
      *                 *        FILE 2: ³Ø¶¹ ²¼Þ®³Ê¯¾² Ì§²Ù           *
      *              <> AUTHOR      : SUZKEN-A.YAMAMOTO                *
      *              <> DATE        : 86/08/21                         *
      *-----------------  MAINTENANCE  HISTORY  -----------------------*
      * (01) KAISYU  NO.45          : 89/03/10  CSK-R.KOITA            *
      * (02) K-122                  : 95/06/05  CSK-H.MIYATA           *
      * (03) K-568                  : 03/07/18  CSK-M.YOSHIDA          *
      * (04) K-1780                 : 15/07/31  CSK-Y.YOSHIGA          *
      * (05) K-2163                 : 19/06/28  CSK-M.MATSUSHIMA       *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT HUIFILE ASSIGN    TO AS-HUIFILE
                          ORGANIZATION SEQUENTIAL
                          FILE  STATUS S-CODE.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
       DATA DIVISION.
       FILE SECTION.
       FD  HUIFILE
           LABEL RECORD STANDARD.
       01  HUI-REC.
           02 FILLER                        PIC  X(260).
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(237).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(20).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(237).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       77  S-CODE                           PIC  X(02).
       01  HUI-R.
           COPY VFILHUI SUPPRESS.
       01  WKB-R.
           02 WKB-SKYCD.
              03 WKB-SKCD1                  PIC  X(04).
              03 WKB-SKCD2                  PIC  X(03).
           02 WKB-SKNCD                     PIC  X(02).
           02 WKB-PGKAM                     PIC  X(04).
           02 WKB-MSHZUM                    PIC  X(01).
           02 FILLER                        PIC  X(06).
       01  WKC-R.
           02 WKC-SKYCD.
              03 WKC-SKCD1                  PIC  X(04).
              03 WKC-SKCD2                  PIC  X(03).
           02 WKC-SKNCD                     PIC  X(02).
           02 WKC-PGENO.
              03 WKC-PGKAM                  PIC  X(04).
              03 WKC-PGSIM                  PIC  X(04).
           02 WKC-GYONO                     PIC  X(02).
           02 WKC-RCKBN                     PIC  X(02).
           02 WKC-DATA.
              03 FILLER                     PIC  X(204).
              03 WKC-DSHZFG                 PIC  X(01).
              03 WKC-MSHZUM                 PIC  X(01).
              03 WKC-MSZRT                  PIC S9(02)V9 COMP-3.
              03 WKC-DPGK60                 PIC S9(09)  COMP-3.
              03 WKC-KGZH60                 PIC  X(02).
              03 FILLER                     PIC  X(01).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-SKYCD                 PIC  X(07).
              03 WIN1-SKNCD                 PIC  X(02).
           02 WIN2-KEY.
              03 WIN2-SKYCD                 PIC  X(07).
              03 WIN2-SKNCD                 PIC  X(02).
       01  SAVE-AREA.
           02 WSV2-KEY.
              03 WSV2-SKYCD                 PIC  X(07).
              03 WSV2-SKNCD                 PIC  X(02).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL ( WIN1-KEY = HIGH-VALUE AND
                          WIN2-KEY = HIGH-VALUE ).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKBFILE
                       WKCFILE
                OUTPUT WKAFILE
                EXTEND HUIFILE.
       M1-L01.
           PERFORM M11-READ-WKB THRU M11-EXIT.
           PERFORM M12-READ-WKC THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKBFILE ( ¾²·­³¼® Íß-¼Þ ÃÞ-À       )            *
      *----------------------------------------------------------------*
       M11-READ-WKB.
           READ WKBFILE INTO WKB-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M11-EXIT.
       M11-L01.
           MOVE WKB-SKYCD TO WIN1-SKYCD.
           MOVE WKB-SKNCD TO WIN1-SKNCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ WKCFILE ( ²Á×Ý¾² ¾²·­³¼® Ò²»² ÃÞ-À )            *
      *----------------------------------------------------------------*
       M12-READ-WKC.
           READ WKCFILE INTO WKC-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M12-EXIT.
       M12-L01.
           MOVE WKC-SKYCD TO WIN2-SKYCD.
           MOVE WKC-SKNCD TO WIN2-SKNCD.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN           ( MATCHING - WKB : WKC     )            *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF WIN1-KEY = WIN2-KEY
              PERFORM M21-MATCH THRU M21-EXIT
           ELSE
              IF WIN1-KEY < WIN2-KEY
                 PERFORM M22-WKB-ONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKC-ONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN2-KEY TO WSV2-KEY.
           PERFORM M211-HENSHU THRU M211-EXIT
                   UNTIL ( WIN2-KEY NOT = WSV2-KEY ).
       M21-L01.
           PERFORM M11-READ-WKB THRU M11-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : WKAFILE ( ÎÝÔ¸ºÞ ²Á×Ý¾²¾²·­³Ò²»²ÃÞ-À) HENSHU-WRITE *
      *----------------------------------------------------------------*
       M211-HENSHU.
           MOVE WKB-PGKAM TO WKC-PGKAM.
           IF WKC-RCKBN = '60'
              IF WKB-MSHZUM = '1'
                 MOVE '1'    TO WKC-MSHZUM
              ELSE
                 MOVE ZERO   TO WKC-MSHZUM.
           WRITE WKA-REC FROM WKC-R.
       M211-L01.
           PERFORM M12-READ-WKC THRU M12-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKBFILE ( ³Ø¶¹ ²¼Þ®³Ê¯¾² Ì§²Ù ) HENSHU-WRITE         *
      *----------------------------------------------------------------*
       M22-WKB-ONL.
           MOVE SPACE          TO HUI-R.
           MOVE 'VBKA1570'     TO HUI-PRGME.
           MOVE WIN1-KEY       TO HUI-DKEY.
           MOVE '¾²·Õ³¼Ö Íß-¼Þ DATA ÉÐ' TO HUI-MSGNI.
           MOVE WKB-R          TO HUI-DATAB.
           MOVE  'Z '    TO   S-CODE.
       M22-L01.
           WRITE HUI-REC FROM HUI-R.
           IF S-CODE NOT = ZERO
              DISPLAY 'R VBKA1570 HUIFILE WRITE ERROR !'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
              UPON CONSOLE
              PERFORM M3-FINL THRU M3-EXIT
              CALL 'VSBABEND'.
       M22-L02.
           PERFORM M11-READ-WKB THRU M11-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKCFILE ONLY ( M231 ÃÞ HENSHU-WRITE )                *
      *----------------------------------------------------------------*
       M23-WKC-ONL.
           MOVE WIN2-KEY TO WSV2-KEY.
           PERFORM M231-HUI-HENSHU THRU M231-EXIT
                   UNTIL ( WIN2-KEY NOT = WSV2-KEY ).
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1 : HUIFILE ( ³Ø¶¹ ²¼Þ®³Ê¯¾² Ì§²Ù ) HENSHU-WRITE       *
      *----------------------------------------------------------------*
       M231-HUI-HENSHU.
           MOVE SPACE          TO HUI-R.
       M231-L01.
           MOVE 'VBKA1570'     TO HUI-PRGME.
           MOVE WIN2-KEY       TO HUI-DKEY.
           MOVE '¾²·Õ³Ò²»²·ÞÖ³ DATA ÉÐ' TO HUI-MSGNI.
           MOVE WKC-R          TO HUI-DATAB.
           MOVE  'Z '    TO   S-CODE.
           WRITE HUI-REC FROM HUI-R.
           IF S-CODE NOT = ZERO
              DISPLAY 'R VBKA1570 HUIFILE WRITE ERROR !'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
              UPON CONSOLE
              PERFORM M3-FINL THRU M3-EXIT
              CALL 'VSBABEND'.
       M231-L02.
           PERFORM M12-READ-WKC THRU M12-EXIT.
       M231-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE HUIFILE
                 WKAFILE
                 WKBFILE
                 WKCFILE.
       M3-EXIT.
           EXIT.
