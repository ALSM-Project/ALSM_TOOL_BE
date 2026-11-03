./ ADD NAME=VBKA1250  01.02 19970926 19971003 095319 00182 00182 TSO67
       ID  DIVISION.
       PROGRAM-ID.     'VBKA1250'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SUZUKEN 10 SYSTEM             *
      *              <> SUBSYSTEM      : 20KA JIME SEIKYU SYORI        *
      *              <> PG.OUTLINE     : MATCHING ( N : 1 )            *
      *                                                                *
      *                 * INPUT FILE   : SEIKYUSYO UTIWAKE   (WKAFILE) *
      *                 * INPUT FILE   : TOKUISAKI JUSYO MST (TJMFILE) *
      *                 * OUTPUT FILE  : HONYAKUGO UTIWAKE   (WKDFILE) *
      *                                                                *
      *              <> AUTHOR         : SUZUKEN-Y.ITO                 *
      *              <> DATE           : 61/08/05                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) SYOHIZEI TUIKA      Â²¶   : 89/03/06  CSK-S.MUNEISHI      *
      * (02) K-261              ÍÝº³   : 97/10/03  CSK-T.NAKAMURA      *
      * (03) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT TJMFILE ASSIGN    TO TJMFILE
                          ORGANIZATION INDEXED
                          ACCESS  MODE SEQUENTIAL
                          RECORD   KEY KEY-TJM
                          FILE  STATUS S-CODE.
           SELECT WKDFILE ASSIGN    TO S-DWKD.
       DATA DIVISION.
       FILE SECTION.
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(280).
       FD  TJMFILE
           LABEL RECORD STANDARD.
       01  TJM-REC.
           02 KEY-TJM                       PIC  X(07).
           02 FILLER                        PIC  X(293).
       FD  WKDFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKD-REC.
           02 FILLER                        PIC  X(280).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STRAGE SECTION                                      *
      *----------------------------------------------------------------*
       77  S-CODE                           PIC  X(02).
       01  WKA-R.
           02  FILLER                       PIC  X(07).                 RFI00040
           02  WKA-SAICD                    PIC  X(07).                 RFI00080
           02  FILLER                       PIC  X(64).                 RFI00040
           02  WKA-ADRES                    PIC  X(100).                RFI00080
           02  WKA-YUBNO                    PIC  X(07).                 RFI00080
           02  FILLER                       PIC  X(95).                 RFI00080
       01  TJM-R.
           COPY VFILTJM SUPPRESS.
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-SAICD                 PIC  X(07).
           02 WIN2-KEY.
              03 WIN2-TOKCD                 PIC  X(07).
       01  SAVE-AREA.
           02 WSV1-KEY                      PIC  X(07).
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
                       TJMFILE
                OUTPUT WKDFILE.
           PERFORM M11-READ-WKA THRU M11-EXIT.
           PERFORM M12-READ-TJM THRU M12-EXIT.
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
           MOVE WKA-SAICD  TO WIN1-SAICD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ TJMFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-TJM.
           MOVE 'Z '    TO   S-CODE.
           READ TJMFILE INTO TJM-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M12-EXIT.
           IF S-CODE NOT = ZERO
              DISPLAY 'R VBKA1250 TJMFILE READ ERROR'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
              UPON CONSOLE
              MOVE HIGH-VALUE TO WIN2-KEY
              GO TO M12-EXIT.
           MOVE TJM-TOKCD  TO WIN2-TOKCD.
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
                 PERFORM M23-TJMONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN1-KEY  TO WSV1-KEY.
           PERFORM M211-WRITE-WKA THRU M211-EXIT
                   UNTIL  WIN1-KEY NOT = WSV1-KEY.
           PERFORM M12-READ-TJM THRU M12-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : WRITE WKAFILE ( MATCH )                            *
      *----------------------------------------------------------------*
       M211-WRITE-WKA.
           MOVE TJM-ADRES TO WKA-ADRES.
           MOVE TJM-YUBNO TO WKA-YUBNO.
           WRITE WKD-REC FROM WKA-R.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKAFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKAONL.
           MOVE WIN1-KEY  TO WSV1-KEY.
           PERFORM M221-WRITE-WKAONL THRU M221-EXIT
                   UNTIL  WIN1-KEY NOT = WSV1-KEY.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.1 : WRITE WKAFILE ( ONLY )                             *
      *----------------------------------------------------------------*
       M221-WRITE-WKAONL.
           MOVE SPACE     TO WKA-ADRES.
           MOVE SPACE     TO WKA-YUBNO.
           WRITE WKD-REC FROM WKA-R.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M221-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : TJMFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-TJMONL.
           PERFORM M12-READ-TJM THRU M12-EXIT.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3: FINAL                                                   *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKAFILE
                 TJMFILE
                 WKDFILE.
       M3-EXIT.
           EXIT.
