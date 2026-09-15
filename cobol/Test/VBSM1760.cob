./ ADD NAME=VBSM1760  01.03 20021217 20030227 195231 00291 00293 MCSK088
       ID  DIVISION.
       PROGRAM-ID.     'VBSM1760'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : S.M                           *
      *              <> PG.OUTLINE     : SITEI DENPYOUSAKI DATA SAKUJO *
      *                                  SYORI                         *
      *                                                                *
      *                 * INPUT FILE   : TOKUISAKI MASTER (SI.TMS)     *
      *                 * INPUT FILE   : TOKUISAKI JIME URIKAKEKIN DATA*
      *                                                          (DWKB)*
      *                 * OUTPUT FILE  : TOKUISAKI JIME URIKAKEKIN DATA*
      *                                                          (DWKA)*
      *                                                                *
      *              <> AUTHOR         : CSK-S.OKA                     *
      *              <> DATE           : 02/12/17                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
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
           02 FILLER                        PIC  X(200).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(200).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE VDCLTMS END-EXEC.
       01  TMS REDEFINES DCLTMS.
           COPY VTABTMS SUPPRESS.
       01  P89-PARM.
           COPY VPARB89 SUPPRESS.
       01  WKA-R.
           02 WKA-STNKG                     PIC  X(01).
           02 WKA-SOKKG                     PIC  X(01).
           02 WKA-TOKCD                     PIC  X(07).
           02 WKA-NOSCD                     PIC  X(02).
           02 WKA-DYYMM                     PIC  9(06).
           02 WKA-SHIME                     PIC  9(02).
           02 WKA-STNCD                     PIC  X(03).
           02 WKA-CKUKG                     PIC  X(05).
           02 WKA-TOKME                     PIC  X(25).
           02 WKA-NOSME                     PIC  X(25).
           02 WKA-SKUCD                     PIC  X(07).
           02 WKA-ZMURI                     PIC S9(13)  COMP-3.
           02 WKA-ZMSEI                     PIC S9(13)  COMP-3.
           02 WKA-ICHYO                     PIC S9(13)  COMP-3.
           02 WKA-NYKIN                     PIC S9(13)  COMP-3.
           02 WKA-ZKRIK                     PIC S9(13)  COMP-3.
           02 WKA-TMKIR.
              03 WKA-TMET1                  PIC S9(13)  COMP-3.
              03 WKA-TMNE1                  PIC S9(13)  COMP-3.
           02 WKA-TMSEI                     PIC S9(13)  COMP-3.
           02 WKA-SAKIG                     PIC S9(13)  COMP-3.
           02 WKA-TMURI                     PIC S9(13)  COMP-3.
           02 WKA-TMHAN                     PIC S9(13)  COMP-3.
           02 WKA-SHZEI.
              03 WKA-SHET1                  PIC S9(13)  COMP-3.
              03 WKA-SHNE1                  PIC S9(13)  COMP-3.
           02 WKA-TSHZE                     PIC S9(13)  COMP-3.
           02 FILLER                        PIC  X(18).
       01  WKB-R.
           02 WKB-STNKG                     PIC  X(01).
           02 WKB-SOKKG                     PIC  X(01).
           02 WKB-TOKCD                     PIC  X(07).
           02 WKB-NOSCD                     PIC  X(02).
           02 WKB-DYYMM                     PIC  9(06).
           02 WKB-SHIME                     PIC  9(02).
           02 WKB-STNCD                     PIC  X(03).
           02 WKB-CKUKG                     PIC  X(05).
           02 WKB-TOKME                     PIC  X(25).
           02 WKB-NOSME                     PIC  X(25).
           02 WKB-SKUCD                     PIC  X(07).
           02 WKB-ZMURI                     PIC S9(13)  COMP-3.
           02 WKB-ZMSEI                     PIC S9(13)  COMP-3.
           02 WKB-ICHYO                     PIC S9(13)  COMP-3.
           02 WKB-NYKIN                     PIC S9(13)  COMP-3.
           02 WKB-ZKRIK                     PIC S9(13)  COMP-3.
           02 WKB-TMKIR.
              03 WKB-TMET1                  PIC S9(13)  COMP-3.
              03 WKB-TMNE1                  PIC S9(13)  COMP-3.
           02 WKB-TMSEI                     PIC S9(13)  COMP-3.
           02 WKB-SAKIG                     PIC S9(13)  COMP-3.
           02 WKB-TMURI                     PIC S9(13)  COMP-3.
           02 WKB-TMHAN                     PIC S9(13)  COMP-3.
           02 WKB-SHZEI.
              03 WKB-SHET1                  PIC S9(13)  COMP-3.
              03 WKB-SHNE1                  PIC S9(13)  COMP-3.
           02 WKB-TSHZE                     PIC S9(13)  COMP-3.
           02 FILLER                        PIC  X(18).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-TOKCD              PIC  X(07).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-TOKCD              PIC  X(07).
       01  SAVE-AREA.
           02 WSV2-KEY1.
              03 WSV2-TOKCD                 PIC  X(07).
       01  EDIT-AREA.
           02 WED1-SQLCD                    PIC  ---9.
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL (WIN2-KEY = HIGH-VALUE).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           PERFORM M10-LOCK-TMS THRU M10-EXIT.
           OPEN INPUT  WKBFILE
                OUTPUT WKAFILE.
           PERFORM M11-OPEN-TMS THRU M11-EXIT.
           PERFORM M12-READ-WKB THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.0 : LOCK-TMS                                             *
      *----------------------------------------------------------------*
       M10-LOCK-TMS.
           MOVE '   TMS'   TO P89-TBLID(1).
           MOVE 'S'        TO P89-LTYPE(1).
           MOVE ALL '*'    TO P89-TABLE(2).
           CALL 'VSBA0890' USING P89-PARM.
       M10-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : OPEN-TMS                                             *
      *----------------------------------------------------------------*
       M11-OPEN-TMS.
           EXEC SQL WHENEVER SQLERROR   GO TO M11-ERR     END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M11-ERR     END-EXEC.
           EXEC SQL DECLARE CSR1 CURSOR FOR
              SELECT A0
              FROM    TMS
              WHERE H5 ý= '4'
              ORDER BY A0
           END-EXEC.
           EXEC SQL OPEN CSR1 END-EXEC.
           PERFORM M111-FETCH-TMS THRU M111-EXIT.
           IF WIN1-KEY = HIGH-VALUE
              GO TO M11-L01.
           GO TO M11-EXIT.
       M11-L01.
           DISPLAY 'M VBSM1760 TABLE(SI.TMS) DATA 0 KEN?!'
           UPON CONSOLE.
           GO TO M11-EXIT.
       M11-ERR.
           DISPLAY 'R VBSM1760 TABLE(SI.TMS) OPEN ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1.1 : FETCH-TMS                                          *
      *----------------------------------------------------------------*
       M111-FETCH-TMS.
           EXEC SQL WHENEVER NOT FOUND  GO TO M111-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M111-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M111-ERR END-EXEC.
           EXEC SQL FETCH CSR1
              INTO :DCLTMS.A0
           END-EXEC.
           MOVE KEY1 OF TMS TO WIN1-TOKCD.
           GO TO M111-EXIT.
       M111-L01.
           MOVE HIGH-VALUE TO WIN1-KEY.
           GO TO M111-EXIT.
       M111-ERR.
           DISPLAY 'R VBSM1760 TABLE(SI.TMS) FETCH  ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ WKBFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-WKB.
           READ WKBFILE INTO WKB-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M12-EXIT.
           MOVE WKB-TOKCD TO WIN2-TOKCD.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN        (MATCHING)                                 *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF WIN1-KEY1 = WIN2-KEY1
              PERFORM M21-MATCH THRU M21-EXIT
           ELSE
              IF WIN1-KEY1 < WIN2-KEY1
                 PERFORM M22-TMSONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKBONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN2-KEY1 TO WSV2-KEY1.
           PERFORM M211-WRITE-WKA THRU M211-EXIT
                   UNTIL WIN2-KEY1 NOT = WSV2-KEY1.
           PERFORM M111-FETCH-TMS THRU M111-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : WRITE WKAFILE                                      *
      *----------------------------------------------------------------*
       M211-WRITE-WKA.
           MOVE SPACE        TO WKA-R.
           INITIALIZE WKA-R.
           MOVE WKB-R        TO WKA-R.
           WRITE WKA-REC     FROM WKA-R.
           PERFORM M12-READ-WKB THRU M12-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : TMSFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-TMSONL.
           PERFORM M111-FETCH-TMS THRU M111-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKBFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKBONL.
           MOVE WIN2-KEY1 TO WSV2-KEY1.
           PERFORM M12-READ-WKB THRU M12-EXIT
                   UNTIL WIN2-KEY1 NOT = WSV2-KEY1.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKAFILE
                 WKBFILE.
           PERFORM M31-CLOSE-TMS THRU M31-EXIT.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1 : CLOSE-TMS                                            *
      *----------------------------------------------------------------*
       M31-CLOSE-TMS.
           EXEC SQL WHENEVER SQLERROR   GO TO M31-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M31-ERR END-EXEC.
           EXEC SQL CLOSE CSR1 END-EXEC.
           GO TO M31-EXIT.
       M31-ERR.
           DISPLAY 'R VBSM1760 TABLE(SI.TMS) CLOSE ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           CALL 'VSBABEND'.
       M31-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *           VBSM1760 END                                         *
      *----------------------------------------------------------------*
