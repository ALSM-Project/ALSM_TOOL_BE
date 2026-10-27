./ ADD NAME=VBKA4900  01.10 20030709 20031001 144030 00336 00337 MCSK042
       ID  DIVISION.
       PROGRAM-ID.     'VBKA4900'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K A                           *
      *              <> PG.OUTLINE     : SEIKYUUSYO YUUSOUSAKI CODE    *
      *                                  HONYAKU                       *
      *                 * INPUT        : CONTROL           (SYSIN)     *
      *                 * INPUT FILE   : TOKUISAKI MASTER (SI.TMS)     *
      *                 * INPUT FILE   : SEIKYUUSYO UTIWAKE DATA (DWKA)*
      *                 * OUTPUT FILE  : SEIKYUUSYO JYOUHOU DATA (DWKB)*
      *                                                                *
      *              <> AUTHOR         : CSK-C.YAMAGATA                *
      *              <> DATE           : 02/07/09                      *
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
           02 FILLER                        PIC  X(80).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(80).
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
       01  PC0-PARM.
           COPY VPARBC0 SUPPRESS.
       01  P228-PARM.
           COPY VPARB228 SUPPRESS.
       01  WKA-R.
           02 WKA-SKYCD                     PIC  X(07).
           02 WKA-SNSCD                     PIC  X(02).
           02 WKA-ZENGT.
              03 WKA-ZGSKY                  PIC S9(13) COMP-3.
              03 WKA-GNYKD                  PIC S9(13) COMP-3.
              03 WKA-NEBIK                  PIC S9(09) COMP-3.
              03 WKA-KURIK                  PIC S9(13) COMP-3.
           02 WKA-TOGET.
              03 WKA-OKAID                  PIC S9(13) COMP-3.
              03 WKA-TGSKY                  PIC S9(13) COMP-3.
              03 WKA-MISKY                  PIC S9(13) COMP-3.
              03 WKA-URKZD                  PIC S9(13) COMP-3.
           02 WKA-ICYOK                     PIC  X(01).
           02 WKA-SHZEI                     PIC S9(13) COMP-3.
           02 WKA-OKAIG                     PIC S9(13) COMP-3.
           02 FILLER                        PIC  X(02).
       01  WKB-R.
           02 WKB-SKYCD                     PIC  X(07).
           02 WKB-SNSCD                     PIC  X(02).
           02 WKB-SYUCD                     PIC  X(07).
           02 WKB-SYNCD                     PIC  X(02).
           02 WKB-SYHHK                     PIC  X(01).
           02 WKB-SYUSO.
              03 WKB-SIMBI                  PIC  X(02).
              03 WKB-KAINO                  PIC  X(02).
              03 WKB-BUNO                   PIC  X(03).
              03 WKB-SOKNO                  PIC  X(03).
              03 WKB-CKUKG                  PIC  X(05).
              03 WKB-STRIYU.
                 04 WKB-TRIYU               OCCURS 15
                                            PIC  X(01).
              03 WKB-STKBN                  PIC  X(01).
              03 WKB-YUBIN                  PIC  X(07).
           02 WKB-SKSIM                     PIC  9(08).
           02 WKB-ICYOK                     PIC  X(01).
           02 WKB-URKZD                     PIC S9(12)  COMP-3.
           02 FILLER                        PIC  X(07).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-TOKCD              PIC  X(07).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-SKYCD              PIC  X(07).
       01  SAVE-AREA.
           02 WSV2-KEY1.
              03 WSV2-SKYCD                 PIC  X(07).
       01  EDIT-AREA.
           02 WED1-SIMEB.
              03 WED1-SYYMM                 PIC  9(06).
              03 WED1-SDD                   PIC  X(02).
           02 WED1-SIMEBR  REDEFINES        WED1-SIMEB.
              03 WED1-SMYY                  PIC  9(04).
              03 WED1-SMMM                  PIC  9(02).
              03 WED1-SMDD                  PIC  9(02).
           02 WED1-SIMEBN  REDEFINES        WED1-SIMEB
                                            PIC  9(08).
           02 WED1-SQLCD                    PIC  ---9.
       01  SYSIN-AREA.
           02 SYS-CARD1.
              03 SYS-DAY                    PIC  X(02).
              03 FILLER                     PIC  X(78).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
       M0-CONTROL.
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL (WIN2-KEY = HIGH-VALUE).
           PERFORM M3-FINL THRU M3-EXIT.
       M0-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           PERFORM M10-LOCK-TMS THRU M10-EXIT.
           PERFORM M11-SYS-CHECK THRU M11-EXIT.
           OPEN INPUT  WKAFILE
                OUTPUT WKBFILE.
           PERFORM M12-OPEN-TMS THRU M12-EXIT.
           PERFORM M13-READ-WKA THRU M13-EXIT.
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
      *    M1.1 : SYSIN CHECK                                          *
      *----------------------------------------------------------------*
       M11-SYS-CHECK.
           ACCEPT SYS-CARD1.
           IF SYS-DAY = '20' OR '15' OR '31'
              CONTINUE
           ELSE
              DISPLAY 'R VBKA4900 SYSIN ERROR = ' SYS-DAY
              UPON CONSOLE
              CALL 'VSBABEND'.
       M11-L01.
           CALL 'VSBA2280' USING P228-PARM.
           MOVE P228-YYMM TO WED1-SYYMM.
           IF SYS-DAY = '20' OR '15'
              MOVE SYS-DAY TO WED1-SDD
           ELSE
              IF P228-DD     <  25
                 IF WED1-SMMM    =  01
                    COMPUTE WED1-SMYY = WED1-SMYY - 1
                    MOVE 12  TO  WED1-SMMM
                 ELSE
                    COMPUTE WED1-SYYMM  = WED1-SYYMM  - 1
                 END-IF
              END-IF
              MOVE WED1-SIMEB TO PC0-YMDS8
              CALL 'VSBA1200'  USING PC0-PARM
              MOVE PC0-YMDS8 TO WED1-SIMEB
           END-IF.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : OPEN-TMS                                             *
      *----------------------------------------------------------------*
       M12-OPEN-TMS.
           EXEC SQL WHENEVER SQLERROR   GO TO M12-ERR     END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M12-ERR     END-EXEC.
           EXEC SQL DECLARE CSR1 CURSOR FOR
              SELECT A0,A7,G9
              FROM    TMS
              ORDER BY A0
           END-EXEC.
           EXEC SQL OPEN CSR1 END-EXEC.
           PERFORM M121-FETCH-TMS THRU M121-EXIT.
           IF WIN1-KEY = HIGH-VALUE
              GO TO M12-L01.
           GO TO M12-EXIT.
       M12-L01.
           DISPLAY 'M VBKA4900 TABLE(SI.TMS) DATA 0 KEN?!'
           UPON CONSOLE.
           GO TO M12-EXIT.
       M12-ERR.
           DISPLAY 'R VBKA4900 TABLE(SI.TMS) OPEN ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.1 : FETCH-TMS                                          *
      *----------------------------------------------------------------*
       M121-FETCH-TMS.
           EXEC SQL WHENEVER NOT FOUND  GO TO M121-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M121-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M121-ERR END-EXEC.
           EXEC SQL FETCH CSR1
              INTO :DCLTMS.A0,:DCLTMS.A7,:DCLTMS.G9
           END-EXEC.
           MOVE KEY1 OF TMS TO WIN1-TOKCD.
           GO TO M121-EXIT.
       M121-L01.
           MOVE HIGH-VALUE TO WIN1-KEY.
           GO TO M121-EXIT.
       M121-ERR.
           DISPLAY 'R VBKA4900 TABLE(SI.TMS) FETCH  ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M121-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.3 : READ WKAFILE                                         *
      *----------------------------------------------------------------*
       M13-READ-WKA.
           READ WKAFILE INTO WKA-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M13-EXIT.
       M13-L01.
           MOVE WKA-SKYCD TO WIN2-SKYCD.
       M13-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF WIN1-KEY1 = WIN2-KEY1
              PERFORM M21-MATCH THRU M21-EXIT
           ELSE
              IF WIN1-KEY1 < WIN2-KEY1
                 PERFORM M22-TMSONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKAONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN2-KEY1 TO WSV2-KEY1.
           IF SEYOS OF TMS = '0'
              PERFORM M211-WKB-WRITE THRU M211-EXIT
                      UNTIL WIN2-KEY1 NOT = WSV2-KEY1
           ELSE
              PERFORM M13-READ-WKA THRU M13-EXIT
                      UNTIL WIN2-KEY1 NOT = WSV2-KEY1
           END-IF.
           PERFORM M121-FETCH-TMS THRU M121-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : WKB WRITE                                          *
      *----------------------------------------------------------------*
       M211-WKB-WRITE.
           MOVE SPACE        TO WKB-R.
           INITIALIZE           WKB-R.
           MOVE WKA-SKYCD    TO WKB-SKYCD.
           MOVE WKA-SNSCD    TO WKB-SNSCD.
           MOVE WED1-SIMEB   TO WKB-SKSIM.
           MOVE WKA-ICYOK    TO WKB-ICYOK.
           MOVE WKA-URKZD    TO WKB-URKZD.
           IF SEYCD OF TMS   =  SPACE
              MOVE WKA-SKYCD TO WKB-SYUCD
              MOVE WKA-SNSCD TO WKB-SYNCD
           ELSE
              MOVE SEYCD OF TMS TO WKB-SYUCD
           END-IF.
           WRITE WKB-REC     FROM WKB-R.
           PERFORM M13-READ-WKA THRU M13-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : TMSFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-TMSONL.
           PERFORM M121-FETCH-TMS THRU M121-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKAFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKAONL.
           DISPLAY 'R VBKA4900 SI.TMS NOT FOUND KEY1 = ' WIN2-KEY
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
           DISPLAY 'R VBKA4900 TABLE(SI.TMS) CLOSE ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           CALL 'VSBABEND'.
       M31-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *           VBKA4900 END                                         *
      *----------------------------------------------------------------*
