./ ADD NAME=VBKF0780  01.09 20010912 20180105 113022 00363 00359 MCSK004
       ID  DIVISION.
       PROGRAM-ID. 'VBKF0780'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : SEIKYU TOKUISAKI SYUKEI SYORI *
      *                                                                *
      *                 * INPUT FILE   : TOKUISAKI SYUKEI DATA  (DWKA) *
      *                 * INPUT FILE   : KAISYUU JYOUHOU MASTER        *
      *                                                       (SI.KJH) *
      *                 * OUTPUT FILE  : SEIKYU DATA (KOUZA FURIKAE)   *
      *                                                         (DWKB) *
      *                                : KOUZA FURIKAE ERROR LIST      *
      *                                                         (PRT1) *
      *                                                                *
      *              <> AUTHOR         : CSK-R.TAKAYANAGI              *
      *              <> DATE           : 99/03/05                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-444              ÍÝº³   : 00/10/06  CSK-T.KINOSHITA     *
      * (02) K-491              ÍÝº³   : 01/09/12  CSK-N.SUGIMURA      *
      * (03) K-491  S-0027      ÍÝº³   : 01/12/05  CSK-M.UEMURA        *
      * (04) K-2023             ÍÝº³   : 18/01/05  CSK-N.EGUCHI        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
           SELECT LPFILE  ASSIGN    TO S-PRT1.
       DATA DIVISION.
       FILE SECTION.
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(050).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(030).
       FD  LPFILE
           LABEL RECORD OMITTED.
       01  LP-REC.
           02 FILLER                        PIC  X(132).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE VDCLKJH END-EXEC.
       01  KJH REDEFINES DCLKJH.
           COPY VTABKJH SUPPRESS.
       01  PC5-PARM.
           COPY VPARBC5  SUPPRESS.
       01  P89-PARM.
           COPY VPARB89 SUPPRESS.
       01  P228-PARM.
           COPY VPARB228 SUPPRESS.
       01  WKA-R.
           02 WKA-SKTCD                     PIC  X(07).
           02 WKA-SKNCD                     PIC  X(02).
           02 WKA-SKDKCD                    PIC  X(04).
           02 WKA-TOKCD                     PIC  X(07).
           02 WKA-KSSBT                     PIC  X(02).
           02 WKA-KSJK                      PIC  X(01).
           02 WKA-KZHKB                     PIC  9(08).
           02 WKA-KFTKB                     PIC  X(01).
           02 WKA-KFKTF                     PIC  X(01).
           02 WKA-KYTGK                     PIC S9(11)  COMP-3.
           02 WKA-KDTKCD                    PIC  X(07).
           02 FILLER                        PIC  X(04).
       01  WKB-R.
           02 WKB-SKTCD                     PIC  X(07).
           02 WKB-SKNCD                     PIC  X(02).
           02 WKB-KZHKB                     PIC  9(08).
           02 WKB-HKOGK                     PIC S9(11)  COMP-3.
           02 WKB-HJPTN                     PIC  X(01).
           02 FILLER                        PIC  X(06).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-SKTCD              PIC  X(07).
                 04 WIN1-SKNCD              PIC  X(02).
       01  SAVE-AREA.
           02 WSV1-KEY.
              03 WSV1-KEY1.
                 04 WSV1-SKTCD.
                    05 WSV1-SKTCD1          PIC  X(04).
                    05 WSV1-SKTCD2          PIC  X(03).
                 04 WSV1-SKNCD              PIC  X(02).
           02 WSV1-SKDKCD                   PIC  X(04).
           02 WSV1-KZHKB                    PIC  9(08).
       01  EDIT-AREA.
           02 WED1-SQLCD                    PIC  ---9.
       01  TOTAL-AREA.
           02 WTT1-KYTGK                    PIC S9(14)  COMP-3.
       01  FLAG-AREA.
           02 WFL1-ERLST                    PIC  X(01).
           02 WFL1-ERROR                    PIC  X(01).
           02 WFL1-CHECK                    PIC  X(01).
       01  LPH-HEADER.
           02 LPH-HEAD1.
              03 FILLER         PIC  X(50) VALUE
                 '  VBKF0780          ** º³»ÞÌØ¶´  ´×- Ø½Ä  **      '.
              03 LPH-DATE       PIC  99/99/99.
              03 FILLER         PIC  X(06) VALUE '  »¸¾²'.
           02 LPH-HEAD2.
              03 FILLER         PIC  X(16) VALUE
                 '  <--¾ ² · Õ ³->'.
           02 LPH-HEAD3.
              03 FILLER         PIC  X(05) VALUE SPACE.
              03 FILLER         PIC  X(49) VALUE
                 'Ä¸²»·  É³¾²    º³»ÞË·µÄ¼ËÞ         º³»ÞË·µÄ¼·Ý¶Þ¸'.
       01  LPD-DETLE.
           02 LPD-DETL1.
              03 FILLER         PIC  X(02) VALUE SPACE.
              03 LPD-SKTCD      PIC  X(08).
              03 FILLER         PIC  X(03) VALUE SPACE.
              03 LPD-SKNCD      PIC  X(02).
              03 FILLER         PIC  X(05) VALUE SPACE.
              03 LPD-KZHKB      PIC  9999/99/99.
              03 FILLER         PIC  X(06) VALUE SPACE.
              03 LPD-KZHGK      PIC  ---,---,---,---,--9.
           02 LPD-DETL2.
              03 FILLER         PIC  X(20) VALUE SPACE.
              03 FILLER         PIC  X(24) VALUE
                 '*-------VBKF0780-------*'.
           02 LPD-DETL3.
              03 FILLER         PIC  X(20) VALUE SPACE.
              03 FILLER         PIC  X(24) VALUE
                 '*                      *'.
           02 LPD-DETL4.
              03 FILLER         PIC  X(20) VALUE SPACE.
              03 FILLER         PIC  X(24) VALUE
                 '*    ´×- DATA Å¼ !     *'.
           02 LPD-DETL5.
              03 FILLER         PIC  X(20) VALUE SPACE.
              03 FILLER         PIC  X(24) VALUE
                 '*----------------------*'.
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
       M0-CONTROL.
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
              UNTIL ( WIN1-KEY  =  HIGH-VALUE ).
           PERFORM M3-FINL THRU M3-EXIT.
       M0-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INIT                                                   *
      *----------------------------------------------------------------*
       M1-INIT.
           PERFORM M10-LOCK-KJH THRU M10-EXIT.
           OPEN INPUT  WKAFILE
                OUTPUT WKBFILE
                       LPFILE.
           CALL 'VSBA2280' USING P228-PARM.
           MOVE P228-DATE     TO PC5-YMDS8.
           CALL 'VSBA1250' USING PC5-PARM.
           MOVE PC5-YMDW6     TO LPH-DATE.
           MOVE SPACE         TO WFL1-ERROR.
       M1-L01.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.0 : LOCK-KJH                                             *
      *----------------------------------------------------------------*
       M10-LOCK-KJH.
           MOVE '   KJH'   TO P89-TBLID(1).
           MOVE 'S'        TO P89-LTYPE(1).
           MOVE ALL '*'    TO P89-TABLE(2).
           CALL 'VSBA0890' USING P89-PARM.
       M10-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKAFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-WKA.
           READ WKAFILE INTO WKA-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M11-EXIT.
           MOVE WKA-SKTCD  TO WIN1-SKTCD.
           MOVE WKA-SKNCD  TO WIN1-SKNCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           MOVE ZERO        TO WTT1-KYTGK.
           MOVE SPACE       TO WFL1-ERLST.
           MOVE WKA-SKDKCD  TO WSV1-SKDKCD.
           MOVE WKA-KZHKB   TO WSV1-KZHKB.
           MOVE WIN1-KEY1   TO WSV1-KEY1.
           PERFORM M21-SYUKEI      THRU M21-EXIT
                   UNTIL ( WIN1-KEY1   NOT =  WSV1-KEY1 )
                      OR ( WFL1-ERLST  NOT =  SPACE     ).
       M2-L01.
           IF ( WTT1-KYTGK  >  999999999 )  AND
              ( WFL1-ERLST  =  SPACE     )
              PERFORM M22-ERR-LIST THRU M22-EXIT
           END-IF.
       M2-L02.
           PERFORM M23-WRITE-WKB   THRU M23-EXIT.
       M2-L03.
           IF ( WFL1-ERLST  =  '1' )
              PERFORM M11-READ-WKA THRU M11-EXIT
                      UNTIL ( WIN1-KEY1  NOT =  WSV1-KEY1 )
           END-IF.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : SYUKEI                                               *
      *----------------------------------------------------------------*
       M21-SYUKEI.
           PERFORM M211-SCHECK THRU M211-EXIT.
           IF WFL1-ERLST NOT =  '1'
              COMPUTE WTT1-KYTGK  =  WTT1-KYTGK  +  WKA-KYTGK
           END-IF.
       M21-L01.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : SYUKEI CHECK                                       *
      *----------------------------------------------------------------*
       M211-SCHECK.
           IF ( WKA-KFKTF   NOT =  '0' )
              MOVE '1'  TO  WFL1-ERLST
              GO TO M211-EXIT
           END-IF.
           IF ( WKA-KSJK NOT = '1' AND '2' AND '3' AND '4' AND '6' )
              MOVE '1'  TO  WFL1-ERLST
              GO TO M211-EXIT
           END-IF.
           IF ( WKA-SKDKCD  NOT =  WSV1-SKDKCD  )
              MOVE '1'  TO  WFL1-ERLST
              GO TO M211-EXIT
           END-IF.
           MOVE SPACE TO WFL1-CHECK.
           IF ( WKA-KSJK      =  '3' OR '6' )
              PERFORM M2111-SELECT-KJH THRU M2111-EXIT
              IF ( WFL1-CHECK    =  '1'  )
                 MOVE '1'  TO  WFL1-ERLST
                 GO TO M211-EXIT
              END-IF
              IF ( KEISN1 OF KJH = SPACE ) AND
                 ( KEISN2 OF KJH = SPACE )
                 MOVE '1'  TO  WFL1-ERLST
              END-IF
           END-IF.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1.1 : SELECT KJH                                       *
      *----------------------------------------------------------------*
       M2111-SELECT-KJH.
           EXEC SQL WHENEVER NOT FOUND  GO TO M2111-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M2111-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M2111-ERR END-EXEC.
           EXEC SQL SELECT D8,D9
              INTO :DCLKJH.D8,:DCLKJH.D9
              FROM    KJH
              WHERE A0=:WKA-SKTCD
                AND A1=:WKA-SKNCD
           END-EXEC.
           GO TO M2111-EXIT.
       M2111-L01.
           MOVE '1'          TO WFL1-CHECK.
           GO TO M2111-EXIT.
       M2111-ERR.
           DISPLAY 'R VBKF0780 TABLE(SI.KJH) SELECT ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M2111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : ERR LIST PRT1                                        *
      *----------------------------------------------------------------*
       M22-ERR-LIST.
           IF ( WFL1-ERROR  =  SPACE )
              MOVE    '1'       TO   WFL1-ERROR
              MOVE  SPACE       TO   LP-REC
              WRITE LP-REC                AFTER PAGE
              WRITE LP-REC FROM LPH-HEAD1 AFTER 1
              WRITE LP-REC FROM LPH-HEAD2 AFTER 2
              WRITE LP-REC FROM LPH-HEAD3 AFTER 1
              MOVE  SPACE       TO LP-REC
              WRITE LP-REC                AFTER 1
           END-IF.
       M22-L01.
           MOVE  SPACE          TO LPD-DETL1.
           STRING WSV1-SKTCD1 '-' WSV1-SKTCD2
            DELIMITED BY SIZE INTO LPD-SKTCD.
           MOVE  WSV1-SKNCD     TO LPD-SKNCD.
           MOVE  WSV1-KZHKB     TO LPD-KZHKB.
           MOVE  WTT1-KYTGK     TO LPD-KZHGK.
           WRITE LP-REC FROM LPD-DETL1 AFTER 1.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WRITE WKBFILE                                        *
      *----------------------------------------------------------------*
       M23-WRITE-WKB.
           MOVE SPACE           TO   WKB-R.
           INITIALIZE                WKB-R.
           MOVE WSV1-SKTCD      TO   WKB-SKTCD.
           MOVE WSV1-SKNCD      TO   WKB-SKNCD.
           MOVE WSV1-KZHKB      TO   WKB-KZHKB.
           IF ( WTT1-KYTGK  >  999999999 )  OR
              ( WTT1-KYTGK  <  ZERO      )  OR
              ( WFL1-ERLST  =  '1'       )
              MOVE ZERO         TO   WKB-HKOGK
           ELSE
              MOVE WTT1-KYTGK   TO   WKB-HKOGK
           END-IF.
           IF WKB-HKOGK = ZERO
              MOVE '2'          TO   WKB-HJPTN
           ELSE
              MOVE '1'          TO   WKB-HJPTN
           END-IF.
       M23-L01.
           WRITE WKB-REC        FROM WKB-R.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           PERFORM M31-PRINT THRU M31-EXIT.
           CLOSE   WKAFILE
                   WKBFILE
                   LPFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1 : PRINT SYORI ( ERR NASI )                             *
      *----------------------------------------------------------------*
       M31-PRINT.
           IF ( WFL1-ERROR  =  SPACE )
              MOVE  SPACE       TO   LP-REC
              WRITE LP-REC                AFTER PAGE
              WRITE LP-REC FROM LPH-HEAD1 AFTER 1
              WRITE LP-REC FROM LPH-HEAD2 AFTER 2
              WRITE LP-REC FROM LPH-HEAD3 AFTER 1
              WRITE LP-REC FROM LPD-DETL2 AFTER 12
              WRITE LP-REC FROM LPD-DETL3 AFTER 1
              WRITE LP-REC FROM LPD-DETL4 AFTER 1
              WRITE LP-REC FROM LPD-DETL3 AFTER 1
              WRITE LP-REC FROM LPD-DETL5 AFTER 1
           END-IF.
       M31-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKF0780 END                                           *
      *----------------------------------------------------------------*
