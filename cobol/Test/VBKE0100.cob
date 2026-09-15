./ ADD NAME=VBKE0100  01.04 20010716 20171213 104739 00393 00422 MCSK004
       ID  DIVISION.
       PROGRAM-ID.     'VBKE0100'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : KAISYU JYOHO MASTER           *
      *                                          KOUMOKU HONYAKU       *
      *                 * INPUT FILE   : KAISYU JYOHO MASTER (SI.KJH)  *
      *                 * INPUT FILE   : TOKUISAKI SIME                *
      *                                     URIKAKEKIN DATA  (DWKA)    *
      *                 * OUTPUT FILE  : KAISYUU YOTEI WORK  (DWKB)    *
      *                 * OUTPUT FILE  : MINUS SEIKYU KAISYU           *
      *                                          YOTEI WORK  (DWKC)    *
      *                                                                *
      *              <> AUTHOR         : CSK-K.INUKAI                  *
      *              <> DATE           : 98/11/11                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-443              ÍÝº³   : 00/08/16  CSK-H.ISHII         *
      * (02) K-444              ÍÝº³   : 00/09/22  CSK-T.KINOSHITA     *
      * (03) K-444              ÍÝº³   : 00/11/24  CSK-T.HATTORI       *
      * (04) K-491              ÍÝº³   : 01/07/16  CSK-T.HATTORI       *
      * (05) K-2023             ÍÝº³   : 17/12/13  CSK-N.EGUCHI        *
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
           02 FILLER                        PIC  X(200).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(183).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(183).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE VDCLKJH END-EXEC.
       01  KJH REDEFINES DCLKJH.
           COPY VTABKJH SUPPRESS.
       01  P89-PARM.
           COPY VPARB89 SUPPRESS.
       01  WKA-R.
           02 WKA-BUKG                      PIC  X(01).
           02 WKA-SOKKG                     PIC  X(01).
           02 WKA-TOKCD                     PIC  X(07).
           02 WKA-NOSCD                     PIC  X(02).
           02 WKA-DYYMM                     PIC  9(06).
           02 WKA-SHIME                     PIC  9(02).
           02 WKA-BUNO                      PIC  X(03).
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
           COPY VTABKYF SUPPRESS.
           02 WKB-SHIME                     PIC  9(02).
           02 WKB-TMSEI                     PIC S9(13)  COMP-3.
           02 WKB-TMUGK                     PIC S9(13)  COMP-3.
           02 WKB-SKNBHS                    PIC  X(01).
           02 WKB-KFTKB                     PIC  X(01).
           02 WKB-KFSSB                     PIC  X(01).
           02 WKB-SKFKSB                    PIC  X(01).
           02 WKB-TGTKJ                     PIC  9(03).
           02 WKB-SKDKCD                    PIC  X(04).
           02 WKB-KSYKB                     PIC  9(02).
           02 WKB-KSSJK                     PIC  X(44).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-TOKCD              PIC  X(07).
                 04 WIN1-NOSCD              PIC  X(02).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-TOKCD              PIC  X(07).
                 04 WIN2-NOSCD              PIC  X(02).
       01  EDIT-AREA.
           02 WED1-SQLCD                    PIC  ---9.
           02 WED1-KSYTB                    PIC  X(02).
       01  FLAG-AREA.
           02 WFL1-OYAUM                    PIC  X(01).
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
           PERFORM M10-LOCK-KJH THRU M10-EXIT.
           OPEN INPUT  WKAFILE
                OUTPUT WKBFILE
                       WKCFILE.
           PERFORM M11-OPEN-KJH THRU M11-EXIT.
           PERFORM M12-READ-WKA THRU M12-EXIT.
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
      *    M1.1 : OPEN-KJH                                             *
      *----------------------------------------------------------------*
       M11-OPEN-KJH.
           EXEC SQL WHENEVER NOT FOUND  CONTINUE          END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M11-ERR     END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M11-ERR     END-EXEC.
           EXEC SQL DECLARE CSR1 CURSOR FOR
              SELECT A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,B1,B5,C8,D1,D3,
                     D5,D6,D7,D8,D9,E0,E1,E2
              FROM    KJH
              ORDER BY A0,A1
           END-EXEC.
           EXEC SQL OPEN CSR1 END-EXEC.
           PERFORM M111-FETCH-KJH THRU M111-EXIT.
           IF WIN1-KEY = HIGH-VALUE
              GO TO M11-L01.
           GO TO M11-EXIT.
       M11-L01.
           DISPLAY 'M VBKE0100 TABLE(SI.KJH) DATA 0 KEN?!'
           UPON CONSOLE.
           GO TO M11-EXIT.
       M11-ERR.
           DISPLAY 'R VBKE0100 TABLE(SI.KJH) OPEN ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1.1 : FETCH-KJH                                          *
      *----------------------------------------------------------------*
       M111-FETCH-KJH.
           EXEC SQL WHENEVER NOT FOUND  GO TO M111-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M111-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M111-ERR END-EXEC.
           EXEC SQL FETCH CSR1
              INTO :DCLKJH.A0,:DCLKJH.A1,:DCLKJH.A2,:DCLKJH.A3,
                   :DCLKJH.A4,:DCLKJH.A5,:DCLKJH.A6,:DCLKJH.A7,
                   :DCLKJH.A8,:DCLKJH.A9,:DCLKJH.B1,:DCLKJH.B5,
                   :DCLKJH.C8,:DCLKJH.D1,:DCLKJH.D3,
                   :DCLKJH.D5,:DCLKJH.D6,:DCLKJH.D7,:DCLKJH.D8,
                   :DCLKJH.D9,:DCLKJH.E0,:DCLKJH.E1,:DCLKJH.E2
           END-EXEC.
           MOVE TOKCD OF KJH TO WIN1-TOKCD.
           MOVE NOSCD OF KJH TO WIN1-NOSCD.
           GO TO M111-EXIT.
       M111-L01.
           MOVE HIGH-VALUE TO WIN1-KEY.
           GO TO M111-EXIT.
       M111-ERR.
           DISPLAY 'R VBKE0100 TABLE(SI.KJH) FETCH  ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ WKAFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-WKA.
           READ WKAFILE INTO WKA-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M12-EXIT.
           MOVE WKA-TOKCD TO WIN2-TOKCD.
           MOVE WKA-NOSCD TO WIN2-NOSCD.
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
                 PERFORM M22-KJHONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKAONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE SPACE         TO WFL1-OYAUM.
           MOVE KSYTB OF KJH  TO WED1-KSYTB.
           IF KDTKCD OF KJH NOT = SPACE AND
            (( TOKCD OF KJH NOT = KDTKCD OF KJH ) OR
             ( NOSCD OF KJH NOT = SPACE ))
              MOVE '1'           TO WFL1-OYAUM
              MOVE KDTKCD OF KJH TO TOKCD OF KJH
              MOVE SPACE         TO NOSCD OF KJH
              PERFORM M211-SELECT-KJH THRU M211-EXIT.
           PERFORM M212-WRITE THRU M212-EXIT.
           PERFORM M111-FETCH-KJH THRU M111-EXIT.
           PERFORM M12-READ-WKA THRU M12-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : SELECT-KJH                                         *
      *----------------------------------------------------------------*
       M211-SELECT-KJH.
           EXEC SQL WHENEVER NOT FOUND  GO TO M211-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M211-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M211-ERR END-EXEC.
           EXEC SQL SELECT A2,A4,A9,B1,B5,C8,D3
              INTO :DCLKJH.A2,:DCLKJH.A4,:DCLKJH.A9,:DCLKJH.B1,
                   :DCLKJH.B5,:DCLKJH.C8,:DCLKJH.D3
              FROM    KJH
              WHERE A0=:DCLKJH.A0
                AND A1=:DCLKJH.A1
           END-EXEC.
           GO TO M211-EXIT.
       M211-L01.
           DISPLAY 'R VBKE0100 SI.KJH NASHI KEY = ' KEY1 OF KJH
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
           GO TO M211-EXIT.
       M211-ERR.
           DISPLAY 'R VBKE0100 TABLE(SI.KJH) SELECT ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2 : WRITE                                              *
      *----------------------------------------------------------------*
       M212-WRITE.
           MOVE SPACE TO WKB-R.
           INITIALIZE WKB-R.
           MOVE WIN1-TOKCD    TO TOKCD  OF WKB-R.
           MOVE WIN1-NOSCD    TO NOSCD  OF WKB-R.
           MOVE KSSBT OF KJH  TO KSSBT  OF WKB-R.
           MOVE ZERO          TO RECKB  OF WKB-R.
           MOVE KDTKCD OF KJH TO KDTKCD OF WKB-R.
           MOVE KSJK OF KJH   TO KSJK   OF WKB-R.
           MOVE SITTS OF KJH  TO SITTS  OF WKB-R.
           MOVE PSTKS OF KJH  TO PSTKS  OF WKB-R.
           MOVE TGKKS OF KJH  TO TGKKS  OF WKB-R.
           MOVE ZERO          TO KYTGF  OF WKB-R.
           MOVE WKA-BUNO      TO BUNO   OF WKB-R.
           MOVE WKA-CKUKG     TO CKUKG  OF WKB-R.
           IF WKA-SHIME = ZERO
              MOVE 20 TO WKB-SHIME
           ELSE
              MOVE WKA-SHIME TO WKB-SHIME.
           MOVE WKA-TMSEI      TO WKB-TMSEI.
           COMPUTE WKB-TMUGK = WKA-TMET1 + WKA-SHET1.
           MOVE SKNBHS OF KJH  TO WKB-SKNBHS.
           MOVE KFTKB  OF KJH  TO WKB-KFTKB.
           MOVE KFSSB  OF KJH  TO WKB-KFSSB.
           MOVE SKFKSB OF KJH  TO WKB-SKFKSB.
           MOVE TGTKJ  OF KJH  TO WKB-TGTKJ.
           MOVE SKDKCD OF KJH  TO WKB-SKDKCD.
           IF WFL1-OYAUM = '1' AND KSSBT OF KJH = '04'
              MOVE KSYTB  OF KJH  TO WKB-KSYKB
           ELSE
              MOVE WED1-KSYTB     TO WKB-KSYKB.
           MOVE KSSJK  OF KJH  TO WKB-KSSJK.
       M212-L01.
           IF KSJK OF WKB-R = '1' OR '3' OR '4' OR '6'
              IF WKB-TMSEI NOT =  ZERO
                 WRITE WKB-REC FROM WKB-R
              END-IF
              IF WKB-TMSEI < ZERO
                 WRITE WKC-REC FROM WKB-R
              END-IF
              GO TO M212-EXIT
           END-IF.
       M212-L02.
           IF KSJK OF WKB-R = '2' OR '8'
              IF WKB-TMUGK NOT = ZERO OR
                 WKB-TMSEI NOT = ZERO
                 WRITE WKB-REC FROM WKB-R
              END-IF
              IF WKB-TMUGK < ZERO
                 WRITE WKC-REC FROM WKB-R
              END-IF
              GO TO M212-EXIT
           END-IF.
       M212-L03.
           IF KSJK OF WKB-R = '5'
              IF WKB-TMSEI NOT = ZERO OR
                 WKB-TMUGK NOT = ZERO
                 WRITE WKB-REC FROM WKB-R
              END-IF
              IF WKB-TMSEI < ZERO OR
                 WKB-TMUGK < ZERO
                 WRITE WKC-REC FROM WKB-R
              END-IF
           END-IF.
       M212-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : KJHFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-KJHONL.
           PERFORM M111-FETCH-KJH THRU M111-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKAFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKAONL.
           PERFORM M12-READ-WKA THRU M12-EXIT.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKAFILE
                 WKBFILE
                 WKCFILE.
           PERFORM M31-CLOSE-KJH THRU M31-EXIT.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1 : CLOSE-KJH                                            *
      *----------------------------------------------------------------*
       M31-CLOSE-KJH.
           EXEC SQL WHENEVER NOT FOUND  CONTINUE      END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M31-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M31-ERR END-EXEC.
           EXEC SQL CLOSE CSR1 END-EXEC.
           GO TO M31-EXIT.
       M31-ERR.
           DISPLAY 'R VBKE0100 TABLE(SI.KJH) CLOSE ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           CALL 'VSBABEND'.
       M31-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *           VBKE0100 END                                         *
      *----------------------------------------------------------------*
