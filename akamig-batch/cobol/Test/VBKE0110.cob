./ ADD NAME=VBKE0110  01.03 20000922 20190826 110950 00399 00383 MCSK015
       ID  DIVISION.
       PROGRAM-ID. 'VBKE0110'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : KAISYU YOTEIBI SANSYUTU       *
      *                                                                *
      *                 * INPUT FILE   : KAISYU YOTEI WORK  (DWKB)     *
      *                 * OUTPUT FILE  : KAISYU YOTEI DATA  (DWKC)     *
      *                                : KAISYU YOTEI HYO              *
      *                                     ERROR DATA      (DWKD)     *
      *              <> AUTHOR         : CSK-K.INUKAI                  *
      *              <> DATE           : 98/11/16                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-443              ÍÝº³   : 00/08/15  CSK-H.ISHII         *
      * (02) K-444              ÍÝº³   : 00/09/22  CSK-T.KINOSHITA     *
      * (03) K-1673             ÍÝº³   : 15/02/04  CSK-N.EGUCHI        *
      * (04) H-010    O-10811   ÍÝº³   : 19/08/23 SCSK-R.SUZUKI        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
           SELECT WKDFILE ASSIGN    TO S-DWKD.
       DATA DIVISION.
       FILE SECTION.
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(183).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(174).
       FD  WKDFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKD-REC.
           02 FILLER                        PIC  X(10).
       WORKING-STORAGE SECTION.
      *-------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                  *
      *-------------------------------------------------------------*
       01  PB3-PARM.
           COPY VPARBB3 SUPPRESS.
       01  PC0-PARM.
           COPY VPARBC0 SUPPRESS.
       01  PC6-PARM.
           COPY VPARBC6 SUPPRESS.
       01  P227-PARM.
           COPY VPARB227 SUPPRESS.
       01  P228-PARM.
           COPY VPARB228 SUPPRESS.
       01  P229-PARM.
           COPY VPARB229 SUPPRESS.
       01  P232-PARM.
           COPY VPARB232 SUPPRESS.
       01  P271-PARM.
           COPY VPARB271 SUPPRESS.
       01  P272-PARM.
           COPY VPARB272 SUPPRESS.
       01  P273-PARM.
           COPY VPARB273 SUPPRESS.
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
           02 WKB-KSYSJ                     PIC  X(44).
       01  WKB-R-X REDEFINES WKB-R.
           02 WKB-WKCR                      PIC  X(130).
           02 FILLER                        PIC  X(53).
       01  WKC-R.
           COPY VTABKYF SUPPRESS.
           02 WKC-SHIME                     PIC  9(02).
           02 WKC-TMSEI                     PIC S9(13)  COMP-3.
           02 WKC-TMUGK                     PIC S9(13)  COMP-3.
           02 WKC-SKNBHS                    PIC  X(01).
           02 WKC-KFTKB                     PIC  X(01).
           02 WKC-KFSSB                     PIC  X(01).
           02 WKC-SKFKSB                    PIC  X(01).
           02 WKC-KSYSJ                     PIC  X(44).
       01  WKD-R.
           02 WKD-TOKCD                     PIC  X(07).
           02 WKD-NOSCD                     PIC  X(02).
           02 WKD-ERRCD                     PIC  X(01).
       01  EDIT-AREA.
           02 WED1-KSKSB                    PIC  9(02).
           02 WED1-SITTS                    PIC  9(02).
           02 WED1-KYKRB.
              03 WED1-KYSYM.
                 04 WED1-KYSY               PIC  9(04).
                 04 WED1-KYMM               PIC  9(02).
              03 WED1-KYDD                  PIC  9(02).
           02 WED1-KSYBI.
              03 WED1-KSSYM.
                 04 WED1-KSSY               PIC  9(04).
                 04 WED1-KSMM               PIC  9(02).
              03 WED1-KSDD                  PIC  9(02).
           02 WED1-SYM.
              03 WED1-SY                    PIC  9(04).
              03 WED1-MM                    PIC  9(02).
       01  SAVE-AREA.
           02 WSV1-KYSYM                    PIC  X(06).
           02 WSV1-SKDKCD                   PIC  X(04).
       01  FLAG-AREA.
           02 WFL1-ERROR                    PIC  X(01).
       PROCEDURE DIVISION.
      *-------------------------------------------------------------*
      *    M0 : CONTROL                                             *
      *-------------------------------------------------------------*
       M0-CONTROL.
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL ( WKB-R = HIGH-VALUE ).
           PERFORM M3-FINL THRU M3-EXIT.
       M0-EXIT.
           STOP RUN.
      *-------------------------------------------------------------*
      *    M1 : INIT                                                *
      *-------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKBFILE
                OUTPUT WKCFILE
                       WKDFILE.
           PERFORM M11-READ-WKB THRU M11-EXIT.
           IF WKB-R = HIGH-VALUE
              GO TO M1-EXIT.
       M1-L01.
           CALL 'VSBA2280' USING P228-PARM.
           MOVE ZERO TO PB3-LDFLG.
           MOVE ZERO TO PC6-LDFLG.
           MOVE ZERO TO P227-LOADS.
           MOVE ZERO TO P229-LOADS.
           MOVE ZERO TO P232-LOADS.
           MOVE ZERO TO WFL1-ERROR.
       M1-L02.
           IF WKB-SHIME = 15
              MOVE 16 TO WED1-KSKSB
           ELSE
              IF WKB-SHIME = 20
                 MOVE 21 TO WED1-KSKSB
              ELSE
                 MOVE 1 TO WED1-KSKSB.
       M1-L03.
           MOVE LOW-VALUE TO WSV1-KYSYM.
           MOVE LOW-VALUE TO WSV1-SKDKCD.
       M1-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M1.1 : READ WKBFILE                                      *
      *-------------------------------------------------------------*
       M11-READ-WKB.
           READ WKBFILE INTO WKB-R
                 AT END
                    MOVE HIGH-VALUE TO WKB-R
                    GO TO M11-EXIT.
       M11-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2 : MAIN                                                *
      *-------------------------------------------------------------*
       M2-MAIN.
           IF KSJK OF WKB-R = '2' OR '8'
              MOVE SITTS OF WKB-R TO WED1-SITTS
           ELSE
              MOVE 1              TO WED1-SITTS.
           MOVE P228-YY   TO WED1-KYSY.
           MOVE WKB-KSYKB TO WED1-KYDD.
           IF WED1-KSKSB NOT > WKB-KSYKB
              COMPUTE WED1-KYMM = P228-MM + ( WED1-SITTS - 1 )
           ELSE
              COMPUTE WED1-KYMM = P228-MM + WED1-SITTS.
           IF WED1-KYMM > 12
              COMPUTE WED1-KYSY = WED1-KYSY + 1
              COMPUTE WED1-KYMM = WED1-KYMM - 12.
       M2-L01.
           IF P228-MM = 12
              MOVE 1 TO WED1-MM
              COMPUTE WED1-SY = P228-YY + 1
           ELSE
              COMPUTE WED1-MM = P228-MM + 1
              MOVE P228-YY TO WED1-SY.
       M2-L02.
           IF WED1-SYM < WED1-KYSYM
              IF WED1-KYDD > 28
                 MOVE WED1-KYKRB TO P272-SYMDX
                 CALL 'VSBA2720' USING P272-PARM
                 MOVE P272-SYMDX TO WED1-KSYBI
              ELSE
                 MOVE WED1-KYKRB TO WED1-KSYBI
              END-IF
           ELSE
              EVALUATE KSSBT OF WKB-R
                 WHEN '03' PERFORM M21-FURIKOMI THRU M21-EXIT,
                 WHEN '04' PERFORM M22-KOUZA-FURIKAE THRU M22-EXIT,
                 WHEN OTHER PERFORM M23-SONOTA THRU M23-EXIT
              END-EVALUATE.
       M2-L03.
           PERFORM M24-WRITE-WKC THRU M24-EXIT.
           PERFORM M11-READ-WKB THRU M11-EXIT.
       M2-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *   M2.1 : KAISYU YOTEIBI SANSYUTU (FURIKOMI)                 *
      *-------------------------------------------------------------*
       M21-FURIKOMI.
           IF WED1-KYDD = '31' OR '30' OR '29'
              MOVE WED1-KYKRB TO PC0-RYMDS8
              CALL 'VSBA1200' USING PC0-PARM
              MOVE PC0-YMDS8  TO PB3-SYMD
              CALL 'VSBA1130' USING PB3-PARM
              IF WED1-KYDD = '31'
                 IF PB3-KYJHS = ZERO
                    MOVE PC0-RYMDS8 TO WED1-KSYBI
                    GO TO M21-EXIT
                 ELSE
                    MOVE 1 TO PC6-EGYNS
                 END-IF
              ELSE
                 IF WED1-KYDD = '30'
                    IF PB3-KYJHS = ZERO
                       MOVE 1 TO PC6-EGYNS
                    ELSE
                       MOVE 2 TO PC6-EGYNS
                    END-IF
                 ELSE
                    IF PB3-KYJHS = ZERO
                       MOVE 2 TO PC6-EGYNS
                    ELSE
                       MOVE 3 TO PC6-EGYNS
                    END-IF
                 END-IF
              END-IF
              MOVE '1'        TO PC6-KUBUN
              MOVE PC0-RYMDS8 TO PC6-HZUK1
              CALL 'VSBA1260' USING PC6-PARM
              MOVE PC6-HZUK1  TO WED1-KSYBI
           ELSE
              MOVE WED1-KYKRB TO PB3-SYMDR
              CALL 'VSBA1130' USING PB3-PARM
              IF PB3-KYJHS = ZERO
                 MOVE WED1-KYKRB TO WED1-KSYBI
              ELSE
                 MOVE '1'     TO PC6-KUBUN
                 MOVE 1       TO PC6-EGYNS
                 MOVE WED1-KYKRB TO PC6-HZUK1
                 CALL 'VSBA1260' USING PC6-PARM
                 IF WED1-KYSYM = PC6-HZSYM
                    MOVE PC6-HZUK1 TO WED1-KSYBI
                 ELSE
                    MOVE ZERO    TO PC6-KUBUN
                    MOVE 1       TO PC6-EGYNS
                    MOVE WED1-KYKRB TO PC6-HZUK1
                    CALL 'VSBA1260' USING PC6-PARM
                    MOVE PC6-HZUK1 TO WED1-KSYBI
                 END-IF
              END-IF
           END-IF.
       M21-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *   M2.2 : KAISYU YOTEIBI SANSYUTU (KOUZA FURIKAE)            *
      *-------------------------------------------------------------*
       M22-KOUZA-FURIKAE.
           IF WED1-KYSYM = WSV1-KYSYM  AND
              WKB-SKDKCD = WSV1-SKDKCD
              MOVE P271-KZHBI TO WED1-KSYBI
              PERFORM M221-WRITE-WKD THRU M221-EXIT
              GO TO M22-EXIT
           ELSE
              MOVE WED1-KYSYM TO WSV1-KYSYM
              MOVE WKB-SKDKCD TO WSV1-SKDKCD
           END-IF.
           MOVE WED1-KYSYM TO P271-SYMDOX.
           MOVE WKB-SKDKCD TO P271-SKDCD.
           CALL 'VSBA2710' USING P271-PARM.
           IF P271-KZHBI = ZERO
              DISPLAY 'R VBKE0110 SI.SKC NASHI '
              UPON CONSOLE
              DISPLAY '  YYYYMM = ' WED1-KYSYM
              UPON CONSOLE
              DISPLAY '  SKDKCD = ' WKB-SKDKCD
              UPON CONSOLE
              PERFORM M3-FINL THRU M3-EXIT
              CALL 'VSBABEND'
           ELSE
              MOVE P271-KZHBI TO WED1-KSYBI
              PERFORM M221-WRITE-WKD THRU M221-EXIT
           END-IF.
       M22-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *   M2.2.1 : WRITE WKDFILE                                    *
      *-------------------------------------------------------------*
       M221-WRITE-WKD.
           IF P271-IDDBI NOT > P228-DATE
              MOVE TOKCD OF WKB-R TO WKD-TOKCD
              MOVE NOSCD OF WKB-R TO WKD-NOSCD
              MOVE '2'            TO WKD-ERRCD
              WRITE WKD-REC FROM WKD-R
              MOVE '1' TO WFL1-ERROR.
       M221-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *   M2.3 : KAISYU YOTEIBI SANSYUTU (SONOTA)                   *
      *-------------------------------------------------------------*
       M23-SONOTA.
           IF WED1-KYDD = '31' OR '30' OR '29'
              MOVE WED1-KYKRB TO PC0-RYMDS8
              CALL 'VSBA1200' USING PC0-PARM
              MOVE PC0-YMDS8  TO P227-DATE
              CALL 'VSBA2270' USING P227-PARM
              IF WED1-KYDD = '31'
                 IF P227-SIGN = ZERO
                    MOVE PC0-RYMDS8 TO WED1-KSYBI
                    GO TO M23-EXIT
                 ELSE
                    MOVE PC0-YMDS8  TO P232-DATE
                    CALL 'VSBA2320' USING P232-PARM
                 END-IF
              ELSE
                 IF WED1-KYDD = '30'
                    MOVE PC0-YMDS8  TO P232-DATE
                    CALL 'VSBA2320' USING P232-PARM
                    IF P227-SIGN NOT = ZERO
                       CALL 'VSBA2320' USING P232-PARM
                    END-IF
                 ELSE
                    MOVE PC0-YMDS8  TO P232-DATE
                    CALL 'VSBA2320' USING P232-PARM
                    CALL 'VSBA2320' USING P232-PARM
                    IF P227-SIGN NOT = ZERO
                       CALL 'VSBA2320' USING P232-PARM
                    END-IF
                 END-IF
              END-IF
              MOVE P232-DATEX  TO WED1-KSYBI
           ELSE
              MOVE WED1-KYKRB TO P227-DATEX
              CALL 'VSBA2270' USING P227-PARM
              IF P227-SIGN = ZERO
                 MOVE WED1-KYKRB TO WED1-KSYBI
              ELSE
                 MOVE WED1-KYKRB TO P232-DATEX
                 CALL 'VSBA2320' USING P232-PARM
                 IF WED1-KYSYM = P232-YYMM
                    MOVE P232-DATEX TO WED1-KSYBI
                 ELSE
                    MOVE 1       TO P229-YOKU
                    MOVE WED1-KYKRB TO P229-DATEX
                    CALL 'VSBA2290' USING P229-PARM
                    MOVE P229-DATEX TO WED1-KSYBI
                 END-IF
              END-IF
           END-IF.
       M23-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *   M2.4 : WRITE WKCFILE                                      *
      *-------------------------------------------------------------*
       M24-WRITE-WKC.
           IF WFL1-ERROR NOT = '1'
              MOVE WKB-WKCR TO WKC-R
              MOVE WKB-KSYSJ TO WKC-KSYSJ
              MOVE WED1-KSYBI TO KSYBI OF WKC-R
              IF KSSBT OF WKB-R = '05' OR '08'
                 MOVE WED1-KSYBI TO P273-SYMDX
                 MOVE WKB-SHIME  TO P273-SIMEB
                 MOVE WKB-TGTKJ  TO P273-NISSU
                 CALL 'VSBA2730' USING P273-PARM
                 MOVE P273-SYMDX TO TYTKJ OF WKC-R
              ELSE
                 MOVE ZERO       TO TYTKJ OF WKC-R
              END-IF
              MOVE P228-DATEX TO SASYMD OF WKC-R
              WRITE WKC-REC FROM WKC-R.
           MOVE ZERO TO WFL1-ERROR.
       M24-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *   M3 : FINAL                                                *
      *-------------------------------------------------------------*
       M3-FINL.
           CLOSE WKBFILE
                 WKCFILE
                 WKDFILE.
       M3-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *      VBKE0110  END                                          *
      *-------------------------------------------------------------*
