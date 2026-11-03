./ ADD NAME=VSBA2730  01.01 19981020 19981020 203252 00250 00265 TSO67
       ID  DIVISION.
       PROGRAM-ID.     'VSBA2730'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : SUBROUTINE                    *
      *              <> PG.OUTLINE     : TEGATA YOTEI KIJITSU          *
      *                                  SANSYUTSU                     *
      *                                                                *
      *                 * INPUT  PARM  : P273-SYMD                     *
      *                                : P273-SIMEB                    *
      *                                : P273-NISSU                    *
      *                 * OUTPUT PARM  : P273-SYMD                     *
      *                                                                *
      *              <> AUTHOR         : CSK-T.NAKAMURA                *
      *              <> DATE           : 98/10/20                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  PC0-PARM.
           COPY VPARBC0 SUPPRESS.
       01  EDIT-AREA.
           02 WED1-CSMEB                    PIC  9(08).
           02 WED1-CSMEBX  REDEFINES  WED1-CSMEB.
              03 WED1-CSMSY                 PIC  9(04).
              03 WED1-CSMMM                 PIC  9(02).
              03 WED1-CSMDD                 PIC  9(02).
           02 WED1-TYTKJ                    PIC  9(08).
           02 WED1-TYTKJX  REDEFINES  WED1-TYTKJ.
              03 WED1-TYTSY                 PIC  9(04).
              03 WED1-TYTMM                 PIC  9(02).
              03 WED1-TYTDD                 PIC  9(02).
           02 WED1-SIMEB                    PIC  9(02).
           02 WED1-NISSU                    PIC  9(03).
           02 WED1-MM                       PIC  9(02).
           02 WED1-AMARI                    PIC  9(02).
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *   LINCAGE SECTION                                              *
      *----------------------------------------------------------------*
       01  PARAM.
           COPY VPARB273.
       PROCEDURE DIVISION USING PARAM.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
       M0-CONTROL.
           PERFORM M1-INIT THRU M1-EXIT.
       M0-EXIT.
           GOBACK.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           PERFORM M11-CSMEB-GET THRU M11-EXIT.
           PERFORM M12-TYTKJ-GET THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : CSMEB GET                                            *
      *----------------------------------------------------------------*
       M11-CSMEB-GET.
           MOVE P273-SYMD  TO WED1-CSMEB.
           MOVE P273-SIMEB TO WED1-SIMEB.
           IF WED1-SIMEB = ZERO
              MOVE 20 TO WED1-SIMEB
           END-IF.
           IF WED1-SIMEB = 15 OR 20
              IF WED1-SIMEB < P273-DD
                 MOVE WED1-SIMEB TO WED1-CSMDD
              ELSE
                 IF WED1-CSMMM = 1
                    COMPUTE WED1-CSMSY = WED1-CSMSY - 1
                    MOVE 12 TO WED1-CSMMM
                 ELSE
                    COMPUTE WED1-CSMMM = WED1-CSMMM - 1
                 END-IF
                 MOVE WED1-SIMEB TO WED1-CSMDD
              END-IF
           ELSE
              IF WED1-CSMMM = 1
                 COMPUTE WED1-CSMSY = WED1-CSMSY - 1
                 MOVE 12 TO WED1-CSMMM
              ELSE
                 COMPUTE WED1-CSMMM = WED1-CSMMM - 1
              END-IF
              MOVE 31 TO WED1-CSMDD
           END-IF.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : TYTKJ GET                                            *
      *----------------------------------------------------------------*
       M12-TYTKJ-GET.
           IF P273-NISSU > 360
              MOVE 360        TO WED1-NISSU
           ELSE
              MOVE P273-NISSU TO WED1-NISSU
           END-IF.
           DIVIDE WED1-NISSU BY 30 GIVING WED1-MM
              REMAINDER WED1-AMARI.
       M12-L01.
           EVALUATE WED1-SIMEB
              WHEN  15
                 PERFORM M121-TYTKJ-SANSTU1 THRU M121-EXIT
                 MOVE WED1-TYTKJ TO P273-SYMD,
              WHEN 20
                 PERFORM M122-TYTKJ-SANSTU2 THRU M122-EXIT
                 MOVE WED1-TYTKJ TO P273-SYMD,
              WHEN 31
                 PERFORM M123-TYTKJ-SANSTU3 THRU M123-EXIT
                 MOVE WED1-TYTKJ TO P273-SYMD,
              WHEN  OTHER
                 CONTINUE
           END-EVALUATE.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.1 : TYTKJ SANSTU 1 ( P273-SIMEB = 15 )                 *
      *----------------------------------------------------------------*
       M121-TYTKJ-SANSTU1.
           EVALUATE WED1-AMARI
              WHEN  ZERO
                 MOVE 15 TO WED1-TYTDD
                 COMPUTE WED1-TYTMM = WED1-CSMMM + WED1-MM
                 PERFORM M1211-TYTMM-SANSTU THRU M1211-EXIT,
              WHEN  1 THRU 12
                 COMPUTE WED1-TYTDD = 15 + WED1-AMARI
                 COMPUTE WED1-TYTMM = WED1-CSMMM + WED1-MM
                 PERFORM M1211-TYTMM-SANSTU THRU M1211-EXIT,
              WHEN 13
                 COMPUTE WED1-TYTMM = WED1-CSMMM + WED1-MM
                 PERFORM M1211-TYTMM-SANSTU THRU M1211-EXIT
                 PERFORM M1212-MATUB-SANSTU THRU M1212-EXIT
                 COMPUTE WED1-TYTDD = PC0-DDS8 - 2,
              WHEN 14
                 COMPUTE WED1-TYTMM = WED1-CSMMM + WED1-MM
                 PERFORM M1211-TYTMM-SANSTU THRU M1211-EXIT
                 PERFORM M1212-MATUB-SANSTU THRU M1212-EXIT
                 COMPUTE WED1-TYTDD = PC0-DDS8 - 1,
              WHEN 15
                 COMPUTE WED1-TYTMM = WED1-CSMMM + WED1-MM
                 PERFORM M1211-TYTMM-SANSTU THRU M1211-EXIT
                 PERFORM M1212-MATUB-SANSTU THRU M1212-EXIT
                 MOVE PC0-DDS8 TO WED1-TYTDD,
              WHEN 16 THRU 29
                 COMPUTE WED1-TYTDD = WED1-AMARI - 15
                 COMPUTE WED1-TYTMM = WED1-CSMMM + WED1-MM + 1
                 PERFORM M1211-TYTMM-SANSTU THRU M1211-EXIT,
              WHEN  OTHER
                 CONTINUE
           END-EVALUATE.
       M121-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.1.1 : TYTMM SANSTU                                     *
      *----------------------------------------------------------------*
       M1211-TYTMM-SANSTU.
           IF WED1-TYTMM > 12
              COMPUTE WED1-TYTMM = WED1-TYTMM - 12
              COMPUTE WED1-TYTSY = WED1-CSMSY + 1
           ELSE
              MOVE WED1-CSMSY TO WED1-TYTSY
           END-IF.
       M1211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.1.2 : MATUB SANSTU                                     *
      *----------------------------------------------------------------*
       M1212-MATUB-SANSTU.
           MOVE WED1-TYTKJ TO PC0-YMDS8.
           MOVE 01         TO PC0-DDS8.
           CALL 'VSBA1200' USING PC0-PARM.
       M1212-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.2 : TYTKJ SANSTU 2 ( P273-SIMEB = 00 OR 20 )           *
      *----------------------------------------------------------------*
       M122-TYTKJ-SANSTU2.
           EVALUATE WED1-AMARI
              WHEN  ZERO
                 MOVE 20 TO WED1-TYTDD
                 COMPUTE WED1-TYTMM = WED1-CSMMM + WED1-MM
                 PERFORM M1211-TYTMM-SANSTU THRU M1211-EXIT,
              WHEN  1 THRU  7
                 COMPUTE WED1-TYTDD = 20 + WED1-AMARI
                 COMPUTE WED1-TYTMM = WED1-CSMMM + WED1-MM
                 PERFORM M1211-TYTMM-SANSTU THRU M1211-EXIT,
              WHEN  8
                 COMPUTE WED1-TYTMM = WED1-CSMMM + WED1-MM
                 PERFORM M1211-TYTMM-SANSTU THRU M1211-EXIT
                 PERFORM M1212-MATUB-SANSTU THRU M1212-EXIT
                 COMPUTE WED1-TYTDD = PC0-DDS8 - 2,
              WHEN  9
                 COMPUTE WED1-TYTMM = WED1-CSMMM + WED1-MM
                 PERFORM M1211-TYTMM-SANSTU THRU M1211-EXIT
                 PERFORM M1212-MATUB-SANSTU THRU M1212-EXIT
                 COMPUTE WED1-TYTDD = PC0-DDS8 - 1,
              WHEN 10
                 COMPUTE WED1-TYTMM = WED1-CSMMM + WED1-MM
                 PERFORM M1211-TYTMM-SANSTU THRU M1211-EXIT
                 PERFORM M1212-MATUB-SANSTU THRU M1212-EXIT
                 MOVE PC0-DDS8 TO WED1-TYTDD,
              WHEN 11 THRU 29
                 COMPUTE WED1-TYTDD = WED1-AMARI - 10
                 COMPUTE WED1-TYTMM = WED1-CSMMM + WED1-MM + 1
                 PERFORM M1211-TYTMM-SANSTU THRU M1211-EXIT,
              WHEN  OTHER
                 CONTINUE
           END-EVALUATE.
       M122-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.3 : TYTKJ SANSTU 3 ( P273-SIMEB = 31 )                 *
      *----------------------------------------------------------------*
       M123-TYTKJ-SANSTU3.
           EVALUATE WED1-AMARI
              WHEN  ZERO
                 COMPUTE WED1-TYTMM = WED1-CSMMM + WED1-MM
                 PERFORM M1211-TYTMM-SANSTU THRU M1211-EXIT
                 PERFORM M1212-MATUB-SANSTU THRU M1212-EXIT
                 MOVE PC0-DDS8 TO WED1-TYTDD,
              WHEN  1 THRU 27
                 MOVE WED1-AMARI TO WED1-TYTDD
                 COMPUTE WED1-TYTMM = WED1-CSMMM + WED1-MM + 1
                 PERFORM M1211-TYTMM-SANSTU THRU M1211-EXIT,
              WHEN 28
                 COMPUTE WED1-TYTMM = WED1-CSMMM + WED1-MM + 1
                 PERFORM M1211-TYTMM-SANSTU THRU M1211-EXIT
                 PERFORM M1212-MATUB-SANSTU THRU M1212-EXIT
                 COMPUTE WED1-TYTDD = PC0-DDS8 - 2,
              WHEN 29
                 COMPUTE WED1-TYTMM = WED1-CSMMM + WED1-MM + 1
                 PERFORM M1211-TYTMM-SANSTU THRU M1211-EXIT
                 PERFORM M1212-MATUB-SANSTU THRU M1212-EXIT
                 COMPUTE WED1-TYTDD = PC0-DDS8 - 1,
              WHEN  OTHER
                 CONTINUE
           END-EVALUATE.
       M123-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VSBA2730 END                                           *
      *----------------------------------------------------------------*
