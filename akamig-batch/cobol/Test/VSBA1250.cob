./ ADD NAME=VSBA1250  01.03 19981002 20190204 105426 00059 00050 MCSK004
       ID  DIVISION.
       PROGRAM-ID.      'VSBA1250'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM      : SUZUKEN 10 SYSTEM                *
      *              <> SUBSYSTEM   : SUBROUTINE                       *
      *              <> PG.OUTLINE  : SEIREKI(8 KETA) =>               *
      *                                       WAREKI HENKAN            *
      *                                                                *
      *                 *  INPUT.PARM  : PC5-YMDS8 => SEIREKI 8        *
      *                 *  OUTPUT.PARM : PC5-TMDW6 => WAREKI(YYMMDD)   *
      *                                                                *
      *              <> AUTHOR      : CSK-K.KOYAMA                     *
      *              <> DATE        : 91/10/03                         *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) YOKEN NO.K-541     ÍÝº³   : 92/01/30  SUZ-Y.KAWAMURA      *
      * (02) YOKEN NO.K-275     ÍÝº³   : 98/10/02  CSK-R.TAKAYANAGI    *
      * (03) K-2135             ÍÝº³   : 19/02/04  CSK-N.EGUCHI        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WED1-SYMD.
           02  WED1-SSYY                    PIC  9(04).
           02  WED1-SSYY-R  REDEFINES  WED1-SSYY.
               03  WED1-SS                  PIC  9(02).
               03  WED1-YY                  PIC  9(02).
           02  WED1-MMDD                    PIC  9(04).
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *   LINCAGE SECTION                                              *
      *----------------------------------------------------------------*
       01  PARAM.
           COPY VPARBC5.
       PROCEDURE DIVISION USING PARAM.
      *----------------------------------------------------------------*
      *    1.0 : MAIN CONTROL                                          *
      *----------------------------------------------------------------*
       M10-MAIN-CONTROL.
           IF PC5-YMDS8 = ZERO
              MOVE ZERO  TO PC5-YMDW6
              GO TO M10-EXIT.
           MOVE PC5-MMS8 TO PC5-MMW6.
           MOVE PC5-DDS8 TO PC5-DDW6.
           IF PC5-YMDS8 < 19890108
              COMPUTE PC5-YYW6 = PC5-YYS8 - 25
           ELSE
              MOVE PC5-RYMDS8  TO  WED1-SYMD
              IF PC5-YMDS8 >= 19890108 AND
                 PC5-YMDS8 <  20190501
                 COMPUTE WED1-SSYY = WED1-SSYY - 1988
                 MOVE WED1-YY  TO  PC5-YYW6
              ELSE
                 COMPUTE WED1-SSYY = WED1-SSYY - 2018
                 MOVE WED1-YY  TO  PC5-YYW6.
       M10-EXIT.
           GOBACK.
      *----------------------------------------------------------------*
      *    VSBA1250 END                                                *
      *----------------------------------------------------------------*
