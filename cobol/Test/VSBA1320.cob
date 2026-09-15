./ ADD NAME=VSBA1320  01.00 19980410 19980410 111328 00036 00036 TSO241
       ID  DIVISION.
       PROGRAM-ID.      'VSBA1320'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM      : SUZUKEN 10 SYSTEM                *
      *              <> SUBSYSTEM   : SUBROUTINE                       *
      *              <> PG.OUTLINE  : NENGETUDO SYUTOKU (MATUJIME)     *
      *                                                                *
      *                 *  INPUT.PARM  : P132-DATE   (SEIREKI)         *
      *                 *  OUTPUT.PARM : P132-GATUDO (SEIREKI)         *
      *                                                                *
      *              <> AUTHOR      : CSK-K.KOYAMA                     *
      *              <> DATE        : 91/11/27                         *
      *-----------------  MAINTENANCE HISTORY  ------------------------*
      *   (01) XXXXXX          ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX         *
      *   (02) XXXXXX           Â²¶   : YY/MM/DD  CSK-X.XXXXXX         *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *   LINKAGE SECTION                                              *
      *----------------------------------------------------------------*
       01  PARAM.
           COPY VPARB132.
       PROCEDURE DIVISION USING PARAM.
      *----------------------------------------------------------------*
      *    1.0 : CONTROL                                               *
      *----------------------------------------------------------------*
       M10-CONTROL.
           MOVE  P132-YY1 TO P132-YY2.
           MOVE  P132-MM1 TO P132-MM2.
       M10-EXIT.
           GOBACK.
      *----------------------------------------------------------------*
      *    END : VSBA1320                                              *
      *----------------------------------------------------------------*
