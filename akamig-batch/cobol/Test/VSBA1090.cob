./ ADD NAME=VSBA1090  01.01 19920130 19920130 162800 00047 00047 TSO43
       ID  DIVISION.
       PROGRAM-ID.      'VSBA1090'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM      : SUZUKEN 10 SYSTEM                *
      *              <> SUBSYSTEM   : M.MA                             *
      *              <> PG.OUTLINE  : SEIREKI HENKAN                   *
      *                               "SEIREKI 6" => "SEIREKI 8"       *
      *                                                                *
      *                 *  INPUT.PARM  : SEIREKI 6 = YYMMDD            *
      *                 *  OUTPUT.PARM : SEIREKI 8 = YYYYMMDD          *
      *                                                                *
      *                    <NOTE>  SEIREKI 6 YY NOT < 50 / 19YYMMDD    *
      *                            SEIREKI 6 YY < 50 / 20YYMMDD        *
      *                                                                *
      *              <> AUTHOR      : CSK-H.MABUCHI                    *
      *              <> DATE        : 91/09/24                         *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) YOKEN NO.K-541     ÍÝº³   : 92/01/30  SUZ-Y.KAWAMURA      *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *   LINCAGE SECTION                                              *
      *----------------------------------------------------------------*
       01  PARAM.
           COPY VPARBA9.
       PROCEDURE DIVISION USING PARAM.
      *----------------------------------------------------------------*
      *    1.0 : MAIN CONTROL                                          *
      *----------------------------------------------------------------*
       M10-MAIN-CONTROL.
           IF PA9-YMDS6 = ZERO
              MOVE ZERO TO PA9-YMDS8
              GO TO M10-EXIT.
           MOVE PA9-YMDS6 TO PA9-YMD8.
           IF PA9-YYS6 < 50
              MOVE 20 TO PA9-SSS8
           ELSE
              MOVE 19 TO PA9-SSS8.
       M10-EXIT.
           GOBACK.
      *----------------------------------------------------------------*
      *    VSBA1090 END                                                *
      *----------------------------------------------------------------*
