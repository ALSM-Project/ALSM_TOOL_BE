./ ADD NAME=VSBA0350  01.01 19910314 19910314 085500 00181 00180 TSO48
       ID  DIVISION.
       PROGRAM-ID.      'VSBA0350'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM      : SUZUKEN 10 SYSTEM                *
      *              <> SUBSYSTEM   : SUBROUTINE                       *
      *              <> PG.OUTLINE  : SEARCH SOSHIKI CODE HENKAN TABLE *
      *                                                                *
      *                 *  INPUT.PARM1 : HANBETSU-CODE                 *
      *                                          = 1 SITEK + KANO      *
      *                                          = 2 STNNO + KANO      *
      *                                          = 3 HSTNO + KANO      *
      *                                          = 4 SITEK + SOKOK     *
      *                                          = 5 STNNO + SOKON     *
      *                                  O KEY TOSHITE SEARCH SURU     *
      *                                : GAITO KEY                     *
      *                         .PARM2 : SOSIKI HENKAN TABLE           *
      *                 *  OUTPUT.PARM1 : RTNCD = 0 / OK               *
      *                                         = 1 / ERROR            *
      *                                 : SOSHIKI TABLE ENTRY          *
      *              <> AUTHOR      : CSK-T.T                          *
      *              <> DATE        : 85/03/14                         *
      *---------------- MAINTENANCE HISTORY ---------------------------*
      * (1) HANBETSU-CODE '5'   Â²¶        86/08/26   CSK-T.HATANO     *
      * (2) YOUKEN NO.O-169     ÍÝº³       91/03/14   SUZ-M.MATSUURA   *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *   WORKING-STORAGE SECTION                                      *
      *----------------------------------------------------------------*
       01  WK-TABLE.
           02  WK-TBL  OCCURS  150  TIMES  INDEXED  BY  WK-I.
               03  TB-HSTNO    PIC X(03).
               03  TB-KANO     PIC X(03).
               03  TB-STNNO    PIC X(03).
               03  TB-SITEK    PIC X(01).
               03  TB-SOKOK    PIC X(01).
               03  TB-SYOKA    PIC X(01).
               03  TB-SOKON    PIC X(03).
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *   LINKAGE SECTION                                              *
      *----------------------------------------------------------------*
       01  PARAM1.
           COPY VPARB35.
       01  PARAM2.
           COPY VPARB34.
       PROCEDURE DIVISION USING PARAM1 PARAM2.
      *----------------------------------------------------------------*
      *    1.0 : MAIN CONTROL                                          *
      *----------------------------------------------------------------*
       M10-MAIN-CONTROL.
      *
           MOVE  PARAM2  TO  WK-TABLE.
      *
           IF        P35-HNBCD = '1'
               PERFORM  M11-SEARCH    THRU  M11-EXIT
           ELSE  IF  P35-HNBCD = '2'
               PERFORM  M12-SEARCH    THRU  M12-EXIT
           ELSE  IF  P35-HNBCD = '3'
               PERFORM  M13-SEARCH    THRU  M13-EXIT
           ELSE  IF  P35-HNBCD = '4'
               PERFORM  M14-SEARCH    THRU  M14-EXIT
           ELSE  IF  P35-HNBCD = '5'
               PERFORM  M15-SEARCH    THRU  M15-EXIT
           ELSE
               MOVE   1   TO  P35-RTNCD.
      *
       M10-EXIT.
           GOBACK.
      *----------------------------------------------------------------*
      *    1.1 : SEARCH SOSHIKI HENKAN TABLE KEY = SITEK & KANO        *
      *----------------------------------------------------------------*
       M11-SEARCH.
           MOVE  ZERO TO P35-RTNCD.
           SET  WK-I TO 1.
           SEARCH  WK-TBL
               AT END
                   MOVE  1  TO  P35-RTNCD
               WHEN   TB-SITEK (WK-I) = '***'
                   MOVE  1  TO  P35-RTNCD
               WHEN   P35-SITEK = TB-SITEK (WK-I)   AND
                      P35-KANO  = TB-KANO (WK-I)
                   MOVE  TB-HSTNO (WK-I) TO P35-HSTNO
                   MOVE  TB-KANO  (WK-I) TO P35-KANO
                   MOVE  TB-STNNO (WK-I) TO P35-STNNO
                   MOVE  TB-SITEK (WK-I) TO P35-SITEK
                   MOVE  TB-SOKOK (WK-I) TO P35-SOKOK
                   MOVE  TB-SYOKA (WK-I) TO P35-SYOKA
                   MOVE  TB-SOKON (WK-I) TO P35-SOKON.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    1.2 : SEARCH SOSHIKI HENKAN TABLE KEY = STNNO & KANO        *
      *----------------------------------------------------------------*
       M12-SEARCH.
           MOVE  ZERO TO P35-RTNCD.
           SET  WK-I TO 1.
           SEARCH  WK-TBL
               AT END
                   MOVE  1  TO  P35-RTNCD
               WHEN   TB-STNNO (WK-I) = '***'
                   MOVE  1  TO  P35-RTNCD
               WHEN   P35-STNNO = TB-STNNO (WK-I)   AND
                      P35-KANO  = TB-KANO (WK-I)
                   MOVE  TB-HSTNO (WK-I) TO P35-HSTNO
                   MOVE  TB-KANO  (WK-I) TO P35-KANO
                   MOVE  TB-STNNO (WK-I) TO P35-STNNO
                   MOVE  TB-SITEK (WK-I) TO P35-SITEK
                   MOVE  TB-SOKOK (WK-I) TO P35-SOKOK
                   MOVE  TB-SYOKA (WK-I) TO P35-SYOKA
                   MOVE  TB-SOKON (WK-I) TO P35-SOKON.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    1.3 : SEARCH SOSHIKI HENKAN TABLE KEY = HSTNO & KANO        *
      *----------------------------------------------------------------*
       M13-SEARCH.
           MOVE  ZERO TO P35-RTNCD.
           SET  WK-I TO 1.
           SEARCH  WK-TBL
               AT END
                   MOVE  1  TO  P35-RTNCD
               WHEN   TB-HSTNO (WK-I) = '***'
                   MOVE  1  TO  P35-RTNCD
               WHEN   P35-HSTNO = TB-HSTNO (WK-I)   AND
                      P35-KANO  = TB-KANO (WK-I)
                   MOVE  TB-HSTNO (WK-I) TO P35-HSTNO
                   MOVE  TB-KANO  (WK-I) TO P35-KANO
                   MOVE  TB-STNNO (WK-I) TO P35-STNNO
                   MOVE  TB-SITEK (WK-I) TO P35-SITEK
                   MOVE  TB-SOKOK (WK-I) TO P35-SOKOK
                   MOVE  TB-SYOKA (WK-I) TO P35-SYOKA
                   MOVE  TB-SOKON (WK-I) TO P35-SOKON.
       M13-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    1.4 : SEARCH SOSHIKI HENKAN TABLE KEY = SITEK & SOKOK       *
      *----------------------------------------------------------------*
       M14-SEARCH.
           MOVE  ZERO TO P35-RTNCD.
           SET  WK-I TO 1.
           SEARCH  WK-TBL
               AT END
                   MOVE  1  TO  P35-RTNCD
               WHEN   TB-SITEK (WK-I) = '***'
                   MOVE  1  TO  P35-RTNCD
               WHEN   P35-SITEK = TB-SITEK (WK-I)   AND
                      P35-SOKOK = TB-SOKOK (WK-I)
                   MOVE  TB-HSTNO (WK-I) TO P35-HSTNO
                   MOVE  TB-KANO  (WK-I) TO P35-KANO
                   MOVE  TB-STNNO (WK-I) TO P35-STNNO
                   MOVE  TB-SITEK (WK-I) TO P35-SITEK
                   MOVE  TB-SOKOK (WK-I) TO P35-SOKOK
                   MOVE  TB-SYOKA (WK-I) TO P35-SYOKA
                   MOVE  TB-SOKON (WK-I) TO P35-SOKON.
       M14-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    1.5 : SEARCH SOSHIKI HENKAN TABLE KEY = STNNO & SOKON       *
      *----------------------------------------------------------------*
       M15-SEARCH.
           MOVE  ZERO TO P35-RTNCD.
           SET  WK-I TO 1.
           SEARCH  WK-TBL
               AT END
                   MOVE  1  TO  P35-RTNCD
               WHEN   TB-SITEK (WK-I) = '***'
                   MOVE  1  TO  P35-RTNCD
               WHEN   P35-STNNO = TB-STNNO (WK-I)   AND
                      P35-SOKON = TB-SOKON (WK-I)
                   MOVE  TB-HSTNO (WK-I) TO P35-HSTNO
                   MOVE  TB-KANO  (WK-I) TO P35-KANO
                   MOVE  TB-STNNO (WK-I) TO P35-STNNO
                   MOVE  TB-SITEK (WK-I) TO P35-SITEK
                   MOVE  TB-SOKOK (WK-I) TO P35-SOKOK
                   MOVE  TB-SYOKA (WK-I) TO P35-SYOKA
                   MOVE  TB-SOKON (WK-I) TO P35-SOKON.
       M15-EXIT.
           EXIT.
