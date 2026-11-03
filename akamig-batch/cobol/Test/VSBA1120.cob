./ ADD NAME=VSBA1120  01.15 20040823 20150902 133913 00109 00080 MCSK082
       ID  DIVISION.
       PROGRAM-ID.     'VSBA1120'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SUZKEN SITEN SYSTEM           *
      *              <> SUBSYSTEM      : SUBROUTINE                    *
      *              <> PG.OUTLINE     : KAISYA NUMBER SYUTOKU         *
      *                                                                *
      *                 * INPUT  PARM  : PB2-HNBCD                     *
      *                                : PB2-BUNO                      *
      *                                : PB2-BUKGO                     *
      *                 * OUTPUT PARM  : PB2-KAINO                     *
      *                                                                *
      *              <> AUTHOR         : SUZ-T.KOGISO                  *
      *              <> DATE           : 91/10/09                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-203              ÍÝº³   : 96/10/03  CSK-M.OOTA          *
      * (02) C-110              ÍÝº³   : 99/07/08  CSK-K.ITO           *
      * (03) K-455              ÍÝº³   : 00/11/14  CSK-C.MIYAWAKI      *
      * (04) K-694               Â²¶   : 04/08/23  CSK-M.EJIRI         *
      * (05) K-848              ÍÝº³   : 05/11/14  CSK-K.OHBA          *
      * (06) K-892              ÍÝº³   : 06/05/22  CSK-K.OHBA          *
      * (07) K-912              ÍÝº³   : 06/09/08  CSK-H.NAKAMURA      *
      * (08) K-1534             ÍÝº³   : 14/04/16  CSK-N.NAGATA        *
      * (09) K-1716             ÍÝº³   : 15/09/02  CSK-K.KOYAMA        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *   LINCAGE SECTION                                              *
      *----------------------------------------------------------------*
       01  PARAM.
           COPY VPARBB2.
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
           IF  PB2-HNBCD = '1'
               IF PB2-BUNO = '471'
                  MOVE '11' TO PB2-KAINO
               ELSE
                  IF PB2-BUNO = '031'
                     MOVE '22' TO PB2-KAINO
                  ELSE
                     IF PB2-BUNO = '091'
                        MOVE '35' TO PB2-KAINO
                     ELSE
                        IF (PB2-BUNO = '341' OR '342' OR '331' OR
                                       '311' OR '351' OR '349') OR
                           (PB2-BUNO >= '700' AND PB2-BUNO <= '749')
                           MOVE '37' TO PB2-KAINO
                        ELSE
                           IF ( PB2-BUNO = '361' OR '371' OR
                                         '381' OR '391' ) OR
                              ( PB2-BUNO >= '650' AND
                                PB2-BUNO <= '699' )
                              MOVE '58' TO PB2-KAINO
                           ELSE
                              IF (PB2-BUNO = '402' OR '403' OR '404' OR
                                   '411' OR '421' OR '432' OR '441' OR
                                   '451' OR '462') OR
                               (PB2-BUNO >= '750' AND
                                PB2-BUNO <= '799')
                                 MOVE '82' TO PB2-KAINO
                              ELSE
                                 MOVE '01' TO PB2-KAINO
           ELSE
               IF PB2-HNBCD = '2'
                  IF PB2-BUKGO = 'À'
                     MOVE '11' TO PB2-KAINO
                  ELSE
                     IF PB2-BUKGO = 'Ã'
                        MOVE '22' TO PB2-KAINO
                     ELSE
                        IF PB2-BUKGO = 'Ë'
                           MOVE '35' TO PB2-KAINO
                        ELSE
                           IF PB2-BUKGO = 'Î' OR 'Ï' OR 'Ð' OR
                                          'Ñ' OR 'Ò' OR 'Ú' OR 'Û'
                              MOVE '37' TO PB2-KAINO
                           ELSE
                              IF PB2-BUKGO = 'Õ' OR 'Ö' OR
                                             '×' OR 'Ø' OR 'W' OR 'X'
                                 MOVE '58' TO PB2-KAINO
                              ELSE
                                 IF PB2-BUKGO = '1' OR '2' OR '3' OR
                                        '4' OR '5' OR '6' OR '7' OR
                                        '8' OR '9' OR '0' OR
                                        'Y' OR 'Z'
                                    MOVE '82' TO PB2-KAINO
                                 ELSE
                                    MOVE '01' TO PB2-KAINO
               ELSE
                  DISPLAY 'R VSBA1120 HANBETSU CODE ERROR PB2-HNBCD = '
                  PB2-HNBCD UPON CONSOLE
                  CALL 'VSBABEND'.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VSBA1120 END                                           *
      *----------------------------------------------------------------*
