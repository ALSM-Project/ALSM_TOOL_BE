./ ADD NAME=VSBA2050  01.08 19961127 20190607 103827 00085 00074 MCSK012
       ID  DIVISION.
       PROGRAM-ID.   'VSBA2050'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM      : SUZUKEN SHITEN SYSTEM            *
      *              <> SUBSYSTEM   : SUBROUTINE                       *
      *              <> PG.OUTLINE  : SHOHIZEIRITU SYUTOKU ROUTINE     *
      *              <> AUTHOR      : CSK-MS.HATTORI                   *
      *              <> DATE        : 96/11/27                         *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-1508            ÍÝº³ : 13/12/11  CSK-T.TAKADA           *
      * (02) K-2163            ÍÝº³ : 19/06/06  CSK-M.MATSUSHIMA       *
      * (03) @@@@@@@@@@@@@@@@   Â²¶ : @@/@@/@@  CSK-@.@@@@@@@@@        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *   WORKING-STORAGE SECTION                                      *
      *----------------------------------------------------------------*
       01  WK-TABLE.
           02 FILLER1  PIC X(21) VALUE '000000001989033100000'.
           02 FILLER2  PIC X(21) VALUE '198904011997033100030'.
           02 FILLER3  PIC X(21) VALUE '199704012014033100050'.
           02 FILLER4  PIC X(21) VALUE '201404012019093000080'.
           02 FILLER5  PIC X(21) VALUE '201910019999999901080'.
           02 FILLER6  PIC X(21) VALUE '201910019999999900100'.
           02 FILLER7  PIC X(21) VALUE '*********************'.
           02 FILLER8  PIC X(21) VALUE '*********************'.
           02 FILLER9  PIC X(21) VALUE '*********************'.
           02 FILLER10 PIC X(21) VALUE '*********************'.
       01  FILLER      REDEFINES WK-TABLE.
           02 WK-TBL OCCURS 10.
              03 WK-STYMD        PIC  X(08).
              03 WK-EDYMD        PIC  X(08).
              03 WK-KZRFG        PIC  X(01).
              03 WK-KGZKB        PIC  X(01).
              03 WK-SHZRT        PIC  9(02)V9.
       01  EDIT-AREA.
           02 WED1-KGZKB                    PIC  X(01).
       01  COUNT-AREA.
           02 I                             PIC S9(04) COMP.
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *   LINKAGE SECTION                                              *
      *----------------------------------------------------------------*
       01  PARAM.
           COPY VPARB205 SUPPRESS.
       PROCEDURE DIVISION USING PARAM.
      *----------------------------------------------------------------*
      *    MAIN-CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-MAIN THRU M1-EXIT.
           GOBACK.
      *----------------------------------------------------------------*
      *    M1 : MAIN                                                   *
      *----------------------------------------------------------------*
       M1-MAIN.
           IF P205-KGZKB = SPACE
              MOVE '0'        TO WED1-KGZKB
           ELSE
              MOVE P205-KGZKB TO WED1-KGZKB.
           PERFORM M11-TBL-SRCH THRU M11-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : SYOUHIZEI RITU TABLE SEARCH                          *
      *----------------------------------------------------------------*
       M11-TBL-SRCH.
           PERFORM VARYING I FROM 1 BY 1
                    UNTIL ( WK-STYMD(I) = '********' )
                       OR ( I > 10 )
              IF ( WK-STYMD(I) NOT > P205-YMDS8-X ) AND
                 ( WK-EDYMD(I) NOT < P205-YMDS8-X ) AND
                   WK-KGZKB(I)     = WED1-KGZKB
                 MOVE WK-SHZRT(I) TO P205-SHZRT
                 IF WK-KZRFG(I) = '1'
                    GO TO M11-EXIT
                 END-IF
              END-IF
           END-PERFORM.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         END : VSBA2050                                         *
      *----------------------------------------------------------------*
