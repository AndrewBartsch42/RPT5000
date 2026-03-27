       IDENTIFICATION DIVISION.                                         00010001
                                                                        00020001
       PROGRAM-ID. RPT5000.                                             00030001
                                                                        00040001
       ENVIRONMENT DIVISION.                                            00050001
                                                                        00060001
       INPUT-OUTPUT SECTION.                                            00070001
                                                                        00080001
       FILE-CONTROL.                                                    00090001
           SELECT CUSTMAST ASSIGN TO CUSTMAST.                          00100001
           SELECT SALESRPT ASSIGN TO RPOT5000.                          00110001
                                                                        00120001
       DATA DIVISION.                                                   00130001
                                                                        00140001
       FILE SECTION.                                                    00150001
                                                                        00160001
       FD  CUSTMAST                                                     00170001
           RECORDING MODE IS F                                          00180001
           LABEL RECORDS ARE STANDARD                                   00190001
           RECORD CONTAINS 130 CHARACTERS                               00200001
           BLOCK CONTAINS 130 CHARACTERS.                               00210001
       01  CUSTOMER-MASTER-RECORD.                                      00220001
           05  CM-BRANCH-NUMBER        PIC 9(2).                        00230001
           05  CM-SALESREP-NUMBER      PIC 9(2).                        00240001
           05  CM-CUSTOMER-NUMBER      PIC 9(5).                        00250001
           05  CM-CUSTOMER-NAME        PIC X(20).                       00260001
           05  CM-SALES-THIS-YTD       PIC S9(5)V9(2).                  00270001
           05  CM-SALES-LAST-YTD       PIC S9(5)V9(2).                  00280001
                                                                        00290001
       FD  SALESRPT                                                     00300001
           RECORDING MODE IS F                                          00310001
           LABEL RECORDS ARE STANDARD                                   00320001
           RECORD CONTAINS 130 CHARACTERS                               00330001
           BLOCK CONTAINS 130 CHARACTERS.                               00340001
       01  PRINT-AREA      PIC X(130).                                  00350001
                                                                        00360001
       WORKING-STORAGE SECTION.                                         00370001
                                                                        00380001
                                                                        00390001
       01 CALCULATED-FIELDS.                                            00400001
          05 CHANGE-AMOUNT             PIC S9(5)V99.                    00410001
       01  SWITCHES.                                                    00420001
           05  CUSTMAST-EOF-SWITCH     PIC X    VALUE "N".              00430001
               88  CUSTMAST-EOF                 VALUE "Y".              00440004
           05  FIRST-RECORD-SWITCH     PIC X    VALUE "Y".              00450001
               88  FIRST-RECORD                 VALUE "Y".              00460004
                                                                        00470001
       01  CONTROL-FIELDS.                                              00480001
           05  OLD-SALESREP-NUMBER     PIC 99.                          00490001
           05  OLD-BRANCH-NUMBER       PIC 99.                          00500001
                                                                        00510001
       01  PRINT-FIELDS.                                                00520001
           05  PAGE-COUNT      PIC S9(3)   VALUE ZERO.                  00530001
           05  LINES-ON-PAGE   PIC S9(3)   VALUE +55.                   00540001
           05  LINE-COUNT      PIC S9(3)   VALUE +99.                   00550001
           05  SPACE-CONTROL   PIC S9.                                  00560001
                                                                        00570001
       01  TOTAL-FIELDS.                                                00580001
           05  SALESREP-TOTAL-THIS-YTD PIC S9(6)V99  VALUE ZERO.        00590001
           05  SALESREP-TOTAL-LAST-YTD PIC S9(6)V99  VALUE ZERO.        00600001
           05  BRANCH-TOTAL-THIS-YTD  PIC S9(6)V99   VALUE ZERO.        00610001
           05  BRANCH-TOTAL-LAST-YTD  PIC S9(6)V99   VALUE ZERO.        00620001
           05  GRAND-TOTAL-THIS-YTD   PIC S9(7)V99   VALUE ZERO.        00630001
           05  GRAND-TOTAL-LAST-YTD   PIC S9(7)V99   VALUE ZERO.        00640001
                                                                        00650001
       01  CURRENT-DATE-AND-TIME.                                       00660001
           05  CD-YEAR         PIC 9999.                                00670001
           05  CD-MONTH        PIC 99.                                  00680001
           05  CD-DAY          PIC 99.                                  00690001
           05  CD-HOURS        PIC 99.                                  00700001
           05  CD-MINUTES      PIC 99.                                  00710001
           05  FILLER          PIC X(9).                                00720001
                                                                        00730001
       01  HEADING-LINE-1.                                              00740001
           05  FILLER          PIC X(7)    VALUE "DATE:  ".             00750001
           05  HL1-MONTH       PIC 9(2).                                00760001
           05  FILLER          PIC X(1)    VALUE "/".                   00770001
           05  HL1-DAY         PIC 9(2).                                00780001
           05  FILLER          PIC X(1)    VALUE "/".                   00790001
           05  HL1-YEAR        PIC 9(4).                                00800001
           05  FILLER          PIC X(16)   VALUE SPACE.                 00810001
           05  FILLER          PIC X(20)   VALUE "YEAR-TO-DATE SALES R".00820001
           05  FILLER          PIC X(5)    VALUE "EPORT".               00830001
           05  FILLER          PIC X(22)   VALUE SPACE.                 00840001
           05  FILLER          PIC X(8)    VALUE "  PAGE: ".            00850001
           05  Hl1-PAGE-NUMBER PIC ZZZ9.                                00860001
           05  FILLER          PIC X(52)   VALUE SPACE.                 00870001
                                                                        00880001
       01  HEADING-LINE-2.                                              00890001
           05  FILLER          PIC X(7)    VALUE "TIME:  ".             00900001
           05  HL2-HOURS       PIC 9(2).                                00910001
           05  FILLER          PIC X(1)    VALUE ":".                   00920001
           05  HL2-MINUTES     PIC 9(2).                                00930001
           05  FILLER          PIC X(70)   VALUE SPACE.                 00940001
           05  FILLER          PIC X(10)   VALUE "RPT5000".             00950001
           05  FILLER          PIC X(52)   VALUE SPACE.                 00960001
                                                                        00970001
                                                                        00980001
       01  HEADING-LINE-3.                                              00990001
           05  FILLER      PIC X(13)   VALUE "BRANCH SALES ".           01000001
           05  FILLER      PIC X(4)    VALUE "CUST".                    01010001
           05  FILLER      PIC X(28)   VALUE SPACE.                     01020001
           05  FILLER      PIC X(5)    VALUE "SALES".                   01030001
           05  FILLER      PIC X(9)    VALUE SPACE.                     01040001
           05  FILLER      PIC X(5)    VALUE "SALES".                   01050001
           05  FILLER      PIC X(9)    VALUE SPACE.                     01060001
           05  FILLER      PIC X(20)   VALUE "CHANGE     CHANGE   ".    01070001
           05  FILLER      PIC X(39)   VALUE SPACE.                     01080001
                                                                        01090001
       01  HEADING-LINE-4.                                              01100001
           05  FILLER      PIC X(13)   VALUE " NUM    REP  ".           01110001
           05  FILLER      PIC X(20)   VALUE "NUM    CUSTOMER NAME".    01120001
           05  FILLER      PIC X(20)   VALUE "           THIS YTD ".    01130001
           05  FILLER      PIC X(20)   VALUE "     LAST YTD       ".    01140001
           05  FILLER      PIC X(20)   VALUE "AMOUNT    PERCENT   ".    01150001
           05  FILLER      PIC X(39)   VALUE SPACE.                     01160001
                                                                        01170001
       01  HEADING-LINE-6.                                              01180001
                                                                        01190001
           05  FILLER              PIC X(6)     VALUE ALL '-'.          01200001
           05  FILLER              PIC X(1)     VALUE SPACE.            01210001
           05  FILLER              PIC X(5)     VALUE ALL '-'.          01220001
           05  FILLER              PIC X(1)     VALUE SPACE.            01230001
           05  FILLER              PIC X(5)     VALUE ALL '-'.          01240001
           05  FILLER              PIC X(2)     VALUE SPACE.            01250001
           05  FILLER              PIC X(20)    VALUE ALL '-'.          01260001
           05  FILLER              PIC X(3)     VALUE SPACE.            01270001
           05  FILLER              PIC X(10)    VALUE ALL '-'.          01280001
           05  FILLER              PIC X(4)     VALUE SPACE.            01290001
           05  FILLER              PIC X(10)    VALUE ALL '-'.          01300001
           05  FILLER              PIC X(4)     VALUE SPACE.            01310001
           05  FILLER              PIC X(10)    VALUE ALL '-'.          01320001
           05  FILLER              PIC X(3)     VALUE SPACE.            01330001
           05  FILLER              PIC X(6)     VALUE ALL '-'.          01340001
                                                                        01350001
       01  CUSTOMER-LINE.                                               01360001
                                                                        01370001
           05  FILLER              PIC X(2)     VALUE SPACE.            01380001
           05  CL-BRANCH-NUMBER    PIC X(2).                            01390001
           05  FILLER              PIC X(4)     VALUE SPACE.            01400001
           05  CL-SALESREP-NUMBER  PIC X(2).                            01410001
           05  FILLER              PIC X(3)     VALUE SPACE.            01420001
           05  CL-CUSTOMER-NUMBER  PIC 9(5).                            01430001
           05  FILLER              PIC X(3)     VALUE SPACE.            01440001
           05  CL-CUSTOMER-NAME    PIC X(20).                           01450001
           05  FILLER              PIC X(3)     VALUE SPACE.            01460001
           05  CL-SALES-THIS-YTD   PIC ZZ,ZZ9.99-.                      01470001
           05  FILLER              PIC X(4)     VALUE SPACE.            01480001
           05  CL-SALES-LAST-YTD   PIC ZZ,ZZ9.99-.                      01490001
           05  FILLER              PIC X(4)     VALUE SPACE.            01500001
           05  CL-CHANGE-AMOUNT    PIC ZZ,ZZ9.99-.                      01510001
           05  FILLER              PIC X(3)     VALUE SPACE.            01520001
           05  CL-CHANGE-PERCENT   PIC ZZ9.9-.                          01530001
           05  FILLER              PIC X(43)    VALUE SPACE.            01540001
                                                                        01550001
       01  GRAND-TOTAL-HEADER.                                          01560001
           05  FILLER              PIC X(41)    VALUE SPACE.            01570001
           05  FILLER              PIC X(13)    VALUE ALL '='.          01580001
           05  FILLER              PIC X(1)     VALUE SPACE.            01590001
           05  FILLER              PIC X(13)    VALUE ALL '='.          01600001
           05  FILLER              PIC X(1)     VALUE SPACE.            01610001
           05  FILLER              PIC X(13)    VALUE ALL '='.          01620001
           05  FILLER              PIC X(3)     VALUE SPACE.            01630001
           05  FILLER              PIC X(6)     VALUE ALL '='.          01640001
                                                                        01650001
       01  SALESREP-FILLER-LINE.                                        01660001
           05  FILLER              PIC X(41)    VALUE SPACE.            01670001
           05  FILLER              PIC X(13)    VALUE ALL '-'.          01680001
           05  FILLER              PIC X(1)     VALUE SPACE.            01690001
           05  FILLER              PIC X(13)    VALUE ALL '-'.          01700001
           05  FILLER              PIC X(1)     VALUE SPACE.            01710001
           05  FILLER              PIC X(13)    VALUE ALL '-'.          01720001
           05  FILLER              PIC X(3)     VALUE SPACE.            01730001
           05  FILLER              PIC X(6)     VALUE ALL '-'.          01740001
                                                                        01750001
       01  SALESREP-TOTAL-LINE.                                         01760001
           05  FILLER              PIC X(29)    VALUE SPACE.            01770001
           05  FILLER              PIC X(14)    VALUE "SALES TOTAL".    01780001
           05  STL-SALES-THIS-YTD  PIC zzz,zz9.99-.                     01790001
           05  FILLER              PIC X(3)    VALUE SPACE.             01800001
           05  STL-SALES-LAST-YTD  PIC zzz,zz9.99-.                     01810001
           05  FILLER              PIC X(3)    VALUE SPACE.             01820001
           05  STL-CHANGE-AMOUNT   PIC zzz,zz9.99-.                     01830001
           05  FILLER              PIC X(3)    VALUE SPACE.             01840001
           05  STL-CHANGE-PERCENT  PIC ZZ9.9-.                          01850001
           05  FILLER              PIC X(47)   VALUE "*".               01860001
                                                                        01870001
       01  BRANCH-FILLER-LINE.                                          01880001
           05  FILLER              PIC X(41)    VALUE SPACE.            01890001
           05  FILLER              PIC X(13)    VALUE ALL '-'.          01900001
           05  FILLER              PIC X(1)     VALUE SPACE.            01910001
           05  FILLER              PIC X(13)    VALUE ALL '-'.          01920001
           05  FILLER              PIC X(1)     VALUE SPACE.            01930001
           05  FILLER              PIC X(13)    VALUE ALL '-'.          01940001
           05  FILLER              PIC X(3)     VALUE SPACE.            01950001
           05  FILLER              PIC X(6)     VALUE ALL '-'.          01960001
                                                                        01970001
       01  BRANCH-TOTAL-LINE.                                           01980001
           05  FILLER              PIC X(29)    VALUE SPACE.            01990001
           05  FILLER              PIC X(14)    VALUE "BRANCH TOTAL".   02000001
           05  BTL-SALES-THIS-YTD  PIC zzz,zz9.99-.                     02010001
           05  FILLER              PIC X(3)    VALUE SPACE.             02020001
           05  BTL-SALES-LAST-YTD  PIC zzz,zz9.99-.                     02030001
           05  FILLER              PIC X(3)    VALUE SPACE.             02040001
           05  BTL-CHANGE-AMOUNT   PIC zzz,zz9.99-.                     02050001
           05  FILLER              PIC X(3)    VALUE SPACE.             02060001
           05  BTL-CHANGE-PERCENT  PIC ZZ9.9-.                          02070001
           05  FILLER              PIC X(47)   VALUE "*".               02080001
                                                                        02090001
       01  GRAND-TOTAL-LINE.                                            02100001
           05  FILLER              PIC X(29)    VALUE SPACE.            02110001
           05  FILLER              PIC X(12)    VALUE "GRAND TOTAL".    02120001
           05  GTL-SALES-THIS-YTD  PIC Z,ZZZ,ZZ9.99-.                   02130001
           05  FILLER              PIC X(1)     VALUE SPACE.            02140001
           05  GTL-SALES-LAST-YTD  PIC Z,ZZZ,ZZ9.99-.                   02150001
           05  FILLER              PIC X(1)     VALUE SPACE.            02160001
           05  GTL-CHANGE-AMOUNT   PIC Z,ZZZ,ZZ9.99-.                   02170001
           05  FILLER              PIC X(3)     VALUE SPACE.            02180001
           05  GTL-CHANGE-PERCENT  PIC ZZ9.9-.                          02190001
           05  FILLER              PIC X(47)    VALUE " **".            02200001
                                                                        02210001
       PROCEDURE DIVISION.                                              02220001
                                                                        02230001
       000-PREPARE-SALES-REPORT.                                        02240001
                                                                        02250001
           OPEN INPUT  CUSTMAST                                         02260001
                OUTPUT SALESRPT.                                        02270001
           PERFORM 100-FORMAT-REPORT-HEADING.                           02280001
           PERFORM 300-PREPARE-SALES-LINES                              02290001
               UNTIL CUSTMAST-EOF-SWITCH = "Y".                         02300001
           PERFORM 500-PRINT-GRAND-TOTALS.                              02310001
           CLOSE CUSTMAST                                               02320001
                 SALESRPT.                                              02330001
           STOP RUN.                                                    02340001
                                                                        02350001
       100-FORMAT-REPORT-HEADING.                                       02360001
                                                                        02370001
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.         02380001
           MOVE CD-MONTH   TO HL1-MONTH.                                02390001
           MOVE CD-DAY     TO HL1-DAY.                                  02400001
           MOVE CD-YEAR    TO HL1-YEAR.                                 02410001
           MOVE CD-HOURS   TO HL2-HOURS.                                02420001
           MOVE CD-MINUTES TO HL2-MINUTES.                              02430001
                                                                        02440001
                                                                        02450001
       300-PREPARE-SALES-LINES.                                         02460001
           PERFORM 310-READ-CUSTOMER-RECORD                             02470004
           EVALUATE TRUE                                                02480004
               WHEN CUSTMAST-EOF                                        02490003
                PERFORM 355-PRINT-SALES-REP-LINE                        02500004
                PERFORM 360-PRINT-BRANCH-LINE                           02510004
               WHEN FIRST-RECORD                                        02520003
                PERFORM 320-PRINT-CUSTOMER-LINE                         02530004
                MOVE "N" TO FIRST-RECORD-SWITCH                         02540004
                MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER          02550004
                MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER              02560004
               WHEN CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER                02570003
                PERFORM 355-PRINT-SALES-REP-LINE                        02580004
                PERFORM 360-PRINT-BRANCH-LINE                           02590004
                PERFORM 320-PRINT-CUSTOMER-LINE                         02600004
                MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER          02610004
                MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER              02620004
               WHEN CM-SALESREP-NUMBER > OLD-SALESREP-NUMBER            02630003
                PERFORM 355-PRINT-SALES-REP-LINE                        02640004
                PERFORM 320-PRINT-CUSTOMER-LINE                         02650004
                MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER          02660004
               WHEN OTHER                                               02670003
                PERFORM 320-PRINT-CUSTOMER-LINE                         02680001
           END-EVALUATE.                                                02690004
                                                                        02700001
       310-READ-CUSTOMER-RECORD.                                        02710001
                                                                        02720001
           READ CUSTMAST                                                02730001
               AT END                                                   02740001
                   MOVE "Y" TO CUSTMAST-EOF-SWITCH.                     02750001
                                                                        02760001
       320-PRINT-CUSTOMER-LINE.                                         02770001
                                                                        02780001
           IF LINE-COUNT > LINES-ON-PAGE                                02790001
               PERFORM 330-PRINT-HEADING-LINES.                         02800001
           IF FIRST-RECORD-SWITCH = "Y"                                 02810001
               MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER                02820001
           ELSE                                                         02830001
               IF CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER                  02840001
                   MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER            02850001
               ELSE                                                     02860001
                   MOVE SPACE TO CL-BRANCH-NUMBER.                      02870001
           MOVE CM-CUSTOMER-NUMBER TO CL-CUSTOMER-NUMBER.               02880001
           MOVE CM-SALESREP-NUMBER TO CL-SALESREP-NUMBER.               02890001
           MOVE CM-CUSTOMER-NAME TO CL-CUSTOMER-NAME.                   02900001
           MOVE CM-SALES-THIS-YTD TO CL-SALES-THIS-YTD.                 02910001
           ADD CM-SALES-THIS-YTD TO SALESREP-TOTAL-THIS-YTD             02911005
           END-ADD.                                                     02912005
           ADD CM-SALES-LAST-YTD TO SALESREP-TOTAL-LAST-YTD             02913005
           END-ADD.                                                     02914005
           MOVE CM-SALES-LAST-YTD TO CL-SALES-LAST-YTD.                 02920001
           COMPUTE CHANGE-AMOUNT =                                      02930001
               CM-SALES-THIS-YTD - CM-SALES-LAST-YTD.                   02940001
           MOVE CHANGE-AMOUNT TO CL-CHANGE-AMOUNT.                      02950001
           IF CM-SALES-LAST-YTD = ZERO                                  02960001
               MOVE 999.9 TO CL-CHANGE-PERCENT                          02970001
           ELSE                                                         02980001
               COMPUTE CL-CHANGE-PERCENT ROUNDED =                      02990001
                   CHANGE-AMOUNT * 100 / CM-SALES-LAST-YTD              03000001
                   ON SIZE ERROR                                        03010001
                       MOVE 999.9 TO CL-CHANGE-PERCENT.                 03020001
           MOVE CUSTOMER-LINE TO PRINT-AREA.                            03030001
           PERFORM 350-WRITE-REPORT-LINE.                               03040001
           MOVE 1 TO SPACE-CONTROL.                                     03050001
           ADD CM-SALES-THIS-YTD TO BRANCH-TOTAL-THIS-YTD.              03060001
           ADD CM-SALES-LAST-YTD TO BRANCH-TOTAL-LAST-YTD.              03070001
                                                                        03080001
                                                                        03090001
       330-PRINT-HEADING-LINES.                                         03100001
           ADD 1 TO PAGE-COUNT.                                         03110001
           MOVE PAGE-COUNT TO Hl1-PAGE-NUMBER.                          03120001
           MOVE HEADING-LINE-1 TO PRINT-AREA.                           03130001
           PERFORM 340-WRITE-PAGE-TOP-LINE.                             03140001
           MOVE HEADING-LINE-2 TO PRINT-AREA.                           03150001
           MOVE 1 TO SPACE-CONTROL.                                     03160001
           PERFORM 350-WRITE-REPORT-LINE.                               03170001
           MOVE HEADING-LINE-3 TO PRINT-AREA.                           03180001
           MOVE 2 TO SPACE-CONTROL.                                     03190001
           PERFORM 350-WRITE-REPORT-LINE.                               03200001
           MOVE HEADING-LINE-4 TO PRINT-AREA.                           03210001
           MOVE 1 TO SPACE-CONTROL.                                     03220001
           PERFORM 350-WRITE-REPORT-LINE.                               03230001
           MOVE ZERO TO LINE-COUNT.                                     03240001
           MOVE 2 TO SPACE-CONTROL.                                     03250001
                                                                        03260001
       340-WRITE-PAGE-TOP-LINE.                                         03270001
                                                                        03280001
           WRITE PRINT-AREA.                                            03290001
           MOVE 1 TO LINE-COUNT.                                        03300001
                                                                        03310001
       350-WRITE-REPORT-LINE.                                           03320001
                                                                        03330001
           WRITE PRINT-AREA.                                            03340001
                                                                        03350004
                                                                        03360001
                                                                        03370001
       355-PRINT-SALES-REP-LINE.                                        03380001
           MOVE SALESREP-TOTAL-THIS-YTD TO STL-SALES-THIS-YTD.          03390001
           MOVE SALESREP-TOTAL-LAST-YTD TO STL-SALES-LAST-YTD.          03400001
           COMPUTE CHANGE-AMOUNT =                                      03410001
               SALESREP-TOTAL-THIS-YTD - SALESREP-TOTAL-LAST-YTD.       03420001
           MOVE CHANGE-AMOUNT TO STL-CHANGE-AMOUNT.                     03430001
           IF SALESREP-TOTAL-LAST-YTD = ZERO                            03440001
               MOVE 999.9 TO STL-CHANGE-PERCENT                         03450001
           ELSE                                                         03460001
               COMPUTE STL-CHANGE-PERCENT ROUNDED =                     03470001
                   CHANGE-AMOUNT * 100 / SALESREP-TOTAL-LAST-YTD        03480001
                   ON SIZE ERROR                                        03490001
                       MOVE 999.9 TO STL-CHANGE-PERCENT.                03500001
           MOVE SALESREP-FILLER-LINE TO PRINT-AREA.                     03510001
           PERFORM 350-WRITE-REPORT-LINE.                               03520001
           MOVE SALESREP-TOTAL-LINE TO PRINT-AREA.                      03530001
           PERFORM 350-WRITE-REPORT-LINE.                               03540001
           MOVE ZERO TO SALESREP-TOTAL-LAST-YTD.                        03550005
           MOVE ZERO TO SALESREP-TOTAL-THIS-YTD.                        03560005
                                                                        03570001
       360-PRINT-BRANCH-LINE.                                           03580001
                                                                        03590001
           MOVE BRANCH-TOTAL-THIS-YTD TO BTL-SALES-THIS-YTD.            03600001
           MOVE BRANCH-TOTAL-LAST-YTD TO BTL-SALES-LAST-YTD.            03610001
           COMPUTE CHANGE-AMOUNT =                                      03620001
               BRANCH-TOTAL-THIS-YTD - BRANCH-TOTAL-LAST-YTD.           03630001
           MOVE CHANGE-AMOUNT TO BTL-CHANGE-AMOUNT.                     03640001
           IF BRANCH-TOTAL-LAST-YTD = ZERO                              03650001
               MOVE 999.9 TO BTL-CHANGE-PERCENT                         03660001
           ELSE                                                         03670001
               COMPUTE BTL-CHANGE-PERCENT ROUNDED =                     03680001
                   CHANGE-AMOUNT * 100 / BRANCH-TOTAL-LAST-YTD          03690001
                   ON SIZE ERROR                                        03700001
                       MOVE 999.9 TO BTL-CHANGE-PERCENT.                03710001
           MOVE BRANCH-FILLER-LINE TO PRINT-AREA.                       03720001
           PERFORM 350-WRITE-REPORT-LINE.                               03730001
           MOVE BRANCH-TOTAL-LINE TO PRINT-AREA.                        03740001
           PERFORM 350-WRITE-REPORT-LINE.                               03750001
           MOVE SPACES TO PRINT-AREA.                                   03760001
           PERFORM 350-WRITE-REPORT-LINE.                               03770001
           ADD BRANCH-TOTAL-THIS-YTD TO GRAND-TOTAL-THIS-YTD.           03780001
           ADD BRANCH-TOTAL-LAST-YTD TO GRAND-TOTAL-LAST-YTD.           03790001
                                                                        03800001
                                                                        03810001
       500-PRINT-GRAND-TOTALS.                                          03820001
                                                                        03830001
           MOVE GRAND-TOTAL-THIS-YTD TO GTL-SALES-THIS-YTD.             03840001
           MOVE GRAND-TOTAL-LAST-YTD TO GTL-SALES-LAST-YTD.             03850001
           COMPUTE CHANGE-AMOUNT =                                      03860001
               GRAND-TOTAL-THIS-YTD - GRAND-TOTAL-LAST-YTD.             03870001
           MOVE CHANGE-AMOUNT TO GTL-CHANGE-AMOUNT.                     03880001
           IF GRAND-TOTAL-LAST-YTD = ZERO                               03890001
               MOVE 999.9 TO GTL-CHANGE-PERCENT                         03900001
           ELSE                                                         03910001
               COMPUTE GTL-CHANGE-PERCENT ROUNDED =                     03920001
                   CHANGE-AMOUNT * 100 / GRAND-TOTAL-LAST-YTD           03930001
                   ON SIZE ERROR                                        03940001
                       MOVE 999.9 TO GTL-CHANGE-PERCENT.                03950001
           MOVE GRAND-TOTAL-HEADER TO PRINT-AREA.                       03960001
           PERFORM 350-WRITE-REPORT-LINE.                               03970001
           MOVE GRAND-TOTAL-LINE TO PRINT-AREA.                         03980001
           PERFORM 350-WRITE-REPORT-LINE.                               03990001
                                                                        04000001
