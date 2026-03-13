       IDENTIFICATION DIVISION.
       PROGRAM-ID. WCSUBRO.
      *================================================================*
      * PROGRAM:    WCSUBRO                                            *
      * AUTHOR:     MAINFRAMEMOD TPA SYSTEMS                           *
      * DATE:       2024-04-05                                         *
      * PURPOSE:    SUBROGATION RECOVERY TRACKING AND CALCULATION      *
      *             THIRD-PARTY LIABILITY, LIEN CALC, SETTLEMENT       *
      *             NEGOTIATION, ATTORNEY ALLOCATION, NET RECOVERY     *
      * PLATFORM:   HPE NONSTOP / GUARDIAN                             *
      *================================================================*
      * CHANGE LOG:                                                    *
      * 2024-04-05  INITIAL DEVELOPMENT                                *
      * 2024-06-18  ADDED MADE-WHOLE DOCTRINE BY JURISDICTION          *
      * 2024-08-30  ADDED MULTI-PARTY RECOVERY TRACKING                *
      * 2024-11-15  ENHANCED ATTORNEY FEE ALLOCATION LOGIC             *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLAIM-FILE
               ASSIGN TO CLMFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CF-CLAIM-NUM
               FILE STATUS IS WS-CLM-FILE-STATUS.
           SELECT SUBRO-FILE
               ASSIGN TO SUBFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS SF-SUBRO-KEY
               FILE STATUS IS WS-SUB-FILE-STATUS.
           SELECT RECOVERY-REPORT-FILE
               ASSIGN TO RECRPT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-RPT-FILE-STATUS.
           SELECT PAYMENT-FILE
               ASSIGN TO PYMTFL
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-PMT-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CLAIM-FILE.
       01  CLAIM-RECORD.
           05  CF-CLAIM-NUM           PIC X(20).
           05  CF-CLAIMANT-NAME       PIC X(60).
           05  CF-JURISDICTION        PIC X(2).
           05  CF-INJURY-DATE         PIC X(8).
           05  CF-CLAIM-STATUS        PIC X(2).
           05  CF-TOTAL-PAID-MED      PIC S9(9)V99 COMP-3.
           05  CF-TOTAL-PAID-IND      PIC S9(9)V99 COMP-3.
           05  CF-TOTAL-RESERVED-MED  PIC S9(9)V99 COMP-3.
           05  CF-TOTAL-RESERVED-IND  PIC S9(9)V99 COMP-3.
           05  CF-TOTAL-EXPENSE       PIC S9(7)V99 COMP-3.
           05  CF-SUBRO-FLAG          PIC X.
               88  CF-HAS-SUBRO       VALUE 'Y'.
               88  CF-NO-SUBRO        VALUE 'N'.
           05  CF-EMPLOYER-NAME       PIC X(60).
           05  CF-POLICY-NUM          PIC X(20).
           05  FILLER                 PIC X(50).

       FD  SUBRO-FILE.
       01  SUBRO-RECORD.
           05  SF-SUBRO-KEY.
               10  SF-CLAIM-NUM       PIC X(20).
               10  SF-PARTY-SEQ       PIC 99.
           05  SF-PARTY-NAME          PIC X(60).
           05  SF-PARTY-TYPE          PIC X(2).
               88  PARTY-INDIVIDUAL   VALUE 'IN'.
               88  PARTY-EMPLOYER     VALUE 'EM'.
               88  PARTY-INSURER      VALUE 'IS'.
               88  PARTY-GOVERNMENT   VALUE 'GV'.
           05  SF-LIABILITY-PCT       PIC 9V99 COMP-3.
           05  SF-STATUS              PIC X(2).
               88  SUBRO-IDENTIFIED   VALUE 'ID'.
               88  SUBRO-LIEN-FILED   VALUE 'LF'.
               88  SUBRO-NEGOTIATING  VALUE 'NG'.
               88  SUBRO-SETTLED      VALUE 'ST'.
               88  SUBRO-COLLECTED    VALUE 'CO'.
               88  SUBRO-ABANDONED    VALUE 'AB'.
           05  SF-LIEN-AMOUNT         PIC S9(9)V99 COMP-3.
           05  SF-SETTLEMENT-AMOUNT   PIC S9(9)V99 COMP-3.
           05  SF-RECOVERY-AMOUNT     PIC S9(9)V99 COMP-3.
           05  SF-ATTORNEY-NAME       PIC X(60).
           05  SF-ATTORNEY-FEE-PCT    PIC 9V99 COMP-3.
           05  SF-ATTORNEY-FEE-AMT    PIC S9(7)V99 COMP-3.
           05  SF-CARRIER-SHARE       PIC S9(9)V99 COMP-3.
           05  SF-CLAIMANT-SHARE      PIC S9(9)V99 COMP-3.
           05  SF-OFFER-HISTORY.
               10  SF-OFFER-ENTRY OCCURS 10 TIMES.
                   15  SF-OFFER-DATE  PIC X(8).
                   15  SF-OFFER-AMT   PIC S9(9)V99 COMP-3.
                   15  SF-OFFER-TYPE  PIC X.
                       88  OFFER-MADE     VALUE 'M'.
                       88  OFFER-COUNTER  VALUE 'C'.
                       88  OFFER-ACCEPTED VALUE 'A'.
                       88  OFFER-REJECTED VALUE 'R'.
           05  SF-OFFER-COUNT         PIC 99 VALUE 0.
           05  SF-DATE-IDENTIFIED     PIC X(8).
           05  SF-DATE-SETTLED        PIC X(8).
           05  SF-MADE-WHOLE-APPLIED  PIC X VALUE 'N'.
               88  MADE-WHOLE-YES     VALUE 'Y'.
               88  MADE-WHOLE-NO      VALUE 'N'.
           05  FILLER                 PIC X(20).

       FD  RECOVERY-REPORT-FILE.
       01  RECOVERY-REPORT-RECORD     PIC X(132).

       FD  PAYMENT-FILE.
       01  PAYMENT-RECORD.
           05  PM-CLAIM-NUM           PIC X(20).
           05  PM-PARTY-SEQ           PIC 99.
           05  PM-AMOUNT              PIC S9(9)V99 COMP-3.
           05  PM-DATE                PIC X(8).
           05  PM-TYPE                PIC X(2).
               88  PM-RECOVERY        VALUE 'RC'.
               88  PM-CREDIT          VALUE 'CR'.

       WORKING-STORAGE SECTION.
      *
      *    Cross-program copybook references
           COPY WCCLMCPY
           COPY WCPMTCPY
       01  WS-FILE-STATUSES.
           05  WS-CLM-FILE-STATUS     PIC XX.
           05  WS-SUB-FILE-STATUS     PIC XX.
           05  WS-RPT-FILE-STATUS     PIC XX.
           05  WS-PMT-FILE-STATUS     PIC XX.

       01  WS-FLAGS.
           05  WS-EOF-FLAG            PIC X VALUE 'N'.
               88  END-OF-FILE        VALUE 'Y'.
           05  WS-PMT-EOF             PIC X VALUE 'N'.
               88  PMT-END-OF-FILE    VALUE 'Y'.

       01  WS-LIEN-CALCULATION.
           05  WS-TOTAL-CLAIM-COST    PIC S9(9)V99 COMP-3 VALUE 0.
           05  WS-LIEN-DEDUCTIONS     PIC S9(9)V99 COMP-3 VALUE 0.
           05  WS-COMPUTED-LIEN       PIC S9(9)V99 COMP-3 VALUE 0.
           05  WS-NET-RECOVERY        PIC S9(9)V99 COMP-3 VALUE 0.
           05  WS-GROSS-SETTLEMENT    PIC S9(9)V99 COMP-3 VALUE 0.
           05  WS-ATTY-FEE-TOTAL     PIC S9(7)V99 COMP-3 VALUE 0.
           05  WS-ATTY-FEE-CARRIER   PIC S9(7)V99 COMP-3 VALUE 0.
           05  WS-ATTY-FEE-CLAIMANT  PIC S9(7)V99 COMP-3 VALUE 0.
           05  WS-FUTURE-CREDIT      PIC S9(9)V99 COMP-3 VALUE 0.
           05  WS-CARRIER-NET        PIC S9(9)V99 COMP-3 VALUE 0.
           05  WS-CLAIMANT-NET       PIC S9(9)V99 COMP-3 VALUE 0.

       01  WS-MADE-WHOLE-TABLE.
           05  WS-MW-ENTRY OCCURS 15 TIMES.
               10  WS-MW-JURIS       PIC XX.
               10  WS-MW-DOCTRINE    PIC X.
                   88  MW-STRICT     VALUE 'S'.
                   88  MW-MODIFIED   VALUE 'M'.
                   88  MW-NONE       VALUE 'N'.
                   88  MW-COMMON     VALUE 'C'.

       01  WS-MW-INIT.
           05  FILLER PIC X(3) VALUE 'ALS'.
           05  FILLER PIC X(3) VALUE 'CAM'.
           05  FILLER PIC X(3) VALUE 'FLM'.
           05  FILLER PIC X(3) VALUE 'GAN'.
           05  FILLER PIC X(3) VALUE 'ILS'.
           05  FILLER PIC X(3) VALUE 'NYM'.
           05  FILLER PIC X(3) VALUE 'TXN'.
           05  FILLER PIC X(3) VALUE 'PAS'.
           05  FILLER PIC X(3) VALUE 'OHC'.
           05  FILLER PIC X(3) VALUE 'NJM'.
           05  FILLER PIC X(3) VALUE 'MAS'.
           05  FILLER PIC X(3) VALUE 'NCS'.
           05  FILLER PIC X(3) VALUE 'VAS'.
       01  WS-MW-INIT-R REDEFINES WS-MW-INIT.
           05  WS-MW-INIT-ENTRY PIC X(3) OCCURS 13 TIMES.

       01  WS-COUNTERS.
           05  WS-CLAIMS-REVIEWED     PIC 9(7) VALUE 0.
           05  WS-LIENS-CALCULATED    PIC 9(7) VALUE 0.
           05  WS-SETTLEMENTS-PROC    PIC 9(5) VALUE 0.
           05  WS-RECOVERIES-POSTED   PIC 9(5) VALUE 0.
           05  WS-TOTAL-RECOVERED     PIC S9(11)V99 COMP-3 VALUE 0.
           05  WS-TOTAL-LIEN-VALUE    PIC S9(11)V99 COMP-3 VALUE 0.

       01  WS-WORK-FIELDS.
           05  WS-WORK-AMT            PIC -(9)9.99.
           05  WS-WORK-PCT            PIC 9.99.
           05  WS-IDX                 PIC 99.
           05  WS-MW-IDX              PIC 99.
           05  WS-MW-FOUND            PIC X VALUE 'N'.
               88  MW-FOUND           VALUE 'Y'.
           05  WS-RPT-LINE            PIC X(132).
           05  WS-CURRENT-DATE        PIC X(8).

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
      *
      *    Inter-program communication calls
           CALL "WCLEGAL"
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-CLAIMS
           PERFORM 5000-PROCESS-PAYMENTS
           PERFORM 6000-GENERATE-RECOVERY-REPORT
           PERFORM 9000-FINALIZE
           STOP RUN.

       1000-INITIALIZE.
           OPEN I-O    CLAIM-FILE
                       SUBRO-FILE
           OPEN INPUT  PAYMENT-FILE
           OPEN OUTPUT RECOVERY-REPORT-FILE
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           PERFORM 1100-LOAD-MADE-WHOLE-TABLE
           INITIALIZE WS-COUNTERS
           DISPLAY 'WCSUBRO: STARTED - SUBROGATION PROCESSING'.

       1100-LOAD-MADE-WHOLE-TABLE.
           PERFORM VARYING WS-MW-IDX FROM 1 BY 1
               UNTIL WS-MW-IDX > 13
               MOVE WS-MW-INIT-ENTRY(WS-MW-IDX)(1:2)
                   TO WS-MW-JURIS(WS-MW-IDX)
               MOVE WS-MW-INIT-ENTRY(WS-MW-IDX)(3:1)
                   TO WS-MW-DOCTRINE(WS-MW-IDX)
           END-PERFORM.

       2000-PROCESS-CLAIMS.
           MOVE LOW-VALUES TO CF-CLAIM-NUM
           START CLAIM-FILE KEY > CF-CLAIM-NUM
           READ CLAIM-FILE NEXT
               AT END SET END-OF-FILE TO TRUE
           END-READ
           PERFORM UNTIL END-OF-FILE
               IF CF-HAS-SUBRO
                   ADD 1 TO WS-CLAIMS-REVIEWED
                   PERFORM 3000-CALCULATE-LIEN
                   PERFORM 4000-PROCESS-SETTLEMENT
               END-IF
               READ CLAIM-FILE NEXT
                   AT END SET END-OF-FILE TO TRUE
               END-READ
           END-PERFORM.

       3000-CALCULATE-LIEN.
           COMPUTE WS-TOTAL-CLAIM-COST =
               CF-TOTAL-PAID-MED + CF-TOTAL-PAID-IND
               + CF-TOTAL-EXPENSE
           COMPUTE WS-LIEN-DEDUCTIONS = 0
           MOVE WS-TOTAL-CLAIM-COST TO WS-COMPUTED-LIEN
           PERFORM 3100-APPLY-MADE-WHOLE
           IF NOT MADE-WHOLE-YES IN SF-MADE-WHOLE-APPLIED
               ADD 1 TO WS-LIENS-CALCULATED
               ADD WS-COMPUTED-LIEN TO WS-TOTAL-LIEN-VALUE
               MOVE CF-CLAIM-NUM TO SF-CLAIM-NUM
               MOVE 01 TO SF-PARTY-SEQ
               READ SUBRO-FILE
               IF WS-SUB-FILE-STATUS = '00'
                   MOVE WS-COMPUTED-LIEN TO SF-LIEN-AMOUNT
                   IF SUBRO-IDENTIFIED
                       MOVE 'LF' TO SF-STATUS
                   END-IF
                   REWRITE SUBRO-RECORD
               END-IF
           END-IF.

       3100-APPLY-MADE-WHOLE.
           MOVE 'N' TO WS-MW-FOUND
           PERFORM VARYING WS-MW-IDX FROM 1 BY 1
               UNTIL WS-MW-IDX > 13 OR MW-FOUND
               IF WS-MW-JURIS(WS-MW-IDX) = CF-JURISDICTION
                   SET MW-FOUND TO TRUE
               END-IF
           END-PERFORM
           IF MW-FOUND
               SUBTRACT 1 FROM WS-MW-IDX
               EVALUATE TRUE
                   WHEN MW-STRICT IN WS-MW-DOCTRINE(WS-MW-IDX)
                       PERFORM 3110-STRICT-MADE-WHOLE
                   WHEN MW-MODIFIED IN WS-MW-DOCTRINE(WS-MW-IDX)
                       PERFORM 3120-MODIFIED-MADE-WHOLE
                   WHEN MW-COMMON IN WS-MW-DOCTRINE(WS-MW-IDX)
                       PERFORM 3130-COMMON-FUND
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-IF.

       3110-STRICT-MADE-WHOLE.
      *    STRICT: CARRIER CANNOT RECOVER UNTIL CLAIMANT IS FULLY
      *    COMPENSATED FOR ALL DAMAGES (ECONOMIC + NON-ECONOMIC)
           MOVE CF-CLAIM-NUM TO SF-CLAIM-NUM
           MOVE 01 TO SF-PARTY-SEQ
           READ SUBRO-FILE
           IF WS-SUB-FILE-STATUS = '00'
               IF SF-SETTLEMENT-AMOUNT > 0
                   IF SF-SETTLEMENT-AMOUNT < WS-TOTAL-CLAIM-COST
                       MOVE 'Y' TO SF-MADE-WHOLE-APPLIED
                       MOVE 0 TO WS-COMPUTED-LIEN
                       REWRITE SUBRO-RECORD
                   END-IF
               END-IF
           END-IF.

       3120-MODIFIED-MADE-WHOLE.
      *    MODIFIED: CARRIER GETS PRO-RATA SHARE EVEN IF CLAIMANT
      *    NOT FULLY COMPENSATED. CARRIER SHARE = LIEN / TOTAL DAMAGES
           MOVE CF-CLAIM-NUM TO SF-CLAIM-NUM
           MOVE 01 TO SF-PARTY-SEQ
           READ SUBRO-FILE
           IF WS-SUB-FILE-STATUS = '00'
               IF SF-SETTLEMENT-AMOUNT > 0
                   COMPUTE WS-COMPUTED-LIEN =
                       (WS-TOTAL-CLAIM-COST /
                       (WS-TOTAL-CLAIM-COST +
                        SF-SETTLEMENT-AMOUNT))
                       * SF-SETTLEMENT-AMOUNT
               END-IF
           END-IF.

       3130-COMMON-FUND.
      *    COMMON FUND: CARRIER MUST CONTRIBUTE PRO-RATA TO
      *    ATTORNEY FEES THAT CREATED THE RECOVERY FUND
           MOVE CF-CLAIM-NUM TO SF-CLAIM-NUM
           MOVE 01 TO SF-PARTY-SEQ
           READ SUBRO-FILE
           IF WS-SUB-FILE-STATUS = '00'
               IF SF-ATTORNEY-FEE-PCT > 0
                   COMPUTE WS-ATTY-FEE-CARRIER =
                       WS-COMPUTED-LIEN * SF-ATTORNEY-FEE-PCT
                   COMPUTE WS-COMPUTED-LIEN =
                       WS-COMPUTED-LIEN - WS-ATTY-FEE-CARRIER
               END-IF
           END-IF.

       4000-PROCESS-SETTLEMENT.
           MOVE CF-CLAIM-NUM TO SF-CLAIM-NUM
           MOVE 01 TO SF-PARTY-SEQ
           READ SUBRO-FILE
           IF WS-SUB-FILE-STATUS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           IF NOT SUBRO-SETTLED AND NOT SUBRO-NEGOTIATING
               EXIT PARAGRAPH
           END-IF
           IF SUBRO-SETTLED
               ADD 1 TO WS-SETTLEMENTS-PROC
               PERFORM 4100-CALCULATE-DISTRIBUTION
           END-IF.

       4100-CALCULATE-DISTRIBUTION.
           MOVE SF-SETTLEMENT-AMOUNT TO WS-GROSS-SETTLEMENT
           COMPUTE WS-ATTY-FEE-TOTAL =
               WS-GROSS-SETTLEMENT * SF-ATTORNEY-FEE-PCT
           COMPUTE WS-ATTY-FEE-CARRIER =
               WS-ATTY-FEE-TOTAL *
               (WS-COMPUTED-LIEN / WS-GROSS-SETTLEMENT)
           COMPUTE WS-ATTY-FEE-CLAIMANT =
               WS-ATTY-FEE-TOTAL - WS-ATTY-FEE-CARRIER
           COMPUTE WS-NET-RECOVERY =
               WS-GROSS-SETTLEMENT - WS-ATTY-FEE-TOTAL
           IF WS-COMPUTED-LIEN <= WS-NET-RECOVERY
               MOVE WS-COMPUTED-LIEN TO WS-CARRIER-NET
               COMPUTE WS-CLAIMANT-NET =
                   WS-NET-RECOVERY - WS-COMPUTED-LIEN
           ELSE
               MOVE WS-NET-RECOVERY TO WS-CARRIER-NET
               MOVE 0 TO WS-CLAIMANT-NET
           END-IF
           COMPUTE WS-FUTURE-CREDIT =
               WS-CARRIER-NET - WS-TOTAL-CLAIM-COST
           IF WS-FUTURE-CREDIT < 0
               MOVE 0 TO WS-FUTURE-CREDIT
           END-IF
           MOVE WS-CARRIER-NET TO SF-CARRIER-SHARE
           MOVE WS-CLAIMANT-NET TO SF-CLAIMANT-SHARE
           MOVE WS-ATTY-FEE-TOTAL TO SF-ATTORNEY-FEE-AMT
           MOVE WS-CARRIER-NET TO SF-RECOVERY-AMOUNT
           REWRITE SUBRO-RECORD.

       5000-PROCESS-PAYMENTS.
           READ PAYMENT-FILE
               AT END SET PMT-END-OF-FILE TO TRUE
           END-READ
           PERFORM UNTIL PMT-END-OF-FILE
               MOVE PM-CLAIM-NUM TO SF-CLAIM-NUM
               MOVE PM-PARTY-SEQ TO SF-PARTY-SEQ
               READ SUBRO-FILE
               IF WS-SUB-FILE-STATUS = '00'
                   ADD PM-AMOUNT TO SF-RECOVERY-AMOUNT
                   ADD PM-AMOUNT TO WS-TOTAL-RECOVERED
                   ADD 1 TO WS-RECOVERIES-POSTED
                   IF SF-RECOVERY-AMOUNT >= SF-CARRIER-SHARE
                       MOVE 'CO' TO SF-STATUS
                   END-IF
                   REWRITE SUBRO-RECORD
               END-IF
               READ PAYMENT-FILE
                   AT END SET PMT-END-OF-FILE TO TRUE
               END-READ
           END-PERFORM.

       6000-GENERATE-RECOVERY-REPORT.
           INITIALIZE WS-RPT-LINE
           STRING
               'SUBROGATION RECOVERY SUMMARY REPORT'
               DELIMITED BY SIZE INTO WS-RPT-LINE
           END-STRING
           WRITE RECOVERY-REPORT-RECORD FROM WS-RPT-LINE
           INITIALIZE WS-RPT-LINE
           STRING 'RUN DATE: ' WS-CURRENT-DATE
               DELIMITED BY SIZE INTO WS-RPT-LINE
           END-STRING
           WRITE RECOVERY-REPORT-RECORD FROM WS-RPT-LINE
           MOVE ALL '=' TO WS-RPT-LINE
           WRITE RECOVERY-REPORT-RECORD FROM WS-RPT-LINE

           INITIALIZE WS-RPT-LINE
           STRING
               'CLAIM NUM           PARTY              '
               'LIEN AMT      SETTLEMENT    RECOVERY   '
               'STATUS'
               DELIMITED BY SIZE INTO WS-RPT-LINE
           END-STRING
           WRITE RECOVERY-REPORT-RECORD FROM WS-RPT-LINE
           MOVE ALL '-' TO WS-RPT-LINE
           WRITE RECOVERY-REPORT-RECORD FROM WS-RPT-LINE

           MOVE LOW-VALUES TO SF-SUBRO-KEY
           START SUBRO-FILE KEY > SF-SUBRO-KEY
           MOVE 'N' TO WS-EOF-FLAG
           READ SUBRO-FILE NEXT
               AT END SET END-OF-FILE TO TRUE
           END-READ
           PERFORM UNTIL END-OF-FILE
               INITIALIZE WS-RPT-LINE
               MOVE SF-LIEN-AMOUNT TO WS-WORK-AMT
               STRING
                   SF-CLAIM-NUM DELIMITED BY SIZE
                   ' ' DELIMITED BY SIZE
                   SF-PARTY-NAME(1:18) DELIMITED BY SIZE
                   ' ' DELIMITED BY SIZE
                   WS-WORK-AMT DELIMITED BY SIZE
                   INTO WS-RPT-LINE
               END-STRING
               MOVE SF-SETTLEMENT-AMOUNT TO WS-WORK-AMT
               STRING
                   WS-RPT-LINE DELIMITED BY '  '
                   '  ' DELIMITED BY SIZE
                   WS-WORK-AMT DELIMITED BY SIZE
                   INTO WS-RPT-LINE
               END-STRING
               MOVE SF-RECOVERY-AMOUNT TO WS-WORK-AMT
               STRING
                   WS-RPT-LINE DELIMITED BY '  '
                   '  ' DELIMITED BY SIZE
                   WS-WORK-AMT DELIMITED BY SIZE
                   '  ' DELIMITED BY SIZE
                   SF-STATUS DELIMITED BY SIZE
                   INTO WS-RPT-LINE
               END-STRING
               WRITE RECOVERY-REPORT-RECORD FROM WS-RPT-LINE
               READ SUBRO-FILE NEXT
                   AT END SET END-OF-FILE TO TRUE
               END-READ
           END-PERFORM

           MOVE ALL '=' TO WS-RPT-LINE
           WRITE RECOVERY-REPORT-RECORD FROM WS-RPT-LINE
           INITIALIZE WS-RPT-LINE
           MOVE WS-TOTAL-LIEN-VALUE TO WS-WORK-AMT
           STRING
               'TOTAL LIEN VALUE:     ' WS-WORK-AMT
               DELIMITED BY SIZE INTO WS-RPT-LINE
           END-STRING
           WRITE RECOVERY-REPORT-RECORD FROM WS-RPT-LINE
           MOVE WS-TOTAL-RECOVERED TO WS-WORK-AMT
           INITIALIZE WS-RPT-LINE
           STRING
               'TOTAL RECOVERED:      ' WS-WORK-AMT
               DELIMITED BY SIZE INTO WS-RPT-LINE
           END-STRING
           WRITE RECOVERY-REPORT-RECORD FROM WS-RPT-LINE.

       9000-FINALIZE.
           CLOSE CLAIM-FILE
                 SUBRO-FILE
                 PAYMENT-FILE
                 RECOVERY-REPORT-FILE
           DISPLAY 'WCSUBRO: COMPLETED'
           DISPLAY 'WCSUBRO: CLAIMS REVIEWED     = '
                   WS-CLAIMS-REVIEWED
           DISPLAY 'WCSUBRO: LIENS CALCULATED    = '
                   WS-LIENS-CALCULATED
           DISPLAY 'WCSUBRO: SETTLEMENTS PROC    = '
                   WS-SETTLEMENTS-PROC
           DISPLAY 'WCSUBRO: RECOVERIES POSTED   = '
                   WS-RECOVERIES-POSTED
           DISPLAY 'WCSUBRO: TOTAL RECOVERED     = '
                   WS-TOTAL-RECOVERED
           DISPLAY 'WCSUBRO: TOTAL LIEN VALUE    = '
                   WS-TOTAL-LIEN-VALUE.
