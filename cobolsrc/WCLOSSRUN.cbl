       IDENTIFICATION DIVISION.
       PROGRAM-ID. WCLOSSRUN.
      *================================================================
      * WCLOSSRUN - Workers' Compensation Loss Run Report Generator
      *
      * Generates comprehensive loss run reports at policy level
      * including paid/outstanding/incurred summaries by category,
      * claim detail listings, loss ratio calculations, development
      * factor application, large loss identification, and multi-year
      * trending. Supports detail, summary, and executive formats.
      *
      * File: POLMSTF (Policy master, key-sequenced)
      * File: CLMMSTF (Claim master, key-sequenced)
      * File: CLMFINF (Claim financial, key-sequenced)
      * File: DEVFCTF (Development factors, key-sequenced)
      * File: LOSSRPT (Report output, sequential)
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT POLICY-FILE
               ASSIGN TO "POLMSTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS POL-KEY
               FILE STATUS IS WS-FILE-STATUS.

           SELECT CLAIM-FILE
               ASSIGN TO "CLMMSTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CLM-KEY
               FILE STATUS IS WS-FILE-STATUS.

           SELECT CLAIM-FIN-FILE
               ASSIGN TO "CLMFINF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FIN-KEY
               FILE STATUS IS WS-FILE-STATUS.

           SELECT DEV-FACTOR-FILE
               ASSIGN TO "DEVFCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DEV-KEY
               FILE STATUS IS WS-FILE-STATUS.

           SELECT REPORT-FILE
               ASSIGN TO "LOSSRPT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  POLICY-FILE.
       01  POLICY-RECORD.
           05 POL-KEY.
              10 POL-NUMBER               PIC X(12).
           05 POL-INSURED-NAME           PIC X(40).
           05 POL-EFF-DATE               PIC 9(8).
           05 POL-EXP-DATE               PIC 9(8).
           05 POL-STATE                  PIC X(2).
           05 POL-EARNED-PREMIUM         PIC 9(9)V99.
           05 POL-STANDARD-PREMIUM       PIC 9(9)V99.
           05 POL-MOD-FACTOR             PIC 9(1)V9999.
           05 POL-STATUS                 PIC X(1).
              88 POL-ACTIVE              VALUE "A".
              88 POL-EXPIRED             VALUE "E".
              88 POL-CANCELLED           VALUE "C".

       FD  CLAIM-FILE.
       01  CLAIM-RECORD.
           05 CLM-KEY.
              10 CLM-POLICY-NUM          PIC X(12).
              10 CLM-NUMBER              PIC X(12).
           05 CLM-CLAIMANT-NAME          PIC X(30).
           05 CLM-INJURY-DATE            PIC 9(8).
           05 CLM-REPORT-DATE            PIC 9(8).
           05 CLM-STATUS                 PIC X(2).
              88 CLM-OPEN                VALUE "OP".
              88 CLM-CLOSED              VALUE "CL".
              88 CLM-REOPENED            VALUE "RO".
           05 CLM-CLASS-CODE             PIC X(4).
           05 CLM-NATURE-INJURY          PIC X(4).
           05 CLM-BODY-PART              PIC X(4).
           05 CLM-CAUSE-INJURY           PIC X(4).
           05 CLM-ACCIDENT-YEAR          PIC 9(4).

       FD  CLAIM-FIN-FILE.
       01  CLAIM-FIN-RECORD.
           05 FIN-KEY.
              10 FIN-POLICY-NUM          PIC X(12).
              10 FIN-CLAIM-NUM           PIC X(12).
           05 FIN-MEDICAL-PAID           PIC 9(9)V99.
           05 FIN-MEDICAL-RESERVE        PIC 9(9)V99.
           05 FIN-INDEMNITY-PAID         PIC 9(9)V99.
           05 FIN-INDEMNITY-RESERVE      PIC 9(9)V99.
           05 FIN-EXPENSE-PAID           PIC 9(9)V99.
           05 FIN-EXPENSE-RESERVE        PIC 9(9)V99.
           05 FIN-SUBROGATION            PIC 9(9)V99.
           05 FIN-RECOVERY               PIC 9(9)V99.
           05 FIN-LAST-PAYMENT-DATE      PIC 9(8).

       FD  DEV-FACTOR-FILE.
       01  DEV-FACTOR-RECORD.
           05 DEV-KEY.
              10 DEV-STATE               PIC X(2).
              10 DEV-MATURITY-MONTHS     PIC 9(3).
           05 DEV-PAID-FACTOR            PIC 9(3)V9999.
           05 DEV-INCURRED-FACTOR        PIC 9(3)V9999.

       FD  REPORT-FILE.
       01  REPORT-LINE                   PIC X(132).

       WORKING-STORAGE SECTION.
      *
      *    Cross-program copybook references
           COPY WCCLMCPY
           COPY WCRSVCPY
           COPY WCPMTCPY
       01  WS-FILE-STATUS                PIC X(2).
       01  WS-CURRENT-DATE               PIC 9(8).
       01  WS-EOF-FLAG                   PIC X(1) VALUE "N".
           88 WS-EOF                     VALUE "Y".

       01  WS-REPORT-PARAMS.
           05 WS-PARAM-POLICY-NUM        PIC X(12).
           05 WS-PARAM-FORMAT            PIC X(1).
              88 WS-FORMAT-DETAIL        VALUE "D".
              88 WS-FORMAT-SUMMARY       VALUE "S".
              88 WS-FORMAT-EXECUTIVE     VALUE "E".
           05 WS-PARAM-AS-OF-DATE        PIC 9(8).
           05 WS-PARAM-YEARS-BACK        PIC 9(2) VALUE 5.
           05 WS-LARGE-LOSS-THRESHOLD    PIC 9(9)V99
                                         VALUE 100000.00.

       01  WS-PAGE-CONTROL.
           05 WS-PAGE-NUMBER             PIC 9(4) VALUE 0.
           05 WS-LINE-COUNT              PIC 9(3) VALUE 99.
           05 WS-MAX-LINES               PIC 9(3) VALUE 55.

      * Policy-level accumulators
       01  WS-POLICY-TOTALS.
           05 WS-POL-MED-PAID           PIC 9(11)V99 VALUE 0.
           05 WS-POL-MED-RESERVE        PIC 9(11)V99 VALUE 0.
           05 WS-POL-MED-INCURRED       PIC 9(11)V99 VALUE 0.
           05 WS-POL-IND-PAID           PIC 9(11)V99 VALUE 0.
           05 WS-POL-IND-RESERVE        PIC 9(11)V99 VALUE 0.
           05 WS-POL-IND-INCURRED       PIC 9(11)V99 VALUE 0.
           05 WS-POL-EXP-PAID           PIC 9(11)V99 VALUE 0.
           05 WS-POL-EXP-RESERVE        PIC 9(11)V99 VALUE 0.
           05 WS-POL-EXP-INCURRED       PIC 9(11)V99 VALUE 0.
           05 WS-POL-SUBRO              PIC 9(11)V99 VALUE 0.
           05 WS-POL-RECOVERY           PIC 9(11)V99 VALUE 0.
           05 WS-POL-TOTAL-PAID         PIC 9(11)V99 VALUE 0.
           05 WS-POL-TOTAL-RESERVE      PIC 9(11)V99 VALUE 0.
           05 WS-POL-TOTAL-INCURRED     PIC 9(11)V99 VALUE 0.
           05 WS-POL-NET-INCURRED       PIC 9(11)V99 VALUE 0.
           05 WS-POL-CLAIM-COUNT        PIC 9(5) VALUE 0.
           05 WS-POL-OPEN-COUNT         PIC 9(5) VALUE 0.
           05 WS-POL-LARGE-LOSS-COUNT   PIC 9(3) VALUE 0.

      * Year-level accumulators (10 years max)
       01  WS-YEAR-TABLE.
           05 WS-YEAR-ENTRY OCCURS 10 TIMES.
              10 WS-YR-ACCIDENT-YEAR     PIC 9(4).
              10 WS-YR-CLAIM-COUNT       PIC 9(5).
              10 WS-YR-PAID              PIC 9(11)V99.
              10 WS-YR-RESERVE           PIC 9(11)V99.
              10 WS-YR-INCURRED          PIC 9(11)V99.
              10 WS-YR-DEVELOPED         PIC 9(11)V99.
              10 WS-YR-PREMIUM           PIC 9(11)V99.
              10 WS-YR-LOSS-RATIO        PIC 9(3)V99.
       01  WS-YEAR-COUNT                 PIC 9(2) VALUE 0.

      * Large loss tracking
       01  WS-LARGE-LOSS-TABLE.
           05 WS-LARGE-LOSS OCCURS 50 TIMES.
              10 WS-LL-CLAIM-NUM         PIC X(12).
              10 WS-LL-CLAIMANT          PIC X(25).
              10 WS-LL-INJURY-DATE       PIC 9(8).
              10 WS-LL-TOTAL-INCURRED    PIC 9(11)V99.
              10 WS-LL-STATUS            PIC X(2).
       01  WS-LARGE-LOSS-COUNT           PIC 9(3) VALUE 0.

      * Loss ratio calculation
       01  WS-LOSS-RATIO-FIELDS.
           05 WS-LR-EARNED-PREMIUM      PIC 9(11)V99.
           05 WS-LR-INCURRED-LOSSES     PIC 9(11)V99.
           05 WS-LR-LOSS-RATIO          PIC 9(3)V99.
           05 WS-LR-DEVELOPED-LOSSES    PIC 9(11)V99.
           05 WS-LR-DEV-LOSS-RATIO      PIC 9(3)V99.

      * Development factor work area
       01  WS-DEV-WORK.
           05 WS-DEV-MATURITY           PIC 9(3).
           05 WS-DEV-PAID-FACTOR        PIC 9(3)V9999.
           05 WS-DEV-INCURRED-FACTOR    PIC 9(3)V9999.

      * Report lines
       01  WS-HDR-1.
           05 FILLER PIC X(40)
              VALUE "WORKERS' COMPENSATION LOSS RUN REPORT   ".
           05 FILLER PIC X(20) VALUE SPACES.
           05 WS-HDR-DATE PIC X(10).
           05 FILLER PIC X(10) VALUE SPACES.
           05 WS-HDR-PAGE-LBL PIC X(6) VALUE "PAGE: ".
           05 WS-HDR-PAGE PIC ZZZ9.
           05 FILLER PIC X(42) VALUE SPACES.

       01  WS-HDR-2.
           05 WS-HDR-POL-LBL PIC X(8) VALUE "POLICY: ".
           05 WS-HDR-POL-NUM PIC X(12).
           05 FILLER PIC X(3) VALUE SPACES.
           05 WS-HDR-INSURED PIC X(40).
           05 FILLER PIC X(3) VALUE SPACES.
           05 WS-HDR-EFF-LBL PIC X(5) VALUE "EFF: ".
           05 WS-HDR-EFF-DT PIC X(10).
           05 FILLER PIC X(3) VALUE SPACES.
           05 WS-HDR-EXP-LBL PIC X(5) VALUE "EXP: ".
           05 WS-HDR-EXP-DT PIC X(10).
           05 FILLER PIC X(33) VALUE SPACES.

       01  WS-DTL-LINE.
           05 WS-DTL-CLM-NUM PIC X(12).
           05 FILLER PIC X(1) VALUE SPACE.
           05 WS-DTL-NAME PIC X(20).
           05 FILLER PIC X(1) VALUE SPACE.
           05 WS-DTL-INJ-DT PIC X(10).
           05 FILLER PIC X(1) VALUE SPACE.
           05 WS-DTL-STATUS PIC X(2).
           05 FILLER PIC X(1) VALUE SPACE.
           05 WS-DTL-MED-PD PIC $$$,$$$,$$9.99.
           05 FILLER PIC X(1) VALUE SPACE.
           05 WS-DTL-IND-PD PIC $$$,$$$,$$9.99.
           05 FILLER PIC X(1) VALUE SPACE.
           05 WS-DTL-EXP-PD PIC $$$,$$$,$$9.99.
           05 FILLER PIC X(1) VALUE SPACE.
           05 WS-DTL-TOTAL PIC $$$,$$$,$$9.99.
           05 FILLER PIC X(13) VALUE SPACES.

       01  WS-TOTAL-LINE.
           05 WS-TOT-LABEL PIC X(35).
           05 WS-TOT-MED PIC $$$$,$$$,$$9.99.
           05 FILLER PIC X(1) VALUE SPACE.
           05 WS-TOT-IND PIC $$$$,$$$,$$9.99.
           05 FILLER PIC X(1) VALUE SPACE.
           05 WS-TOT-EXP PIC $$$$,$$$,$$9.99.
           05 FILLER PIC X(1) VALUE SPACE.
           05 WS-TOT-ALL PIC $$$$,$$$,$$9.99.
           05 FILLER PIC X(18) VALUE SPACES.

       01  WS-YEAR-LINE.
           05 WS-YL-YEAR PIC 9(4).
           05 FILLER PIC X(3) VALUE SPACES.
           05 WS-YL-CLAIMS PIC ZZ,ZZ9.
           05 FILLER PIC X(2) VALUE SPACES.
           05 WS-YL-PAID PIC $$$$,$$$,$$9.99.
           05 FILLER PIC X(2) VALUE SPACES.
           05 WS-YL-INCURRED PIC $$$$,$$$,$$9.99.
           05 FILLER PIC X(2) VALUE SPACES.
           05 WS-YL-DEVELOPED PIC $$$$,$$$,$$9.99.
           05 FILLER PIC X(2) VALUE SPACES.
           05 WS-YL-PREMIUM PIC $$$$,$$$,$$9.99.
           05 FILLER PIC X(2) VALUE SPACES.
           05 WS-YL-RATIO PIC ZZ9.99.
           05 WS-YL-PCT PIC X(1) VALUE "%".
           05 FILLER PIC X(27) VALUE SPACES.

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-POLICY
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           OPEN INPUT POLICY-FILE
           OPEN INPUT CLAIM-FILE
           OPEN INPUT CLAIM-FIN-FILE
           OPEN INPUT DEV-FACTOR-FILE
           OPEN OUTPUT REPORT-FILE
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE WS-CURRENT-DATE TO WS-PARAM-AS-OF-DATE
           INITIALIZE WS-POLICY-TOTALS
           INITIALIZE WS-YEAR-TABLE.

       2000-PROCESS-POLICY.
      *    Read policy record
           MOVE WS-PARAM-POLICY-NUM TO POL-NUMBER
           READ POLICY-FILE
               INVALID KEY
                   STRING "POLICY NOT FOUND: "
                       DELIMITED BY SIZE
                       WS-PARAM-POLICY-NUM DELIMITED BY SIZE
                       INTO REPORT-LINE
                   WRITE REPORT-LINE
                   GO TO 2000-EXIT
           END-READ

      *    Write report header
           PERFORM 5000-WRITE-PAGE-HEADER

      *    Process all claims for this policy
           MOVE POL-NUMBER TO CLM-POLICY-NUM
           MOVE SPACES TO CLM-NUMBER
           START CLAIM-FILE KEY >= CLM-KEY
               INVALID KEY
                   GO TO 2500-WRITE-POLICY-TOTALS
           END-START

           MOVE "N" TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ CLAIM-FILE NEXT
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       IF CLM-POLICY-NUM NOT = POL-NUMBER
                           SET WS-EOF TO TRUE
                       ELSE
                           PERFORM 3000-PROCESS-CLAIM
                       END-IF
               END-READ
           END-PERFORM.

       2500-WRITE-POLICY-TOTALS.
      *    Calculate final totals
           COMPUTE WS-POL-TOTAL-PAID =
               WS-POL-MED-PAID + WS-POL-IND-PAID + WS-POL-EXP-PAID
           COMPUTE WS-POL-TOTAL-RESERVE =
               WS-POL-MED-RESERVE + WS-POL-IND-RESERVE
               + WS-POL-EXP-RESERVE
           COMPUTE WS-POL-TOTAL-INCURRED =
               WS-POL-TOTAL-PAID + WS-POL-TOTAL-RESERVE
           COMPUTE WS-POL-NET-INCURRED =
               WS-POL-TOTAL-INCURRED - WS-POL-SUBRO
               - WS-POL-RECOVERY

      *    Loss ratio
           MOVE POL-EARNED-PREMIUM TO WS-LR-EARNED-PREMIUM
           MOVE WS-POL-NET-INCURRED TO WS-LR-INCURRED-LOSSES
           IF WS-LR-EARNED-PREMIUM > 0
               COMPUTE WS-LR-LOSS-RATIO =
                   (WS-LR-INCURRED-LOSSES / WS-LR-EARNED-PREMIUM)
                   * 100
           ELSE
               MOVE 0 TO WS-LR-LOSS-RATIO
           END-IF

      *    Write totals section
           PERFORM 5100-WRITE-TOTALS-SECTION

      *    Write year-by-year trending
           IF WS-YEAR-COUNT > 0
               PERFORM 5200-WRITE-YEAR-TREND
           END-IF

      *    Write large loss listing
           IF WS-LARGE-LOSS-COUNT > 0
               PERFORM 5300-WRITE-LARGE-LOSSES
           END-IF

      *    Write loss ratio summary
           PERFORM 5400-WRITE-LOSS-RATIO.

       2000-EXIT.
           EXIT.

       3000-PROCESS-CLAIM.
      *    Process individual claim -- read financials and accumulate
           ADD 1 TO WS-POL-CLAIM-COUNT
           IF CLM-OPEN OR CLM-REOPENED
               ADD 1 TO WS-POL-OPEN-COUNT
           END-IF

      *    Read financial record
           MOVE CLM-POLICY-NUM TO FIN-POLICY-NUM
           MOVE CLM-NUMBER TO FIN-CLAIM-NUM
           READ CLAIM-FIN-FILE
               INVALID KEY
                   GO TO 3000-EXIT
           END-READ

      *    Accumulate policy totals
           ADD FIN-MEDICAL-PAID TO WS-POL-MED-PAID
           ADD FIN-MEDICAL-RESERVE TO WS-POL-MED-RESERVE
           ADD FIN-INDEMNITY-PAID TO WS-POL-IND-PAID
           ADD FIN-INDEMNITY-RESERVE TO WS-POL-IND-RESERVE
           ADD FIN-EXPENSE-PAID TO WS-POL-EXP-PAID
           ADD FIN-EXPENSE-RESERVE TO WS-POL-EXP-RESERVE
           ADD FIN-SUBROGATION TO WS-POL-SUBRO
           ADD FIN-RECOVERY TO WS-POL-RECOVERY

           COMPUTE WS-POL-MED-INCURRED =
               WS-POL-MED-PAID + WS-POL-MED-RESERVE
           COMPUTE WS-POL-IND-INCURRED =
               WS-POL-IND-PAID + WS-POL-IND-RESERVE

      *    Accumulate by accident year
           PERFORM 3100-ACCUMULATE-YEAR

      *    Check for large loss
           COMPUTE WS-POL-TOTAL-INCURRED =
               FIN-MEDICAL-PAID + FIN-MEDICAL-RESERVE
               + FIN-INDEMNITY-PAID + FIN-INDEMNITY-RESERVE
               + FIN-EXPENSE-PAID + FIN-EXPENSE-RESERVE
               - FIN-SUBROGATION - FIN-RECOVERY
           IF WS-POL-TOTAL-INCURRED >= WS-LARGE-LOSS-THRESHOLD
               PERFORM 3200-RECORD-LARGE-LOSS
           END-IF

      *    Write detail line (if detail format)
           IF WS-FORMAT-DETAIL
               PERFORM 5050-WRITE-DETAIL-LINE
           END-IF.
       3000-EXIT.
           EXIT.

       3100-ACCUMULATE-YEAR.
      *    Find or create year entry and accumulate
           PERFORM VARYING WS-YEAR-COUNT FROM 1 BY 0
               UNTIL WS-YEAR-COUNT > 10
               IF WS-YR-ACCIDENT-YEAR(WS-YEAR-COUNT)
                   = CLM-ACCIDENT-YEAR
                   OR WS-YR-ACCIDENT-YEAR(WS-YEAR-COUNT) = 0
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-YEAR-COUNT
           END-PERFORM

           IF WS-YEAR-COUNT <= 10
               IF WS-YR-ACCIDENT-YEAR(WS-YEAR-COUNT) = 0
                   MOVE CLM-ACCIDENT-YEAR
                       TO WS-YR-ACCIDENT-YEAR(WS-YEAR-COUNT)
                   MOVE POL-EARNED-PREMIUM
                       TO WS-YR-PREMIUM(WS-YEAR-COUNT)
               END-IF
               ADD 1 TO WS-YR-CLAIM-COUNT(WS-YEAR-COUNT)
               COMPUTE WS-YR-PAID(WS-YEAR-COUNT) =
                   WS-YR-PAID(WS-YEAR-COUNT)
                   + FIN-MEDICAL-PAID + FIN-INDEMNITY-PAID
                   + FIN-EXPENSE-PAID
               COMPUTE WS-YR-INCURRED(WS-YEAR-COUNT) =
                   WS-YR-INCURRED(WS-YEAR-COUNT)
                   + FIN-MEDICAL-PAID + FIN-MEDICAL-RESERVE
                   + FIN-INDEMNITY-PAID + FIN-INDEMNITY-RESERVE
                   + FIN-EXPENSE-PAID + FIN-EXPENSE-RESERVE
                   - FIN-SUBROGATION - FIN-RECOVERY

      *        Apply development factor
               PERFORM 3150-APPLY-DEV-FACTOR
           END-IF.

       3150-APPLY-DEV-FACTOR.
      *    Look up and apply development factor for immature years
           COMPUTE WS-DEV-MATURITY =
               (FUNCTION INTEGER-OF-DATE(WS-PARAM-AS-OF-DATE)
               - FUNCTION INTEGER-OF-DATE(CLM-INJURY-DATE))
               / 30
           MOVE POL-STATE TO DEV-STATE
           MOVE WS-DEV-MATURITY TO DEV-MATURITY-MONTHS
           READ DEV-FACTOR-FILE
               INVALID KEY
                   MOVE 1.0000 TO WS-DEV-INCURRED-FACTOR
                   GO TO 3150-APPLY
           END-READ
           MOVE DEV-INCURRED-FACTOR TO WS-DEV-INCURRED-FACTOR.

       3150-APPLY.
           COMPUTE WS-YR-DEVELOPED(WS-YEAR-COUNT) =
               WS-YR-INCURRED(WS-YEAR-COUNT)
               * WS-DEV-INCURRED-FACTOR.

       3200-RECORD-LARGE-LOSS.
      *    Add to large loss table
           IF WS-LARGE-LOSS-COUNT < 50
               ADD 1 TO WS-LARGE-LOSS-COUNT
               MOVE CLM-NUMBER
                   TO WS-LL-CLAIM-NUM(WS-LARGE-LOSS-COUNT)
               MOVE CLM-CLAIMANT-NAME(1:25)
                   TO WS-LL-CLAIMANT(WS-LARGE-LOSS-COUNT)
               MOVE CLM-INJURY-DATE
                   TO WS-LL-INJURY-DATE(WS-LARGE-LOSS-COUNT)
               MOVE WS-POL-TOTAL-INCURRED
                   TO WS-LL-TOTAL-INCURRED(WS-LARGE-LOSS-COUNT)
               MOVE CLM-STATUS
                   TO WS-LL-STATUS(WS-LARGE-LOSS-COUNT)
               ADD 1 TO WS-POL-LARGE-LOSS-COUNT
           END-IF.

       5000-WRITE-PAGE-HEADER.
           ADD 1 TO WS-PAGE-NUMBER
           IF WS-PAGE-NUMBER > 1
               MOVE SPACES TO REPORT-LINE
               WRITE REPORT-LINE BEFORE ADVANCING PAGE
           END-IF
           MOVE WS-CURRENT-DATE TO WS-HDR-DATE
           MOVE WS-PAGE-NUMBER TO WS-HDR-PAGE
           WRITE REPORT-LINE FROM WS-HDR-1
           MOVE POL-NUMBER TO WS-HDR-POL-NUM
           MOVE POL-INSURED-NAME TO WS-HDR-INSURED
           MOVE POL-EFF-DATE TO WS-HDR-EFF-DT
           MOVE POL-EXP-DATE TO WS-HDR-EXP-DT
           WRITE REPORT-LINE FROM WS-HDR-2
           MOVE ALL "-" TO REPORT-LINE
           WRITE REPORT-LINE
           IF WS-FORMAT-DETAIL
               MOVE "CLAIM NUMBER  CLAIMANT NAME        "
                 & "INJURY DT   ST  "
                 & "MEDICAL PAID   INDEMNITY PAID  "
                 & "EXPENSE PAID  "
                 & "TOTAL INCURRED" TO REPORT-LINE
               WRITE REPORT-LINE
               MOVE ALL "-" TO REPORT-LINE
               WRITE REPORT-LINE
           END-IF
           MOVE 6 TO WS-LINE-COUNT.

       5050-WRITE-DETAIL-LINE.
      *    Write individual claim detail
           IF WS-LINE-COUNT >= WS-MAX-LINES
               PERFORM 5000-WRITE-PAGE-HEADER
           END-IF
           MOVE CLM-NUMBER TO WS-DTL-CLM-NUM
           MOVE CLM-CLAIMANT-NAME(1:20) TO WS-DTL-NAME
           MOVE CLM-INJURY-DATE TO WS-DTL-INJ-DT
           MOVE CLM-STATUS TO WS-DTL-STATUS
           MOVE FIN-MEDICAL-PAID TO WS-DTL-MED-PD
           MOVE FIN-INDEMNITY-PAID TO WS-DTL-IND-PD
           MOVE FIN-EXPENSE-PAID TO WS-DTL-EXP-PD
           COMPUTE WS-DTL-TOTAL =
               FIN-MEDICAL-PAID + FIN-MEDICAL-RESERVE
               + FIN-INDEMNITY-PAID + FIN-INDEMNITY-RESERVE
               + FIN-EXPENSE-PAID + FIN-EXPENSE-RESERVE
               - FIN-SUBROGATION - FIN-RECOVERY
           WRITE REPORT-LINE FROM WS-DTL-LINE
           ADD 1 TO WS-LINE-COUNT.

       5100-WRITE-TOTALS-SECTION.
      *    Write policy-level financial summary
           IF WS-LINE-COUNT >= WS-MAX-LINES - 10
               PERFORM 5000-WRITE-PAGE-HEADER
           END-IF
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE ALL "=" TO REPORT-LINE
           WRITE REPORT-LINE
           STRING "POLICY FINANCIAL SUMMARY  (CLAIMS: "
               DELIMITED BY SIZE
               WS-POL-CLAIM-COUNT DELIMITED BY SIZE
               "  OPEN: " DELIMITED BY SIZE
               WS-POL-OPEN-COUNT DELIMITED BY SIZE
               ")" DELIMITED BY SIZE
               INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE ALL "-" TO REPORT-LINE
           WRITE REPORT-LINE

      *    Paid totals
           MOVE "TOTAL PAID:" TO WS-TOT-LABEL
           MOVE WS-POL-MED-PAID TO WS-TOT-MED
           MOVE WS-POL-IND-PAID TO WS-TOT-IND
           MOVE WS-POL-EXP-PAID TO WS-TOT-EXP
           COMPUTE WS-TOT-ALL =
               WS-POL-MED-PAID + WS-POL-IND-PAID
               + WS-POL-EXP-PAID
           WRITE REPORT-LINE FROM WS-TOTAL-LINE

      *    Reserve totals
           MOVE "OUTSTANDING RESERVES:" TO WS-TOT-LABEL
           MOVE WS-POL-MED-RESERVE TO WS-TOT-MED
           MOVE WS-POL-IND-RESERVE TO WS-TOT-IND
           MOVE WS-POL-EXP-RESERVE TO WS-TOT-EXP
           COMPUTE WS-TOT-ALL =
               WS-POL-MED-RESERVE + WS-POL-IND-RESERVE
               + WS-POL-EXP-RESERVE
           WRITE REPORT-LINE FROM WS-TOTAL-LINE

      *    Incurred totals
           MOVE "TOTAL INCURRED:" TO WS-TOT-LABEL
           COMPUTE WS-TOT-MED =
               WS-POL-MED-PAID + WS-POL-MED-RESERVE
           COMPUTE WS-TOT-IND =
               WS-POL-IND-PAID + WS-POL-IND-RESERVE
           COMPUTE WS-TOT-EXP =
               WS-POL-EXP-PAID + WS-POL-EXP-RESERVE
           COMPUTE WS-TOT-ALL =
               WS-POL-TOTAL-INCURRED
           WRITE REPORT-LINE FROM WS-TOTAL-LINE

      *    Subrogation/Recovery
           STRING "SUBROGATION: " DELIMITED BY SIZE
               WS-POL-SUBRO DELIMITED BY SIZE
               "   RECOVERY: " DELIMITED BY SIZE
               WS-POL-RECOVERY DELIMITED BY SIZE
               INTO REPORT-LINE
           WRITE REPORT-LINE

           STRING "NET INCURRED: " DELIMITED BY SIZE
               WS-POL-NET-INCURRED DELIMITED BY SIZE
               INTO REPORT-LINE
           WRITE REPORT-LINE
           ADD 12 TO WS-LINE-COUNT.

       5200-WRITE-YEAR-TREND.
      *    Write accident year trending
           IF WS-LINE-COUNT >= WS-MAX-LINES - 15
               PERFORM 5000-WRITE-PAGE-HEADER
           END-IF
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE "ACCIDENT YEAR TRENDING" TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE "YEAR   CLAIMS     PAID LOSSES     "
             & "INCURRED LOSSES  DEVELOPED LOSSES "
             & "EARNED PREMIUM  LOSS RATIO"
             TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE ALL "-" TO REPORT-LINE
           WRITE REPORT-LINE

           PERFORM VARYING WS-YEAR-COUNT FROM 1 BY 1
               UNTIL WS-YEAR-COUNT > 10
                   OR WS-YR-ACCIDENT-YEAR(WS-YEAR-COUNT) = 0
               MOVE WS-YR-ACCIDENT-YEAR(WS-YEAR-COUNT)
                   TO WS-YL-YEAR
               MOVE WS-YR-CLAIM-COUNT(WS-YEAR-COUNT)
                   TO WS-YL-CLAIMS
               MOVE WS-YR-PAID(WS-YEAR-COUNT) TO WS-YL-PAID
               MOVE WS-YR-INCURRED(WS-YEAR-COUNT)
                   TO WS-YL-INCURRED
               MOVE WS-YR-DEVELOPED(WS-YEAR-COUNT)
                   TO WS-YL-DEVELOPED
               MOVE WS-YR-PREMIUM(WS-YEAR-COUNT)
                   TO WS-YL-PREMIUM
               IF WS-YR-PREMIUM(WS-YEAR-COUNT) > 0
                   COMPUTE WS-YR-LOSS-RATIO(WS-YEAR-COUNT) =
                       (WS-YR-DEVELOPED(WS-YEAR-COUNT) /
                        WS-YR-PREMIUM(WS-YEAR-COUNT)) * 100
               END-IF
               MOVE WS-YR-LOSS-RATIO(WS-YEAR-COUNT)
                   TO WS-YL-RATIO
               WRITE REPORT-LINE FROM WS-YEAR-LINE
           END-PERFORM.

       5300-WRITE-LARGE-LOSSES.
      *    Write large loss listing
           IF WS-LINE-COUNT >= WS-MAX-LINES - 10
               PERFORM 5000-WRITE-PAGE-HEADER
           END-IF
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           STRING "LARGE LOSSES (>$"
               DELIMITED BY SIZE
               WS-LARGE-LOSS-THRESHOLD DELIMITED BY SIZE
               ")" DELIMITED BY SIZE
               INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE ALL "-" TO REPORT-LINE
           WRITE REPORT-LINE

           PERFORM VARYING WS-LARGE-LOSS-COUNT FROM 1 BY 1
               UNTIL WS-LARGE-LOSS-COUNT > WS-POL-LARGE-LOSS-COUNT
               STRING WS-LL-CLAIM-NUM(WS-LARGE-LOSS-COUNT)
                   DELIMITED BY SIZE
                   "  " DELIMITED BY SIZE
                   WS-LL-CLAIMANT(WS-LARGE-LOSS-COUNT)
                   DELIMITED BY SIZE
                   "  " DELIMITED BY SIZE
                   WS-LL-INJURY-DATE(WS-LARGE-LOSS-COUNT)
                   DELIMITED BY SIZE
                   "  " DELIMITED BY SIZE
                   WS-LL-STATUS(WS-LARGE-LOSS-COUNT)
                   DELIMITED BY SIZE
                   "  INCURRED: $" DELIMITED BY SIZE
                   WS-LL-TOTAL-INCURRED(WS-LARGE-LOSS-COUNT)
                   DELIMITED BY SIZE
                   INTO REPORT-LINE
               WRITE REPORT-LINE
           END-PERFORM.

       5400-WRITE-LOSS-RATIO.
      *    Write loss ratio analysis
           IF WS-LINE-COUNT >= WS-MAX-LINES - 8
               PERFORM 5000-WRITE-PAGE-HEADER
           END-IF
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE ALL "=" TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE "LOSS RATIO ANALYSIS" TO REPORT-LINE
           WRITE REPORT-LINE
           STRING "EARNED PREMIUM:      $"
               DELIMITED BY SIZE
               WS-LR-EARNED-PREMIUM DELIMITED BY SIZE
               INTO REPORT-LINE
           WRITE REPORT-LINE
           STRING "NET INCURRED LOSSES: $"
               DELIMITED BY SIZE
               WS-LR-INCURRED-LOSSES DELIMITED BY SIZE
               INTO REPORT-LINE
           WRITE REPORT-LINE
           STRING "LOSS RATIO:           "
               DELIMITED BY SIZE
               WS-LR-LOSS-RATIO DELIMITED BY SIZE
               "%" DELIMITED BY SIZE
               INTO REPORT-LINE
           WRITE REPORT-LINE
           STRING "MOD FACTOR:           "
               DELIMITED BY SIZE
               POL-MOD-FACTOR DELIMITED BY SIZE
               INTO REPORT-LINE
           WRITE REPORT-LINE.

       9000-TERMINATE.
           CLOSE POLICY-FILE
           CLOSE CLAIM-FILE
           CLOSE CLAIM-FIN-FILE
           CLOSE DEV-FACTOR-FILE
           CLOSE REPORT-FILE
           STOP RUN.
