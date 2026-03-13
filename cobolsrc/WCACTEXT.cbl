       IDENTIFICATION DIVISION.
       PROGRAM-ID. WCACTEXT.
      *================================================================
      * WCACTEXT - Workers' Compensation Actuarial Data Extract
      *
      * Generates actuarial data extracts including loss triangles
      * (paid and incurred by accident year and development period),
      * individual large claim data, exposure data by class code,
      * loss rate calculations, NCCI statistical plan reporting,
      * experience modification factor data, and frequency/severity
      * analysis. Output in fixed-format for actuarial model import.
      *
      * File: CLMMSTF (Claim master)
      * File: CLMFINF (Claim financials)
      * File: EXPOSRF (Exposure/payroll by class code)
      * File: STPLANF (Statistical plan data)
      * Output: ACTXTRI (Triangle extract)
      * Output: ACTXCLM (Large claim extract)
      * Output: ACTXEXP (Exposure extract)
      * Output: ACTXNCI (NCCI statistical extract)
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
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

           SELECT EXPOSURE-FILE
               ASSIGN TO "EXPOSRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS EXP-KEY
               FILE STATUS IS WS-FILE-STATUS.

           SELECT TRIANGLE-OUTPUT
               ASSIGN TO "ACTXTRI"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

           SELECT LARGE-CLAIM-OUTPUT
               ASSIGN TO "ACTXCLM"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

           SELECT EXPOSURE-OUTPUT
               ASSIGN TO "ACTXEXP"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

           SELECT NCCI-OUTPUT
               ASSIGN TO "ACTXNCI"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CLAIM-FILE.
       01  CLAIM-RECORD.
           05 CLM-KEY.
              10 CLM-POLICY-NUM          PIC X(12).
              10 CLM-NUMBER              PIC X(12).
           05 CLM-CLAIMANT-NAME          PIC X(30).
           05 CLM-INJURY-DATE            PIC 9(8).
           05 CLM-REPORT-DATE            PIC 9(8).
           05 CLM-STATUS                 PIC X(2).
           05 CLM-CLASS-CODE             PIC X(4).
           05 CLM-NATURE-INJURY          PIC X(4).
           05 CLM-BODY-PART              PIC X(4).
           05 CLM-CAUSE-INJURY           PIC X(4).
           05 CLM-STATE                  PIC X(2).
           05 CLM-ACCIDENT-YEAR          PIC 9(4).
           05 CLM-CLAIM-TYPE             PIC X(2).
              88 CLM-TYPE-MO             VALUE "MO".
              88 CLM-TYPE-TT             VALUE "TT".
              88 CLM-TYPE-PP             VALUE "PP".
              88 CLM-TYPE-PT             VALUE "PT".
              88 CLM-TYPE-FA             VALUE "FA".

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

       FD  EXPOSURE-FILE.
       01  EXPOSURE-RECORD.
           05 EXP-KEY.
              10 EXP-POLICY-NUM          PIC X(12).
              10 EXP-CLASS-CODE          PIC X(4).
              10 EXP-YEAR                PIC 9(4).
           05 EXP-STATE                  PIC X(2).
           05 EXP-PAYROLL                PIC 9(11)V99.
           05 EXP-EMPLOYEE-COUNT         PIC 9(5).
           05 EXP-MANUAL-RATE            PIC 9(3)V9999.
           05 EXP-MANUAL-PREMIUM         PIC 9(9)V99.
           05 EXP-MOD-FACTOR             PIC 9(1)V9999.
           05 EXP-MOD-PREMIUM            PIC 9(9)V99.

       FD  TRIANGLE-OUTPUT.
       01  TRIANGLE-RECORD.
      *    Fixed format: AY(4) + DevPeriod(3) + PaidLoss(15)
      *    + IncurredLoss(15) + ClaimCount(7) + RecType(1) = 45
           05 TRI-ACCIDENT-YEAR          PIC 9(4).
           05 TRI-DEV-PERIOD             PIC 9(3).
           05 TRI-PAID-LOSS              PIC 9(13)V99.
           05 TRI-INCURRED-LOSS          PIC 9(13)V99.
           05 TRI-CLAIM-COUNT            PIC 9(7).
           05 TRI-REC-TYPE               PIC X(1).

       FD  LARGE-CLAIM-OUTPUT.
       01  LARGE-CLAIM-RECORD.
      *    Fixed format for large claims >$100K
           05 LC-POLICY-NUM              PIC X(12).
           05 LC-CLAIM-NUM               PIC X(12).
           05 LC-ACCIDENT-YEAR           PIC 9(4).
           05 LC-STATE                   PIC X(2).
           05 LC-CLASS-CODE              PIC X(4).
           05 LC-CLAIM-TYPE              PIC X(2).
           05 LC-INJURY-DATE             PIC 9(8).
           05 LC-NATURE-INJURY           PIC X(4).
           05 LC-BODY-PART               PIC X(4).
           05 LC-CAUSE-INJURY            PIC X(4).
           05 LC-STATUS                  PIC X(2).
           05 LC-MEDICAL-PAID            PIC 9(11)V99.
           05 LC-MEDICAL-RESERVE         PIC 9(11)V99.
           05 LC-INDEMNITY-PAID          PIC 9(11)V99.
           05 LC-INDEMNITY-RESERVE       PIC 9(11)V99.
           05 LC-EXPENSE-PAID            PIC 9(11)V99.
           05 LC-EXPENSE-RESERVE         PIC 9(11)V99.
           05 LC-TOTAL-INCURRED          PIC 9(11)V99.

       FD  EXPOSURE-OUTPUT.
       01  EXPOSURE-OUT-RECORD.
      *    Fixed format exposure data
           05 EO-POLICY-NUM              PIC X(12).
           05 EO-STATE                   PIC X(2).
           05 EO-CLASS-CODE              PIC X(4).
           05 EO-YEAR                    PIC 9(4).
           05 EO-PAYROLL                 PIC 9(13)V99.
           05 EO-EMPLOYEES               PIC 9(7).
           05 EO-MANUAL-RATE             PIC 9(5)V9999.
           05 EO-MANUAL-PREMIUM          PIC 9(11)V99.
           05 EO-CLAIMS                  PIC 9(7).
           05 EO-LOSSES                  PIC 9(13)V99.
           05 EO-LOSS-RATE               PIC 9(5)V9999.
           05 EO-FREQUENCY               PIC 9(3)V9999.

       FD  NCCI-OUTPUT.
       01  NCCI-RECORD.
      *    NCCI Unit Statistical Plan format (simplified)
           05 NCCI-CARRIER-CODE          PIC X(5).
           05 NCCI-POLICY-NUM            PIC X(12).
           05 NCCI-STATE                 PIC X(2).
           05 NCCI-CLASS-CODE            PIC X(4).
           05 NCCI-EXPOSURE-AMT          PIC 9(11)V99.
           05 NCCI-CLAIM-COUNT           PIC 9(5).
           05 NCCI-MED-PAID              PIC 9(9)V99.
           05 NCCI-MED-INCURRED          PIC 9(9)V99.
           05 NCCI-IND-PAID              PIC 9(9)V99.
           05 NCCI-IND-INCURRED          PIC 9(9)V99.
           05 NCCI-ALAE-PAID             PIC 9(9)V99.
           05 NCCI-ALAE-INCURRED         PIC 9(9)V99.
           05 NCCI-VAL-DATE              PIC 9(8).
           05 NCCI-REPORT-LEVEL          PIC 9(1).

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

       01  WS-PARAMS.
           05 WS-POLICY-NUM              PIC X(12).
           05 WS-CARRIER-CODE            PIC X(5) VALUE "99999".
           05 WS-VALUATION-DATE          PIC 9(8).
           05 WS-OLDEST-AY               PIC 9(4).
           05 WS-CURRENT-AY              PIC 9(4).
           05 WS-LARGE-CLAIM-THRESH      PIC 9(9)V99 VALUE 100000.00.

      * Triangle accumulation (10 accident years x 12 dev periods)
       01  WS-TRIANGLE-DATA.
           05 WS-TRI-YEAR OCCURS 10 TIMES.
              10 WS-TRI-AY               PIC 9(4).
              10 WS-TRI-PERIOD OCCURS 12 TIMES.
                 15 WS-TRI-PAID          PIC 9(13)V99.
                 15 WS-TRI-INCURRED      PIC 9(13)V99.
                 15 WS-TRI-COUNT         PIC 9(7).
       01  WS-TRI-YEAR-IDX              PIC 9(2).
       01  WS-TRI-PER-IDX               PIC 9(2).

      * Class code accumulation for loss rates
       01  WS-CLASS-TABLE.
           05 WS-CLASS-ENTRY OCCURS 100 TIMES.
              10 WS-CLS-CODE             PIC X(4).
              10 WS-CLS-STATE            PIC X(2).
              10 WS-CLS-PAYROLL          PIC 9(13)V99.
              10 WS-CLS-EMPLOYEES        PIC 9(7).
              10 WS-CLS-CLAIM-COUNT      PIC 9(7).
              10 WS-CLS-TOTAL-LOSSES     PIC 9(13)V99.
              10 WS-CLS-LOSS-RATE        PIC 9(5)V9999.
              10 WS-CLS-FREQUENCY        PIC 9(3)V9999.
       01  WS-CLASS-COUNT                PIC 9(3) VALUE 0.

      * NCCI accumulation
       01  WS-NCCI-TABLE.
           05 WS-NCCI-ENTRY OCCURS 100 TIMES.
              10 WS-NCI-CLASS            PIC X(4).
              10 WS-NCI-STATE            PIC X(2).
              10 WS-NCI-EXPOSURE         PIC 9(11)V99.
              10 WS-NCI-CLAIMS           PIC 9(5).
              10 WS-NCI-MED-PD           PIC 9(9)V99.
              10 WS-NCI-MED-INC          PIC 9(9)V99.
              10 WS-NCI-IND-PD           PIC 9(9)V99.
              10 WS-NCI-IND-INC          PIC 9(9)V99.
              10 WS-NCI-ALAE-PD          PIC 9(9)V99.
              10 WS-NCI-ALAE-INC         PIC 9(9)V99.
       01  WS-NCCI-COUNT                 PIC 9(3) VALUE 0.

      * Work fields
       01  WS-WORK.
           05 WS-TOTAL-INCURRED          PIC 9(13)V99.
           05 WS-DEV-MONTHS              PIC 9(3).
           05 WS-DEV-PERIOD              PIC 9(2).
           05 WS-YEAR-IDX                PIC 9(2).
           05 WS-CLASS-IDX               PIC 9(3).
           05 WS-NCCI-IDX                PIC 9(3).
           05 WS-FOUND-FLAG              PIC X(1).
           05 WS-CLAIM-COUNTER           PIC 9(7) VALUE 0.
           05 WS-LARGE-CLAIM-CTR         PIC 9(5) VALUE 0.
           05 WS-TRI-REC-CTR             PIC 9(7) VALUE 0.

      * Frequency/severity
       01  WS-FREQ-SEV.
           05 WS-TOTAL-CLAIMS            PIC 9(7) VALUE 0.
           05 WS-TOTAL-PAYROLL           PIC 9(15)V99 VALUE 0.
           05 WS-TOTAL-LOSSES            PIC 9(15)V99 VALUE 0.
           05 WS-OVERALL-FREQUENCY       PIC 9(3)V9999.
           05 WS-OVERALL-SEVERITY        PIC 9(9)V99.
           05 WS-OVERALL-PURE-PREM       PIC 9(7)V99.

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-EXTRACT-CLAIM-DATA
           PERFORM 3000-EXTRACT-EXPOSURE-DATA
           PERFORM 4000-WRITE-TRIANGLES
           PERFORM 5000-WRITE-NCCI-EXTRACT
           PERFORM 6000-WRITE-FREQ-SEV-SUMMARY
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           OPEN INPUT CLAIM-FILE
           OPEN INPUT CLAIM-FIN-FILE
           OPEN INPUT EXPOSURE-FILE
           OPEN OUTPUT TRIANGLE-OUTPUT
           OPEN OUTPUT LARGE-CLAIM-OUTPUT
           OPEN OUTPUT EXPOSURE-OUTPUT
           OPEN OUTPUT NCCI-OUTPUT
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE WS-CURRENT-DATE TO WS-VALUATION-DATE
           MOVE WS-CURRENT-DATE(1:4) TO WS-CURRENT-AY
           COMPUTE WS-OLDEST-AY = WS-CURRENT-AY - 10
           INITIALIZE WS-TRIANGLE-DATA
           INITIALIZE WS-CLASS-TABLE
           INITIALIZE WS-NCCI-TABLE.

       2000-EXTRACT-CLAIM-DATA.
      *    Read all claims and build triangle + large claim data
           MOVE WS-POLICY-NUM TO CLM-POLICY-NUM
           MOVE SPACES TO CLM-NUMBER
           START CLAIM-FILE KEY >= CLM-KEY
               INVALID KEY
                   GO TO 2000-EXIT
           END-START

           MOVE "N" TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ CLAIM-FILE NEXT
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       IF CLM-POLICY-NUM NOT = WS-POLICY-NUM
                           SET WS-EOF TO TRUE
                       ELSE
                           PERFORM 2100-PROCESS-CLAIM
                       END-IF
               END-READ
           END-PERFORM.
       2000-EXIT.
           EXIT.

       2100-PROCESS-CLAIM.
      *    Read financial data for this claim
           MOVE CLM-POLICY-NUM TO FIN-POLICY-NUM
           MOVE CLM-NUMBER TO FIN-CLAIM-NUM
           READ CLAIM-FIN-FILE
               INVALID KEY
                   GO TO 2100-EXIT
           END-READ

           ADD 1 TO WS-CLAIM-COUNTER

      *    Calculate total incurred
           COMPUTE WS-TOTAL-INCURRED =
               FIN-MEDICAL-PAID + FIN-MEDICAL-RESERVE
               + FIN-INDEMNITY-PAID + FIN-INDEMNITY-RESERVE
               + FIN-EXPENSE-PAID + FIN-EXPENSE-RESERVE
               - FIN-SUBROGATION - FIN-RECOVERY

      *    Determine development period (months from injury to valuation)
           COMPUTE WS-DEV-MONTHS =
               (FUNCTION INTEGER-OF-DATE(WS-VALUATION-DATE)
               - FUNCTION INTEGER-OF-DATE(CLM-INJURY-DATE))
               / 30
           COMPUTE WS-DEV-PERIOD =
               (WS-DEV-MONTHS / 12) + 1
           IF WS-DEV-PERIOD > 12
               MOVE 12 TO WS-DEV-PERIOD
           END-IF

      *    Accumulate into triangle
           IF CLM-ACCIDENT-YEAR >= WS-OLDEST-AY
               PERFORM 2200-ACCUM-TRIANGLE
           END-IF

      *    Check large claim threshold
           IF WS-TOTAL-INCURRED >= WS-LARGE-CLAIM-THRESH
               PERFORM 2300-WRITE-LARGE-CLAIM
           END-IF

      *    Accumulate for class code loss rates
           PERFORM 2400-ACCUM-CLASS-LOSSES

      *    Accumulate NCCI data
           PERFORM 2500-ACCUM-NCCI.
       2100-EXIT.
           EXIT.

       2200-ACCUM-TRIANGLE.
      *    Find year index in triangle array
           COMPUTE WS-YEAR-IDX =
               CLM-ACCIDENT-YEAR - WS-OLDEST-AY + 1
           IF WS-YEAR-IDX < 1 OR WS-YEAR-IDX > 10
               GO TO 2200-EXIT
           END-IF

           MOVE CLM-ACCIDENT-YEAR
               TO WS-TRI-AY(WS-YEAR-IDX)

           IF WS-DEV-PERIOD >= 1 AND WS-DEV-PERIOD <= 12
               ADD FIN-MEDICAL-PAID
                   TO WS-TRI-PAID(WS-YEAR-IDX, WS-DEV-PERIOD)
               ADD FIN-INDEMNITY-PAID
                   TO WS-TRI-PAID(WS-YEAR-IDX, WS-DEV-PERIOD)
               ADD FIN-EXPENSE-PAID
                   TO WS-TRI-PAID(WS-YEAR-IDX, WS-DEV-PERIOD)
               ADD WS-TOTAL-INCURRED
                   TO WS-TRI-INCURRED(WS-YEAR-IDX, WS-DEV-PERIOD)
               ADD 1 TO WS-TRI-COUNT(WS-YEAR-IDX, WS-DEV-PERIOD)
           END-IF.
       2200-EXIT.
           EXIT.

       2300-WRITE-LARGE-CLAIM.
      *    Write individual large claim record
           ADD 1 TO WS-LARGE-CLAIM-CTR
           MOVE CLM-POLICY-NUM TO LC-POLICY-NUM
           MOVE CLM-NUMBER TO LC-CLAIM-NUM
           MOVE CLM-ACCIDENT-YEAR TO LC-ACCIDENT-YEAR
           MOVE CLM-STATE TO LC-STATE
           MOVE CLM-CLASS-CODE TO LC-CLASS-CODE
           MOVE CLM-CLAIM-TYPE TO LC-CLAIM-TYPE
           MOVE CLM-INJURY-DATE TO LC-INJURY-DATE
           MOVE CLM-NATURE-INJURY TO LC-NATURE-INJURY
           MOVE CLM-BODY-PART TO LC-BODY-PART
           MOVE CLM-CAUSE-INJURY TO LC-CAUSE-INJURY
           MOVE CLM-STATUS TO LC-STATUS
           MOVE FIN-MEDICAL-PAID TO LC-MEDICAL-PAID
           MOVE FIN-MEDICAL-RESERVE TO LC-MEDICAL-RESERVE
           MOVE FIN-INDEMNITY-PAID TO LC-INDEMNITY-PAID
           MOVE FIN-INDEMNITY-RESERVE TO LC-INDEMNITY-RESERVE
           MOVE FIN-EXPENSE-PAID TO LC-EXPENSE-PAID
           MOVE FIN-EXPENSE-RESERVE TO LC-EXPENSE-RESERVE
           MOVE WS-TOTAL-INCURRED TO LC-TOTAL-INCURRED
           WRITE LARGE-CLAIM-RECORD.

       2400-ACCUM-CLASS-LOSSES.
      *    Find or create class code entry
           MOVE "N" TO WS-FOUND-FLAG
           PERFORM VARYING WS-CLASS-IDX FROM 1 BY 1
               UNTIL WS-CLASS-IDX > WS-CLASS-COUNT
                   OR WS-FOUND-FLAG = "Y"
               IF WS-CLS-CODE(WS-CLASS-IDX) = CLM-CLASS-CODE
                   AND WS-CLS-STATE(WS-CLASS-IDX) = CLM-STATE
                   MOVE "Y" TO WS-FOUND-FLAG
               END-IF
           END-PERFORM

           IF WS-FOUND-FLAG = "N"
               ADD 1 TO WS-CLASS-COUNT
               MOVE WS-CLASS-COUNT TO WS-CLASS-IDX
               MOVE CLM-CLASS-CODE TO WS-CLS-CODE(WS-CLASS-IDX)
               MOVE CLM-STATE TO WS-CLS-STATE(WS-CLASS-IDX)
               MOVE ZEROS TO WS-CLS-CLAIM-COUNT(WS-CLASS-IDX)
               MOVE ZEROS TO WS-CLS-TOTAL-LOSSES(WS-CLASS-IDX)
           ELSE
               SUBTRACT 1 FROM WS-CLASS-IDX
           END-IF

           ADD 1 TO WS-CLS-CLAIM-COUNT(WS-CLASS-IDX)
           ADD WS-TOTAL-INCURRED
               TO WS-CLS-TOTAL-LOSSES(WS-CLASS-IDX)

      *    Frequency/severity totals
           ADD 1 TO WS-TOTAL-CLAIMS
           ADD WS-TOTAL-INCURRED TO WS-TOTAL-LOSSES.

       2500-ACCUM-NCCI.
      *    Accumulate NCCI unit statistical data
           MOVE "N" TO WS-FOUND-FLAG
           PERFORM VARYING WS-NCCI-IDX FROM 1 BY 1
               UNTIL WS-NCCI-IDX > WS-NCCI-COUNT
                   OR WS-FOUND-FLAG = "Y"
               IF WS-NCI-CLASS(WS-NCCI-IDX) = CLM-CLASS-CODE
                   AND WS-NCI-STATE(WS-NCCI-IDX) = CLM-STATE
                   MOVE "Y" TO WS-FOUND-FLAG
               END-IF
           END-PERFORM

           IF WS-FOUND-FLAG = "N"
               ADD 1 TO WS-NCCI-COUNT
               MOVE WS-NCCI-COUNT TO WS-NCCI-IDX
               MOVE CLM-CLASS-CODE TO WS-NCI-CLASS(WS-NCCI-IDX)
               MOVE CLM-STATE TO WS-NCI-STATE(WS-NCCI-IDX)
               INITIALIZE WS-NCCI-ENTRY(WS-NCCI-IDX)
               MOVE CLM-CLASS-CODE TO WS-NCI-CLASS(WS-NCCI-IDX)
               MOVE CLM-STATE TO WS-NCI-STATE(WS-NCCI-IDX)
           ELSE
               SUBTRACT 1 FROM WS-NCCI-IDX
           END-IF

           ADD 1 TO WS-NCI-CLAIMS(WS-NCCI-IDX)
           ADD FIN-MEDICAL-PAID TO WS-NCI-MED-PD(WS-NCCI-IDX)
           COMPUTE WS-NCI-MED-INC(WS-NCCI-IDX) =
               WS-NCI-MED-INC(WS-NCCI-IDX)
               + FIN-MEDICAL-PAID + FIN-MEDICAL-RESERVE
           ADD FIN-INDEMNITY-PAID TO WS-NCI-IND-PD(WS-NCCI-IDX)
           COMPUTE WS-NCI-IND-INC(WS-NCCI-IDX) =
               WS-NCI-IND-INC(WS-NCCI-IDX)
               + FIN-INDEMNITY-PAID + FIN-INDEMNITY-RESERVE
           ADD FIN-EXPENSE-PAID TO WS-NCI-ALAE-PD(WS-NCCI-IDX)
           COMPUTE WS-NCI-ALAE-INC(WS-NCCI-IDX) =
               WS-NCI-ALAE-INC(WS-NCCI-IDX)
               + FIN-EXPENSE-PAID + FIN-EXPENSE-RESERVE.

       3000-EXTRACT-EXPOSURE-DATA.
      *    Read exposure file and write exposure extract
           MOVE WS-POLICY-NUM TO EXP-POLICY-NUM
           MOVE SPACES TO EXP-CLASS-CODE
           MOVE ZEROS TO EXP-YEAR
           START EXPOSURE-FILE KEY >= EXP-KEY
               INVALID KEY
                   GO TO 3000-EXIT
           END-START

           MOVE "N" TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ EXPOSURE-FILE NEXT
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       IF EXP-POLICY-NUM NOT = WS-POLICY-NUM
                           SET WS-EOF TO TRUE
                       ELSE
                           PERFORM 3100-PROCESS-EXPOSURE
                       END-IF
               END-READ
           END-PERFORM.
       3000-EXIT.
           EXIT.

       3100-PROCESS-EXPOSURE.
      *    Calculate loss rate and frequency for class code
      *    Match to class code loss accumulator
           MOVE "N" TO WS-FOUND-FLAG
           PERFORM VARYING WS-CLASS-IDX FROM 1 BY 1
               UNTIL WS-CLASS-IDX > WS-CLASS-COUNT
                   OR WS-FOUND-FLAG = "Y"
               IF WS-CLS-CODE(WS-CLASS-IDX) = EXP-CLASS-CODE
                   AND WS-CLS-STATE(WS-CLASS-IDX) = EXP-STATE
                   MOVE "Y" TO WS-FOUND-FLAG
               END-IF
           END-PERFORM

           IF WS-FOUND-FLAG = "Y"
               SUBTRACT 1 FROM WS-CLASS-IDX
               MOVE EXP-PAYROLL
                   TO WS-CLS-PAYROLL(WS-CLASS-IDX)
               MOVE EXP-EMPLOYEE-COUNT
                   TO WS-CLS-EMPLOYEES(WS-CLASS-IDX)

      *        Loss rate = losses / (payroll / 100)
               IF EXP-PAYROLL > 0
                   COMPUTE WS-CLS-LOSS-RATE(WS-CLASS-IDX) =
                       (WS-CLS-TOTAL-LOSSES(WS-CLASS-IDX)
                       / (EXP-PAYROLL / 100))
               END-IF

      *        Frequency = claims / (payroll / 1,000,000)
               IF EXP-PAYROLL > 0
                   COMPUTE WS-CLS-FREQUENCY(WS-CLASS-IDX) =
                       (WS-CLS-CLAIM-COUNT(WS-CLASS-IDX)
                       / (EXP-PAYROLL / 1000000))
               END-IF
           END-IF

      *    Write exposure output record
           MOVE EXP-POLICY-NUM TO EO-POLICY-NUM
           MOVE EXP-STATE TO EO-STATE
           MOVE EXP-CLASS-CODE TO EO-CLASS-CODE
           MOVE EXP-YEAR TO EO-YEAR
           MOVE EXP-PAYROLL TO EO-PAYROLL
           MOVE EXP-EMPLOYEE-COUNT TO EO-EMPLOYEES
           MOVE EXP-MANUAL-RATE TO EO-MANUAL-RATE
           MOVE EXP-MANUAL-PREMIUM TO EO-MANUAL-PREMIUM
           IF WS-FOUND-FLAG = "Y"
               MOVE WS-CLS-CLAIM-COUNT(WS-CLASS-IDX) TO EO-CLAIMS
               MOVE WS-CLS-TOTAL-LOSSES(WS-CLASS-IDX) TO EO-LOSSES
               MOVE WS-CLS-LOSS-RATE(WS-CLASS-IDX) TO EO-LOSS-RATE
               MOVE WS-CLS-FREQUENCY(WS-CLASS-IDX)
                   TO EO-FREQUENCY
           ELSE
               MOVE ZEROS TO EO-CLAIMS
               MOVE ZEROS TO EO-LOSSES
               MOVE ZEROS TO EO-LOSS-RATE
               MOVE ZEROS TO EO-FREQUENCY
           END-IF
           WRITE EXPOSURE-OUT-RECORD

      *    Accumulate overall payroll for freq/sev
           ADD EXP-PAYROLL TO WS-TOTAL-PAYROLL

      *    Match NCCI entries to exposure
           PERFORM VARYING WS-NCCI-IDX FROM 1 BY 1
               UNTIL WS-NCCI-IDX > WS-NCCI-COUNT
               IF WS-NCI-CLASS(WS-NCCI-IDX) = EXP-CLASS-CODE
                   AND WS-NCI-STATE(WS-NCCI-IDX) = EXP-STATE
                   MOVE EXP-PAYROLL
                       TO WS-NCI-EXPOSURE(WS-NCCI-IDX)
               END-IF
           END-PERFORM.

       4000-WRITE-TRIANGLES.
      *    Write triangle records -- cumulative by development period
           PERFORM VARYING WS-TRI-YEAR-IDX FROM 1 BY 1
               UNTIL WS-TRI-YEAR-IDX > 10
                   OR WS-TRI-AY(WS-TRI-YEAR-IDX) = 0
               PERFORM VARYING WS-TRI-PER-IDX FROM 1 BY 1
                   UNTIL WS-TRI-PER-IDX > 12
                   IF WS-TRI-COUNT(WS-TRI-YEAR-IDX,
                                    WS-TRI-PER-IDX) > 0
                       MOVE WS-TRI-AY(WS-TRI-YEAR-IDX)
                           TO TRI-ACCIDENT-YEAR
                       MOVE WS-TRI-PER-IDX TO TRI-DEV-PERIOD
                       MOVE WS-TRI-PAID(WS-TRI-YEAR-IDX,
                                         WS-TRI-PER-IDX)
                           TO TRI-PAID-LOSS
                       MOVE WS-TRI-INCURRED(WS-TRI-YEAR-IDX,
                                             WS-TRI-PER-IDX)
                           TO TRI-INCURRED-LOSS
                       MOVE WS-TRI-COUNT(WS-TRI-YEAR-IDX,
                                          WS-TRI-PER-IDX)
                           TO TRI-CLAIM-COUNT
                       MOVE "C" TO TRI-REC-TYPE
                       WRITE TRIANGLE-RECORD
                       ADD 1 TO WS-TRI-REC-CTR
                   END-IF
               END-PERFORM
           END-PERFORM.

       5000-WRITE-NCCI-EXTRACT.
      *    Write NCCI unit statistical plan records
           PERFORM VARYING WS-NCCI-IDX FROM 1 BY 1
               UNTIL WS-NCCI-IDX > WS-NCCI-COUNT
               MOVE WS-CARRIER-CODE TO NCCI-CARRIER-CODE
               MOVE WS-POLICY-NUM TO NCCI-POLICY-NUM
               MOVE WS-NCI-STATE(WS-NCCI-IDX) TO NCCI-STATE
               MOVE WS-NCI-CLASS(WS-NCCI-IDX) TO NCCI-CLASS-CODE
               MOVE WS-NCI-EXPOSURE(WS-NCCI-IDX)
                   TO NCCI-EXPOSURE-AMT
               MOVE WS-NCI-CLAIMS(WS-NCCI-IDX) TO NCCI-CLAIM-COUNT
               MOVE WS-NCI-MED-PD(WS-NCCI-IDX) TO NCCI-MED-PAID
               MOVE WS-NCI-MED-INC(WS-NCCI-IDX) TO NCCI-MED-INCURRED
               MOVE WS-NCI-IND-PD(WS-NCCI-IDX) TO NCCI-IND-PAID
               MOVE WS-NCI-IND-INC(WS-NCCI-IDX) TO NCCI-IND-INCURRED
               MOVE WS-NCI-ALAE-PD(WS-NCCI-IDX) TO NCCI-ALAE-PAID
               MOVE WS-NCI-ALAE-INC(WS-NCCI-IDX)
                   TO NCCI-ALAE-INCURRED
               MOVE WS-VALUATION-DATE TO NCCI-VAL-DATE
               MOVE 1 TO NCCI-REPORT-LEVEL
               WRITE NCCI-RECORD
           END-PERFORM.

       6000-WRITE-FREQ-SEV-SUMMARY.
      *    Calculate and write frequency/severity summary
      *    (Written as header comment in triangle output)
           IF WS-TOTAL-PAYROLL > 0
               COMPUTE WS-OVERALL-FREQUENCY =
                   WS-TOTAL-CLAIMS /
                   (WS-TOTAL-PAYROLL / 1000000)
           END-IF
           IF WS-TOTAL-CLAIMS > 0
               COMPUTE WS-OVERALL-SEVERITY =
                   WS-TOTAL-LOSSES / WS-TOTAL-CLAIMS
           END-IF
           IF WS-TOTAL-PAYROLL > 0
               COMPUTE WS-OVERALL-PURE-PREM =
                   WS-TOTAL-LOSSES /
                   (WS-TOTAL-PAYROLL / 100)
           END-IF

      *    Write summary as final triangle record (type S)
           MOVE 9999 TO TRI-ACCIDENT-YEAR
           MOVE 0 TO TRI-DEV-PERIOD
           MOVE WS-TOTAL-LOSSES TO TRI-PAID-LOSS
           MOVE WS-TOTAL-LOSSES TO TRI-INCURRED-LOSS
           MOVE WS-TOTAL-CLAIMS TO TRI-CLAIM-COUNT
           MOVE "S" TO TRI-REC-TYPE
           WRITE TRIANGLE-RECORD.

       9000-TERMINATE.
           CLOSE CLAIM-FILE
           CLOSE CLAIM-FIN-FILE
           CLOSE EXPOSURE-FILE
           CLOSE TRIANGLE-OUTPUT
           CLOSE LARGE-CLAIM-OUTPUT
           CLOSE EXPOSURE-OUTPUT
           CLOSE NCCI-OUTPUT
           STOP RUN.
