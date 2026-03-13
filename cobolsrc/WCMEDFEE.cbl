       IDENTIFICATION DIVISION.
       PROGRAM-ID. WCMEDFEE.
      *================================================================*
      * WCMEDFEE - Medical Fee Schedule Lookup Program                  *
      *                                                                 *
      * Provides comprehensive medical fee schedule lookups for         *
      * workers' compensation bill review. Handles state-specific       *
      * fee schedules, Medicare-based calculations, DRG-based           *
      * hospital reimbursement, pharmacy formulary checks, and          *
      * geographic adjustment factors.                                  *
      *                                                                 *
      * PATHWAY: WC-MEDFEE-SVR                                         *
      * FILES:                                                          *
      *   $DATA1.WCFILES.FEESCHED  - CPT/HCPCS fee schedule            *
      *   $DATA1.WCFILES.DRGTBL   - DRG reimbursement table            *
      *   $DATA1.WCFILES.PHARMNDC - Pharmacy NDC formulary             *
      *   $DATA1.WCFILES.GAFTBL   - Geographic adjustment factors      *
      *   $DATA1.WCFILES.MODTBL   - Procedure modifier rules           *
      *                                                                 *
      * MODIFICATION LOG:                                               *
      * DATE       PROGRAMMER   DESCRIPTION                             *
      * ---------- ------------ --------------------------------------- *
      * 2024-04-10 J.WILLIAMS   INITIAL DEVELOPMENT                    *
      * 2024-07-22 J.WILLIAMS   MODIFIER APPLICATION LOGIC             *
      * 2024-10-05 S.CHEN       PHARMACY FORMULARY CHECKING            *
      * 2025-01-15 J.WILLIAMS   DRG HOSPITAL REIMBURSEMENT             *
      * 2025-03-10 K.PATEL      GEOGRAPHIC ADJUSTMENT FACTORS          *
      *================================================================*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. TANDEM.
       OBJECT-COMPUTER. TANDEM.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FEE-SCHED-FILE
               ASSIGN TO "$DATA1.WCFILES.FEESCHED"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FS-KEY
               FILE STATUS IS WS-FS-STATUS.

           SELECT DRG-TABLE-FILE
               ASSIGN TO "$DATA1.WCFILES.DRGTBL"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS DRG-KEY
               FILE STATUS IS WS-DRG-STATUS.

           SELECT PHARM-NDC-FILE
               ASSIGN TO "$DATA1.WCFILES.PHARMNDC"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS NDC-KEY
               FILE STATUS IS WS-NDC-STATUS.

           SELECT GAF-TABLE-FILE
               ASSIGN TO "$DATA1.WCFILES.GAFTBL"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS GAF-KEY
               FILE STATUS IS WS-GAF-STATUS.

           SELECT MODIFIER-FILE
               ASSIGN TO "$DATA1.WCFILES.MODTBL"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS MOD-KEY
               FILE STATUS IS WS-MOD-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  FEE-SCHED-FILE.
       01  FEE-SCHED-RECORD.
           05  FS-KEY.
               10  FS-STATE-CODE          PIC X(02).
               10  FS-CPT-CODE            PIC X(05).
               10  FS-EFF-DATE            PIC 9(08).
           05  FS-DESCRIPTION             PIC X(50).
           05  FS-MAX-ALLOWABLE           PIC S9(07)V99 COMP-3.
           05  FS-PROFESSIONAL-FEE        PIC S9(07)V99 COMP-3.
           05  FS-FACILITY-FEE            PIC S9(07)V99 COMP-3.
           05  FS-MEDICARE-BASE-RATE      PIC S9(07)V99 COMP-3.
           05  FS-MEDICARE-PCT            PIC S9(03)V99 COMP-3.
           05  FS-FEE-TYPE                PIC X(01).
      *        S = State-specific schedule
      *        M = Medicare-based (% of Medicare)
      *        U = Usual and customary
      *        R = RBRVS-based
           05  FS-RVU-WORK               PIC S9(03)V99 COMP-3.
           05  FS-RVU-PRACTICE           PIC S9(03)V99 COMP-3.
           05  FS-RVU-MALPRACTICE        PIC S9(03)V99 COMP-3.
           05  FS-CONVERSION-FACTOR       PIC S9(05)V99 COMP-3.
           05  FS-STATUS-FLAG             PIC X(01).
      *        A = Active, I = Inactive, D = Deleted

       FD  DRG-TABLE-FILE.
       01  DRG-TABLE-RECORD.
           05  DRG-KEY.
               10  DRG-STATE-CODE         PIC X(02).
               10  DRG-CODE               PIC X(04).
           05  DRG-DESCRIPTION            PIC X(50).
           05  DRG-BASE-RATE              PIC S9(09)V99 COMP-3.
           05  DRG-WEIGHT                 PIC S9(03)V9999 COMP-3.
           05  DRG-GEOMETRIC-LOS          PIC S9(03)V9 COMP-3.
           05  DRG-OUTLIER-THRESHOLD      PIC S9(09)V99 COMP-3.
           05  DRG-OUTLIER-PCT            PIC S9(03)V99 COMP-3.

       FD  PHARM-NDC-FILE.
       01  PHARM-NDC-RECORD.
           05  NDC-KEY.
               10  NDC-STATE-CODE         PIC X(02).
               10  NDC-CODE               PIC X(11).
           05  NDC-DRUG-NAME              PIC X(40).
           05  NDC-GENERIC-NAME           PIC X(40).
           05  NDC-FORMULARY-STATUS       PIC X(01).
      *        F = Formulary, N = Non-formulary, P = Prior auth req
      *        X = Excluded (not covered)
           05  NDC-MAX-PRICE              PIC S9(07)V99 COMP-3.
           05  NDC-DISPENSING-FEE         PIC S9(05)V99 COMP-3.
           05  NDC-AWP                    PIC S9(07)V99 COMP-3.
           05  NDC-AWP-DISCOUNT-PCT       PIC S9(03)V99 COMP-3.
           05  NDC-THERAPEUTIC-CLASS      PIC X(06).
           05  NDC-SCHEDULE               PIC X(01).
      *        2-5 = DEA schedule, 0 = non-controlled

       FD  GAF-TABLE-FILE.
       01  GAF-TABLE-RECORD.
           05  GAF-KEY.
               10  GAF-STATE-CODE         PIC X(02).
               10  GAF-ZIP-PREFIX         PIC X(03).
           05  GAF-WORK-FACTOR            PIC S9(01)V9999 COMP-3.
           05  GAF-PRACTICE-FACTOR        PIC S9(01)V9999 COMP-3.
           05  GAF-MALPRACTICE-FACTOR     PIC S9(01)V9999 COMP-3.
           05  GAF-COMPOSITE-FACTOR       PIC S9(01)V9999 COMP-3.

       FD  MODIFIER-FILE.
       01  MODIFIER-RECORD.
           05  MOD-KEY.
               10  MOD-STATE-CODE         PIC X(02).
               10  MOD-CODE               PIC X(02).
           05  MOD-DESCRIPTION            PIC X(30).
           05  MOD-PCT-ADJUSTMENT         PIC S9(03)V99 COMP-3.
           05  MOD-APPLIES-TO             PIC X(01).
      *        P = Professional only, F = Facility only, B = Both
           05  MOD-STACKING-ALLOWED       PIC X(01).
      *        Y = Can stack with other modifiers, N = No stacking

       WORKING-STORAGE SECTION.
      *
      *    Cross-program copybook references
           COPY WCCLMCPY
           COPY WCPRVCPY

       01  WS-FILE-STATUSES.
           05  WS-FS-STATUS              PIC X(02).
           05  WS-DRG-STATUS             PIC X(02).
           05  WS-NDC-STATUS             PIC X(02).
           05  WS-GAF-STATUS             PIC X(02).
           05  WS-MOD-STATUS             PIC X(02).

       01  WS-REQUEST-AREA.
           05  WS-REQ-TYPE               PIC 9(02).
      *        01 = Professional fee lookup
      *        02 = Facility fee lookup
      *        03 = DRG hospital lookup
      *        04 = Pharmacy NDC lookup
      *        05 = Fee with modifiers
      *        06 = Fee with GAF adjustment
           05  WS-REQ-STATE              PIC X(02).
           05  WS-REQ-CPT-CODE           PIC X(05).
           05  WS-REQ-DRG-CODE           PIC X(04).
           05  WS-REQ-NDC-CODE           PIC X(11).
           05  WS-REQ-ZIP-CODE           PIC X(05).
           05  WS-REQ-SERVICE-DATE       PIC 9(08).
           05  WS-REQ-BILLED-AMT         PIC S9(09)V99 COMP-3.
           05  WS-REQ-UNITS              PIC S9(03) COMP-3.
           05  WS-REQ-PLACE-OF-SVC       PIC X(02).
           05  WS-REQ-MODIFIER-1         PIC X(02).
           05  WS-REQ-MODIFIER-2         PIC X(02).
           05  WS-REQ-MODIFIER-3         PIC X(02).
           05  WS-REQ-LOS-DAYS           PIC S9(03) COMP-3.

       01  WS-RESULT-AREA.
           05  WS-RES-STATUS             PIC 9(02).
      *        00 = Found, 01 = Not found, 02 = Inactive
      *        03 = Non-formulary, 04 = Excluded
           05  WS-RES-MAX-ALLOWABLE      PIC S9(09)V99 COMP-3.
           05  WS-RES-RECOMMENDED-PAY    PIC S9(09)V99 COMP-3.
           05  WS-RES-REDUCTION-AMT      PIC S9(09)V99 COMP-3.
           05  WS-RES-REDUCTION-REASON   PIC X(30).
           05  WS-RES-FEE-TYPE           PIC X(01).
           05  WS-RES-FORMULARY-STATUS   PIC X(01).

       01  WS-WORK-FIELDS.
           05  WS-CALCULATED-FEE         PIC S9(09)V99 COMP-3.
           05  WS-MODIFIER-ADJ-PCT       PIC S9(03)V99 COMP-3.
           05  WS-GAF-ADJ-FACTOR         PIC S9(01)V9999 COMP-3.
           05  WS-RBRVS-FEE              PIC S9(09)V99 COMP-3.
           05  WS-OUTLIER-AMT            PIC S9(09)V99 COMP-3.
           05  WS-NDC-ALLOWED-PRICE      PIC S9(07)V99 COMP-3.

       01  WS-PATHSEND-FIELDS.
           05  WS-SERVER-NAME            PIC X(24).
           05  WS-PS-ERROR               PIC S9(04) COMP.

       PROCEDURE DIVISION.

       0000-MAIN-PROCESS.
      *
      *    Inter-program communication calls
           PATHSEND USING "MEDBILL"
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-REQUESTS
               UNTIL WS-PS-ERROR NOT = ZERO
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           OPEN INPUT FEE-SCHED-FILE
                      DRG-TABLE-FILE
                      PHARM-NDC-FILE
                      GAF-TABLE-FILE
                      MODIFIER-FILE
           MOVE ZERO TO WS-PS-ERROR
           MOVE "WC-MEDFEE-SVR" TO WS-SERVER-NAME.

       2000-PROCESS-REQUESTS.
           ENTER TAL "SERVERCLASS_DIALOG_BEGIN_"
               USING WS-SERVER-NAME
               GIVING WS-PS-ERROR
           IF WS-PS-ERROR = ZERO
               EVALUATE WS-REQ-TYPE
                   WHEN 01
                       PERFORM 3000-PROFESSIONAL-FEE-LOOKUP
                   WHEN 02
                       PERFORM 3100-FACILITY-FEE-LOOKUP
                   WHEN 03
                       PERFORM 4000-DRG-HOSPITAL-LOOKUP
                   WHEN 04
                       PERFORM 5000-PHARMACY-NDC-LOOKUP
                   WHEN 05
                       PERFORM 6000-FEE-WITH-MODIFIERS
                   WHEN 06
                       PERFORM 7000-FEE-WITH-GAF
                   WHEN OTHER
                       MOVE 99 TO WS-RES-STATUS
               END-EVALUATE
               ENTER TAL "SERVERCLASS_DIALOG_END_"
           END-IF.

       3000-PROFESSIONAL-FEE-LOOKUP.
      *---------------------------------------------------------------*
      * Look up the maximum allowable professional fee for a CPT code *
      * in the specified state. Handles state-specific schedules,     *
      * Medicare-based calculations, and RBRVS-based fees.            *
      *---------------------------------------------------------------*
           MOVE WS-REQ-STATE TO FS-STATE-CODE
           MOVE WS-REQ-CPT-CODE TO FS-CPT-CODE
           MOVE WS-REQ-SERVICE-DATE TO FS-EFF-DATE
           READ FEE-SCHED-FILE
               INVALID KEY
                   PERFORM 3010-TRY-EARLIER-DATE
               NOT INVALID KEY
                   PERFORM 3020-CALC-PROFESSIONAL-FEE
           END-READ.

       3010-TRY-EARLIER-DATE.
      *    Exact date not found -- position to most recent entry
           MOVE WS-REQ-STATE TO FS-STATE-CODE
           MOVE WS-REQ-CPT-CODE TO FS-CPT-CODE
           MOVE 99999999 TO FS-EFF-DATE
           START FEE-SCHED-FILE
               KEY IS NOT GREATER THAN FS-KEY
               INVALID KEY
                   MOVE 01 TO WS-RES-STATUS
               NOT INVALID KEY
                   READ FEE-SCHED-FILE NEXT
                       AT END
                           MOVE 01 TO WS-RES-STATUS
                       NOT AT END
                           IF FS-STATE-CODE = WS-REQ-STATE
                           AND FS-CPT-CODE = WS-REQ-CPT-CODE
                               PERFORM 3020-CALC-PROFESSIONAL-FEE
                           ELSE
                               MOVE 01 TO WS-RES-STATUS
                           END-IF
                   END-READ
           END-START.

       3020-CALC-PROFESSIONAL-FEE.
           IF FS-STATUS-FLAG = "I" OR "D"
               MOVE 02 TO WS-RES-STATUS
           ELSE
               MOVE 00 TO WS-RES-STATUS
               EVALUATE FS-FEE-TYPE
                   WHEN "S"
      *                State-specific: use the stored fee directly
                       MOVE FS-PROFESSIONAL-FEE
                           TO WS-RES-MAX-ALLOWABLE
                   WHEN "M"
      *                Medicare-based: base rate * percentage
                       COMPUTE WS-RES-MAX-ALLOWABLE =
                           FS-MEDICARE-BASE-RATE *
                           (FS-MEDICARE-PCT / 100)
                   WHEN "R"
      *                RBRVS: (work RVU * GPCIw + practice RVU * GPCIpe
      *                + malpractice RVU * GPCImp) * conversion factor
                       PERFORM 3030-CALC-RBRVS-FEE
                   WHEN "U"
      *                Usual and customary: accept billed up to max
                       IF WS-REQ-BILLED-AMT <
                           FS-MAX-ALLOWABLE
                           MOVE WS-REQ-BILLED-AMT
                               TO WS-RES-MAX-ALLOWABLE
                       ELSE
                           MOVE FS-MAX-ALLOWABLE
                               TO WS-RES-MAX-ALLOWABLE
                       END-IF
               END-EVALUATE
      *        Multiply by units of service
               COMPUTE WS-RES-MAX-ALLOWABLE =
                   WS-RES-MAX-ALLOWABLE * WS-REQ-UNITS
      *        Determine recommended payment (lesser of billed/allowed)
               IF WS-REQ-BILLED-AMT <=
                   WS-RES-MAX-ALLOWABLE
                   MOVE WS-REQ-BILLED-AMT
                       TO WS-RES-RECOMMENDED-PAY
                   MOVE ZERO TO WS-RES-REDUCTION-AMT
               ELSE
                   MOVE WS-RES-MAX-ALLOWABLE
                       TO WS-RES-RECOMMENDED-PAY
                   COMPUTE WS-RES-REDUCTION-AMT =
                       WS-REQ-BILLED-AMT -
                       WS-RES-MAX-ALLOWABLE
                   MOVE "FEE SCHEDULE REDUCTION"
                       TO WS-RES-REDUCTION-REASON
               END-IF
               MOVE FS-FEE-TYPE TO WS-RES-FEE-TYPE
           END-IF.

       3030-CALC-RBRVS-FEE.
      *    Load geographic adjustment factors for provider location
           MOVE WS-REQ-STATE TO GAF-STATE-CODE
           MOVE WS-REQ-ZIP-CODE(1:3) TO GAF-ZIP-PREFIX
           READ GAF-TABLE-FILE
               INVALID KEY
      *            No GAF -- use factor of 1.0000
                   MOVE 1.0000 TO GAF-WORK-FACTOR
                   MOVE 1.0000 TO GAF-PRACTICE-FACTOR
                   MOVE 1.0000 TO GAF-MALPRACTICE-FACTOR
           END-READ
      *    RBRVS formula
           COMPUTE WS-RBRVS-FEE =
               ((FS-RVU-WORK * GAF-WORK-FACTOR)
               + (FS-RVU-PRACTICE * GAF-PRACTICE-FACTOR)
               + (FS-RVU-MALPRACTICE * GAF-MALPRACTICE-FACTOR))
               * FS-CONVERSION-FACTOR
           MOVE WS-RBRVS-FEE TO WS-RES-MAX-ALLOWABLE.

       3100-FACILITY-FEE-LOOKUP.
      *    Same as professional but uses facility fee column
           MOVE WS-REQ-STATE TO FS-STATE-CODE
           MOVE WS-REQ-CPT-CODE TO FS-CPT-CODE
           MOVE WS-REQ-SERVICE-DATE TO FS-EFF-DATE
           READ FEE-SCHED-FILE
               INVALID KEY
                   MOVE 01 TO WS-RES-STATUS
               NOT INVALID KEY
                   MOVE 00 TO WS-RES-STATUS
                   MOVE FS-FACILITY-FEE
                       TO WS-RES-MAX-ALLOWABLE
                   COMPUTE WS-RES-MAX-ALLOWABLE =
                       WS-RES-MAX-ALLOWABLE * WS-REQ-UNITS
                   IF WS-REQ-BILLED-AMT <=
                       WS-RES-MAX-ALLOWABLE
                       MOVE WS-REQ-BILLED-AMT
                           TO WS-RES-RECOMMENDED-PAY
                   ELSE
                       MOVE WS-RES-MAX-ALLOWABLE
                           TO WS-RES-RECOMMENDED-PAY
                       COMPUTE WS-RES-REDUCTION-AMT =
                           WS-REQ-BILLED-AMT -
                           WS-RES-MAX-ALLOWABLE
                       MOVE "FACILITY FEE SCHEDULE"
                           TO WS-RES-REDUCTION-REASON
                   END-IF
           END-READ.

       4000-DRG-HOSPITAL-LOOKUP.
      *---------------------------------------------------------------*
      * Hospital inpatient DRG-based reimbursement.                   *
      * Payment = Base Rate * DRG Weight + Outlier Adjustment         *
      * Outlier applies when charges exceed threshold.                *
      *---------------------------------------------------------------*
           MOVE WS-REQ-STATE TO DRG-STATE-CODE
           MOVE WS-REQ-DRG-CODE TO DRG-CODE
           READ DRG-TABLE-FILE
               INVALID KEY
                   MOVE 01 TO WS-RES-STATUS
               NOT INVALID KEY
                   MOVE 00 TO WS-RES-STATUS
      *            Standard DRG payment
                   COMPUTE WS-RES-MAX-ALLOWABLE =
                       DRG-BASE-RATE * DRG-WEIGHT
      *            Check for cost outlier
                   IF WS-REQ-BILLED-AMT >
                       DRG-OUTLIER-THRESHOLD
                       COMPUTE WS-OUTLIER-AMT =
                           (WS-REQ-BILLED-AMT -
                           DRG-OUTLIER-THRESHOLD) *
                           (DRG-OUTLIER-PCT / 100)
                       ADD WS-OUTLIER-AMT
                           TO WS-RES-MAX-ALLOWABLE
                   END-IF
      *            Recommended pay is always DRG amount (not billed)
                   MOVE WS-RES-MAX-ALLOWABLE
                       TO WS-RES-RECOMMENDED-PAY
                   IF WS-REQ-BILLED-AMT >
                       WS-RES-MAX-ALLOWABLE
                       COMPUTE WS-RES-REDUCTION-AMT =
                           WS-REQ-BILLED-AMT -
                           WS-RES-MAX-ALLOWABLE
                       MOVE "DRG-BASED REDUCTION"
                           TO WS-RES-REDUCTION-REASON
                   ELSE
                       MOVE ZERO TO WS-RES-REDUCTION-AMT
                   END-IF
           END-READ.

       5000-PHARMACY-NDC-LOOKUP.
      *---------------------------------------------------------------*
      * Pharmacy formulary and pricing check.                         *
      * Pricing: AWP - discount % + dispensing fee                    *
      * Formulary status determines if prior auth is needed.          *
      *---------------------------------------------------------------*
           MOVE WS-REQ-STATE TO NDC-STATE-CODE
           MOVE WS-REQ-NDC-CODE TO NDC-CODE
           READ PHARM-NDC-FILE
               INVALID KEY
                   MOVE 01 TO WS-RES-STATUS
               NOT INVALID KEY
                   EVALUATE NDC-FORMULARY-STATUS
                       WHEN "F"
                           MOVE 00 TO WS-RES-STATUS
                       WHEN "P"
                           MOVE 00 TO WS-RES-STATUS
                       WHEN "N"
                           MOVE 03 TO WS-RES-STATUS
                       WHEN "X"
                           MOVE 04 TO WS-RES-STATUS
                   END-EVALUATE
                   MOVE NDC-FORMULARY-STATUS
                       TO WS-RES-FORMULARY-STATUS
      *            Calculate allowed price: AWP - discount + disp fee
                   COMPUTE WS-NDC-ALLOWED-PRICE =
                       (NDC-AWP *
                       (1 - NDC-AWP-DISCOUNT-PCT / 100))
                       + NDC-DISPENSING-FEE
      *            Apply unit multiplier
                   COMPUTE WS-RES-MAX-ALLOWABLE =
                       WS-NDC-ALLOWED-PRICE * WS-REQ-UNITS
                   IF WS-REQ-BILLED-AMT <=
                       WS-RES-MAX-ALLOWABLE
                       MOVE WS-REQ-BILLED-AMT
                           TO WS-RES-RECOMMENDED-PAY
                   ELSE
                       MOVE WS-RES-MAX-ALLOWABLE
                           TO WS-RES-RECOMMENDED-PAY
                       COMPUTE WS-RES-REDUCTION-AMT =
                           WS-REQ-BILLED-AMT -
                           WS-RES-MAX-ALLOWABLE
                       MOVE "PHARMACY FEE REDUCTION"
                           TO WS-RES-REDUCTION-REASON
                   END-IF
           END-READ.

       6000-FEE-WITH-MODIFIERS.
      *---------------------------------------------------------------*
      * Apply procedure modifiers to the base fee.                    *
      * Common WC modifiers:                                          *
      *   -26  Professional component only                            *
      *   -TC  Technical component only                               *
      *   -80  Assistant surgeon (16% of primary surgeon fee)         *
      *   -50  Bilateral procedure (150% of unilateral fee)           *
      *   -51  Multiple procedures (50% of second+ procedures)        *
      *   -59  Distinct procedural service                            *
      *---------------------------------------------------------------*
           PERFORM 3000-PROFESSIONAL-FEE-LOOKUP
           IF WS-RES-STATUS = 00
               MOVE 100.00 TO WS-MODIFIER-ADJ-PCT
      *        Apply modifier 1
               IF WS-REQ-MODIFIER-1 NOT = SPACES
                   PERFORM 6100-APPLY-MODIFIER-1
               END-IF
      *        Apply modifier 2 (if stacking allowed)
               IF WS-REQ-MODIFIER-2 NOT = SPACES
                   PERFORM 6200-APPLY-MODIFIER-2
               END-IF
      *        Apply combined modifier adjustment
               COMPUTE WS-RES-MAX-ALLOWABLE =
                   WS-RES-MAX-ALLOWABLE *
                   (WS-MODIFIER-ADJ-PCT / 100)
      *        Recalculate recommended payment
               IF WS-REQ-BILLED-AMT <=
                   WS-RES-MAX-ALLOWABLE
                   MOVE WS-REQ-BILLED-AMT
                       TO WS-RES-RECOMMENDED-PAY
               ELSE
                   MOVE WS-RES-MAX-ALLOWABLE
                       TO WS-RES-RECOMMENDED-PAY
                   COMPUTE WS-RES-REDUCTION-AMT =
                       WS-REQ-BILLED-AMT -
                       WS-RES-MAX-ALLOWABLE
                   MOVE "MODIFIER ADJUSTED FEE"
                       TO WS-RES-REDUCTION-REASON
               END-IF
           END-IF.

       6100-APPLY-MODIFIER-1.
           MOVE WS-REQ-STATE TO MOD-STATE-CODE
           MOVE WS-REQ-MODIFIER-1 TO MOD-CODE
           READ MODIFIER-FILE
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   MULTIPLY MOD-PCT-ADJUSTMENT
                       BY WS-MODIFIER-ADJ-PCT
                   DIVIDE 100 INTO WS-MODIFIER-ADJ-PCT
           END-READ.

       6200-APPLY-MODIFIER-2.
           MOVE WS-REQ-STATE TO MOD-STATE-CODE
           MOVE WS-REQ-MODIFIER-2 TO MOD-CODE
           READ MODIFIER-FILE
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   IF MOD-STACKING-ALLOWED = "Y"
                       MULTIPLY MOD-PCT-ADJUSTMENT
                           BY WS-MODIFIER-ADJ-PCT
                       DIVIDE 100 INTO WS-MODIFIER-ADJ-PCT
                   END-IF
           END-READ.

       7000-FEE-WITH-GAF.
      *---------------------------------------------------------------*
      * Apply Geographic Area Factor adjustment to the fee.           *
      * GAF is based on provider ZIP code and adjusts for local       *
      * cost variations (labor, rent, malpractice insurance).         *
      *---------------------------------------------------------------*
           PERFORM 3000-PROFESSIONAL-FEE-LOOKUP
           IF WS-RES-STATUS = 00
               MOVE WS-REQ-STATE TO GAF-STATE-CODE
               MOVE WS-REQ-ZIP-CODE(1:3) TO GAF-ZIP-PREFIX
               READ GAF-TABLE-FILE
                   INVALID KEY
      *                No GAF entry -- use unadjusted fee
                       CONTINUE
                   NOT INVALID KEY
                       COMPUTE WS-RES-MAX-ALLOWABLE =
                           WS-RES-MAX-ALLOWABLE *
                           GAF-COMPOSITE-FACTOR
                       IF WS-REQ-BILLED-AMT <=
                           WS-RES-MAX-ALLOWABLE
                           MOVE WS-REQ-BILLED-AMT
                               TO WS-RES-RECOMMENDED-PAY
                       ELSE
                           MOVE WS-RES-MAX-ALLOWABLE
                               TO WS-RES-RECOMMENDED-PAY
                           COMPUTE WS-RES-REDUCTION-AMT =
                               WS-REQ-BILLED-AMT -
                               WS-RES-MAX-ALLOWABLE
                           MOVE "GAF ADJUSTED FEE SCHED"
                               TO WS-RES-REDUCTION-REASON
                       END-IF
               END-READ
           END-IF.

       9000-TERMINATE.
           CLOSE FEE-SCHED-FILE
                 DRG-TABLE-FILE
                 PHARM-NDC-FILE
                 GAF-TABLE-FILE
                 MODIFIER-FILE.
