       IDENTIFICATION DIVISION.
       PROGRAM-ID. WCUTIL.
      *================================================================*
      * WCUTIL - Workers' Compensation Utilization Review Program       *
      *                                                                 *
      * Manages the utilization review process for workers' comp        *
      * medical treatment. Handles pre-certification, concurrent        *
      * review, treatment authorization, medical necessity              *
      * determination, peer review routing, and nurse case              *
      * management referral triggers.                                   *
      *                                                                 *
      * PATHWAY: WC-UTIL-SVR                                           *
      * FILES:                                                          *
      *   $DATA1.WCFILES.URAUTH   - Authorization records              *
      *   $DATA1.WCFILES.URGUIDL  - Treatment duration guidelines      *
      *   $DATA1.WCFILES.URDIAG   - Diagnosis-based review rules       *
      *   $DATA1.WCFILES.URPEERQ  - Peer review queue                  *
      *   $DATA1.WCFILES.URIMESQ  - IME scheduling queue               *
      *   $DATA1.WCFILES.URNCMRF  - NCM referral triggers              *
      *                                                                 *
      * MODIFICATION LOG:                                               *
      * DATE       PROGRAMMER   DESCRIPTION                             *
      * ---------- ------------ --------------------------------------- *
      * 2024-05-15 L.MARTINEZ   INITIAL DEVELOPMENT                    *
      * 2024-08-20 L.MARTINEZ   PEER REVIEW ROUTING                    *
      * 2024-11-10 S.CHEN       NCM REFERRAL TRIGGERS                  *
      * 2025-02-01 L.MARTINEZ   IME SCHEDULING                         *
      * 2025-03-15 J.WILLIAMS   TREATMENT PLAN MONITORING              *
      *================================================================*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. TANDEM.
       OBJECT-COMPUTER. TANDEM.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AUTH-FILE
               ASSIGN TO "$DATA1.WCFILES.URAUTH"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AUTH-KEY
               FILE STATUS IS WS-AUTH-STATUS.

           SELECT GUIDELINE-FILE
               ASSIGN TO "$DATA1.WCFILES.URGUIDL"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS GL-KEY
               FILE STATUS IS WS-GL-STATUS.

           SELECT DIAG-RULE-FILE
               ASSIGN TO "$DATA1.WCFILES.URDIAG"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS DR-KEY
               FILE STATUS IS WS-DR-STATUS.

           SELECT PEER-REVIEW-FILE
               ASSIGN TO "$DATA1.WCFILES.URPEERQ"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PR-KEY
               FILE STATUS IS WS-PR-STATUS.

           SELECT IME-SCHED-FILE
               ASSIGN TO "$DATA1.WCFILES.URIMESQ"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS IME-KEY
               FILE STATUS IS WS-IME-STATUS.

           SELECT NCM-REFERRAL-FILE
               ASSIGN TO "$DATA1.WCFILES.URNCMRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS NCM-KEY
               FILE STATUS IS WS-NCM-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  AUTH-FILE.
       01  AUTH-RECORD.
           05  AUTH-KEY.
               10  AUTH-CLAIM-NUMBER      PIC 9(10).
               10  AUTH-SEQUENCE          PIC 9(04).
           05  AUTH-REQUEST-DATE          PIC 9(08).
           05  AUTH-DECISION-DATE         PIC 9(08).
           05  AUTH-TYPE                  PIC X(01).
      *        P = Pre-certification
      *        C = Concurrent review
      *        R = Retrospective review
      *        E = Extension request
           05  AUTH-STATUS                PIC X(01).
      *        A = Approved, D = Denied, M = Modified, P = Pended
      *        W = Withdrawn, E = Expired
           05  AUTH-ICD10-PRIMARY         PIC X(07).
           05  AUTH-ICD10-SECONDARY       PIC X(07).
           05  AUTH-CPT-REQUESTED         PIC X(05).
           05  AUTH-TREATMENT-TYPE        PIC X(02).
      *        SG = Surgery, PT = Physical therapy, MR = MRI/imaging
      *        RX = Prescription, IN = Injection, DC = Chiropractic
      *        PS = Pain management, OT = Occupational therapy
      *        SP = Specialist referral, HH = Home health
           05  AUTH-SESSIONS-REQUESTED    PIC 9(04).
           05  AUTH-SESSIONS-APPROVED     PIC 9(04).
           05  AUTH-DAYS-AUTHORIZED       PIC 9(04).
           05  AUTH-PROVIDER-NAME         PIC X(35).
           05  AUTH-PROVIDER-NPI          PIC X(10).
           05  AUTH-REVIEWER-ID           PIC X(08).
           05  AUTH-DENY-REASON-CODE      PIC X(04).
           05  AUTH-PEER-REVIEW-REQD      PIC X(01).
           05  AUTH-NOTES                 PIC X(200).

       FD  GUIDELINE-FILE.
       01  GUIDELINE-RECORD.
           05  GL-KEY.
               10  GL-ICD10-CODE         PIC X(07).
               10  GL-TREATMENT-TYPE     PIC X(02).
           05  GL-EXPECTED-DURATION-DAYS PIC 9(04).
           05  GL-MAX-SESSIONS           PIC 9(04).
           05  GL-MAX-EXTENSION-SESSIONS PIC 9(04).
           05  GL-REVIEW-INTERVAL-DAYS   PIC 9(03).
           05  GL-AUTO-APPROVE-SESSIONS  PIC 9(03).
           05  GL-REQUIRES-PREAUTH       PIC X(01).
           05  GL-NCM-TRIGGER-FLAG       PIC X(01).
           05  GL-DESCRIPTION            PIC X(60).

       FD  DIAG-RULE-FILE.
       01  DIAG-RULE-RECORD.
           05  DR-KEY.
               10  DR-ICD10-CODE         PIC X(07).
           05  DR-SEVERITY               PIC 9(01).
      *        1 = Minor, 2 = Moderate, 3 = Severe, 4 = Catastrophic
           05  DR-AUTO-NCM               PIC X(01).
           05  DR-MAX-TTD-DAYS           PIC 9(04).
           05  DR-EXPECTED-MMI-DAYS      PIC 9(04).
           05  DR-SURGERY-LIKELY         PIC X(01).
           05  DR-CHRONIC-RISK           PIC X(01).
           05  DR-OPIOID-RISK            PIC X(01).
           05  DR-RTW-PROB-PCT           PIC 9(03).
           05  DR-COMPLICATION-FLAGS     PIC X(04).

       FD  PEER-REVIEW-FILE.
       01  PEER-REVIEW-RECORD.
           05  PR-KEY.
               10  PR-CLAIM-NUMBER       PIC 9(10).
               10  PR-AUTH-SEQUENCE      PIC 9(04).
           05  PR-REFERRAL-DATE          PIC 9(08).
           05  PR-DUE-DATE               PIC 9(08).
           05  PR-REVIEWER-SPECIALTY     PIC X(20).
           05  PR-REVIEWER-NAME          PIC X(35).
           05  PR-STATUS                 PIC X(01).
      *        Q = Queued, A = Assigned, C = Complete, O = Overdue
           05  PR-DETERMINATION          PIC X(01).
      *        U = Upheld (deny stands), R = Reversed (approve)
      *        M = Modified
           05  PR-NOTES                  PIC X(200).

       FD  IME-SCHED-FILE.
       01  IME-SCHED-RECORD.
           05  IME-KEY.
               10  IME-CLAIM-NUMBER      PIC 9(10).
               10  IME-SEQUENCE          PIC 9(03).
           05  IME-REQUEST-DATE          PIC 9(08).
           05  IME-EXAM-DATE             PIC 9(08).
           05  IME-EXAMINER-NAME         PIC X(35).
           05  IME-EXAMINER-SPECIALTY    PIC X(20).
           05  IME-PURPOSE               PIC X(02).
      *        CM = Compensability/causation
      *        MM = Maximum medical improvement
      *        TD = Treatment dispute
      *        IM = Impairment rating
      *        RW = Return to work capability
           05  IME-STATUS                PIC X(01).
      *        S = Scheduled, C = Complete, X = Cancelled, N = No-show
           05  IME-REPORT-RECEIVED       PIC X(01).

       FD  NCM-REFERRAL-FILE.
       01  NCM-REFERRAL-RECORD.
           05  NCM-KEY.
               10  NCM-CLAIM-NUMBER      PIC 9(10).
               10  NCM-REFERRAL-SEQ      PIC 9(03).
           05  NCM-REFERRAL-DATE         PIC 9(08).
           05  NCM-TRIGGER-REASON        PIC X(02).
      *        DG = Diagnosis severity
      *        DU = Duration exceeded guidelines
      *        SG = Surgery scheduled
      *        OP = Opioid prescriptions
      *        NR = No RTW progress
      *        CS = Catastrophic injury
      *        MU = Multiple providers
           05  NCM-PRIORITY              PIC 9(01).
      *        1 = Urgent, 2 = High, 3 = Standard
           05  NCM-ASSIGNED-NURSE        PIC X(08).
           05  NCM-STATUS                PIC X(01).
      *        R = Referred, A = Accepted, C = Closed
           05  NCM-NOTES                 PIC X(100).

       WORKING-STORAGE SECTION.

       01  WS-FILE-STATUSES.
           05  WS-AUTH-STATUS            PIC X(02).
           05  WS-GL-STATUS              PIC X(02).
           05  WS-DR-STATUS              PIC X(02).
           05  WS-PR-STATUS              PIC X(02).
           05  WS-IME-STATUS             PIC X(02).
           05  WS-NCM-STATUS             PIC X(02).

       01  WS-REQUEST.
           05  WS-REQ-MSG-TYPE           PIC 9(02).
      *        01 = Pre-certification request
      *        02 = Concurrent review
      *        03 = Extension request
      *        04 = Query authorization
      *        05 = Route to peer review
      *        06 = Schedule IME
      *        07 = NCM referral check
           05  WS-REQ-CLAIM-NUM          PIC 9(10).
           05  WS-REQ-ICD10              PIC X(07).
           05  WS-REQ-TREATMENT-TYPE     PIC X(02).
           05  WS-REQ-CPT-CODE           PIC X(05).
           05  WS-REQ-SESSIONS-REQ       PIC 9(04).
           05  WS-REQ-PROVIDER-NPI       PIC X(10).
           05  WS-REQ-PROVIDER-NAME      PIC X(35).
           05  WS-REQ-DOI-DATE           PIC 9(08).
           05  WS-REQ-SERVICE-DATE       PIC 9(08).

       01  WS-WORK-FIELDS.
           05  WS-CURRENT-DATE           PIC 9(08).
           05  WS-DAYS-SINCE-DOI         PIC 9(05).
           05  WS-NEXT-AUTH-SEQ          PIC 9(04).
           05  WS-TOTAL-SESSIONS-USED    PIC 9(05).
           05  WS-TOTAL-SESSIONS-APRVD   PIC 9(05).
           05  WS-SESSIONS-REMAINING     PIC S9(05).
           05  WS-AUTH-DECISION          PIC X(01).
           05  WS-NCM-TRIGGER-FOUND      PIC X(01).
           05  WS-PS-ERROR               PIC S9(04) COMP.
           05  WS-SERVER-NAME            PIC X(24).

       01  WS-DATE-WORK.
           05  WS-DATE-1                 PIC 9(08).
           05  WS-DATE-2                 PIC 9(08).
           05  WS-DATE-DIFF              PIC S9(05) COMP.

       PROCEDURE DIVISION.

       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-REQUESTS
               UNTIL WS-PS-ERROR NOT = ZERO
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           OPEN I-O AUTH-FILE
                    PEER-REVIEW-FILE
                    IME-SCHED-FILE
                    NCM-REFERRAL-FILE
           OPEN INPUT GUIDELINE-FILE
                      DIAG-RULE-FILE
           MOVE ZERO TO WS-PS-ERROR
           MOVE "WC-UTIL-SVR" TO WS-SERVER-NAME.

       2000-PROCESS-REQUESTS.
           ENTER TAL "SERVERCLASS_DIALOG_BEGIN_"
               USING WS-SERVER-NAME
               GIVING WS-PS-ERROR
           IF WS-PS-ERROR = ZERO
               EVALUATE WS-REQ-MSG-TYPE
                   WHEN 01
                       PERFORM 3000-PRECERTIFICATION
                   WHEN 02
                       PERFORM 4000-CONCURRENT-REVIEW
                   WHEN 03
                       PERFORM 5000-EXTENSION-REQUEST
                   WHEN 04
                       PERFORM 6000-QUERY-AUTHORIZATION
                   WHEN 05
                       PERFORM 7000-ROUTE-PEER-REVIEW
                   WHEN 06
                       PERFORM 7500-SCHEDULE-IME
                   WHEN 07
                       PERFORM 8000-CHECK-NCM-TRIGGERS
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
               ENTER TAL "SERVERCLASS_DIALOG_END_"
           END-IF.

       3000-PRECERTIFICATION.
      *---------------------------------------------------------------*
      * Pre-certification: evaluate treatment request against          *
      * guidelines before treatment begins. Auto-approve if within     *
      * guideline parameters; route to clinical review if exceeds.     *
      *---------------------------------------------------------------*
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD

      *    Look up treatment guidelines for this diagnosis + type
           MOVE WS-REQ-ICD10 TO GL-ICD10-CODE
           MOVE WS-REQ-TREATMENT-TYPE TO GL-TREATMENT-TYPE
           READ GUIDELINE-FILE
               INVALID KEY
      *            No guideline -- auto-approve initial sessions
                   MOVE "A" TO WS-AUTH-DECISION
                   MOVE WS-REQ-SESSIONS-REQ
                       TO WS-TOTAL-SESSIONS-APRVD
                   PERFORM 3500-WRITE-AUTH-RECORD
                   PERFORM 8000-CHECK-NCM-TRIGGERS
                   EXIT PARAGRAPH
           END-READ

      *    Evaluate against guidelines
           EVALUATE TRUE
               WHEN GL-REQUIRES-PREAUTH = "N"
      *            Pre-auth not required -- auto-approve
                   MOVE "A" TO WS-AUTH-DECISION
                   MOVE WS-REQ-SESSIONS-REQ
                       TO WS-TOTAL-SESSIONS-APRVD

               WHEN WS-REQ-SESSIONS-REQ <=
                    GL-AUTO-APPROVE-SESSIONS
      *            Within auto-approve threshold
                   MOVE "A" TO WS-AUTH-DECISION
                   MOVE WS-REQ-SESSIONS-REQ
                       TO WS-TOTAL-SESSIONS-APRVD

               WHEN WS-REQ-SESSIONS-REQ <= GL-MAX-SESSIONS
      *            Within max but exceeds auto-approve -- clinical review
                   PERFORM 3200-CLINICAL-REVIEW

               WHEN WS-REQ-SESSIONS-REQ > GL-MAX-SESSIONS
      *            Exceeds guideline maximum -- pend or deny
                   EVALUATE WS-REQ-TREATMENT-TYPE
                       WHEN "SG"
      *                    Surgery requests always get clinical review
                           PERFORM 3200-CLINICAL-REVIEW
                       WHEN "PS"
      *                    Pain management exceeding guidelines -- pend
                           MOVE "P" TO WS-AUTH-DECISION
                           MOVE "MN01"
                               TO AUTH-DENY-REASON-CODE
                       WHEN OTHER
      *                    Modify to guideline maximum
                           MOVE "M" TO WS-AUTH-DECISION
                           MOVE GL-MAX-SESSIONS
                               TO WS-TOTAL-SESSIONS-APRVD
                   END-EVALUATE
           END-EVALUATE

           PERFORM 3500-WRITE-AUTH-RECORD

      *    Check if this diagnosis triggers NCM referral
           PERFORM 8000-CHECK-NCM-TRIGGERS.

       3200-CLINICAL-REVIEW.
      *    Determine based on diagnosis severity and treatment type
           MOVE WS-REQ-ICD10 TO DR-ICD10-CODE
           READ DIAG-RULE-FILE
               INVALID KEY
      *            Unknown diagnosis -- pend for review
                   MOVE "P" TO WS-AUTH-DECISION
               NOT INVALID KEY
                   EVALUATE DR-SEVERITY
                       WHEN 1
      *                    Minor -- approve up to guideline max
                           MOVE "A" TO WS-AUTH-DECISION
                           IF WS-REQ-SESSIONS-REQ >
                               GL-MAX-SESSIONS
                               MOVE GL-MAX-SESSIONS
                                   TO WS-TOTAL-SESSIONS-APRVD
                               MOVE "M" TO WS-AUTH-DECISION
                           ELSE
                               MOVE WS-REQ-SESSIONS-REQ
                                   TO WS-TOTAL-SESSIONS-APRVD
                           END-IF
                       WHEN 2
      *                    Moderate -- approve but schedule review
                           MOVE "A" TO WS-AUTH-DECISION
                           MOVE WS-REQ-SESSIONS-REQ
                               TO WS-TOTAL-SESSIONS-APRVD
                       WHEN 3
      *                    Severe -- approve and trigger NCM
                           MOVE "A" TO WS-AUTH-DECISION
                           MOVE WS-REQ-SESSIONS-REQ
                               TO WS-TOTAL-SESSIONS-APRVD
                       WHEN 4
      *                    Catastrophic -- approve all, NCM mandatory
                           MOVE "A" TO WS-AUTH-DECISION
                           MOVE WS-REQ-SESSIONS-REQ
                               TO WS-TOTAL-SESSIONS-APRVD
                   END-EVALUATE
           END-READ.

       3500-WRITE-AUTH-RECORD.
           PERFORM 3510-GET-NEXT-AUTH-SEQ
           MOVE WS-REQ-CLAIM-NUM TO AUTH-CLAIM-NUMBER
           MOVE WS-NEXT-AUTH-SEQ TO AUTH-SEQUENCE
           MOVE WS-CURRENT-DATE TO AUTH-REQUEST-DATE
           IF WS-AUTH-DECISION = "A" OR "M"
               MOVE WS-CURRENT-DATE TO AUTH-DECISION-DATE
           ELSE
               MOVE ZEROES TO AUTH-DECISION-DATE
           END-IF
           MOVE "P" TO AUTH-TYPE
           MOVE WS-AUTH-DECISION TO AUTH-STATUS
           MOVE WS-REQ-ICD10 TO AUTH-ICD10-PRIMARY
           MOVE SPACES TO AUTH-ICD10-SECONDARY
           MOVE WS-REQ-CPT-CODE TO AUTH-CPT-REQUESTED
           MOVE WS-REQ-TREATMENT-TYPE TO AUTH-TREATMENT-TYPE
           MOVE WS-REQ-SESSIONS-REQ TO AUTH-SESSIONS-REQUESTED
           MOVE WS-TOTAL-SESSIONS-APRVD TO AUTH-SESSIONS-APPROVED
           MOVE WS-REQ-PROVIDER-NAME TO AUTH-PROVIDER-NAME
           MOVE WS-REQ-PROVIDER-NPI TO AUTH-PROVIDER-NPI
           WRITE AUTH-RECORD
               INVALID KEY
                   CONTINUE
           END-WRITE.

       3510-GET-NEXT-AUTH-SEQ.
           MOVE WS-REQ-CLAIM-NUM TO AUTH-CLAIM-NUMBER
           MOVE 9999 TO AUTH-SEQUENCE
           START AUTH-FILE
               KEY IS NOT GREATER THAN AUTH-KEY
               INVALID KEY
                   MOVE 1 TO WS-NEXT-AUTH-SEQ
               NOT INVALID KEY
                   READ AUTH-FILE PREVIOUS
                       AT END
                           MOVE 1 TO WS-NEXT-AUTH-SEQ
                       NOT AT END
                           IF AUTH-CLAIM-NUMBER =
                               WS-REQ-CLAIM-NUM
                               ADD 1 TO AUTH-SEQUENCE
                                   GIVING WS-NEXT-AUTH-SEQ
                           ELSE
                               MOVE 1 TO WS-NEXT-AUTH-SEQ
                           END-IF
                   END-READ
           END-START.

       4000-CONCURRENT-REVIEW.
      *---------------------------------------------------------------*
      * Concurrent review: evaluate ongoing treatment against          *
      * duration guidelines. Triggered at review intervals.            *
      *---------------------------------------------------------------*
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD

      *    Calculate days since DOI
           MOVE WS-REQ-DOI-DATE TO WS-DATE-1
           MOVE WS-CURRENT-DATE TO WS-DATE-2
           PERFORM 8500-CALC-DATE-DIFF
           MOVE WS-DATE-DIFF TO WS-DAYS-SINCE-DOI

      *    Check duration guidelines
           MOVE WS-REQ-ICD10 TO GL-ICD10-CODE
           MOVE WS-REQ-TREATMENT-TYPE TO GL-TREATMENT-TYPE
           READ GUIDELINE-FILE
               INVALID KEY
                   EXIT PARAGRAPH
           END-READ

      *    If duration exceeded guidelines, flag for review
           IF WS-DAYS-SINCE-DOI > GL-EXPECTED-DURATION-DAYS
               MOVE "DU" TO NCM-TRIGGER-REASON
               PERFORM 8100-WRITE-NCM-REFERRAL
           END-IF.

       5000-EXTENSION-REQUEST.
      *---------------------------------------------------------------*
      * Extension: additional sessions beyond initial authorization.   *
      * Check cumulative sessions against extended maximums.           *
      *---------------------------------------------------------------*
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD

           MOVE WS-REQ-ICD10 TO GL-ICD10-CODE
           MOVE WS-REQ-TREATMENT-TYPE TO GL-TREATMENT-TYPE
           READ GUIDELINE-FILE
               INVALID KEY
                   MOVE "P" TO WS-AUTH-DECISION
                   PERFORM 3500-WRITE-AUTH-RECORD
                   EXIT PARAGRAPH
           END-READ

      *    Count total sessions already approved
           PERFORM 5100-COUNT-APPROVED-SESSIONS

      *    Check if extension is within guideline limits
           COMPUTE WS-SESSIONS-REMAINING =
               GL-MAX-SESSIONS + GL-MAX-EXTENSION-SESSIONS
               - WS-TOTAL-SESSIONS-USED

           IF WS-SESSIONS-REMAINING <= ZERO
      *        All sessions exhausted -- route to peer review
               MOVE "D" TO WS-AUTH-DECISION
               MOVE "MN02" TO AUTH-DENY-REASON-CODE
               MOVE "Y" TO AUTH-PEER-REVIEW-REQD
               PERFORM 3500-WRITE-AUTH-RECORD
               PERFORM 7000-ROUTE-PEER-REVIEW
           ELSE
               IF WS-REQ-SESSIONS-REQ <=
                   WS-SESSIONS-REMAINING
                   MOVE "A" TO WS-AUTH-DECISION
                   MOVE WS-REQ-SESSIONS-REQ
                       TO WS-TOTAL-SESSIONS-APRVD
               ELSE
                   MOVE "M" TO WS-AUTH-DECISION
                   MOVE WS-SESSIONS-REMAINING
                       TO WS-TOTAL-SESSIONS-APRVD
               END-IF
               PERFORM 3500-WRITE-AUTH-RECORD
           END-IF.

       5100-COUNT-APPROVED-SESSIONS.
           MOVE ZERO TO WS-TOTAL-SESSIONS-USED
           MOVE WS-REQ-CLAIM-NUM TO AUTH-CLAIM-NUMBER
           MOVE ZEROES TO AUTH-SEQUENCE
           START AUTH-FILE
               KEY IS NOT LESS THAN AUTH-KEY
               INVALID KEY
                   EXIT PARAGRAPH
           END-START
           PERFORM UNTIL WS-AUTH-STATUS NOT = "00"
               READ AUTH-FILE NEXT
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF AUTH-CLAIM-NUMBER = WS-REQ-CLAIM-NUM
                       AND AUTH-TREATMENT-TYPE =
                           WS-REQ-TREATMENT-TYPE
                       AND (AUTH-STATUS = "A" OR "M")
                           ADD AUTH-SESSIONS-APPROVED
                               TO WS-TOTAL-SESSIONS-USED
                       ELSE
                           IF AUTH-CLAIM-NUMBER NOT =
                               WS-REQ-CLAIM-NUM
                               EXIT PERFORM
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.

       6000-QUERY-AUTHORIZATION.
           MOVE WS-REQ-CLAIM-NUM TO AUTH-CLAIM-NUMBER
           MOVE ZEROES TO AUTH-SEQUENCE
           START AUTH-FILE
               KEY IS NOT LESS THAN AUTH-KEY
               INVALID KEY
                   CONTINUE
           END-START.

       7000-ROUTE-PEER-REVIEW.
      *---------------------------------------------------------------*
      * Route denied or pended authorization to physician peer review. *
      *---------------------------------------------------------------*
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE WS-REQ-CLAIM-NUM TO PR-CLAIM-NUMBER
           MOVE WS-NEXT-AUTH-SEQ TO PR-AUTH-SEQUENCE
           MOVE WS-CURRENT-DATE TO PR-REFERRAL-DATE
      *    Due in 5 business days (7 calendar for simplicity)
           PERFORM 8600-ADD-DAYS-TO-DATE
           MOVE "Q" TO PR-STATUS
           MOVE SPACES TO PR-DETERMINATION
      *    Determine reviewer specialty from treatment type
           EVALUATE WS-REQ-TREATMENT-TYPE
               WHEN "SG"
                   MOVE "ORTHOPEDIC SURGERY" TO PR-REVIEWER-SPECIALTY
               WHEN "PT" THRU "OT"
                   MOVE "PHYSICAL MEDICINE" TO PR-REVIEWER-SPECIALTY
               WHEN "PS"
                   MOVE "PAIN MANAGEMENT" TO PR-REVIEWER-SPECIALTY
               WHEN "DC"
                   MOVE "CHIROPRACTIC" TO PR-REVIEWER-SPECIALTY
               WHEN OTHER
                   MOVE "GENERAL MEDICINE" TO PR-REVIEWER-SPECIALTY
           END-EVALUATE
           WRITE PEER-REVIEW-RECORD
               INVALID KEY
                   CONTINUE
           END-WRITE.

       7500-SCHEDULE-IME.
      *---------------------------------------------------------------*
      * Schedule Independent Medical Examination                       *
      *---------------------------------------------------------------*
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE WS-REQ-CLAIM-NUM TO IME-CLAIM-NUMBER
           MOVE 1 TO IME-SEQUENCE
           MOVE WS-CURRENT-DATE TO IME-REQUEST-DATE
           MOVE ZEROES TO IME-EXAM-DATE
           MOVE "S" TO IME-STATUS
           MOVE "N" TO IME-REPORT-RECEIVED
           WRITE IME-SCHED-RECORD
               INVALID KEY
                   CONTINUE
           END-WRITE.

       8000-CHECK-NCM-TRIGGERS.
      *---------------------------------------------------------------*
      * Evaluate whether this claim should be referred to a Nurse      *
      * Case Manager. Triggers include:                                *
      *   - Catastrophic injury diagnosis                              *
      *   - Surgery scheduled                                         *
      *   - Treatment duration exceeding guidelines                   *
      *   - Opioid prescription risk                                  *
      *   - No return-to-work progress after threshold days            *
      *---------------------------------------------------------------*
           MOVE "N" TO WS-NCM-TRIGGER-FOUND

           MOVE WS-REQ-ICD10 TO DR-ICD10-CODE
           READ DIAG-RULE-FILE
               INVALID KEY
                   EXIT PARAGRAPH
           END-READ

      *    Catastrophic injury -- immediate NCM
           IF DR-SEVERITY = 4
               MOVE "Y" TO WS-NCM-TRIGGER-FOUND
               MOVE "CS" TO NCM-TRIGGER-REASON
               MOVE 1 TO NCM-PRIORITY
               PERFORM 8100-WRITE-NCM-REFERRAL
               EXIT PARAGRAPH
           END-IF

      *    Surgery likely -- proactive NCM
           IF DR-SURGERY-LIKELY = "Y"
           AND WS-REQ-TREATMENT-TYPE = "SG"
               MOVE "Y" TO WS-NCM-TRIGGER-FOUND
               MOVE "SG" TO NCM-TRIGGER-REASON
               MOVE 2 TO NCM-PRIORITY
               PERFORM 8100-WRITE-NCM-REFERRAL
           END-IF

      *    Opioid risk
           IF DR-OPIOID-RISK = "Y"
               MOVE "Y" TO WS-NCM-TRIGGER-FOUND
               MOVE "OP" TO NCM-TRIGGER-REASON
               MOVE 2 TO NCM-PRIORITY
               PERFORM 8100-WRITE-NCM-REFERRAL
           END-IF

      *    Guideline-triggered NCM
           MOVE WS-REQ-ICD10 TO GL-ICD10-CODE
           MOVE WS-REQ-TREATMENT-TYPE TO GL-TREATMENT-TYPE
           READ GUIDELINE-FILE
               INVALID KEY
                   EXIT PARAGRAPH
           END-READ
           IF GL-NCM-TRIGGER-FLAG = "Y"
               MOVE "Y" TO WS-NCM-TRIGGER-FOUND
               MOVE "DG" TO NCM-TRIGGER-REASON
               MOVE 3 TO NCM-PRIORITY
               PERFORM 8100-WRITE-NCM-REFERRAL
           END-IF.

       8100-WRITE-NCM-REFERRAL.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE WS-REQ-CLAIM-NUM TO NCM-CLAIM-NUMBER
           MOVE 1 TO NCM-REFERRAL-SEQ
           MOVE WS-CURRENT-DATE TO NCM-REFERRAL-DATE
           MOVE "R" TO NCM-STATUS
           MOVE SPACES TO NCM-ASSIGNED-NURSE
           WRITE NCM-REFERRAL-RECORD
               INVALID KEY
                   CONTINUE
           END-WRITE.

       8500-CALC-DATE-DIFF.
      *    Calculate difference in days between WS-DATE-1 and WS-DATE-2
      *    Uses PATHSEND to date utility server
           ENTER TAL "PATHSEND_LINK_" USING
               "$DATA1.WCFILES.DATESRV"
               WS-DATE-1
               WS-DATE-2
               WS-DATE-DIFF.

       8600-ADD-DAYS-TO-DATE.
      *    Add 7 days to WS-CURRENT-DATE for due date
           ENTER TAL "PATHSEND_LINK_" USING
               "$DATA1.WCFILES.DATESRV"
               WS-CURRENT-DATE
               7
               PR-DUE-DATE.

       9000-TERMINATE.
           CLOSE AUTH-FILE
                 GUIDELINE-FILE
                 DIAG-RULE-FILE
                 PEER-REVIEW-FILE
                 IME-SCHED-FILE
                 NCM-REFERRAL-FILE.
