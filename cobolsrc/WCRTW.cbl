       IDENTIFICATION DIVISION.
       PROGRAM-ID. WCRTW.
      *================================================================*
      * WCRTW - Return to Work Tracking Program                        *
      *                                                                 *
      * Manages the return-to-work lifecycle for injured workers:       *
      * status tracking, modified duty matching, functional capacity    *
      * evaluations, vocational rehabilitation referrals, wage loss     *
      * during modified duty, and milestone reporting.                  *
      *                                                                 *
      * PATHWAY: WC-RTW-SVR                                            *
      * FILES:                                                          *
      *   $DATA1.WCFILES.RTWSTAT  - RTW status records                 *
      *   $DATA1.WCFILES.RTWJOBS  - Modified duty job inventory        *
      *   $DATA1.WCFILES.RTWFCE   - FCE tracking records               *
      *   $DATA1.WCFILES.RTWVR    - Vocational rehab referrals         *
      *   $DATA1.WCFILES.RTWMILE  - Milestone tracking                 *
      *                                                                 *
      * PATHSEND DEPENDENCIES:                                          *
      *   WC-BEN-CALC  - Benefits calculation for wage loss             *
      *   WC-JURIS-SVR - Jurisdiction rules for VR triggers             *
      *                                                                 *
      * MODIFICATION LOG:                                               *
      * DATE       PROGRAMMER   DESCRIPTION                             *
      * ---------- ------------ --------------------------------------- *
      * 2024-06-01 T.NGUYEN     INITIAL DEVELOPMENT                    *
      * 2024-09-15 T.NGUYEN     MODIFIED DUTY MATCHING                 *
      * 2024-12-10 L.MARTINEZ   VR REFERRAL TRIGGERS                   *
      * 2025-02-15 T.NGUYEN     WAGE LOSS CALCULATIONS                 *
      * 2025-03-20 K.PATEL      MILESTONE REPORTING                    *
      *================================================================*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. TANDEM.
       OBJECT-COMPUTER. TANDEM.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RTW-STATUS-FILE
               ASSIGN TO "$DATA1.WCFILES.RTWSTAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RS-KEY
               FILE STATUS IS WS-RS-STATUS.

           SELECT RTW-JOBS-FILE
               ASSIGN TO "$DATA1.WCFILES.RTWJOBS"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RJ-KEY
               FILE STATUS IS WS-RJ-STATUS.

           SELECT RTW-FCE-FILE
               ASSIGN TO "$DATA1.WCFILES.RTWFCE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FC-KEY
               FILE STATUS IS WS-FC-STATUS.

           SELECT RTW-VR-FILE
               ASSIGN TO "$DATA1.WCFILES.RTWVR"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS VR-KEY
               FILE STATUS IS WS-VR-STATUS.

           SELECT RTW-MILESTONE-FILE
               ASSIGN TO "$DATA1.WCFILES.RTWMILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RM-KEY
               FILE STATUS IS WS-RM-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  RTW-STATUS-FILE.
       01  RTW-STATUS-RECORD.
           05  RS-KEY.
               10  RS-CLAIM-NUMBER       PIC 9(10).
               10  RS-STATUS-DATE        PIC 9(08).
           05  RS-STATUS-CODE            PIC X(02).
      *        FD = Full duty (claim closable)
      *        MD = Modified duty (restricted work)
      *        TD = Transitional duty (temporary accommodations)
      *        NW = No work (total disability)
      *        LD = Light duty program
      *        VR = Vocational rehabilitation
      *        SR = Selective return (partial duties)
      *        TM = Terminated/separated during disability
           05  RS-RESTRICTIONS.
               10  RS-LIFT-LIMIT-LBS     PIC 9(03).
               10  RS-STAND-HOURS        PIC 9(01)V9.
               10  RS-SIT-HOURS          PIC 9(01)V9.
               10  RS-WALK-HOURS         PIC 9(01)V9.
               10  RS-NO-REPETITIVE-MOTION PIC X(01).
               10  RS-NO-OVERHEAD-REACH  PIC X(01).
               10  RS-NO-BENDING         PIC X(01).
               10  RS-NO-CLIMBING        PIC X(01).
               10  RS-NO-DRIVING         PIC X(01).
               10  RS-HOURS-PER-DAY      PIC 9(01)V9.
               10  RS-DAYS-PER-WEEK      PIC 9(01).
           05  RS-TREATING-PHYSICIAN     PIC X(35).
           05  RS-PHYSICIAN-NPI          PIC X(10).
           05  RS-NEXT-REVIEW-DATE       PIC 9(08).
           05  RS-EMPLOYER-ACCOM-AVAIL   PIC X(01).
      *        Y = Yes, N = No, P = Pending evaluation
           05  RS-MODIFIED-JOB-ID        PIC X(10).
           05  RS-MODIFIED-HOURLY-WAGE   PIC S9(05)V99 COMP-3.
           05  RS-PRE-INJURY-HOURLY-WAGE PIC S9(05)V99 COMP-3.
           05  RS-NOTES                  PIC X(100).

       FD  RTW-JOBS-FILE.
       01  RTW-JOBS-RECORD.
           05  RJ-KEY.
               10  RJ-EMPLOYER-ID        PIC X(10).
               10  RJ-JOB-ID             PIC X(10).
           05  RJ-JOB-TITLE              PIC X(30).
           05  RJ-DEPARTMENT             PIC X(20).
           05  RJ-PHYSICAL-DEMANDS.
               10  RJ-LIFT-MAX-LBS       PIC 9(03).
               10  RJ-STAND-REQUIRED     PIC 9(01)V9.
               10  RJ-SIT-REQUIRED       PIC 9(01)V9.
               10  RJ-WALK-REQUIRED      PIC 9(01)V9.
               10  RJ-REPETITIVE-MOTION  PIC X(01).
               10  RJ-OVERHEAD-REACH     PIC X(01).
               10  RJ-BENDING-REQUIRED   PIC X(01).
               10  RJ-CLIMBING-REQUIRED  PIC X(01).
               10  RJ-DRIVING-REQUIRED   PIC X(01).
           05  RJ-HOURS-PER-DAY          PIC 9(01)V9.
           05  RJ-DAYS-PER-WEEK          PIC 9(01).
           05  RJ-HOURLY-WAGE            PIC S9(05)V99 COMP-3.
           05  RJ-AVAILABLE              PIC X(01).
           05  RJ-DURATION-WEEKS         PIC 9(03).

       FD  RTW-FCE-FILE.
       01  RTW-FCE-RECORD.
           05  FC-KEY.
               10  FC-CLAIM-NUMBER       PIC 9(10).
               10  FC-EVAL-DATE          PIC 9(08).
           05  FC-EVALUATOR-NAME         PIC X(35).
           05  FC-FACILITY               PIC X(30).
           05  FC-TYPE                   PIC X(01).
      *        B = Baseline, P = Progress, D = Discharge, I = IME
           05  FC-RESULTS.
               10  FC-LIFT-FLOOR-LBS     PIC 9(03).
               10  FC-LIFT-WAIST-LBS     PIC 9(03).
               10  FC-LIFT-OVERHEAD-LBS  PIC 9(03).
               10  FC-CARRY-LBS          PIC 9(03).
               10  FC-PUSH-PULL-LBS      PIC 9(03).
               10  FC-STAND-TOLERANCE    PIC 9(01)V9.
               10  FC-SIT-TOLERANCE      PIC 9(01)V9.
               10  FC-WALK-TOLERANCE     PIC 9(01)V9.
               10  FC-GRIP-RIGHT-LBS     PIC 9(03).
               10  FC-GRIP-LEFT-LBS      PIC 9(03).
           05  FC-CONSISTENCY-RATING     PIC X(01).
      *        G = Good (reliable), F = Fair, P = Poor (unreliable)
           05  FC-WORK-LEVEL             PIC X(01).
      *        S = Sedentary, L = Light, M = Medium, H = Heavy, V = Very Heavy
           05  FC-MMI-OPINION            PIC X(01).
      *        Y = At MMI, N = Not at MMI, U = Unknown
           05  FC-REPORT-RECEIVED        PIC X(01).

       FD  RTW-VR-FILE.
       01  RTW-VR-RECORD.
           05  VR-KEY.
               10  VR-CLAIM-NUMBER       PIC 9(10).
               10  VR-REFERRAL-SEQ       PIC 9(03).
           05  VR-REFERRAL-DATE          PIC 9(08).
           05  VR-TRIGGER-REASON         PIC X(02).
      *        JE = Job eliminated during disability
      *        TM = Employee terminated
      *        NM = No modified duty available
      *        MD = Max duration of modified duty reached
      *        FC = FCE shows can't return to prior job
      *        PR = Physician recommends retraining
           05  VR-COUNSELOR-NAME         PIC X(35).
           05  VR-STATUS                 PIC X(01).
      *        R = Referred, A = Accepted, P = Plan developed
      *        T = Training, J = Job placement, C = Closed-successful
      *        X = Closed-unsuccessful
           05  VR-PLAN-TYPE              PIC X(02).
      *        OJ = On-the-job training
      *        SR = Short-term retraining
      *        LR = Long-term retraining (degree program)
      *        DJ = Direct job placement
      *        SE = Self-employment
      *        LM = Labor market survey only
           05  VR-ESTIMATED-COST         PIC S9(07)V99 COMP-3.
           05  VR-START-DATE             PIC 9(08).
           05  VR-TARGET-END-DATE        PIC 9(08).
           05  VR-ACTUAL-END-DATE        PIC 9(08).

       FD  RTW-MILESTONE-FILE.
       01  RTW-MILESTONE-RECORD.
           05  RM-KEY.
               10  RM-CLAIM-NUMBER       PIC 9(10).
               10  RM-MILESTONE-DATE     PIC 9(08).
               10  RM-MILESTONE-CODE     PIC X(04).
           05  RM-DESCRIPTION            PIC X(50).
           05  RM-PRIOR-STATUS           PIC X(02).
           05  RM-NEW-STATUS             PIC X(02).
           05  RM-ADJUSTER-ID            PIC X(08).
           05  RM-SYSTEM-GENERATED       PIC X(01).

       WORKING-STORAGE SECTION.
      *
      *    Cross-program copybook references
           COPY WCCLMCPY
           COPY WCEMPCPY

       01  WS-FILE-STATUSES.
           05  WS-RS-STATUS              PIC X(02).
           05  WS-RJ-STATUS              PIC X(02).
           05  WS-FC-STATUS              PIC X(02).
           05  WS-VR-STATUS              PIC X(02).
           05  WS-RM-STATUS              PIC X(02).

       01  WS-REQUEST.
           05  WS-REQ-MSG-TYPE           PIC 9(02).
      *        01 = Update RTW status
      *        02 = Match modified duty job
      *        03 = Record FCE results
      *        04 = Check VR triggers
      *        05 = Calculate modified duty wage loss
      *        06 = Record milestone
      *        07 = Query RTW history
      *        08 = Generate RTW report
           05  WS-REQ-CLAIM-NUM          PIC 9(10).
           05  WS-REQ-EMPLOYER-ID        PIC X(10).
           05  WS-REQ-STATUS-CODE        PIC X(02).
           05  WS-REQ-EFFECTIVE-DATE     PIC 9(08).
           05  WS-REQ-PRE-INJ-WAGE      PIC S9(05)V99 COMP-3.
           05  WS-REQ-JURISDICTION       PIC X(02).

       01  WS-WORK-FIELDS.
           05  WS-CURRENT-DATE           PIC 9(08).
           05  WS-MATCH-FOUND            PIC X(01).
           05  WS-BEST-MATCH-JOB-ID      PIC X(10).
           05  WS-BEST-MATCH-SCORE       PIC 9(03).
           05  WS-CURRENT-SCORE          PIC 9(03).
           05  WS-WAGE-LOSS-WEEKLY       PIC S9(07)V99 COMP-3.
           05  WS-MODIFIED-WEEKLY        PIC S9(07)V99 COMP-3.
           05  WS-PRE-INJ-WEEKLY         PIC S9(07)V99 COMP-3.
           05  WS-VR-TRIGGER-FOUND       PIC X(01).
           05  WS-DAYS-ON-MODIFIED       PIC 9(05).
           05  WS-PS-ERROR               PIC S9(04) COMP.
           05  WS-SERVER-NAME            PIC X(24).

       01  WS-CURRENT-RESTRICTIONS.
           05  WS-CR-LIFT-LIMIT          PIC 9(03).
           05  WS-CR-STAND-HRS           PIC 9(01)V9.
           05  WS-CR-SIT-HRS             PIC 9(01)V9.
           05  WS-CR-NO-REPMOTION        PIC X(01).
           05  WS-CR-NO-OVERHEAD         PIC X(01).
           05  WS-CR-NO-BENDING          PIC X(01).
           05  WS-CR-NO-CLIMBING         PIC X(01).
           05  WS-CR-NO-DRIVING          PIC X(01).
           05  WS-CR-HOURS-DAY           PIC 9(01)V9.
           05  WS-CR-DAYS-WEEK           PIC 9(01).

       01  WS-PATHSEND-REQUEST.
           05  WS-PS-SERVER              PIC X(24).
           05  WS-PS-MSG-CODE            PIC 9(02).
           05  WS-PS-CLAIM-NUM           PIC 9(10).
           05  WS-PS-JURISDICTION        PIC X(02).
           05  WS-PS-AWW                 PIC S9(07)V99 COMP-3.
           05  WS-PS-POST-INJ-EARN      PIC S9(07)V99 COMP-3.
           05  WS-PS-RESPONSE           PIC X(200).

       PROCEDURE DIVISION.

       0000-MAIN-PROCESS.
      *
      *    Inter-program communication calls
           CALL "WCBENCALC"
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-REQUESTS
               UNTIL WS-PS-ERROR NOT = ZERO
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           OPEN I-O RTW-STATUS-FILE
                    RTW-JOBS-FILE
                    RTW-FCE-FILE
                    RTW-VR-FILE
                    RTW-MILESTONE-FILE
           MOVE ZERO TO WS-PS-ERROR
           MOVE "WC-RTW-SVR" TO WS-SERVER-NAME.

       2000-PROCESS-REQUESTS.
           ENTER TAL "SERVERCLASS_DIALOG_BEGIN_"
               USING WS-SERVER-NAME
               GIVING WS-PS-ERROR
           IF WS-PS-ERROR = ZERO
               EVALUATE WS-REQ-MSG-TYPE
                   WHEN 01
                       PERFORM 3000-UPDATE-RTW-STATUS
                   WHEN 02
                       PERFORM 4000-MATCH-MODIFIED-DUTY
                   WHEN 03
                       PERFORM 5000-RECORD-FCE
                   WHEN 04
                       PERFORM 6000-CHECK-VR-TRIGGERS
                   WHEN 05
                       PERFORM 7000-CALC-MODIFIED-DUTY-WAGE-LOSS
                   WHEN 06
                       PERFORM 7500-RECORD-MILESTONE
                   WHEN 07
                       PERFORM 8000-QUERY-RTW-HISTORY
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
               ENTER TAL "SERVERCLASS_DIALOG_END_"
           END-IF.

       3000-UPDATE-RTW-STATUS.
      *---------------------------------------------------------------*
      * Update the return-to-work status for a claim. Validates        *
      * status transitions and records a milestone for each change.    *
      *                                                                *
      * Valid transitions:                                             *
      *   NW -> MD, TD, LD, FD, VR, TM                                *
      *   MD -> FD, NW, TD, VR, TM                                    *
      *   TD -> FD, MD, NW, VR, TM                                    *
      *   LD -> FD, MD, NW, VR, TM                                    *
      *   VR -> FD, NW, TM                                            *
      *   FD -> (terminal, no further transitions)                     *
      *---------------------------------------------------------------*
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD

      *    Read current status to validate transition
           MOVE WS-REQ-CLAIM-NUM TO RS-CLAIM-NUMBER
           MOVE 99999999 TO RS-STATUS-DATE
           START RTW-STATUS-FILE
               KEY IS NOT GREATER THAN RS-KEY
               INVALID KEY
      *            No prior status -- initial entry OK
                   MOVE "  " TO RM-PRIOR-STATUS
                   PERFORM 3100-WRITE-NEW-STATUS
                   EXIT PARAGRAPH
           END-START
           READ RTW-STATUS-FILE PREVIOUS
               AT END
                   MOVE "  " TO RM-PRIOR-STATUS
                   PERFORM 3100-WRITE-NEW-STATUS
                   EXIT PARAGRAPH
           END-READ

           IF RS-CLAIM-NUMBER NOT = WS-REQ-CLAIM-NUM
               MOVE "  " TO RM-PRIOR-STATUS
               PERFORM 3100-WRITE-NEW-STATUS
               EXIT PARAGRAPH
           END-IF

      *    Validate transition is allowed
           EVALUATE RS-STATUS-CODE
               WHEN "FD"
      *            Full duty is terminal -- cannot transition further
                   EXIT PARAGRAPH
               WHEN "NW"
                   IF WS-REQ-STATUS-CODE = "MD" OR "TD"
                   OR WS-REQ-STATUS-CODE = "LD" OR "FD"
                   OR WS-REQ-STATUS-CODE = "VR" OR "TM"
                       MOVE RS-STATUS-CODE TO RM-PRIOR-STATUS
                       PERFORM 3100-WRITE-NEW-STATUS
                   END-IF
               WHEN "MD" THRU "LD"
                   IF WS-REQ-STATUS-CODE = "FD" OR "NW"
                   OR WS-REQ-STATUS-CODE = "TD" OR "VR"
                   OR WS-REQ-STATUS-CODE = "TM"
                       MOVE RS-STATUS-CODE TO RM-PRIOR-STATUS
                       PERFORM 3100-WRITE-NEW-STATUS
                   END-IF
               WHEN "VR"
                   IF WS-REQ-STATUS-CODE = "FD" OR "NW"
                   OR WS-REQ-STATUS-CODE = "TM"
                       MOVE RS-STATUS-CODE TO RM-PRIOR-STATUS
                       PERFORM 3100-WRITE-NEW-STATUS
                   END-IF
               WHEN OTHER
                   MOVE RS-STATUS-CODE TO RM-PRIOR-STATUS
                   PERFORM 3100-WRITE-NEW-STATUS
           END-EVALUATE.

       3100-WRITE-NEW-STATUS.
           MOVE WS-REQ-CLAIM-NUM TO RS-CLAIM-NUMBER
           MOVE WS-CURRENT-DATE TO RS-STATUS-DATE
           MOVE WS-REQ-STATUS-CODE TO RS-STATUS-CODE
           WRITE RTW-STATUS-RECORD
               INVALID KEY
                   REWRITE RTW-STATUS-RECORD
           END-WRITE
      *    Record milestone for this transition
           MOVE WS-REQ-CLAIM-NUM TO RM-CLAIM-NUMBER
           MOVE WS-CURRENT-DATE TO RM-MILESTONE-DATE
           MOVE "RTWS" TO RM-MILESTONE-CODE
           STRING "RTW STATUS: " DELIMITED SIZE
                  WS-REQ-STATUS-CODE DELIMITED SIZE
                  " FROM " DELIMITED SIZE
                  RM-PRIOR-STATUS DELIMITED SIZE
               INTO RM-DESCRIPTION
           MOVE WS-REQ-STATUS-CODE TO RM-NEW-STATUS
           MOVE "Y" TO RM-SYSTEM-GENERATED
           WRITE RTW-MILESTONE-RECORD
               INVALID KEY
                   CONTINUE
           END-WRITE.

       4000-MATCH-MODIFIED-DUTY.
      *---------------------------------------------------------------*
      * Match the injured worker's restrictions against available       *
      * modified duty jobs from the employer's inventory. Uses a        *
      * scoring algorithm: higher score = better match.                 *
      *---------------------------------------------------------------*
      *    Get current restrictions from latest status
           PERFORM 4100-LOAD-CURRENT-RESTRICTIONS

           MOVE "N" TO WS-MATCH-FOUND
           MOVE SPACES TO WS-BEST-MATCH-JOB-ID
           MOVE ZERO TO WS-BEST-MATCH-SCORE

      *    Scan available jobs for this employer
           MOVE WS-REQ-EMPLOYER-ID TO RJ-EMPLOYER-ID
           MOVE SPACES TO RJ-JOB-ID
           START RTW-JOBS-FILE
               KEY IS NOT LESS THAN RJ-KEY
               INVALID KEY
                   EXIT PARAGRAPH
           END-START

           PERFORM UNTIL WS-RJ-STATUS NOT = "00"
               READ RTW-JOBS-FILE NEXT
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF RJ-EMPLOYER-ID = WS-REQ-EMPLOYER-ID
                       AND RJ-AVAILABLE = "Y"
                           PERFORM 4200-SCORE-JOB-MATCH
                           IF WS-CURRENT-SCORE > WS-BEST-MATCH-SCORE
                               MOVE WS-CURRENT-SCORE
                                   TO WS-BEST-MATCH-SCORE
                               MOVE RJ-JOB-ID
                                   TO WS-BEST-MATCH-JOB-ID
                               MOVE "Y" TO WS-MATCH-FOUND
                           END-IF
                       ELSE
                           IF RJ-EMPLOYER-ID NOT =
                               WS-REQ-EMPLOYER-ID
                               EXIT PERFORM
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.

       4100-LOAD-CURRENT-RESTRICTIONS.
           MOVE WS-REQ-CLAIM-NUM TO RS-CLAIM-NUMBER
           MOVE 99999999 TO RS-STATUS-DATE
           START RTW-STATUS-FILE
               KEY IS NOT GREATER THAN RS-KEY
               INVALID KEY
                   MOVE ZEROES TO WS-CURRENT-RESTRICTIONS
                   EXIT PARAGRAPH
           END-START
           READ RTW-STATUS-FILE PREVIOUS
               AT END
                   MOVE ZEROES TO WS-CURRENT-RESTRICTIONS
               NOT AT END
                   IF RS-CLAIM-NUMBER = WS-REQ-CLAIM-NUM
                       MOVE RS-LIFT-LIMIT-LBS TO WS-CR-LIFT-LIMIT
                       MOVE RS-STAND-HOURS TO WS-CR-STAND-HRS
                       MOVE RS-SIT-HOURS TO WS-CR-SIT-HRS
                       MOVE RS-NO-REPETITIVE-MOTION
                           TO WS-CR-NO-REPMOTION
                       MOVE RS-NO-OVERHEAD-REACH TO WS-CR-NO-OVERHEAD
                       MOVE RS-NO-BENDING TO WS-CR-NO-BENDING
                       MOVE RS-NO-CLIMBING TO WS-CR-NO-CLIMBING
                       MOVE RS-NO-DRIVING TO WS-CR-NO-DRIVING
                       MOVE RS-HOURS-PER-DAY TO WS-CR-HOURS-DAY
                       MOVE RS-DAYS-PER-WEEK TO WS-CR-DAYS-WEEK
                   END-IF
           END-READ.

       4200-SCORE-JOB-MATCH.
      *---------------------------------------------------------------*
      * Score how well a job matches the worker's restrictions.        *
      * 100 = perfect match. Deductions for each mismatch.            *
      *---------------------------------------------------------------*
           MOVE 100 TO WS-CURRENT-SCORE

      *    Lifting check (most critical restriction)
           IF WS-CR-LIFT-LIMIT > 0
           AND RJ-LIFT-MAX-LBS > WS-CR-LIFT-LIMIT
               SUBTRACT 30 FROM WS-CURRENT-SCORE
           END-IF

      *    Standing tolerance
           IF WS-CR-STAND-HRS > 0
           AND RJ-STAND-REQUIRED > WS-CR-STAND-HRS
               SUBTRACT 15 FROM WS-CURRENT-SCORE
           END-IF

      *    Sitting tolerance
           IF WS-CR-SIT-HRS > 0
           AND RJ-SIT-REQUIRED > WS-CR-SIT-HRS
               SUBTRACT 10 FROM WS-CURRENT-SCORE
           END-IF

      *    Repetitive motion restriction
           IF WS-CR-NO-REPMOTION = "Y"
           AND RJ-REPETITIVE-MOTION = "Y"
               SUBTRACT 15 FROM WS-CURRENT-SCORE
           END-IF

      *    Overhead reach restriction
           IF WS-CR-NO-OVERHEAD = "Y"
           AND RJ-OVERHEAD-REACH = "Y"
               SUBTRACT 10 FROM WS-CURRENT-SCORE
           END-IF

      *    Bending restriction
           IF WS-CR-NO-BENDING = "Y"
           AND RJ-BENDING-REQUIRED = "Y"
               SUBTRACT 10 FROM WS-CURRENT-SCORE
           END-IF

      *    Climbing restriction
           IF WS-CR-NO-CLIMBING = "Y"
           AND RJ-CLIMBING-REQUIRED = "Y"
               SUBTRACT 10 FROM WS-CURRENT-SCORE
           END-IF

      *    Driving restriction
           IF WS-CR-NO-DRIVING = "Y"
           AND RJ-DRIVING-REQUIRED = "Y"
               SUBTRACT 10 FROM WS-CURRENT-SCORE
           END-IF

      *    Hours per day check
           IF WS-CR-HOURS-DAY > 0
           AND RJ-HOURS-PER-DAY > WS-CR-HOURS-DAY
               SUBTRACT 10 FROM WS-CURRENT-SCORE
           END-IF

      *    Floor at zero
           IF WS-CURRENT-SCORE < 0
               MOVE ZERO TO WS-CURRENT-SCORE
           END-IF.

       5000-RECORD-FCE.
      *    Write FCE record -- data populated in request buffer
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE WS-REQ-CLAIM-NUM TO FC-CLAIM-NUMBER
           MOVE WS-CURRENT-DATE TO FC-EVAL-DATE
           WRITE RTW-FCE-RECORD
               INVALID KEY
                   REWRITE RTW-FCE-RECORD
           END-WRITE.

       6000-CHECK-VR-TRIGGERS.
      *---------------------------------------------------------------*
      * Evaluate whether vocational rehabilitation should be referred. *
      * Triggers:                                                      *
      *   - No modified duty available at employer                     *
      *   - Employee terminated during disability                      *
      *   - FCE shows inability to return to prior occupation          *
      *   - Maximum modified duty duration reached                     *
      *---------------------------------------------------------------*
           MOVE "N" TO WS-VR-TRIGGER-FOUND

      *    Check if employer has no accommodations
           MOVE WS-REQ-CLAIM-NUM TO RS-CLAIM-NUMBER
           MOVE 99999999 TO RS-STATUS-DATE
           START RTW-STATUS-FILE
               KEY IS NOT GREATER THAN RS-KEY
               INVALID KEY
                   EXIT PARAGRAPH
           END-START
           READ RTW-STATUS-FILE PREVIOUS
               AT END
                   EXIT PARAGRAPH
           END-READ

           IF RS-CLAIM-NUMBER = WS-REQ-CLAIM-NUM
      *        Employee terminated
               IF RS-STATUS-CODE = "TM"
                   MOVE "Y" TO WS-VR-TRIGGER-FOUND
                   MOVE "TM" TO VR-TRIGGER-REASON
                   PERFORM 6100-CREATE-VR-REFERRAL
               END-IF
      *        No accommodations available
               IF RS-EMPLOYER-ACCOM-AVAIL = "N"
                   MOVE "Y" TO WS-VR-TRIGGER-FOUND
                   MOVE "NM" TO VR-TRIGGER-REASON
                   PERFORM 6100-CREATE-VR-REFERRAL
               END-IF
           END-IF.

       6100-CREATE-VR-REFERRAL.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE WS-REQ-CLAIM-NUM TO VR-CLAIM-NUMBER
           MOVE 1 TO VR-REFERRAL-SEQ
           MOVE WS-CURRENT-DATE TO VR-REFERRAL-DATE
           MOVE "R" TO VR-STATUS
           MOVE SPACES TO VR-COUNSELOR-NAME
           MOVE ZEROES TO VR-ESTIMATED-COST
           WRITE RTW-VR-RECORD
               INVALID KEY
                   CONTINUE
           END-WRITE.

       7000-CALC-MODIFIED-DUTY-WAGE-LOSS.
      *---------------------------------------------------------------*
      * Calculate wage loss during modified duty for TPD benefits.     *
      * Uses PATHSEND to call WC-BEN-CALC for the TPD computation.    *
      *---------------------------------------------------------------*
      *    Get current modified duty wage from latest status
           PERFORM 4100-LOAD-CURRENT-RESTRICTIONS
           MOVE WS-REQ-CLAIM-NUM TO RS-CLAIM-NUMBER
           MOVE 99999999 TO RS-STATUS-DATE
           START RTW-STATUS-FILE
               KEY IS NOT GREATER THAN RS-KEY
               INVALID KEY
                   EXIT PARAGRAPH
           END-START
           READ RTW-STATUS-FILE PREVIOUS
               AT END
                   EXIT PARAGRAPH
           END-READ

           IF RS-CLAIM-NUMBER = WS-REQ-CLAIM-NUM
      *        Calculate weekly wages
               COMPUTE WS-MODIFIED-WEEKLY =
                   RS-MODIFIED-HOURLY-WAGE *
                   RS-HOURS-PER-DAY *
                   RS-DAYS-PER-WEEK
               COMPUTE WS-PRE-INJ-WEEKLY =
                   RS-PRE-INJURY-HOURLY-WAGE * 40

      *        Wage loss = pre-injury weekly - modified duty weekly
               COMPUTE WS-WAGE-LOSS-WEEKLY =
                   WS-PRE-INJ-WEEKLY - WS-MODIFIED-WEEKLY

      *        PATHSEND to benefits calculator for TPD computation
               MOVE "WC-BEN-CALC" TO WS-PS-SERVER
               MOVE 03 TO WS-PS-MSG-CODE
               MOVE WS-REQ-CLAIM-NUM TO WS-PS-CLAIM-NUM
               MOVE WS-REQ-JURISDICTION TO WS-PS-JURISDICTION
               MOVE WS-PRE-INJ-WEEKLY TO WS-PS-AWW
               MOVE WS-MODIFIED-WEEKLY TO WS-PS-POST-INJ-EARN

               ENTER TAL "PATHSEND_LINK_" USING
                   WS-PS-SERVER
                   WS-PS-MSG-CODE
                   WS-PS-CLAIM-NUM
                   WS-PS-JURISDICTION
                   WS-PS-AWW
                   WS-PS-POST-INJ-EARN
                   WS-PS-RESPONSE
           END-IF.

       7500-RECORD-MILESTONE.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE WS-REQ-CLAIM-NUM TO RM-CLAIM-NUMBER
           MOVE WS-CURRENT-DATE TO RM-MILESTONE-DATE
           MOVE "N" TO RM-SYSTEM-GENERATED
           WRITE RTW-MILESTONE-RECORD
               INVALID KEY
                   CONTINUE
           END-WRITE.

       8000-QUERY-RTW-HISTORY.
      *    Return all RTW status records for a claim
           MOVE WS-REQ-CLAIM-NUM TO RS-CLAIM-NUMBER
           MOVE ZEROES TO RS-STATUS-DATE
           START RTW-STATUS-FILE
               KEY IS NOT LESS THAN RS-KEY
               INVALID KEY
                   EXIT PARAGRAPH
           END-START
           PERFORM UNTIL WS-RS-STATUS NOT = "00"
               READ RTW-STATUS-FILE NEXT
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF RS-CLAIM-NUMBER NOT = WS-REQ-CLAIM-NUM
                           EXIT PERFORM
                       END-IF
               END-READ
           END-PERFORM.

       9000-TERMINATE.
           CLOSE RTW-STATUS-FILE
                 RTW-JOBS-FILE
                 RTW-FCE-FILE
                 RTW-VR-FILE
                 RTW-MILESTONE-FILE.
