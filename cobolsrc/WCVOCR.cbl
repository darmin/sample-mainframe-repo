       IDENTIFICATION DIVISION.
       PROGRAM-ID. WCVOCR.
      *================================================================
      * WCVOCR - Workers' Compensation Vocational Rehabilitation
      *
      * Tracks vocational rehabilitation cases including referral
      * criteria evaluation, rehab plan development, transferable
      * skills analysis, labor market surveys, job placement,
      * retraining programs, cost tracking, and outcome reporting.
      *
      * File: VOCREFF (Voc rehab case records, key-sequenced)
      * File: VOCSKLF (Transferable skills, entry-sequenced)
      * File: VOCJOBF (Job placement records, key-sequenced)
      * File: VOCRPTF (Voc rehab report output)
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VOC-CASE-FILE
               ASSIGN TO "VOCREFF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS VOC-CASE-KEY
               FILE STATUS IS WS-FILE-STATUS.

           SELECT VOC-SKILLS-FILE
               ASSIGN TO "VOCSKLF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

           SELECT VOC-JOB-FILE
               ASSIGN TO "VOCJOBF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS VOC-JOB-KEY
               FILE STATUS IS WS-FILE-STATUS.

           SELECT VOC-REPORT-FILE
               ASSIGN TO "VOCRPTF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  VOC-CASE-FILE.
       01  VOC-CASE-RECORD.
           05 VOC-CASE-KEY.
              10 VOC-CLAIM-NUMBER         PIC X(12).
              10 VOC-REFERRAL-SEQ         PIC 9(3).
           05 VOC-STATUS                  PIC X(2).
              88 VOC-STAT-REFERRED        VALUE "RF".
              88 VOC-STAT-EVAL            VALUE "EV".
              88 VOC-STAT-PLAN-DEV        VALUE "PD".
              88 VOC-STAT-RETRAINING      VALUE "RT".
              88 VOC-STAT-JOB-SEARCH      VALUE "JS".
              88 VOC-STAT-PLACED          VALUE "PL".
              88 VOC-STAT-CLOSED-SUCC     VALUE "CS".
              88 VOC-STAT-CLOSED-FAIL     VALUE "CF".
              88 VOC-STAT-DENIED          VALUE "DN".
           05 VOC-REFERRAL-DATE           PIC 9(8).
           05 VOC-COUNSELOR-ID            PIC X(8).
           05 VOC-COUNSELOR-NAME          PIC X(30).
           05 VOC-EXPERT-ID               PIC X(8).
           05 VOC-EXPERT-NAME             PIC X(30).
           05 VOC-CLAIMANT-INFO.
              10 VOC-CLAIMANT-NAME        PIC X(30).
              10 VOC-CLAIMANT-DOB         PIC 9(8).
              10 VOC-EDUCATION-LEVEL      PIC X(2).
                 88 VOC-ED-LESS-HS        VALUE "LH".
                 88 VOC-ED-HS-DIPLOMA     VALUE "HS".
                 88 VOC-ED-SOME-COLLEGE   VALUE "SC".
                 88 VOC-ED-ASSOCIATES     VALUE "AS".
                 88 VOC-ED-BACHELORS      VALUE "BA".
                 88 VOC-ED-MASTERS        VALUE "MA".
                 88 VOC-ED-DOCTORAL       VALUE "DO".
              10 VOC-PRE-INJURY-JOB       PIC X(30).
              10 VOC-PRE-INJURY-DOT       PIC X(9).
              10 VOC-PRE-INJURY-WAGE      PIC 9(7)V99.
              10 VOC-YEARS-EXPERIENCE     PIC 9(2).
              10 VOC-INJURY-DATE          PIC 9(8).
           05 VOC-MEDICAL-RESTRICTIONS.
              10 VOC-LIFT-RESTRICT        PIC 9(3).
              10 VOC-STAND-RESTRICT       PIC 9(2).
              10 VOC-SIT-RESTRICT         PIC 9(2).
              10 VOC-WALK-RESTRICT        PIC 9(2).
              10 VOC-REACH-RESTRICT       PIC X(1).
                 88 VOC-REACH-NO-LIMIT    VALUE "N".
                 88 VOC-REACH-LIMITED     VALUE "L".
                 88 VOC-REACH-NONE        VALUE "X".
              10 VOC-GRIP-RESTRICT        PIC X(1).
              10 VOC-COGNITIVE-LIMIT      PIC X(1).
                 88 VOC-COG-NONE          VALUE "N".
                 88 VOC-COG-MILD          VALUE "M".
                 88 VOC-COG-MODERATE      VALUE "O".
                 88 VOC-COG-SEVERE        VALUE "S".
              10 VOC-PHYSICAL-DEMAND      PIC X(1).
                 88 VOC-DEMAND-SEDENTARY  VALUE "S".
                 88 VOC-DEMAND-LIGHT      VALUE "L".
                 88 VOC-DEMAND-MEDIUM     VALUE "M".
                 88 VOC-DEMAND-HEAVY      VALUE "H".
           05 VOC-REFERRAL-CRITERIA.
              10 VOC-UNABLE-PRIOR-JOB     PIC X(1).
              10 VOC-PERMANENT-RESTRICT   PIC X(1).
              10 VOC-EMPLOYER-NO-JOB      PIC X(1).
              10 VOC-MEETS-CRITERIA       PIC X(1).
              10 VOC-CRITERIA-EVAL-DATE   PIC 9(8).
              10 VOC-CRITERIA-NOTES       PIC X(100).
           05 VOC-REHAB-PLAN.
              10 VOC-PLAN-TYPE            PIC X(2).
                 88 VOC-PLAN-DIRECT-PLACE VALUE "DP".
                 88 VOC-PLAN-OJT          VALUE "OJ".
                 88 VOC-PLAN-SHORT-RETRAIN VALUE "SR".
                 88 VOC-PLAN-LONG-RETRAIN VALUE "LR".
                 88 VOC-PLAN-SELF-EMPLOY  VALUE "SE".
              10 VOC-PLAN-STATUS          PIC X(1).
                 88 VOC-PLAN-DRAFT        VALUE "D".
                 88 VOC-PLAN-APPROVED     VALUE "A".
                 88 VOC-PLAN-IN-PROGRESS  VALUE "I".
                 88 VOC-PLAN-COMPLETED    VALUE "C".
                 88 VOC-PLAN-ABANDONED    VALUE "X".
              10 VOC-PLAN-START-DATE      PIC 9(8).
              10 VOC-PLAN-TARGET-END      PIC 9(8).
              10 VOC-PLAN-ACTUAL-END      PIC 9(8).
              10 VOC-TARGET-OCCUPATION    PIC X(30).
              10 VOC-TARGET-DOT-CODE      PIC X(9).
              10 VOC-TARGET-WAGE          PIC 9(7)V99.
              10 VOC-PLAN-DESCRIPTION     PIC X(200).
           05 VOC-RETRAINING.
              10 VOC-SCHOOL-NAME          PIC X(40).
              10 VOC-PROGRAM-NAME         PIC X(40).
              10 VOC-PROGRAM-LENGTH       PIC 9(3).
              10 VOC-PROGRAM-UNIT         PIC X(1).
                 88 VOC-UNIT-WEEKS        VALUE "W".
                 88 VOC-UNIT-MONTHS       VALUE "M".
              10 VOC-TUITION-COST         PIC 9(7)V99.
              10 VOC-BOOKS-COST           PIC 9(5)V99.
              10 VOC-SUPPLIES-COST        PIC 9(5)V99.
              10 VOC-PROGRESS-PCT         PIC 9(3).
              10 VOC-CURRENT-GPA          PIC 9(1)V99.
              10 VOC-CREDITS-COMPLETED    PIC 9(3).
              10 VOC-CREDITS-REQUIRED     PIC 9(3).
           05 VOC-LABOR-MARKET.
              10 VOC-LMS-DATE             PIC 9(8).
              10 VOC-LMS-AREA             PIC X(30).
              10 VOC-JOBS-IDENTIFIED      PIC 9(4).
              10 VOC-AVG-WAGE-RANGE-LOW   PIC 9(7)V99.
              10 VOC-AVG-WAGE-RANGE-HIGH  PIC 9(7)V99.
              10 VOC-GROWTH-OUTLOOK       PIC X(1).
                 88 VOC-GROWTH-STRONG     VALUE "S".
                 88 VOC-GROWTH-MODERATE   VALUE "M".
                 88 VOC-GROWTH-DECLINING  VALUE "D".
           05 VOC-COST-TRACKING.
              10 VOC-EVAL-COST            PIC 9(7)V99.
              10 VOC-COUNSELING-COST      PIC 9(7)V99.
              10 VOC-RETRAIN-COST         PIC 9(7)V99.
              10 VOC-JOB-SEARCH-COST      PIC 9(7)V99.
              10 VOC-TOOL-EQUIP-COST      PIC 9(7)V99.
              10 VOC-TRAVEL-COST          PIC 9(5)V99.
              10 VOC-TOTAL-COST           PIC 9(8)V99.
              10 VOC-BUDGET-APPROVED      PIC 9(8)V99.
           05 VOC-OUTCOME.
              10 VOC-PLACEMENT-DATE       PIC 9(8).
              10 VOC-PLACEMENT-EMPLOYER   PIC X(30).
              10 VOC-PLACEMENT-JOB-TITLE  PIC X(30).
              10 VOC-PLACEMENT-WAGE       PIC 9(7)V99.
              10 VOC-WAGE-EARNING-CAP     PIC 9(7)V99.
              10 VOC-PLACEMENT-90-DAY     PIC X(1).
                 88 VOC-90-DAY-RETAINED   VALUE "Y".
                 88 VOC-90-DAY-LOST       VALUE "N".
                 88 VOC-90-DAY-PENDING    VALUE "P".
              10 VOC-CLOSE-DATE           PIC 9(8).
              10 VOC-CLOSE-REASON         PIC X(2).

       FD  VOC-SKILLS-FILE.
       01  VOC-SKILLS-RECORD.
           05 VOC-SKL-CLAIM-NUMBER        PIC X(12).
           05 VOC-SKL-SEQ                 PIC 9(3).
           05 VOC-SKL-DOT-CODE            PIC X(9).
           05 VOC-SKL-TITLE               PIC X(30).
           05 VOC-SKL-SVP-LEVEL           PIC 9(1).
           05 VOC-SKL-PHYSICAL-DEMAND     PIC X(1).
           05 VOC-SKL-WITHIN-RESTRICT     PIC X(1).
           05 VOC-SKL-WAGE-RANGE-LOW      PIC 9(7)V99.
           05 VOC-SKL-WAGE-RANGE-HIGH     PIC 9(7)V99.
           05 VOC-SKL-AVAILABILITY        PIC X(1).
              88 VOC-SKL-AVAILABLE        VALUE "Y".
              88 VOC-SKL-RARE             VALUE "R".
              88 VOC-SKL-UNAVAILABLE      VALUE "N".

       FD  VOC-JOB-FILE.
       01  VOC-JOB-RECORD.
           05 VOC-JOB-KEY.
              10 VOC-JOB-CLAIM-NUMBER     PIC X(12).
              10 VOC-JOB-SEQ              PIC 9(3).
           05 VOC-JOB-DATE                PIC 9(8).
           05 VOC-JOB-EMPLOYER            PIC X(30).
           05 VOC-JOB-TITLE               PIC X(30).
           05 VOC-JOB-WAGE               PIC 9(7)V99.
           05 VOC-JOB-CONTACT-TYPE        PIC X(1).
              88 VOC-JOB-CT-PHONE         VALUE "P".
              88 VOC-JOB-CT-EMAIL         VALUE "E".
              88 VOC-JOB-CT-IN-PERSON     VALUE "I".
              88 VOC-JOB-CT-ONLINE        VALUE "O".
           05 VOC-JOB-RESULT              PIC X(1).
              88 VOC-JOB-APPLIED          VALUE "A".
              88 VOC-JOB-INTERVIEWED      VALUE "I".
              88 VOC-JOB-OFFERED          VALUE "O".
              88 VOC-JOB-DECLINED         VALUE "D".
              88 VOC-JOB-REJECTED         VALUE "R".
              88 VOC-JOB-HIRED            VALUE "H".
           05 VOC-JOB-NOTES               PIC X(200).

       FD  VOC-REPORT-FILE.
       01  VOC-REPORT-LINE                PIC X(132).

       WORKING-STORAGE SECTION.
      *
      *    Cross-program copybook references
           COPY WCCLMCPY
           COPY WCEMPCPY
       01  WS-FILE-STATUS                 PIC X(2).
       01  WS-CURRENT-DATE                PIC 9(8).
       01  WS-EOF-FLAG                    PIC X(1) VALUE "N".
           88 WS-EOF                      VALUE "Y".

       01  WS-REQUEST-AREA.
           05 WS-REQ-FUNCTION             PIC X(2).
              88 WS-REQ-ADD-REFERRAL      VALUE "AR".
              88 WS-REQ-EVAL-CRITERIA     VALUE "EC".
              88 WS-REQ-DEVELOP-PLAN      VALUE "DP".
              88 WS-REQ-ADD-SKILLS        VALUE "AS".
              88 WS-REQ-LOG-JOB-CONTACT   VALUE "JC".
              88 WS-REQ-UPDATE-RETRAIN    VALUE "UR".
              88 WS-REQ-UPDATE-PLACEMENT  VALUE "UP".
              88 WS-REQ-CLOSE-CASE        VALUE "CC".
              88 WS-REQ-GEN-REPORT        VALUE "GR".
           05 WS-REQ-CLAIM-NUMBER         PIC X(12).
           05 WS-REQ-DATA                 PIC X(500).

       01  WS-REPORT-TOTALS.
           05 WS-RPT-TOTAL-CASES          PIC 9(5) VALUE 0.
           05 WS-RPT-OPEN-CASES           PIC 9(5) VALUE 0.
           05 WS-RPT-PLACED-CASES         PIC 9(5) VALUE 0.
           05 WS-RPT-FAILED-CASES         PIC 9(5) VALUE 0.
           05 WS-RPT-DENIED-CASES         PIC 9(5) VALUE 0.
           05 WS-RPT-TOTAL-EVAL-COST      PIC 9(9)V99 VALUE 0.
           05 WS-RPT-TOTAL-RETRAIN-COST   PIC 9(9)V99 VALUE 0.
           05 WS-RPT-TOTAL-ALL-COST       PIC 9(9)V99 VALUE 0.
           05 WS-RPT-TOTAL-PRE-WAGE       PIC 9(9)V99 VALUE 0.
           05 WS-RPT-TOTAL-POST-WAGE      PIC 9(9)V99 VALUE 0.
           05 WS-RPT-WAGE-REPLACE-PCT     PIC 9(3)V99 VALUE 0.
           05 WS-RPT-PLACEMENT-RATE       PIC 9(3)V99 VALUE 0.
           05 WS-RPT-90-DAY-RETAIN-RATE   PIC 9(3)V99 VALUE 0.
           05 WS-RPT-PLACED-90-COUNT      PIC 9(5) VALUE 0.
           05 WS-RPT-RETAINED-90-COUNT    PIC 9(5) VALUE 0.
           05 WS-RPT-CLOSED-TOTAL         PIC 9(5) VALUE 0.

       01  WS-WORK-FIELDS.
           05 WS-WAGE-RATIO               PIC 9(3)V99.
           05 WS-EARNING-CAPACITY-LOSS     PIC 9(7)V99.
           05 WS-DATE-DIFF-DAYS           PIC S9(5).
           05 WS-COST-ACCUM               PIC 9(8)V99.

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
      *
      *    Inter-program communication calls
           CALL "WCBENCALC"
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-REQUEST
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           OPEN I-O VOC-CASE-FILE
           OPEN I-O VOC-JOB-FILE
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD.

       2000-PROCESS-REQUEST.
           EVALUATE TRUE
               WHEN WS-REQ-ADD-REFERRAL
                   PERFORM 3000-ADD-REFERRAL
               WHEN WS-REQ-EVAL-CRITERIA
                   PERFORM 3100-EVALUATE-CRITERIA
               WHEN WS-REQ-DEVELOP-PLAN
                   PERFORM 3200-DEVELOP-PLAN
               WHEN WS-REQ-ADD-SKILLS
                   PERFORM 3300-ADD-TRANSFERABLE-SKILLS
               WHEN WS-REQ-LOG-JOB-CONTACT
                   PERFORM 3400-LOG-JOB-CONTACT
               WHEN WS-REQ-UPDATE-RETRAIN
                   PERFORM 3500-UPDATE-RETRAINING
               WHEN WS-REQ-UPDATE-PLACEMENT
                   PERFORM 3600-UPDATE-PLACEMENT
               WHEN WS-REQ-CLOSE-CASE
                   PERFORM 3700-CLOSE-CASE
               WHEN WS-REQ-GEN-REPORT
                   PERFORM 4000-GENERATE-OUTCOME-REPORT
           END-EVALUATE.

       3000-ADD-REFERRAL.
      *    Create new vocational rehab referral
           INITIALIZE VOC-CASE-RECORD
           MOVE WS-REQ-CLAIM-NUMBER TO VOC-CLAIM-NUMBER
           MOVE 1 TO VOC-REFERRAL-SEQ
           SET VOC-STAT-REFERRED TO TRUE
           MOVE WS-CURRENT-DATE TO VOC-REFERRAL-DATE
           MOVE WS-REQ-DATA(1:30)  TO VOC-CLAIMANT-NAME
           MOVE WS-REQ-DATA(31:8)  TO VOC-CLAIMANT-DOB
           MOVE WS-REQ-DATA(39:2)  TO VOC-EDUCATION-LEVEL
           MOVE WS-REQ-DATA(41:30) TO VOC-PRE-INJURY-JOB
           MOVE WS-REQ-DATA(71:9)  TO VOC-PRE-INJURY-DOT
           MOVE WS-REQ-DATA(80:9)  TO VOC-PRE-INJURY-WAGE
           MOVE WS-REQ-DATA(89:2)  TO VOC-YEARS-EXPERIENCE
           MOVE WS-REQ-DATA(91:8)  TO VOC-INJURY-DATE
           INITIALIZE VOC-COST-TRACKING
           INITIALIZE VOC-OUTCOME
           WRITE VOC-CASE-RECORD
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "ERROR WRITING VOC REFERRAL: " WS-FILE-STATUS
           END-IF.

       3100-EVALUATE-CRITERIA.
      *    Evaluate whether claimant meets voc rehab referral criteria
      *    Criteria: (1) cannot return to prior job, (2) permanent
      *    restrictions, (3) employer has no suitable modified duty
           MOVE WS-REQ-CLAIM-NUMBER TO VOC-CLAIM-NUMBER
           MOVE 1 TO VOC-REFERRAL-SEQ
           READ VOC-CASE-FILE
               INVALID KEY
                   DISPLAY "VOC CASE NOT FOUND"
                   GO TO 3100-EXIT
           END-READ

      *    Parse medical restriction data
           MOVE WS-REQ-DATA(1:3)   TO VOC-LIFT-RESTRICT
           MOVE WS-REQ-DATA(4:2)   TO VOC-STAND-RESTRICT
           MOVE WS-REQ-DATA(6:2)   TO VOC-SIT-RESTRICT
           MOVE WS-REQ-DATA(8:2)   TO VOC-WALK-RESTRICT
           MOVE WS-REQ-DATA(10:1)  TO VOC-REACH-RESTRICT
           MOVE WS-REQ-DATA(11:1)  TO VOC-GRIP-RESTRICT
           MOVE WS-REQ-DATA(12:1)  TO VOC-COGNITIVE-LIMIT

      *    Determine physical demand level from restrictions
           EVALUATE TRUE
               WHEN VOC-LIFT-RESTRICT <= 10
                   SET VOC-DEMAND-SEDENTARY TO TRUE
               WHEN VOC-LIFT-RESTRICT <= 20
                   SET VOC-DEMAND-LIGHT TO TRUE
               WHEN VOC-LIFT-RESTRICT <= 50
                   SET VOC-DEMAND-MEDIUM TO TRUE
               WHEN OTHER
                   SET VOC-DEMAND-HEAVY TO TRUE
           END-EVALUATE

      *    Evaluate three referral criteria
           MOVE WS-REQ-DATA(13:1) TO VOC-UNABLE-PRIOR-JOB
           MOVE WS-REQ-DATA(14:1) TO VOC-PERMANENT-RESTRICT
           MOVE WS-REQ-DATA(15:1) TO VOC-EMPLOYER-NO-JOB
           MOVE WS-CURRENT-DATE TO VOC-CRITERIA-EVAL-DATE
           MOVE WS-REQ-DATA(16:100) TO VOC-CRITERIA-NOTES

      *    All three must be "Y" to qualify
           IF VOC-UNABLE-PRIOR-JOB = "Y"
               AND VOC-PERMANENT-RESTRICT = "Y"
               AND VOC-EMPLOYER-NO-JOB = "Y"
               MOVE "Y" TO VOC-MEETS-CRITERIA
               SET VOC-STAT-EVAL TO TRUE
           ELSE
               MOVE "N" TO VOC-MEETS-CRITERIA
               SET VOC-STAT-DENIED TO TRUE
           END-IF

           REWRITE VOC-CASE-RECORD.
       3100-EXIT.
           EXIT.

       3200-DEVELOP-PLAN.
      *    Develop vocational rehabilitation plan
           MOVE WS-REQ-CLAIM-NUMBER TO VOC-CLAIM-NUMBER
           MOVE 1 TO VOC-REFERRAL-SEQ
           READ VOC-CASE-FILE
               INVALID KEY
                   DISPLAY "VOC CASE NOT FOUND"
                   GO TO 3200-EXIT
           END-READ

           MOVE WS-REQ-DATA(1:2)   TO VOC-PLAN-TYPE
           MOVE WS-CURRENT-DATE    TO VOC-PLAN-START-DATE
           MOVE WS-REQ-DATA(3:8)   TO VOC-PLAN-TARGET-END
           MOVE WS-REQ-DATA(11:30) TO VOC-TARGET-OCCUPATION
           MOVE WS-REQ-DATA(41:9)  TO VOC-TARGET-DOT-CODE
           MOVE WS-REQ-DATA(50:9)  TO VOC-TARGET-WAGE
           MOVE WS-REQ-DATA(59:200) TO VOC-PLAN-DESCRIPTION
           MOVE WS-REQ-DATA(259:9) TO VOC-BUDGET-APPROVED
           SET VOC-PLAN-DRAFT TO TRUE
           SET VOC-STAT-PLAN-DEV TO TRUE

      *    Assign vocational counselor and expert
           MOVE WS-REQ-DATA(268:8) TO VOC-COUNSELOR-ID
           MOVE WS-REQ-DATA(276:30) TO VOC-COUNSELOR-NAME

      *    If retraining plan, populate school info
           IF VOC-PLAN-SHORT-RETRAIN OR VOC-PLAN-LONG-RETRAIN
               MOVE WS-REQ-DATA(306:40) TO VOC-SCHOOL-NAME
               MOVE WS-REQ-DATA(346:40) TO VOC-PROGRAM-NAME
               MOVE WS-REQ-DATA(386:3)  TO VOC-PROGRAM-LENGTH
               MOVE WS-REQ-DATA(389:1)  TO VOC-PROGRAM-UNIT
               MOVE WS-REQ-DATA(390:9)  TO VOC-TUITION-COST
               MOVE WS-REQ-DATA(399:7)  TO VOC-BOOKS-COST
               MOVE WS-REQ-DATA(406:7)  TO VOC-SUPPLIES-COST
               MOVE ZEROS TO VOC-PROGRESS-PCT
               MOVE ZEROS TO VOC-CREDITS-COMPLETED
               MOVE WS-REQ-DATA(413:3)  TO VOC-CREDITS-REQUIRED
           END-IF

      *    Initial eval cost
           MOVE WS-REQ-DATA(416:9) TO VOC-EVAL-COST
           ADD VOC-EVAL-COST TO VOC-TOTAL-COST

           REWRITE VOC-CASE-RECORD.
       3200-EXIT.
           EXIT.

       3300-ADD-TRANSFERABLE-SKILLS.
      *    Record transferable skills analysis results
           OPEN OUTPUT VOC-SKILLS-FILE
           INITIALIZE VOC-SKILLS-RECORD
           MOVE WS-REQ-CLAIM-NUMBER TO VOC-SKL-CLAIM-NUMBER
           MOVE WS-REQ-DATA(1:3) TO VOC-SKL-SEQ
           MOVE WS-REQ-DATA(4:9) TO VOC-SKL-DOT-CODE
           MOVE WS-REQ-DATA(13:30) TO VOC-SKL-TITLE
           MOVE WS-REQ-DATA(43:1) TO VOC-SKL-SVP-LEVEL
           MOVE WS-REQ-DATA(44:1) TO VOC-SKL-PHYSICAL-DEMAND
           MOVE WS-REQ-DATA(45:1) TO VOC-SKL-WITHIN-RESTRICT
           MOVE WS-REQ-DATA(46:9) TO VOC-SKL-WAGE-RANGE-LOW
           MOVE WS-REQ-DATA(55:9) TO VOC-SKL-WAGE-RANGE-HIGH
           MOVE WS-REQ-DATA(64:1) TO VOC-SKL-AVAILABILITY
           WRITE VOC-SKILLS-RECORD
           CLOSE VOC-SKILLS-FILE.

       3400-LOG-JOB-CONTACT.
      *    Record job placement contact/application
           INITIALIZE VOC-JOB-RECORD
           MOVE WS-REQ-CLAIM-NUMBER TO VOC-JOB-CLAIM-NUMBER
           MOVE WS-REQ-DATA(1:3)   TO VOC-JOB-SEQ
           MOVE WS-CURRENT-DATE    TO VOC-JOB-DATE
           MOVE WS-REQ-DATA(4:30)  TO VOC-JOB-EMPLOYER
           MOVE WS-REQ-DATA(34:30) TO VOC-JOB-TITLE
           MOVE WS-REQ-DATA(64:9)  TO VOC-JOB-WAGE
           MOVE WS-REQ-DATA(73:1)  TO VOC-JOB-CONTACT-TYPE
           MOVE WS-REQ-DATA(74:1)  TO VOC-JOB-RESULT
           MOVE WS-REQ-DATA(75:200) TO VOC-JOB-NOTES
           WRITE VOC-JOB-RECORD

      *    Update job search costs on main case
           MOVE WS-REQ-CLAIM-NUMBER TO VOC-CLAIM-NUMBER
           MOVE 1 TO VOC-REFERRAL-SEQ
           READ VOC-CASE-FILE
               INVALID KEY
                   GO TO 3400-EXIT
           END-READ
           SET VOC-STAT-JOB-SEARCH TO TRUE
           ADD 75.00 TO VOC-JOB-SEARCH-COST
           ADD 75.00 TO VOC-TOTAL-COST
           REWRITE VOC-CASE-RECORD.
       3400-EXIT.
           EXIT.

       3500-UPDATE-RETRAINING.
      *    Update retraining progress for active students
           MOVE WS-REQ-CLAIM-NUMBER TO VOC-CLAIM-NUMBER
           MOVE 1 TO VOC-REFERRAL-SEQ
           READ VOC-CASE-FILE
               INVALID KEY
                   DISPLAY "VOC CASE NOT FOUND"
                   GO TO 3500-EXIT
           END-READ

           SET VOC-STAT-RETRAINING TO TRUE
           SET VOC-PLAN-IN-PROGRESS TO TRUE
           MOVE WS-REQ-DATA(1:3) TO VOC-PROGRESS-PCT
           MOVE WS-REQ-DATA(4:3) TO VOC-CURRENT-GPA
           MOVE WS-REQ-DATA(7:3) TO VOC-CREDITS-COMPLETED

      *    Accumulate retraining costs (tuition installment)
           COMPUTE WS-COST-ACCUM =
               FUNCTION NUMVAL(WS-REQ-DATA(10:9))
           ADD WS-COST-ACCUM TO VOC-RETRAIN-COST
           ADD WS-COST-ACCUM TO VOC-TOTAL-COST

      *    Check budget compliance
           IF VOC-TOTAL-COST > VOC-BUDGET-APPROVED
               DISPLAY "WARNING: VOC REHAB BUDGET EXCEEDED FOR "
                       VOC-CLAIM-NUMBER
                       " TOTAL: " VOC-TOTAL-COST
                       " BUDGET: " VOC-BUDGET-APPROVED
           END-IF

           REWRITE VOC-CASE-RECORD.
       3500-EXIT.
           EXIT.

       3600-UPDATE-PLACEMENT.
      *    Record job placement outcome
           MOVE WS-REQ-CLAIM-NUMBER TO VOC-CLAIM-NUMBER
           MOVE 1 TO VOC-REFERRAL-SEQ
           READ VOC-CASE-FILE
               INVALID KEY
                   DISPLAY "VOC CASE NOT FOUND"
                   GO TO 3600-EXIT
           END-READ

           SET VOC-STAT-PLACED TO TRUE
           MOVE WS-CURRENT-DATE TO VOC-PLACEMENT-DATE
           MOVE WS-REQ-DATA(1:30) TO VOC-PLACEMENT-EMPLOYER
           MOVE WS-REQ-DATA(31:30) TO VOC-PLACEMENT-JOB-TITLE
           MOVE WS-REQ-DATA(61:9) TO VOC-PLACEMENT-WAGE
           SET VOC-90-DAY-PENDING TO TRUE

      *    Calculate wage earning capacity
           IF VOC-PRE-INJURY-WAGE > 0
               COMPUTE VOC-WAGE-EARNING-CAP =
                   (VOC-PLACEMENT-WAGE / VOC-PRE-INJURY-WAGE) * 100
               IF VOC-PLACEMENT-WAGE < VOC-PRE-INJURY-WAGE
                   COMPUTE WS-EARNING-CAPACITY-LOSS =
                       VOC-PRE-INJURY-WAGE - VOC-PLACEMENT-WAGE
               ELSE
                   MOVE ZEROS TO WS-EARNING-CAPACITY-LOSS
               END-IF
           END-IF

           REWRITE VOC-CASE-RECORD.
       3600-EXIT.
           EXIT.

       3700-CLOSE-CASE.
      *    Close vocational rehabilitation case
           MOVE WS-REQ-CLAIM-NUMBER TO VOC-CLAIM-NUMBER
           MOVE 1 TO VOC-REFERRAL-SEQ
           READ VOC-CASE-FILE
               INVALID KEY
                   DISPLAY "VOC CASE NOT FOUND"
                   GO TO 3700-EXIT
           END-READ

           MOVE WS-CURRENT-DATE TO VOC-CLOSE-DATE
           MOVE WS-REQ-DATA(1:2) TO VOC-CLOSE-REASON
           MOVE WS-REQ-DATA(3:1) TO VOC-PLACEMENT-90-DAY

      *    Determine close status based on outcome
           IF VOC-PLACEMENT-WAGE > 0
               AND VOC-90-DAY-RETAINED
               SET VOC-STAT-CLOSED-SUCC TO TRUE
               SET VOC-PLAN-COMPLETED TO TRUE
               MOVE WS-CURRENT-DATE TO VOC-PLAN-ACTUAL-END
           ELSE
               SET VOC-STAT-CLOSED-FAIL TO TRUE
               SET VOC-PLAN-ABANDONED TO TRUE
           END-IF

           REWRITE VOC-CASE-RECORD.
       3700-EXIT.
           EXIT.

       4000-GENERATE-OUTCOME-REPORT.
      *    Generate vocational rehabilitation outcome report
           OPEN OUTPUT VOC-REPORT-FILE
           INITIALIZE WS-REPORT-TOTALS

      *    Report header
           STRING "VOCATIONAL REHABILITATION OUTCOME REPORT"
               DELIMITED BY SIZE
               "    DATE: " DELIMITED BY SIZE
               WS-CURRENT-DATE DELIMITED BY SIZE
               INTO VOC-REPORT-LINE
           WRITE VOC-REPORT-LINE
           MOVE ALL "=" TO VOC-REPORT-LINE
           WRITE VOC-REPORT-LINE
           MOVE SPACES TO VOC-REPORT-LINE
           WRITE VOC-REPORT-LINE

      *    Read all cases
           MOVE LOW-VALUES TO VOC-CASE-KEY
           START VOC-CASE-FILE KEY >= VOC-CASE-KEY
               INVALID KEY
                   GO TO 4000-WRITE-SUMMARY
           END-START

           MOVE "N" TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ VOC-CASE-FILE NEXT
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-RPT-TOTAL-CASES
                       EVALUATE TRUE
                           WHEN VOC-STAT-CLOSED-SUCC
                               ADD 1 TO WS-RPT-PLACED-CASES
                               ADD 1 TO WS-RPT-CLOSED-TOTAL
                               ADD VOC-PRE-INJURY-WAGE
                                   TO WS-RPT-TOTAL-PRE-WAGE
                               ADD VOC-PLACEMENT-WAGE
                                   TO WS-RPT-TOTAL-POST-WAGE
                               IF VOC-90-DAY-RETAINED
                                   ADD 1
                                       TO WS-RPT-RETAINED-90-COUNT
                               END-IF
                               ADD 1 TO WS-RPT-PLACED-90-COUNT
                           WHEN VOC-STAT-CLOSED-FAIL
                               ADD 1 TO WS-RPT-FAILED-CASES
                               ADD 1 TO WS-RPT-CLOSED-TOTAL
                           WHEN VOC-STAT-DENIED
                               ADD 1 TO WS-RPT-DENIED-CASES
                           WHEN OTHER
                               ADD 1 TO WS-RPT-OPEN-CASES
                       END-EVALUATE
                       ADD VOC-EVAL-COST
                           TO WS-RPT-TOTAL-EVAL-COST
                       ADD VOC-RETRAIN-COST
                           TO WS-RPT-TOTAL-RETRAIN-COST
                       ADD VOC-TOTAL-COST
                           TO WS-RPT-TOTAL-ALL-COST

      *                Write detail line
                       STRING VOC-CLAIM-NUMBER DELIMITED BY SIZE
                           "  " DELIMITED BY SIZE
                           VOC-CLAIMANT-NAME(1:20)
                               DELIMITED BY SIZE
                           "  " DELIMITED BY SIZE
                           VOC-STATUS DELIMITED BY SIZE
                           "  PRE-WAGE: " DELIMITED BY SIZE
                           VOC-PRE-INJURY-WAGE DELIMITED BY SIZE
                           "  POST-WAGE: " DELIMITED BY SIZE
                           VOC-PLACEMENT-WAGE DELIMITED BY SIZE
                           INTO VOC-REPORT-LINE
                       WRITE VOC-REPORT-LINE
               END-READ
           END-PERFORM.

       4000-WRITE-SUMMARY.
      *    Calculate outcome metrics
           IF WS-RPT-CLOSED-TOTAL > 0
               COMPUTE WS-RPT-PLACEMENT-RATE =
                   (WS-RPT-PLACED-CASES / WS-RPT-CLOSED-TOTAL)
                   * 100
           END-IF
           IF WS-RPT-PLACED-90-COUNT > 0
               COMPUTE WS-RPT-90-DAY-RETAIN-RATE =
                   (WS-RPT-RETAINED-90-COUNT /
                    WS-RPT-PLACED-90-COUNT) * 100
           END-IF
           IF WS-RPT-TOTAL-PRE-WAGE > 0
               COMPUTE WS-RPT-WAGE-REPLACE-PCT =
                   (WS-RPT-TOTAL-POST-WAGE /
                    WS-RPT-TOTAL-PRE-WAGE) * 100
           END-IF

           MOVE SPACES TO VOC-REPORT-LINE
           WRITE VOC-REPORT-LINE
           MOVE ALL "=" TO VOC-REPORT-LINE
           WRITE VOC-REPORT-LINE
           STRING "SUMMARY: TOTAL=" DELIMITED BY SIZE
               WS-RPT-TOTAL-CASES DELIMITED BY SIZE
               " OPEN=" DELIMITED BY SIZE
               WS-RPT-OPEN-CASES DELIMITED BY SIZE
               " PLACED=" DELIMITED BY SIZE
               WS-RPT-PLACED-CASES DELIMITED BY SIZE
               " FAILED=" DELIMITED BY SIZE
               WS-RPT-FAILED-CASES DELIMITED BY SIZE
               " DENIED=" DELIMITED BY SIZE
               WS-RPT-DENIED-CASES DELIMITED BY SIZE
               INTO VOC-REPORT-LINE
           WRITE VOC-REPORT-LINE
           STRING "PLACEMENT RATE: " DELIMITED BY SIZE
               WS-RPT-PLACEMENT-RATE DELIMITED BY SIZE
               "%  90-DAY RETENTION: " DELIMITED BY SIZE
               WS-RPT-90-DAY-RETAIN-RATE DELIMITED BY SIZE
               "%  WAGE REPLACEMENT: " DELIMITED BY SIZE
               WS-RPT-WAGE-REPLACE-PCT DELIMITED BY SIZE
               "%" DELIMITED BY SIZE
               INTO VOC-REPORT-LINE
           WRITE VOC-REPORT-LINE
           STRING "TOTAL COST: $" DELIMITED BY SIZE
               WS-RPT-TOTAL-ALL-COST DELIMITED BY SIZE
               "  EVAL: $" DELIMITED BY SIZE
               WS-RPT-TOTAL-EVAL-COST DELIMITED BY SIZE
               "  RETRAIN: $" DELIMITED BY SIZE
               WS-RPT-TOTAL-RETRAIN-COST DELIMITED BY SIZE
               INTO VOC-REPORT-LINE
           WRITE VOC-REPORT-LINE

           CLOSE VOC-REPORT-FILE.

       9000-TERMINATE.
           CLOSE VOC-CASE-FILE
           CLOSE VOC-JOB-FILE
           STOP RUN.
