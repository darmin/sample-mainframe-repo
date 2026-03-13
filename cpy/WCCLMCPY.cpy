      ******************************************************************
      * COPYBOOK:  WCCLMCPY
      * PURPOSE:   Workers' Compensation Claim Master Record Layout
      * SYSTEM:    Claims Management System (CMS)
      * PLATFORM:  HPE NonStop / Enscribe Key-Sequenced File
      * FILE:      $DATA1.CLMDB.CLMMASTR
      * KEY:       WC-CLAIM-NUMBER (primary), alternate key on SSN
      * AUTHOR:    TPA Systems Development
      * CREATED:   2023-11-15
      * MODIFIED:  2025-02-10 - Added NCCI 2024 body part codes
      *
      * Record length: 1024 bytes (fixed)
      * This is the core claim record for all WC claims. One record
      * per claim. Related records in CLMRSV, CLMPMT, CLMDOC files.
      ******************************************************************
      *
       01  WC-CLAIM-MASTER-RECORD.
      *
      *--- Claim Identification
      *
           05  WC-CLAIM-NUMBER             PIC X(12).
           05  WC-CLAIM-SUFFIX             PIC X(2).
           05  WC-TPA-CLIENT-CODE          PIC X(6).
           05  WC-POLICY-NUMBER            PIC X(15).
           05  WC-OCCURRENCE-NUMBER        PIC 9(4).
      *
      *--- Claimant Information
      *
           05  WC-CLAIMANT-INFO.
               10  WC-CLMT-SSN            PIC X(9).
               10  WC-CLMT-LAST-NAME      PIC X(25).
               10  WC-CLMT-FIRST-NAME     PIC X(15).
               10  WC-CLMT-MIDDLE-INIT    PIC X(1).
               10  WC-CLMT-DOB            PIC X(8).
               10  WC-CLMT-GENDER         PIC X(1).
                   88  WC-CLMT-MALE           VALUE "M".
                   88  WC-CLMT-FEMALE         VALUE "F".
                   88  WC-CLMT-UNKNOWN-GEN    VALUE "U".
               10  WC-CLMT-MARITAL-STATUS PIC X(1).
                   88  WC-CLMT-SINGLE         VALUE "S".
                   88  WC-CLMT-MARRIED        VALUE "M".
                   88  WC-CLMT-DIVORCED       VALUE "D".
                   88  WC-CLMT-WIDOWED        VALUE "W".
               10  WC-CLMT-DEPENDENTS     PIC 99.
               10  WC-CLMT-ADDRESS.
                   15  WC-CLMT-ADDR-LINE1 PIC X(30).
                   15  WC-CLMT-ADDR-LINE2 PIC X(30).
                   15  WC-CLMT-CITY       PIC X(20).
                   15  WC-CLMT-STATE      PIC X(2).
                   15  WC-CLMT-ZIP        PIC X(10).
               10  WC-CLMT-PHONE-HOME     PIC X(10).
               10  WC-CLMT-PHONE-CELL     PIC X(10).
               10  WC-CLMT-EMAIL          PIC X(40).
      *
      *--- Employer Information (at time of injury)
      *
           05  WC-EMPLOYER-INFO.
               10  WC-EMPR-FEIN           PIC X(9).
               10  WC-EMPR-NAME           PIC X(35).
               10  WC-EMPR-LOCATION-CODE  PIC X(6).
               10  WC-EMPR-SIC-CODE       PIC X(4).
               10  WC-EMPR-NAICS-CODE     PIC X(6).
               10  WC-EMPR-CONTACT-NAME   PIC X(25).
               10  WC-EMPR-CONTACT-PHONE  PIC X(10).
      *
      *--- Injury Details
      *
           05  WC-INJURY-DETAIL.
               10  WC-DATE-OF-INJURY      PIC X(8).
               10  WC-TIME-OF-INJURY      PIC X(4).
               10  WC-DATE-REPORTED-EMPR  PIC X(8).
               10  WC-DATE-REPORTED-TPA   PIC X(8).
               10  WC-INJURY-DESCRIPTION  PIC X(80).
               10  WC-ACCIDENT-LOCATION   PIC X(40).
               10  WC-ACCIDENT-STATE      PIC X(2).
      *
      *---     Body Part Codes (NCCI Standard)
      *
               10  WC-BODY-PART-PRIMARY   PIC X(2).
                   88  WC-BP-HEAD             VALUE "10".
                   88  WC-BP-NECK             VALUE "20".
                   88  WC-BP-UPPER-BACK       VALUE "30".
                   88  WC-BP-LOWER-BACK       VALUE "31".
                   88  WC-BP-SHOULDER         VALUE "32".
                   88  WC-BP-UPPER-ARM        VALUE "33".
                   88  WC-BP-ELBOW            VALUE "34".
                   88  WC-BP-FOREARM          VALUE "35".
                   88  WC-BP-WRIST            VALUE "36".
                   88  WC-BP-HAND             VALUE "37".
                   88  WC-BP-FINGER           VALUE "38".
                   88  WC-BP-THUMB            VALUE "39".
                   88  WC-BP-HIP              VALUE "40".
                   88  WC-BP-UPPER-LEG        VALUE "41".
                   88  WC-BP-KNEE             VALUE "42".
                   88  WC-BP-LOWER-LEG        VALUE "43".
                   88  WC-BP-ANKLE            VALUE "44".
                   88  WC-BP-FOOT             VALUE "45".
                   88  WC-BP-TOE              VALUE "46".
                   88  WC-BP-CHEST            VALUE "50".
                   88  WC-BP-ABDOMEN          VALUE "52".
                   88  WC-BP-LUNGS            VALUE "55".
                   88  WC-BP-HEART            VALUE "56".
                   88  WC-BP-MULTIPLE         VALUE "90".
                   88  WC-BP-NO-PHYSICAL      VALUE "91".
               10  WC-BODY-PART-SECONDARY PIC X(2).
               10  WC-BODY-SIDE           PIC X(1).
                   88  WC-SIDE-LEFT           VALUE "L".
                   88  WC-SIDE-RIGHT          VALUE "R".
                   88  WC-SIDE-BILATERAL      VALUE "B".
                   88  WC-SIDE-NOT-APPLICABLE VALUE "N".
      *
      *---     Nature of Injury Codes (NCCI Standard)
      *
               10  WC-NATURE-OF-INJURY    PIC X(2).
                   88  WC-NOI-STRAIN          VALUE "10".
                   88  WC-NOI-SPRAIN          VALUE "12".
                   88  WC-NOI-CONTUSION       VALUE "20".
                   88  WC-NOI-LACERATION      VALUE "30".
                   88  WC-NOI-FRACTURE        VALUE "40".
                   88  WC-NOI-DISLOCATION     VALUE "42".
                   88  WC-NOI-CRUSH           VALUE "50".
                   88  WC-NOI-AMPUTATION      VALUE "52".
                   88  WC-NOI-BURN            VALUE "60".
                   88  WC-NOI-CARPAL-TUNNEL   VALUE "71".
                   88  WC-NOI-HERNIATED-DISC  VALUE "72".
                   88  WC-NOI-HEARING-LOSS    VALUE "73".
                   88  WC-NOI-CONCUSSION      VALUE "80".
                   88  WC-NOI-DERMATITIS      VALUE "82".
                   88  WC-NOI-MENTAL-STRESS   VALUE "84".
                   88  WC-NOI-INFECTION       VALUE "86".
                   88  WC-NOI-OCC-DISEASE     VALUE "90".
                   88  WC-NOI-DEATH           VALUE "99".
      *
      *---     Cause of Injury Codes (NCCI Standard)
      *
               10  WC-CAUSE-OF-INJURY     PIC X(2).
                   88  WC-COI-FALL-SAME-LEVEL VALUE "10".
                   88  WC-COI-FALL-DIFF-LEVEL VALUE "12".
                   88  WC-COI-STRUCK-BY       VALUE "20".
                   88  WC-COI-STRUCK-AGAINST  VALUE "25".
                   88  WC-COI-CAUGHT-IN       VALUE "30".
                   88  WC-COI-LIFTING         VALUE "40".
                   88  WC-COI-PUSHING-PULL    VALUE "42".
                   88  WC-COI-REPETITIVE-MOT  VALUE "44".
                   88  WC-COI-MOTOR-VEHICLE   VALUE "50".
                   88  WC-COI-SLIP-TRIP       VALUE "55".
                   88  WC-COI-CUT-PUNCTURE    VALUE "60".
                   88  WC-COI-BURN-SCALD      VALUE "65".
                   88  WC-COI-CUMULATIVE      VALUE "70".
                   88  WC-COI-EXPOSURE        VALUE "80".
                   88  WC-COI-OTHER           VALUE "99".
      *
      *--- Jurisdiction and Regulatory
      *
           05  WC-JURISDICTION-INFO.
               10  WC-JURISDICTION-STATE   PIC X(2).
               10  WC-FILING-JURIS-STATE   PIC X(2).
               10  WC-STATE-CASE-NUMBER    PIC X(15).
               10  WC-WCB-DISTRICT        PIC X(4).
               10  WC-WCB-HEARING-DATE    PIC X(8).
               10  WC-STATUTE-DATE        PIC X(8).
               10  WC-FROI-FILED-DATE     PIC X(8).
               10  WC-FROI-STATUS         PIC X(2).
                   88  WC-FROI-NOT-FILED      VALUE "NF".
                   88  WC-FROI-FILED          VALUE "FL".
                   88  WC-FROI-ACCEPTED       VALUE "AC".
                   88  WC-FROI-REJECTED       VALUE "RJ".
               10  WC-SROI-FILED-DATE     PIC X(8).
      *
      *--- Claim Type
      *
           05  WC-CLAIM-TYPE-CODE         PIC X(2).
               88  WC-TYPE-MEDICAL-ONLY       VALUE "MO".
               88  WC-TYPE-LOST-TIME          VALUE "LT".
               88  WC-TYPE-FATALITY           VALUE "FT".
               88  WC-TYPE-OCC-DISEASE        VALUE "OD".
               88  WC-TYPE-REOPENED           VALUE "RO".
      *
      *--- Claim Status
      *
           05  WC-CLAIM-STATUS            PIC X(2).
               88  WC-STATUS-OPEN             VALUE "OP".
               88  WC-STATUS-CLOSED           VALUE "CL".
               88  WC-STATUS-REOPENED         VALUE "RO".
               88  WC-STATUS-PENDING          VALUE "PD".
               88  WC-STATUS-LITIGATED        VALUE "LI".
               88  WC-STATUS-DENIED           VALUE "DN".
               88  WC-STATUS-VOID             VALUE "VD".
      *
      *--- Compensability
      *
           05  WC-COMPENSABILITY-INFO.
               10  WC-COMP-STATUS         PIC X(2).
                   88  WC-COMP-ACCEPTED       VALUE "AC".
                   88  WC-COMP-DENIED         VALUE "DN".
                   88  WC-COMP-PENDING        VALUE "PD".
                   88  WC-COMP-PARTIAL        VALUE "PT".
                   88  WC-COMP-DISPUTED       VALUE "DS".
               10  WC-COMP-DECISION-DATE  PIC X(8).
               10  WC-COMP-REASON-CODE    PIC X(4).
               10  WC-COMP-DENY-REASON    PIC X(60).
      *
      *--- Key Dates
      *
           05  WC-KEY-DATES.
               10  WC-DATE-CREATED        PIC X(8).
               10  WC-DATE-CLOSED         PIC X(8).
               10  WC-DATE-REOPENED       PIC X(8).
               10  WC-LAST-ACTIVITY-DATE  PIC X(8).
               10  WC-NEXT-DIARY-DATE     PIC X(8).
               10  WC-LAST-RESERVE-REVIEW PIC X(8).
               10  WC-MMI-DATE            PIC X(8).
               10  WC-RTW-DATE            PIC X(8).
               10  WC-LAST-DATE-WORKED    PIC X(8).
               10  WC-RETURN-TO-WORK-TYPE PIC X(1).
                   88  WC-RTW-FULL-DUTY       VALUE "F".
                   88  WC-RTW-LIGHT-DUTY      VALUE "L".
                   88  WC-RTW-MODIFIED-DUTY   VALUE "M".
                   88  WC-RTW-NOT-RETURNED    VALUE "N".
      *
      *--- Assignment
      *
           05  WC-ASSIGNMENT-INFO.
               10  WC-ADJUSTER-ID         PIC X(8).
               10  WC-ADJUSTER-NAME       PIC X(25).
               10  WC-SUPERVISOR-ID       PIC X(8).
               10  WC-BRANCH-OFFICE       PIC X(4).
               10  WC-TEAM-CODE           PIC X(4).
      *
      *--- Financial Summary (denormalized for performance)
      *
           05  WC-FINANCIAL-SUMMARY.
               10  WC-TOTAL-INCURRED     PIC S9(9)V99 COMP.
               10  WC-TOTAL-PAID         PIC S9(9)V99 COMP.
               10  WC-TOTAL-OUTSTANDING  PIC S9(9)V99 COMP.
               10  WC-TOTAL-RECOVERY     PIC S9(9)V99 COMP.
      *
      *--- Audit Trail
      *
           05  WC-AUDIT-INFO.
               10  WC-CREATED-BY         PIC X(8).
               10  WC-CREATED-TIMESTAMP  PIC X(26).
               10  WC-MODIFIED-BY        PIC X(8).
               10  WC-MODIFIED-TIMESTAMP PIC X(26).
               10  WC-RECORD-VERSION     PIC 9(6).
      *
      *--- Reserved / Future Use
      *
           05  WC-FILLER-AREA            PIC X(32).
