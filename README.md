# Sample TPA Mainframe Codebase

This repository contains sample HPE NonStop mainframe source code representing a typical Third-Party Administrator (TPA) claims processing system.

## Structure

```
talsrc/          TAL (Transaction Application Language) source
  CLMPROC.tal      Claims Processing Server
  POLMAINT.tal     Policy Maintenance Server
  PMTPROC.tal      Payment Processing Server

cobolsrc/        COBOL source
  CLMENTRY.cbl     Claims Entry and Validation
  ELGCHECK.cbl     Eligibility Verification

taclsrc/         TACL scripts
  DAILYRUN.tacl    Daily Batch Processing

scsrc/           SCREEN COBOL source
  CLMSCR01.scbl    Claims Inquiry Screen

cpy/             COBOL Copybooks
  CLMCOPY.cpy      Shared Claim Record Definitions
```

## System Overview

- **Claims Processing**: CLMENTRY validates and submits claims via PATHSEND to CLMPROC
- **Policy Management**: POLMAINT handles CRUD for policy master records in Enscribe
- **Eligibility**: ELGCHECK provides real-time member eligibility verification
- **Payments**: PMTPROC manages check runs, EFT, and payment reconciliation
- **Batch**: DAILYRUN orchestrates the nightly batch cycle
- **Inquiry**: CLMSCR01 provides terminal-based claim lookup for CSRs
