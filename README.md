# Workers' Compensation TPA Mainframe System

Production HPE NonStop mainframe codebase for a Third-Party Administrator (TPA) managing workers' compensation claims. ~31,000 lines of TAL, COBOL, TACL, and SCREEN COBOL.

## System Architecture

```
                    ┌─────────────┐
                    │  Terminals   │  (6500/T16 via Pathway)
                    └──────┬──────┘
                           │
                    ┌──────┴──────┐
                    │  SCREEN     │  FNOLSCR, CLMSCR01, CLMSUM,
                    │  COBOL      │  RSVSCR, PMTSCR, DIARYSCR,
                    │  Screens    │  WCBENSCR, MEDRSCR
                    └──────┬──────┘
                           │ PATHSEND
          ┌────────────────┼────────────────┐
          │                │                │
    ┌─────┴─────┐   ┌─────┴─────┐   ┌─────┴─────┐
    │  Claims    │   │  Payment  │   │  Policy   │
    │  Servers   │   │  Servers  │   │  Servers  │
    └─────┬─────┘   └─────┬─────┘   └───────────┘
          │                │
    ┌─────┴─────────┬──────┴──────┐
    │  Enscribe     │  TMF        │
    │  Files        │  Protected  │
    └───────────────┴─────────────┘
```

## Directory Structure

```
talsrc/          TAL (Transaction Application Language) — 20 files
cobolsrc/        COBOL — 21 files
taclsrc/         TACL batch scripts — 4 files
scsrc/           SCREEN COBOL terminal screens — 8 files
cpy/             COBOL copybooks — 6 files
```

## Module Index

### FNOL (First Notice of Loss)
| File | Language | Lines | Description |
|------|----------|-------|-------------|
| FNOLPROC.tal | TAL | 535 | FNOL processing server — intake, classification, auto-assignment |
| FNOLVAL.cbl | COBOL | 587 | FNOL validation — employer, employee, injury, jurisdiction |
| FNOLSCR.scbl | SCOBOL | 584 | Multi-page FNOL data entry screen |

### Claim Setup & Management
| File | Language | Lines | Description |
|------|----------|-------|-------------|
| CLMSETUP.tal | TAL | 588 | Claim creation from validated FNOL, claim number generation |
| CLMASGN.cbl | COBOL | 455 | Adjuster assignment — workload balancing, team routing |
| CLMSTAT.cbl | COBOL | 473 | Status management — transitions, closure validation, reopening |
| CLMDIARY.tal | TAL | 612 | Diary/notes management — due dates, escalation, queues |
| CLMSCR01.scbl | SCOBOL | 231 | Claims inquiry screen |
| CLMSUM.scbl | SCOBOL | 505 | Claim summary screen — financials, status, diary |
| DIARYSCR.scbl | SCOBOL | 500 | Diary management screen — adjuster workload, filtering |

### Claims Processing
| File | Language | Lines | Description |
|------|----------|-------|-------------|
| CLMPROC.tal | TAL | 185 | Claims adjudication server — validation, fee schedule, auto-adjudicate |
| CLMENTRY.cbl | COBOL | 198 | Claims entry and validation program |
| ELGCHECK.cbl | COBOL | 216 | Eligibility verification against policy master |

### Workers' Compensation — Compensability & Benefits
| File | Language | Lines | Description |
|------|----------|-------|-------------|
| WCCOMP.tal | TAL | 754 | Compensability determination — AOE/COE, exclusions, apportionment |
| WCBENCALC.tal | TAL | 715 | Benefits calculation — TTD/TPD/PPD/PTD rates, waiting periods, COLA |
| WCJURIS.tal | TAL | 644 | Jurisdiction rules engine — 50 states + DC + federal |
| WCWAGE.tal | TAL | 684 | AWW calculation — wage history, concurrent employment, seasonal |
| WCPPDRATE.tal | TAL | 714 | PPD rating calculator — AMA Guides, scheduled/unscheduled, combining |
| WCEXMOD.tal | TAL | 616 | Experience modification factor calculator |
| WCBENSCR.scbl | SCOBOL | 523 | Benefits calculation screen |

### Reserves
| File | Language | Lines | Description |
|------|----------|-------|-------------|
| RSVRCALC.tal | TAL | 591 | Reserve calculation — medical, indemnity, expense, IBNR, staircase |
| RSVRAPPR.cbl | COBOL | 625 | Reserve approval workflow — authority levels, routing |
| RSVRHIST.tal | TAL | 633 | Reserve history — transactions, triangles, snapshots |
| RSVSCR.scbl | SCOBOL | 583 | Reserve entry/inquiry screen |

### Payment Processing
| File | Language | Lines | Description |
|------|----------|-------|-------------|
| PMTPROC.tal | TAL | 282 | Payment server — check runs, EFT, reconciliation |
| PMTAPPR.tal | TAL | 702 | Payment approval — authority, duplicate detection, lien checking |
| PMTEFT.tal | TAL | 753 | EFT/ACH processing — NACHA format, routing validation, batch |
| PMTCHECK.cbl | COBOL | 548 | Check printing — MICR, positive pay, register |
| PMTVOID.cbl | COBOL | 683 | Void/reissue — stale check, escheatment, stop payment |
| PMT1099.cbl | COBOL | 624 | 1099 processing — accumulation, IRS filing, TIN matching |
| PMTSCR.scbl | SCOBOL | 601 | Payment entry screen |

### Medical Management
| File | Language | Lines | Description |
|------|----------|-------|-------------|
| MEDBILL.tal | TAL | 658 | Medical bill repricing — CPT, modifiers, fee schedule, network |
| WCMEDFEE.cbl | COBOL | 580 | Fee schedule lookup — state-specific, Medicare-based, DRG |
| WCUTIL.cbl | COBOL | 692 | Utilization review — pre-cert, ODG guidelines, peer review |
| WCNCM.cbl | COBOL | 660 | Nurse case management — treatment plans, provider coordination |
| WCPHARM.tal | TAL | 745 | Pharmacy benefits — DUR, formulary, NDC, controlled substance |
| WCRTW.cbl | COBOL | 692 | Return to work tracking — modified duty, FCE, milestones |
| MEDRSCR.scbl | SCOBOL | 542 | Medical bill review screen |

### Legal & Litigation
| File | Language | Lines | Description |
|------|----------|-------|-------------|
| WCLEGAL.tal | TAL | 657 | Litigation management — defense counsel, hearings, settlement |
| WCSUBRO.cbl | COBOL | 492 | Subrogation — lien calculation, recovery, allocation |

### Compliance & Reporting
| File | Language | Lines | Description |
|------|----------|-------|-------------|
| WCEDI837.cbl | COBOL | 817 | EDI 837 medical claim parsing (HIPAA X12) |
| WCEDI835.cbl | COBOL | 627 | EDI 835 remittance advice generation |
| WCSTATE.cbl | COBOL | 564 | State filing — FROI/SROI, IAIABC Claims EDI |
| WCMSA.cbl | COBOL | 568 | Medicare Set-Aside — allocation, CMS submission, Section 111 |
| WCSIU.tal | TAL | 666 | SIU fraud detection — red flag scoring, pattern analysis |
| WCLOSSRUN.cbl | COBOL | 718 | Loss run report generator — policy-level, trending |
| WCACTEXT.cbl | COBOL | 664 | Actuarial data extract — triangles, exposure, statistical plan |
| WCVOCR.cbl | COBOL | 693 | Vocational rehabilitation — skills analysis, retraining, placement |

### Policy Management
| File | Language | Lines | Description |
|------|----------|-------|-------------|
| POLMAINT.tal | TAL | 241 | Policy CRUD — Enscribe key-sequenced, eligibility, deductibles |

### Batch Processing
| File | Language | Lines | Description |
|------|----------|-------|-------------|
| DAILYRUN.tacl | TACL | 169 | Daily batch — adjudication, EOB, premiums, accumulators |
| WCBATCH.tacl | TACL | 359 | Monthly WC batch — reserves, actuarial, state filing |
| EDIBATCH.tacl | TACL | 357 | EDI batch — 837/835, clearinghouse, acknowledgments |
| RPTBATCH.tacl | TACL | 427 | Report batch — loss runs, aging, compliance, large loss |

### Copybooks (Shared Record Definitions)
| File | Lines | Description |
|------|-------|-------------|
| CLMCOPY.cpy | 64 | General claim and provider records |
| WCCLMCPY.cpy | 249 | WC claim master — body part, nature, cause codes |
| WCRSVCPY.cpy | 234 | Reserve records — categories, transactions, authority |
| WCPMTCPY.cpy | 266 | Payment records — types, methods, status, 1099, liens |
| WCEMPCPY.cpy | 295 | Employer/policy records — FEIN, exp mod, locations, wages |
| WCPRVCPY.cpy | 216 | Provider records — NPI, network, fee schedule, 1099 |

## Technology Stack

- **Platform:** HPE NonStop (Tandem), Guardian OS
- **Languages:** TAL, COBOL, SCREEN COBOL, TACL
- **Database:** Enscribe (key-sequenced and relative files)
- **Transactions:** TMF (Transaction Management Facility)
- **IPC:** PATHSEND / Pathway TS/MP
- **Terminal:** 6520/T16 via Pathway server classes

## Domain Standards

- **NCCI** — National Council on Compensation Insurance (class codes, injury codes)
- **IAIABC** — International Association of Industrial Accident Boards and Commissions (Claims EDI)
- **HIPAA X12** — EDI 837/835 for medical claims and remittance
- **AMA Guides** — Impairment rating (editions 4, 5, 6 by jurisdiction)
- **NACHA** — ACH/EFT payment file format
- **IRS Pub 1220** — Electronic 1099 filing format
