# Issue Templates — college-rpp

This folder contains issue templates for the recurring procedures in
**Section 7 (Procedure: Updating the Postsecondary Dataset)** of the
Postsecondary Pathways Technical Guide. Each template mirrors the
checklist in its corresponding guide section, so an issue = one full
pass through that procedure for one school site / pull cycle.

Not a GitHub-rendered file — this is just for maintainers browsing the repo.

## Templates

| File | Guide Section | Phase | Frequency |
|---|---|---|---|
| `7.1_submitting_graduate_files.yml` | 7.1 | Submitting Graduate Files | June, per site |
| `7.2_update_master_student_list.yml` | 7.2 | Update Master Student List | June–August, per site |
| `7.4_data_retrieval.yml` | 7.4 | Data Retrieval | August, November, April |
| `7.5_data_prep_merging.yml` | 7.5 | Data Preparation & Merging | Each pull, per site |
| `7.6_missing_data_followup.yml` | 7.6 | Missing Data & Follow-Up | November only; stays open Dec–May |

**Not templated:**
- **7.3 (Add Postsecondary Plans to PSD)** — marked "under development" in
  the guide. Add a template once the procedure is finalized.
- **7.7 (Timing Note)** and **7.8 (Staff Contacts)** — reference sections,
  not actionable procedures.

## Conventions

- **Labels:** every template applies `data-merge` plus its section number
  (e.g. `7.5`) so issues can be filtered by phase. `7.6` also gets
  `november-only`.
- **Effective Date dropdown:** used in 7.4 and 7.5, where the same
  procedure runs across August/November/April. Keep the option list
  identical across templates (`August`, `November`, `April`) so filtering
  works consistently.
- **Checklist wording:** checkbox labels are paraphrased from the guide's
  own bullet points, in the same order, so the issue can be worked
  side-by-side with the guide.
- **Issues Found / Notes field:** not always explicit in the guide, but
  added to every template as a place to log discrepancies without
  cluttering the checklist itself.

## Maintenance

If the Technical Guide changes (new steps, renumbered sections, updated
NSC file naming conventions, etc.), update the corresponding template to
match. If the guide's section numbering changes, rename the template
file to match — the leading number is what keeps them sorted correctly
in the "New issue" picker.
