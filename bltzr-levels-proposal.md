# Should bltzr itself learn word levels?

Status: assessment written while building `bltzread`; not yet discussed with the user.
Everything in `bltzread` is built against bltzr as it is today (man page dated
2026-10-09). Nothing here is required for `bltzread` to work.

## Recommendation

Yes, but a small version. Do three things in bltzr and leave the rest in `bltzread`.

1. **Per-occurrence offsets in JSON** (`--offsets`). Each occurrence reports
   buffer-relative start/end, the form as written, and the lemma candidate(s).
2. **A graded vocabulary file** replacing the binary known list: one term per
   line with a level 1-5 (and an "ignored" marker), plus commands to read and
   change it (`bltzr levels CODE`, `bltzr level set CODE TERM N`), and
   `--skip-level N` meaning "drop words at level N or above". Plain skip
   files keep working and mean level 5.
3. **A form-to-lemma command** (`bltzr lemmas CODE`): words on stdin, TSV
   `form<TAB>lemma[,lemma...]` on stdout.

Leave the promotion policy (clean reads, thresholds, lookup effects) in the
client. That is reading-UI behavior; other front ends will want different
rules. bltzr only needs to *store and honor* levels.

Why it is worth it: levels would also help plain CLI users (extract only
level 1-3 words for study cards, skip level 4-5), and it removes the two
worst hacks in `bltzread`.

## What `bltzread` does today, and what would go away

| Today (built here) | With bltzr levels + offsets |
|---|---|
| `bltzread-bltzr.el`: Elisp tokenizer plus a multi-pass "one word per line" run to learn form to lemma, working around the 20-example cap | Delete. One `bltzr blitz --offsets -o json` call returns every occurrence with form and lemma. |
| `bltzread-word-regexp`, `bltzread-word-regexp-alist` | Delete. bltzr's own word boundaries are used, so no mismatch for languages with hard segmentation. |
| `bltzread-lists.el`: parse bltzr.toml for skip paths, read word files, append with `.bak` | Delete. `bltzr level set` owns the file and backups. |
| `bltzread-review.el` queue and `bltzread-known-write-policy` | Keep, but only as an optional "confirm before bltzr is told" step; writing becomes one command. |
| State file with per-word `:level` | State keeps only `:reads`, `:note`, and history. Levels live in bltzr's file, so the CLI and Emacs always agree. |
| `bltzread-track-by`, `bltzread-list-target` | `bltzread-list-target` disappears (levels are per term; family vs. exact becomes a bltzr concern). `bltzread-track-by` stays. |
| Lookup cache of lemma maps | Unneeded. |

Net effect: roughly a third of the non-UI code disappears, and the biggest
correctness risk (our tokenizer disagreeing with bltzr's) goes away.

## Costs and open questions

- The man page says bltzr "does not choose candidates based on sentence
  meaning"; with offsets, an occurrence of *saw* carries two lemma
  candidates. The client still has to pick or show both.
- Level semantics must interoperate with the existing exact-word and
  word-family files (whose meaning is spelling vs. family, not level). Needs
  a clear migration story; the man page already has a MIGRATING section to
  imitate.
- A levels file needs the same atomic write and `.bak` discipline the skip
  lists have.
- Source lives on the unmounted `Iddhibiddhi` disk, so this was assessed from
  the man page and observed JSON only.

## Migration plan for `bltzread` if this ships

1. Add `bltzread-backend` option: `legacy` (today) or `levels`.
2. Implement the `levels` backend (small: one process call, parse offsets).
3. Flip the default; later remove `legacy` and the files listed above.
