# bltzread handoff (2026-10-26)

Module: `~/.config/emacs/smdi-emacs-modules/bltzread/`. Tests: `emacs --batch -L . -l bltzread-tests.el -f ert-run-tests-batch-and-exit` (29 pass; bltzr is real, sdcv stubbed).

## Done since the first interactive test
- Items 1-6 of the old handoff (playground colour, popup delay, close popup, undo, ignore toggle, entry cycling) are now covered by ERT tests.
- Numerals never marked (`bltzread-skip-numerals`); `{`/`}` take counts; level applies to the whole word (`bltzread-level-scope`),
  reads count per form; lookup counting (`bltzread-lookup-counts` command|interaction|popup, `bltzread-lookup-dwell`, once per word per session);
  popup above the word (`bltzread-popup-position`, `bltzread-popup-gap`), hides on buffer/window/focus change and after a level change;
  dictionary names resolve forgivingly, unlisted languages search nothing, `M-x bltzread-dict-diagnose`;
  DPD fix = re-sorted index copy via `tools/sdcv-fix-index.py` + `bltzread-sdcv-data-dir-alist`.
- Whole-word level lives under `"<lemma>:BASIC"`-style `lemma:` keys in the same words table; state file version unchanged (v1 loads).
- Manuals rebuilt (`bltzread.info`, `bltzread-tutorial.info`).

## Docs updated (2026-10-26)
- Keys are now single keys; the prefix defaults to `s` (was `C-c l`). Documented in `bltzread.org`, the tutorial and `../subtle/subtle-customizing.org`.
- Popup is always above the word (`bltzread-popup-only-on-motion`, `bltzread-popup-scroll-hook`); prefetch removed; `bltzread-known-write-policy` default `unset`; `bltzread-dict-filters`; `bltzread-avy-unknown`; `bltzread-range-farthest-source` as function, `bltzread-range-start-function`, `bltzread-offer-record`.
- New "Using bltzread with subtle" sections (settings and snippet, no bridge package). Manuals rebuilt.
- Not checked in code: `q` closing a bltzread card first needs bltzread wired into `subtle-dismiss-functions`; bltzread does not do it itself.

## Renamed
The project was renamed from `lq` to `bltzread`: files, symbols (`bltzread-...`), docs, `.info` manuals and the folder.
`bltzread-state-directory` still falls back to the old `lq/` folder when `bltzread/` does not exist, so progress is kept.
init.el / init.org must load `bltzread` from `~/.config/emacs/smdi-emacs-modules/bltzread` (see the use-package block).

## Open
- nov.el key clash check (`d D u c i l N s`), interactive checklist, language auto-detect (bltzr-coverage idea),
  optional generic StarDict reader (no index fixing), bltzr levels proposal (`bltzr-levels-proposal.md`).
