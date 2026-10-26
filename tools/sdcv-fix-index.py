#!/usr/bin/env python3
"""Write a copy of a StarDict dictionary whose .idx and .syn are sorted the way sdcv expects.

sdcv binary-searches the index (and the .syn file) using stardict_strcmp: ASCII
case-insensitive comparison first, plain strcmp as the tie-break.  Some exports
(for example the Digital Pali Dictionary) are not in that order, so exact lookups miss.

Usage: sdcv-fix-index.py SRC_DIR OUT_DIR
SRC_DIR holds NAME.ifo; OUT_DIR gets the fixed .ifo/.idx/.syn and a symlink to the
(untouched) .dict.dz.  Nothing in SRC_DIR is modified.
"""
import functools, glob, os, struct, sys, gzip


def ascii_fold(b):
    return bytes(c + 32 if 65 <= c <= 90 else c for c in b)


def cmp(a, b):
    x, y = ascii_fold(a), ascii_fold(b)
    return (x > y) - (x < y) or (a > b) - (a < b)


KEY = functools.cmp_to_key(lambda a, b: cmp(a[0], b[0]))


def read_idx(path, off64):
    data = open(path, "rb").read()
    n = 12 if off64 else 8
    out, i = [], 0
    while i < len(data):
        j = data.index(b"\0", i)
        out.append((data[i:j], data[j + 1:j + 1 + n]))
        i = j + 1 + n
    return out


def main(src, out):
    ifo = glob.glob(os.path.join(src, "*.ifo"))[0]
    base = os.path.splitext(ifo)[0]
    name = os.path.basename(base)
    info = dict(l.rstrip("\n").split("=", 1) for l in open(ifo, encoding="utf-8") if "=" in l)
    off64 = info.get("idxoffsetbits") == "64"
    os.makedirs(out, exist_ok=True)
    entries = read_idx(base + ".idx", off64)
    order = sorted(range(len(entries)), key=lambda i: KEY(entries[i]))
    new_index = {old: new for new, old in enumerate(order)}
    with open(os.path.join(out, name + ".idx"), "wb") as f:
        for old in order:
            w, rest = entries[old]
            f.write(w + b"\0" + rest)
    for ext in (".syn", ".syn.dz"):
        if os.path.exists(base + ext):
            raw = gzip.open(base + ext).read() if ext.endswith(".dz") else open(base + ext, "rb").read()
            syns, i = [], 0
            while i < len(raw):
                j = raw.index(b"\0", i)
                syns.append((raw[i:j], new_index[struct.unpack(">I", raw[j + 1:j + 5])[0]]))
                i = j + 5
            syns.sort(key=lambda s: KEY((s[0],)))
            with open(os.path.join(out, name + ".syn"), "wb") as f:
                for w, idx in syns:
                    f.write(w + b"\0" + struct.pack(">I", idx))
            break
    for f in os.listdir(src):
        if f.endswith(".ifo"):
            open(os.path.join(out, f), "w", encoding="utf-8").write(open(os.path.join(src, f), encoding="utf-8").read())
        elif f.startswith(name + ".dict"):
            dst = os.path.join(out, f)
            if not os.path.lexists(dst):
                os.symlink(os.path.join(src, f), dst)
    print("wrote", out, "entries", len(entries))


if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    main(*sys.argv[1:])
