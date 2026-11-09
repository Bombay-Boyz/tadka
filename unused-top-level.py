#!/usr/bin/env python3
"""Rough dead-code finder for this repository. Run from the repository root:

    python3 tools/unused-top-level.py

For every top-level function with a type signature in src/, it counts uses,
ignoring comments, strings, and export/import lists, and reports functions that
are (a) never used anywhere, or (b) used only by tests, never by library code.
A function exported from the `Tadka` module is marked public: it may be unused
inside the project on purpose, but an unused public function is also one that no
test exercises, which is worth knowing.

It is a text scan, not a compiler: it can miss dead code (a name that is merely
mentioned elsewhere) and does not look at types, constructors, record fields,
class methods or instances. For a precise answer use `weeder` or HPC coverage;
see the notes in the answer that came with this script.
"""
import glob
import re


def strip(s):
    s = re.sub(r"\{-.*?-\}", "", s, flags=re.S)
    s = re.sub(r"--.*", "", s)
    return re.sub(r'"(\\.|[^"\\])*"', '""', s)


def drop_lists(s):
    out = []
    for l in s.split("\n"):
        t = l.strip()
        if re.fullmatch(r"[,(]?\s*[A-Za-z_][\w']*(\s*\(\.\.\))?\s*,?", t) and l.startswith(" "):
            continue
        out.append(re.sub(r"\(.*", "", t) if t.startswith("import ") else l)
    return "\n".join(out)


src = [f for f in glob.glob("src/**/*.hs", recursive=True) if "Width/Table" not in f]
other = glob.glob("interop/**/*.hs", recursive=True) + glob.glob("tadka-highlight/**/*.hs", recursive=True)
tests = glob.glob("test/**/*.hs", recursive=True)
code = {f: drop_lists(strip(open(f).read())) for f in src + other + tests}

tadka = strip(open("src/Tadka.hs").read())
public = set(re.findall(r"[A-Za-z_][\w']*", tadka[tadka.index("module Tadka"):tadka.index(") where")]))


def uses(name, files, own=None):
    n = 0
    for f in files:
        n += len(re.findall(r"(?<![\w'.])" + re.escape(name) + r"(?![\w'])", code[f]))
        if f == own:
            n -= len(re.findall(r"^" + re.escape(name) + r"\b", code[f], re.M))
    return n


never, test_only = [], []
for f in src:
    for m in re.finditer(r"^([a-z_][\w']*)\s*::", code[f], re.M):
        name = m.group(1)
        in_lib = uses(name, src, own=f) + uses(name, other)
        in_tests = uses(name, tests)
        if in_lib == 0 and in_tests == 0:
            never.append((f, name))
        elif in_lib == 0:
            test_only.append((f, name))


def show(title, items):
    print(f"\n{title}: {len(items)}")
    for f, n in sorted(items):
        print(f"  {f}: {n}" + ("   [public]" if n in public else ""))


show("Never used anywhere (library, interop, tests)", never)
show("Used only by tests, never by library code", test_only)
