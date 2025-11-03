# 6502-like CPU Simulator + PL/0-like Language (Node.js)

A tiny, hackable virtual machine inspired by the 6502 and a PL/0-style language that compiles to the VM’s instruction set.  
The authoritative description of the ISA and language is in **manual.pdf**. This repository provides a Node.js implementation (or scaffold) and runnable examples.

> **Why?** A compact playground for toy compilers, virtual machines, and algorithm demos.

---

## Contents

- `manual.pdf` — Instruction set & language reference (authoritative spec).
- `cpu-sim.js` — Node.js standalone implementation for the cpu and pl0 language
- `c.transpiler.js` -Node.js standalone implementation for the cpu, pl0 language and transpiler from a c like laguage to pl0 language.

---

## Features

- **Simple VM** with registers, byte-addressed memory, a **data stack**, labels, subroutine calls/returns, and a clear fetch–decode–execute loop.
- **Instruction set** including data movement, stack ops, arithmetic, control flow, `CALL`/`RET`, a `PL0CALL` bridge, and `HALT`.
- **PL/0-like language**: variables, expressions, `if`, `while`, `begin…end`, and simple procedures; compiled into VM instructions.
- **Manual-first**: the PDF is the normative spec; the code is kept small and readable so you can extend both together.

---

## Quick Start

1. **Install Node.js** (v16+ recommended).
2. Clone the repo and run the demo:

npm cpu-sim.js
or
reference the implementation in the pdf.

You should see stack/memory prints after a short run.
Replace the scaffold with your full implementation when ready.

## Instruction Set (high-level)

See manual.pdf for exact semantics and examples. Typical ops include:

- **Data/Memory:** LOAD, STORE, PEEK, POKE
- **Stack:** PUSH, POP
- **ALU:** ADD, SUB, MUL, DIV (integer)
- **Control Flow:** JMP, JZ, JNZ
- **Subroutines:** CALL, RET
- **Bridge:** PL0CALL <programName>
- **System:** HALT

Labels are declared inline (e.g., loop:) and resolved before execution.

## PL/0 Subset (high-level)

- **Blocks & statements:** begin … end, assignments, if … then …, while … do …, call.
- **Expressions:** integer arithmetic with + - * /.
- **Memory/stack bridges:** helpers (e.g., peek, poke, push, pop) may be surfaced to map to VM ops where useful.
- **Compilation pipeline:** tokenize → parse (AST) → generate VM instruction list → store in PL0Programs[name].

## Developing & Extending

- **New opcodes:** Extend the switch(op) in the VM’s execute loop and document them in manual.pdf.
- **Language features:** Update tokenizer/parser/codegen accordingly; keep the PDF grammar/examples in sync.
- **Testing:** Add small focused .pl0 programs and VM snippets; assert final stack/memory states.

## AI-Assisted Creation & Provenance

Some parts of this repository were created or refined with the assistance of large language models (LLMs) at the author’s direction. The author reviewed and integrated the results.

The intent is to place this work as completely as possible into the public domain via CC0-1.0 (see License below).

If you believe any snippet inadvertently reproduces third-party copyrighted code in a way that conflicts with the license, please open an issue with details (file, lines, source link). We will promptly rewrite or remove the material.

Privacy note: Don’t paste sensitive or proprietary material into issues or pull requests; treat prompts/logs as public.

## License

This project is dedicated to the public domain under CC0-1.0.

SPDX-License-Identifier: CC0-1.0

See LICENSE
