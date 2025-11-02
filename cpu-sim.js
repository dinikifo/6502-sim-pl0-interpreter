// Minimal scaffold for the 6502-like CPU + PL/0 bridge.
// Replace with your full implementation that matches manual.pdf.

class CounterMachine {
  constructor(numRegisters = 4, memorySize = 256) {
    this.registers = new Array(numRegisters).fill(0);
    this.memory = new Array(memorySize).fill(0);
    this.dataStack = [];
    this.ip = 0;
    this.instructions = [];
    this.labels = new Map(); // label -> index
    this.callStack = [];
    this.running = false;
  }

  addInstructions(list) {
    this.instructions.push(...list);
    this._indexLabels();
  }

  _indexLabels() {
    this.labels.clear();
    this.instructions.forEach((ins, idx) => {
      if (typeof ins === 'string' && ins.endsWith(':')) {
        const label = ins.slice(0, -1).trim();
        if (label) this.labels.set(label, idx + 1); // next instruction
      }
    });
  }

  reset() {
    this.ip = 0;
    this.dataStack.length = 0;
    this.callStack.length = 0;
    this.running = false;
  }

  // Basic fetch/decode/execute loop for a tiny subset; extend per manual.pdf
  execute(maxSteps = 100000) {
    this.running = true;
    let steps = 0;
    while (this.running && this.ip < this.instructions.length) {
      if (steps++ > maxSteps) throw new Error('Step limit exceeded (possible infinite loop).');

      const raw = this.instructions[this.ip];
      // Skip labels
      if (typeof raw === 'string' && raw.endsWith(':')) {
        this.ip++;
        continue;
      }

      const ins = typeof raw === 'string' ? raw.trim() : raw;
      if (typeof ins !== 'string') throw new Error(`Instruction must be string at ${this.ip}`);

      const [op, ...args] = ins.split(/\s+/);
      switch (op.toUpperCase()) {
        case 'HALT':
          this.running = false;
          this.ip++;
          break;

        case 'PUSH': {
          // PUSH <number>
          const v = Number(args[0]);
          if (!Number.isFinite(v)) throw new Error(`PUSH expects number, got ${args[0]}`);
          this.dataStack.push(v | 0);
          this.ip++;
          break;
        }

        case 'POP': {
          // POP -> discards top or POP rX (not implemented in scaffold)
          if (this.dataStack.length === 0) throw new Error('POP on empty stack');
          this.dataStack.pop();
          this.ip++;
          break;
        }

        case 'JMP': {
          const target = args[0];
          if (this.labels.has(target)) {
            this.ip = this.labels.get(target);
          } else {
            const n = Number(target);
            if (!Number.isInteger(n)) throw new Error(`Unknown label or offset: ${target}`);
            this.ip = n;
          }
          break;
        }

        case 'CALL': {
          const target = args[0];
          this.callStack.push(this.ip + 1);
          if (this.labels.has(target)) {
            this.ip = this.labels.get(target);
          } else {
            throw new Error(`Unknown label: ${target}`);
          }
          break;
        }

        case 'RET': {
          if (this.callStack.length === 0) {
            this.running = false;
            this.ip++;
          } else {
            this.ip = this.callStack.pop();
          }
          break;
        }

        // Stub for PL0CALL bridge.
        case 'PL0CALL': {
          const name = args[0];
          if (!PL0Programs[name]) {
            throw new Error(`PL0 program not found: ${name}`);
          }
          // Save current ip, run program, restore ip (naive sequencing)
          const returnIp = this.ip + 1;
          this._runInline(PL0Programs[name]);
          this.ip = returnIp;
          break;
        }

        default:
          throw new Error(`Unknown opcode in scaffold: ${op}`);
      }
    }
  }

  _runInline(instructionList) {
    // Very small nested executor to support PL0CALL in the scaffold.
    const saved = {
      registers: this.registers.slice(),
      memory: this.memory.slice(),
      dataStack: this.dataStack,
    };
    try {
      const nested = new CounterMachine(this.registers.length, this.memory.length);
      nested.registers = saved.registers.slice();
      nested.memory = saved.memory.slice();
      nested.dataStack = saved.dataStack; // share stack to pass values
      nested.addInstructions(instructionList);
      nested.execute();
      // copy back memory/registers if desired
      this.registers = nested.registers.slice();
      this.memory = nested.memory.slice();
      this.dataStack = nested.dataStack;
    } catch (e) {
      throw e;
    }
  }
}

// ----- PL/0 compiler placeholders -----
// Replace with your tokenizer/parser/codegen per manual.pdf.
const PL0Programs = Object.create(null);

/**
 * Compile PL/0 source into VM instructions and store under a program name.
 * @param {string} source - PL/0 source text
 * @param {number} [baseAddr] - optional base for variable layout
 * @param {string} [name] - optional program name; if omitted, the first 'program <name>' is used
 * @returns {string[]} the compiled instruction list
 */
function compilePL0(source, baseAddr = 0, name = 'main') {
  // This is intentionally a stub. You should:
  //   1) tokenize, 2) parse (AST), 3) generate VM instructions, 4) store in PL0Programs[name].
  // For now, we build a trivial program that immediately HALTs.
  const instructions = [
    // Your codegen would go here
    'HALT',
  ];
  PL0Programs[name] = instructions;
  return instructions;
}

module.exports = { CounterMachine, compilePL0, PL0Programs };
