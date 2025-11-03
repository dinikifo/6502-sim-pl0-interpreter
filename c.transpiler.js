class CounterMachine {
  constructor(numCounters, memorySize = 256, dataStackSize = 256) {
    this.numCounters = numCounters;
    this.counters = new Array(numCounters).fill(0);
    this.memory = new Array(memorySize).fill(0);
    this.instructions = [];
    this.pointer = 0;
    this.callStack = [];
    this.dataStack = [];               // New data stack for PUSH/POP
    this.dataStackMax = dataStackSize;
    this.running = false;
    this.labelMap = {};
  }

  addInstruction(instruction) {
    this.instructions.push(instruction);
  }

  addInstructions(instructionsArray) {
    instructionsArray.forEach(line => this.addInstruction(line));
  }

  execute() {
    // 1) Build label map for jumps/calls
    this.buildLabelMap();
    // 2) Initialize pointer and set running flag
    this.pointer = 0;
    this.running = true;
    // 3) Fetch-decode-execute loop
    while (this.running && this.pointer < this.instructions.length) {
      let line = this.instructions[this.pointer].trim();
      // Skip labels
      if (line.endsWith(':')) {
        this.pointer++;
        continue;
      }
      let parts = line.split(/\s+/);
      let op = parts[0];
      let args = parts.slice(1);
      switch (op) {
        case 'LOAD': {
          // Format: LOAD rX, #imm  or  LOAD rX, [addr]
          let [regPart, valPart] = args;
          if (regPart.endsWith(',')) {
            regPart = regPart.slice(0, -1);
          }
          let regIndex = parseInt(regPart.replace('r', ''), 10);
          if (valPart.startsWith('#')) {
            // Immediate value
            let immediate = parseInt(valPart.slice(1), 10);
            this.counters[regIndex] = immediate;
          } else if (valPart.startsWith('[')) {
            // Memory read
            let addrStr = valPart.slice(1, -1);
            let addr;
            if (addrStr.toLowerCase().startsWith('r')) {
              // Indirect addressing: address is in register
              let addrRegIndex = parseInt(addrStr.replace('r', ''), 10);
              addr = this.counters[addrRegIndex];
            } else {
              addr = parseInt(addrStr, 10);
            }
            this.counters[regIndex] = this.memory[addr];
          }
          this.pointer++;
          break;
        }
        case 'STORE': {
          // Format: STORE rX, [addr]
          let [regPart, addrPart] = args;
          if (regPart.endsWith(',')) {
            regPart = regPart.slice(0, -1);
          }
          let xIndex = parseInt(regPart.replace('r', ''), 10);
          let addrStr = addrPart.slice(1, -1);
          let addr;
          if (addrStr.toLowerCase().startsWith('r')) {
            // Indirect addressing for store
            let addrRegIndex = parseInt(addrStr.replace('r', ''), 10);
            addr = this.counters[addrRegIndex];
          } else {
            addr = parseInt(addrStr, 10);
          }
          this.memory[addr] = this.counters[xIndex];
          this.pointer++;
          break;
        }
        case 'PUSH': {
          // Format: PUSH rX
          let regIndex = parseInt(args[0].replace('r',''), 10);
          if (this.dataStack.length >= this.dataStackMax) {
            console.error("Data stack overflow");
            this.running = false;
            break;
          }
          this.dataStack.push(this.counters[regIndex]);
          this.pointer++;
          break;
        }
        case 'POP': {
          // Format: POP rX
          let regIndex = parseInt(args[0].replace('r',''), 10);
          if (this.dataStack.length === 0) {
            console.error("Data stack underflow");
            this.running = false;
            break;
          }
          this.counters[regIndex] = this.dataStack.pop();
          this.pointer++;
          break;
        }
        case 'PEEK': {
          // Format: PEEK rX, [addr]  (similar to LOAD)
          let [regPart, addrPart] = args;
          if (regPart.endsWith(',')) {
            regPart = regPart.slice(0, -1);
          }
          let regIndex = parseInt(regPart.replace('r', ''), 10);
          let addrStr = addrPart.slice(1, -1);
          let addr;
          if (addrStr.toLowerCase().startsWith('r')) {
            // address in register
            let addrRegIndex = parseInt(addrStr.replace('r', ''), 10);
            addr = this.counters[addrRegIndex];
          } else {
            addr = parseInt(addrStr, 10);
          }
          this.counters[regIndex] = this.memory[addr];
          this.pointer++;
          break;
        }
        case 'POKE': {
          // Format: POKE rX, [addr]  (similar to STORE)
          let [regPart, addrPart] = args;
          if (regPart.endsWith(',')) {
            regPart = regPart.slice(0, -1);
          }
          let xIndex = parseInt(regPart.replace('r', ''), 10);
          let addrStr = addrPart.slice(1, -1);
          let addr;
          if (addrStr.toLowerCase().startsWith('r')) {
            let addrRegIndex = parseInt(addrStr.replace('r', ''), 10);
            addr = this.counters[addrRegIndex];
          } else {
            addr = parseInt(addrStr, 10);
          }
          this.memory[addr] = this.counters[xIndex];
          this.pointer++;
          break;
        }
        case 'ADD': {
          // Format: ADD rX, rY  => rX = rX + rY
          let [rX, rY] = args;
          if (rX.endsWith(',')) rX = rX.slice(0, -1);
          let xIndex = parseInt(rX.replace('r', ''), 10);
          let yIndex = parseInt(rY.replace('r', ''), 10);
          this.counters[xIndex] += this.counters[yIndex];
          this.pointer++;
          break;
        }
        case 'SUB': {
          // Format: SUB rX, rY  => rX = rX - rY
          let [rX, rY] = args;
          if (rX.endsWith(',')) rX = rX.slice(0, -1);
          let xIndex = parseInt(rX.replace('r', ''), 10);
          let yIndex = parseInt(rY.replace('r', ''), 10);
          this.counters[xIndex] -= this.counters[yIndex];
          this.pointer++;
          break;
        }
        case 'MUL': {
          // Format: MUL rX, rY  => rX = rX * rY
          let [rX, rY] = args;
          if (rX.endsWith(',')) rX = rX.slice(0, -1);
          let xIndex = parseInt(rX.replace('r', ''), 10);
          let yIndex = parseInt(rY.replace('r', ''), 10);
          this.counters[xIndex] *= this.counters[yIndex];
          this.pointer++;
          break;
        }
        case 'DIV': {
          // Format: DIV rX, rY  => rX = rX / rY (integer division)
          let [rX, rY] = args;
          if (rX.endsWith(',')) rX = rX.slice(0, -1);
          let xIndex = parseInt(rX.replace('r', ''), 10);
          let yIndex = parseInt(rY.replace('r', ''), 10);
          if (this.counters[yIndex] === 0) {
            console.error("Division by zero");
            this.running = false;
            break;
          }
          this.counters[xIndex] = Math.floor(this.counters[xIndex] / this.counters[yIndex]);
          this.pointer++;
          break;
        }
        case 'JMP': {
          // Format: JMP label
          let label = args[0];
          this.pointer = this.labelMap[label];
          break;
        }
        case 'JZ': {
          // Format: JZ rX, label (jump if rX is zero)
          let [regPart, label] = args;
          if (regPart.endsWith(',')) {
            regPart = regPart.slice(0, -1);
          }
          let regIndex = parseInt(regPart.replace('r', ''), 10);
          if (this.counters[regIndex] === 0) {
            this.pointer = this.labelMap[label];
          } else {
            this.pointer++;
          }
          break;
        }
        case 'JNZ': {
          // Format: JNZ rX, label (jump if rX is not zero)
          let [regPart, label] = args;
          if (regPart.endsWith(',')) {
            regPart = regPart.slice(0, -1);
          }
          let regIndex = parseInt(regPart.replace('r', ''), 10);
          if (this.counters[regIndex] !== 0) {
            this.pointer = this.labelMap[label];
          } else {
            this.pointer++;
          }
          break;
        }
        case 'CALL': {
          // Normal subroutine call: push return address and jump to label
          let label = args[0];
          this.callStack.push(this.pointer + 1);
          this.pointer = this.labelMap[label];
          break;
        }
        case 'PL0CALL': {
          // Call a compiled PL/0 program by name
          let programName = args[0];
          if (programName.endsWith(',')) {
            programName = programName.slice(0, -1);
          }
          let newInstrs = PL0Programs[programName];
          if (!newInstrs) {
            console.error("No compiled PL/0 program named:", programName);
            this.running = false;
            break;
          }
          // Save current context on call stack
          this.callStack.push({
            instructions: this.instructions,
            labelMap: this.labelMap,
            returnPointer: this.pointer + 1
          });
          // Switch to the called program's instruction list and labels
          this.instructions = newInstrs;
          this.buildLabelMap();
          this.pointer = 0;
          break;
        }
        case 'RET': {
          // Return from subroutine or PL/0 call
          if (this.callStack.length > 0) {
            let top = this.callStack.pop();
            if (typeof top === 'number') {
              // Normal CALL return
              this.pointer = top;
            } else {
              // Return from PL0CALL
              this.instructions = top.instructions;
              this.labelMap = top.labelMap;
              this.pointer = top.returnPointer;
            }
          } else {
            // No context to return to: halt
            this.running = false;
          }
          break;
        }
        case 'HALT': {
          this.running = false;
          this.pointer++;
          break;
        }
        default: {
          console.error("Unknown instruction:", op, "at line", this.pointer, "-", line);
          this.running = false;
        }
      }
    }
  }

  buildLabelMap() {
    this.labelMap = {};
    for (let i = 0; i < this.instructions.length; i++) {
      let line = this.instructions[i].trim();
      if (line.endsWith(':')) {
        let labelName = line.slice(0, -1);
        this.labelMap[labelName] = i;
      }
    }
  }
}


// Tokenizer: splits source code into tokens (identifiers, numbers, symbols, keywords)
function tokenize(input) {
  // Remove single-line comments (// ...)
  input = input.replace(/\/\/.*$/gm, '');
  input = input
    .replace(/:=/g, ' := ')
    .replace(/\(/g, ' ( ')
    .replace(/\)/g, ' ) ')
    .replace(/,/g, ' , ')
    .replace(/;/g, ' ; ')
    .replace(/\./g, ' . ');
  const parts = input.split(/\s+/).filter(s => s.length > 0);
  const tokens = [];
  const keywords = ["program", "var", "begin", "end", "call", "if", "then", "while", "do", "odd", "push", "pop", "peek", "poke"];
  const symbols = ["+", "-", "*", "/", ":=", "(", ")", ",", ";", ".", "="];
  for (let rt of parts) {
    if (/^\d+$/.test(rt)) {
      tokens.push({ type: "number", value: parseInt(rt, 10) });
    } else if (keywords.includes(rt.toLowerCase())) {
      tokens.push({ type: "keyword", value: rt.toLowerCase() });
    } else if (symbols.includes(rt)) {
      tokens.push({ type: "symbol", value: rt });
    } else if (/^[a-zA-Z]\w*$/.test(rt)) {
      tokens.push({ type: "ident", value: rt });
    } else {
      tokens.push({ type: "unknown", value: rt });
    }
  }
  return tokens;
}


class PL0Parser {
  constructor(tokens) {
    this.tokens = tokens;
    this.pos = 0;
    this.varTable = new Map();
    this.nextVarAddr = 0;
    this.labelCounter = 100;
    this.tempAddr = 254;
  }

  currentToken() {
    return this.tokens[this.pos] || { type: "EOF", value: "" };
  }
  eat(expected) {
    const token = this.currentToken();
    if (token.value === expected || token.type === expected) {
      this.pos++;
      return token;
    } else {
      throw new Error(`Parse error: expected ${expected}, got ${token.value} at pos=${this.pos}`);
    }
  }
  newLabel() {
    const label = `label_${this.labelCounter}`;
    this.labelCounter++;
    return label;
  }
  newTemp() {
    const t = this.tempAddr;
    this.tempAddr--;
    return t;
  }
  declareVar(ident) {
    if (this.varTable.has(ident)) {
      throw new Error(`Variable '${ident}' already declared`);
    }
    const addr = this.nextVarAddr;
    this.nextVarAddr++;
    this.varTable.set(ident, addr);
    return addr;
  }
  getVarAddr(ident) {
    if (!this.varTable.has(ident)) {
      throw new Error(`Unknown variable '${ident}'`);
    }
    return this.varTable.get(ident);
  }

  // Grammar:
  // program -> "program" ident ";" block "."
  parseProgram() {
    this.eat("program");
    const progNameToken = this.currentToken();
    this.eat("ident");
    this.eat(";");
    const [blockAST, blockCode] = this.parseBlock();
    this.eat(".");
    const programAST = { type: "program", name: progNameToken.value, block: blockAST };
    return [programAST, blockCode];
  }

  // block -> varDecl? statement
  parseBlock() {
    let varDecls = [];
    let codeVars = [];
    if (this.currentToken().value === "var") {
      [varDecls, codeVars] = this.parseVarDecl();
    }
    const [stmtAST, stmtCode] = this.parseStatement();
    const blockAST = { type: "block", varDecls, statement: stmtAST };
    // Combine variable initialization code (if any) with statement code
    const blockCode = codeVars.concat(stmtCode);
    return [blockAST, blockCode];
  }

  // varDecl -> "var" ident {"," ident} ";"
  parseVarDecl() {
    this.eat("var");
    const decls = [];
    while (true) {
      const idToken = this.currentToken();
      this.eat("ident");
      const addr = this.declareVar(idToken.value);
      decls.push({ type: "varDecl", ident: idToken.value, addr: addr });
      if (this.currentToken().value === ",") {
        this.eat(",");
      } else {
        break;
      }
    }
    this.eat(";");
    // No specific code for var declarations (initial values default to 0)
    return [decls, []];
  }

  // statement -> assignment | callStmt | ifStmt | whileStmt | compoundStmt | pushStmt | popStmt | peekStmt | pokeStmt | (empty)
  parseStatement() {
    const tk = this.currentToken();
    if (tk.value === "call") {
      return this.parseCallStatement();
    } else if (tk.value === "if") {
      return this.parseIfStatement();
    } else if (tk.value === "while") {
      return this.parseWhileStatement();
    } else if (tk.value === "begin") {
      return this.parseCompoundStatement();
    } else if (tk.value === "push") {
      return this.parsePushStatement();
    } else if (tk.value === "pop") {
      return this.parsePopStatement();
    } else if (tk.value === "peek") {
      return this.parsePeekStatement();
    } else if (tk.value === "poke") {
      return this.parsePokeStatement();
    } else if (tk.type === "ident") {
      return this.parseAssignment();
    } else {
      // empty statement (possibly just a semicolon or end of block)
      return [{ type: "noop" }, []];
    }
  }

  // callStmt -> "call" ident ";"
  parseCallStatement() {
    this.eat("call");
    const idToken = this.currentToken();
    this.eat("ident");
    this.eat(";");
    const ast = { type: "call", ident: idToken.value };
    const code = [`PL0CALL ${idToken.value}`];
    return [ast, code];
  }

  // assignment -> ident ":=" expression ";"
  parseAssignment() {
    const idToken = this.currentToken();
    this.eat("ident");
    this.eat(":=");
    const [exprAST, exprCode] = this.parseExpression();
    this.eat(";");
    const addr = this.getVarAddr(idToken.value);
    const code = [
      ...exprCode,
      `STORE r0, [${addr}]`
    ];
    const ast = { type: "assign", ident: idToken.value, expr: exprAST };
    return [ast, code];
  }

  // pushStmt -> "push" ident ";"
  parsePushStatement() {
    this.eat("push");
    const idToken = this.currentToken();
    this.eat("ident");
    this.eat(";");
    const addr = this.getVarAddr(idToken.value);
    // Load variable value into r0, then push it
    const code = [
      `LOAD r0, [${addr}]`,
      `PUSH r0`
    ];
    const ast = { type: "push", ident: idToken.value };
    return [ast, code];
  }

  // popStmt -> "pop" ident ";"
  parsePopStatement() {
    this.eat("pop");
    const idToken = this.currentToken();
    this.eat("ident");
    this.eat(";");
    const addr = this.getVarAddr(idToken.value);
    // Pop top of stack into r0, then store r0 into variable
    const code = [
      `POP r0`,
      `STORE r0, [${addr}]`
    ];
    const ast = { type: "pop", ident: idToken.value };
    return [ast, code];
  }

  // peekStmt -> "peek(" ident "," ident ") ;"
  parsePeekStatement() {
    this.eat("peek");
    this.eat("(");
    const destToken = this.currentToken();
    this.eat("ident");
    this.eat(",");
    const addrToken = this.currentToken();
    this.eat("ident");
    this.eat(")");
    this.eat(";");
    const destAddr = this.getVarAddr(destToken.value);
    const addrVarAddr = this.getVarAddr(addrToken.value);
    // Load the address value from addrVar into r0, then peek memory at that address into r1, then store into dest
    const code = [
      `LOAD r0, [${addrVarAddr}]`,
      `PEEK r1, [r0]`,
      `STORE r1, [${destAddr}]`
    ];
    const ast = { type: "peek", dest: destToken.value, addr: addrToken.value };
    return [ast, code];
  }

  // pokeStmt -> "poke(" ident "," ident ") ;"
  parsePokeStatement() {
    this.eat("poke");
    this.eat("(");
    const addrToken = this.currentToken();
    this.eat("ident");
    this.eat(",");
    const valToken = this.currentToken();
    this.eat("ident");
    this.eat(")");
    this.eat(";");
    const addrVarAddr = this.getVarAddr(addrToken.value);
    const valAddr = this.getVarAddr(valToken.value);
    // Load the target address from addrVar into r0, load the value into r1, then poke value into memory at that address
    const code = [
      `LOAD r0, [${addrVarAddr}]`,
      `LOAD r1, [${valAddr}]`,
      `POKE r1, [r0]`
    ];
    const ast = { type: "poke", addr: addrToken.value, val: valToken.value };
    return [ast, code];
  }

  // ifStmt -> "if" expression "then" statement
  parseIfStatement() {
    this.eat("if");
    const [condAST, condCode] = this.parseExpression();
    this.eat("then");
    const [thenAST, thenCode] = this.parseStatement();
    const skipLabel = this.newLabel();
    const code = [
      ...condCode,
      `JZ r0, ${skipLabel}`,
      ...thenCode,
      `${skipLabel}:`
    ];
    const ast = { type: "if", condition: condAST, thenPart: thenAST };
    return [ast, code];
  }

  // whileStmt -> "while" expression "do" statement
  parseWhileStatement() {
    this.eat("while");
    const startLabel = this.newLabel();
    const exitLabel = this.newLabel();
    const loopStart = `${startLabel}:`;
    const [condAST, condCode] = this.parseExpression();
    this.eat("do");
    const [bodyAST, bodyCode] = this.parseStatement();
    const code = [
      loopStart,
      ...condCode,
      `JZ r0, ${exitLabel}`,
      ...bodyCode,
      `JMP ${startLabel}`,
      `${exitLabel}:`
    ];
    const ast = { type: "while", condition: condAST, body: bodyAST };
    return [ast, code];
  }

  // compoundStmt -> "begin" statement { ";" statement } "end"
  parseCompoundStatement() {
  this.eat("begin");
  const stmts = [];
  const codeAll = [];
  // Loop until we reach "end"
  while (this.currentToken().value !== "end") {
    const [stmtAST, stmtCode] = this.parseStatement();
    stmts.push(stmtAST);
    codeAll.push(...stmtCode);
    // If there's a semicolon, consume it (optional between statements)
    if (this.currentToken().value === ";") {
      this.eat(";");
    }
  }
  this.eat("end");
  const ast = { type: "compound", statements: stmts };
  return [ast, codeAll];
}

  // expression -> term { (+|-) term }
  parseExpression() {
    // Parse left term
    let [leftAST, leftCode] = this.parseTerm();
    // Handle + and - operators
    while (this.currentToken().value === "+" || this.currentToken().value === "-") {
      const opToken = this.currentToken().value;
      this.eat(opToken);
      const [rightAST, rightCode] = this.parseTerm();
      // Build AST node for binary operation
      const binAST = { type: "binop", op: opToken, left: leftAST, right: rightAST };
      // Reserve a temporary memory cell to save left operand
      const tempAddr = this.newTemp();
      const storeLeft = `STORE r0, [${tempAddr}]`;
      const loadLeftIntoR1 = `LOAD r1, [${tempAddr}]`;
      const opInstruction = opToken === "+" ? "ADD r0, r1" : "SUB r0, r1";
      const code = [
        ...leftCode,
        storeLeft,
        ...rightCode,
        loadLeftIntoR1,
        opInstruction
      ];
      leftAST = binAST;
      leftCode = code;
    }
    return [leftAST, leftCode];
  }

  // term -> factor { (*|/) factor }
  parseTerm() {
    let [leftAST, leftCode] = this.parseFactor();
    while (this.currentToken().value === "*" || this.currentToken().value === "/") {
      const opToken = this.currentToken().value;
      this.eat(opToken);
      const [rightAST, rightCode] = this.parseFactor();
      const binAST = { type: "binop", op: opToken, left: leftAST, right: rightAST };
      const tempAddr = this.newTemp();
      const storeLeft = `STORE r0, [${tempAddr}]`;
      const loadLeftIntoR1 = `LOAD r1, [${tempAddr}]`;
      const opInstruction = opToken === "*" ? "MUL r0, r1" : "DIV r0, r1";
      const code = [
        ...leftCode,
        storeLeft,
        ...rightCode,
        loadLeftIntoR1,
        opInstruction
      ];
      leftAST = binAST;
      leftCode = code;
    }
    return [leftAST, leftCode];
  }

  // factor -> number | ident | "(" expression ")"
  parseFactor() {
    const tk = this.currentToken();
    if (tk.type === "number") {
      this.eat("number");
      const code = [`LOAD r0, #${tk.value}`];
      return [{ type: "num", value: tk.value }, code];
    } else if (tk.type === "ident") {
      this.eat("ident");
      const addr = this.getVarAddr(tk.value);
      const code = [`LOAD r0, [${addr}]`];
      return [{ type: "var", name: tk.value }, code];
    } else if (tk.value === "(") {
      this.eat("(");
      const [exprAST, exprCode] = this.parseExpression();
      this.eat(")");
      return [exprAST, exprCode];
    } else {
      throw new Error("Unexpected token in factor: " + JSON.stringify(tk));
    }
  }
}

// Global storage for compiled PL/0 programs
const PL0Programs = {};

// Compile PL/0 source code into CounterMachine instructions
function compilePL0(programText, baseAddr = 0) {
  const tokens = tokenize(programText);
  const parser = new PL0Parser(tokens);
  if (baseAddr !== 0) {
    parser.nextVarAddr = baseAddr;  // start allocating vars at a given memory address
  }
  const [ast, code] = parser.parseProgram();
  code.push("RET");  // ensure program returns to caller
  PL0Programs[ast.name] = code;
  return code;
}

// ======================================================
// Tiny C → PL/0 transpiler
// (no changes to your CPU or PL/0 language needed)
// ======================================================

// ------------- Tokenizer -------------
function tinyc_tokenize(source) {
  // normalize whitespace
  source = source.replace(/\r/g, ' ').replace(/\n/g, ' ').replace(/\t/g, ' ');

  // punctuation / operators we care about
  const punct = ['(', ')', '{', '}', ';', '=', '+', '-', '*', '/', ',', '<', '>'];

  // space them out
  for (const p of punct) {
    source = source.split(p).join(` ${p} `);
  }

  const parts = source.split(/\s+/).filter(Boolean);

  const keywords = new Set([
    'int',
    'void',
    'if',
    'while',
    'push',
    'pop',
    'peek',
    'poke'
  ]);

  return parts.map(part => {
    if (keywords.has(part)) {
      return { type: 'keyword', value: part };
    }
    if (punct.includes(part)) {
      return { type: 'symbol', value: part };
    }
    if (/^\d+$/.test(part)) {
      return { type: 'number', value: parseInt(part, 10) };
    }
    return { type: 'ident', value: part };
  });
}

// ------------- Parser (C-side AST) -------------
class TinyCParser {
  constructor(tokens) {
    this.tokens = tokens;
    this.pos = 0;
  }

  current() {
    return this.tokens[this.pos] || { type: 'eof', value: '' };
  }

  eat(expected) {
    const tok = this.current();
    if (tok.value === expected || tok.type === expected) {
      this.pos++;
      return tok;
    }
    throw new Error(`Expected ${expected} but found ${tok.value}`);
  }

  // program -> (varDecl)* (function)*
  // we will ultimately emit only the first function as "program ..."
  parseProgram() {
    const globals = [];
    while (this.current().value === 'int') {
      globals.push(...this.parseVarDecl());
    }

    const funcs = [];
    while (this.current().value === 'void') {
      funcs.push(this.parseFunction());
    }

    if (funcs.length === 0) {
      throw new Error('No function found (expected at least void main())');
    }

    return {
      type: 'program',
      globals,
      funcs
    };
  }

  // int x, y, z;
  parseVarDecl() {
    this.eat('int');
    const names = [];
    names.push(this.eat('ident').value);
    while (this.current().value === ',') {
      this.eat(',');
      names.push(this.eat('ident').value);
    }
    this.eat(';');
    return names;
  }

  // void name() { ... }
  parseFunction() {
    this.eat('void');
    const name = this.eat('ident').value;
    this.eat('(');
    this.eat(')');
    const body = this.parseBlock();
    return {
      type: 'function',
      name,
      body
    };
  }

  // { stmt* }
  parseBlock() {
    this.eat('{');
    const stmts = [];
    while (this.current().value !== '}') {
      stmts.push(this.parseStatement());
    }
    this.eat('}');
    return stmts;
  }

  parseStatement() {
    const tok = this.current();

    // if (...)
    if (tok.value === 'if') {
      return this.parseIf();
    }

    // while (...)
    if (tok.value === 'while') {
      return this.parseWhile();
    }

    // push x;
    if (tok.value === 'push') {
      return this.parsePush();
    }

    // pop x;
    if (tok.value === 'pop') {
      return this.parsePop();
    }

    // peek(a, b);
    if (tok.value === 'peek') {
      return this.parsePeek();
    }

    // poke(a, b);
    if (tok.value === 'poke') {
      return this.parsePoke();
    }

    // ident ... -> either call() or assignment
    if (tok.type === 'ident') {
      const next = this.tokens[this.pos + 1];
      if (next && next.value === '(') {
        return this.parseCall();
      }
      return this.parseAssign();
    }

    throw new Error(`Unexpected token in statement: ${tok.value}`);
  }

  // x = expr ;
  parseAssign() {
    const id = this.eat('ident').value;
    this.eat('=');
    const expr = this.parseExpression();
    this.eat(';');
    return {
      type: 'assign',
      id,
      expr
    };
  }

  // foo();
  parseCall() {
    const id = this.eat('ident').value;
    this.eat('(');
    this.eat(')');
    this.eat(';');
    return {
      type: 'call',
      id
    };
  }

  // if (expr) { ... }
  parseIf() {
    this.eat('if');
    this.eat('(');
    const cond = this.parseExpression();
    this.eat(')');
    const then = this.parseBlock();
    return {
      type: 'if',
      cond,
      then
    };
  }

  // while (expr) { ... }
  parseWhile() {
    this.eat('while');
    this.eat('(');
    const cond = this.parseExpression();
    this.eat(')');
    const body = this.parseBlock();
    return {
      type: 'while',
      cond,
      body
    };
  }

  // push x;
  parsePush() {
    this.eat('push');
    const id = this.eat('ident').value;
    this.eat(';');
    return {
      type: 'push',
      id
    };
  }

  // pop x;
  parsePop() {
    this.eat('pop');
    const id = this.eat('ident').value;
    this.eat(';');
    return {
      type: 'pop',
      id
    };
  }

  // peek(a, b);
  parsePeek() {
    this.eat('peek');
    this.eat('(');
    const dest = this.eat('ident').value;
    this.eat(',');
    const addr = this.eat('ident').value;
    this.eat(')');
    this.eat(';');
    return {
      type: 'peek',
      dest,
      addr
    };
  }

  // poke(a, b);
  parsePoke() {
    this.eat('poke');
    this.eat('(');
    const addr = this.eat('ident').value;
    this.eat(',');
    const value = this.eat('ident').value;
    this.eat(')');
    this.eat(';');
    return {
      type: 'poke',
      addr,
      value
    };
  }

  // ----- expression with precedence -----
  // expr -> term (('+'|'-') term)*
  parseExpression() {
    let node = this.parseTerm();
    while (this.current().value === '+' || this.current().value === '-') {
      const op = this.eat(this.current().value).value;
      const right = this.parseTerm();
      node = {
        type: 'binop',
        op,
        left: node,
        right
      };
    }
    return node;
  }

  // term -> factor (('*' | '/') factor)*
  parseTerm() {
    let node = this.parseFactor();
    while (this.current().value === '*' || this.current().value === '/') {
      const op = this.eat(this.current().value).value;
      const right = this.parseFactor();
      node = {
        type: 'binop',
        op,
        left: node,
        right
      };
    }
    return node;
  }

  // factor -> number | ident | '(' expr ')'
  parseFactor() {
    const tok = this.current();
    if (tok.type === 'number') {
      this.eat('number');
      return { type: 'num', value: tok.value };
    }
    if (tok.type === 'ident') {
      this.eat('ident');
      return { type: 'var', name: tok.value };
    }
    if (tok.value === '(') {
      this.eat('(');
      const expr = this.parseExpression();
      this.eat(')');
      return expr;
    }
    throw new Error(`Unexpected token in expression: ${tok.value}`);
  }
}

// ------------- PL/0 emitter -------------
function pl0_emit(ast) {
  // pick main (or just first function)
  const mainFunc = ast.funcs.find(f => f.name === 'main') || ast.funcs[0];

  const lines = [];
  lines.push(`program ${mainFunc.name};`);

  if (ast.globals.length > 0) {
    lines.push(`var ${ast.globals.join(', ')};`);
  }

  lines.push('begin');

  for (const stmt of mainFunc.body) {
    lines.push(...emitStmt(stmt));
  }

  lines.push('end.');
  return lines.join('\n');
}

function emitStmt(stmt) {
  switch (stmt.type) {
    case 'assign':
      return [`${stmt.id} := ${emitExpr(stmt.expr)};`];

    case 'call':
      return [`call ${stmt.id};`];

    case 'push':
      return [`push ${stmt.id};`];

    case 'pop':
      return [`pop ${stmt.id};`];

    case 'peek':
      // PL/0 side already expects peek(dest, addrVar);
      return [`peek(${stmt.dest}, ${stmt.addr});`];

    case 'poke':
      // PL/0 side already expects poke(addrVar, valueVar);
      return [`poke(${stmt.addr}, ${stmt.value});`];

    case 'if': {
      const out = [];
      out.push(`if ${emitExpr(stmt.cond)} then`);
      out.push('begin');
      for (const s of stmt.then) {
        out.push(...emitStmt(s));
      }
      out.push('end');
      return out;
    }

    case 'while': {
      const out = [];
      out.push(`while ${emitExpr(stmt.cond)} do`);
      out.push('begin');
      for (const s of stmt.body) {
        out.push(...emitStmt(s));
      }
      out.push('end');
      return out;
    }

    default:
      throw new Error(`Unknown statement type: ${stmt.type}`);
  }
}

function emitExpr(e) {
  switch (e.type) {
    case 'num':
      return e.value;
    case 'var':
      return e.name;
    case 'binop':
      return `${emitExpr(e.left)} ${e.op} ${emitExpr(e.right)}`;
    default:
      throw new Error(`Unknown expression node: ${e.type}`);
  }
}

// ------------- example usage -------------
 const src = `
 int x, y, addr;
 void main() {
   x = 5;
   push x;
   peek(y, addr);
   while (x) {
     x = x - 1;
   }
 }
 `;
 const tokens = tinyc_tokenize(src);
 const parser = new TinyCParser(tokens);
 const ast = parser.parseProgram();
 console.log(pl0_emit(ast));
