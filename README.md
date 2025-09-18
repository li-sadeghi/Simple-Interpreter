# Simple-Interpreter

This project is a simple programming language implementation in **Racket**.  
It includes a lexer, parser, interpreter, and tools for running tests and example solutions.

## Structure
- `lexer.rkt` – Lexical analysis  
- `parser.rkt` – Syntax parsing  
- `interpreter.rkt` – Core interpreter  
- `datatypes.rkt`, `environment.rkt`, `store.rkt` – Supporting components  
- `test-runner.rkt` – Runs test cases from `tests/`  
- `solution-runner.rkt` – Runs programs from `solutions/`  

## Usage
1. Install [Racket](https://racket-lang.org/) (v8.0 or later).  
2. Run tests:  
   ```bash
   racket test-runner.rkt
   ```
3. Run a solution:  
   ```bash
   racket solution-runner.rkt
   ```

## Notes
- Example programs are in `solutions/`  
- Test cases are in `tests/`  
- See `ProjectReport.pdf` for detailed documentation  
