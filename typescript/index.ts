import { readFileSync } from 'fs';
import { createInterface } from 'readline';

import { bold, green, red } from 'chalk';
import { Parser, ParsingError } from './source/parser';
import { Interpreter, RuntimeError } from './source/eval';
import { Datum, toString } from './source/datum';

const RL = createInterface({
  input: process.stdin,
  output: process.stdout,
  prompt: green('> '),
});

const PARSER = new Parser();

const INTERP = new Interpreter();

if (process.argv.length > 3) {
  RL.write(red('Error: too many arguments given'));
  process.exit(1);
}

if (process.argv.length === 3) {
  const file = readFileSync(process.argv[2], 'utf-8');

  const parsed = PARSER.parseLines(file);

  if (parsed instanceof ParsingError) {
    for (const parseError of (parsed as ParsingError[])) {
      RL.write(red(`Error while parsing: ${parseError.description}.\n`));
    }

    process.exit(1);
  }

  for (const expr of (parsed as Datum[])) {
    const result = INTERP.evaluate(expr);

    if (result instanceof RuntimeError) {
      RL.write(red(`Error while executing: ${result.description}.\n`));
      process.exit(1);
    }
  }

  process.exit(0);
}

const runRepl = () => {
  RL.question(green('> '), (line => {
    const parsed = PARSER.parseLine(line);

    if (parsed instanceof ParsingError) {
      RL.write(red(`Error while parsing: ${parsed.description}.\n`));
      return runRepl();
    }

    const result = INTERP.evaluate(parsed);

    if (result instanceof RuntimeError) {
      RL.write(red(`Error while executing: ${result.description}.\n`));
      return runRepl();
    }

    RL.write(bold(toString(result)));
    RL.write('\n');
    runRepl();
  }));
};

runRepl();
