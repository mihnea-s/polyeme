import { readFileSync } from 'fs';
import { createInterface } from 'readline';

import { bold, green, magenta, red } from 'chalk';
import { Parser, ParsingError } from './source/parser';
import { Interpreter, RuntimeError } from './source/eval';
import { Datum, toString } from './source/datum';
import { Preloaded } from './source/preloaded';

const VERSION = '0.1.0';

const RL = createInterface({
  input: process.stdin,
  output: process.stdout,
});

const PARSER = new Parser();

const INTERP = new Interpreter();

console.log(magenta(`Polyeme JS -- ${VERSION}`));

INTERP.preload(Preloaded);

if (process.argv.length > 3) {
  console.log(red('Error: too many arguments given'));
  process.exit(1);
}

if (process.argv.length === 3) {
  const file = readFileSync(process.argv[2], 'utf-8');

  const parsed = PARSER.parseLines(file);

  if (parsed[0] instanceof ParsingError) {
    for (const parseError of (parsed as ParsingError[])) {
      console.log(red(`Error while parsing: ${parseError.description}.`));
    }

    process.exit(1);
  }

  for (const expr of (parsed as Datum[])) {
    const result = INTERP.evaluate(expr);

    if (result instanceof RuntimeError) {
      console.log(red(`Error while executing: ${result.description}.`));
      process.exit(1);
    }
  }

  process.exit(0);
}

const runRepl = (indent: number = 0) => {
  let buffer: string = '';

  RL.question(green(indent == 0 ? '> ' : '| '), (line => {
    buffer += line;
    indent += (line.match(/\(/) ?? []).length - (line.match(/\)/) ?? []).length;

    if (indent > 0) {
      return runRepl(indent);
    } else {
      line = buffer;
      buffer = '';
    }

    const parsed = PARSER.parseLine(line);

    if (parsed instanceof ParsingError) {
      console.log(red(`Error while parsing: ${parsed.description}.`));
      return runRepl();
    }

    const result = INTERP.evaluate(parsed);

    if (result instanceof RuntimeError) {
      console.log(red(`Error while executing: ${result.description}.`));
      return runRepl();
    }

    console.log(bold(toString(result)));
    runRepl();
  }));
};

runRepl();
