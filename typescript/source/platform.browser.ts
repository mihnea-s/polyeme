/**
 * Tiny makeshft stand-in for NodeJs APIs in the browser.
 */

export abstract class ReadPort {
  abstract read(): string;
  abstract destroy(): string;
}

export abstract class WritePort {
  abstract write(): string;
  abstract destroy(): string;
}

export abstract class ReadWritePort {
  abstract read(): string;
  abstract write(): string;
  abstract destroy(): string;
}

class UnsupportedError { }

let browserStdin: ReadPort | null = null;

export function getStdin(): ReadPort {
  if (!browserStdin) {
    throw new UnsupportedError();
  }

  return browserStdin;
}

export function setStdin(stdin: ReadPort) {
  browserStdin = stdin;
}

let browserStdout: WritePort | null = null;

export function getStdout(): WritePort {
  if (!browserStdout) {
    throw new UnsupportedError();
  }

  return browserStdout;
}

export function setStdout(stdout: WritePort) {
  browserStdout = stdout;
}

export function systemExec(_: string, __: string[]): [WritePort, ReadPort] {
  throw new UnsupportedError();
}

export function readFileToString(_: string): string {
  throw new UnsupportedError();
}

export function openFileRead(_: string): ReadPort {
  throw new UnsupportedError();
}

export function openFileWrite(_: string): WritePort {
  throw new UnsupportedError();
}