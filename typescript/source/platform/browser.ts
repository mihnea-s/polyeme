
export class ReadPort { }

export class WritePort { }

export class ReadWritePort { }

class UnsupportedError { }

export function getStdin(): ReadPort {
  throw new UnsupportedError();
}

export function getStdout() {
  throw new UnsupportedError();
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