import { spawn } from 'child_process';
import { createReadStream, createWriteStream, readFileSync } from 'fs';
import { stdin, stdout } from 'process';
import { Readable, Writable, Duplex } from 'stream';

export class ReadPort extends Readable { }

export class WritePort extends Writable { }

export class ReadWritePort extends Duplex { }

export function getStdin(): ReadPort {
  return stdin;
}

export function getStdout() {
  return stdout;
}

export function systemExec(cmd: string, args: string[]): [WritePort, ReadPort] {
  const proc = spawn(cmd, args);
  return [proc.stdin, proc.stdout];
}

export function readFileToString(file: string): string {
  return readFileSync(file, 'utf-8');
}

export function openFileRead(file: string): ReadPort {
  return createReadStream(file);
}

export function openFileWrite(file: string): WritePort {
  return createWriteStream(file);
}