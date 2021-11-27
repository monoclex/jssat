// all values explained in ./domino/src/server_data.rs

export const fetchOverview = () =>
  fetch("http://localhost:8000/overview").then(
    (resp) => resp.json() as Promise<Overview>
  );

export const fetchMoment = (idx: number) =>
  fetch(`http://localhost:8000/moment/${idx}`).then(
    (resp) => resp.json() as Promise<Moment>
  );

export interface Overview {
  totalMoments: number;
  sources: Source[];
}

export interface Source {
  name?: string;
  text: string;
}

export interface SourceSpan {
  line: number;
  column: number;
}

export interface Moment {
  callstack: Frame[];
  code: FrameCode;
  source?: MomentSource;
  values: MomentValues;
}

export interface Frame {
  preview: string;
  moment: number;
  prevMoment?: number;
  nextMoment?: number;
}

export interface FrameCode {
  lines: CodeLine[];
  highlighted: number;
}

export interface CodeLine {
  display: string;
}

export interface MomentSource {
  sourceId: number;
  locations: SourceLocation[];
}

export interface SourceLocation {
  start: SourceSpan;
  end: SourceSpan;
}

export interface MomentValues {
  registers: Record<number, MomentValue>;
  lists: Record<number, MomentValue[]>;
  records: Record<number, [MomentRecordKey, MomentValue][]>;
}

export type MomentValue
  = { kind: "atom", atom: string }
  | { kind: "num", num: number }
  | { kind: "bool", bool: boolean }
  | { kind: "bytes", bytes: string  }
  | { kind: "fnptr", fnptr: number }
  | { kind: "rec", rec: number }
  | { kind: "list", list: number }
  ;

export type MomentRecordKey
  = { kind: "atom", atom: string }
  | { kind: "num", num: number }
  | { kind: "bool", bool: boolean }
  | { kind: "bytes", bytes: string }
  | { kind: "fnptr", fnptr: number }
  ;
