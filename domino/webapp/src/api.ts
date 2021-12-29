// all values explained in ./domino/src/server_data.rs

const url = (path: string) => {
  if (Boolean(process.env.NODE_ENV)) {
    // debug
    return "http://localhost:8000" + path;
  } else return path;
};

export const fetchOverview = () =>
  fetch(url("/overview")).then((resp) => resp.json() as Promise<Overview>);

export const fetchMoment = (idx: number) =>
  fetch(url(`/moment/${idx}`)).then((resp) => resp.json() as Promise<Moment>);

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
  header: string;
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
  registers: Record<string, MomentValue>;
  lists: Record<string, MomentValue[]>;
  records: Record<string, [MomentRecordKey, MomentValue][]>;
}

export type MomentValue =
  | { kind: "atom"; atom: string }
  | { kind: "num"; num: number }
  | { kind: "bool"; bool: boolean }
  | { kind: "bytes"; bytes: string }
  | { kind: "fnptr"; fnptr: number }
  | { kind: "rec"; rec: string }
  | { kind: "list"; list: string };

export type MomentRecordKey =
  | { kind: "atom"; atom: string }
  | { kind: "num"; num: number }
  | { kind: "bool"; bool: boolean }
  | { kind: "bytes"; bytes: string }
  | { kind: "fnptr"; fnptr: number };
