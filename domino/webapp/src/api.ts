// all values explained in ./domino/src/server_data.rs

export interface SourceSpan {}

export const fetchOverview = () =>
  fetch("http://localhost:8000/overview").then(
    (resp) => resp.json() as Promise<Overview>
  );

export interface Overview {
  totalMoments: number;
}

export const fetchMoment = (idx: number) =>
  fetch(`http://localhost:8000/moment/${idx}`).then(
    (resp) => resp.json() as Promise<Moment>
  );

export interface Moment {
  callstack: Frame[];
  code: FrameCode;
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
  source: SourceSpan;
}
