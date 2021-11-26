// all values explained in ./domino/src/server_data.rs

const id = <T>(x: T): T => x;
const slow = id ; // <T>(x: T): Promise<T> => new Promise(resolve => setTimeout(() => resolve(x), 500));

export interface SourceSpan {}

export const fetchOverview = () => fetch("http://localhost:8000/overview").then(slow).then(resp => resp.json() as Promise<Overview>);

export interface Overview {
    totalMoments: number
}

export const fetchMoment = (idx: number) => fetch(`http://localhost:8000/moment/${idx}`).then(slow).then(resp => resp.json() as Promise<Moment>);

export interface Moment {
    callstack: Frame[],
    code: FrameCode,
}

export interface Frame {
    preview: string
}

export interface FrameCode {
    lines: CodeLine[],
    highlighted: number,
}

export interface CodeLine {
    display: string,
    source: SourceSpan,
}