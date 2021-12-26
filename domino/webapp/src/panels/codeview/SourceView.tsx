import { For } from "solid-js";
import { SourceLocation, SourceSpan } from "../../api";

export interface SourceInfo {
  source: string[];
  highlight: SourceLocation;
}

interface SourceViewProps {
  sourceInfo: SourceInfo;
}

export function SourceView(props: SourceViewProps) {
  const MAX_LINES_AWAY = 30;

  const {
    source,
    highlight: { start, end },
  } = props.sourceInfo;

  // split the lines into pre, focus, and post
  const pre = [];
  const focus = [];
  const post = [];

  const lineStart = Math.max(start.line - MAX_LINES_AWAY, 0);
  const lineEnd = Math.min(end.line + MAX_LINES_AWAY, source.length);

  for (let i = lineStart; i < lineEnd; i++) {
    const line = source[i];

    if (i < start.line - 1) pre.push(line);
    else if (i >= end.line) post.push(line);
    else focus.push(line);
  }

  return (
    <>
      <RenderLines className="codeview-pre" lines={pre} />
      <RenderHighlightedLines
        className="codeview-focus"
        lines={focus}
        start={start}
        end={end}
      />
      <RenderLines className="codeview-post" lines={post} />
    </>
  );
}

interface RenderLinesProps {
  className: string;
  lines: string[];
}

function RenderLines(props: RenderLinesProps) {
  return (
    <div className={props.className}>
      <For each={props.lines}>
        {(line) => (line ? <span>{line}</span> : <span> </span>)}
      </For>
    </div>
  );
}

interface RenderHighlightedLinesProps extends RenderLinesProps {
  start: SourceSpan;
  end: SourceSpan;
}

function RenderHighlightedLines(props: RenderHighlightedLinesProps) {
  return (
    <div className={props.className}>
      <For each={props.lines}>
        {(line, idx) => {
          let pre = "";
          let content = line;
          let post = "";

          const atStart = idx() == 0;
          if (atStart) {
            const col = props.start.column;
            pre = content.substr(0, col);
            content = content.substring(col);
          }

          const atEnd = idx() == props.lines.length - 1;
          if (atEnd) {
            const col = props.end.column - pre.length;
            post = content.substr(col);
            content = content.substring(0, col);
          }

          if (pre === "" && post === "") {
            const limitEnd = atEnd ? " limitEnd" : "";
            return <span className={"highlighted" + limitEnd}>{content}</span>;
          } else {
            const stretchToEnd = atStart && !atEnd;
            const stretchName = stretchToEnd ? " line-to-end" : "";
            return (
              <div className="line-split">
                {pre && <span>{pre}</span>}
                <span className={"highlighted" + stretchName}>{content}</span>
                {post && <span>{post}</span>}
              </div>
            );
          }
        }}
      </For>
    </div>
  );
}
