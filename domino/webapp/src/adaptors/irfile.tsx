import { Accessor, createEffect, createSignal } from "solid-js";
import { Moment, Source, SourceLocation } from "../api";
import { CodeViewProps } from "../panels/codeview/CodeView";
import { Frame } from "../panels/codeview/FrameView";

export function IRFileAdaptor(
  sources: Source[],
  moment: Accessor<Moment | undefined>
): () => CodeViewProps | undefined {
  const sourceLines = sources.map(({ text }) => text.split("\n"));

  const [sourceIdx, setSourceIdx] = createSignal(0);

  createEffect(() => {
    // when `moment` changes, reset `sourceIdx`
    moment();
    setSourceIdx(0);
  });

  const derivedMoment = () => {
    const source = moment()?.source;
    if (!source) return undefined;

    return source;
  };

  return () => {
    const source = derivedMoment();
    if (!source) return undefined;

    const idx = sourceIdx();

    const sourceInfo = {
      source: sourceLines[source.sourceId],
      highlight: source.locations[idx],
    };

    const frameInfo = {
      frames: source.locations.map((location, id) =>
        makeFramePreview(sourceLines[source.sourceId], location, id)
      ),
      focused: idx,
    };

    return {
      sourceInfo,
      frameInfo,
      onFrame: setSourceIdx,
    };
  };
}

function makeFramePreview(
  lines: string[],
  location: SourceLocation,
  id: number
): Frame {
  let display;

  if (location.start.line === location.end.line) {
    const lineIdx = location.start.line - 1;
    display = lines[lineIdx].substring(
      location.start.column,
      location.end.column
    );
  } else {
    const start = lines[location.start.line - 1].substring(
      location.start.column
    );
    const end = lines[location.end.line - 1].substring(0, location.end.column);
    display = start + end;
  }

  if (display.length >= 32) {
    display =
      display.substring(0, 14) + "..." + display.substring(display.length - 15);
  }

  return {
    id,
    display: `${display} ${location.start.line}:${location.start.column}->${location.end.line}:${location.end.column}`,
  };
}
