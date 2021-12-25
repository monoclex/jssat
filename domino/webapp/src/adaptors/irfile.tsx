import { Accessor, createSignal } from "solid-js";
import { Moment, Source } from "../api";
import { CodeViewProps } from "../panels/codeview/CodeView";

export function IRFileAdaptor(
  sources: Source[],
  moment: Accessor<Moment>
): () => CodeViewProps | undefined {
  const sourceLines = sources.map(({ text }) => text.split("\n"));

  const [sourceIdx, setSourceIdx] = createSignal(0);

  const derivedMoment = () => {
    const source = moment().source;
    if (!source) return undefined;

    setSourceIdx(0);
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
      frames: sourceLines[source.sourceId].map((display, id) => ({
        display,
        id,
      })),
      focused: idx,
    };

    return {
      sourceInfo,
      frameInfo,
      onFrame: setSourceIdx,
    };
  };
}
