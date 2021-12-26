import { Accessor, Setter } from "solid-js";
import { Moment } from "../api";
import { CodeViewProps } from "../panels/codeview/CodeView";
import { FrameId } from "../panels/codeview/FrameView";

export function JssatIRAdaptor([moment, setMoment]: [
  Accessor<Moment | undefined>,
  Setter<FrameId>
]): () => CodeViewProps | undefined {
  return () => {
    const now = moment();
    if (!now) return undefined;

    const sourceInfo = {
      source: now.code.lines.map(({ display }) => display),
      highlight: {
        start: {
          line: now.code.highlighted + 1,
          column: 0,
        },
        end: {
          line: now.code.highlighted + 1,
          column: now.code.lines[now.code.highlighted].display.length,
        },
      },
    };

    const frameInfo = {
      frames: now.callstack.map(({ preview, moment }) => ({
        id: moment,
        display: preview,
      })),
      focused: now.callstack[0].moment,
    };

    return {
      sourceInfo,
      frameInfo,
      onFrame: setMoment,
    };
  };
}
