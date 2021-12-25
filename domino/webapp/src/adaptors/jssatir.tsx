import { Accessor, Setter } from "solid-js";
import { Moment } from "../api";
import { CodeViewProps } from "../panels/codeview/CodeView";

export function JssatIRAdaptor([moment, setMoment]: [
  Accessor<Moment>,
  Setter<Moment>
]): () => CodeViewProps | undefined {
  return () => {
    const now = moment();

    const sourceInfo = {
      source: now.code.lines.map(({ display }) => display),
      highlight: {
        start: {
          line: now.code.highlighted,
          column: 0,
        },
        end: {
          line: now.code.highlighted,
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
