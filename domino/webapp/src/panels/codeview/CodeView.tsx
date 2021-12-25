import { Accessor, Setter, Show } from "solid-js";
import { FrameInfo, FrameView, FrameId } from "./FrameView";
import { SourceInfo, SourceView } from "./SourceView";

export interface CodeViewProps {
  sourceInfo: SourceInfo;
  frameInfo: FrameInfo;
  onFrame: Setter<FrameId>;
}

export function CodeView(props: CodeViewProps) {
  return (
    <>
      <div class="codeview">
        <SourceView sourceInfo={props.sourceInfo} />
      </div>
      <div className="frames">
        <FrameView frameInfo={props.frameInfo} onFrame={props.onFrame} />
      </div>
    </>
  );
}
