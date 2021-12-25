import { For, Setter } from "solid-js";

export type FrameId = number;

export interface FrameInfo {
  frames: Frame[];
  focused: FrameId;
}

export interface Frame {
  id: FrameId;
  display: string;
}

interface FrameViewProps {
  frameInfo: FrameInfo;
  onFrame: Setter<FrameId>;
}

export function FrameView(props: FrameViewProps) {
  return (
    <For each={props.frameInfo.frames}>
      {(frame) => (
        <DisplayFrame
          frame={frame}
          focused={frame.id === props.frameInfo.focused}
          onFrame={props.onFrame}
        />
      )}
    </For>
  );
}

interface DisplayFrameProps {
  frame: Frame;
  focused: boolean;
  onFrame: Setter<FrameId>;
}

function DisplayFrame(props: DisplayFrameProps) {
  return (
    <span
      className={props.focused ? "frame-focus" : "frame"}
      onClick={() => props.onFrame(props.frame.id)}
    >
      {props.frame.display}
    </span>
  );
}
