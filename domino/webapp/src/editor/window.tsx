import type { JSX } from "solid-js";
import { For } from "solid-js";

export type WindowPanel = () => JSX.Element;

interface WindowProps {
  panels: WindowPanel[];
}

export function Window(props: WindowProps) {
  return (
    <>
      <For each={props.panels}>
        {(Panel) => (
          <div className="vertical-panel">
            <Panel />
          </div>
        )}
      </For>
    </>
  );
}
