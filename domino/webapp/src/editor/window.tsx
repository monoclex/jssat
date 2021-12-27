import { ErrorBoundary, JSX, Show } from "solid-js";
import { For } from "solid-js";
import { ShowError } from "../components/ShowError";

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
            <ErrorBoundary fallback={ShowError}>
              <Panel />
            </ErrorBoundary>
          </div>
        )}
      </For>
    </>
  );
}

export function adaptPanel<T>(
  adaptor: () => T | undefined,
  Component: (props: T) => JSX.Element
) {
  return () => (
    <Show when={adaptor()}>{(props) => <Component {...props} />}</Show>
  );
}
