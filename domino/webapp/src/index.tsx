import createDebounce from "@solid-primitives/debounce";
import {
  Component,
  createEffect,
  createResource,
  createSignal,
  ErrorBoundary,
  For,
  JSX,
  JSXElement,
  lazy,
  Show,
  Suspense,
} from "solid-js";
import { render } from "solid-js/web";
import { fetchMoment, fetchOverview, Frame, Moment, Overview } from "./api";
import "./styles.css";

const ShowError = (err: Error) => {
  return (
    <>
      <h1>eror!</h1>
      <p>uh oh: {err.toString()}</p>
    </>
  );
};

const ShowLoading = (message: string) => () => <h1>{message}</h1>;

const App = () => {
  const [overview] = createResource(fetchOverview);

  return (
    <ErrorBoundary fallback={ShowError}>
      <Suspense fallback={ShowLoading("contacting backend...")}>
        <Show when={overview()}>
          {(overview) => (
            <div className="app-container">
              <CallstackView totalMoments={overview.totalMoments} />
              <PanelView />
            </div>
          )}
        </Show>
      </Suspense>
    </ErrorBoundary>
  );
};

interface CallstackViewProps {
  totalMoments: number;
}

const CallstackView = (props: CallstackViewProps) => {
  const [value, setValue] = createSignal(1);

  const [moment, setMoment] = createSignal<Moment | undefined>(undefined);
  createEffect(async () => setMoment(await fetchMoment(value())));

  return (
    <div className="callstack">
      <div className="callstack-current-frame">
        <Show when={moment()}>
          {(moment) => (
            <pre>
              <For each={moment.code.lines}>
                {(line) => line.display + "\n"}
              </For>
            </pre>
          )}
        </Show>
      </div>
      <div className="callstack-frames">
        <Show when={moment()}>
          {(moment) => (
            <pre>
              <For each={moment.callstack}>
                {(frame) => <CallstackFrame frame={frame} />}
              </For>
            </pre>
          )}
        </Show>
      </div>
      <div className="callstack-time-slider">
        <Slider
          totalMoments={props.totalMoments}
          value={value}
          setValue={setValue}
        />
      </div>
    </div>
  );
};

interface CallstackFrameProps {
  frame: Frame;
}

const CallstackFrame = (props: CallstackFrameProps) => {
  return <div className="callstack-frame">{props.frame.preview}</div>;
};

interface SliderProps {
  totalMoments: number;
  value: () => number;
  setValue: (value: number) => void;
}

const Slider = (props: SliderProps) => (
  <input
    className="time-slider"
    type="range"
    min={1}
    max={props.totalMoments}
    value={props.value()}
    onInput={(e) => props.setValue(parseInt(e.currentTarget.value))}
  />
);

const PanelView = () => <div className="panel">ill put some stuff here</div>;

render(App, document.body);
