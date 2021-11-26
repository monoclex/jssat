import { VirtualContainer } from "@minht11/solid-virtual-container";
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
import {
  CodeLine,
  fetchMoment,
  fetchOverview,
  Frame,
  Moment,
  Overview,
} from "./api";
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
  const [value, setValue] = createSignal(0);
  createEffect(() => {
    console.log("value is currently", value());
  });

  console.error("adding evt list");
  document.addEventListener("keydown", (e) => {
    console.log("keydown fired!");
    switch (e.key) {
      case "ArrowLeft":
        if (value() > 0) setValue(value() - 1);
        break;
      case "ArrowRight":
        if (value() < props.totalMoments - 1) setValue(value() + 1);
        break;
    }
  });

  const [moment, setMoment] = createSignal<Moment | undefined>(undefined);
  createEffect(async () => setMoment(await fetchMoment(value())));

  const MAX_VISIBLE_LINES_FROM_CENTER = 30;

  return (
    <div className="callstack">
      <div className="callstack-current-frame">
        <Show when={moment()}>
          {(moment) => (
            <>
              <div className="callstack-line-prefocus">
                <For
                  each={moment.code.lines.filter(
                    (_, idx) =>
                      idx >
                        moment.code.highlighted -
                          MAX_VISIBLE_LINES_FROM_CENTER &&
                      idx < moment.code.highlighted
                  )}
                >
                  {(line) => <DrawCodeLine codeLine={line} />}
                </For>
              </div>
              <div className="callstack-line-focus">
                <DrawCodeLine
                  codeLine={moment.code.lines[moment.code.highlighted]}
                />
              </div>
              <div className="callstack-line-postfocus">
                <For
                  each={moment.code.lines.filter(
                    (_, idx) =>
                      idx > moment.code.highlighted &&
                      idx <
                        moment.code.highlighted + MAX_VISIBLE_LINES_FROM_CENTER
                  )}
                >
                  {(line) => <DrawCodeLine codeLine={line} />}
                </For>
              </div>
            </>
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

interface CodeLineProps {
  codeLine: CodeLine;
}

const DrawCodeLine = (props: CodeLineProps) => {
  return <span>{props.codeLine.display}</span>;
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
    min={0}
    max={props.totalMoments - 1}
    value={props.value()}
    onInput={(e) => props.setValue(parseInt(e.currentTarget.value))}
  />
);

const PanelView = () => <div className="panel">ill put some stuff here</div>;

render(App, document.body);
