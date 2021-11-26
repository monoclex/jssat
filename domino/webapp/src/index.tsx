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

  const [moment, setMoment] = createSignal<Moment | undefined>(undefined);
  createEffect(async () => setMoment(await fetchMoment(value())));

  document.addEventListener("keydown", (e) => {
    const setValueLim = (value: number) => {
      if (value < 0) return;
      if (value >= props.totalMoments) return;
      setValue(value);
    };

    switch (e.key) {
      case "ArrowLeft":
        const prevMoment = moment()?.callstack[0].prevMoment;
        setValueLim(prevMoment ? prevMoment : value() - 1);
        break;
      case "ArrowRight":
        const nextMoment = moment()?.callstack[0].nextMoment;
        setValueLim(nextMoment ? nextMoment : value() + 1);
        break;
      case "ArrowUp":
        setValueLim(value() - 1);
        break;
      case "ArrowDown":
        setValueLim(value() + 1);
        break;
    }
  });

  return (
    <div className="callstack">
      <div className="callstack-codeview">
        <Show when={moment()}>{(moment) => <CodeView moment={moment} />}</Show>
      </div>
      <div className="callstack-frames">
        <Show when={moment()}>
          {(moment) => (
            <For each={moment.callstack}>
              {(frame, idx) => (
                <CallstackFrame
                  isCurrent={idx() == 0}
                  frame={frame}
                  setMoment={setValue}
                />
              )}
            </For>
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

interface CodeViewProps {
  moment: Moment;
}

const CodeView = (props: CodeViewProps) => {
  const inRangeRel = (
    value: number,
    fixpoint: number,
    relativeStart: number,
    relativeEnd: number
  ) => value > fixpoint - relativeStart && value < fixpoint + relativeEnd;

  const MAX_LINES_AWAY = 30;

  return (
    <>
      <div className="callstack-codeview-prefocus">
        <For
          each={props.moment.code.lines.filter((_, idx) =>
            inRangeRel(idx, props.moment.code.highlighted, MAX_LINES_AWAY, 0)
          )}
        >
          {(line) => <DrawCodeLine codeLine={line} />}
        </For>
      </div>
      <div className="callstack-codeview-focus">
        <DrawCodeLine
          codeLine={props.moment.code.lines[props.moment.code.highlighted]}
        />
      </div>
      <div className="callstack-codeview-postfocus">
        <For
          each={props.moment.code.lines.filter((_, idx) =>
            inRangeRel(idx, props.moment.code.highlighted, 0, MAX_LINES_AWAY)
          )}
        >
          {(line) => <DrawCodeLine codeLine={line} />}
        </For>
      </div>
    </>
  );
};

interface CodeLineProps {
  codeLine: CodeLine;
}

const DrawCodeLine = (props: CodeLineProps) => {
  // TODO: add clickables for registers (to view values),
  //   and clickables for "go to snapshot moment"
  return <span>{props.codeLine.display}</span>;
};

interface CallstackFrameProps {
  frame: Frame;
  isCurrent: boolean;
  setMoment: (value: number) => void;
}

const CallstackFrame = (props: CallstackFrameProps) => {
  return (
    <div
      className={
        "callstack-frame" + (props.isCurrent ? " callstack-current-frame" : "")
      }
      onClick={() => {
        props.setMoment(props.frame.moment);
      }}
    >
      {props.frame.preview}
    </div>
  );
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
    ref={(elem) => {
      elem.addEventListener("keydown", (e) => {
        if (
          e.key === "ArrowLeft" ||
          e.key === "ArrowRight" ||
          e.key === "ArrowUp" ||
          e.key === "ArrowDown"
        ) {
          // prevent from advancing the instruction, as we already have a DOM hook
          // for arrow keys. not doing this could cause the IP to be moved twice
          e.preventDefault();
        }
      });
    }}
  />
);

const PanelView = () => <div className="panel">ill put some stuff here</div>;

render(App, document.body);
