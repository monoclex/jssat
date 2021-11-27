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
  MomentSource,
  Overview,
  Source,
  SourceLocation,
} from "./api";
import "./styles.less";

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
          {(overview) => <AppContainer overview={overview} />}
        </Show>
      </Suspense>
    </ErrorBoundary>
  );
};

interface CallstackViewProps {
  overview: Overview;
  value: () => number;
  setValue: (v: number) => void;
  moment: () => Moment | undefined;
}

interface AppContainerProps {
  overview: Overview;
}

const AppContainer = (props: AppContainerProps) => {
  const [value, setValue] = createSignal(0);

  const [moment, setMoment] = createSignal<Moment | undefined>(undefined);
  createEffect(async () => setMoment(await fetchMoment(value())));

  return (
    <>
      <div className="vertical-panel">
        <CallstackView
          overview={props.overview}
          value={value}
          setValue={setValue}
          moment={moment}
        />
      </div>
      <div className="vertical-panel">
        <Show when={moment()?.source}>
          {(source) => (
            <SourcePanel sources={props.overview.sources} source={source} />
          )}
        </Show>
      </div>
      <PanelView />
    </>
  );
};

const CallstackView = (props: CallstackViewProps) => {
  document.addEventListener("keydown", (e) => {
    const setValueLim = (value: number) => {
      if (value < 0) return;
      if (value >= props.overview.totalMoments) return;
      props.setValue(value);
    };

    switch (e.key) {
      case "ArrowLeft":
        const prevMoment = props.moment()?.callstack[0].prevMoment;
        setValueLim(prevMoment ? prevMoment : props.value() - 1);
        break;
      case "ArrowRight":
        const nextMoment = props.moment()?.callstack[0].nextMoment;
        setValueLim(nextMoment ? nextMoment : props.value() + 1);
        break;
      case "ArrowUp":
        setValueLim(props.value() - 1);
        break;
      case "ArrowDown":
        setValueLim(props.value() + 1);
        break;
    }
  });

  return (
    <>
      <div className="codeview">
        <Show when={props.moment()}>
          {(moment) => <CodeView moment={moment} />}
        </Show>
      </div>
      <div className="frames">
        <Show when={props.moment()}>
          {(moment) => (
            <For each={moment.callstack}>
              {(frame, idx) => (
                <CallstackFrame
                  isCurrent={idx() == 0}
                  frame={frame}
                  setMoment={props.setValue}
                />
              )}
            </For>
          )}
        </Show>
      </div>
      <div className="time-slider">
        <Slider
          totalMoments={props.overview.totalMoments}
          value={props.value}
          setValue={props.setValue}
        />
      </div>
    </>
  );
};

interface CodeViewProps {
  moment: Moment;
}

const inRangeRel = (
  value: number,
  fixpoint: number,
  relativeStart: number,
  relativeEnd: number
) => value > fixpoint - relativeStart && value < fixpoint + relativeEnd;
const CodeView = (props: CodeViewProps) => {
  const MAX_LINES_AWAY = 30;

  return (
    <>
      <div className="codeview-pre">
        <For
          each={props.moment.code.lines.filter((_, idx) =>
            inRangeRel(idx, props.moment.code.highlighted, MAX_LINES_AWAY, 0)
          )}
        >
          {(line) => <DrawCodeLine codeLine={line} />}
        </For>
      </div>
      <div className="highlighted">
        <DrawCodeLine
          codeLine={props.moment.code.lines[props.moment.code.highlighted]}
        />
      </div>
      <div className="codeview-post">
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
    <span
      className={props.isCurrent ? "frame-focus" : "frame"}
      onClick={() => {
        props.setMoment(props.frame.moment);
      }}
    >
      {props.frame.preview}
    </span>
  );
};

interface SliderProps {
  totalMoments: number;
  value: () => number;
  setValue: (value: number) => void;
}

const Slider = (props: SliderProps) => (
  <input
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

interface SourcePanelProps {
  sources: Source[];
  source: MomentSource;
}

const SourcePanel = (props: SourcePanelProps) => {
  const source = props.sources[props.source.sourceId].text.split("\n");

  const [currentLocation, setLocation] = createSignal(0);

  return (
    <>
      <SourceView
        source={source}
        location={() => props.source.locations[currentLocation()]}
      />
      <div className="frames">
        <For each={props.source.locations}>
          {(location, idx) => (
            <span
              className={currentLocation() == idx() ? "frame-focus" : "frame"}
              onClick={() => setLocation(idx())}
            >
              {JSON.stringify(location.start)} to {JSON.stringify(location.end)}
            </span>
          )}
        </For>
      </div>
    </>
  );
};

interface SourceViewProps {
  source: string[];
  location: () => SourceLocation;
}

const SourceView = (props: SourceViewProps) => {
  return (
    <>
      {() => {
        const startLine = props.location().start.line;
        const endLine = props.location().end.line;

        const MAX_LINES_AWAY = 30;

        const pre = [];
        const focus = [];
        const post = [];

        const upper = Math.min(endLine + MAX_LINES_AWAY, props.source.length);
        for (let i = Math.max(0, startLine - MAX_LINES_AWAY); i < upper; i++) {
          if (i < startLine - 1) pre.push(props.source[i]);
          else if (i >= endLine) post.push(props.source[i]);
          else focus.push(props.source[i]);
        }

        return (
          <div className="codeview">
            <div className="codeview-pre">
              <For each={pre}>{(line) => <span>{line}</span>}</For>
            </div>
            <div className="codeview-focus">
              <For each={focus}>
                {(line, idx) => {
                  let pre = "";
                  let content = line;
                  let post = "";

                  const atStart = idx() == 0;
                  if (atStart) {
                    const col = props.location().start.column;
                    pre = content.substr(0, col);
                    content = content.substring(col);
                  }

                  const atEnd = idx() == focus.length - 1;
                  if (atEnd) {
                    const col = props.location().end.column - pre.length;
                    post = content.substr(col);
                    content = content.substring(0, col);
                  }

                  if (pre === "" && post === "") {
                    const limitEnd = atEnd ? " limitEnd" : "";
                    return (
                      <span className={"highlighted" + limitEnd}>
                        {content}
                      </span>
                    );
                  } else {
                    const stretchToEnd = atStart && !atEnd;
                    const stretchName = stretchToEnd ? " line-to-end" : "";
                    return (
                      <div className="line-split">
                        {pre && <span>{pre}</span>}
                        <span className={"highlighted" + stretchName}>
                          {content}
                        </span>
                        {post && <span>{post}</span>}
                      </div>
                    );
                  }
                }}
              </For>
            </div>
            <div className="codeview-post">
              <For each={post}>{(line) => <span>{line}</span>}</For>
            </div>
          </div>
        );
      }}
    </>
  );
};

const PanelView = () => <div className="panel">ill put some stuff here</div>;

render(App, document.body);
