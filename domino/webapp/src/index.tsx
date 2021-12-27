import {
  Accessor,
  createEffect,
  createResource,
  createSignal,
  ErrorBoundary,
  For,
  Setter,
  Show,
  Suspense,
} from "solid-js";
import { render } from "solid-js/web";
import { IRFileAdaptor as adaptIRFile } from "./adaptors/irfile";
import { JssatIRAdaptor as adaptJssatIR } from "./adaptors/jssatir";
import { adaptTypeTree } from "./adaptors/typetree";
import {
  CodeLine,
  fetchMoment,
  fetchOverview,
  Frame,
  Moment,
  MomentSource,
  MomentValue,
  MomentValues,
  Overview,
  Source,
  SourceLocation,
} from "./api";
import { adaptPanel, Window } from "./editor/window";
import { CodeView } from "./panels/codeview/CodeView";
import { adaptSplitView } from "./panels/SplitView";
import { TreeView } from "./panels/TreeView";
import { ShowError } from "./components/ShowError";
import "./styles.less";

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

interface AppContainerProps {
  overview: Overview;
}

const AppContainer = (props: AppContainerProps) => {
  const [value, setValue] = createSignal(0);
  Object.defineProperty(globalThis, "moment", {
    get: value,
    set: setValue,
  });

  const [moment, setMoment] = createSignal<Moment | undefined>(undefined);
  createEffect(async () => setMoment(await fetchMoment(value())));

  document.addEventListener("keydown", (e) => {
    const setValueLim = (value: number) => {
      if (value < 0) return;
      if (value >= props.overview.totalMoments) return;
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

  const panels = [
    adaptPanel(adaptJssatIR([moment, setValue]), CodeView),
    adaptPanel(adaptIRFile(props.overview.sources, moment), CodeView),
    adaptSplitView(adaptPanel(adaptTypeTree(moment), TreeView), () => (
      <CallstackView
        overview={props.overview}
        value={value}
        setValue={setValue}
      />
    )),
  ];

  return <Window panels={panels} />;
};

interface CallstackViewProps {
  overview: Overview;
  value: Accessor<number>;
  setValue: Setter<number>;
}

const CallstackView = (props: CallstackViewProps) => {
  return (
    <>
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

render(App, document.body);
