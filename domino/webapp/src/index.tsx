import {
  createEffect,
  createResource,
  createSignal,
  ErrorBoundary,
  For,
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
import { TreeView } from "./panels/treeview/TreeView";
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

interface AppContainerProps {
  overview: Overview;
}

const AppContainer = (props: AppContainerProps) => {
  const [value, setValue] = createSignal(0);
  Object.defineProperty(globalThis, "moment", {
    get: value,
    set: setValue,
  });

  document.addEventListener("keydown", (e) => {
    const setValueLim = (value: number) => {
      if (value < 0) return;
      if (value >= props.overview.totalMoments) return;
      setValue(value);
    };

    switch (e.key) {
      case "ArrowLeft":
        // const prevMoment = props.moment()?.callstack[0].prevMoment;
        // setValueLim(prevMoment ? prevMoment : props.value() - 1);
        break;
      case "ArrowRight":
        // const nextMoment = props.moment()?.callstack[0].nextMoment;
        // setValueLim(nextMoment ? nextMoment : props.value() + 1);
        break;
      case "ArrowUp":
        setValueLim(value() - 1);
        break;
      case "ArrowDown":
        setValueLim(value() + 1);
        break;
    }
  });

  const [moment, setMoment] = createSignal<Moment | undefined>(undefined);
  createEffect(async () => setMoment(await fetchMoment(value())));

  const panels = [
    adaptPanel(adaptJssatIR([moment, setValue]), CodeView),
    adaptPanel(adaptIRFile(props.overview.sources, moment), CodeView),
    adaptPanel(adaptTypeTree(moment), TreeView),
  ];

  return <Window panels={panels} />;
};

const CallstackView = (props: any) => {
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
