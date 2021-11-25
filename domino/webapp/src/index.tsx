import { render } from "solid-js/web";
import "./styles.css";

const App = () => (
  <div className="app-container">
    <CallstackView />
    <PanelView />
  </div>
);

const CallstackView = () => (
  <div className="callstack">
    <div className="callstack-current-frame">this is the current frame</div>
    <div className="callstack-frames">
      big ol' list of frames!
      <Loremipsum n={10} />
    </div>
    <div className="callstack-time-slider">
      <Slider />
    </div>
  </div>
);

const Slider = () => {
  return (
    <input className="time-slider" type="range" min="1" max="10" value="5" />
  );
};

const Loremipsum = ({ n }: { n: number }) => {
  if (n == 0) return <h1>Lorem ipsum</h1>;
  return (
    <>
      <Loremipsum n={0} />
      <Loremipsum n={n - 1} />
    </>
  );
};

const PanelView = () => <div className="panel">ill put some stuff here</div>;

render(App, document.body);
