import { WindowPanel } from "../editor/window";

interface SplitViewProps {
  upper: WindowPanel;
  lower: WindowPanel;
}

export function adaptSplitView(upper: WindowPanel, lower: WindowPanel) {
  return () => <SplitView upper={upper} lower={lower} />;
}

export function SplitView(props: SplitViewProps) {
  // this takes advantage of the fact that panels are in `veritcal-panel` divs,
  // which are already column oriented
  return (
    <>
      <div className="grow">
        <props.upper />
      </div>
      <props.lower />
    </>
  );
}
